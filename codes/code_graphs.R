# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 13: Descriptive Time-Series Figures
#
# Figures:
#   1. University exposure, pooled sample
#   2. Labor-force participation
#   3. Employment
#   4. Unemployment among labor-force participants
#   5. Undereducation
#   6. Overeducation
#   7. Any vertical mismatch
#   8. Horizontal mismatch: broad definition
#
# Design:
#   - Exposure figure: one pooled panel
#   - Outcome figures: two panels, Women and Men
#   - Three lines:
#       Q1 provinces: lowest average university exposure
#       Q4 provinces: highest average university exposure
#       National mean
#
# Important:
#   Province exposure quartiles are defined once and remain fixed over time.
#   All annual series are survey-weighted.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 200
)

library(data.table)
library(fst)
library(ggplot2)
library(scales)


# ==============================================================================
# STEP 1: Define paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step13_time_series_figures_revised"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The final analysis-ready dataset was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Read only required variables
# ==============================================================================

variables_to_read <- c(
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  
  # Treatment
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Employment-status outcomes
  "labor_force_participation",
  "employed",
  "unemployed",
  
  # Mismatch outcomes
  "vertical_mismatch_final",
  "overeducated_final",
  "undereducated_final",
  "horizontal_mismatch_detailed_broad"
)

cat("\nReading variables for time-series figures...\n")

dt <- as.data.table(
  read_fst(
    input_file,
    columns = variables_to_read
  )
)

cat(
  "\nObservations:",
  format(nrow(dt), big.mark = ","),
  "\n"
)

cat(
  "Variables:",
  ncol(dt),
  "\n"
)

cat(
  "Memory size:",
  format(object.size(dt), units = "GB"),
  "\n"
)


# ==============================================================================
# STEP 3: Construct analytical variables
# ==============================================================================

dt[
  ,
  historical_province := province_historical
]

dt[
  ,
  cohort := as.integer(analysis_birth_year)
]

dt[
  ,
  sex := fcase(
    gender == 1L, "Women",
    gender == 2L, "Men",
    default = NA_character_
  )
]

dt[
  ,
  urban := fcase(
    urban_rural == "Urban", 1L,
    urban_rural == "Rural", 0L,
    default = NA_integer_
  )
]

dt[
  ,
  valid_weight :=
    !is.na(weight) &
    is.finite(weight) &
    weight > 0
]

dt[
  ,
  valid_cohort :=
    !is.na(cohort) &
    cohort >= 1355L &
    cohort <= 1383L
]

dt[
  ,
  sample_main :=
    complete_undergrad_exposure == 1L &
    !is.na(undergrad_exposure_18_20) &
    valid_weight &
    valid_cohort &
    !is.na(historical_province) &
    !is.na(year) &
    !is.na(sex) &
    !is.na(urban)
]

cat(
  "\nMain analytical sample:",
  format(dt[sample_main == TRUE, .N], big.mark = ","),
  "\n"
)


# ==============================================================================
# STEP 4: Define fixed province exposure quartiles
#
# Province exposure is calculated from unique province-cohort treatment cells.
# Quartile assignments remain fixed across all survey years.
# ==============================================================================

province_cohort_exposure <- unique(
  dt[
    sample_main == TRUE,
    .(
      historical_province,
      cohort,
      undergrad_exposure_18_20
    )
  ]
)

province_cohort_check <- province_cohort_exposure[
  ,
  .(
    unique_exposure_values =
      uniqueN(undergrad_exposure_18_20)
  ),
  by = .(
    historical_province,
    cohort
  )
]

problem_cells <- province_cohort_check[
  unique_exposure_values != 1L
]

if (nrow(problem_cells) > 0L) {
  stop(
    "Exposure is not unique within some historical province-cohort cells."
  )
}

province_exposure_summary <- province_cohort_exposure[
  ,
  .(
    province_mean_exposure =
      mean(
        undergrad_exposure_18_20,
        na.rm = TRUE
      ),
    
    province_median_exposure =
      median(
        undergrad_exposure_18_20,
        na.rm = TRUE
      ),
    
    number_of_cohorts =
      uniqueN(cohort)
  ),
  by = historical_province
]

setorder(
  province_exposure_summary,
  province_mean_exposure,
  historical_province
)

province_exposure_summary[
  ,
  exposure_rank := seq_len(.N)
]

number_of_provinces <- nrow(
  province_exposure_summary
)

province_exposure_summary[
  ,
  exposure_quartile_number :=
    pmin(
      4L,
      ceiling(
        4 * exposure_rank /
          number_of_provinces
      )
    )
]

province_exposure_summary[
  ,
  exposure_quartile :=
    paste0(
      "Q",
      exposure_quartile_number
    )
]

cat(
  "\nNumber of historical provinces:",
  number_of_provinces,
  "\n"
)

cat(
  "\nProvince exposure groups:\n"
)

print(
  province_exposure_summary[
    ,
    .(
      historical_province,
      province_mean_exposure,
      exposure_rank,
      exposure_quartile
    )
  ]
)

fwrite(
  province_exposure_summary,
  file.path(
    output_folder,
    "step13_fixed_province_exposure_quartiles.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 5: Merge quartile assignments into individual data
# ==============================================================================

dt <- merge(
  dt,
  province_exposure_summary[
    ,
    .(
      historical_province,
      province_mean_exposure,
      exposure_quartile
    )
  ],
  by = "historical_province",
  all.x = TRUE,
  sort = FALSE
)

cat(
  "\nMissing province quartile assignments:",
  dt[
    sample_main == TRUE &
      is.na(exposure_quartile),
    .N
  ],
  "\n"
)


# ==============================================================================
# STEP 6: Save Q1 and Q4 province lists
# ==============================================================================

q1_q4_provinces <- province_exposure_summary[
  exposure_quartile %in% c(
    "Q1",
    "Q4"
  ),
  .(
    exposure_group = exposure_quartile,
    historical_province,
    province_mean_exposure,
    exposure_rank
  )
]

setorder(
  q1_q4_provinces,
  exposure_group,
  province_mean_exposure
)

print(q1_q4_provinces)

fwrite(
  q1_q4_provinces,
  file.path(
    output_folder,
    "step13_q1_q4_province_lists.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 7: Weighted-mean helper
# ==============================================================================

safe_weighted_mean <- function(
    outcome,
    weights
) {
  
  valid <- (
    !is.na(outcome) &
      !is.na(weights) &
      is.finite(weights) &
      weights > 0
  )
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  weighted.mean(
    outcome[valid],
    weights[valid]
  )
}


# ==============================================================================
# STEP 8: Create pooled exposure time series
#
# Exposure is common to men and women within a province-cohort cell.
# Therefore, the exposure graph is shown for the pooled sample only.
# ==============================================================================

exposure_quartile_series <- dt[
  sample_main == TRUE &
    !is.na(undergrad_exposure_18_20) &
    exposure_quartile %in% c(
      "Q1",
      "Q4"
    ),
  .(
    value =
      safe_weighted_mean(
        undergrad_exposure_18_20,
        weight
      ),
    
    observations = .N,
    
    weighted_population =
      sum(
        weight,
        na.rm = TRUE
      )
  ),
  by = .(
    year,
    exposure_quartile
  )
]

exposure_quartile_series[
  ,
  comparison_group := fcase(
    exposure_quartile == "Q1",
    "Q1: Low-exposure provinces",
    
    exposure_quartile == "Q4",
    "Q4: High-exposure provinces",
    
    default = NA_character_
  )
]

exposure_national_series <- dt[
  sample_main == TRUE &
    !is.na(undergrad_exposure_18_20),
  .(
    value =
      safe_weighted_mean(
        undergrad_exposure_18_20,
        weight
      ),
    
    observations = .N,
    
    weighted_population =
      sum(
        weight,
        na.rm = TRUE
      )
  ),
  by = year
]

exposure_national_series[
  ,
  `:=`(
    exposure_quartile = NA_character_,
    comparison_group = "National mean"
  )
]

exposure_series <- rbindlist(
  list(
    exposure_quartile_series,
    exposure_national_series
  ),
  use.names = TRUE,
  fill = TRUE
)

exposure_series[
  ,
  `:=`(
    outcome = "University exposure",
    value_type = "Exposure per 100,000"
  )
]

exposure_series[
  ,
  comparison_group := factor(
    comparison_group,
    levels = c(
      "Q1: Low-exposure provinces",
      "National mean",
      "Q4: High-exposure provinces"
    )
  )
]

setorder(
  exposure_series,
  comparison_group,
  year
)


# ==============================================================================
# STEP 9: Function to construct gender-specific outcome time series
# ==============================================================================

create_time_series <- function(
    data,
    outcome_variable,
    sample_condition,
    outcome_name
) {
  
  analysis_data <- data[
    eval(sample_condition) &
      !is.na(get(outcome_variable))
  ]
  
  quartile_series <- analysis_data[
    exposure_quartile %in% c(
      "Q1",
      "Q4"
    ),
    .(
      value =
        safe_weighted_mean(
          get(outcome_variable),
          weight
        ),
      
      observations = .N,
      
      weighted_population =
        sum(
          weight,
          na.rm = TRUE
        )
    ),
    by = .(
      year,
      sex,
      exposure_quartile
    )
  ]
  
  quartile_series[
    ,
    comparison_group := fcase(
      exposure_quartile == "Q1",
      "Q1: Low-exposure provinces",
      
      exposure_quartile == "Q4",
      "Q4: High-exposure provinces",
      
      default = NA_character_
    )
  ]
  
  national_series <- analysis_data[
    ,
    .(
      value =
        safe_weighted_mean(
          get(outcome_variable),
          weight
        ),
      
      observations = .N,
      
      weighted_population =
        sum(
          weight,
          na.rm = TRUE
        )
    ),
    by = .(
      year,
      sex
    )
  ]
  
  national_series[
    ,
    `:=`(
      exposure_quartile = NA_character_,
      comparison_group = "National mean"
    )
  ]
  
  combined_series <- rbindlist(
    list(
      quartile_series,
      national_series
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  combined_series[
    ,
    value := 100 * value
  ]
  
  combined_series[
    ,
    `:=`(
      outcome = outcome_name,
      value_type = "Percent"
    )
  ]
  
  combined_series[
    ,
    sex := factor(
      sex,
      levels = c(
        "Women",
        "Men"
      )
    )
  ]
  
  combined_series[
    ,
    comparison_group := factor(
      comparison_group,
      levels = c(
        "Q1: Low-exposure provinces",
        "National mean",
        "Q4: High-exposure provinces"
      )
    )
  ]
  
  setorder(
    combined_series,
    sex,
    comparison_group,
    year
  )
  
  combined_series
}


# ==============================================================================
# STEP 10: Define outcome-specific samples
# ==============================================================================

sample_lfp_condition <- quote(
  sample_main == TRUE &
    !is.na(labor_force_participation)
)

sample_employment_condition <- quote(
  sample_main == TRUE &
    !is.na(employed)
)

sample_unemployment_condition <- quote(
  sample_main == TRUE &
    labor_force_participation == 1L &
    !is.na(unemployed)
)

sample_vertical_condition <- quote(
  sample_main == TRUE &
    employed == 1L &
    !is.na(vertical_mismatch_final)
)

sample_overeducation_condition <- quote(
  sample_main == TRUE &
    employed == 1L &
    !is.na(overeducated_final)
)

sample_undereducation_condition <- quote(
  sample_main == TRUE &
    employed == 1L &
    !is.na(undereducated_final)
)

sample_horizontal_condition <- quote(
  sample_main == TRUE &
    employed == 1L &
    !is.na(horizontal_mismatch_detailed_broad)
)


# ==============================================================================
# STEP 11: Construct all outcome series
# ==============================================================================

lfp_series <- create_time_series(
  data = dt,
  outcome_variable = "labor_force_participation",
  sample_condition = sample_lfp_condition,
  outcome_name = "Labor-force participation"
)

employment_series <- create_time_series(
  data = dt,
  outcome_variable = "employed",
  sample_condition = sample_employment_condition,
  outcome_name = "Employment"
)

unemployment_series <- create_time_series(
  data = dt,
  outcome_variable = "unemployed",
  sample_condition = sample_unemployment_condition,
  outcome_name = "Unemployment"
)

undereducation_series <- create_time_series(
  data = dt,
  outcome_variable = "undereducated_final",
  sample_condition = sample_undereducation_condition,
  outcome_name = "Undereducation"
)

overeducation_series <- create_time_series(
  data = dt,
  outcome_variable = "overeducated_final",
  sample_condition = sample_overeducation_condition,
  outcome_name = "Overeducation"
)

vertical_mismatch_series <- create_time_series(
  data = dt,
  outcome_variable = "vertical_mismatch_final",
  sample_condition = sample_vertical_condition,
  outcome_name = "Vertical mismatch"
)

horizontal_mismatch_series <- create_time_series(
  data = dt,
  outcome_variable = "horizontal_mismatch_detailed_broad",
  sample_condition = sample_horizontal_condition,
  outcome_name = "Horizontal mismatch"
)


# ==============================================================================
# STEP 12: Save all underlying graph data
# ==============================================================================

fwrite(
  exposure_series,
  file.path(
    output_folder,
    "step13_exposure_time_series_pooled.csv"
  ),
  bom = TRUE
)

fwrite(
  lfp_series,
  file.path(
    output_folder,
    "step13_lfp_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  employment_series,
  file.path(
    output_folder,
    "step13_employment_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  unemployment_series,
  file.path(
    output_folder,
    "step13_unemployment_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  undereducation_series,
  file.path(
    output_folder,
    "step13_undereducation_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  overeducation_series,
  file.path(
    output_folder,
    "step13_overeducation_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  vertical_mismatch_series,
  file.path(
    output_folder,
    "step13_vertical_mismatch_time_series.csv"
  ),
  bom = TRUE
)

fwrite(
  horizontal_mismatch_series,
  file.path(
    output_folder,
    "step13_horizontal_mismatch_time_series.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Journal-style plotting theme
# ==============================================================================

journal_theme <- theme_classic(
  base_size = 12
) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13,
      hjust = 0
    ),
    
    plot.subtitle = element_text(
      size = 10.5,
      margin = margin(
        b = 8
      )
    ),
    
    plot.caption = element_text(
      size = 8.5,
      hjust = 0,
      color = "grey30",
      margin = margin(
        t = 8
      )
    ),
    
    axis.title = element_text(
      size = 11
    ),
    
    axis.text = element_text(
      size = 9.5,
      color = "black"
    ),
    
    axis.line = element_line(
      linewidth = 0.4,
      color = "black"
    ),
    
    axis.ticks = element_line(
      linewidth = 0.4,
      color = "black"
    ),
    
    strip.background = element_rect(
      fill = "grey92",
      color = "black",
      linewidth = 0.4
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    panel.spacing = grid::unit(
      1.2,
      "lines"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 9.5
    ),
    
    legend.key.width = grid::unit(
      1.8,
      "cm"
    ),
    
    plot.margin = margin(
      10,
      12,
      12,
      10
    )
  )


# ==============================================================================
# STEP 14: One-panel pooled exposure figure
# ==============================================================================

create_exposure_figure <- function(
    graph_data,
    title,
    subtitle,
    caption
) {
  
  ggplot(
    graph_data,
    aes(
      x = year,
      y = value,
      group = comparison_group,
      color = comparison_group,
      linetype = comparison_group
    )
  ) +
    geom_line(
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    geom_point(
      size = 1.7,
      stroke = 0.3,
      na.rm = TRUE
    ) +
    scale_x_continuous(
      breaks = sort(
        unique(graph_data$year)
      ),
      labels = function(x) {
        ifelse(
          x %% 2 == 0,
          as.character(x),
          ""
        )
      },
      expand = expansion(
        mult = c(
          0.015,
          0.025
        )
      )
    ) +
    scale_color_manual(
      values = c(
        "Q1: Low-exposure provinces" = "#377EB8",
        "National mean" = "#222222",
        "Q4: High-exposure provinces" = "#E41A1C"
      ),
      drop = FALSE
    ) +
    scale_linetype_manual(
      values = c(
        "Q1: Low-exposure provinces" = "dashed",
        "National mean" = "solid",
        "Q4: High-exposure provinces" = "longdash"
      ),
      drop = FALSE
    ) +
    scale_y_continuous(
      labels = label_number(
        accuracy = 1,
        big.mark = ","
      ),
      expand = expansion(
        mult = c(
          0.04,
          0.08
        )
      )
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Survey year",
      y = "Enrollment exposure per 100,000 population",
      caption = caption,
      color = NULL,
      linetype = NULL
    ) +
    journal_theme
}


# ==============================================================================
# STEP 15: Two-panel outcome plotting function
# ==============================================================================

create_journal_figure <- function(
    graph_data,
    title,
    subtitle,
    y_axis_title,
    caption
) {
  
  ggplot(
    graph_data,
    aes(
      x = year,
      y = value,
      group = comparison_group,
      color = comparison_group,
      linetype = comparison_group
    )
  ) +
    geom_line(
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    geom_point(
      size = 1.7,
      stroke = 0.3,
      na.rm = TRUE
    ) +
    facet_wrap(
      ~sex,
      nrow = 1,
      scales = "fixed"
    ) +
    scale_x_continuous(
      breaks = sort(
        unique(graph_data$year)
      ),
      labels = function(x) {
        ifelse(
          x %% 2 == 0,
          as.character(x),
          ""
        )
      },
      expand = expansion(
        mult = c(
          0.015,
          0.025
        )
      )
    ) +
    scale_y_continuous(
      labels = function(x) {
        paste0(
          number(
            x,
            accuracy = 1
          ),
          "%"
        )
      },
      expand = expansion(
        mult = c(
          0.04,
          0.08
        )
      )
    ) +
    scale_color_manual(
      values = c(
        "Q1: Low-exposure provinces" = "#377EB8",
        "National mean" = "#222222",
        "Q4: High-exposure provinces" = "#E41A1C"
      ),
      drop = FALSE
    ) +
    scale_linetype_manual(
      values = c(
        "Q1: Low-exposure provinces" = "dashed",
        "National mean" = "solid",
        "Q4: High-exposure provinces" = "longdash"
      ),
      drop = FALSE
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Survey year",
      y = y_axis_title,
      caption = caption,
      color = NULL,
      linetype = NULL
    ) +
    journal_theme
}


# ==============================================================================
# STEP 16: Common graph caption
# ==============================================================================

common_caption <- paste0(
  "Notes: Survey-weighted annual means. ",
  "Q1 and Q4 are fixed province groups based on average exposure."
)


# ==============================================================================
# STEP 17: Create all figures
# ==============================================================================

figure_exposure <- create_exposure_figure(
  graph_data = exposure_series,
  
  title =
    "Average university exposure of observed cohorts by province group",
  
  subtitle =
    "Average undergraduate enrollment exposure at ages 18–20",
  
  caption = paste0(
    common_caption,
    " Exposure refers to the cohorts observed in each survey year."
  )
)


figure_lfp <- create_journal_figure(
  graph_data = lfp_series,
  
  title =
    "Labor-force participation by province exposure group and gender",
  
  subtitle =
    "Survey-weighted annual labor-force participation rates",
  
  y_axis_title =
    "Labor-force participation rate",
  
  caption = common_caption
)


figure_employment <- create_journal_figure(
  graph_data = employment_series,
  
  title =
    "Employment by province exposure group and gender",
  
  subtitle =
    "Survey-weighted annual employment rates",
  
  y_axis_title =
    "Employment rate",
  
  caption = common_caption
)


figure_unemployment <- create_journal_figure(
  graph_data = unemployment_series,
  
  title =
    "Unemployment by province exposure group and gender",
  
  subtitle =
    "Survey-weighted annual unemployment rates among labor-force participants",
  
  y_axis_title =
    "Unemployment rate",
  
  caption = paste0(
    common_caption,
    " The unemployment sample includes labor-force participants only."
  )
)


figure_undereducation <- create_journal_figure(
  graph_data = undereducation_series,
  
  title =
    "Undereducation by province exposure group and gender",
  
  subtitle =
    "Employed workers with valid education and occupation classifications",
  
  y_axis_title =
    "Undereducation rate",
  
  caption = paste0(
    common_caption,
    " Undereducation is defined relative to occupation-specific modal education."
  )
)


figure_overeducation <- create_journal_figure(
  graph_data = overeducation_series,
  
  title =
    "Overeducation by province exposure group and gender",
  
  subtitle =
    "Employed workers with valid education and occupation classifications",
  
  y_axis_title =
    "Overeducation rate",
  
  caption = paste0(
    common_caption,
    " Overeducation is defined relative to occupation-specific modal education."
  )
)


figure_vertical <- create_journal_figure(
  graph_data = vertical_mismatch_series,
  
  title =
    "Vertical mismatch by province exposure group and gender",
  
  subtitle =
    "Any overeducation or undereducation among employed workers",
  
  y_axis_title =
    "Vertical mismatch rate",
  
  caption = paste0(
    common_caption,
    " Vertical mismatch includes overeducation or undereducation."
  )
)


figure_horizontal <- create_journal_figure(
  graph_data = horizontal_mismatch_series,
  
  title =
    "Horizontal mismatch by province exposure group and gender",
  
  subtitle =
    "Broad field-of-study mismatch among tertiary-educated employed workers",
  
  y_axis_title =
    "Horizontal mismatch rate",
  
  caption = paste0(
    common_caption,
    " Horizontal mismatch uses the preferred broad field-to-occupation definition."
  )
)


# ==============================================================================
# STEP 18: Display figures in RStudio
# ==============================================================================

print(figure_exposure)
print(figure_lfp)
print(figure_employment)
print(figure_unemployment)
print(figure_undereducation)
print(figure_overeducation)
print(figure_vertical)
print(figure_horizontal)


# ==============================================================================
# STEP 19: Save figures as PNG and PDF
# ==============================================================================

save_figure <- function(
    plot_object,
    filename_stub
) {
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0(
        filename_stub,
        ".png"
      )
    ),
    plot = plot_object,
    width = 10,
    height = 6.3,
    units = "in",
    dpi = 400,
    bg = "white"
  )
  
  ggsave(
    filename = file.path(
      output_folder,
      paste0(
        filename_stub,
        ".pdf"
      )
    ),
    plot = plot_object,
    width = 10,
    height = 6.3,
    units = "in",
    device = cairo_pdf,
    bg = "white"
  )
}


save_figure(
  figure_exposure,
  "figure_01_university_exposure_pooled"
)

save_figure(
  figure_lfp,
  "figure_02_labor_force_participation"
)

save_figure(
  figure_employment,
  "figure_03_employment"
)

save_figure(
  figure_unemployment,
  "figure_04_unemployment"
)

save_figure(
  figure_undereducation,
  "figure_05_undereducation"
)

save_figure(
  figure_overeducation,
  "figure_06_overeducation"
)

save_figure(
  figure_vertical,
  "figure_07_vertical_mismatch"
)

save_figure(
  figure_horizontal,
  "figure_08_horizontal_mismatch"
)


# ==============================================================================
# STEP 20: Save graph-data validation
# ==============================================================================

outcome_figure_data <- rbindlist(
  list(
    lfp_series,
    employment_series,
    unemployment_series,
    undereducation_series,
    overeducation_series,
    vertical_mismatch_series,
    horizontal_mismatch_series
  ),
  use.names = TRUE,
  fill = TRUE
)

outcome_validation <- outcome_figure_data[
  ,
  .(
    minimum_year =
      min(
        year,
        na.rm = TRUE
      ),
    
    maximum_year =
      max(
        year,
        na.rm = TRUE
      ),
    
    number_of_years =
      uniqueN(year),
    
    minimum_value =
      min(
        value,
        na.rm = TRUE
      ),
    
    maximum_value =
      max(
        value,
        na.rm = TRUE
      ),
    
    missing_values =
      sum(
        is.na(value)
      ),
    
    observations_across_cells =
      sum(
        observations,
        na.rm = TRUE
      )
  ),
  by = .(
    outcome,
    sex,
    comparison_group
  )
]

exposure_validation <- exposure_series[
  ,
  .(
    minimum_year =
      min(
        year,
        na.rm = TRUE
      ),
    
    maximum_year =
      max(
        year,
        na.rm = TRUE
      ),
    
    number_of_years =
      uniqueN(year),
    
    minimum_value =
      min(
        value,
        na.rm = TRUE
      ),
    
    maximum_value =
      max(
        value,
        na.rm = TRUE
      ),
    
    missing_values =
      sum(
        is.na(value)
      ),
    
    observations_across_cells =
      sum(
        observations,
        na.rm = TRUE
      )
  ),
  by = comparison_group
]

print(outcome_validation)
print(exposure_validation)

fwrite(
  outcome_validation,
  file.path(
    output_folder,
    "step13_outcome_time_series_validation.csv"
  ),
  bom = TRUE
)

fwrite(
  exposure_validation,
  file.path(
    output_folder,
    "step13_exposure_time_series_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "REVISED STEP 13 FIGURES COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain changes:\n",
  "- University exposure is now shown in one pooled panel.\n",
  "- Outcome figures remain separated for women and men.\n",
  "- Figure notes were shortened.\n",
  "- Figure height was increased to prevent caption truncation.\n"
)

cat(
  "\nFigures and underlying data were saved in:\n",
  output_folder,
  "\n"
)