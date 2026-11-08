# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 21: Final Empirical Organization, Tables, and Figures
#
# Purpose:
#   1. Collect final estimates from Steps 14–20
#   2. Create main-text tables
#   3. Create appendix tables
#   4. Create publication-ready figures
#   5. Create a master empirical-results index
#
# No new regressions are estimated.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 500
)

library(data.table)
library(ggplot2)


# ==============================================================================
# STEP 1: Define folders
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

analysis_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

output_folder <- file.path(
  analysis_folder,
  "step21_final_empirical_organization"
)

table_folder <- file.path(
  output_folder,
  "tables"
)

figure_folder <- file.path(
  output_folder,
  "figures"
)

appendix_folder <- file.path(
  output_folder,
  "appendix"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  appendix_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(analysis_folder)) {
  stop(
    paste0(
      "Analysis folder not found:\n",
      analysis_folder
    )
  )
}


# ==============================================================================
# STEP 2: Define known input files
# ==============================================================================

step14_file <- file.path(
  analysis_folder,
  "step14_selection_joint_mismatch",
  "step14_selection_joint_regression_results.csv"
)

step17_file <- file.path(
  analysis_folder,
  "step17_occupational_destination",
  "step17_occupation_regression_results.csv"
)

step18_file <- file.path(
  analysis_folder,
  "step18_labor_demand_mechanism",
  "step18_mechanism_regression_results.csv"
)

step18_national_file <- file.path(
  analysis_folder,
  "step18_labor_demand_mechanism",
  "step18_national_labor_demand_time_series.csv"
)

step19_file <- file.path(
  analysis_folder,
  "step19_alternative_mismatch_definitions",
  "step19_alternative_mismatch_regression_results.csv"
)

step20C_file <- file.path(
  analysis_folder,
  "step20_timing_placebo",
  "step20C_timing_regression_results.csv"
)

step20D_file <- file.path(
  analysis_folder,
  "step20_timing_placebo",
  "step20D_joint_exposure_regression_results.csv"
)


# ==============================================================================
# STEP 3: Automatically locate Step 15 field-results file
# ==============================================================================

step15_candidates <- list.files(
  analysis_folder,
  pattern = "step15.*\\.csv$|field.*heterogeneity.*\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

step15_candidates <- step15_candidates[
  !grepl(
    "validation|diagnostic|failed|compact|table|summary",
    basename(step15_candidates),
    ignore.case = TRUE
  )
]

cat(
  "\nPossible Step 15 field-result files:\n"
)

print(step15_candidates)

step15_file <- if (
  length(step15_candidates) > 0L
) {
  step15_candidates[1]
} else {
  NA_character_
}


# ==============================================================================
# STEP 4: Safe file reader
# ==============================================================================

read_result_file <- function(
    file_path,
    file_label,
    required = TRUE
) {
  
  if (
    is.na(file_path) ||
    !file.exists(file_path)
  ) {
    
    message_text <- paste0(
      "Missing file for ",
      file_label,
      ":\n",
      file_path
    )
    
    if (required) {
      stop(message_text)
    } else {
      warning(message_text)
      return(NULL)
    }
  }
  
  result <- fread(
    file_path,
    showProgress = FALSE
  )
  
  result[
    ,
    source_file := file_label
  ]
  
  cat(
    "\nLoaded:",
    file_label,
    "\nPath:",
    file_path,
    "\nRows:",
    nrow(result),
    "\nColumns:",
    ncol(result),
    "\n"
  )
  
  return(result)
}


# ==============================================================================
# STEP 5: Read result files
# ==============================================================================

step14 <- read_result_file(
  step14_file,
  "Step 14 selection and joint mismatch",
  required = TRUE
)

step15 <- read_result_file(
  step15_file,
  "Step 15 field heterogeneity",
  required = FALSE
)

step17 <- read_result_file(
  step17_file,
  "Step 17 occupational destinations",
  required = TRUE
)

step18 <- read_result_file(
  step18_file,
  "Step 18 labor-demand mechanism",
  required = TRUE
)

national_series <- read_result_file(
  step18_national_file,
  "Step 18 national labor-demand series",
  required = TRUE
)

step19 <- read_result_file(
  step19_file,
  "Step 19 alternative mismatch definitions",
  required = TRUE
)

step20C <- read_result_file(
  step20C_file,
  "Step 20C timing windows",
  required = TRUE
)

step20D <- read_result_file(
  step20D_file,
  "Step 20D joint timing model",
  required = TRUE
)


# ==============================================================================
# STEP 6: Inspect Step 14 structure
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 14 COLUMN NAMES\n",
  "============================================================\n"
)

print(names(step14))

if ("outcome" %in% names(step14)) {
  
  cat(
    "\nStep 14 outcomes:\n"
  )
  
  print(
    unique(step14$outcome)
  )
}

if ("analysis_group" %in% names(step14)) {
  
  cat(
    "\nStep 14 analysis groups:\n"
  )
  
  print(
    unique(step14$analysis_group)
  )
}

if ("subgroup" %in% names(step14)) {
  
  cat(
    "\nStep 14 subgroups:\n"
  )
  
  print(
    unique(step14$subgroup)
  )
}


# ==============================================================================
# STEP 7: General formatting helpers
# ==============================================================================

format_stars <- function(p_value) {
  
  fcase(
    is.na(p_value), "",
    p_value < 0.01, "***",
    p_value < 0.05, "**",
    p_value < 0.10, "*",
    default = ""
  )
}


format_estimate <- function(
    estimate,
    standard_error,
    p_value
) {
  
  paste0(
    formatC(
      estimate,
      format = "f",
      digits = 3
    ),
    format_stars(p_value),
    " (",
    formatC(
      standard_error,
      format = "f",
      digits = 3
    ),
    ")"
  )
}


find_first_column <- function(
    data,
    candidate_names
) {
  
  matched_name <- intersect(
    candidate_names,
    names(data)
  )
  
  if (length(matched_name) == 0L) {
    return(NA_character_)
  }
  
  matched_name[1]
}


# ==============================================================================
# STEP 8: Detect Step 14 column names
# ==============================================================================

step14_outcome_column <- find_first_column(
  step14,
  c(
    "outcome",
    "outcome_name"
  )
)

step14_group_column <- find_first_column(
  step14,
  c(
    "subgroup",
    "analysis_group",
    "group"
  )
)

step14_effect_column <- find_first_column(
  step14,
  c(
    "effect_pp_for_10_percent_increase",
    "effect_pp",
    "estimate"
  )
)

step14_se_column <- find_first_column(
  step14,
  c(
    "standard_error_pp",
    "standard_error",
    "se"
  )
)

step14_p_column <- find_first_column(
  step14,
  c(
    "p_value",
    "p"
  )
)

step14_observation_column <- find_first_column(
  step14,
  c(
    "observations",
    "n",
    "N"
  )
)

required_step14_columns <- c(
  step14_outcome_column,
  step14_group_column,
  step14_effect_column,
  step14_se_column,
  step14_p_column
)

if (any(is.na(required_step14_columns))) {
  
  stop(
    paste0(
      "Could not identify all required Step 14 columns.\n\n",
      "Available columns:\n",
      paste(
        names(step14),
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# STEP 9: Create Table 1 — Main mismatch results
# ==============================================================================

conversion_factor <- 100 * log(1.10)

# Joint outcomes from Step 14
step14_main <- step14[
  grepl(
    "double|overeducated.*horizontal|fully matched|fully_matched",
    get(step14_outcome_column),
    ignore.case = TRUE
  ),
  .(
    outcome_original =
      as.character(get(step14_outcome_column)),
    
    analysis_group =
      as.character(get(step14_group_column)),
    
    effect =
      as.numeric(get(step14_effect_column)),
    
    standard_error =
      conversion_factor *
      as.numeric(get(step14_se_column)),
    
    p_value =
      as.numeric(get(step14_p_column)),
    
    observations =
      if (!is.na(step14_observation_column)) {
        as.numeric(get(step14_observation_column))
      } else {
        NA_real_
      }
  )
]

step14_main[
  ,
  outcome := fcase(
    grepl(
      "double|overeducated.*horizontal",
      outcome_original,
      ignore.case = TRUE
    ),
    "Double mismatch",
    
    grepl(
      "fully matched|fully_matched",
      outcome_original,
      ignore.case = TRUE
    ),
    "Fully matched employment",
    
    default = outcome_original
  )
]

# Horizontal mismatch from Step 19
horizontal_main <- step19[
  outcome == "Horizontal mismatch" &
    definition == "Broad horizontal",
  .(
    outcome = "Horizontal mismatch",
    analysis_group = subgroup,
    effect = effect_pp_for_10_percent_increase,
    standard_error = standard_error_pp,
    p_value,
    observations
  )
]

# Overeducation from Step 19
overeducation_main <- step19[
  outcome == "Overeducation" &
    definition == "Final preferred vertical",
  .(
    outcome = "Overeducation",
    analysis_group = subgroup,
    effect = effect_pp_for_10_percent_increase,
    standard_error = standard_error_pp,
    p_value,
    observations
  )
]

# Combine all central outcomes
table1_long <- rbindlist(
  list(
    overeducation_main,
    horizontal_main,
    step14_main[
      ,
      .(
        outcome,
        analysis_group,
        effect,
        standard_error,
        p_value,
        observations
      )
    ]
  ),
  fill = TRUE
)

# Harmonize group labels
table1_long[
  ,
  analysis_group := fcase(
    analysis_group %in% c("Pooled", "All", "Overall"),
    "Pooled",
    
    analysis_group %in% c("Women", "Female", "Females"),
    "Women",
    
    analysis_group %in% c("Men", "Male", "Males"),
    "Men",
    
    default = analysis_group
  )
]

table1_long <- table1_long[
  analysis_group %in% c(
    "Pooled",
    "Women",
    "Men"
  )
]

# Format estimates
table1_long[
  ,
  table_entry := format_estimate(
    effect,
    standard_error,
    p_value
  )
]

table1_long[
  ,
  outcome_order := fcase(
    outcome == "Overeducation", 1L,
    outcome == "Horizontal mismatch", 2L,
    outcome == "Double mismatch", 3L,
    outcome == "Fully matched employment", 4L,
    default = 99L
  )
]

setorder(
  table1_long,
  outcome_order,
  analysis_group
)

# Wide publication table
table1_wide <- dcast(
  table1_long,
  outcome + outcome_order ~ analysis_group,
  value.var = "table_entry"
)

setorder(
  table1_wide,
  outcome_order
)

table1_wide[
  ,
  outcome_order := NULL
]

# Save outputs
table1_path <- file.path(
  table_folder,
  "Table_1_Main_Mismatch_Results.csv"
)

table1_long_path <- file.path(
  table_folder,
  "Table_1_Main_Mismatch_Results_Long.csv"
)

fwrite(
  table1_wide,
  table1_path,
  bom = TRUE
)

fwrite(
  table1_long,
  table1_long_path,
  bom = TRUE
)

cat(
  "\nTable 1 created successfully:\n",
  table1_path,
  "\n"
)

print(table1_wide)

cat(
  "\nFile exists:",
  file.exists(table1_path),
  "\n"
)
# ==============================================================================
# STEP 10: Create Table 2 — Occupational destinations
# ==============================================================================

table2_long <- step17[
  outcome %in% c(
    "Professionals",
    "High-skill occupations",
    "Service and sales occupations",
    "Elementary occupations"
  ) &
    analysis_group %in% c(
      "Pooled",
      "Women",
      "Men"
    ),
  .(
    outcome,
    analysis_group,
    effect =
      effect_pp_for_10_percent_increase,
    standard_error =
      standard_error_pp,
    p_value,
    observations
  )
]

table2_long[
  ,
  table_entry := format_estimate(
    effect,
    standard_error,
    p_value
  )
]

table2_long[
  ,
  outcome_order := fcase(
    outcome == "Professionals", 1L,
    outcome == "High-skill occupations", 2L,
    outcome == "Service and sales occupations", 3L,
    outcome == "Elementary occupations", 4L,
    default = 99L
  )
]

setorder(
  table2_long,
  outcome_order,
  analysis_group
)

table2_wide <- dcast(
  table2_long,
  outcome + outcome_order ~ analysis_group,
  value.var = "table_entry"
)

setorder(
  table2_wide,
  outcome_order
)

table2_wide[
  ,
  outcome_order := NULL
]

fwrite(
  table2_wide,
  file.path(
    table_folder,
    "Table_2_Occupational_Destination_Results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Create Table 3 — Labor-demand mechanism
# ==============================================================================

table3_long <- step18[
  outcome %in% c(
    "Tertiary educated among employed workers",
    "High-skill occupation among employed workers",
    "Tertiary educated and in a high-skill occupation",
    "Tertiary educated and in a lower-skill occupation"
  ) &
    analysis_group %in% c(
      "Pooled",
      "Women",
      "Men"
    ),
  .(
    outcome,
    analysis_group,
    effect =
      effect_pp_for_10_percent_increase,
    standard_error =
      standard_error_pp,
    p_value,
    observations
  )
]

table3_long[
  ,
  table_entry := format_estimate(
    effect,
    standard_error,
    p_value
  )
]

table3_wide <- dcast(
  table3_long,
  outcome ~ analysis_group,
  value.var = "table_entry"
)

fwrite(
  table3_wide,
  file.path(
    table_folder,
    "Table_3_Labor_Demand_Mechanism.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Create Table 4 — Preferred and future exposure
# ==============================================================================

table4 <- step20D[
  subgroup == "Pooled",
  .(
    outcome,
    
    ages_18_20 =
      format_estimate(
        preferred_effect_pp_for_10_percent_increase,
        preferred_standard_error_pp,
        preferred_p_value
      ),
    
    ages_24_26 =
      format_estimate(
        future_effect_pp_for_10_percent_increase,
        future_standard_error_pp,
        future_p_value
      ),
    
    preferred_minus_future =
      format_estimate(
        difference_effect_pp,
        difference_standard_error_pp,
        difference_p_value
      ),
    
    observations
  )
]

fwrite(
  table4,
  file.path(
    table_folder,
    "Table_4_Joint_Preferred_and_Future_Exposure.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Create selected field-of-study table
# ==============================================================================

if (!is.null(step15)) {
  
  cat(
    "\n============================================================\n",
    "STEP 15 COLUMN NAMES\n",
    "============================================================\n"
  )
  
  print(names(step15))
  
  field_column <- find_first_column(
    step15,
    c(
      "field",
      "field_group",
      "education_field",
      "analysis_group"
    )
  )
  
  outcome_column <- find_first_column(
    step15,
    c(
      "outcome",
      "outcome_name"
    )
  )
  
  subgroup_column <- find_first_column(
    step15,
    c(
      "subgroup",
      "sex",
      "gender_group"
    )
  )
  
  effect_column <- find_first_column(
    step15,
    c(
      "effect_pp_for_10_percent_increase",
      "effect_pp",
      "estimate"
    )
  )
  
  se_column <- find_first_column(
    step15,
    c(
      "standard_error_pp",
      "standard_error",
      "se"
    )
  )
  
  p_column <- find_first_column(
    step15,
    c(
      "p_value",
      "p"
    )
  )
  
  if (
    all(
      !is.na(
        c(
          field_column,
          outcome_column,
          effect_column,
          se_column,
          p_column
        )
      )
    )
  ) {
    
    selected_field_results <- step15[
      grepl(
        "business|administration|law|engineering|ict|information",
        get(field_column),
        ignore.case = TRUE
      ) &
        grepl(
          "horizontal|double|fully matched",
          get(outcome_column),
          ignore.case = TRUE
        ),
      .(
        field =
          get(field_column),
        
        outcome =
          get(outcome_column),
        
        subgroup =
          if (!is.na(subgroup_column)) {
            get(subgroup_column)
          } else {
            "Pooled"
          },
        
        effect =
          get(effect_column),
        
        standard_error =
          get(se_column),
        
        p_value =
          get(p_column)
      )
    ]
    
    selected_field_results[
      ,
      table_entry := format_estimate(
        effect,
        standard_error,
        p_value
      )
    ]
    
    fwrite(
      selected_field_results,
      file.path(
        table_folder,
        "Table_5_Selected_Field_Heterogeneity.csv"
      ),
      bom = TRUE
    )
    
  } else {
    
    warning(
      paste0(
        "Step 15 was loaded, but required columns were not identified.\n",
        "The full Step 15 dataset will still be saved in the appendix."
      )
    )
  }
  
  fwrite(
    step15,
    file.path(
      appendix_folder,
      "Appendix_Table_A3_All_Field_Heterogeneity_Results.csv"
    ),
    bom = TRUE
  )
}


# ==============================================================================
# STEP 14: Appendix Table A1 — Alternative mismatch definitions
# ==============================================================================

appendix_A1 <- step19[
  subgroup == "Pooled" &
    outcome %in% c(
      "Overeducation",
      "Undereducation",
      "Horizontal mismatch",
      "Double mismatch",
      "Fully matched"
    ),
  .(
    outcome,
    definition,
    effect =
      effect_pp_for_10_percent_increase,
    standard_error =
      standard_error_pp,
    p_value,
    observations
  )
]

appendix_A1[
  ,
  table_entry := format_estimate(
    effect,
    standard_error,
    p_value
  )
]

appendix_A1_wide <- dcast(
  appendix_A1,
  definition ~ outcome,
  value.var = "table_entry"
)

fwrite(
  appendix_A1_wide,
  file.path(
    appendix_folder,
    "Appendix_Table_A1_Alternative_Mismatch_Definitions.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Appendix Table A2 — Exposure timing
# ==============================================================================

appendix_A2 <- step20C[
  sample_type == "Common four-window sample" &
    subgroup == "Pooled" &
    outcome %in% c(
      "Horizontal mismatch",
      "Double mismatch",
      "Fully matched",
      "High-skill occupation among tertiary-employed workers",
      "Tertiary-educated lower-skill employment"
    ),
  .(
    outcome,
    exposure_window,
    timing_order,
    effect =
      effect_pp_for_10_percent_increase,
    standard_error =
      standard_error_pp,
    p_value,
    observations
  )
]

appendix_A2[
  ,
  table_entry := format_estimate(
    effect,
    standard_error,
    p_value
  )
]

appendix_A2_wide <- dcast(
  appendix_A2,
  exposure_window + timing_order ~ outcome,
  value.var = "table_entry"
)

setorder(
  appendix_A2_wide,
  timing_order
)

fwrite(
  appendix_A2_wide,
  file.path(
    appendix_folder,
    "Appendix_Table_A2_Exposure_Timing.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Publication figure theme
# ==============================================================================

paper_theme <- theme_classic(
  base_size = 12
) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.background = element_rect(
      fill = "white",
      color = NA
    ),
    legend.key = element_rect(
      fill = "white",
      color = NA
    ),
    axis.title = element_text(
      color = "black"
    ),
    axis.text = element_text(
      color = "black"
    ),
    strip.text = element_text(
      color = "black",
      face = "bold"
    ),
    strip.background = element_rect(
      fill = "white",
      color = "black"
    ),
    legend.text = element_text(
      color = "black"
    )
  )


# ==============================================================================
# STEP 17: Figure 1 — Occupational destinations
# ==============================================================================

figure1_data <- step17[
  outcome %in% c(
    "Professionals",
    "High-skill occupations",
    "Service and sales occupations",
    "Elementary occupations"
  ) &
    analysis_group %in% c(
      "Pooled",
      "Women",
      "Men"
    ),
  .(
    outcome,
    analysis_group,
    estimate =
      effect_pp_for_10_percent_increase,
    lower =
      lower_95_pp,
    upper =
      upper_95_pp
  )
]

figure1_data[
  ,
  outcome := factor(
    outcome,
    levels = rev(
      c(
        "Professionals",
        "High-skill occupations",
        "Service and sales occupations",
        "Elementary occupations"
      )
    )
  )
]

figure1_data[
  ,
  analysis_group := factor(
    analysis_group,
    levels = c(
      "Pooled",
      "Women",
      "Men"
    )
  )
]

figure1 <- ggplot(
  figure1_data,
  aes(
    x = estimate,
    y = outcome,
    shape = analysis_group
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(
      xmin = lower,
      xmax = upper
    ),
    orientation = "y",
    width = 0.12,
    position = position_dodge(
      width = 0.5
    )
  ) +
  geom_point(
    size = 2.8,
    position = position_dodge(
      width = 0.5
    )
  ) +
  labs(
    x = "Percentage-point effect of a 10% increase in exposure",
    y = NULL,
    shape = NULL
  ) +
  paper_theme +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_1_Occupational_Destination_Effects.pdf"
  ),
  plot = figure1,
  width = 8,
  height = 5.2,
  bg = "white"
)

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_1_Occupational_Destination_Effects.png"
  ),
  plot = figure1,
  width = 8,
  height = 5.2,
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# STEP 18: Figure 2 — Exposure timing
# ==============================================================================

figure2_data <- step20C[
  sample_type == "Common four-window sample" &
    subgroup == "Pooled" &
    outcome %in% c(
      "Double mismatch",
      "High-skill occupation among tertiary-employed workers",
      "Tertiary-educated lower-skill employment"
    ),
  .(
    outcome,
    exposure_window,
    estimate =
      effect_pp_for_10_percent_increase,
    lower =
      lower_95_pp,
    upper =
      upper_95_pp
  )
]

figure2_data[
  ,
  exposure_window := factor(
    exposure_window,
    levels = c(
      "Ages 15–17",
      "Ages 18–20",
      "Ages 21–23",
      "Ages 24–26"
    )
  )
]

figure2_data[
  ,
  outcome := fcase(
    outcome == "Double mismatch",
    "Double mismatch",
    
    outcome ==
      "High-skill occupation among tertiary-employed workers",
    "High-skill occupation among graduates",
    
    outcome ==
      "Tertiary-educated lower-skill employment",
    "Tertiary-educated lower-skill employment"
  )
]

figure2 <- ggplot(
  figure2_data,
  aes(
    x = exposure_window,
    y = estimate,
    group = 1
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(
      ymin = lower,
      ymax = upper
    ),
    width = 0.10
  ) +
  geom_line(
    linewidth = 0.7
  ) +
  geom_point(
    size = 2.5
  ) +
  facet_wrap(
    ~outcome,
    scales = "free_y",
    ncol = 3
  ) +
  labs(
    x = "Exposure window",
    y = "Percentage-point effect of a 10% increase in exposure"
  ) +
  paper_theme +
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1
    )
  )

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_2_Exposure_Timing_Effects.pdf"
  ),
  plot = figure2,
  width = 11,
  height = 4.8,
  bg = "white"
)

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_2_Exposure_Timing_Effects.png"
  ),
  plot = figure2,
  width = 11,
  height = 4.8,
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# STEP 19: Figure 3 — Graduate supply and high-skill employment
# ==============================================================================

national_long <- melt(
  national_series,
  id.vars = "year",
  measure.vars = c(
    "tertiary_employed_share",
    "high_skill_job_share"
  ),
  variable.name = "series",
  value.name = "share"
)

national_long[
  ,
  series := fcase(
    series == "tertiary_employed_share",
    "Tertiary-educated employment share",
    
    series == "high_skill_job_share",
    "High-skill employment share"
  )
]

figure3 <- ggplot(
  national_long,
  aes(
    x = year,
    y = 100 * share,
    linetype = series,
    shape = series
  )
) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    x = "Survey year",
    y = "Percent of employed workers",
    linetype = NULL,
    shape = NULL
  ) +
  paper_theme +
  theme(
    legend.position = "bottom"
  )

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_3_Graduate_Supply_and_High_Skill_Jobs.pdf"
  ),
  plot = figure3,
  width = 8,
  height = 5.2,
  bg = "white"
)

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_3_Graduate_Supply_and_High_Skill_Jobs.png"
  ),
  plot = figure3,
  width = 8,
  height = 5.2,
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# STEP 20: Figure 4 — Graduate-pressure ratio
# ==============================================================================

figure4 <- ggplot(
  national_series,
  aes(
    x = year,
    y = graduate_pressure_ratio
  )
) +
  geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_line(
    linewidth = 0.9
  ) +
  geom_point(
    size = 2
  ) +
  labs(
    x = "Survey year",
    y = "Tertiary-educated workers per high-skill job"
  ) +
  paper_theme

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_4_Graduate_Pressure_Ratio.pdf"
  ),
  plot = figure4,
  width = 8,
  height = 5.2,
  bg = "white"
)

ggsave(
  filename = file.path(
    figure_folder,
    "Figure_4_Graduate_Pressure_Ratio.png"
  ),
  plot = figure4,
  width = 8,
  height = 5.2,
  dpi = 300,
  bg = "white"
)


# ==============================================================================
# STEP 21: Create empirical-results index
# ==============================================================================

results_index <- data.table(
  display_order = 1:10,
  
  empirical_component = c(
    "Employment quantity",
    "Separate vertical mismatch",
    "Horizontal mismatch",
    "Joint mismatch",
    "Gender heterogeneity",
    "Field-of-study heterogeneity",
    "Occupational destinations",
    "Labor-demand mechanism",
    "Alternative mismatch definitions",
    "Timing and future exposure"
  ),
  
  recommended_location = c(
    "Brief main text or appendix",
    "Main text",
    "Main text",
    "Main text",
    "Main text",
    "Main text and appendix",
    "Main text",
    "Supporting mechanism section",
    "Robustness appendix",
    "Main text plus appendix"
  ),
  
  central_message = c(
    "University expansion did not materially change employment quantity.",
    "Overeducation rises while undereducation weakly falls.",
    "Horizontal mismatch rises, especially among men.",
    "Double mismatch rises and fully matched employment falls.",
    "Mismatch and occupational downgrading effects are concentrated among men.",
    "Business and law drive central field effects; engineering and ICT support the mechanism.",
    "Graduates move away from professional and high-skill occupations toward lower-skill work.",
    "Educated men increasingly enter lower-skill occupations.",
    "Central findings survive alternative vertical and horizontal mismatch definitions.",
    "Ages 18–20 exposure remains robust; occupational future-exposure results are comparatively clean."
  )
)

fwrite(
  results_index,
  file.path(
    output_folder,
    "step21_master_empirical_results_index.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Create output plan
# ==============================================================================

output_plan <- data.table(
  item_order = 1:10,
  
  item_type = c(
    "Table",
    "Table",
    "Table",
    "Table",
    "Table",
    "Figure",
    "Figure",
    "Figure",
    "Figure",
    "Appendix"
  ),
  
  item_name = c(
    "Main mismatch results",
    "Occupational destination results",
    "Labor-demand mechanism",
    "Joint preferred and future exposure",
    "Selected field heterogeneity",
    "Occupational destination effects",
    "Exposure timing effects",
    "Graduate supply and high-skill employment",
    "Graduate-pressure ratio",
    "Alternative definitions and full field results"
  ),
  
  recommended_location = c(
    "Main results",
    "Mechanisms",
    "Mechanisms or appendix",
    "Timing and robustness",
    "Field heterogeneity",
    "Mechanisms",
    "Timing and robustness",
    "Institutional context or mechanisms",
    "Mechanisms",
    "Appendix"
  )
)

fwrite(
  output_plan,
  file.path(
    output_folder,
    "step21_main_output_plan.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Validate created files
# ==============================================================================

expected_main_files <- c(
  file.path(
    table_folder,
    "Table_1_Main_Mismatch_Results.csv"
  ),
  file.path(
    table_folder,
    "Table_2_Occupational_Destination_Results.csv"
  ),
  file.path(
    table_folder,
    "Table_3_Labor_Demand_Mechanism.csv"
  ),
  file.path(
    table_folder,
    "Table_4_Joint_Preferred_and_Future_Exposure.csv"
  ),
  file.path(
    figure_folder,
    "Figure_1_Occupational_Destination_Effects.pdf"
  ),
  file.path(
    figure_folder,
    "Figure_2_Exposure_Timing_Effects.pdf"
  ),
  file.path(
    figure_folder,
    "Figure_3_Graduate_Supply_and_High_Skill_Jobs.pdf"
  ),
  file.path(
    figure_folder,
    "Figure_4_Graduate_Pressure_Ratio.pdf"
  )
)

output_validation <- data.table(
  file_path = expected_main_files,
  file_name = basename(expected_main_files),
  exists = file.exists(expected_main_files),
  file_size_kb = fifelse(
    file.exists(expected_main_files),
    round(
      file.info(expected_main_files)$size /
        1024,
      2
    ),
    NA_real_
  )
)

print(output_validation)

fwrite(
  output_validation,
  file.path(
    output_folder,
    "step21_output_validation.csv"
  ),
  bom = TRUE
)

if (any(output_validation$exists == FALSE)) {
  
  warning(
    paste0(
      "Some expected Step 21 outputs were not created:\n",
      paste(
        output_validation[
          exists == FALSE,
          file_name
        ],
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# STEP 24: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 21 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain output folder:\n",
  output_folder,
  "\n"
)

cat(
  "\nTables folder:\n",
  table_folder,
  "\n"
)

cat(
  "\nFigures folder:\n",
  figure_folder,
  "\n"
)

cat(
  "\nAppendix folder:\n",
  appendix_folder,
  "\n"
)

cat(
  "\nMost important outputs:\n",
  "1. Table_1_Main_Mismatch_Results.csv\n",
  "2. Table_2_Occupational_Destination_Results.csv\n",
  "3. Table_3_Labor_Demand_Mechanism.csv\n",
  "4. Table_4_Joint_Preferred_and_Future_Exposure.csv\n",
  "5. Table_5_Selected_Field_Heterogeneity.csv, if Step 15 was located\n",
  "6. Figure_1_Occupational_Destination_Effects.pdf\n",
  "7. Figure_2_Exposure_Timing_Effects.pdf\n",
  "8. Figure_3_Graduate_Supply_and_High_Skill_Jobs.pdf\n",
  "9. Figure_4_Graduate_Pressure_Ratio.pdf\n",
  "10. step21_output_validation.csv\n"
)