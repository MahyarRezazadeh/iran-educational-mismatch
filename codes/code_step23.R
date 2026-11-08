# ==============================================================================
# PROJECT: University Expansion and Occupational Reallocation in Iran
#
# PURPOSE:
#   Estimate the effect of university exposure on two-digit ISCO occupational
#   destinations among tertiary-educated employed workers.
#
# IMPORTANT FEATURES:
#   - Preserves genuine missing occupation codes
#   - Excludes armed-forces codes 00 and 01
#   - Uses the preferred log exposure specification
#   - Includes province, birth-cohort, and survey-year fixed effects
#   - Uses survey weights
#   - Two-way clusters standard errors by province and birth cohort
#   - Estimates pooled, women, and men models
#   - Produces complete and selected occupation results
#   - Produces a coefficient figure for selected occupations
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
library(fixest)
library(fst)
library(ggplot2)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

data_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step23_isco2_occupational_reallocation_corrected"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(data_file)) {
  stop(
    paste0(
      "The data file was not found:\n",
      data_file
    )
  )
}


# ==============================================================================
# STEP 2: Inspect available variables
# ==============================================================================

fst_information <- metadata_fst(data_file)

available_variables <- fst_information$columnNames

required_variables <- c(
  "isco_2digit",
  "tertiary_educated",
  "employed",
  "log_undergrad_exposure_18_20",
  "province_historical",
  "analysis_birth_year",
  "year",
  "gender",
  "urban_rural",
  "weight"
)

missing_variables <- setdiff(
  required_variables,
  available_variables
)

if (length(missing_variables) > 0L) {
  stop(
    paste0(
      "The following required variables are missing:\n",
      paste(
        missing_variables,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# STEP 3: Read required variables
# ==============================================================================

dt <- as.data.table(
  read_fst(
    data_file,
    columns = required_variables
  )
)

cat(
  "\nRows read:",
  format(
    nrow(dt),
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 4: Construct gender indicator
# ==============================================================================

# Confirmed coding:
# gender = 1: women
# gender = 2: men

cat(
  "\n============================================================\n",
  "GENDER CODING\n",
  "============================================================\n"
)

print(
  dt[
    ,
    .N,
    by = gender
  ][
    order(gender)
  ]
)

dt[
  ,
  female := fcase(
    gender == 1, 1L,
    gender == 2, 0L,
    default = NA_integer_
  )
]

cat("\nConstructed female indicator:\n")

print(
  dt[
    ,
    .N,
    by = .(
      gender,
      female
    )
  ][
    order(gender)
  ]
)


# ==============================================================================
# STEP 5: Construct urban indicator
# ==============================================================================

cat(
  "\n============================================================\n",
  "URBAN/RURAL CODING\n",
  "============================================================\n"
)

print(
  dt[
    ,
    .N,
    by = urban_rural
  ][
    order(urban_rural)
  ]
)

urban_text <- tolower(
  trimws(
    as.character(
      dt$urban_rural
    )
  )
)

dt[
  ,
  urban := fcase(
    urban_text == "urban", 1L,
    urban_text == "rural", 0L,
    default = NA_integer_
  )
]

cat("\nConstructed urban indicator:\n")

print(
  dt[
    ,
    .N,
    by = .(
      urban_rural,
      urban
    )
  ][
    order(urban_rural)
  ]
)


# ==============================================================================
# STEP 6: Correctly standardize two-digit ISCO codes
# ==============================================================================

# IMPORTANT:
# Do not apply sprintf() directly to a character variable containing missing
# values. That can turn genuine missing values into the character string "NA".

dt[
  ,
  isco_2digit_numeric :=
    suppressWarnings(
      as.integer(
        trimws(
          as.character(
            isco_2digit
          )
        )
      )
    )
]

dt[
  ,
  isco_2digit_clean := fifelse(
    is.na(isco_2digit_numeric),
    NA_character_,
    sprintf(
      "%02d",
      isco_2digit_numeric
    )
  )
]

cat(
  "\n============================================================\n",
  "CLEANED TWO-DIGIT ISCO CODES\n",
  "============================================================\n"
)

print(
  dt[
    !is.na(isco_2digit_clean),
    .N,
    by = isco_2digit_clean
  ][
    order(isco_2digit_clean)
  ],
  nrows = Inf
)

cat(
  "\nOriginal missing occupation codes:",
  dt[is.na(isco_2digit), .N],
  "\n"
)

cat(
  "Cleaned missing occupation codes:",
  dt[is.na(isco_2digit_clean), .N],
  "\n"
)


# ==============================================================================
# STEP 7: Create ISCO-08 two-digit crosswalk
# ==============================================================================

isco_crosswalk <- data.table(
  isco_2digit_clean = c(
    "11", "12", "13", "14",
    "21", "22", "23", "24", "25", "26",
    "31", "32", "33", "34", "35",
    "41", "42", "43", "44",
    "51", "52", "53", "54",
    "61", "62", "63",
    "71", "72", "73", "74", "75",
    "81", "82", "83",
    "91", "92", "93", "94", "95", "96"
  ),
  
  occupation_label = c(
    "Chief executives, senior officials, and legislators",
    "Administrative and commercial managers",
    "Production and specialized-services managers",
    "Hospitality, retail, and other-services managers",
    
    "Science and engineering professionals",
    "Health professionals",
    "Teaching professionals",
    "Business and administration professionals",
    "ICT professionals",
    "Legal, social, and cultural professionals",
    
    "Science and engineering associate professionals",
    "Health associate professionals",
    "Business and administration associate professionals",
    "Legal, social, cultural, and related associate professionals",
    "Information and communications technicians",
    
    "General and keyboard clerks",
    "Customer-services clerks",
    "Numerical and material-recording clerks",
    "Other clerical-support workers",
    
    "Personal-service workers",
    "Sales workers",
    "Personal-care workers",
    "Protective-services workers",
    
    "Market-oriented skilled agricultural workers",
    "Skilled forestry, fishery, and hunting workers",
    "Subsistence farmers, fishers, hunters, and gatherers",
    
    "Building and related-trades workers",
    "Metal, machinery, and related-trades workers",
    "Handicraft and printing workers",
    "Electrical and electronic-trades workers",
    "Food-processing, woodworking, garment, and related workers",
    
    "Stationary plant and machine operators",
    "Assemblers",
    "Drivers and mobile-plant operators",
    
    "Cleaners and helpers",
    "Agricultural, forestry, and fishery laborers",
    "Laborers in mining, construction, manufacturing, and transport",
    "Food-preparation assistants",
    "Street and related sales and service workers",
    "Refuse workers and other elementary workers"
  )
)


# ==============================================================================
# STEP 8: Construct the analysis sample
# ==============================================================================

analysis_dt <- dt[
  tertiary_educated == 1 &
    employed == 1 &
    !is.na(isco_2digit_clean) &
    !isco_2digit_clean %in% c("00", "01") &
    !is.na(log_undergrad_exposure_18_20) &
    is.finite(log_undergrad_exposure_18_20) &
    !is.na(province_historical) &
    !is.na(analysis_birth_year) &
    analysis_birth_year >= 1355 &
    analysis_birth_year <= 1383 &
    !is.na(year) &
    !is.na(female) &
    !is.na(urban) &
    !is.na(weight) &
    is.finite(weight) &
    weight > 0
]

cat(
  "\n============================================================\n",
  "ANALYSIS SAMPLE\n",
  "============================================================\n"
)

cat(
  "\nAnalysis observations:",
  format(
    nrow(analysis_dt),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Historical provinces:",
  uniqueN(
    analysis_dt$province_historical
  ),
  "\n"
)

cat(
  "Birth cohorts:",
  uniqueN(
    analysis_dt$analysis_birth_year
  ),
  "\n"
)

cat(
  "Survey years:",
  uniqueN(
    analysis_dt$year
  ),
  "\n"
)

cat(
  "Missing occupation codes retained:",
  analysis_dt[
    is.na(isco_2digit_clean) |
      isco_2digit_clean == "NA",
    .N
  ],
  "\n"
)

if (
  analysis_dt[
    is.na(isco_2digit_clean) |
    isco_2digit_clean == "NA",
    .N
  ] > 0L
) {
  stop(
    "Missing occupation codes remain in the analysis sample."
  )
}

cat("\nSample by gender:\n")

print(
  analysis_dt[
    ,
    .N,
    by = .(
      subgroup = fifelse(
        female == 1,
        "Women",
        "Men"
      )
    )
  ]
)


# ==============================================================================
# STEP 9: Check occupation labels
# ==============================================================================

occupation_codes_in_sample <- sort(
  unique(
    analysis_dt$isco_2digit_clean
  )
)

unmatched_codes <- setdiff(
  occupation_codes_in_sample,
  isco_crosswalk$isco_2digit_clean
)

if (length(unmatched_codes) > 0L) {
  
  warning(
    paste0(
      "The following occupation codes are not in the crosswalk:\n",
      paste(
        unmatched_codes,
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# STEP 10: Create occupation descriptives
# ==============================================================================

total_analysis_weight <- analysis_dt[
  ,
  sum(weight)
]

occupation_descriptives <- analysis_dt[
  ,
  .(
    observations = .N,
    weighted_workers = sum(weight),
    weighted_share =
      sum(weight) /
      total_analysis_weight
  ),
  by = isco_2digit_clean
]

occupation_descriptives <- merge(
  occupation_descriptives,
  isco_crosswalk,
  by = "isco_2digit_clean",
  all.x = TRUE
)

gender_occupation_counts <- analysis_dt[
  ,
  .(
    subgroup_observations = .N,
    subgroup_weighted_workers =
      sum(weight)
  ),
  by = .(
    isco_2digit_clean,
    subgroup = fifelse(
      female == 1,
      "Women",
      "Men"
    )
  )
]

gender_counts_wide <- dcast(
  gender_occupation_counts,
  isco_2digit_clean ~ subgroup,
  value.var = "subgroup_observations",
  fill = 0
)

setnames(
  gender_counts_wide,
  old = c(
    "Women",
    "Men"
  ),
  new = c(
    "women_observations",
    "men_observations"
  ),
  skip_absent = TRUE
)

occupation_descriptives <- merge(
  occupation_descriptives,
  gender_counts_wide,
  by = "isco_2digit_clean",
  all.x = TRUE
)

setorder(
  occupation_descriptives,
  -weighted_share
)

cat(
  "\n============================================================\n",
  "OCCUPATION DESCRIPTIVES\n",
  "============================================================\n"
)

print(
  occupation_descriptives,
  nrows = Inf
)


# ==============================================================================
# STEP 11: Define helper functions
# ==============================================================================

conversion_factor <- 100 * log(1.10)

significance_stars <- function(p_value) {
  
  fcase(
    is.na(p_value), "",
    p_value < 0.01, "***",
    p_value < 0.05, "**",
    p_value < 0.10, "*",
    default = ""
  )
}


estimate_occupation_model <- function(
    data,
    occupation_code,
    subgroup
) {
  
  estimation_data <- copy(data)
  
  estimation_data[
    ,
    occupation_outcome :=
      as.integer(
        isco_2digit_clean ==
          occupation_code
      )
  ]
  
  if (subgroup == "Pooled") {
    
    model_formula <- (
      occupation_outcome ~
        log_undergrad_exposure_18_20 +
        female +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Women") {
    
    estimation_data <- estimation_data[
      female == 1
    ]
    
    model_formula <- (
      occupation_outcome ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Men") {
    
    estimation_data <- estimation_data[
      female == 0
    ]
    
    model_formula <- (
      occupation_outcome ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else {
    
    stop(
      paste0(
        "Unknown subgroup: ",
        subgroup
      )
    )
  }
  
  model <- feols(
    model_formula,
    data = estimation_data,
    weights = ~weight,
    cluster =
      ~province_historical +
      analysis_birth_year,
    warn = TRUE,
    notes = FALSE
  )
  
  treatment_name <-
    "log_undergrad_exposure_18_20"
  
  coefficient <-
    unname(
      coef(model)[
        treatment_name
      ]
    )
  
  standard_error <-
    unname(
      se(model)[
        treatment_name
      ]
    )
  
  p_value <-
    unname(
      pvalue(model)[
        treatment_name
      ]
    )
  
  effect_pp <-
    conversion_factor *
    coefficient
  
  standard_error_pp <-
    conversion_factor *
    standard_error
  
  data.table(
    isco_2digit_clean =
      occupation_code,
    
    subgroup =
      subgroup,
    
    coefficient_log_exposure =
      coefficient,
    
    standard_error_log_exposure =
      standard_error,
    
    p_value =
      p_value,
    
    effect_pp_10_percent =
      effect_pp,
    
    standard_error_pp_10_percent =
      standard_error_pp,
    
    lower_95 =
      effect_pp -
      1.96 *
      standard_error_pp,
    
    upper_95 =
      effect_pp +
      1.96 *
      standard_error_pp,
    
    significance =
      significance_stars(
        p_value
      ),
    
    model_observations =
      nobs(model)
  )
}


# ==============================================================================
# STEP 12: Estimate all two-digit occupation models
# ==============================================================================

occupation_codes <- sort(
  unique(
    analysis_dt$isco_2digit_clean
  )
)

subgroups <- c(
  "Pooled",
  "Women",
  "Men"
)

all_results <- rbindlist(
  lapply(
    occupation_codes,
    function(current_occupation) {
      
      cat(
        "\nEstimating occupation:",
        current_occupation,
        "\n"
      )
      
      rbindlist(
        lapply(
          subgroups,
          function(current_subgroup) {
            
            cat(
              "  Subgroup:",
              current_subgroup,
              "\n"
            )
            
            estimate_occupation_model(
              data = analysis_dt,
              occupation_code =
                current_occupation,
              subgroup =
                current_subgroup
            )
          }
        )
      )
    }
  )
)


# ==============================================================================
# STEP 13: Merge labels and descriptive statistics
# ==============================================================================

all_results <- merge(
  all_results,
  isco_crosswalk,
  by = "isco_2digit_clean",
  all.x = TRUE
)

all_results <- merge(
  all_results,
  occupation_descriptives[
    ,
    .(
      isco_2digit_clean,
      total_occupation_observations =
        observations,
      weighted_workers,
      weighted_share,
      women_occupation_observations =
        women_observations,
      men_occupation_observations =
        men_observations
    )
  ],
  by = "isco_2digit_clean",
  all.x = TRUE
)

all_results[
  ,
  formatted_result := paste0(
    formatC(
      effect_pp_10_percent,
      format = "f",
      digits = 3
    ),
    significance,
    " (",
    formatC(
      standard_error_pp_10_percent,
      format = "f",
      digits = 3
    ),
    ")"
  )
]


# ==============================================================================
# STEP 14: Apply transparent main-text selection rule
# ==============================================================================

# Main selection rule:
#   1. Weighted share of at least 1% among tertiary-employed workers
#   2. At least 5,000 total occupation observations
#
# The complete estimates for all occupations remain in the appendix output.

selected_occupation_codes <- occupation_descriptives[
  weighted_share >= 0.01 &
    observations >= 5000,
  isco_2digit_clean
]

main_results <- all_results[
  isco_2digit_clean %in%
    selected_occupation_codes
]

main_results[
  ,
  sufficient_subgroup_sample := fcase(
    subgroup == "Pooled",
    TRUE,
    
    subgroup == "Women",
    women_occupation_observations >= 500,
    
    subgroup == "Men",
    men_occupation_observations >= 500,
    
    default = FALSE
  )
]

main_results[
  ,
  subgroup_order := fcase(
    subgroup == "Pooled", 1L,
    subgroup == "Women", 2L,
    subgroup == "Men", 3L,
    default = 99L
  )
]

setorder(
  main_results,
  -weighted_share,
  subgroup_order
)

setorder(
  all_results,
  isco_2digit_clean,
  subgroup
)


# ==============================================================================
# STEP 15: Create wide main-results table
# ==============================================================================

main_table_wide <- dcast(
  main_results,
  isco_2digit_clean +
    occupation_label +
    weighted_share ~
    subgroup,
  value.var = "formatted_result"
)

setorder(
  main_table_wide,
  -weighted_share
)

setcolorder(
  main_table_wide,
  c(
    "isco_2digit_clean",
    "occupation_label",
    "weighted_share",
    "Pooled",
    "Women",
    "Men"
  )
)


# ==============================================================================
# STEP 16: Create a manually focused figure sample
# ==============================================================================

# These occupations are substantively useful for showing both:
#   - higher-skill destinations
#   - lower-skill destinations
#
# This list is chosen before interpreting statistical significance.

figure_occupation_codes <- c(
  "21", # Science and engineering professionals
  "22", # Health professionals
  "23", # Teaching professionals
  "24", # Business and administration professionals
  "31", # Science and engineering associate professionals
  "33", # Business and administration associate professionals
  "41", # General and keyboard clerks
  "51", # Personal-service workers
  "52", # Sales workers
  "72", # Metal and machinery trades
  "75", # Food, garment, and related craft workers
  "83", # Drivers and mobile-plant operators
  "93"  # Laborers in mining, construction, manufacturing, and transport
)

figure_results <- all_results[
  isco_2digit_clean %in%
    figure_occupation_codes
]

figure_results[
  ,
  occupation_label := factor(
    occupation_label,
    levels = rev(
      isco_crosswalk[
        isco_2digit_clean %in%
          figure_occupation_codes,
        occupation_label
      ]
    )
  )
]


# ==============================================================================
# STEP 17: Produce coefficient figure
# ==============================================================================

coefficient_figure <- ggplot(
  figure_results,
  aes(
    x = effect_pp_10_percent,
    y = occupation_label,
    shape = subgroup
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  geom_errorbarh(
    aes(
      xmin = lower_95,
      xmax = upper_95
    ),
    height = 0.15,
    position = position_dodge(
      width = 0.55
    )
  ) +
  geom_point(
    size = 2.3,
    position = position_dodge(
      width = 0.55
    )
  ) +
  labs(
    title =
      "University exposure and two-digit occupational destinations",
    subtitle =
      "Tertiary-educated employed workers",
    x =
      "Percentage-point effect of a 10% increase in exposure",
    y =
      NULL,
    shape =
      NULL
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(coefficient_figure)


# ==============================================================================
# STEP 18: Save outputs
# ==============================================================================

descriptive_file <- file.path(
  output_folder,
  "isco2_occupation_descriptives_corrected.csv"
)

all_results_file <- file.path(
  output_folder,
  "isco2_all_regression_results_corrected.csv"
)

main_results_file <- file.path(
  output_folder,
  "isco2_main_selected_results_corrected.csv"
)

main_table_file <- file.path(
  output_folder,
  "isco2_main_selected_table_corrected.csv"
)

figure_results_file <- file.path(
  output_folder,
  "isco2_figure_results_corrected.csv"
)

figure_file <- file.path(
  output_folder,
  "isco2_occupational_reallocation_figure.png"
)

fwrite(
  occupation_descriptives,
  descriptive_file,
  bom = TRUE
)

fwrite(
  all_results,
  all_results_file,
  bom = TRUE
)

fwrite(
  main_results,
  main_results_file,
  bom = TRUE
)

fwrite(
  main_table_wide,
  main_table_file,
  bom = TRUE
)

fwrite(
  figure_results,
  figure_results_file,
  bom = TRUE
)

ggsave(
  filename = figure_file,
  plot = coefficient_figure,
  width = 10,
  height = 8,
  dpi = 300
)


# ==============================================================================
# STEP 19: Validate saved files
# ==============================================================================

output_validation <- data.table(
  file = c(
    descriptive_file,
    all_results_file,
    main_results_file,
    main_table_file,
    figure_results_file,
    figure_file
  )
)

output_validation[
  ,
  exists := file.exists(file)
]

cat(
  "\n============================================================\n",
  "OUTPUT VALIDATION\n",
  "============================================================\n"
)

print(
  output_validation
)

if (any(output_validation$exists == FALSE)) {
  stop(
    "One or more output files were not created."
  )
}


# ==============================================================================
# STEP 20: Print final selected results
# ==============================================================================

cat(
  "\n============================================================\n",
  "SELECTED TWO-DIGIT OCCUPATION RESULTS\n",
  "============================================================\n"
)

print(
  main_table_wide,
  nrows = Inf
)

cat(
  "\n============================================================\n",
  "ANALYSIS COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nOutput folder:\n",
  output_folder,
  "\n"
)
