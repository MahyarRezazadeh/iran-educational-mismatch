# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 11: Final Mismatch Results and Gender Heterogeneity
#
# Purpose:
#   1. Inspect specification-level robustness
#   2. Identify sign reversals and unstable specifications
#   3. Produce a final compact mismatch table
#   4. Test female–male differences using gender-specific fixed effects
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Interpretation:
#   Reported effects are percentage-point changes associated with a
#   10 percent increase in university exposure.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 300
)

required_packages <- c(
  "data.table",
  "fst",
  "fixest"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0L) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(fixest)


# ==============================================================================
# STEP 1: Define paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

step10_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step10_mismatch_analysis"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step11_final_mismatch_results"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

step10_results_file <- file.path(
  step10_folder,
  "step10_mismatch_all_results.csv"
)

step10_descriptives_file <- file.path(
  step10_folder,
  "step10_mismatch_weighted_descriptives.csv"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Final analysis dataset not found:\n",
      input_file
    )
  )
}

if (!file.exists(step10_results_file)) {
  stop(
    paste0(
      "Step 10 results file not found:\n",
      step10_results_file
    )
  )
}


# ==============================================================================
# STEP 2: Read existing Step 10 results
# ==============================================================================

results <- fread(
  step10_results_file,
  encoding = "UTF-8"
)

descriptives <- fread(
  step10_descriptives_file,
  encoding = "UTF-8"
)

cat(
  "\nStep 10 result rows:",
  nrow(results),
  "\n"
)

cat(
  "Descriptive result rows:",
  nrow(descriptives),
  "\n"
)


# ==============================================================================
# STEP 3: Standardize labels
# ==============================================================================

results[
  ,
  subgroup := fifelse(
    subgroup == "women",
    "Women",
    fifelse(
      subgroup == "men",
      "Men",
      "Pooled"
    )
  )
]

descriptives[
  ,
  subgroup := fifelse(
    tolower(subgroup) == "women",
    "Women",
    fifelse(
      tolower(subgroup) == "men",
      "Men",
      "Pooled"
    )
  )
]


# ==============================================================================
# STEP 4: Inspect all log-exposure specifications
# ==============================================================================

log_results <- results[
  treatment == "log_undergrad_exposure"
]

log_results_compact <- log_results[
  ,
  .(
    outcome,
    subgroup,
    specification,
    
    coefficient =
      round(coefficient, 6),
    
    effect_pp_10pct =
      round(
        effect_pp_for_10_percent_increase,
        4
      ),
    
    standard_error =
      round(standard_error, 6),
    
    p_value =
      round(p_value, 4),
    
    observations
  )
]

setorder(
  log_results_compact,
  outcome,
  subgroup,
  specification
)

fwrite(
  log_results_compact,
  file.path(
    output_folder,
    "step11_all_log_specifications_compact.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 5: Identify sign reversals relative to the main specification
# ==============================================================================

main_signs <- log_results[
  specification == "Main log exposure",
  .(
    outcome,
    subgroup,
    main_coefficient = coefficient,
    main_sign = sign(coefficient)
  )
]

sign_reversal_check <- merge(
  log_results,
  main_signs,
  by = c(
    "outcome",
    "subgroup"
  ),
  all.x = TRUE
)

sign_reversal_check[
  ,
  sign_reversal :=
    as.integer(
      sign(coefficient) != main_sign &
        coefficient != 0 &
        main_coefficient != 0
    )
]

sign_reversals <- sign_reversal_check[
  sign_reversal == 1L,
  .(
    outcome,
    subgroup,
    specification,
    
    main_effect_pp_10pct =
      100 *
      main_coefficient *
      log(1.10),
    
    robustness_effect_pp_10pct =
      effect_pp_for_10_percent_increase,
    
    p_value
  )
]

setorder(
  sign_reversals,
  outcome,
  subgroup,
  specification
)

print(sign_reversals)

fwrite(
  sign_reversals,
  file.path(
    output_folder,
    "step11_mismatch_sign_reversals.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 6: Detailed robustness table for key outcomes
# ==============================================================================

key_outcomes <- c(
  "Any vertical mismatch",
  "Overeducated",
  "Undereducated",
  "Horizontal mismatch: broad",
  "Horizontal mismatch: strict"
)

key_robustness_table <- log_results[
  outcome %in% key_outcomes,
  .(
    outcome,
    subgroup,
    specification,
    
    effect_pp_for_10_percent_increase =
      round(
        effect_pp_for_10_percent_increase,
        4
      ),
    
    lower_95_pp =
      round(
        lower_95_pp_for_10_percent_increase,
        4
      ),
    
    upper_95_pp =
      round(
        upper_95_pp_for_10_percent_increase,
        4
      ),
    
    p_value =
      round(p_value, 4),
    
    observations
  )
]

setorder(
  key_robustness_table,
  outcome,
  subgroup,
  specification
)

fwrite(
  key_robustness_table,
  file.path(
    output_folder,
    "step11_key_mismatch_robustness_table.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 7: Create main final mismatch table
# ==============================================================================

main_results <- results[
  specification == "Main log exposure"
]

main_final_table <- merge(
  main_results,
  descriptives[
    ,
    .(
      outcome,
      subgroup,
      weighted_rate_percent
    )
  ],
  by = c(
    "outcome",
    "subgroup"
  ),
  all.x = TRUE
)

main_final_table <- main_final_table[
  outcome %in% key_outcomes,
  .(
    outcome,
    subgroup,
    
    descriptive_rate_percent =
      round(
        weighted_rate_percent,
        2
      ),
    
    effect_pp_for_10_percent_increase =
      round(
        effect_pp_for_10_percent_increase,
        4
      ),
    
    standard_error_pp_for_10_percent =
      round(
        100 *
          standard_error *
          log(1.10),
        4
      ),
    
    lower_95_pp =
      round(
        lower_95_pp_for_10_percent_increase,
        4
      ),
    
    upper_95_pp =
      round(
        upper_95_pp_for_10_percent_increase,
        4
      ),
    
    p_value =
      round(p_value, 4),
    
    observations
  )
]

setorder(
  main_final_table,
  outcome,
  subgroup
)

print(main_final_table)

fwrite(
  main_final_table,
  file.path(
    output_folder,
    "step11_final_main_mismatch_table.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 8: Create robustness-stability summary
# ==============================================================================

stability_summary <- log_results[
  outcome %in% key_outcomes,
  .(
    minimum_effect_pp_10pct =
      min(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    maximum_effect_pp_10pct =
      max(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    mean_effect_pp_10pct =
      mean(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    positive_specifications =
      sum(
        coefficient > 0,
        na.rm = TRUE
      ),
    
    negative_specifications =
      sum(
        coefficient < 0,
        na.rm = TRUE
      ),
    
    significant_5_percent =
      sum(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    significant_10_percent =
      sum(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    total_specifications = .N
  ),
  by = .(
    outcome,
    subgroup
  )
]

setorder(
  stability_summary,
  outcome,
  subgroup
)

fwrite(
  stability_summary,
  file.path(
    output_folder,
    "step11_final_mismatch_stability_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Read variables required for formal gender-interaction tests
# ==============================================================================

variables_to_read <- c(
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  "employed",
  "vertical_mismatch_final",
  "overeducated_final",
  "undereducated_final",
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict"
)

cat(
  "\nReading variables for gender-interaction models...\n"
)

dt <- as.data.table(
  read_fst(
    input_file,
    columns = variables_to_read
  )
)


# ==============================================================================
# STEP 10: Construct analytical variables
# ==============================================================================

dt[
  ,
  historical_province :=
    province_historical
]

dt[
  ,
  cohort :=
    as.integer(analysis_birth_year)
]

dt[
  ,
  female := fcase(
    gender == 1L, 1L,
    gender == 2L, 0L,
    default = NA_integer_
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
  log_undergrad_exposure :=
    fifelse(
      !is.na(undergrad_exposure_18_20) &
        undergrad_exposure_18_20 > 0,
      log(undergrad_exposure_18_20),
      NA_real_
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
    !is.na(log_undergrad_exposure) &
    valid_weight &
    valid_cohort &
    !is.na(historical_province) &
    !is.na(year) &
    !is.na(female) &
    !is.na(urban)
]


# ==============================================================================
# STEP 11: Create gender-specific fixed-effect identifiers
#
# These allow:
#   - province effects to differ by gender
#   - cohort effects to differ by gender
#   - survey-year effects to differ by gender
#
# This is preferable to imposing identical fixed effects for women and men.
# ==============================================================================

dt[
  ,
  province_gender :=
    interaction(
      historical_province,
      gender,
      drop = TRUE
    )
]

dt[
  ,
  cohort_gender :=
    interaction(
      cohort,
      gender,
      drop = TRUE
    )
]

dt[
  ,
  year_gender :=
    interaction(
      year,
      gender,
      drop = TRUE
    )
]


# ==============================================================================
# STEP 12: Define outcomes for interaction tests
# ==============================================================================

interaction_outcomes <- data.table(
  outcome = c(
    "Any vertical mismatch",
    "Overeducated",
    "Undereducated",
    "Horizontal mismatch: broad",
    "Horizontal mismatch: strict"
  ),
  
  outcome_variable = c(
    "vertical_mismatch_final",
    "overeducated_final",
    "undereducated_final",
    "horizontal_mismatch_detailed_broad",
    "horizontal_mismatch_detailed_strict"
  )
)


# ==============================================================================
# STEP 13: Function to estimate gender-specific-FE interaction model
#
# Reference gender:
#   Men, because female = 0 for men.
#
# Coefficients:
#   log_undergrad_exposure
#       = effect for men
#
#   log_undergrad_exposure:female
#       = female-minus-male difference
#
#   Female effect
#       = male effect + interaction coefficient
# ==============================================================================

estimate_gender_interaction <- function(
    data,
    outcome_name,
    outcome_variable
) {
  
  analysis_data <- data[
    sample_main == TRUE &
      employed == 1L &
      !is.na(get(outcome_variable))
  ]
  
  model_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ log_undergrad_exposure * female + urban | ",
      "province_gender + cohort_gender + year_gender"
    )
  )
  
  model <- feols(
    model_formula,
    data = analysis_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  coefficient_vector <- coef(model)
  variance_matrix <- vcov(model)
  
  treatment_term <- "log_undergrad_exposure"
  interaction_term <- "log_undergrad_exposure:female"
  
  beta_male <-
    unname(
      coefficient_vector[treatment_term]
    )
  
  beta_difference <-
    unname(
      coefficient_vector[interaction_term]
    )
  
  beta_female <-
    beta_male +
    beta_difference
  
  variance_male <-
    variance_matrix[
      treatment_term,
      treatment_term
    ]
  
  variance_difference <-
    variance_matrix[
      interaction_term,
      interaction_term
    ]
  
  covariance_term <-
    variance_matrix[
      treatment_term,
      interaction_term
    ]
  
  variance_female <-
    variance_male +
    variance_difference +
    2 * covariance_term
  
  se_male <-
    sqrt(variance_male)
  
  se_difference <-
    sqrt(variance_difference)
  
  se_female <-
    sqrt(variance_female)
  
  z_male <-
    beta_male /
    se_male
  
  z_female <-
    beta_female /
    se_female
  
  z_difference <-
    beta_difference /
    se_difference
  
  p_male <-
    2 *
    pnorm(
      abs(z_male),
      lower.tail = FALSE
    )
  
  p_female <-
    2 *
    pnorm(
      abs(z_female),
      lower.tail = FALSE
    )
  
  p_difference <-
    2 *
    pnorm(
      abs(z_difference),
      lower.tail = FALSE
    )
  
  effect_multiplier <-
    100 *
    log(1.10)
  
  data.table(
    outcome = outcome_name,
    
    male_coefficient =
      beta_male,
    
    male_standard_error =
      se_male,
    
    male_effect_pp_10pct =
      effect_multiplier *
      beta_male,
    
    male_p_value =
      p_male,
    
    female_coefficient =
      beta_female,
    
    female_standard_error =
      se_female,
    
    female_effect_pp_10pct =
      effect_multiplier *
      beta_female,
    
    female_p_value =
      p_female,
    
    female_minus_male_coefficient =
      beta_difference,
    
    difference_standard_error =
      se_difference,
    
    female_minus_male_effect_pp_10pct =
      effect_multiplier *
      beta_difference,
    
    difference_p_value =
      p_difference,
    
    observations =
      nobs(model)
  )
}


# ==============================================================================
# STEP 14: Estimate all formal gender-difference tests
# ==============================================================================

gender_interaction_results_list <- vector(
  mode = "list",
  length = nrow(interaction_outcomes)
)

for (i in seq_len(nrow(interaction_outcomes))) {
  
  current_outcome_name <-
    interaction_outcomes$outcome[i]
  
  current_outcome_variable <-
    interaction_outcomes$outcome_variable[i]
  
  cat(
    "\nEstimating gender interaction for:",
    current_outcome_name,
    "\n"
  )
  
  gender_interaction_results_list[[i]] <-
    estimate_gender_interaction(
      data = dt,
      outcome_name = current_outcome_name,
      outcome_variable = current_outcome_variable
    )
}

gender_interaction_results <- rbindlist(
  gender_interaction_results_list
)

print(gender_interaction_results)

fwrite(
  gender_interaction_results,
  file.path(
    output_folder,
    "step11_gender_interactions_gender_specific_FE.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Create publication-style compact interaction table
# ==============================================================================

gender_interaction_compact <- gender_interaction_results[
  ,
  .(
    outcome,
    
    male_effect_pp_10pct =
      round(
        male_effect_pp_10pct,
        4
      ),
    
    male_p_value =
      round(
        male_p_value,
        4
      ),
    
    female_effect_pp_10pct =
      round(
        female_effect_pp_10pct,
        4
      ),
    
    female_p_value =
      round(
        female_p_value,
        4
      ),
    
    female_minus_male_effect_pp_10pct =
      round(
        female_minus_male_effect_pp_10pct,
        4
      ),
    
    difference_p_value =
      round(
        difference_p_value,
        4
      ),
    
    observations
  )
]

fwrite(
  gender_interaction_compact,
  file.path(
    output_folder,
    "step11_gender_interactions_compact.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Final console summary
# ==============================================================================

cat(
  "\n============================================================\n",
  "FINAL MAIN MISMATCH TABLE\n",
  "============================================================\n"
)

print(main_final_table)

cat(
  "\n============================================================\n",
  "FORMAL FEMALE-MALE DIFFERENCE TESTS\n",
  "============================================================\n"
)

print(gender_interaction_compact)

cat(
  "\n============================================================\n",
  "STEP 11 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain final table:\n",
  file.path(
    output_folder,
    "step11_final_main_mismatch_table.csv"
  ),
  "\n"
)

cat(
  "\nRobustness table:\n",
  file.path(
    output_folder,
    "step11_key_mismatch_robustness_table.csv"
  ),
  "\n"
)

cat(
  "\nSign-reversal diagnostic:\n",
  file.path(
    output_folder,
    "step11_mismatch_sign_reversals.csv"
  ),
  "\n"
)

cat(
  "\nGender-difference tests:\n",
  file.path(
    output_folder,
    "step11_gender_interactions_gender_specific_FE.csv"
  ),
  "\n"
)