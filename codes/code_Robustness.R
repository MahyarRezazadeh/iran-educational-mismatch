# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 9: Core Outcome Robustness and Final Results
#
# Outcomes:
#   1. Labor-force participation
#   2. Employment
#   3. Unemployment among labor-force participants
#   4. Currently married
#   5. Ever married
#
# Samples:
#   - Pooled
#   - Women
#   - Men
#
# Specifications:
#   1. Baseline survey-weighted model
#   2. Unweighted model
#   3. Excluding Semnan
#   4. Excluding Semnan and Qazvin
#   5. Excluding recent cross-region movers
#   6. Province-specific linear cohort trends
#
# Fixed effects:
#   - Historical province
#   - Corrected birth cohort: analysis_birth_year
#   - Survey year
#
# Inference:
#   Two-way clustered standard errors by province and corrected birth cohort.
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

required_packages <- c(
  "data.table",
  "fst",
  "fixest"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
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

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step9_core_outcome_robustness"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The final dataset was not found:\n",
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
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  "labor_force_participation",
  "employed",
  "unemployed",
  "currently_married",
  "ever_married",
  "recent_cross_historical_region_move"
)

cat("\nReading Step 9 variables...\n")

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
# STEP 3: Standardize variables
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
  cohort_centered := cohort - 1369L
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
  female := fcase(
    gender == 1L, 1L,
    gender == 2L, 0L,
    default = NA_integer_
  )
]

dt[
  ,
  exposure_per_100 :=
    undergrad_exposure_18_20 / 100
]


# ==============================================================================
# STEP 4: Define valid samples
# ==============================================================================

dt[
  ,
  valid_weight :=
    as.integer(
      !is.na(weight) &
        is.finite(weight) &
        weight > 0
    )
]

dt[
  ,
  valid_cohort :=
    as.integer(
      !is.na(cohort) &
        cohort >= 1355L &
        cohort <= 1383L
    )
]

dt[
  ,
  sample_main :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(undergrad_exposure_18_20) &
        valid_weight == 1L &
        valid_cohort == 1L &
        !is.na(historical_province) &
        !is.na(year) &
        !is.na(gender) &
        !is.na(urban)
    )
]

dt[
  ,
  sample_lfp :=
    as.integer(
      sample_main == 1L &
        !is.na(labor_force_participation)
    )
]

dt[
  ,
  sample_employment :=
    as.integer(
      sample_main == 1L &
        !is.na(employed)
    )
]

dt[
  ,
  sample_unemployment :=
    as.integer(
      sample_main == 1L &
        labor_force_participation == 1L &
        !is.na(unemployed)
    )
]

dt[
  ,
  sample_currently_married :=
    as.integer(
      sample_main == 1L &
        !is.na(currently_married)
    )
]

dt[
  ,
  sample_ever_married :=
    as.integer(
      sample_main == 1L &
        !is.na(ever_married)
    )
]

cat(
  "\nMain sample:",
  format(dt[sample_main == 1L, .N], big.mark = ","),
  "\n"
)


# ==============================================================================
# STEP 5: Calculate treatment scaling
# ==============================================================================

main_exposure_sd <- dt[
  sample_main == 1L,
  sqrt(
    weighted.mean(
      (
        undergrad_exposure_18_20 -
          weighted.mean(
            undergrad_exposure_18_20,
            weight,
            na.rm = TRUE
          )
      )^2,
      weight,
      na.rm = TRUE
    )
  )
]

exposure_sd_in_100_units <- main_exposure_sd / 100

cat(
  "\nWeighted treatment SD:",
  round(main_exposure_sd, 3),
  "\n"
)

cat(
  "Treatment SD in units of 100:",
  round(exposure_sd_in_100_units, 3),
  "\n"
)


# ==============================================================================
# STEP 6: Define outcome metadata
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Labor-force participation",
    "Employment",
    "Unemployment",
    "Currently married",
    "Ever married"
  ),
  
  outcome_variable = c(
    "labor_force_participation",
    "employed",
    "unemployed",
    "currently_married",
    "ever_married"
  ),
  
  sample_variable = c(
    "sample_lfp",
    "sample_employment",
    "sample_unemployment",
    "sample_currently_married",
    "sample_ever_married"
  )
)


# ==============================================================================
# STEP 7: Define robustness specifications
# ==============================================================================

specification_definitions <- data.table(
  specification = c(
    "Baseline weighted",
    "Unweighted",
    "Exclude Semnan",
    "Exclude Semnan and Qazvin",
    "Exclude recent cross-region movers",
    "Province-specific cohort trends"
  ),
  
  weighted = c(
    TRUE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  
  sample_rule = c(
    "all",
    "all",
    "exclude_semnan",
    "exclude_semnan_qazvin",
    "exclude_recent_migrants",
    "all"
  ),
  
  province_trends = c(
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    TRUE
  )
)


# ==============================================================================
# STEP 8: Function to extract treatment results
# ==============================================================================

extract_treatment_result <- function(
    model,
    outcome_name,
    subgroup_name,
    specification_name
) {
  
  estimate <- unname(
    coef(model)["exposure_per_100"]
  )
  
  standard_error <- unname(
    se(model)["exposure_per_100"]
  )
  
  p_value <- unname(
    pvalue(model)["exposure_per_100"]
  )
  
  confidence_interval <- confint(
    model,
    parm = "exposure_per_100",
    level = 0.95
  )
  
  lower_95 <- confidence_interval[1, 1]
  upper_95 <- confidence_interval[1, 2]
  
  data.table(
    outcome = outcome_name,
    subgroup = subgroup_name,
    specification = specification_name,
    
    coefficient = estimate,
    standard_error = standard_error,
    p_value = p_value,
    
    lower_95 = lower_95,
    upper_95 = upper_95,
    
    effect_percentage_points_per_100 =
      100 * estimate,
    
    lower_95_percentage_points_per_100 =
      100 * lower_95,
    
    upper_95_percentage_points_per_100 =
      100 * upper_95,
    
    effect_percentage_points_per_sd =
      100 * estimate * exposure_sd_in_100_units,
    
    standard_error_percentage_points_per_sd =
      100 * standard_error * exposure_sd_in_100_units,
    
    observations = nobs(model)
  )
}


# ==============================================================================
# STEP 9: Function to estimate one robustness model
# ==============================================================================

estimate_one_model <- function(
    data,
    outcome_variable,
    sample_variable,
    subgroup,
    weighted_model,
    sample_rule,
    province_trends
) {
  
  analysis_data <- data[
    get(sample_variable) == 1L
  ]
  
  if (subgroup == "Women") {
    analysis_data <- analysis_data[
      gender == 1L
    ]
  }
  
  if (subgroup == "Men") {
    analysis_data <- analysis_data[
      gender == 2L
    ]
  }
  
  if (sample_rule == "exclude_semnan") {
    analysis_data <- analysis_data[
      historical_province != "Semnan"
    ]
  }
  
  if (sample_rule == "exclude_semnan_qazvin") {
    analysis_data <- analysis_data[
      !historical_province %in%
        c(
          "Semnan",
          "Qazvin"
        )
    ]
  }
  
  if (sample_rule == "exclude_recent_migrants") {
    analysis_data <- analysis_data[
      is.na(recent_cross_historical_region_move) |
        recent_cross_historical_region_move == 0L
    ]
  }
  
  if (subgroup == "Pooled") {
    right_hand_side <-
      "exposure_per_100 + female + urban"
  } else {
    right_hand_side <-
      "exposure_per_100 + urban"
  }
  
  if (province_trends) {
    
    fixed_effects <-
      "historical_province[cohort_centered] + cohort + year"
    
  } else {
    
    fixed_effects <-
      "historical_province + cohort + year"
  }
  
  model_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      right_hand_side,
      " | ",
      fixed_effects
    )
  )
  
  if (weighted_model) {
    
    model <- feols(
      model_formula,
      data = analysis_data,
      weights = ~weight,
      cluster = ~historical_province + cohort
    )
    
  } else {
    
    model <- feols(
      model_formula,
      data = analysis_data,
      cluster = ~historical_province + cohort
    )
  }
  
  model
}


# ==============================================================================
# STEP 10: Estimate all core-outcome robustness models
#
# Results are saved after every outcome so progress is not lost.
# ==============================================================================

core_robustness_results_list <- list()

result_counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  sample_variable <-
    outcome_definitions$sample_variable[outcome_index]
  
  cat(
    "\n============================================================\n",
    "OUTCOME:",
    outcome_name,
    "\n",
    "============================================================\n"
  )
  
  for (spec_index in seq_len(nrow(specification_definitions))) {
    
    specification_name <-
      specification_definitions$specification[spec_index]
    
    weighted_model <-
      specification_definitions$weighted[spec_index]
    
    sample_rule <-
      specification_definitions$sample_rule[spec_index]
    
    province_trends <-
      specification_definitions$province_trends[spec_index]
    
    for (subgroup_name in c(
      "Pooled",
      "Women",
      "Men"
    )) {
      
      cat(
        "Estimating:",
        specification_name,
        "-",
        subgroup_name,
        "\n"
      )
      
      current_model <- estimate_one_model(
        data = dt,
        outcome_variable = outcome_variable,
        sample_variable = sample_variable,
        subgroup = subgroup_name,
        weighted_model = weighted_model,
        sample_rule = sample_rule,
        province_trends = province_trends
      )
      
      core_robustness_results_list[[result_counter]] <-
        extract_treatment_result(
          model = current_model,
          outcome_name = outcome_name,
          subgroup_name = subgroup_name,
          specification_name = specification_name
        )
      
      result_counter <- result_counter + 1L
      
      rm(current_model)
      invisible(gc())
    }
  }
  
  partial_results <- rbindlist(
    core_robustness_results_list,
    fill = TRUE
  )
  
  fwrite(
    partial_results,
    file.path(
      output_folder,
      "step9_core_outcome_robustness_partial.csv"
    ),
    bom = TRUE
  )
}


core_robustness_results <- rbindlist(
  core_robustness_results_list,
  fill = TRUE
)

setorder(
  core_robustness_results,
  outcome,
  subgroup,
  specification
)

print(
  core_robustness_results
)

fwrite(
  core_robustness_results,
  file.path(
    output_folder,
    "step9_core_outcome_robustness_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Create a compact percentage-point table
# ==============================================================================

compact_results <- core_robustness_results[
  ,
  .(
    outcome,
    subgroup,
    specification,
    
    effect_pp_per_100 =
      round(
        effect_percentage_points_per_100,
        4
      ),
    
    standard_error_pp_per_100 =
      round(
        100 * standard_error,
        4
      ),
    
    lower_95_pp_per_100 =
      round(
        lower_95_percentage_points_per_100,
        4
      ),
    
    upper_95_pp_per_100 =
      round(
        upper_95_percentage_points_per_100,
        4
      ),
    
    p_value =
      round(
        p_value,
        4
      ),
    
    effect_pp_per_sd =
      round(
        effect_percentage_points_per_sd,
        4
      ),
    
    observations
  )
]

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step9_core_outcome_robustness_compact.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Gender-interaction models
#
# The interaction coefficient tests whether the effect for women differs
# statistically from the effect for men.
#
# Male exposure effect:
#   exposure_per_100
#
# Female-minus-male difference:
#   exposure_per_100:female
#
# Female total effect:
#   exposure_per_100 + exposure_per_100:female
# ==============================================================================

gender_interaction_results_list <- list()

for (i in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[i]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[i]
  
  sample_variable <-
    outcome_definitions$sample_variable[i]
  
  interaction_data <- dt[
    get(sample_variable) == 1L
  ]
  
  cat(
    "\nEstimating gender interaction:",
    outcome_name,
    "\n"
  )
  
  interaction_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ exposure_per_100 * female + urban | ",
      "historical_province + cohort + year"
    )
  )
  
  interaction_model <- feols(
    interaction_formula,
    data = interaction_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  model_coefs <- coef(interaction_model)
  model_vcov <- vcov(interaction_model)
  
  beta_male <-
    model_coefs["exposure_per_100"]
  
  beta_difference <-
    model_coefs["exposure_per_100:female"]
  
  beta_female <-
    beta_male + beta_difference
  
  var_male <-
    model_vcov[
      "exposure_per_100",
      "exposure_per_100"
    ]
  
  var_difference <-
    model_vcov[
      "exposure_per_100:female",
      "exposure_per_100:female"
    ]
  
  covariance_term <-
    model_vcov[
      "exposure_per_100",
      "exposure_per_100:female"
    ]
  
  se_male <- sqrt(var_male)
  
  se_difference <- sqrt(var_difference)
  
  se_female <- sqrt(
    var_male +
      var_difference +
      2 * covariance_term
  )
  
  p_difference <- 2 * pnorm(
    abs(beta_difference / se_difference),
    lower.tail = FALSE
  )
  
  p_male <- 2 * pnorm(
    abs(beta_male / se_male),
    lower.tail = FALSE
  )
  
  p_female <- 2 * pnorm(
    abs(beta_female / se_female),
    lower.tail = FALSE
  )
  
  gender_interaction_results_list[[i]] <- data.table(
    outcome = outcome_name,
    
    male_effect =
      beta_male,
    
    male_standard_error =
      se_male,
    
    male_p_value =
      p_male,
    
    female_effect =
      beta_female,
    
    female_standard_error =
      se_female,
    
    female_p_value =
      p_female,
    
    female_minus_male_difference =
      beta_difference,
    
    difference_standard_error =
      se_difference,
    
    difference_p_value =
      p_difference,
    
    male_effect_pp_per_100 =
      100 * beta_male,
    
    female_effect_pp_per_100 =
      100 * beta_female,
    
    female_minus_male_pp_per_100 =
      100 * beta_difference,
    
    observations =
      nobs(interaction_model)
  )
  
  rm(
    interaction_model,
    interaction_data,
    model_coefs,
    model_vcov
  )
  
  invisible(gc())
}


gender_interaction_results <- rbindlist(
  gender_interaction_results_list
)

print(
  gender_interaction_results
)

fwrite(
  gender_interaction_results,
  file.path(
    output_folder,
    "step9_gender_interaction_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Baseline-only final table
# ==============================================================================

baseline_final_table <- core_robustness_results[
  specification == "Baseline weighted",
  .(
    outcome,
    subgroup,
    
    coefficient,
    
    effect_percentage_points_per_100,
    
    standard_error_percentage_points =
      100 * standard_error,
    
    lower_95_percentage_points_per_100,
    
    upper_95_percentage_points_per_100,
    
    p_value,
    
    effect_percentage_points_per_sd,
    
    observations
  )
]

setorder(
  baseline_final_table,
  outcome,
  subgroup
)

fwrite(
  baseline_final_table,
  file.path(
    output_folder,
    "step9_baseline_final_core_outcomes.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 14: Specification-stability summary
#
# This shows the smallest and largest estimated effect across robustness
# specifications for each outcome and subgroup.
# ==============================================================================

stability_summary <- core_robustness_results[
  ,
  .(
    minimum_effect_pp_per_100 =
      min(
        effect_percentage_points_per_100,
        na.rm = TRUE
      ),
    
    maximum_effect_pp_per_100 =
      max(
        effect_percentage_points_per_100,
        na.rm = TRUE
      ),
    
    mean_effect_pp_per_100 =
      mean(
        effect_percentage_points_per_100,
        na.rm = TRUE
      ),
    
    specifications_negative =
      sum(
        effect_percentage_points_per_100 < 0,
        na.rm = TRUE
      ),
    
    specifications_positive =
      sum(
        effect_percentage_points_per_100 > 0,
        na.rm = TRUE
      ),
    
    specifications_significant_5pct =
      sum(
        p_value < 0.05,
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

print(
  stability_summary
)

fwrite(
  stability_summary,
  file.path(
    output_folder,
    "step9_specification_stability_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Save sample summary
# ==============================================================================

sample_summary <- data.table(
  outcome = outcome_definitions$outcome,
  
  sample_variable =
    outcome_definitions$sample_variable,
  
  observations = sapply(
    outcome_definitions$sample_variable,
    function(v) {
      dt[
        get(v) == 1L,
        .N
      ]
    }
  )
)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "step9_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Final completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 9 COMPLETED SUCCESSFULLY\n",
  "============================================================\n"
)

cat(
  "\nMain robustness-results file:\n",
  file.path(
    output_folder,
    "step9_core_outcome_robustness_results.csv"
  ),
  "\n"
)

cat(
  "\nCompact results file:\n",
  file.path(
    output_folder,
    "step9_core_outcome_robustness_compact.csv"
  ),
  "\n"
)

cat(
  "\nGender-interaction file:\n",
  file.path(
    output_folder,
    "step9_gender_interaction_results.csv"
  ),
  "\n"
)

cat(
  "\nSpecification-stability file:\n",
  file.path(
    output_folder,
    "step9_specification_stability_summary.csv"
  ),
  "\n"
)