# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 10: Educational Mismatch Analysis
#
# Main treatment:
#   log_undergrad_exposure = log(undergrad_exposure_18_20)
#
# Robustness treatment:
#   exposure_per_100 = undergrad_exposure_18_20 / 100
#
# Outcomes:
#   Vertical mismatch:
#     - Any vertical mismatch
#     - Overeducation
#     - Undereducation
#     - Correct educational match
#
#   Horizontal mismatch:
#     - Broad definition
#     - Strict definition
#
# Samples:
#   - Pooled
#   - Women
#   - Men
#
# Fixed effects:
#   - Historical province
#   - Corrected birth cohort
#   - Survey year
#
# Inference:
#   Two-way clustered standard errors by province and cohort
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
# STEP 1: Define file paths
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
  "step10_mismatch_analysis"
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
  
  # Employment
  "employed",
  
  # Migration robustness
  "recent_cross_historical_region_move",
  
  # Vertical mismatch outcomes
  "vertical_mismatch_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final",
  
  # Horizontal mismatch outcomes
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict"
)

cat("\nReading mismatch-analysis variables...\n")

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


# ==============================================================================
# STEP 4: Construct treatment variables
# ==============================================================================

dt[
  ,
  exposure_per_100 :=
    undergrad_exposure_18_20 / 100
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


# ==============================================================================
# STEP 5: Define main analytical sample
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
        undergrad_exposure_18_20 > 0 &
        valid_weight == 1L &
        valid_cohort == 1L &
        !is.na(historical_province) &
        !is.na(year) &
        !is.na(gender) &
        !is.na(urban)
    )
]

cat(
  "\nMain analytical sample:",
  format(
    dt[sample_main == 1L, .N],
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 6: Define mismatch outcome samples
#
# Vertical mismatch:
#   Employed individuals with valid occupation and education classification.
#
# Horizontal mismatch:
#   Employed tertiary-educated individuals with valid field and occupation data.
#   Nonmissing horizontal mismatch variables define the eligible sample.
# ==============================================================================

dt[
  ,
  sample_vertical_mismatch :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(vertical_mismatch_final)
    )
]

dt[
  ,
  sample_overeducated :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(overeducated_final)
    )
]

dt[
  ,
  sample_undereducated :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(undereducated_final)
    )
]

dt[
  ,
  sample_vertically_matched :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(vertically_matched_final)
    )
]

dt[
  ,
  sample_horizontal_broad :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(horizontal_mismatch_detailed_broad)
    )
]

dt[
  ,
  sample_horizontal_strict :=
    as.integer(
      sample_main == 1L &
        employed == 1L &
        !is.na(horizontal_mismatch_detailed_strict)
    )
]


# ==============================================================================
# STEP 7: Verify vertical mismatch categories
#
# Overeducated, undereducated, and vertically matched should sum to one
# in the valid vertical-mismatch sample.
# ==============================================================================

vertical_category_check <- dt[
  sample_vertical_mismatch == 1L,
  .(
    observations = .N,
    
    invalid_sum =
      sum(
        overeducated_final +
          undereducated_final +
          vertically_matched_final != 1,
        na.rm = TRUE
      ),
    
    mismatch_disagreement =
      sum(
        vertical_mismatch_final !=
          (
            overeducated_final == 1L |
              undereducated_final == 1L
          ),
        na.rm = TRUE
      )
  )
]

print(vertical_category_check)

fwrite(
  vertical_category_check,
  file.path(
    output_folder,
    "step10_vertical_category_check.csv"
  ),
  bom = TRUE
)

if (
  vertical_category_check$invalid_sum > 0L |
  vertical_category_check$mismatch_disagreement > 0L
) {
  warning(
    "Some vertical mismatch categories are internally inconsistent."
  )
}


# ==============================================================================
# STEP 8: Save sample summary
# ==============================================================================

sample_summary <- data.table(
  outcome = c(
    "Any vertical mismatch",
    "Overeducated",
    "Undereducated",
    "Vertically matched",
    "Horizontal mismatch: broad",
    "Horizontal mismatch: strict"
  ),
  
  sample_variable = c(
    "sample_vertical_mismatch",
    "sample_overeducated",
    "sample_undereducated",
    "sample_vertically_matched",
    "sample_horizontal_broad",
    "sample_horizontal_strict"
  ),
  
  observations = c(
    dt[sample_vertical_mismatch == 1L, .N],
    dt[sample_overeducated == 1L, .N],
    dt[sample_undereducated == 1L, .N],
    dt[sample_vertically_matched == 1L, .N],
    dt[sample_horizontal_broad == 1L, .N],
    dt[sample_horizontal_strict == 1L, .N]
  )
)

print(sample_summary)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "step10_mismatch_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Weighted descriptive mismatch rates
# ==============================================================================

weighted_binary_mean <- function(data, outcome, sample) {
  
  data[
    get(sample) == 1L,
    weighted.mean(
      get(outcome),
      weight,
      na.rm = TRUE
    )
  ]
}

outcome_definitions <- data.table(
  outcome = c(
    "Any vertical mismatch",
    "Overeducated",
    "Undereducated",
    "Vertically matched",
    "Horizontal mismatch: broad",
    "Horizontal mismatch: strict"
  ),
  
  outcome_variable = c(
    "vertical_mismatch_final",
    "overeducated_final",
    "undereducated_final",
    "vertically_matched_final",
    "horizontal_mismatch_detailed_broad",
    "horizontal_mismatch_detailed_strict"
  ),
  
  sample_variable = c(
    "sample_vertical_mismatch",
    "sample_overeducated",
    "sample_undereducated",
    "sample_vertically_matched",
    "sample_horizontal_broad",
    "sample_horizontal_strict"
  )
)

descriptive_results_list <- list()
descriptive_counter <- 1L

for (i in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <- outcome_definitions$outcome[i]
  outcome_variable <- outcome_definitions$outcome_variable[i]
  sample_variable <- outcome_definitions$sample_variable[i]
  
  for (subgroup_name in c(
    "Pooled",
    "Women",
    "Men"
  )) {
    
    subgroup_data <- dt
    
    if (subgroup_name == "Women") {
      subgroup_data <- subgroup_data[
        gender == 1L
      ]
    }
    
    if (subgroup_name == "Men") {
      subgroup_data <- subgroup_data[
        gender == 2L
      ]
    }
    
    rate <- weighted_binary_mean(
      subgroup_data,
      outcome_variable,
      sample_variable
    )
    
    observations <- subgroup_data[
      get(sample_variable) == 1L,
      .N
    ]
    
    descriptive_results_list[[descriptive_counter]] <-
      data.table(
        outcome = outcome_name,
        subgroup = subgroup_name,
        weighted_rate = rate,
        weighted_rate_percent = 100 * rate,
        observations = observations
      )
    
    descriptive_counter <- descriptive_counter + 1L
  }
}

mismatch_descriptives <- rbindlist(
  descriptive_results_list
)

print(mismatch_descriptives)

fwrite(
  mismatch_descriptives,
  file.path(
    output_folder,
    "step10_mismatch_weighted_descriptives.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 10: Function to estimate pooled, women, and men models
# ==============================================================================

estimate_mismatch_models <- function(
    data,
    outcome_variable,
    sample_variable,
    treatment_variable,
    weighted_model = TRUE,
    sample_rule = "all",
    province_trends = FALSE
) {
  
  analysis_data <- data[
    get(sample_variable) == 1L
  ]
  
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
  
  if (province_trends) {
    
    fixed_effects <-
      "historical_province[cohort_centered] + cohort + year"
    
  } else {
    
    fixed_effects <-
      "historical_province + cohort + year"
  }
  
  pooled_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      treatment_variable,
      " + female + urban | ",
      fixed_effects
    )
  )
  
  subgroup_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      treatment_variable,
      " + urban | ",
      fixed_effects
    )
  )
  
  estimate_model <- function(
    formula,
    model_data
  ) {
    
    if (weighted_model) {
      
      feols(
        formula,
        data = model_data,
        weights = ~weight,
        cluster = ~historical_province + cohort
      )
      
    } else {
      
      feols(
        formula,
        data = model_data,
        cluster = ~historical_province + cohort
      )
    }
  }
  
  pooled_model <- estimate_model(
    pooled_formula,
    analysis_data
  )
  
  female_model <- estimate_model(
    subgroup_formula,
    analysis_data[gender == 1L]
  )
  
  male_model <- estimate_model(
    subgroup_formula,
    analysis_data[gender == 2L]
  )
  
  list(
    pooled = pooled_model,
    women = female_model,
    men = male_model
  )
}


# ==============================================================================
# STEP 11: Function to extract treatment coefficient
# ==============================================================================

extract_mismatch_result <- function(
    model,
    outcome_name,
    subgroup_name,
    specification_name,
    treatment_variable
) {
  
  estimate <- unname(
    coef(model)[treatment_variable]
  )
  
  standard_error <- unname(
    se(model)[treatment_variable]
  )
  
  p_value <- unname(
    pvalue(model)[treatment_variable]
  )
  
  confidence_interval <- confint(
    model,
    parm = treatment_variable,
    level = 0.95
  )
  
  lower_95 <- confidence_interval[1, 1]
  upper_95 <- confidence_interval[1, 2]
  
  result <- data.table(
    outcome = outcome_name,
    subgroup = subgroup_name,
    specification = specification_name,
    treatment = treatment_variable,
    
    coefficient = estimate,
    standard_error = standard_error,
    p_value = p_value,
    
    lower_95 = lower_95,
    upper_95 = upper_95,
    
    observations = nobs(model)
  )
  
  if (treatment_variable == "log_undergrad_exposure") {
    
    result[
      ,
      `:=`(
        effect_pp_for_1_percent_increase =
          coefficient,
        
        effect_pp_for_10_percent_increase =
          100 * coefficient * log(1.10),
        
        lower_95_pp_for_10_percent_increase =
          100 * lower_95 * log(1.10),
        
        upper_95_pp_for_10_percent_increase =
          100 * upper_95 * log(1.10)
      )
    ]
    
  } else {
    
    result[
      ,
      `:=`(
        effect_pp_for_1_percent_increase =
          NA_real_,
        
        effect_pp_for_10_percent_increase =
          NA_real_,
        
        lower_95_pp_for_10_percent_increase =
          NA_real_,
        
        upper_95_pp_for_10_percent_increase =
          NA_real_
      )
    ]
  }
  
  result[
    ,
    effect_pp_per_100_exposure :=
      fifelse(
        treatment_variable == "exposure_per_100",
        100 * coefficient,
        NA_real_
      )
  ]
  
  result
}


# ==============================================================================
# STEP 12: Define main and robustness specifications
# ==============================================================================

specification_definitions <- data.table(
  specification = c(
    "Main log exposure",
    "Linear exposure per 100",
    "Log exposure unweighted",
    "Log exposure excluding Semnan",
    "Log exposure excluding Semnan and Qazvin",
    "Log exposure excluding recent migrants",
    "Log exposure with province cohort trends"
  ),
  
  treatment_variable = c(
    "log_undergrad_exposure",
    "exposure_per_100",
    "log_undergrad_exposure",
    "log_undergrad_exposure",
    "log_undergrad_exposure",
    "log_undergrad_exposure",
    "log_undergrad_exposure"
  ),
  
  weighted_model = c(
    TRUE,
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
    FALSE,
    TRUE
  )
)


# ==============================================================================
# STEP 13: Estimate all mismatch models
#
# 6 outcomes × 7 specifications × 3 subgroups = 126 models
# ==============================================================================

all_results_list <- list()
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
    
    treatment_variable <-
      specification_definitions$treatment_variable[spec_index]
    
    weighted_model <-
      specification_definitions$weighted_model[spec_index]
    
    sample_rule <-
      specification_definitions$sample_rule[spec_index]
    
    province_trends <-
      specification_definitions$province_trends[spec_index]
    
    cat(
      "Estimating:",
      specification_name,
      "\n"
    )
    
    current_models <- estimate_mismatch_models(
      data = dt,
      outcome_variable = outcome_variable,
      sample_variable = sample_variable,
      treatment_variable = treatment_variable,
      weighted_model = weighted_model,
      sample_rule = sample_rule,
      province_trends = province_trends
    )
    
    for (subgroup_name in c(
      "pooled",
      "women",
      "men"
    )) {
      
      all_results_list[[result_counter]] <-
        extract_mismatch_result(
          model = current_models[[subgroup_name]],
          outcome_name = outcome_name,
          subgroup_name = subgroup_name,
          specification_name = specification_name,
          treatment_variable = treatment_variable
        )
      
      result_counter <- result_counter + 1L
    }
    
    rm(current_models)
    invisible(gc())
  }
  
  partial_results <- rbindlist(
    all_results_list,
    fill = TRUE
  )
  
  fwrite(
    partial_results,
    file.path(
      output_folder,
      "step10_mismatch_results_partial.csv"
    ),
    bom = TRUE
  )
}


# ==============================================================================
# STEP 14: Combine and save all results
# ==============================================================================

mismatch_results <- rbindlist(
  all_results_list,
  fill = TRUE
)

setorder(
  mismatch_results,
  outcome,
  subgroup,
  specification
)

print(mismatch_results)

fwrite(
  mismatch_results,
  file.path(
    output_folder,
    "step10_mismatch_all_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Main log-exposure results
# ==============================================================================

main_log_results <- mismatch_results[
  specification == "Main log exposure",
  .(
    outcome,
    subgroup,
    
    coefficient,
    standard_error,
    p_value,
    
    lower_95,
    upper_95,
    
    effect_pp_for_1_percent_increase,
    effect_pp_for_10_percent_increase,
    
    lower_95_pp_for_10_percent_increase,
    upper_95_pp_for_10_percent_increase,
    
    observations
  )
]

setorder(
  main_log_results,
  outcome,
  subgroup
)

print(main_log_results)

fwrite(
  main_log_results,
  file.path(
    output_folder,
    "step10_mismatch_main_log_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Linear-exposure robustness results
# ==============================================================================

linear_results <- mismatch_results[
  specification == "Linear exposure per 100",
  .(
    outcome,
    subgroup,
    
    coefficient,
    standard_error,
    p_value,
    
    lower_95,
    upper_95,
    
    effect_pp_per_100_exposure,
    
    observations
  )
]

setorder(
  linear_results,
  outcome,
  subgroup
)

fwrite(
  linear_results,
  file.path(
    output_folder,
    "step10_mismatch_linear_exposure_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Specification-stability summary for log models
# ==============================================================================

log_stability_summary <- mismatch_results[
  treatment == "log_undergrad_exposure",
  .(
    minimum_effect_pp_for_10_percent =
      min(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    maximum_effect_pp_for_10_percent =
      max(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    mean_effect_pp_for_10_percent =
      mean(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    negative_specifications =
      sum(
        coefficient < 0,
        na.rm = TRUE
      ),
    
    positive_specifications =
      sum(
        coefficient > 0,
        na.rm = TRUE
      ),
    
    significant_5pct =
      sum(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    significant_10pct =
      sum(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    total_log_specifications = .N
  ),
  by = .(
    outcome,
    subgroup
  )
]

setorder(
  log_stability_summary,
  outcome,
  subgroup
)

print(log_stability_summary)

fwrite(
  log_stability_summary,
  file.path(
    output_folder,
    "step10_mismatch_log_stability_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 18: Complementarity check
#
# Because vertically matched = 1 - vertical mismatch, the coefficients should
# be equal in magnitude and opposite in sign.
# ==============================================================================

complementarity_check <- dcast(
  main_log_results[
    outcome %in% c(
      "Any vertical mismatch",
      "Vertically matched"
    )
  ],
  subgroup ~ outcome,
  value.var = "coefficient"
)

complementarity_check[
  ,
  coefficient_sum :=
    `Any vertical mismatch` +
    `Vertically matched`
]

print(complementarity_check)

fwrite(
  complementarity_check,
  file.path(
    output_folder,
    "step10_vertical_complementarity_check.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Display main model results
# ==============================================================================

cat(
  "\n============================================================\n",
  "MAIN LOG-EXPOSURE MISMATCH RESULTS\n",
  "============================================================\n"
)

print(
  main_log_results[
    ,
    .(
      outcome,
      subgroup,
      
      coefficient = round(
        coefficient,
        5
      ),
      
      standard_error = round(
        standard_error,
        5
      ),
      
      effect_pp_10pct_increase = round(
        effect_pp_for_10_percent_increase,
        4
      ),
      
      p_value = round(
        p_value,
        4
      ),
      
      observations
    )
  ]
)


# ==============================================================================
# STEP 20: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 10 MISMATCH ANALYSIS COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain treatment:\n",
  "log_undergrad_exposure\n"
)

cat(
  "\nMain interpretation:\n",
  "effect_pp_for_10_percent_increase gives the percentage-point\n",
  "change in mismatch associated with a 10% increase in exposure.\n"
)

cat(
  "\nMain results file:\n",
  file.path(
    output_folder,
    "step10_mismatch_main_log_results.csv"
  ),
  "\n"
)

cat(
  "\nAll robustness results:\n",
  file.path(
    output_folder,
    "step10_mismatch_all_results.csv"
  ),
  "\n"
)