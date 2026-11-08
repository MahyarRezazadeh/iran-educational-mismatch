

# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# ROBUSTNESS CHECK: Excluding Historical Tehran
#
# Purpose:
#   Compare the main mismatch results in:
#     1. Full national sample
#     2. Sample excluding historical Tehran
#     3. Sample excluding historical Tehran and recent cross-region movers
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Outcomes:
#   - Any vertical mismatch
#   - Overeducation
#   - Undereducation
#   - Horizontal mismatch: broad
#   - Horizontal mismatch: strict
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
#   Two-way clustered standard errors by historical province and cohort
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
  "step12_excluding_tehran"
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
# STEP 2: Read required variables
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
  
  # Employment and migration
  "employed",
  "recent_cross_historical_region_move",
  
  # Mismatch outcomes
  "vertical_mismatch_final",
  "overeducated_final",
  "undereducated_final",
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict"
)

cat("\nReading Tehran robustness variables...\n")

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
  log_undergrad_exposure := fifelse(
    !is.na(undergrad_exposure_18_20) &
      undergrad_exposure_18_20 > 0,
    log(undergrad_exposure_18_20),
    NA_real_
  )
]


# ==============================================================================
# STEP 4: Define main analytical sample
# ==============================================================================

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
    !is.na(gender) &
    !is.na(urban)
]


# ==============================================================================
# STEP 5: Confirm Tehran label and sample share
# ==============================================================================

province_counts <- dt[
  sample_main == TRUE,
  .(
    observations = .N,
    weighted_population = sum(weight, na.rm = TRUE)
  ),
  by = historical_province
][
  order(-observations)
]

print(province_counts)

fwrite(
  province_counts,
  file.path(
    output_folder,
    "step12_province_counts.csv"
  ),
  bom = TRUE
)

if (!"Tehran" %in% province_counts$historical_province) {
  stop(
    paste0(
      "The province label 'Tehran' was not found.\n",
      "Inspect step12_province_counts.csv and replace the label if needed."
    )
  )
}

tehran_sample_summary <- dt[
  sample_main == TRUE,
  .(
    total_observations = .N,
    
    tehran_observations =
      sum(
        historical_province == "Tehran",
        na.rm = TRUE
      ),
    
    tehran_share_percent =
      100 *
      mean(
        historical_province == "Tehran",
        na.rm = TRUE
      ),
    
    weighted_tehran_share_percent =
      100 *
      sum(
        weight[historical_province == "Tehran"],
        na.rm = TRUE
      ) /
      sum(
        weight,
        na.rm = TRUE
      )
  )
]

print(tehran_sample_summary)

fwrite(
  tehran_sample_summary,
  file.path(
    output_folder,
    "step12_tehran_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 6: Define mismatch outcomes
# ==============================================================================

outcome_definitions <- data.table(
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
# STEP 7: Define robustness samples
# ==============================================================================

sample_definitions <- data.table(
  specification = c(
    "Full sample",
    "Exclude Tehran",
    "Exclude Tehran and recent cross-region movers"
  ),
  
  sample_rule = c(
    "full",
    "exclude_tehran",
    "exclude_tehran_migrants"
  )
)


# ==============================================================================
# STEP 8: Function to prepare one analytical sample
# ==============================================================================

prepare_analysis_data <- function(
    data,
    outcome_variable,
    subgroup,
    sample_rule
) {
  
  analysis_data <- data[
    sample_main == TRUE &
      employed == 1L &
      !is.na(get(outcome_variable))
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
  
  if (sample_rule == "exclude_tehran") {
    analysis_data <- analysis_data[
      historical_province != "Tehran"
    ]
  }
  
  if (sample_rule == "exclude_tehran_migrants") {
    analysis_data <- analysis_data[
      historical_province != "Tehran" &
        (
          is.na(recent_cross_historical_region_move) |
            recent_cross_historical_region_move == 0L
        )
    ]
  }
  
  analysis_data
}


# ==============================================================================
# STEP 9: Function to estimate one model
# ==============================================================================

estimate_one_model <- function(
    analysis_data,
    outcome_variable,
    subgroup
) {
  
  if (subgroup == "Pooled") {
    
    model_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + female + urban | ",
        "historical_province + cohort + year"
      )
    )
    
  } else {
    
    model_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + urban | ",
        "historical_province + cohort + year"
      )
    )
  }
  
  feols(
    model_formula,
    data = analysis_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
}


# ==============================================================================
# STEP 10: Function to extract treatment result
# ==============================================================================

extract_result <- function(
    model,
    outcome_name,
    subgroup_name,
    specification_name
) {
  
  treatment_term <- "log_undergrad_exposure"
  
  estimate <- unname(
    coef(model)[treatment_term]
  )
  
  standard_error <- unname(
    se(model)[treatment_term]
  )
  
  p_value <- unname(
    pvalue(model)[treatment_term]
  )
  
  confidence_interval <- confint(
    model,
    parm = treatment_term,
    level = 0.95
  )
  
  lower_95 <- confidence_interval[1, 1]
  upper_95 <- confidence_interval[1, 2]
  
  effect_multiplier <- 100 * log(1.10)
  
  data.table(
    outcome = outcome_name,
    subgroup = subgroup_name,
    specification = specification_name,
    
    coefficient = estimate,
    standard_error = standard_error,
    p_value = p_value,
    
    effect_pp_for_10_percent_increase =
      effect_multiplier * estimate,
    
    standard_error_pp_for_10_percent =
      effect_multiplier * standard_error,
    
    lower_95_pp_for_10_percent =
      effect_multiplier * lower_95,
    
    upper_95_pp_for_10_percent =
      effect_multiplier * upper_95,
    
    observations = nobs(model),
    
    provinces =
      length(
        fixef(model)$historical_province
      )
  )
}


# ==============================================================================
# STEP 11: Estimate all models
#
# 5 outcomes × 3 samples × 3 subgroups = 45 models
# ==============================================================================

results_list <- list()
counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  cat(
    "\n============================================================\n",
    "OUTCOME:",
    outcome_name,
    "\n",
    "============================================================\n"
  )
  
  for (sample_index in seq_len(nrow(sample_definitions))) {
    
    specification_name <-
      sample_definitions$specification[sample_index]
    
    sample_rule <-
      sample_definitions$sample_rule[sample_index]
    
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
      
      analysis_data <- prepare_analysis_data(
        data = dt,
        outcome_variable = outcome_variable,
        subgroup = subgroup_name,
        sample_rule = sample_rule
      )
      
      current_model <- estimate_one_model(
        analysis_data = analysis_data,
        outcome_variable = outcome_variable,
        subgroup = subgroup_name
      )
      
      results_list[[counter]] <- extract_result(
        model = current_model,
        outcome_name = outcome_name,
        subgroup_name = subgroup_name,
        specification_name = specification_name
      )
      
      counter <- counter + 1L
      
      rm(
        analysis_data,
        current_model
      )
      
      invisible(gc())
    }
  }
  
  partial_results <- rbindlist(
    results_list,
    fill = TRUE
  )
  
  fwrite(
    partial_results,
    file.path(
      output_folder,
      "step12_excluding_tehran_results_partial.csv"
    ),
    bom = TRUE
  )
}


# ==============================================================================
# STEP 12: Combine and save results
# ==============================================================================

tehran_results <- rbindlist(
  results_list,
  fill = TRUE
)

setorder(
  tehran_results,
  outcome,
  subgroup,
  specification
)

print(tehran_results)

fwrite(
  tehran_results,
  file.path(
    output_folder,
    "step12_excluding_tehran_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Create compact results table
# ==============================================================================

compact_results <- tehran_results[
  ,
  .(
    outcome,
    subgroup,
    specification,
    
    effect_pp_10pct =
      round(
        effect_pp_for_10_percent_increase,
        4
      ),
    
    standard_error_pp_10pct =
      round(
        standard_error_pp_for_10_percent,
        4
      ),
    
    lower_95_pp =
      round(
        lower_95_pp_for_10_percent,
        4
      ),
    
    upper_95_pp =
      round(
        upper_95_pp_for_10_percent,
        4
      ),
    
    p_value =
      round(
        p_value,
        4
      ),
    
    observations,
    provinces
  )
]

print(compact_results)

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step12_excluding_tehran_compact.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 14: Compare Tehran exclusion directly with the full sample
# ==============================================================================

comparison_table <- dcast(
  tehran_results,
  outcome + subgroup ~ specification,
  value.var = "effect_pp_for_10_percent_increase"
)

comparison_table[
  ,
  change_after_excluding_tehran :=
    `Exclude Tehran` -
    `Full sample`
]

comparison_table[
  ,
  change_after_excluding_tehran_and_movers :=
    `Exclude Tehran and recent cross-region movers` -
    `Full sample`
]

setorder(
  comparison_table,
  outcome,
  subgroup
)

print(comparison_table)

fwrite(
  comparison_table,
  file.path(
    output_folder,
    "step12_tehran_effect_comparison.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Sign and significance stability
# ==============================================================================

baseline_results <- tehran_results[
  specification == "Full sample",
  .(
    outcome,
    subgroup,
    baseline_coefficient = coefficient,
    baseline_sign = sign(coefficient),
    baseline_p_value = p_value
  )
]

stability_check <- merge(
  tehran_results[
    specification != "Full sample"
  ],
  baseline_results,
  by = c(
    "outcome",
    "subgroup"
  ),
  all.x = TRUE
)

stability_check[
  ,
  `:=`(
    sign_reversal =
      as.integer(
        sign(coefficient) != baseline_sign
      ),
    
    remained_significant_5pct =
      as.integer(
        baseline_p_value < 0.05 &
          p_value < 0.05
      ),
    
    became_insignificant_5pct =
      as.integer(
        baseline_p_value < 0.05 &
          p_value >= 0.05
      )
  )
]

fwrite(
  stability_check,
  file.path(
    output_folder,
    "step12_tehran_stability_check.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Final console summary
# ==============================================================================

cat(
  "\n============================================================\n",
  "TEHRAN EXCLUSION ROBUSTNESS COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain results:\n",
  file.path(
    output_folder,
    "step12_excluding_tehran_results.csv"
  ),
  "\n"
)

cat(
  "\nCompact table:\n",
  file.path(
    output_folder,
    "step12_excluding_tehran_compact.csv"
  ),
  "\n"
)

cat(
  "\nDirect comparison:\n",
  file.path(
    output_folder,
    "step12_tehran_effect_comparison.csv"
  ),
  "\n"
)

cat(
  "\nSample summary:\n",
  file.path(
    output_folder,
    "step12_tehran_sample_summary.csv"
  ),
  "\n"
)