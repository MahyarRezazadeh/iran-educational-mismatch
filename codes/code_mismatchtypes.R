# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 14A: Identify variables for selection and joint-mismatch analysis
# ==============================================================================

rm(list = ls())
invisible(gc())

library(fst)

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "File not found:\n",
      input_file
    )
  )
}

# Read one row only to retrieve all column names.
all_variable_names <- names(
  read_fst(
    input_file,
    from = 1,
    to = 1
  )
)

# Search for variables potentially related to:
# - education level
# - tertiary education
# - occupation
# - field of study
# - mismatch
candidate_variables <- all_variable_names[
  grepl(
    paste0(
      "educ|degree|school|tertiary|university|",
      "field|isced|occupation|isco|",
      "vertical|horizontal|overeduc|undereduc|matched"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]

cat(
  "\n============================================================\n",
  "CANDIDATE VARIABLES\n",
  "============================================================\n"
)

print(candidate_variables)

writeLines(
  candidate_variables,
  file.path(
    project_folder,
    "LFS_analysis_outputs",
    "step14_candidate_variable_names.txt"
  )
)

# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 14: Selection Pathways and Joint Mismatch Outcomes
#
# Objectives:
#   1. Examine selection into tertiary education and employment
#   2. Estimate the unconditional probability of holding a mismatched graduate job
#   3. Decompose joint vertical and horizontal mismatch
#   4. Report pooled, women, and men estimates
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Fixed effects:
#   historical province
#   corrected birth cohort
#   survey year
#
# Inference:
#   two-way clustered by historical province and birth cohort
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 250
)

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
  "step14_selection_joint_mismatch"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Final analysis-ready dataset not found:\n",
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
  
  # Employment and education
  "employed",
  "tertiary_educated",
  
  # Vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final",
  "vertical_mismatch_final",
  
  # Horizontal mismatch
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict",
  
  # Existing refined joint variables
  "fully_matched_refined",
  "overeducated_and_horizontal_mismatch_refined",
  "both_refined_vertical_horizontal_mismatch",
  "either_refined_vertical_horizontal_mismatch",
  
  # Migration robustness
  "recent_cross_historical_region_move"
)

cat("\nReading Step 14 variables...\n")

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

cat(
  "\nMain analytical sample:",
  format(
    dt[sample_main == TRUE, .N],
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 5: Construct selection-pathway outcomes
#
# These outcomes are defined for the full analytical population.
# ==============================================================================

dt[
  ,
  tertiary_outcome := fifelse(
    sample_main == TRUE &
      !is.na(tertiary_educated),
    as.integer(tertiary_educated == 1L),
    NA_integer_
  )
]

dt[
  ,
  tertiary_employed := fifelse(
    sample_main == TRUE &
      !is.na(tertiary_educated) &
      !is.na(employed),
    as.integer(
      tertiary_educated == 1L &
        employed == 1L
    ),
    NA_integer_
  )
]

# Eligible for preferred detailed horizontal-mismatch classification.
dt[
  ,
  horizontal_eligible_fullsample := fifelse(
    sample_main == TRUE,
    as.integer(
      employed == 1L &
        tertiary_educated == 1L &
        horizontal_detailed_eligible == 1L &
        !is.na(horizontal_mismatch_detailed_broad)
    ),
    NA_integer_
  )
]

# Unconditional probability of being tertiary educated, employed,
# and horizontally mismatched.
dt[
  ,
  mismatched_graduate_job := fifelse(
    sample_main == TRUE &
      !is.na(tertiary_educated) &
      !is.na(employed),
    as.integer(
      tertiary_educated == 1L &
        employed == 1L &
        horizontal_detailed_eligible == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

# Strict-definition counterpart.
dt[
  ,
  mismatched_graduate_job_strict := fifelse(
    sample_main == TRUE &
      !is.na(tertiary_educated) &
      !is.na(employed),
    as.integer(
      tertiary_educated == 1L &
        employed == 1L &
        horizontal_detailed_eligible == 1L &
        horizontal_mismatch_detailed_strict == 1L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 6: Define joint mismatch eligible sample
#
# Joint outcomes require:
#   employed
#   valid final vertical mismatch classification
#   valid detailed horizontal mismatch classification
# ==============================================================================

dt[
  ,
  joint_mismatch_eligible :=
    sample_main == TRUE &
    employed == 1L &
    vertical_fullperiod_eligible_final == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(overeducated_final) &
    !is.na(undereducated_final) &
    !is.na(vertically_matched_final) &
    !is.na(horizontal_mismatch_detailed_broad)
]


# ==============================================================================
# STEP 7: Construct exhaustive joint mismatch categories
# ==============================================================================

dt[
  ,
  fully_matched_joint := fifelse(
    joint_mismatch_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  horizontal_only := fifelse(
    joint_mismatch_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  overeducation_only := fifelse(
    joint_mismatch_eligible,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  double_mismatch_over_horizontal := fifelse(
    joint_mismatch_eligible,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  undereducation_only := fifelse(
    joint_mismatch_eligible,
    as.integer(
      undereducated_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  undereducation_horizontal := fifelse(
    joint_mismatch_eligible,
    as.integer(
      undereducated_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 8: Validate exhaustive categories
# ==============================================================================

joint_category_check <- dt[
  joint_mismatch_eligible == TRUE,
  .(
    observations = .N,
    
    invalid_category_sum = sum(
      fully_matched_joint +
        horizontal_only +
        overeducation_only +
        double_mismatch_over_horizontal +
        undereducation_only +
        undereducation_horizontal != 1L,
      na.rm = TRUE
    ),
    
    disagreement_with_existing_double = sum(
      !is.na(
        overeducated_and_horizontal_mismatch_refined
      ) &
        double_mismatch_over_horizontal !=
        overeducated_and_horizontal_mismatch_refined,
      na.rm = TRUE
    )
  )
]

print(joint_category_check)

fwrite(
  joint_category_check,
  file.path(
    output_folder,
    "step14_joint_category_validation.csv"
  ),
  bom = TRUE
)

if (joint_category_check$invalid_category_sum > 0L) {
  warning(
    "The six joint mismatch categories do not sum to one for all eligible observations."
  )
}


# ==============================================================================
# STEP 9: Define outcomes
# ==============================================================================

selection_outcomes <- data.table(
  outcome = c(
    "Tertiary educated",
    "Tertiary educated and employed",
    "Horizontal-mismatch eligible",
    "Mismatched graduate job: broad",
    "Mismatched graduate job: strict"
  ),
  
  outcome_variable = c(
    "tertiary_outcome",
    "tertiary_employed",
    "horizontal_eligible_fullsample",
    "mismatched_graduate_job",
    "mismatched_graduate_job_strict"
  ),
  
  sample_type = "Full population"
)

joint_outcomes <- data.table(
  outcome = c(
    "Fully matched",
    "Horizontal mismatch only",
    "Overeducation only",
    "Double mismatch: overeducated and horizontal",
    "Undereducation only",
    "Undereducation and horizontal mismatch"
  ),
  
  outcome_variable = c(
    "fully_matched_joint",
    "horizontal_only",
    "overeducation_only",
    "double_mismatch_over_horizontal",
    "undereducation_only",
    "undereducation_horizontal"
  ),
  
  sample_type = "Joint mismatch eligible"
)

outcome_definitions <- rbindlist(
  list(
    selection_outcomes,
    joint_outcomes
  )
)


# ==============================================================================
# STEP 10: Weighted descriptive rates
# ==============================================================================

weighted_mean_safe <- function(x, w) {
  
  valid <- (
    !is.na(x) &
      !is.na(w) &
      is.finite(w) &
      w > 0
  )
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  weighted.mean(
    x[valid],
    w[valid]
  )
}

descriptive_results <- list()
counter <- 1L

for (i in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[i]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[i]
  
  sample_type <-
    outcome_definitions$sample_type[i]
  
  for (subgroup_name in c(
    "Pooled",
    "Women",
    "Men"
  )) {
    
    current_data <- dt[
      sample_main == TRUE &
        !is.na(get(outcome_variable))
    ]
    
    if (subgroup_name == "Women") {
      current_data <- current_data[
        gender == 1L
      ]
    }
    
    if (subgroup_name == "Men") {
      current_data <- current_data[
        gender == 2L
      ]
    }
    
    descriptive_results[[counter]] <- data.table(
      outcome = outcome_name,
      sample_type = sample_type,
      subgroup = subgroup_name,
      
      weighted_rate =
        weighted_mean_safe(
          current_data[[outcome_variable]],
          current_data$weight
        ),
      
      weighted_rate_percent =
        100 *
        weighted_mean_safe(
          current_data[[outcome_variable]],
          current_data$weight
        ),
      
      observations = nrow(current_data)
    )
    
    counter <- counter + 1L
  }
}

selection_joint_descriptives <- rbindlist(
  descriptive_results
)

print(selection_joint_descriptives)

fwrite(
  selection_joint_descriptives,
  file.path(
    output_folder,
    "step14_selection_joint_descriptives.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Regression function
# ==============================================================================

estimate_gender_models <- function(
    data,
    outcome_variable
) {
  
  analysis_data <- data[
    sample_main == TRUE &
      !is.na(get(outcome_variable))
  ]
  
  pooled_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + female + urban | ",
        "historical_province + cohort + year"
      )
    ),
    data = analysis_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  women_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + urban | ",
        "historical_province + cohort + year"
      )
    ),
    data = analysis_data[
      gender == 1L
    ],
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  men_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + urban | ",
        "historical_province + cohort + year"
      )
    ),
    data = analysis_data[
      gender == 2L
    ],
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  list(
    Pooled = pooled_model,
    Women = women_model,
    Men = men_model
  )
}


# ==============================================================================
# STEP 12: Extract treatment result
# ==============================================================================

extract_result <- function(
    model,
    outcome_name,
    sample_type,
    subgroup_name
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
  
  multiplier <- 100 * log(1.10)
  
  data.table(
    outcome = outcome_name,
    sample_type = sample_type,
    subgroup = subgroup_name,
    
    coefficient = estimate,
    standard_error = standard_error,
    p_value = p_value,
    
    effect_pp_for_10_percent_increase =
      multiplier * estimate,
    
    standard_error_pp_for_10_percent =
      multiplier * standard_error,
    
    lower_95_pp_for_10_percent =
      multiplier * confidence_interval[1, 1],
    
    upper_95_pp_for_10_percent =
      multiplier * confidence_interval[1, 2],
    
    observations = nobs(model)
  )
}


# ==============================================================================
# STEP 13: Estimate all selection and joint mismatch models
# ==============================================================================

all_results <- list()
result_counter <- 1L

for (i in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[i]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[i]
  
  sample_type <-
    outcome_definitions$sample_type[i]
  
  cat(
    "\n============================================================\n",
    "Estimating:",
    outcome_name,
    "\n",
    "============================================================\n"
  )
  
  current_models <- estimate_gender_models(
    data = dt,
    outcome_variable = outcome_variable
  )
  
  for (subgroup_name in names(current_models)) {
    
    all_results[[result_counter]] <- extract_result(
      model = current_models[[subgroup_name]],
      outcome_name = outcome_name,
      sample_type = sample_type,
      subgroup_name = subgroup_name
    )
    
    result_counter <- result_counter + 1L
  }
  
  rm(current_models)
  invisible(gc())
}

selection_joint_results <- rbindlist(
  all_results
)

setorder(
  selection_joint_results,
  sample_type,
  outcome,
  subgroup
)

print(selection_joint_results)

fwrite(
  selection_joint_results,
  file.path(
    output_folder,
    "step14_selection_joint_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 14: Compact publication-style results
# ==============================================================================

compact_results <- selection_joint_results[
  ,
  .(
    outcome,
    sample_type,
    subgroup,
    
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
    
    observations
  )
]

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step14_selection_joint_compact.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Gender-specific fixed-effect interaction tests
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

interaction_results <- list()

for (i in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[i]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[i]
  
  interaction_data <- dt[
    sample_main == TRUE &
      !is.na(get(outcome_variable))
  ]
  
  interaction_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure * female + urban | ",
        "province_gender + cohort_gender + year_gender"
      )
    ),
    data = interaction_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  coefficient_vector <- coef(
    interaction_model
  )
  
  variance_matrix <- vcov(
    interaction_model
  )
  
  treatment_term <-
    "log_undergrad_exposure"
  
  interaction_term <-
    "log_undergrad_exposure:female"
  
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
  
  se_male <-
    sqrt(variance_male)
  
  se_difference <-
    sqrt(variance_difference)
  
  se_female <-
    sqrt(
      variance_male +
        variance_difference +
        2 * covariance_term
    )
  
  multiplier <-
    100 * log(1.10)
  
  interaction_results[[i]] <- data.table(
    outcome = outcome_name,
    
    male_effect_pp_10pct =
      multiplier * beta_male,
    
    female_effect_pp_10pct =
      multiplier * beta_female,
    
    female_minus_male_pp_10pct =
      multiplier * beta_difference,
    
    difference_standard_error_pp =
      multiplier * se_difference,
    
    difference_p_value =
      2 * pnorm(
        abs(
          beta_difference /
            se_difference
        ),
        lower.tail = FALSE
      ),
    
    observations =
      nobs(interaction_model)
  )
  
  rm(
    interaction_model,
    interaction_data
  )
  
  invisible(gc())
}

gender_interaction_results <- rbindlist(
  interaction_results
)

print(gender_interaction_results)

fwrite(
  gender_interaction_results,
  file.path(
    output_folder,
    "step14_gender_interaction_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Sample summary
# ==============================================================================

sample_summary <- data.table(
  sample = c(
    "Main analytical population",
    "Joint vertical-horizontal mismatch eligible"
  ),
  
  observations = c(
    dt[sample_main == TRUE, .N],
    dt[joint_mismatch_eligible == TRUE, .N]
  ),
  
  women = c(
    dt[
      sample_main == TRUE &
        gender == 1L,
      .N
    ],
    
    dt[
      joint_mismatch_eligible == TRUE &
        gender == 1L,
      .N
    ]
  ),
  
  men = c(
    dt[
      sample_main == TRUE &
        gender == 2L,
      .N
    ],
    
    dt[
      joint_mismatch_eligible == TRUE &
        gender == 2L,
      .N
    ]
  )
)

print(sample_summary)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "step14_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 14 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step14_joint_category_validation.csv\n",
  "2. step14_selection_joint_descriptives.csv\n",
  "3. step14_selection_joint_regression_results.csv\n",
  "4. step14_selection_joint_compact.csv\n",
  "5. step14_gender_interaction_results.csv\n",
  "6. step14_sample_summary.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)
