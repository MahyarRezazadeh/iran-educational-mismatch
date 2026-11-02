# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 19: Alternative Mismatch Definitions
#
# Objectives:
#   1. Test whether the main results depend on the vertical-mismatch benchmark
#   2. Compare broad and strict horizontal-mismatch definitions
#   3. Reconstruct double mismatch and fully matched employment consistently
#   4. Estimate pooled, women, and men models
#
# Vertical definitions:
#   A. Period-specific occupation benchmark
#   B. Full-period occupation benchmark
#   C. Final preferred occupation benchmark
#
# Horizontal definitions:
#   A. Broad field-to-occupation crosswalk
#   B. Strict field-to-occupation crosswalk
#
# Central outcomes:
#   - Overeducation
#   - Undereducation
#   - Any vertical mismatch
#   - Horizontal mismatch
#   - Double mismatch
#   - Fully matched employment
#
# Treatment:
#   log(undergrad_exposure_18_20)
#
# Reported effects:
#   Percentage-point effect of a 10% increase in university exposure
#
# Fixed effects:
#   Historical province
#   Corrected birth cohort
#   Survey year
#
# Inference:
#   Survey-weighted linear probability models
#   Two-way clustering by historical province and birth cohort
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
  "step19_alternative_mismatch_definitions"
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
  # Identifiers, weights, and controls
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
  
  # Period-specific vertical mismatch
  "vertical_mismatch_eligible",
  "overeducated",
  "undereducated",
  "vertically_matched",
  "any_vertical_mismatch",
  "vertical_mismatch_period",
  
  # Full-period vertical mismatch
  "vertical_fullperiod_eligible",
  "overeducated_fullperiod",
  "undereducated_fullperiod",
  "vertically_matched_fullperiod",
  "vertical_mismatch_fullperiod",
  
  # Final preferred vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final",
  "vertical_mismatch_final",
  
  # Horizontal mismatch
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict",
  "horizontal_match_detailed_broad",
  "horizontal_match_detailed_strict"
)

cat("\nReading Step 19 variables...\n")

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


# ==============================================================================
# STEP 3: Construct analytical variables
# ==============================================================================

dt[
  ,
  historical_province := province_historical
]

dt[
  ,
  cohort := as.integer(
    analysis_birth_year
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
# STEP 4: Define vertical-mismatch samples
# ==============================================================================

dt[
  ,
  eligible_vertical_period :=
    sample_main == TRUE &
    employed == 1L &
    vertical_mismatch_eligible == 1L &
    !is.na(overeducated) &
    !is.na(undereducated) &
    !is.na(vertically_matched) &
    !is.na(any_vertical_mismatch)
]

dt[
  ,
  eligible_vertical_fullperiod :=
    sample_main == TRUE &
    employed == 1L &
    vertical_fullperiod_eligible == 1L &
    !is.na(overeducated_fullperiod) &
    !is.na(undereducated_fullperiod) &
    !is.na(vertically_matched_fullperiod) &
    !is.na(vertical_mismatch_fullperiod)
]

dt[
  ,
  eligible_vertical_final :=
    sample_main == TRUE &
    employed == 1L &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(undereducated_final) &
    !is.na(vertically_matched_final) &
    !is.na(vertical_mismatch_final)
]


# ==============================================================================
# STEP 5: Define horizontal-mismatch sample
# ==============================================================================

dt[
  ,
  eligible_horizontal :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(horizontal_mismatch_detailed_broad) &
    !is.na(horizontal_mismatch_detailed_strict)
]


# ==============================================================================
# STEP 6: Construct joint eligible samples
# ==============================================================================

dt[
  ,
  joint_period_eligible :=
    eligible_vertical_period == TRUE &
    eligible_horizontal == TRUE
]

dt[
  ,
  joint_fullperiod_eligible :=
    eligible_vertical_fullperiod == TRUE &
    eligible_horizontal == TRUE
]

dt[
  ,
  joint_final_eligible :=
    eligible_vertical_final == TRUE &
    eligible_horizontal == TRUE
]


# ==============================================================================
# STEP 7: Construct double-mismatch outcomes
# ==============================================================================

# Period-specific vertical + broad horizontal
dt[
  ,
  double_period_broad := fifelse(
    joint_period_eligible,
    as.integer(
      overeducated == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

# Period-specific vertical + strict horizontal
dt[
  ,
  double_period_strict := fifelse(
    joint_period_eligible,
    as.integer(
      overeducated == 1L &
        horizontal_mismatch_detailed_strict == 1L
    ),
    NA_integer_
  )
]

# Full-period vertical + broad horizontal
dt[
  ,
  double_fullperiod_broad := fifelse(
    joint_fullperiod_eligible,
    as.integer(
      overeducated_fullperiod == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

# Full-period vertical + strict horizontal
dt[
  ,
  double_fullperiod_strict := fifelse(
    joint_fullperiod_eligible,
    as.integer(
      overeducated_fullperiod == 1L &
        horizontal_mismatch_detailed_strict == 1L
    ),
    NA_integer_
  )
]

# Final preferred vertical + broad horizontal
dt[
  ,
  double_final_broad := fifelse(
    joint_final_eligible,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

# Final preferred vertical + strict horizontal
dt[
  ,
  double_final_strict := fifelse(
    joint_final_eligible,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_strict == 1L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 8: Construct fully matched outcomes
# ==============================================================================

dt[
  ,
  fully_matched_period_broad := fifelse(
    joint_period_eligible,
    as.integer(
      vertically_matched == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched_period_strict := fifelse(
    joint_period_eligible,
    as.integer(
      vertically_matched == 1L &
        horizontal_mismatch_detailed_strict == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched_fullperiod_broad := fifelse(
    joint_fullperiod_eligible,
    as.integer(
      vertically_matched_fullperiod == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched_fullperiod_strict := fifelse(
    joint_fullperiod_eligible,
    as.integer(
      vertically_matched_fullperiod == 1L &
        horizontal_mismatch_detailed_strict == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched_final_broad := fifelse(
    joint_final_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched_final_strict := fifelse(
    joint_final_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_strict == 0L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 9: Validate vertical classifications
# ==============================================================================

vertical_validation <- data.table(
  definition = c(
    "Period-specific",
    "Full-period",
    "Final preferred"
  ),
  
  observations = c(
    dt[eligible_vertical_period == TRUE, .N],
    dt[eligible_vertical_fullperiod == TRUE, .N],
    dt[eligible_vertical_final == TRUE, .N]
  ),
  
  invalid_category_sum = c(
    dt[
      eligible_vertical_period == TRUE,
      sum(
        overeducated +
          undereducated +
          vertically_matched != 1L,
        na.rm = TRUE
      )
    ],
    
    dt[
      eligible_vertical_fullperiod == TRUE,
      sum(
        overeducated_fullperiod +
          undereducated_fullperiod +
          vertically_matched_fullperiod != 1L,
        na.rm = TRUE
      )
    ],
    
    dt[
      eligible_vertical_final == TRUE,
      sum(
        overeducated_final +
          undereducated_final +
          vertically_matched_final != 1L,
        na.rm = TRUE
      )
    ]
  )
)

print(vertical_validation)

fwrite(
  vertical_validation,
  file.path(
    output_folder,
    "step19_vertical_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 10: Validate joint outcomes
# ==============================================================================

joint_validation <- data.table(
  definition = c(
    "Period + broad",
    "Period + strict",
    "Full-period + broad",
    "Full-period + strict",
    "Final + broad",
    "Final + strict"
  ),
  
  observations = c(
    dt[joint_period_eligible == TRUE, .N],
    dt[joint_period_eligible == TRUE, .N],
    dt[joint_fullperiod_eligible == TRUE, .N],
    dt[joint_fullperiod_eligible == TRUE, .N],
    dt[joint_final_eligible == TRUE, .N],
    dt[joint_final_eligible == TRUE, .N]
  ),
  
  double_mismatch_rate = c(
    dt[
      joint_period_eligible == TRUE,
      weighted.mean(
        double_period_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_period_eligible == TRUE,
      weighted.mean(
        double_period_strict,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_fullperiod_eligible == TRUE,
      weighted.mean(
        double_fullperiod_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_fullperiod_eligible == TRUE,
      weighted.mean(
        double_fullperiod_strict,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_final_eligible == TRUE,
      weighted.mean(
        double_final_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_final_eligible == TRUE,
      weighted.mean(
        double_final_strict,
        weight,
        na.rm = TRUE
      )
    ]
  ),
  
  fully_matched_rate = c(
    dt[
      joint_period_eligible == TRUE,
      weighted.mean(
        fully_matched_period_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_period_eligible == TRUE,
      weighted.mean(
        fully_matched_period_strict,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_fullperiod_eligible == TRUE,
      weighted.mean(
        fully_matched_fullperiod_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_fullperiod_eligible == TRUE,
      weighted.mean(
        fully_matched_fullperiod_strict,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_final_eligible == TRUE,
      weighted.mean(
        fully_matched_final_broad,
        weight,
        na.rm = TRUE
      )
    ],
    
    dt[
      joint_final_eligible == TRUE,
      weighted.mean(
        fully_matched_final_strict,
        weight,
        na.rm = TRUE
      )
    ]
  )
)

print(joint_validation)

fwrite(
  joint_validation,
  file.path(
    output_folder,
    "step19_joint_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Define all outcomes
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    # Period-specific vertical
    "Overeducation",
    "Undereducation",
    "Any vertical mismatch",
    
    # Full-period vertical
    "Overeducation",
    "Undereducation",
    "Any vertical mismatch",
    
    # Final preferred vertical
    "Overeducation",
    "Undereducation",
    "Any vertical mismatch",
    
    # Horizontal mismatch
    "Horizontal mismatch",
    "Horizontal mismatch",
    
    # Double mismatch
    "Double mismatch",
    "Double mismatch",
    "Double mismatch",
    "Double mismatch",
    "Double mismatch",
    "Double mismatch",
    
    # Fully matched
    "Fully matched",
    "Fully matched",
    "Fully matched",
    "Fully matched",
    "Fully matched",
    "Fully matched"
  ),
  
  definition = c(
    "Period-specific vertical",
    "Period-specific vertical",
    "Period-specific vertical",
    
    "Full-period vertical",
    "Full-period vertical",
    "Full-period vertical",
    
    "Final preferred vertical",
    "Final preferred vertical",
    "Final preferred vertical",
    
    "Broad horizontal",
    "Strict horizontal",
    
    "Period vertical + broad horizontal",
    "Period vertical + strict horizontal",
    "Full-period vertical + broad horizontal",
    "Full-period vertical + strict horizontal",
    "Final vertical + broad horizontal",
    "Final vertical + strict horizontal",
    
    "Period vertical + broad horizontal",
    "Period vertical + strict horizontal",
    "Full-period vertical + broad horizontal",
    "Full-period vertical + strict horizontal",
    "Final vertical + broad horizontal",
    "Final vertical + strict horizontal"
  ),
  
  outcome_variable = c(
    "overeducated",
    "undereducated",
    "any_vertical_mismatch",
    
    "overeducated_fullperiod",
    "undereducated_fullperiod",
    "vertical_mismatch_fullperiod",
    
    "overeducated_final",
    "undereducated_final",
    "vertical_mismatch_final",
    
    "horizontal_mismatch_detailed_broad",
    "horizontal_mismatch_detailed_strict",
    
    "double_period_broad",
    "double_period_strict",
    "double_fullperiod_broad",
    "double_fullperiod_strict",
    "double_final_broad",
    "double_final_strict",
    
    "fully_matched_period_broad",
    "fully_matched_period_strict",
    "fully_matched_fullperiod_broad",
    "fully_matched_fullperiod_strict",
    "fully_matched_final_broad",
    "fully_matched_final_strict"
  ),
  
  sample_variable = c(
    "eligible_vertical_period",
    "eligible_vertical_period",
    "eligible_vertical_period",
    
    "eligible_vertical_fullperiod",
    "eligible_vertical_fullperiod",
    "eligible_vertical_fullperiod",
    
    "eligible_vertical_final",
    "eligible_vertical_final",
    "eligible_vertical_final",
    
    "eligible_horizontal",
    "eligible_horizontal",
    
    "joint_period_eligible",
    "joint_period_eligible",
    "joint_fullperiod_eligible",
    "joint_fullperiod_eligible",
    "joint_final_eligible",
    "joint_final_eligible",
    
    "joint_period_eligible",
    "joint_period_eligible",
    "joint_fullperiod_eligible",
    "joint_fullperiod_eligible",
    "joint_final_eligible",
    "joint_final_eligible"
  ),
  
  outcome_family = c(
    rep("Vertical robustness", 9),
    rep("Horizontal robustness", 2),
    rep("Joint robustness", 12)
  )
)


# ==============================================================================
# STEP 12: Regression function
# ==============================================================================

estimate_model <- function(
    model_data,
    outcome_variable,
    subgroup
) {
  
  if (subgroup == "Pooled") {
    
    regression_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + female + urban | ",
        "historical_province + cohort + year"
      )
    )
    
  } else {
    
    regression_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ log_undergrad_exposure + urban | ",
        "historical_province + cohort + year"
      )
    )
  }
  
  feols(
    regression_formula,
    data = model_data,
    weights = ~weight,
    cluster = ~historical_province + cohort,
    notes = FALSE,
    warn = TRUE
  )
}


# ==============================================================================
# STEP 13: Extract results
# ==============================================================================

extract_result <- function(
    model,
    outcome,
    definition,
    outcome_family,
    subgroup,
    model_data
) {
  
  treatment_term <- "log_undergrad_exposure"
  
  coefficient <- unname(
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
  
  conversion_factor <- 100 * log(1.10)
  
  data.table(
    outcome = outcome,
    definition = definition,
    outcome_family = outcome_family,
    subgroup = subgroup,
    
    effect_pp_for_10_percent_increase =
      conversion_factor * coefficient,
    
    standard_error_pp =
      conversion_factor * standard_error,
    
    lower_95_pp =
      conversion_factor *
      confidence_interval[1, 1],
    
    upper_95_pp =
      conversion_factor *
      confidence_interval[1, 2],
    
    p_value = p_value,
    
    significance = fcase(
      p_value < 0.01, "***",
      p_value < 0.05, "**",
      p_value < 0.10, "*",
      default = ""
    ),
    
    observations = nobs(model),
    
    historical_province_clusters =
      uniqueN(model_data$historical_province),
    
    cohort_clusters =
      uniqueN(model_data$cohort),
    
    survey_years =
      uniqueN(model_data$year)
  )
}


# ==============================================================================
# STEP 14: Estimate all models
# ==============================================================================

regression_results <- list()
failed_models <- list()

result_counter <- 1L
failure_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome <-
    outcome_definitions$outcome[outcome_index]
  
  current_definition <-
    outcome_definitions$definition[outcome_index]
  
  current_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_sample <-
    outcome_definitions$sample_variable[outcome_index]
  
  current_family <-
    outcome_definitions$outcome_family[outcome_index]
  
  for (
    current_subgroup in
    c(
      "Pooled",
      "Women",
      "Men"
    )
  ) {
    
    cat(
      "\nEstimating:",
      current_outcome,
      "|",
      current_definition,
      "|",
      current_subgroup,
      "\n"
    )
    
    model_data <- dt[
      get(current_sample) == TRUE &
        !is.na(get(current_variable))
    ]
    
    if (current_subgroup == "Women") {
      model_data <- model_data[
        gender == 1L
      ]
    }
    
    if (current_subgroup == "Men") {
      model_data <- model_data[
        gender == 2L
      ]
    }
    
    if (
      nrow(model_data) < 500L ||
      uniqueN(model_data$historical_province) < 5L ||
      uniqueN(model_data$cohort) < 5L
    ) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = current_outcome,
        definition = current_definition,
        subgroup = current_subgroup,
        reason = "Insufficient observations or clusters",
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      next
    }
    
    current_model <- tryCatch(
      estimate_model(
        model_data = model_data,
        outcome_variable = current_variable,
        subgroup = current_subgroup
      ),
      error = function(e) e
    )
    
    if (inherits(current_model, "error")) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = current_outcome,
        definition = current_definition,
        subgroup = current_subgroup,
        reason = current_model$message,
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      next
    }
    
    regression_results[[result_counter]] <-
      extract_result(
        model = current_model,
        outcome = current_outcome,
        definition = current_definition,
        outcome_family = current_family,
        subgroup = current_subgroup,
        model_data = model_data
      )
    
    result_counter <- result_counter + 1L
    
    rm(
      current_model,
      model_data
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 15: Combine results
# ==============================================================================

alternative_mismatch_results <- rbindlist(
  regression_results,
  fill = TRUE
)

setorder(
  alternative_mismatch_results,
  outcome,
  definition,
  subgroup
)


# ==============================================================================
# STEP 16: Multiple-testing adjustment
# ==============================================================================

alternative_mismatch_results[
  ,
  p_value_fdr_within_outcome_subgroup :=
    p.adjust(
      p_value,
      method = "BH"
    ),
  by = .(
    outcome,
    subgroup
  )
]

alternative_mismatch_results[
  ,
  p_value_fdr_within_family_subgroup :=
    p.adjust(
      p_value,
      method = "BH"
    ),
  by = .(
    outcome_family,
    subgroup
  )
]


# ==============================================================================
# STEP 17: Add robustness indicators
# ==============================================================================

alternative_mismatch_results[
  ,
  survives_fdr_10 :=
    p_value_fdr_within_outcome_subgroup < 0.10
]

alternative_mismatch_results[
  ,
  survives_fdr_05 :=
    p_value_fdr_within_outcome_subgroup < 0.05
]

alternative_mismatch_results[
  ,
  reliable_sample :=
    observations >= fifelse(
      subgroup == "Pooled",
      5000L,
      2000L
    ) &
    historical_province_clusters >= 15L &
    cohort_clusters >= 15L &
    survey_years >= 5L
]


# ==============================================================================
# STEP 18: Save full regression results
# ==============================================================================

fwrite(
  alternative_mismatch_results,
  file.path(
    output_folder,
    "step19_alternative_mismatch_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Compact results table
# ==============================================================================

compact_results <- alternative_mismatch_results[
  ,
  .(
    outcome,
    definition,
    outcome_family,
    subgroup,
    
    estimate =
      round(
        effect_pp_for_10_percent_increase,
        3
      ),
    
    standard_error =
      round(
        standard_error_pp,
        3
      ),
    
    lower_95 =
      round(
        lower_95_pp,
        3
      ),
    
    upper_95 =
      round(
        upper_95_pp,
        3
      ),
    
    p_value =
      round(
        p_value,
        4
      ),
    
    fdr_outcome =
      round(
        p_value_fdr_within_outcome_subgroup,
        4
      ),
    
    fdr_family =
      round(
        p_value_fdr_within_family_subgroup,
        4
      ),
    
    significance,
    survives_fdr_10,
    survives_fdr_05,
    observations,
    reliable_sample
  )
]

compact_results[
  ,
  estimate_with_stars :=
    paste0(
      formatC(
        estimate,
        format = "f",
        digits = 3
      ),
      significance
    )
]

compact_results[
  ,
  standard_error_parentheses :=
    paste0(
      "(",
      formatC(
        standard_error,
        format = "f",
        digits = 3
      ),
      ")"
    )
]

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step19_alternative_mismatch_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Main pooled robustness table
# ==============================================================================

main_pooled_results <- compact_results[
  subgroup == "Pooled" &
    outcome %in% c(
      "Overeducation",
      "Undereducation",
      "Horizontal mismatch",
      "Double mismatch",
      "Fully matched"
    ) &
    reliable_sample == TRUE,
  .(
    outcome,
    definition,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    fdr_outcome,
    observations
  )
]

main_pooled_results[
  ,
  table_entry := paste0(
    estimate_with_stars,
    " ",
    standard_error_parentheses
  )
]

main_pooled_table_wide <- dcast(
  main_pooled_results,
  definition ~ outcome,
  value.var = "table_entry"
)

fwrite(
  main_pooled_table_wide,
  file.path(
    output_folder,
    "step19_main_pooled_robustness_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Gender robustness table for central joint outcomes
# ==============================================================================

gender_joint_results <- compact_results[
  outcome %in% c(
    "Double mismatch",
    "Fully matched",
    "Horizontal mismatch"
  ) &
    definition %in% c(
      "Broad horizontal",
      "Strict horizontal",
      "Final vertical + broad horizontal",
      "Final vertical + strict horizontal"
    ) &
    reliable_sample == TRUE,
  .(
    outcome,
    definition,
    subgroup,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    fdr_outcome
  )
]

gender_joint_results[
  ,
  table_entry := paste0(
    estimate_with_stars,
    " ",
    standard_error_parentheses
  )
]

gender_joint_table_wide <- dcast(
  gender_joint_results,
  outcome + definition ~ subgroup,
  value.var = "table_entry"
)

fwrite(
  gender_joint_table_wide,
  file.path(
    output_folder,
    "step19_gender_joint_robustness_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Sign-stability summary
# ==============================================================================

sign_stability_summary <- alternative_mismatch_results[
  reliable_sample == TRUE,
  .(
    number_of_definitions = .N,
    
    minimum_effect =
      min(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    maximum_effect =
      max(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    all_positive =
      all(
        effect_pp_for_10_percent_increase > 0
      ),
    
    all_negative =
      all(
        effect_pp_for_10_percent_increase < 0
      ),
    
    share_p_below_010 =
      mean(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    share_fdr_below_010 =
      mean(
        p_value_fdr_within_outcome_subgroup < 0.10,
        na.rm = TRUE
      )
  ),
  by = .(
    outcome,
    subgroup
  )
]

print(sign_stability_summary)

fwrite(
  sign_stability_summary,
  file.path(
    output_folder,
    "step19_sign_stability_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Results surviving FDR correction
# ==============================================================================

results_surviving_fdr <- alternative_mismatch_results[
  reliable_sample == TRUE &
    p_value_fdr_within_outcome_subgroup < 0.10
][
  order(
    outcome,
    subgroup,
    p_value_fdr_within_outcome_subgroup
  )
]

fwrite(
  results_surviving_fdr,
  file.path(
    output_folder,
    "step19_results_surviving_FDR.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Save failed-model log
# ==============================================================================

if (length(failed_models) > 0L) {
  
  failed_model_table <- rbindlist(
    failed_models,
    fill = TRUE
  )
  
} else {
  
  failed_model_table <- data.table(
    outcome = character(),
    definition = character(),
    subgroup = character(),
    reason = character(),
    observations = integer()
  )
}

fwrite(
  failed_model_table,
  file.path(
    output_folder,
    "step19_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Diagnostic summary
# ==============================================================================

diagnostic_summary <- alternative_mismatch_results[
  ,
  .(
    models_estimated = .N,
    
    reliable_models =
      sum(
        reliable_sample,
        na.rm = TRUE
      ),
    
    conventional_p_below_010 =
      sum(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    conventional_p_below_005 =
      sum(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    FDR_p_below_010 =
      sum(
        p_value_fdr_within_outcome_subgroup < 0.10,
        na.rm = TRUE
      )
  ),
  by = .(
    outcome,
    subgroup
  )
]

print(diagnostic_summary)

fwrite(
  diagnostic_summary,
  file.path(
    output_folder,
    "step19_diagnostic_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Print central results
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 19 CENTRAL ROBUSTNESS RESULTS\n",
  "============================================================\n"
)

print(
  alternative_mismatch_results[
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
      
      effect_pp =
        round(
          effect_pp_for_10_percent_increase,
          3
        ),
      
      standard_error =
        round(
          standard_error_pp,
          3
        ),
      
      p_value =
        round(
          p_value,
          4
        ),
      
      FDR =
        round(
          p_value_fdr_within_outcome_subgroup,
          4
        ),
      
      observations
    )
  ]
)


# ==============================================================================
# STEP 27: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 19 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step19_vertical_validation.csv\n",
  "2. step19_joint_validation.csv\n",
  "3. step19_alternative_mismatch_regression_results.csv\n",
  "4. step19_alternative_mismatch_compact_results.csv\n",
  "5. step19_main_pooled_robustness_table_wide.csv\n",
  "6. step19_gender_joint_robustness_table_wide.csv\n",
  "7. step19_sign_stability_summary.csv\n",
  "8. step19_results_surviving_FDR.csv\n",
  "9. step19_failed_models.csv\n",
  "10. step19_diagnostic_summary.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nInterpretation priorities:\n",
  "- Check whether double mismatch remains positive across all definitions.\n",
  "- Check whether fully matched employment remains negative.\n",
  "- Compare broad and strict horizontal definitions.\n",
  "- Compare period-specific, full-period, and final vertical benchmarks.\n",
  "- Emphasize sign stability and effect-size similarity, not only significance.\n"
)