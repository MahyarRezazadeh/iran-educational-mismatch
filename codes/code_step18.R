# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 18: Graduate Supply and High-Skill Labor-Demand Mechanism
#
# Main questions:
#   1. Did university exposure increase the share of employed workers who
#      possess tertiary education?
#   2. Did exposure increase the availability of high-skill employment?
#   3. Did graduate supply grow faster than high-skill occupational absorption?
#   4. Did exposure increase the probability of being tertiary educated but
#      employed in a lower-skill occupation?
#
# Individual-level outcomes:
#   - Tertiary educated among employed workers
#   - High-skill occupation among employed workers
#   - Professional occupation among employed workers
#   - Tertiary educated and in a high-skill occupation
#   - Tertiary educated and in a lower-skill occupation
#   - Tertiary educated and in a professional occupation
#
# Descriptive province-year measures:
#   - Tertiary-educated employment share
#   - High-skill employment share
#   - Professional employment share
#   - Graduate-pressure ratio:
#         tertiary-educated workers / high-skill workers
#
# Treatment:
#   log(undergrad_exposure_18_20)
#
# Reported regression effects:
#   Percentage-point effect associated with a 10% increase in exposure
#
# Fixed effects:
#   Historical province
#   Corrected birth cohort
#   Survey year
#
# Controls:
#   Pooled models: female and urban
#   Gender-specific models: urban
#
# Inference:
#   Survey-weighted linear probability models
#   Two-way clustering by historical province and corrected birth cohort
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 400
)

library(data.table)
library(fst)
library(fixest)


# ==============================================================================
# STEP 1: Define project paths
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
  "step18_labor_demand_mechanism"
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
  # Identification, controls, and weights
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
  
  # Occupation
  "isco_2digit",
  "isco_2digit_before_final_validation",
  "occupation_code_standardized",
  
  # Existing mismatch outcomes for comparison
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final"
)

cat("\nReading Step 18 variables...\n")

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
# STEP 4: Clean ISCO occupation codes
# ==============================================================================

dt[
  ,
  isco2_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isco_2digit)
      )
    )
  )
]

dt[
  ,
  isco_major_group := fcase(
    isco2_clean >= 10L &
      isco2_clean <= 99L,
    as.integer(
      floor(isco2_clean / 10)
    ),
    
    isco2_clean >= 1L &
      isco2_clean <= 9L,
    isco2_clean,
    
    default = NA_integer_
  )
]

dt[
  !isco_major_group %in% 1:9,
  isco_major_group := NA_integer_
]


# ==============================================================================
# STEP 5: Construct occupation indicators
#
# Standard broad skill classification:
#   High skill: ISCO major groups 1–3
#   Lower skill: ISCO major groups 4–9
#
# Professional occupations:
#   ISCO major group 2
# ==============================================================================

dt[
  ,
  valid_occupation :=
    employed == 1L &
    !is.na(isco_major_group)
]

dt[
  ,
  high_skill_job := fifelse(
    valid_occupation,
    as.integer(
      isco_major_group %in% 1:3
    ),
    NA_integer_
  )
]

dt[
  ,
  lower_skill_job := fifelse(
    valid_occupation,
    as.integer(
      isco_major_group %in% 4:9
    ),
    NA_integer_
  )
]

dt[
  ,
  professional_job := fifelse(
    valid_occupation,
    as.integer(
      isco_major_group == 2L
    ),
    NA_integer_
  )
]

dt[
  ,
  manager_job := fifelse(
    valid_occupation,
    as.integer(
      isco_major_group == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  technician_job := fifelse(
    valid_occupation,
    as.integer(
      isco_major_group == 3L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 6: Define the employed analytical sample
# ==============================================================================

dt[
  ,
  employed_sample :=
    sample_main == TRUE &
    employed == 1L &
    !is.na(tertiary_educated)
]

dt[
  ,
  employed_occupation_sample :=
    employed_sample == TRUE &
    valid_occupation == TRUE
]


# ==============================================================================
# STEP 7: Construct labor-supply and labor-demand mechanism outcomes
#
# Outcomes are defined for employed workers.
#
# The joint outcomes identify whether an employed person is:
#   - tertiary educated and in a high-skill occupation
#   - tertiary educated and in a lower-skill occupation
#   - tertiary educated and in a professional occupation
# ==============================================================================

dt[
  ,
  employed_tertiary := fifelse(
    employed_sample,
    as.integer(
      tertiary_educated == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  employed_high_skill := fifelse(
    employed_occupation_sample,
    high_skill_job,
    NA_integer_
  )
]

dt[
  ,
  employed_professional := fifelse(
    employed_occupation_sample,
    professional_job,
    NA_integer_
  )
]

dt[
  ,
  tertiary_high_skill := fifelse(
    employed_occupation_sample,
    as.integer(
      tertiary_educated == 1L &
        high_skill_job == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  tertiary_lower_skill := fifelse(
    employed_occupation_sample,
    as.integer(
      tertiary_educated == 1L &
        lower_skill_job == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  tertiary_professional := fifelse(
    employed_occupation_sample,
    as.integer(
      tertiary_educated == 1L &
        professional_job == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  nontertiary_high_skill := fifelse(
    employed_occupation_sample,
    as.integer(
      tertiary_educated == 0L &
        high_skill_job == 1L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 8: Validate outcome construction
# ==============================================================================

outcome_validation <- dt[
  employed_occupation_sample == TRUE,
  .(
    observations = .N,
    
    invalid_high_low_sum =
      sum(
        employed_high_skill +
          lower_skill_job != 1L,
        na.rm = TRUE
      ),
    
    invalid_tertiary_destination_sum =
      sum(
        tertiary_high_skill +
          tertiary_lower_skill !=
          employed_tertiary,
        na.rm = TRUE
      ),
    
    tertiary_observations =
      sum(
        tertiary_educated == 1L,
        na.rm = TRUE
      ),
    
    nontertiary_observations =
      sum(
        tertiary_educated == 0L,
        na.rm = TRUE
      )
  )
]

print(outcome_validation)

fwrite(
  outcome_validation,
  file.path(
    output_folder,
    "step18_outcome_validation.csv"
  ),
  bom = TRUE
)

if (
  outcome_validation$invalid_high_low_sum > 0L ||
  outcome_validation$invalid_tertiary_destination_sum > 0L
) {
  stop(
    "Step 18 labor-demand outcomes failed internal validation."
  )
}


# ==============================================================================
# STEP 9: Define outcomes
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Tertiary educated among employed workers",
    "High-skill occupation among employed workers",
    "Professional occupation among employed workers",
    "Tertiary educated and in a high-skill occupation",
    "Tertiary educated and in a lower-skill occupation",
    "Tertiary educated and in a professional occupation",
    "Non-tertiary educated and in a high-skill occupation"
  ),
  
  outcome_variable = c(
    "employed_tertiary",
    "employed_high_skill",
    "employed_professional",
    "tertiary_high_skill",
    "tertiary_lower_skill",
    "tertiary_professional",
    "nontertiary_high_skill"
  ),
  
  sample_variable = c(
    "employed_sample",
    "employed_occupation_sample",
    "employed_occupation_sample",
    "employed_occupation_sample",
    "employed_occupation_sample",
    "employed_occupation_sample",
    "employed_occupation_sample"
  ),
  
  outcome_family = c(
    "Primary",
    "Primary",
    "Secondary",
    "Primary",
    "Primary",
    "Secondary",
    "Secondary"
  )
)


# ==============================================================================
# STEP 10: Weighted-mean helper
# ==============================================================================

safe_weighted_mean <- function(
    x,
    w
) {
  
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


# ==============================================================================
# STEP 11: Weighted descriptive rates
# ==============================================================================

descriptive_results <- list()
descriptive_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome <-
    outcome_definitions$outcome[outcome_index]
  
  current_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_sample <-
    outcome_definitions$sample_variable[outcome_index]
  
  for (
    current_group in
    c(
      "Pooled",
      "Women",
      "Men"
    )
  ) {
    
    current_data <- dt[
      get(current_sample) == TRUE &
        !is.na(get(current_variable))
    ]
    
    if (current_group == "Women") {
      current_data <- current_data[
        gender == 1L
      ]
    }
    
    if (current_group == "Men") {
      current_data <- current_data[
        gender == 2L
      ]
    }
    
    descriptive_results[[descriptive_counter]] <-
      data.table(
        outcome =
          current_outcome,
        
        analysis_group =
          current_group,
        
        weighted_rate =
          safe_weighted_mean(
            current_data[[current_variable]],
            current_data$weight
          ),
        
        weighted_rate_percent =
          100 *
          safe_weighted_mean(
            current_data[[current_variable]],
            current_data$weight
          ),
        
        observations =
          nrow(current_data),
        
        historical_provinces =
          uniqueN(
            current_data$historical_province
          ),
        
        cohorts =
          uniqueN(
            current_data$cohort
          ),
        
        survey_years =
          uniqueN(
            current_data$year
          )
      )
    
    descriptive_counter <-
      descriptive_counter + 1L
  }
}

mechanism_descriptives <- rbindlist(
  descriptive_results,
  fill = TRUE
)

setorder(
  mechanism_descriptives,
  outcome,
  analysis_group
)

fwrite(
  mechanism_descriptives,
  file.path(
    output_folder,
    "step18_mechanism_descriptive_rates.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Regression-estimation function
# ==============================================================================

estimate_mechanism_model <- function(
    data,
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
    data = data,
    weights = ~weight,
    cluster = ~historical_province + cohort,
    notes = FALSE,
    warn = TRUE
  )
}


# ==============================================================================
# STEP 13: Extract regression results
# ==============================================================================

extract_mechanism_result <- function(
    model,
    outcome_name,
    outcome_family,
    subgroup,
    model_data
) {
  
  treatment_term <-
    "log_undergrad_exposure"
  
  coefficient <-
    unname(
      coef(model)[treatment_term]
    )
  
  standard_error <-
    unname(
      se(model)[treatment_term]
    )
  
  p_value <-
    unname(
      pvalue(model)[treatment_term]
    )
  
  confidence_interval <- confint(
    model,
    parm = treatment_term,
    level = 0.95
  )
  
  conversion_factor <-
    100 * log(1.10)
  
  data.table(
    outcome =
      outcome_name,
    
    outcome_family =
      outcome_family,
    
    analysis_group =
      subgroup,
    
    effect_pp_for_10_percent_increase =
      conversion_factor *
      coefficient,
    
    standard_error_pp =
      conversion_factor *
      standard_error,
    
    lower_95_pp =
      conversion_factor *
      confidence_interval[1, 1],
    
    upper_95_pp =
      conversion_factor *
      confidence_interval[1, 2],
    
    p_value =
      p_value,
    
    significance = fcase(
      p_value < 0.01, "***",
      p_value < 0.05, "**",
      p_value < 0.10, "*",
      default = ""
    ),
    
    observations =
      nobs(model),
    
    historical_province_clusters =
      uniqueN(
        model_data$historical_province
      ),
    
    cohort_clusters =
      uniqueN(
        model_data$cohort
      ),
    
    survey_years =
      uniqueN(
        model_data$year
      )
  )
}


# ==============================================================================
# STEP 14: Estimate mechanism regressions
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
  
  current_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_sample <-
    outcome_definitions$sample_variable[outcome_index]
  
  current_family <-
    outcome_definitions$outcome_family[outcome_index]
  
  for (
    current_group in
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
      current_group,
      "\n"
    )
    
    model_data <- dt[
      get(current_sample) == TRUE &
        !is.na(get(current_variable))
    ]
    
    if (current_group == "Women") {
      model_data <- model_data[
        gender == 1L
      ]
    }
    
    if (current_group == "Men") {
      model_data <- model_data[
        gender == 2L
      ]
    }
    
    current_model <- tryCatch(
      estimate_mechanism_model(
        data =
          model_data,
        
        outcome_variable =
          current_variable,
        
        subgroup =
          current_group
      ),
      error = function(e) e
    )
    
    if (inherits(current_model, "error")) {
      
      failed_models[[failure_counter]] <-
        data.table(
          outcome =
            current_outcome,
          
          analysis_group =
            current_group,
          
          reason =
            current_model$message,
          
          observations =
            nrow(model_data)
        )
      
      failure_counter <-
        failure_counter + 1L
      
      next
    }
    
    regression_results[[result_counter]] <-
      extract_mechanism_result(
        model =
          current_model,
        
        outcome_name =
          current_outcome,
        
        outcome_family =
          current_family,
        
        subgroup =
          current_group,
        
        model_data =
          model_data
      )
    
    result_counter <-
      result_counter + 1L
    
    rm(
      current_model,
      model_data
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 15: Combine regression results
# ==============================================================================

mechanism_regression_results <- rbindlist(
  regression_results,
  fill = TRUE
)

setorder(
  mechanism_regression_results,
  outcome,
  analysis_group
)


# ==============================================================================
# STEP 16: False-discovery-rate adjustments
# ==============================================================================

mechanism_regression_results[
  ,
  p_value_fdr_all :=
    p.adjust(
      p_value,
      method = "BH"
    ),
  by = analysis_group
]

mechanism_regression_results[
  ,
  p_value_fdr_primary := NA_real_
]

for (
  current_group in
  unique(
    mechanism_regression_results$analysis_group
  )
) {
  
  selected_rows <- which(
    mechanism_regression_results$analysis_group ==
      current_group &
      mechanism_regression_results$outcome_family ==
      "Primary"
  )
  
  mechanism_regression_results[
    selected_rows,
    p_value_fdr_primary :=
      p.adjust(
        p_value,
        method = "BH"
      )
  ]
}


# ==============================================================================
# STEP 17: Save regression results
# ==============================================================================

fwrite(
  mechanism_regression_results,
  file.path(
    output_folder,
    "step18_mechanism_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 18: Compact regression table
# ==============================================================================

compact_results <- mechanism_regression_results[
  ,
  .(
    outcome,
    outcome_family,
    analysis_group,
    
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
    
    p_value_fdr_primary =
      round(
        p_value_fdr_primary,
        4
      ),
    
    p_value_fdr_all =
      round(
        p_value_fdr_all,
        4
      ),
    
    significance,
    observations,
    historical_province_clusters,
    cohort_clusters,
    survey_years
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
    "step18_mechanism_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Main publication-style mechanism table
# ==============================================================================

main_results <- compact_results[
  outcome_family == "Primary",
  .(
    outcome,
    analysis_group,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    p_value_fdr_primary,
    observations
  )
]

main_results[
  ,
  table_entry := paste0(
    estimate_with_stars,
    " ",
    standard_error_parentheses
  )
]

main_table_wide <- dcast(
  main_results,
  outcome ~ analysis_group,
  value.var = "table_entry"
)

fwrite(
  main_table_wide,
  file.path(
    output_folder,
    "step18_main_mechanism_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Construct descriptive province-year labor-demand measures
#
# These measures are descriptive and should not be presented as equivalent to
# the individual-level causal fixed-effects estimates.
# ==============================================================================

province_year_data <- dt[
  sample_main == TRUE &
    employed == 1L &
    valid_weight == TRUE,
  .(
    employed_weight =
      sum(
        weight,
        na.rm = TRUE
      ),
    
    tertiary_employed_weight =
      sum(
        weight *
          as.integer(
            tertiary_educated == 1L
          ),
        na.rm = TRUE
      ),
    
    valid_occupation_weight =
      sum(
        weight *
          as.integer(
            !is.na(isco_major_group)
          ),
        na.rm = TRUE
      ),
    
    high_skill_weight =
      sum(
        weight *
          as.integer(
            isco_major_group %in% 1:3
          ),
        na.rm = TRUE
      ),
    
    professional_weight =
      sum(
        weight *
          as.integer(
            isco_major_group == 2L
          ),
        na.rm = TRUE
      ),
    
    tertiary_lower_skill_weight =
      sum(
        weight *
          as.integer(
            tertiary_educated == 1L &
              isco_major_group %in% 4:9
          ),
        na.rm = TRUE
      ),
    
    observations = .N
  ),
  by = .(
    historical_province,
    year
  )
]


# ==============================================================================
# STEP 21: Calculate province-year shares and pressure ratios
# ==============================================================================

province_year_data[
  ,
  tertiary_employed_share :=
    tertiary_employed_weight /
    employed_weight
]

province_year_data[
  ,
  high_skill_job_share :=
    high_skill_weight /
    valid_occupation_weight
]

province_year_data[
  ,
  professional_job_share :=
    professional_weight /
    valid_occupation_weight
]

province_year_data[
  ,
  tertiary_lower_skill_share :=
    tertiary_lower_skill_weight /
    valid_occupation_weight
]

province_year_data[
  ,
  graduate_pressure_ratio :=
    fifelse(
      high_skill_weight > 0,
      tertiary_employed_weight /
        high_skill_weight,
      NA_real_
    )
]

province_year_data[
  ,
  graduate_surplus_share :=
    tertiary_employed_share -
    high_skill_job_share
]


# ==============================================================================
# STEP 22: Validate province-year measures
# ==============================================================================

province_year_validation <- province_year_data[
  ,
  .(
    province_year_cells = .N,
    
    historical_provinces =
      uniqueN(
        historical_province
      ),
    
    survey_years =
      uniqueN(year),
    
    minimum_observations =
      min(
        observations,
        na.rm = TRUE
      ),
    
    median_observations =
      median(
        observations,
        na.rm = TRUE
      ),
    
    maximum_observations =
      max(
        observations,
        na.rm = TRUE
      ),
    
    missing_pressure_ratio =
      sum(
        is.na(
          graduate_pressure_ratio
        )
      ),
    
    minimum_pressure_ratio =
      min(
        graduate_pressure_ratio,
        na.rm = TRUE
      ),
    
    maximum_pressure_ratio =
      max(
        graduate_pressure_ratio,
        na.rm = TRUE
      )
  )
]

print(province_year_validation)

fwrite(
  province_year_validation,
  file.path(
    output_folder,
    "step18_province_year_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Save province-year measures
# ==============================================================================

setorder(
  province_year_data,
  historical_province,
  year
)

fwrite(
  province_year_data,
  file.path(
    output_folder,
    "step18_province_year_labor_demand_measures.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: National weighted time series
# ==============================================================================

national_year_series <- province_year_data[
  ,
  .(
    tertiary_employed_share =
      sum(
        tertiary_employed_weight,
        na.rm = TRUE
      ) /
      sum(
        employed_weight,
        na.rm = TRUE
      ),
    
    high_skill_job_share =
      sum(
        high_skill_weight,
        na.rm = TRUE
      ) /
      sum(
        valid_occupation_weight,
        na.rm = TRUE
      ),
    
    professional_job_share =
      sum(
        professional_weight,
        na.rm = TRUE
      ) /
      sum(
        valid_occupation_weight,
        na.rm = TRUE
      ),
    
    tertiary_lower_skill_share =
      sum(
        tertiary_lower_skill_weight,
        na.rm = TRUE
      ) /
      sum(
        valid_occupation_weight,
        na.rm = TRUE
      ),
    
    graduate_pressure_ratio =
      sum(
        tertiary_employed_weight,
        na.rm = TRUE
      ) /
      sum(
        high_skill_weight,
        na.rm = TRUE
      ),
    
    graduate_surplus_share =
      (
        sum(
          tertiary_employed_weight,
          na.rm = TRUE
        ) /
          sum(
            employed_weight,
            na.rm = TRUE
          )
      ) -
      (
        sum(
          high_skill_weight,
          na.rm = TRUE
        ) /
          sum(
            valid_occupation_weight,
            na.rm = TRUE
          )
      )
  ),
  by = year
]

setorder(
  national_year_series,
  year
)

fwrite(
  national_year_series,
  file.path(
    output_folder,
    "step18_national_labor_demand_time_series.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Results surviving FDR correction
# ==============================================================================

results_surviving_fdr <- mechanism_regression_results[
  (
    !is.na(p_value_fdr_primary) &
      p_value_fdr_primary < 0.10
  ) |
    p_value_fdr_all < 0.10
][
  order(
    analysis_group,
    p_value_fdr_all
  )
]

fwrite(
  results_surviving_fdr,
  file.path(
    output_folder,
    "step18_results_surviving_FDR.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Save failed models
# ==============================================================================

if (length(failed_models) > 0L) {
  
  failed_model_table <- rbindlist(
    failed_models,
    fill = TRUE
  )
  
} else {
  
  failed_model_table <- data.table(
    outcome = character(),
    analysis_group = character(),
    reason = character(),
    observations = integer()
  )
}

fwrite(
  failed_model_table,
  file.path(
    output_folder,
    "step18_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Diagnostic summary
# ==============================================================================

diagnostic_summary <- mechanism_regression_results[
  ,
  .(
    models_estimated = .N,
    
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
    
    primary_FDR_below_010 =
      sum(
        p_value_fdr_primary < 0.10,
        na.rm = TRUE
      ),
    
    all_outcome_FDR_below_010 =
      sum(
        p_value_fdr_all < 0.10,
        na.rm = TRUE
      )
  ),
  by = analysis_group
]

print(diagnostic_summary)

fwrite(
  diagnostic_summary,
  file.path(
    output_folder,
    "step18_diagnostic_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 28: Print main mechanism results
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 18 PRIMARY MECHANISM RESULTS\n",
  "============================================================\n"
)

print(
  mechanism_regression_results[
    outcome_family == "Primary",
    .(
      outcome,
      analysis_group,
      
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
      
      FDR_primary =
        round(
          p_value_fdr_primary,
          4
        ),
      
      observations
    )
  ]
)


# ==============================================================================
# STEP 29: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 18 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step18_outcome_validation.csv\n",
  "2. step18_mechanism_descriptive_rates.csv\n",
  "3. step18_mechanism_regression_results.csv\n",
  "4. step18_mechanism_compact_results.csv\n",
  "5. step18_main_mechanism_table_wide.csv\n",
  "6. step18_province_year_labor_demand_measures.csv\n",
  "7. step18_province_year_validation.csv\n",
  "8. step18_national_labor_demand_time_series.csv\n",
  "9. step18_results_surviving_FDR.csv\n",
  "10. step18_failed_models.csv\n",
  "11. step18_diagnostic_summary.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nInterpretation priorities:\n",
  "- Compare the tertiary-educated employment effect with the high-skill-job effect.\n",
  "- Focus on tertiary-educated workers entering lower-skill occupations.\n",
  "- Examine whether the mechanism is concentrated among men.\n",
  "- Treat province-year pressure measures as descriptive supporting evidence.\n",
  "- Do not interpret the descriptive pressure ratio as an individual causal estimate.\n"
)