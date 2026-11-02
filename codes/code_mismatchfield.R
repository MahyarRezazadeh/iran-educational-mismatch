# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 15: Field-of-Study Heterogeneity
#
# Main questions:
#   1. Which fields drive the increase in horizontal mismatch?
#   2. Which fields drive the increase in double mismatch?
#   3. In which fields does fully matched employment decline?
#   4. Are the field-specific effects concentrated among men?
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Reported effect:
#   Percentage-point effect of a 10% increase in university exposure
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
#   Two-way clustered by historical province and birth cohort
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
  "step15_field_heterogeneity"
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
  # Identifiers and controls
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
  
  # Field of study
  "isced2013_broad_code",
  "education_field_harmonized",
  "education_field_detailed",
  "horizontal_detailed_eligible",
  
  # Horizontal mismatch
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict",
  
  # Vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final",
  "vertical_mismatch_final"
)

cat("\nReading Step 15 variables...\n")

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
# STEP 4: Inspect original field codes
#
# This output allows us to confirm that the field-code interpretation matches
# the underlying dataset.
# ==============================================================================

raw_field_diagnostic <- dt[
  sample_main == TRUE &
    tertiary_educated == 1L,
  .(
    observations = .N,
    weighted_population = sum(weight, na.rm = TRUE)
  ),
  by = .(
    isced2013_broad_code,
    education_field_harmonized
  )
]

setorder(
  raw_field_diagnostic,
  isced2013_broad_code,
  education_field_harmonized
)

print(raw_field_diagnostic)

fwrite(
  raw_field_diagnostic,
  file.path(
    output_folder,
    "step15_raw_field_code_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 5: Standardize ISCED broad field code
#
# The code handles numeric values such as:
#   1, 2, 3, ..., 10
#
# It also handles character forms such as:
#   "01", "02", ..., "10"
#
# Non-numeric content is removed before conversion.
# ==============================================================================

dt[
  ,
  isced_broad_code_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isced2013_broad_code)
      )
    )
  )
]

# Correct possible one-digit and two-digit representations.
#
# Examples:
#   "01" becomes 1
#   "07" becomes 7
#   "10" remains 10

dt[
  ,
  field_group := fcase(
    isced_broad_code_clean == 0L,
    "00 General programmes",
    
    isced_broad_code_clean == 1L,
    "01 Education",
    
    isced_broad_code_clean == 2L,
    "02 Arts and humanities",
    
    isced_broad_code_clean == 3L,
    "03 Social sciences and journalism",
    
    isced_broad_code_clean == 4L,
    "04 Business, administration and law",
    
    isced_broad_code_clean == 5L,
    "05 Natural sciences and mathematics",
    
    isced_broad_code_clean == 6L,
    "06 Information and communication technologies",
    
    isced_broad_code_clean == 7L,
    "07 Engineering, manufacturing and construction",
    
    isced_broad_code_clean == 8L,
    "08 Agriculture and veterinary",
    
    isced_broad_code_clean == 9L,
    "09 Health and welfare",
    
    isced_broad_code_clean == 10L,
    "10 Services",
    
    default = NA_character_
  )
]


# ==============================================================================
# STEP 6: Fallback using harmonized field labels
#
# This is used only when the ISCED numeric code could not be interpreted.
# ==============================================================================

dt[
  is.na(field_group) &
    !is.na(education_field_harmonized),
  field_group := fcase(
    grepl(
      "education|teacher|teaching",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "01 Education",
    
    grepl(
      "art|humanit|language|literature|religion|history|philosophy",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "02 Arts and humanities",
    
    grepl(
      "social|journal|information|communication|econom|psycholog|sociolog|politic",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "03 Social sciences and journalism",
    
    grepl(
      "business|administration|management|account|finance|law",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "04 Business, administration and law",
    
    grepl(
      "natural|science|mathemat|statistic|biology|chemistry|physics",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "05 Natural sciences and mathematics",
    
    grepl(
      "information technology|computer|computing|software|ict",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "06 Information and communication technologies",
    
    grepl(
      "engineering|manufactur|construction|architecture",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "07 Engineering, manufacturing and construction",
    
    grepl(
      "agriculture|forestry|fish|veterinary",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "08 Agriculture and veterinary",
    
    grepl(
      "health|medicine|medical|nursing|pharmacy|welfare|dent",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "09 Health and welfare",
    
    grepl(
      "service|transport|security|hospitality|tourism",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "10 Services",
    
    default = NA_character_
  )
]


# ==============================================================================
# STEP 7: Define the field order
# ==============================================================================

field_levels <- c(
  "01 Education",
  "02 Arts and humanities",
  "03 Social sciences and journalism",
  "04 Business, administration and law",
  "05 Natural sciences and mathematics",
  "06 Information and communication technologies",
  "07 Engineering, manufacturing and construction",
  "08 Agriculture and veterinary",
  "09 Health and welfare",
  "10 Services",
  "00 General programmes"
)

dt[
  ,
  field_group := factor(
    field_group,
    levels = field_levels
  )
]


# ==============================================================================
# STEP 8: Validate field classification
# ==============================================================================

field_classification_validation <- dt[
  sample_main == TRUE &
    tertiary_educated == 1L,
  .(
    observations = .N,
    classified = sum(!is.na(field_group)),
    unclassified = sum(is.na(field_group)),
    classification_rate =
      mean(!is.na(field_group)),
    weighted_classification_rate =
      weighted.mean(
        !is.na(field_group),
        weight,
        na.rm = TRUE
      )
  )
]

print(field_classification_validation)

fwrite(
  field_classification_validation,
  file.path(
    output_folder,
    "step15_field_classification_validation.csv"
  ),
  bom = TRUE
)

unclassified_fields <- dt[
  sample_main == TRUE &
    tertiary_educated == 1L &
    is.na(field_group),
  .(
    observations = .N,
    weighted_population = sum(weight, na.rm = TRUE)
  ),
  by = .(
    isced2013_broad_code,
    education_field_harmonized
  )
]

setorder(
  unclassified_fields,
  -observations
)

fwrite(
  unclassified_fields,
  file.path(
    output_folder,
    "step15_unclassified_field_values.csv"
  ),
  bom = TRUE
)

if (
  field_classification_validation$classification_rate < 0.95
) {
  warning(
    paste0(
      "Less than 95% of tertiary-educated observations were classified. ",
      "Inspect step15_unclassified_field_values.csv."
    )
  )
}


# ==============================================================================
# STEP 9: Construct joint mismatch outcomes
# ==============================================================================

dt[
  ,
  joint_mismatch_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    vertical_fullperiod_eligible_final == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(field_group) &
    !is.na(overeducated_final) &
    !is.na(undereducated_final) &
    !is.na(vertically_matched_final) &
    !is.na(horizontal_mismatch_detailed_broad)
]

dt[
  ,
  double_mismatch := fifelse(
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


# ==============================================================================
# STEP 10: Define outcome-specific samples
# ==============================================================================

dt[
  ,
  horizontal_field_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(horizontal_mismatch_detailed_broad) &
    !is.na(field_group)
]

dt[
  ,
  overeducation_field_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(field_group)
]


# ==============================================================================
# STEP 11: Field-level sample summary
# ==============================================================================

field_sample_summary <- dt[
  sample_main == TRUE &
    tertiary_educated == 1L &
    !is.na(field_group),
  .(
    tertiary_observations = .N,
    
    employed_observations =
      sum(
        employed == 1L,
        na.rm = TRUE
      ),
    
    horizontal_eligible_observations =
      sum(
        horizontal_field_eligible,
        na.rm = TRUE
      ),
    
    joint_eligible_observations =
      sum(
        joint_mismatch_eligible,
        na.rm = TRUE
      ),
    
    women_joint_observations =
      sum(
        joint_mismatch_eligible &
          gender == 1L,
        na.rm = TRUE
      ),
    
    men_joint_observations =
      sum(
        joint_mismatch_eligible &
          gender == 2L,
        na.rm = TRUE
      ),
    
    historical_provinces =
      uniqueN(
        historical_province[
          joint_mismatch_eligible
        ]
      ),
    
    cohorts =
      uniqueN(
        cohort[
          joint_mismatch_eligible
        ]
      ),
    
    survey_years =
      uniqueN(
        year[
          joint_mismatch_eligible
        ]
      ),
    
    weighted_population =
      sum(
        weight,
        na.rm = TRUE
      )
  ),
  by = field_group
]

field_sample_summary[
  ,
  weighted_share :=
    weighted_population /
    sum(weighted_population)
]

setorder(
  field_sample_summary,
  field_group
)

print(field_sample_summary)

fwrite(
  field_sample_summary,
  file.path(
    output_folder,
    "step15_field_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Minimum sample requirements
#
# These restrictions avoid emphasizing estimates based on very small fields.
# All estimates are still saved, but a reliability indicator is included.
# ==============================================================================

minimum_pooled_observations <- 5000L
minimum_gender_observations <- 2000L
minimum_province_clusters <- 15L
minimum_cohort_clusters <- 15L


# ==============================================================================
# STEP 13: Weighted descriptive mean helper
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
# STEP 14: Define outcomes
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Horizontal mismatch",
    "Double mismatch",
    "Fully matched",
    "Overeducated"
  ),
  
  outcome_variable = c(
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched_joint",
    "overeducated_final"
  ),
  
  eligibility_variable = c(
    "horizontal_field_eligible",
    "joint_mismatch_eligible",
    "joint_mismatch_eligible",
    "overeducation_field_eligible"
  )
)


# ==============================================================================
# STEP 15: Calculate field-specific descriptive rates
# ==============================================================================

descriptive_list <- list()
descriptive_counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  eligibility_variable <-
    outcome_definitions$eligibility_variable[outcome_index]
  
  for (current_field in field_levels) {
    
    for (current_subgroup in c(
      "Pooled",
      "Women",
      "Men"
    )) {
      
      current_data <- dt[
        get(eligibility_variable) == TRUE &
          as.character(field_group) == current_field &
          !is.na(get(outcome_variable))
      ]
      
      if (current_subgroup == "Women") {
        current_data <- current_data[
          gender == 1L
        ]
      }
      
      if (current_subgroup == "Men") {
        current_data <- current_data[
          gender == 2L
        ]
      }
      
      descriptive_list[[descriptive_counter]] <- data.table(
        outcome = outcome_name,
        field_group = current_field,
        subgroup = current_subgroup,
        
        weighted_rate =
          safe_weighted_mean(
            current_data[[outcome_variable]],
            current_data$weight
          ),
        
        weighted_rate_percent =
          100 *
          safe_weighted_mean(
            current_data[[outcome_variable]],
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
          )
      )
      
      descriptive_counter <-
        descriptive_counter + 1L
    }
  }
}

field_descriptive_results <- rbindlist(
  descriptive_list,
  fill = TRUE
)

setorder(
  field_descriptive_results,
  outcome,
  field_group,
  subgroup
)

fwrite(
  field_descriptive_results,
  file.path(
    output_folder,
    "step15_field_descriptive_rates.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Regression-estimation helper
# ==============================================================================

estimate_field_model <- function(
    data,
    outcome_variable,
    subgroup_name
) {
  
  if (subgroup_name == "Pooled") {
    
    model <- feols(
      as.formula(
        paste0(
          outcome_variable,
          " ~ log_undergrad_exposure + female + urban | ",
          "historical_province + cohort + year"
        )
      ),
      data = data,
      weights = ~weight,
      cluster = ~historical_province + cohort
    )
  }
  
  if (subgroup_name %in% c(
    "Women",
    "Men"
  )) {
    
    model <- feols(
      as.formula(
        paste0(
          outcome_variable,
          " ~ log_undergrad_exposure + urban | ",
          "historical_province + cohort + year"
        )
      ),
      data = data,
      weights = ~weight,
      cluster = ~historical_province + cohort
    )
  }
  
  model
}


# ==============================================================================
# STEP 17: Safely extract treatment result
# ==============================================================================

extract_field_result <- function(
    model,
    outcome_name,
    field_name,
    subgroup_name,
    model_data
) {
  
  treatment_term <- "log_undergrad_exposure"
  
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
  
  observations <-
    nobs(model)
  
  province_clusters <-
    uniqueN(
      model_data$historical_province
    )
  
  cohort_clusters <-
    uniqueN(
      model_data$cohort
    )
  
  minimum_required_observations <- fifelse(
    subgroup_name == "Pooled",
    minimum_pooled_observations,
    minimum_gender_observations
  )
  
  reliable_sample <- (
    observations >= minimum_required_observations &
      province_clusters >= minimum_province_clusters &
      cohort_clusters >= minimum_cohort_clusters
  )
  
  data.table(
    outcome = outcome_name,
    field_group = field_name,
    subgroup = subgroup_name,
    
    raw_log_coefficient =
      coefficient,
    
    raw_standard_error =
      standard_error,
    
    effect_pp_for_10_percent_increase =
      conversion_factor * coefficient,
    
    standard_error_pp_for_10_percent =
      conversion_factor * standard_error,
    
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
      observations,
    
    historical_province_clusters =
      province_clusters,
    
    cohort_clusters =
      cohort_clusters,
    
    reliable_sample =
      reliable_sample
  )
}


# ==============================================================================
# STEP 18: Estimate all field-specific models
# ==============================================================================

regression_results <- list()
regression_counter <- 1L
failed_models <- list()
failure_counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  eligibility_variable <-
    outcome_definitions$eligibility_variable[outcome_index]
  
  for (current_field in field_levels) {
    
    for (current_subgroup in c(
      "Pooled",
      "Women",
      "Men"
    )) {
      
      cat(
        "\nEstimating:",
        outcome_name,
        "|",
        current_field,
        "|",
        current_subgroup,
        "\n"
      )
      
      model_data <- dt[
        get(eligibility_variable) == TRUE &
          as.character(field_group) == current_field &
          !is.na(get(outcome_variable))
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
      
      minimum_rows_for_estimation <- 500L
      
      if (
        nrow(model_data) < minimum_rows_for_estimation ||
        uniqueN(model_data$historical_province) < 5L ||
        uniqueN(model_data$cohort) < 5L
      ) {
        
        failed_models[[failure_counter]] <- data.table(
          outcome = outcome_name,
          field_group = current_field,
          subgroup = current_subgroup,
          reason = "Insufficient observations or clusters",
          observations = nrow(model_data),
          province_clusters =
            uniqueN(model_data$historical_province),
          cohort_clusters =
            uniqueN(model_data$cohort)
        )
        
        failure_counter <-
          failure_counter + 1L
        
        next
      }
      
      current_model <- tryCatch(
        estimate_field_model(
          data = model_data,
          outcome_variable = outcome_variable,
          subgroup_name = current_subgroup
        ),
        error = function(e) {
          e
        }
      )
      
      if (inherits(current_model, "error")) {
        
        failed_models[[failure_counter]] <- data.table(
          outcome = outcome_name,
          field_group = current_field,
          subgroup = current_subgroup,
          reason = current_model$message,
          observations = nrow(model_data),
          province_clusters =
            uniqueN(model_data$historical_province),
          cohort_clusters =
            uniqueN(model_data$cohort)
        )
        
        failure_counter <-
          failure_counter + 1L
        
        next
      }
      
      regression_results[[regression_counter]] <-
        extract_field_result(
          model = current_model,
          outcome_name = outcome_name,
          field_name = current_field,
          subgroup_name = current_subgroup,
          model_data = model_data
        )
      
      regression_counter <-
        regression_counter + 1L
      
      rm(
        current_model,
        model_data
      )
      
      invisible(gc())
    }
  }
}

field_regression_results <- rbindlist(
  regression_results,
  fill = TRUE
)

setorder(
  field_regression_results,
  outcome,
  field_group,
  subgroup
)

print(field_regression_results)

fwrite(
  field_regression_results,
  file.path(
    output_folder,
    "step15_field_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Save failed-model log
# ==============================================================================

if (length(failed_models) > 0L) {
  
  failed_model_results <- rbindlist(
    failed_models,
    fill = TRUE
  )
  
} else {
  
  failed_model_results <- data.table(
    outcome = character(),
    field_group = character(),
    subgroup = character(),
    reason = character(),
    observations = integer(),
    province_clusters = integer(),
    cohort_clusters = integer()
  )
}

fwrite(
  failed_model_results,
  file.path(
    output_folder,
    "step15_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Create compact publication table
# ==============================================================================

compact_results <- field_regression_results[
  ,
  .(
    outcome,
    field_group,
    subgroup,
    
    estimate =
      round(
        effect_pp_for_10_percent_increase,
        3
      ),
    
    standard_error =
      round(
        standard_error_pp_for_10_percent,
        3
      ),
    
    p_value =
      round(
        p_value,
        4
      ),
    
    significance,
    
    observations,
    
    historical_province_clusters,
    
    cohort_clusters,
    
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
    "step15_field_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Create a wide main-table format
#
# Only reliable-sample estimates are included in this version.
# ==============================================================================

main_table_estimates <- compact_results[
  reliable_sample == TRUE,
  .(
    outcome,
    field_group,
    subgroup,
    estimate_with_stars
  )
]

main_table_wide <- dcast(
  main_table_estimates,
  outcome + field_group ~ subgroup,
  value.var = "estimate_with_stars"
)

fwrite(
  main_table_wide,
  file.path(
    output_folder,
    "step15_field_main_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Formal pooled field-interaction models
#
# These models test whether exposure effects differ across fields.
#
# Field-specific province, cohort, and survey-year fixed effects are included.
# This allows the common effects of province, cohort, and year to vary by field.
# ==============================================================================

dt[
  ,
  province_field :=
    interaction(
      historical_province,
      field_group,
      drop = TRUE
    )
]

dt[
  ,
  cohort_field :=
    interaction(
      cohort,
      field_group,
      drop = TRUE
    )
]

dt[
  ,
  year_field :=
    interaction(
      year,
      field_group,
      drop = TRUE
    )
]

field_interaction_results <- list()
interaction_counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  eligibility_variable <-
    outcome_definitions$eligibility_variable[outcome_index]
  
  interaction_data <- dt[
    get(eligibility_variable) == TRUE &
      !is.na(field_group) &
      !is.na(get(outcome_variable))
  ]
  
  # Use Education as the reference field when available.
  interaction_data[
    ,
    field_group := relevel(
      droplevels(field_group),
      ref = "01 Education"
    )
  ]
  
  interaction_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ i(field_group, log_undergrad_exposure, ",
        "ref = '01 Education') + female + urban | ",
        "province_field + cohort_field + year_field"
      )
    ),
    data = interaction_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  model_coefficients <- coeftable(
    interaction_model
  )
  
  coefficient_names <- rownames(
    model_coefficients
  )
  
  treatment_rows <- grepl(
    "field_group::",
    coefficient_names
  ) &
    grepl(
      "log_undergrad_exposure",
      coefficient_names
    )
  
  current_results <- data.table(
    outcome = outcome_name,
    
    coefficient_name =
      coefficient_names[treatment_rows],
    
    coefficient =
      model_coefficients[
        treatment_rows,
        "Estimate"
      ],
    
    standard_error =
      model_coefficients[
        treatment_rows,
        "Std. Error"
      ],
    
    p_value =
      model_coefficients[
        treatment_rows,
        "Pr(>|t|)"
      ]
  )
  
  current_results[
    ,
    `:=`(
      effect_pp_for_10_percent_increase =
        100 * log(1.10) * coefficient,
      
      standard_error_pp_for_10_percent =
        100 * log(1.10) * standard_error
    )
  ]
  
  field_interaction_results[[interaction_counter]] <-
    current_results
  
  interaction_counter <-
    interaction_counter + 1L
  
  rm(
    interaction_model,
    interaction_data
  )
  
  invisible(gc())
}

field_interaction_coefficients <- rbindlist(
  field_interaction_results,
  fill = TRUE
)

fwrite(
  field_interaction_coefficients,
  file.path(
    output_folder,
    "step15_field_interaction_coefficients.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Identify strongest reliable results
# ==============================================================================

strongest_positive_results <- field_regression_results[
  reliable_sample == TRUE &
    p_value < 0.10
][
  order(
    -effect_pp_for_10_percent_increase
  )
]

strongest_negative_results <- field_regression_results[
  reliable_sample == TRUE &
    p_value < 0.10
][
  order(
    effect_pp_for_10_percent_increase
  )
]

fwrite(
  strongest_positive_results,
  file.path(
    output_folder,
    "step15_significant_positive_results.csv"
  ),
  bom = TRUE
)

fwrite(
  strongest_negative_results,
  file.path(
    output_folder,
    "step15_significant_negative_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Final diagnostic summary
# ==============================================================================

diagnostic_summary <- field_regression_results[
  ,
  .(
    models_estimated = .N,
    
    reliable_models =
      sum(
        reliable_sample,
        na.rm = TRUE
      ),
    
    significant_10_percent =
      sum(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    significant_5_percent =
      sum(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    significant_1_percent =
      sum(
        p_value < 0.01,
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
    "step15_model_diagnostic_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 15 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nImportant outputs:\n",
  "1. step15_raw_field_code_diagnostic.csv\n",
  "2. step15_field_classification_validation.csv\n",
  "3. step15_unclassified_field_values.csv\n",
  "4. step15_field_sample_summary.csv\n",
  "5. step15_field_descriptive_rates.csv\n",
  "6. step15_field_regression_results.csv\n",
  "7. step15_field_compact_results.csv\n",
  "8. step15_field_main_table_wide.csv\n",
  "9. step15_field_interaction_coefficients.csv\n",
  "10. step15_significant_positive_results.csv\n",
  "11. step15_significant_negative_results.csv\n",
  "12. step15_failed_models.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nFirst checks:\n",
  "- Field classification rate should preferably exceed 95%.\n",
  "- Review unclassified field values, if any.\n",
  "- Review field sample sizes before interpreting estimates.\n",
  "- Prioritize reliable_sample == TRUE results.\n"
)


# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 15B: Corrected Field-of-Study Interaction Analysis
#
# Purpose:
#   1. Estimate field-specific exposure effects in one unified model
#   2. Formally test whether effects differ across fields
#   3. Calculate total effects for each field
#   4. Correct for multiple hypothesis testing
#
# Primary outcomes:
#   - Horizontal mismatch
#   - Double mismatch
#   - Fully matched employment
#
# Secondary outcome:
#   - Overeducation
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Reported effects:
#   Percentage-point effects associated with a 10% increase in exposure
#
# Fixed effects:
#   Field-specific historical-province fixed effects
#   Field-specific corrected-cohort fixed effects
#   Field-specific survey-year fixed effects
#
# Controls:
#   Pooled models: female and urban
#   Gender-specific models: urban
#
# Inference:
#   Two-way clustered by historical province and corrected birth cohort
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
  "step15B_corrected_field_interactions"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Final analysis-ready dataset was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Read required variables
# ==============================================================================

variables_to_read <- c(
  # Identifiers, controls, and weights
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  
  # Treatment
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Employment and tertiary education
  "employed",
  "tertiary_educated",
  
  # Field of study
  "isced2013_broad_code",
  "education_field_harmonized",
  "horizontal_detailed_eligible",
  
  # Horizontal mismatch
  "horizontal_mismatch_detailed_broad",
  
  # Vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final"
)

cat("\nReading Step 15B variables...\n")

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
# STEP 4: Clean and classify ISCED broad fields
# ==============================================================================

dt[
  ,
  isced_broad_code_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isced2013_broad_code)
      )
    )
  )
]

dt[
  ,
  field_group := fcase(
    isced_broad_code_clean == 1L,
    "01 Education",
    
    isced_broad_code_clean == 2L,
    "02 Arts and humanities",
    
    isced_broad_code_clean == 3L,
    "03 Social sciences and journalism",
    
    isced_broad_code_clean == 4L,
    "04 Business, administration and law",
    
    isced_broad_code_clean == 5L,
    "05 Natural sciences and mathematics",
    
    isced_broad_code_clean == 6L,
    "06 Information and communication technologies",
    
    isced_broad_code_clean == 7L,
    "07 Engineering, manufacturing and construction",
    
    isced_broad_code_clean == 8L,
    "08 Agriculture and veterinary",
    
    isced_broad_code_clean == 9L,
    "09 Health and welfare",
    
    isced_broad_code_clean == 10L,
    "10 Services",
    
    default = NA_character_
  )
]


# ==============================================================================
# STEP 5: Fallback classification from harmonized labels
# ==============================================================================

dt[
  is.na(field_group) &
    !is.na(education_field_harmonized),
  field_group := fcase(
    grepl(
      "education|teacher|teaching",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "01 Education",
    
    grepl(
      "art|humanit|language|literature|religion|history|philosophy",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "02 Arts and humanities",
    
    grepl(
      "social|journal|econom|psycholog|sociolog|politic",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "03 Social sciences and journalism",
    
    grepl(
      "business|administration|management|account|finance|law",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "04 Business, administration and law",
    
    grepl(
      "natural|science|mathemat|statistic|biology|chemistry|physics",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "05 Natural sciences and mathematics",
    
    grepl(
      "information technology|computer|computing|software|ict",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "06 Information and communication technologies",
    
    grepl(
      "engineering|manufactur|construction|architecture",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "07 Engineering, manufacturing and construction",
    
    grepl(
      "agriculture|forestry|fish|veterinary",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "08 Agriculture and veterinary",
    
    grepl(
      "health|medicine|medical|nursing|pharmacy|welfare|dent",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "09 Health and welfare",
    
    grepl(
      "service|transport|security|hospitality|tourism",
      education_field_harmonized,
      ignore.case = TRUE
    ),
    "10 Services",
    
    default = NA_character_
  )
]


# ==============================================================================
# STEP 6: Define field order and reference category
# ==============================================================================

field_levels <- c(
  "01 Education",
  "02 Arts and humanities",
  "03 Social sciences and journalism",
  "04 Business, administration and law",
  "05 Natural sciences and mathematics",
  "06 Information and communication technologies",
  "07 Engineering, manufacturing and construction",
  "08 Agriculture and veterinary",
  "09 Health and welfare",
  "10 Services"
)

reference_field <- "01 Education"

dt[
  ,
  field_group := factor(
    field_group,
    levels = field_levels
  )
]

dt[
  ,
  field_group := relevel(
    field_group,
    ref = reference_field
  )
]


# ==============================================================================
# STEP 7: Validate field classification
# ==============================================================================

field_validation <- dt[
  sample_main == TRUE &
    tertiary_educated == 1L,
  .(
    tertiary_observations = .N,
    
    classified_observations =
      sum(
        !is.na(field_group)
      ),
    
    unclassified_observations =
      sum(
        is.na(field_group)
      ),
    
    classification_rate =
      mean(
        !is.na(field_group)
      ),
    
    weighted_classification_rate =
      weighted.mean(
        !is.na(field_group),
        weight,
        na.rm = TRUE
      )
  )
]

print(field_validation)

fwrite(
  field_validation,
  file.path(
    output_folder,
    "step15B_field_classification_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 8: Define eligibility samples and joint outcomes
# ==============================================================================

dt[
  ,
  horizontal_field_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(horizontal_mismatch_detailed_broad) &
    !is.na(field_group)
]

dt[
  ,
  vertical_field_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(undereducated_final) &
    !is.na(vertically_matched_final) &
    !is.na(field_group)
]

dt[
  ,
  joint_mismatch_eligible :=
    horizontal_field_eligible == TRUE &
    vertical_field_eligible == TRUE
]

dt[
  ,
  double_mismatch := fifelse(
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
  fully_matched_joint := fifelse(
    joint_mismatch_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 9: Define primary and secondary outcomes
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Horizontal mismatch",
    "Double mismatch",
    "Fully matched",
    "Overeducated"
  ),
  
  outcome_variable = c(
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched_joint",
    "overeducated_final"
  ),
  
  eligibility_variable = c(
    "horizontal_field_eligible",
    "joint_mismatch_eligible",
    "joint_mismatch_eligible",
    "vertical_field_eligible"
  ),
  
  outcome_family = c(
    "Primary",
    "Primary",
    "Primary",
    "Secondary"
  )
)


# ==============================================================================
# STEP 10: Create field-specific fixed-effect variables
# ==============================================================================

dt[
  ,
  province_field :=
    interaction(
      historical_province,
      field_group,
      drop = TRUE,
      lex.order = TRUE
    )
]

dt[
  ,
  cohort_field :=
    interaction(
      cohort,
      field_group,
      drop = TRUE,
      lex.order = TRUE
    )
]

dt[
  ,
  year_field :=
    interaction(
      year,
      field_group,
      drop = TRUE,
      lex.order = TRUE
    )
]


# ==============================================================================
# STEP 11: Conversion factor
#
# Converts the log-exposure coefficient into the percentage-point effect
# associated with a 10% increase in university exposure.
# ==============================================================================

effect_conversion <- 100 * log(1.10)


# ==============================================================================
# STEP 12: Safe matrix-inversion helper
# ==============================================================================

safe_matrix_inverse <- function(matrix_object) {
  
  inverse_result <- tryCatch(
    solve(matrix_object),
    error = function(e) NULL
  )
  
  if (!is.null(inverse_result)) {
    return(inverse_result)
  }
  
  eigen_result <- eigen(
    matrix_object,
    symmetric = TRUE
  )
  
  tolerance <- max(
    dim(matrix_object)
  ) *
    max(
      abs(eigen_result$values)
    ) *
    .Machine$double.eps
  
  inverse_values <- ifelse(
    abs(eigen_result$values) > tolerance,
    1 / eigen_result$values,
    0
  )
  
  eigen_result$vectors %*%
    diag(inverse_values) %*%
    t(eigen_result$vectors)
}


# ==============================================================================
# STEP 13: Function to estimate corrected interaction model
# ==============================================================================

estimate_interaction_model <- function(
    analysis_data,
    outcome_variable,
    subgroup_name
) {
  
  if (subgroup_name == "Pooled") {
    
    regression_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ ",
        "log_undergrad_exposure + ",
        "i(field_group, log_undergrad_exposure, ",
        "ref = '",
        reference_field,
        "') + ",
        "female + urban | ",
        "province_field + cohort_field + year_field"
      )
    )
  } else {
    
    regression_formula <- as.formula(
      paste0(
        outcome_variable,
        " ~ ",
        "log_undergrad_exposure + ",
        "i(field_group, log_undergrad_exposure, ",
        "ref = '",
        reference_field,
        "') + ",
        "urban | ",
        "province_field + cohort_field + year_field"
      )
    )
  }
  
  feols(
    regression_formula,
    data = analysis_data,
    weights = ~weight,
    cluster = ~historical_province + cohort,
    notes = FALSE,
    warn = TRUE
  )
}


# ==============================================================================
# STEP 14: Extract corrected field-specific total effects
# ==============================================================================

extract_field_effects <- function(
    model,
    outcome_name,
    outcome_family,
    subgroup_name,
    analysis_data
) {
  
  coefficient_vector <- coef(model)
  variance_matrix <- vcov(model)
  coefficient_names <- names(coefficient_vector)
  
  base_term <- "log_undergrad_exposure"
  
  if (!base_term %in% coefficient_names) {
    stop(
      paste0(
        "The baseline exposure coefficient was not found for ",
        outcome_name,
        ", ",
        subgroup_name,
        "."
      )
    )
  }
  
  beta_base <- unname(
    coefficient_vector[base_term]
  )
  
  variance_base <- variance_matrix[
    base_term,
    base_term
  ]
  
  output_list <- list()
  
  for (current_field in field_levels) {
    
    field_data <- analysis_data[
      as.character(field_group) == current_field
    ]
    
    field_observations <- nrow(field_data)
    
    field_province_clusters <- uniqueN(
      field_data$historical_province
    )
    
    field_cohort_clusters <- uniqueN(
      field_data$cohort
    )
    
    field_survey_years <- uniqueN(
      field_data$year
    )
    
    if (current_field == reference_field) {
      
      field_effect <- beta_base
      field_variance <- variance_base
      field_standard_error <- sqrt(field_variance)
      
      difference_from_reference <- 0
      difference_standard_error <- NA_real_
      difference_p_value <- NA_real_
      interaction_coefficient_name <- NA_character_
      
    } else {
      
      possible_interaction_rows <- coefficient_names[
        grepl(
          "field_group::",
          coefficient_names,
          fixed = TRUE
        ) &
          grepl(
            current_field,
            coefficient_names,
            fixed = TRUE
          ) &
          grepl(
            "log_undergrad_exposure",
            coefficient_names,
            fixed = TRUE
          )
      ]
      
      if (length(possible_interaction_rows) != 1L) {
        
        output_list[[current_field]] <- data.table(
          outcome = outcome_name,
          outcome_family = outcome_family,
          subgroup = subgroup_name,
          field_group = current_field,
          reference_field = reference_field,
          model_status = "Interaction coefficient not uniquely identified",
          observations = field_observations,
          historical_province_clusters = field_province_clusters,
          cohort_clusters = field_cohort_clusters,
          survey_years = field_survey_years
        )
        
        next
      }
      
      interaction_term <- possible_interaction_rows[1]
      
      beta_interaction <- unname(
        coefficient_vector[interaction_term]
      )
      
      variance_interaction <- variance_matrix[
        interaction_term,
        interaction_term
      ]
      
      covariance_base_interaction <- variance_matrix[
        base_term,
        interaction_term
      ]
      
      field_effect <-
        beta_base +
        beta_interaction
      
      field_variance <-
        variance_base +
        variance_interaction +
        2 * covariance_base_interaction
      
      field_standard_error <-
        sqrt(
          max(
            field_variance,
            0
          )
        )
      
      difference_from_reference <-
        beta_interaction
      
      difference_standard_error <-
        sqrt(
          max(
            variance_interaction,
            0
          )
        )
      
      difference_p_value <-
        2 * pnorm(
          abs(
            difference_from_reference /
              difference_standard_error
          ),
          lower.tail = FALSE
        )
      
      interaction_coefficient_name <-
        interaction_term
    }
    
    field_test_statistic <-
      field_effect /
      field_standard_error
    
    field_p_value <-
      2 * pnorm(
        abs(field_test_statistic),
        lower.tail = FALSE
      )
    
    output_list[[current_field]] <- data.table(
      outcome = outcome_name,
      outcome_family = outcome_family,
      subgroup = subgroup_name,
      field_group = current_field,
      reference_field = reference_field,
      model_status = "Estimated",
      
      effect_pp_for_10_percent_increase =
        effect_conversion *
        field_effect,
      
      standard_error_pp =
        effect_conversion *
        field_standard_error,
      
      lower_95_pp =
        effect_conversion *
        (
          field_effect -
            1.96 *
            field_standard_error
        ),
      
      upper_95_pp =
        effect_conversion *
        (
          field_effect +
            1.96 *
            field_standard_error
        ),
      
      p_value =
        field_p_value,
      
      difference_from_education_pp =
        effect_conversion *
        difference_from_reference,
      
      difference_standard_error_pp =
        effect_conversion *
        difference_standard_error,
      
      difference_p_value =
        difference_p_value,
      
      interaction_coefficient_name =
        interaction_coefficient_name,
      
      observations =
        field_observations,
      
      historical_province_clusters =
        field_province_clusters,
      
      cohort_clusters =
        field_cohort_clusters,
      
      survey_years =
        field_survey_years
    )
  }
  
  rbindlist(
    output_list,
    fill = TRUE
  )
}


# ==============================================================================
# STEP 15: Omnibus test of equal exposure effects across fields
#
# Null hypothesis:
#   All field-by-exposure interaction coefficients equal zero.
#
# Rejection means the exposure effect differs across at least some fields.
# ==============================================================================

calculate_omnibus_test <- function(
    model,
    outcome_name,
    outcome_family,
    subgroup_name
) {
  
  coefficient_vector <- coef(model)
  variance_matrix <- vcov(model)
  coefficient_names <- names(coefficient_vector)
  
  interaction_terms <- coefficient_names[
    grepl(
      "field_group::",
      coefficient_names,
      fixed = TRUE
    ) &
      grepl(
        "log_undergrad_exposure",
        coefficient_names,
        fixed = TRUE
      )
  ]
  
  if (length(interaction_terms) == 0L) {
    
    return(
      data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = subgroup_name,
        number_of_interactions = 0L,
        omnibus_statistic = NA_real_,
        degrees_of_freedom = NA_integer_,
        omnibus_p_value = NA_real_
      )
    )
  }
  
  interaction_coefficients <- coefficient_vector[
    interaction_terms
  ]
  
  interaction_variance <- variance_matrix[
    interaction_terms,
    interaction_terms,
    drop = FALSE
  ]
  
  inverse_variance <- safe_matrix_inverse(
    interaction_variance
  )
  
  test_statistic <- as.numeric(
    t(interaction_coefficients) %*%
      inverse_variance %*%
      interaction_coefficients
  )
  
  degrees_of_freedom <- length(
    interaction_coefficients
  )
  
  omnibus_p_value <- pchisq(
    test_statistic,
    df = degrees_of_freedom,
    lower.tail = FALSE
  )
  
  data.table(
    outcome = outcome_name,
    outcome_family = outcome_family,
    subgroup = subgroup_name,
    number_of_interactions =
      degrees_of_freedom,
    omnibus_statistic =
      test_statistic,
    degrees_of_freedom =
      degrees_of_freedom,
    omnibus_p_value =
      omnibus_p_value
  )
}


# ==============================================================================
# STEP 16: Estimate all corrected interaction models
# ==============================================================================

field_effect_results <- list()
omnibus_results <- list()
failed_models <- list()

effect_counter <- 1L
omnibus_counter <- 1L
failure_counter <- 1L

for (outcome_index in seq_len(nrow(outcome_definitions))) {
  
  outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  eligibility_variable <-
    outcome_definitions$eligibility_variable[outcome_index]
  
  outcome_family <-
    outcome_definitions$outcome_family[outcome_index]
  
  for (current_subgroup in c(
    "Pooled",
    "Women",
    "Men"
  )) {
    
    cat(
      "\n============================================================\n",
      "Estimating corrected field interactions:\n",
      outcome_name,
      "|",
      current_subgroup,
      "\n",
      "============================================================\n"
    )
    
    model_data <- dt[
      get(eligibility_variable) == TRUE &
        !is.na(get(outcome_variable)) &
        !is.na(field_group)
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
    
    fields_present <- unique(
      as.character(
        model_data$field_group
      )
    )
    
    if (!reference_field %in% fields_present) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = current_subgroup,
        reason = "Reference field is absent from the estimation sample",
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      next
    }
    
    if (length(fields_present) < 2L) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = current_subgroup,
        reason = "Fewer than two fields are present",
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      next
    }
    
    current_model <- tryCatch(
      estimate_interaction_model(
        analysis_data = model_data,
        outcome_variable = outcome_variable,
        subgroup_name = current_subgroup
      ),
      error = function(e) e
    )
    
    if (inherits(current_model, "error")) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = current_subgroup,
        reason = current_model$message,
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      next
    }
    
    current_field_effects <- tryCatch(
      extract_field_effects(
        model = current_model,
        outcome_name = outcome_name,
        outcome_family = outcome_family,
        subgroup_name = current_subgroup,
        analysis_data = model_data
      ),
      error = function(e) e
    )
    
    if (inherits(current_field_effects, "error")) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = current_subgroup,
        reason = paste0(
          "Effect extraction failed: ",
          current_field_effects$message
        ),
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      
    } else {
      
      field_effect_results[[effect_counter]] <-
        current_field_effects
      
      effect_counter <- effect_counter + 1L
    }
    
    current_omnibus_result <- tryCatch(
      calculate_omnibus_test(
        model = current_model,
        outcome_name = outcome_name,
        outcome_family = outcome_family,
        subgroup_name = current_subgroup
      ),
      error = function(e) e
    )
    
    if (inherits(current_omnibus_result, "error")) {
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_name,
        outcome_family = outcome_family,
        subgroup = current_subgroup,
        reason = paste0(
          "Omnibus test failed: ",
          current_omnibus_result$message
        ),
        observations = nrow(model_data)
      )
      
      failure_counter <- failure_counter + 1L
      
    } else {
      
      omnibus_results[[omnibus_counter]] <-
        current_omnibus_result
      
      omnibus_counter <- omnibus_counter + 1L
    }
    
    rm(
      current_model,
      model_data,
      current_field_effects,
      current_omnibus_result
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 17: Combine corrected field-effect results
# ==============================================================================

if (length(field_effect_results) == 0L) {
  stop("No corrected field-interaction models were successfully estimated.")
}

corrected_field_effects <- rbindlist(
  field_effect_results,
  fill = TRUE
)

setorder(
  corrected_field_effects,
  outcome,
  subgroup,
  field_group
)


# ==============================================================================
# STEP 18: Add significance markers
# ==============================================================================

corrected_field_effects[
  ,
  significance := fcase(
    p_value < 0.01, "***",
    p_value < 0.05, "**",
    p_value < 0.10, "*",
    default = ""
  )
]

corrected_field_effects[
  ,
  difference_significance := fcase(
    difference_p_value < 0.01, "***",
    difference_p_value < 0.05, "**",
    difference_p_value < 0.10, "*",
    default = ""
  )
]


# ==============================================================================
# STEP 19: Multiple-testing corrections
#
# Correction 1:
#   Field-specific total-effect p-values within each outcome and subgroup.
#
# Correction 2:
#   Differences from Education within each outcome and subgroup.
#
# Correction 3:
#   Main pooled primary family:
#   10 fields x 3 primary outcomes.
# ==============================================================================

corrected_field_effects[
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

corrected_field_effects[
  ,
  difference_p_value_fdr_within_outcome_subgroup :=
    p.adjust(
      difference_p_value,
      method = "BH"
    ),
  by = .(
    outcome,
    subgroup
  )
]

corrected_field_effects[
  ,
  p_value_fdr_primary_pooled := NA_real_
]

primary_pooled_rows <- which(
  corrected_field_effects$outcome_family == "Primary" &
    corrected_field_effects$subgroup == "Pooled" &
    !is.na(corrected_field_effects$p_value)
)

corrected_field_effects[
  primary_pooled_rows,
  p_value_fdr_primary_pooled :=
    p.adjust(
      p_value,
      method = "BH"
    )
]


# ==============================================================================
# STEP 20: Add FDR significance indicators
# ==============================================================================

corrected_field_effects[
  ,
  significant_fdr_10_within :=
    !is.na(
      p_value_fdr_within_outcome_subgroup
    ) &
    p_value_fdr_within_outcome_subgroup < 0.10
]

corrected_field_effects[
  ,
  significant_fdr_05_within :=
    !is.na(
      p_value_fdr_within_outcome_subgroup
    ) &
    p_value_fdr_within_outcome_subgroup < 0.05
]

corrected_field_effects[
  ,
  significant_fdr_primary_pooled_10 :=
    !is.na(
      p_value_fdr_primary_pooled
    ) &
    p_value_fdr_primary_pooled < 0.10
]


# ==============================================================================
# STEP 21: Reliability indicators
# ==============================================================================

corrected_field_effects[
  ,
  minimum_required_observations := fifelse(
    subgroup == "Pooled",
    5000L,
    2000L
  )
]

corrected_field_effects[
  ,
  reliable_sample :=
    observations >= minimum_required_observations &
    historical_province_clusters >= 15L &
    cohort_clusters >= 15L &
    survey_years >= 5L
]


# ==============================================================================
# STEP 22: Save full corrected estimates
# ==============================================================================

fwrite(
  corrected_field_effects,
  file.path(
    output_folder,
    "step15B_corrected_field_total_effects.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Combine and save omnibus tests
# ==============================================================================

corrected_omnibus_tests <- rbindlist(
  omnibus_results,
  fill = TRUE
)

corrected_omnibus_tests[
  ,
  omnibus_p_value_fdr :=
    p.adjust(
      omnibus_p_value,
      method = "BH"
    )
]

setorder(
  corrected_omnibus_tests,
  outcome,
  subgroup
)

print(corrected_omnibus_tests)

fwrite(
  corrected_omnibus_tests,
  file.path(
    output_folder,
    "step15B_field_heterogeneity_omnibus_tests.csv"
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
    outcome_family = character(),
    subgroup = character(),
    reason = character(),
    observations = integer()
  )
}

fwrite(
  failed_model_table,
  file.path(
    output_folder,
    "step15B_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Compact results table
# ==============================================================================

compact_results <- corrected_field_effects[
  model_status == "Estimated",
  .(
    outcome,
    outcome_family,
    field_group,
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
    
    p_value =
      round(
        p_value,
        4
      ),
    
    p_value_fdr_within =
      round(
        p_value_fdr_within_outcome_subgroup,
        4
      ),
    
    p_value_fdr_primary_pooled =
      round(
        p_value_fdr_primary_pooled,
        4
      ),
    
    difference_from_education =
      round(
        difference_from_education_pp,
        3
      ),
    
    difference_p_value =
      round(
        difference_p_value,
        4
      ),
    
    difference_p_value_fdr =
      round(
        difference_p_value_fdr_within_outcome_subgroup,
        4
      ),
    
    significance,
    difference_significance,
    observations,
    historical_province_clusters,
    cohort_clusters,
    survey_years,
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
    "step15B_corrected_field_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Main pooled primary-outcome table
# ==============================================================================

main_pooled_results <- compact_results[
  outcome_family == "Primary" &
    subgroup == "Pooled" &
    reliable_sample == TRUE,
  .(
    outcome,
    field_group,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    p_value_fdr_within,
    p_value_fdr_primary_pooled,
    difference_from_education,
    difference_p_value,
    observations
  )
]

setorder(
  main_pooled_results,
  field_group,
  outcome
)

fwrite(
  main_pooled_results,
  file.path(
    output_folder,
    "step15B_main_pooled_primary_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Wide publication-style table
# ==============================================================================

main_table_effects <- main_pooled_results[
  ,
  .(
    field_group,
    outcome,
    table_entry = paste0(
      estimate_with_stars,
      " ",
      standard_error_parentheses
    )
  )
]

main_table_wide <- dcast(
  main_table_effects,
  field_group ~ outcome,
  value.var = "table_entry"
)

fwrite(
  main_table_wide,
  file.path(
    output_folder,
    "step15B_main_pooled_primary_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 28: Results surviving FDR correction
# ==============================================================================

results_surviving_fdr <- corrected_field_effects[
  model_status == "Estimated" &
    reliable_sample == TRUE &
    (
      significant_fdr_10_within == TRUE |
        significant_fdr_primary_pooled_10 == TRUE
    )
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
    "step15B_results_surviving_FDR.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 29: Formal differences from Education
# ==============================================================================

field_differences_from_education <- corrected_field_effects[
  model_status == "Estimated" &
    field_group != reference_field,
  .(
    outcome,
    outcome_family,
    subgroup,
    field_group,
    
    difference_from_education_pp,
    
    difference_standard_error_pp,
    
    difference_p_value,
    
    difference_p_value_fdr =
      difference_p_value_fdr_within_outcome_subgroup,
    
    difference_significance,
    
    reliable_sample,
    
    observations
  )
]

setorder(
  field_differences_from_education,
  outcome,
  subgroup,
  field_group
)

fwrite(
  field_differences_from_education,
  file.path(
    output_folder,
    "step15B_formal_differences_from_education.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 30: Diagnostic summary
# ==============================================================================

diagnostic_summary <- corrected_field_effects[
  model_status == "Estimated",
  .(
    field_effects_estimated = .N,
    
    reliable_field_effects =
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
    
    FDR_within_p_below_010 =
      sum(
        p_value_fdr_within_outcome_subgroup < 0.10,
        na.rm = TRUE
      ),
    
    FDR_within_p_below_005 =
      sum(
        p_value_fdr_within_outcome_subgroup < 0.05,
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
    "step15B_diagnostic_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 31: Print main results
# ==============================================================================

cat(
  "\n============================================================\n",
  "CORRECTED POOLED PRIMARY FIELD EFFECTS\n",
  "============================================================\n"
)

print(
  corrected_field_effects[
    outcome_family == "Primary" &
      subgroup == "Pooled" &
      model_status == "Estimated",
    .(
      outcome,
      field_group,
      
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
      
      FDR_within =
        round(
          p_value_fdr_within_outcome_subgroup,
          4
        ),
      
      FDR_primary_family =
        round(
          p_value_fdr_primary_pooled,
          4
        ),
      
      difference_from_education =
        round(
          difference_from_education_pp,
          3
        ),
      
      difference_p_value =
        round(
          difference_p_value,
          4
        )
    )
  ]
)

cat(
  "\n============================================================\n",
  "OMNIBUS FIELD-HETEROGENEITY TESTS\n",
  "============================================================\n"
)

print(corrected_omnibus_tests)


# ==============================================================================
# STEP 32: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 15B COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step15B_corrected_field_total_effects.csv\n",
  "2. step15B_field_heterogeneity_omnibus_tests.csv\n",
  "3. step15B_corrected_field_compact_results.csv\n",
  "4. step15B_main_pooled_primary_results.csv\n",
  "5. step15B_main_pooled_primary_table_wide.csv\n",
  "6. step15B_formal_differences_from_education.csv\n",
  "7. step15B_results_surviving_FDR.csv\n",
  "8. step15B_diagnostic_summary.csv\n",
  "9. step15B_failed_models.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nInterpretation rules:\n",
  "- Total-effect p-values test whether the effect within a field equals zero.\n",
  "- Difference p-values test whether the field effect differs from Education.\n",
  "- Omnibus p-values test whether exposure effects differ jointly across fields.\n",
  "- Prioritize pooled primary results that survive FDR correction.\n"
)