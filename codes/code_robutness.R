# ============================================================
# ADDITIONAL ROBUSTNESS CHECKS FOR THE THREE MAIN MISMATCH OUTCOMES
# ============================================================
#
# Main outcomes:
#   1. Horizontal mismatch
#   2. Double mismatch
#   3. Fully matched employment
#
# Main treatment:
#   Log average undergraduate/professional enrollment per
#   100,000 population at ages 18–20
#
# Baseline specification:
#   - Historical-province fixed effects
#   - Birth-cohort fixed effects
#   - Survey-year fixed effects
#   - Female and urban controls
#   - LFS survey weights
#   - Two-way clustering by province and birth cohort
#
# Robustness checks:
#   - Alternative clustering
#   - Unweighted estimation
#   - Province-specific linear cohort trends
#   - Excluding identified cross-province movers
#   - Excluding Tehran
#   - Excluding Tehran and movers
#   - Province exclusions
#   - Leave-one-province-out analysis
#
# ============================================================


# ------------------------------------------------------------
# 0. Packages
# ------------------------------------------------------------

required_packages <- c(
  "data.table",
  "fst",
  "fixest",
  "ggplot2"
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
library(ggplot2)


# ------------------------------------------------------------
# 1. File paths
# ------------------------------------------------------------

project_dir <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_dir,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

output_dir <- file.path(
  project_dir,
  "LFS_analysis_outputs",
  "additional_robustness_main_outcomes"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop("Input file was not found: ", input_file)
}


# ------------------------------------------------------------
# 2. Identify available variables
# ------------------------------------------------------------

file_metadata <- fst::metadata_fst(input_file)
available_vars <- file_metadata$columnNames

cat(
  "Number of columns in the fst file:",
  length(available_vars),
  "\n"
)


# Return the first available variable from a candidate list
first_available <- function(candidates, available) {
  
  found <- candidates[candidates %in% available]
  
  if (length(found) == 0L) {
    return(NA_character_)
  }
  
  found[1L]
}


# Core variables
treatment_var <- first_available(
  c("log_undergrad_exposure_18_20"),
  available_vars
)

province_var <- first_available(
  c("province_historical", "historical_province"),
  available_vars
)

cohort_var <- first_available(
  c("analysis_birth_year", "birth_year"),
  available_vars
)

survey_year_var <- first_available(
  c("year", "survey_year"),
  available_vars
)

weight_var <- first_available(
  c("weight", "survey_weight"),
  available_vars
)

gender_var <- first_available(
  c("gender", "sex"),
  available_vars
)

urban_var <- first_available(
  c("urban", "urban_rural_clean2", "urban_rural"),
  available_vars
)

previous_province_var <- first_available(
  c(
    "previous_province_historical",
    "previous_residence_province_historical",
    "previous_residence_province"
  ),
  available_vars
)

sample_var <- first_available(
  c(
    "sample_main_valid_cohort",
    "sample_main",
    "baseline_exposure_sample"
  ),
  available_vars
)


# Exact outcome names in the current dataset
outcome_candidates <- c(
  horizontal_mismatch =
    "horizontal_mismatch_detailed_broad",
  
  double_mismatch =
    "both_refined_vertical_horizontal_mismatch",
  
  fully_matched =
    "fully_matched_refined"
)


# Exact eligibility variables
eligibility_candidates <- c(
  horizontal_mismatch =
    "horizontal_mismatch_eligible",
  
  double_mismatch =
    "both_refined_mismatch_available",
  
  fully_matched =
    "both_refined_mismatch_available"
)


# ------------------------------------------------------------
# 3. Validate required variables
# ------------------------------------------------------------

essential_vars <- c(
  treatment_var,
  province_var,
  cohort_var,
  survey_year_var,
  weight_var,
  gender_var
)

if (anyNA(essential_vars)) {
  
  stop(
    "At least one essential variable was not found.\n",
    "Treatment: ", treatment_var, "\n",
    "Province: ", province_var, "\n",
    "Cohort: ", cohort_var, "\n",
    "Survey year: ", survey_year_var, "\n",
    "Weight: ", weight_var, "\n",
    "Gender: ", gender_var
  )
}


missing_outcomes <- setdiff(
  unname(outcome_candidates),
  available_vars
)

if (length(missing_outcomes) > 0L) {
  
  stop(
    "The following outcome variables were not found: ",
    paste(missing_outcomes, collapse = ", ")
  )
}


missing_eligibility <- setdiff(
  unique(unname(eligibility_candidates)),
  available_vars
)

if (length(missing_eligibility) > 0L) {
  
  warning(
    "The following eligibility variables were not found: ",
    paste(missing_eligibility, collapse = ", "),
    ". Nonmissing outcome values will be used instead."
  )
}


cat("\nVariables selected:\n")
cat("Treatment:", treatment_var, "\n")
cat("Province:", province_var, "\n")
cat("Birth cohort:", cohort_var, "\n")
cat("Survey year:", survey_year_var, "\n")
cat("Weight:", weight_var, "\n")
cat("Gender:", gender_var, "\n")
cat("Urban:", urban_var, "\n")
cat("Previous province:", previous_province_var, "\n")
cat("Sample flag:", sample_var, "\n")

print(outcome_candidates)
print(eligibility_candidates)


# ------------------------------------------------------------
# 4. Read only required columns
# ------------------------------------------------------------

columns_to_read <- unique(
  na.omit(
    c(
      treatment_var,
      province_var,
      cohort_var,
      survey_year_var,
      weight_var,
      gender_var,
      urban_var,
      previous_province_var,
      sample_var,
      unname(outcome_candidates),
      unname(eligibility_candidates)
    )
  )
)

cat(
  "\nReading",
  length(columns_to_read),
  "columns...\n"
)

dt <- as.data.table(
  fst::read_fst(
    input_file,
    columns = columns_to_read,
    as.data.table = TRUE
  )
)

cat(
  "Rows read:",
  format(nrow(dt), big.mark = ","),
  "\n"
)


# ------------------------------------------------------------
# 5. Rename variables used in this script
# ------------------------------------------------------------

setnames(dt, treatment_var, "treatment")
setnames(dt, province_var, "province")
setnames(dt, cohort_var, "birth_cohort")
setnames(dt, survey_year_var, "survey_year")
setnames(dt, weight_var, "survey_weight")
setnames(dt, gender_var, "gender_original")

if (!is.na(urban_var)) {
  setnames(dt, urban_var, "urban_original")
}

if (!is.na(previous_province_var)) {
  setnames(dt, previous_province_var, "previous_province")
}

if (!is.na(sample_var)) {
  setnames(dt, sample_var, "main_sample_flag")
}


# Rename outcomes
setnames(
  dt,
  old = unname(outcome_candidates),
  new = names(outcome_candidates)
)


# Rename eligibility indicators when available
if (
  "horizontal_mismatch_eligible" %in% names(dt)
) {
  setnames(
    dt,
    "horizontal_mismatch_eligible",
    "horizontal_eligibility_original"
  )
}

if (
  "both_refined_mismatch_available" %in% names(dt)
) {
  setnames(
    dt,
    "both_refined_mismatch_available",
    "joint_eligibility_original"
  )
}


# ------------------------------------------------------------
# 6. Construct control variables
# ------------------------------------------------------------

# Verified coding:
# gender == 1 means women
# gender == 2 means men
dt[
  ,
  female := as.integer(gender_original == 1)
]


# Standardize urban status
if ("urban_original" %in% names(dt)) {
  
  if (
    is.numeric(dt$urban_original) ||
    is.integer(dt$urban_original) ||
    is.logical(dt$urban_original)
  ) {
    
    unique_urban_values <- sort(
      unique(dt$urban_original[!is.na(dt$urban_original)])
    )
    
    cat(
      "\nUnique numeric urban values:",
      paste(unique_urban_values, collapse = ", "),
      "\n"
    )
    
    # Main project coding has urban == 1 and rural == 0
    dt[
      ,
      urban := as.integer(urban_original == 1)
    ]
    
  } else {
    
    dt[
      ,
      urban_text :=
        tolower(trimws(as.character(urban_original)))
    ]
    
    dt[
      ,
      urban := fifelse(
        urban_text %in% c("urban", "1"),
        1L,
        fifelse(
          urban_text %in% c("rural", "0"),
          0L,
          NA_integer_
        )
      )
    ]
  }
  
} else {
  
  warning(
    "No urban variable was found. ",
    "Models will include only the female control."
  )
  
  dt[, urban := NA_integer_]
}


# ------------------------------------------------------------
# 7. Construct the baseline sample
# ------------------------------------------------------------

if ("main_sample_flag" %in% names(dt)) {
  
  dt[
    ,
    base_sample :=
      main_sample_flag == 1
  ]
  
} else {
  
  dt[, base_sample := TRUE]
}


# Essential sample restrictions
dt[
  ,
  base_sample :=
    base_sample &
    !is.na(treatment) &
    is.finite(treatment) &
    !is.na(province) &
    !is.na(birth_cohort) &
    !is.na(survey_year) &
    !is.na(female) &
    !is.na(survey_weight) &
    is.finite(survey_weight) &
    survey_weight > 0
]


# ------------------------------------------------------------
# 8. Construct outcome-specific eligibility
# ------------------------------------------------------------

if ("horizontal_eligibility_original" %in% names(dt)) {
  
  dt[
    ,
    eligible_horizontal :=
      horizontal_eligibility_original == 1
  ]
  
} else {
  
  dt[
    ,
    eligible_horizontal :=
      !is.na(horizontal_mismatch)
  ]
}


if ("joint_eligibility_original" %in% names(dt)) {
  
  dt[
    ,
    eligible_joint :=
      joint_eligibility_original == 1
  ]
  
} else {
  
  dt[
    ,
    eligible_joint :=
      !is.na(double_mismatch) &
      !is.na(fully_matched)
  ]
}


# Validate logical consistency of joint outcomes
joint_validation <- dt[
  base_sample == TRUE &
    eligible_joint == TRUE,
  .(
    observations = .N,
    
    double_mismatch_missing =
      sum(is.na(double_mismatch)),
    
    fully_matched_missing =
      sum(is.na(fully_matched)),
    
    both_equal_one =
      sum(
        double_mismatch == 1 &
          fully_matched == 1,
        na.rm = TRUE
      ),
    
    double_mismatch_rate =
      mean(double_mismatch, na.rm = TRUE),
    
    fully_matched_rate =
      mean(fully_matched, na.rm = TRUE)
  )
]

fwrite(
  joint_validation,
  file.path(
    output_dir,
    "joint_outcome_validation.csv"
  )
)

cat("\nJoint outcome validation:\n")
print(joint_validation)


# ------------------------------------------------------------
# 9. Construct migration indicator
# ------------------------------------------------------------

if ("previous_province" %in% names(dt)) {
  
  dt[
    ,
    province_clean :=
      tolower(trimws(as.character(province)))
  ]
  
  dt[
    ,
    previous_province_clean :=
      tolower(trimws(as.character(previous_province)))
  ]
  
  dt[
    previous_province_clean %in%
      c("", "na", "nan", "null", "."),
    previous_province_clean := NA_character_
  ]
  
  # Identified mover:
  # previous province is observed and differs from current province
  dt[
    ,
    recent_cross_province_mover :=
      !is.na(previous_province_clean) &
      previous_province_clean != province_clean
  ]
  
} else {
  
  warning(
    "Previous-province information was not found. ",
    "Migration-restricted specifications will be skipped."
  )
  
  dt[, province_clean :=
       tolower(trimws(as.character(province)))]
  
  dt[, recent_cross_province_mover := NA]
}


# ------------------------------------------------------------
# 10. Inspect province labels
# ------------------------------------------------------------

province_counts <- dt[
  base_sample == TRUE,
  .(
    observations = .N,
    weighted_population =
      sum(survey_weight, na.rm = TRUE)
  ),
  by = .(
    province,
    province_clean
  )
][order(province_clean)]

fwrite(
  province_counts,
  file.path(
    output_dir,
    "province_name_and_sample_check.csv"
  )
)

cat("\nProvince names and counts:\n")
print(province_counts)


# ------------------------------------------------------------
# 11. Define robustness specifications
# ------------------------------------------------------------

robustness_specs <- list(
  
  baseline = list(
    label = "Baseline",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) rep(TRUE, nrow(x))
  ),
  
  province_cluster = list(
    label = "Cluster by province",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "province",
    filter_function = function(x) rep(TRUE, nrow(x))
  ),
  
  cohort_cluster = list(
    label = "Cluster by birth cohort",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "cohort",
    filter_function = function(x) rep(TRUE, nrow(x))
  ),
  
  unweighted = list(
    label = "Unweighted",
    weighted = FALSE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) rep(TRUE, nrow(x))
  ),
  
  province_cohort_trends = list(
    label = "Province-specific cohort trends",
    weighted = TRUE,
    province_trends = TRUE,
    cluster_type = "two_way",
    filter_function = function(x) rep(TRUE, nrow(x))
  ),
  
  exclude_recent_movers = list(
    label = "Exclude identified cross-province movers",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      is.na(x$recent_cross_province_mover) |
        x$recent_cross_province_mover == FALSE
    }
  ),
  
  exclude_tehran = list(
    label = "Exclude Tehran",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      !x$province_clean %in% c("tehran")
    }
  ),
  
  exclude_tehran_and_movers = list(
    label = "Exclude Tehran and identified movers",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      !x$province_clean %in% c("tehran") &
        (
          is.na(x$recent_cross_province_mover) |
            x$recent_cross_province_mover == FALSE
        )
    }
  ),
  
  exclude_semnan = list(
    label = "Exclude Semnan",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      !x$province_clean %in% c("semnan")
    }
  ),
  
  exclude_semnan_qazvin = list(
    label = "Exclude Semnan and Qazvin",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      !x$province_clean %in%
        c("semnan", "qazvin")
    }
  ),
  
  exclude_new_provinces = list(
    label = "Exclude Qazvin, Qom, and Golestan",
    weighted = TRUE,
    province_trends = FALSE,
    cluster_type = "two_way",
    filter_function = function(x) {
      !x$province_clean %in%
        c("qazvin", "qom", "golestan")
    }
  )
)


# ------------------------------------------------------------
# 12. Define outcomes
# ------------------------------------------------------------

outcome_info <- list(
  
  horizontal_mismatch = list(
    label = "Horizontal mismatch",
    eligibility = "eligible_horizontal"
  ),
  
  double_mismatch = list(
    label = "Double mismatch",
    eligibility = "eligible_joint"
  ),
  
  fully_matched = list(
    label = "Fully matched employment",
    eligibility = "eligible_joint"
  )
)


# ------------------------------------------------------------
# 13. Functions for estimation
# ------------------------------------------------------------

# A 10 percent increase in exposure
effect_scale <- 100 * log(1.10)


build_formula <- function(
    outcome,
    include_urban,
    province_trends
) {
  
  controls <- "female"
  
  if (include_urban) {
    controls <- paste(
      controls,
      "urban",
      sep = " + "
    )
  }
  
  if (province_trends) {
    
    formula_text <- paste0(
      outcome,
      " ~ treatment + ",
      controls,
      " | province + birth_cohort + survey_year + ",
      "province[birth_cohort]"
    )
    
  } else {
    
    formula_text <- paste0(
      outcome,
      " ~ treatment + ",
      controls,
      " | province + birth_cohort + survey_year"
    )
  }
  
  as.formula(formula_text)
}


get_cluster_formula <- function(cluster_type) {
  
  if (cluster_type == "two_way") {
    return(~ province + birth_cohort)
  }
  
  if (cluster_type == "province") {
    return(~ province)
  }
  
  if (cluster_type == "cohort") {
    return(~ birth_cohort)
  }
  
  stop("Unknown cluster type: ", cluster_type)
}


extract_treatment_result <- function(
    model,
    outcome_label,
    specification_id,
    specification_label,
    weighted,
    province_trends,
    cluster_type
) {
  
  coefficient_table <- as.data.table(
    coeftable(model),
    keep.rownames = "term"
  )
  
  treatment_row <- coefficient_table[
    term == "treatment"
  ]
  
  if (nrow(treatment_row) != 1L) {
    stop(
      "Treatment coefficient could not be identified."
    )
  }
  
  estimate_column <- grep(
    "^Estimate$",
    names(treatment_row),
    value = TRUE
  )[1L]
  
  se_column <- grep(
    "Std\\. Error|Std. Error",
    names(treatment_row),
    value = TRUE
  )[1L]
  
  p_column <- grep(
    "Pr\\(",
    names(treatment_row),
    value = TRUE
  )[1L]
  
  beta <- treatment_row[[estimate_column]]
  se_beta <- treatment_row[[se_column]]
  p_value <- treatment_row[[p_column]]
  
  effect_pp <- effect_scale * beta
  se_pp <- effect_scale * se_beta
  
  data.table(
    outcome = outcome_label,
    specification_id = specification_id,
    specification = specification_label,
    weighted = weighted,
    province_specific_cohort_trends =
      province_trends,
    clustering = cluster_type,
    
    coefficient_log_exposure = beta,
    standard_error_log_exposure = se_beta,
    
    effect_pp_for_10_percent_increase =
      effect_pp,
    
    standard_error_pp = se_pp,
    
    lower_95_pp =
      effect_pp - 1.96 * se_pp,
    
    upper_95_pp =
      effect_pp + 1.96 * se_pp,
    
    p_value = p_value,
    observations = nobs(model)
  )
}






# ------------------------------------------------------------
# Correct the double-mismatch outcome
# ------------------------------------------------------------

correct_double_mismatch <- fst::read_fst(
  input_file,
  columns = "overeducated_and_horizontal_mismatch_refined",
  as.data.table = TRUE
)

# The fst column is read in the same row order as dt
if (nrow(correct_double_mismatch) != nrow(dt)) {
  stop(
    "Row counts do not match. Do not assign the corrected outcome."
  )
}

# Replace the incorrect broad vertical-horizontal outcome
dt[
  ,
  double_mismatch :=
    correct_double_mismatch$
    overeducated_and_horizontal_mismatch_refined
]

# Check the corrected outcome
dt[
  base_sample == TRUE &
    eligible_joint == TRUE,
  .(
    observations = .N,
    missing_double_mismatch =
      sum(is.na(double_mismatch)),
    double_mismatch_rate =
      mean(double_mismatch, na.rm = TRUE)
  )
]

# ------------------------------------------------------------
# 14. Estimate all robustness specifications
# ------------------------------------------------------------

all_results <- list()
failed_models <- list()

result_counter <- 0L
failure_counter <- 0L


# Reset stored results
all_results <- list()
failed_models <- list()

result_counter <- 0L
failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <- outcome_info[[outcome_name]]$label
  eligibility_var <- outcome_info[[outcome_name]]$eligibility
  
  
  for (spec_id in names(robustness_specs)) {
    
    spec <- robustness_specs[[spec_id]]
    
    
    # Skip migration specifications if previous-province
    # information is unavailable
    if (
      spec_id %in%
      c(
        "exclude_recent_movers",
        "exclude_tehran_and_movers"
      ) &&
      !"previous_province" %in% names(dt)
    ) {
      next
    }
    
    
    # Apply the specification-specific restriction
    filter_vector <- spec$filter_function(dt)
    
    
    estimation_sample <- dt[
      base_sample == TRUE &
        get(eligibility_var) == TRUE &
        !is.na(get(outcome_name)) &
        filter_vector
    ]
    
    
    # Include urban only when it is observed and varies
    include_urban <- (
      "urban" %in% names(estimation_sample) &&
        sum(!is.na(estimation_sample$urban)) > 0L &&
        uniqueN(
          estimation_sample$urban[
            !is.na(estimation_sample$urban)
          ]
        ) > 1L
    )
    
    
    if (include_urban) {
      estimation_sample <- estimation_sample[!is.na(urban)]
    }
    
    
    if (nrow(estimation_sample) == 0L) {
      
      failure_counter <- failure_counter + 1L
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_label,
        specification_id = spec_id,
        specification = spec$label,
        error_message =
          "The estimation sample contained zero observations."
      )
      
      next
    }
    
    
    model_formula <- build_formula(
      outcome = outcome_name,
      include_urban = include_urban,
      province_trends = spec$province_trends
    )
    
    
    cluster_formula <- get_cluster_formula(
      spec$cluster_type
    )
    
    
    cat(
      "\nEstimating:",
      outcome_label,
      "|",
      spec$label,
      "| N =",
      format(
        nrow(estimation_sample),
        big.mark = ","
      ),
      "\n"
    )
    
    
    model_attempt <- tryCatch(
      
      {
        
        if (spec$weighted) {
          
          feols(
            fml = model_formula,
            data = estimation_sample,
            weights = ~ survey_weight,
            cluster = cluster_formula,
            warn = FALSE,
            notes = FALSE
          )
          
        } else {
          
          feols(
            fml = model_formula,
            data = estimation_sample,
            cluster = cluster_formula,
            warn = FALSE,
            notes = FALSE
          )
        }
      },
      
      error = function(e) e
    )
    
    
    if (inherits(model_attempt, "error")) {
      
      failure_counter <- failure_counter + 1L
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_label,
        specification_id = spec_id,
        specification = spec$label,
        error_message = conditionMessage(model_attempt)
      )
      
      next
    }
    
    
    extraction_attempt <- tryCatch(
      
      extract_treatment_result(
        model = model_attempt,
        outcome_label = outcome_label,
        specification_id = spec_id,
        specification_label = spec$label,
        weighted = spec$weighted,
        province_trends = spec$province_trends,
        cluster_type = spec$cluster_type
      ),
      
      error = function(e) e
    )
    
    
    if (inherits(extraction_attempt, "error")) {
      
      failure_counter <- failure_counter + 1L
      
      failed_models[[failure_counter]] <- data.table(
        outcome = outcome_label,
        specification_id = spec_id,
        specification = spec$label,
        error_message = conditionMessage(extraction_attempt)
      )
      
      next
    }
    
    
    result_counter <- result_counter + 1L
    all_results[[result_counter]] <- extraction_attempt
  }
}


# Stop only if every model failed
if (length(all_results) == 0L) {
  
  if (length(failed_models) > 0L) {
    
    print(
      rbindlist(
        failed_models,
        use.names = TRUE,
        fill = TRUE
      )
    )
  }
  
  stop(
    "No models were successfully estimated. ",
    "Inspect the printed failure messages."
  )
}


# Combine successful estimates
robustness_results <- rbindlist(
  all_results,
  use.names = TRUE,
  fill = TRUE
)


# Combine failed-model records
if (length(failed_models) > 0L) {
  
  failed_results <- rbindlist(
    failed_models,
    use.names = TRUE,
    fill = TRUE
  )
  
} else {
  
  failed_results <- data.table(
    outcome = character(),
    specification_id = character(),
    specification = character(),
    error_message = character()
  )
}


# Save detailed estimates and failures
fwrite(
  robustness_results,
  file.path(
    output_dir,
    "additional_robustness_detailed_results.csv"
  )
)

fwrite(
  failed_results,
  file.path(
    output_dir,
    "additional_robustness_failed_models.csv"
  )
)


# ------------------------------------------------------------
# 15. Format robustness results
# ------------------------------------------------------------

robustness_results[
  ,
  stars := fifelse(
    p_value < 0.01,
    "***",
    fifelse(
      p_value < 0.05,
      "**",
      fifelse(
        p_value < 0.10,
        "*",
        ""
      )
    )
  )
]


robustness_results[
  ,
  formatted_result := sprintf(
    "%.3f%s (%.3f)",
    effect_pp_for_10_percent_increase,
    stars,
    standard_error_pp
  )
]


# Specifications for the compact main table
main_table_specifications <- c(
  "baseline",
  "unweighted",
  "province_cohort_trends",
  "exclude_recent_movers",
  "exclude_tehran",
  "exclude_tehran_and_movers",
  "exclude_semnan",
  "exclude_semnan_qazvin",
  "exclude_new_provinces"
)


main_table_long <- robustness_results[
  specification_id %in% main_table_specifications,
  .(
    specification_id,
    specification,
    outcome,
    formatted_result,
    observations
  )
]


main_table_wide <- dcast(
  main_table_long,
  specification_id + specification ~ outcome,
  value.var = "formatted_result"
)


main_table_wide[
  ,
  specification_order := match(
    specification_id,
    main_table_specifications
  )
]

setorder(
  main_table_wide,
  specification_order
)

main_table_wide[
  ,
  specification_order := NULL
]


fwrite(
  main_table_wide,
  file.path(
    output_dir,
    "additional_robustness_main_table_wide.csv"
  )
)


# Sample-size table
sample_size_table <- dcast(
  main_table_long,
  specification_id + specification ~ outcome,
  value.var = "observations"
)

fwrite(
  sample_size_table,
  file.path(
    output_dir,
    "additional_robustness_sample_sizes.csv"
  )
)


# ------------------------------------------------------------
# 16. Alternative clustering results
# ------------------------------------------------------------

clustering_results <- robustness_results[
  specification_id %in%
    c(
      "baseline",
      "province_cluster",
      "cohort_cluster"
    ),
  .(
    outcome,
    specification_id,
    specification,
    effect_pp_for_10_percent_increase,
    standard_error_pp,
    p_value,
    observations
  )
]


clustering_results[
  ,
  formatted_se := sprintf(
    "%.3f",
    standard_error_pp
  )
]


clustering_table_wide <- dcast(
  clustering_results,
  outcome + effect_pp_for_10_percent_increase ~ specification,
  value.var = "formatted_se"
)


fwrite(
  clustering_results,
  file.path(
    output_dir,
    "alternative_clustering_detailed.csv"
  )
)

fwrite(
  clustering_table_wide,
  file.path(
    output_dir,
    "alternative_clustering_table_wide.csv"
  )
)


# ------------------------------------------------------------
# 17. Sign-stability summary
# ------------------------------------------------------------

sign_stability <- robustness_results[
  specification_id %in% main_table_specifications,
  .(
    number_specifications = .N,
    
    minimum_effect = min(
      effect_pp_for_10_percent_increase,
      na.rm = TRUE
    ),
    
    maximum_effect = max(
      effect_pp_for_10_percent_increase,
      na.rm = TRUE
    ),
    
    all_effects_positive = all(
      effect_pp_for_10_percent_increase > 0
    ),
    
    all_effects_negative = all(
      effect_pp_for_10_percent_increase < 0
    ),
    
    significant_at_10_percent = sum(
      p_value < 0.10,
      na.rm = TRUE
    ),
    
    significant_at_5_percent = sum(
      p_value < 0.05,
      na.rm = TRUE
    ),
    
    significant_at_1_percent = sum(
      p_value < 0.01,
      na.rm = TRUE
    )
  ),
  by = outcome
]


fwrite(
  sign_stability,
  file.path(
    output_dir,
    "additional_robustness_sign_stability.csv"
  )
)


# ------------------------------------------------------------
# 18. Leave-one-province-out analysis
# ------------------------------------------------------------

province_list <- sort(
  unique(
    as.character(
      dt[
        base_sample == TRUE &
          !is.na(province),
        province
      ]
    )
  )
)


loo_results <- list()
loo_failures <- list()

loo_counter <- 0L
loo_failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <- outcome_info[[outcome_name]]$label
  eligibility_var <- outcome_info[[outcome_name]]$eligibility
  
  
  for (excluded_province in province_list) {
    
    estimation_sample <- dt[
      base_sample == TRUE &
        get(eligibility_var) == TRUE &
        !is.na(get(outcome_name)) &
        as.character(province) != excluded_province
    ]
    
    
    include_urban <- (
      "urban" %in% names(estimation_sample) &&
        sum(!is.na(estimation_sample$urban)) > 0L &&
        uniqueN(
          estimation_sample$urban[
            !is.na(estimation_sample$urban)
          ]
        ) > 1L
    )
    
    
    if (include_urban) {
      estimation_sample <- estimation_sample[!is.na(urban)]
    }
    
    
    model_formula <- build_formula(
      outcome = outcome_name,
      include_urban = include_urban,
      province_trends = FALSE
    )
    
    
    model_attempt <- tryCatch(
      
      feols(
        fml = model_formula,
        data = estimation_sample,
        weights = ~ survey_weight,
        cluster = ~ province + birth_cohort,
        warn = FALSE,
        notes = FALSE
      ),
      
      error = function(e) e
    )
    
    
    if (inherits(model_attempt, "error")) {
      
      loo_failure_counter <- loo_failure_counter + 1L
      
      loo_failures[[loo_failure_counter]] <- data.table(
        outcome = outcome_label,
        excluded_province = excluded_province,
        error_message = conditionMessage(model_attempt)
      )
      
      next
    }
    
    
    result_row <- tryCatch(
      
      extract_treatment_result(
        model = model_attempt,
        outcome_label = outcome_label,
        specification_id = "leave_one_province_out",
        specification_label = "Leave one province out",
        weighted = TRUE,
        province_trends = FALSE,
        cluster_type = "two_way"
      ),
      
      error = function(e) e
    )
    
    
    if (inherits(result_row, "error")) {
      
      loo_failure_counter <- loo_failure_counter + 1L
      
      loo_failures[[loo_failure_counter]] <- data.table(
        outcome = outcome_label,
        excluded_province = excluded_province,
        error_message = conditionMessage(result_row)
      )
      
      next
    }
    
    
    result_row[
      ,
      excluded_province := excluded_province
    ]
    
    loo_counter <- loo_counter + 1L
    loo_results[[loo_counter]] <- result_row
  }
}


if (length(loo_results) == 0L) {
  stop("All leave-one-province-out models failed.")
}


leave_one_out_results <- rbindlist(
  loo_results,
  use.names = TRUE,
  fill = TRUE
)


if (length(loo_failures) > 0L) {
  
  leave_one_out_failures <- rbindlist(
    loo_failures,
    use.names = TRUE,
    fill = TRUE
  )
  
} else {
  
  leave_one_out_failures <- data.table(
    outcome = character(),
    excluded_province = character(),
    error_message = character()
  )
}


fwrite(
  leave_one_out_results,
  file.path(
    output_dir,
    "leave_one_province_out_results.csv"
  )
)

fwrite(
  leave_one_out_failures,
  file.path(
    output_dir,
    "leave_one_province_out_failures.csv"
  )
)


# ------------------------------------------------------------
# 19. Leave-one-province-out summary
# ------------------------------------------------------------

baseline_values <- robustness_results[
  specification_id == "baseline",
  .(
    outcome,
    baseline_effect =
      effect_pp_for_10_percent_increase
  )
]


leave_one_out_summary <- leave_one_out_results[
  ,
  .(
    minimum_effect = min(
      effect_pp_for_10_percent_increase,
      na.rm = TRUE
    ),
    
    maximum_effect = max(
      effect_pp_for_10_percent_increase,
      na.rm = TRUE
    ),
    
    minimum_excluding_province =
      excluded_province[
        which.min(
          effect_pp_for_10_percent_increase
        )
      ],
    
    maximum_excluding_province =
      excluded_province[
        which.max(
          effect_pp_for_10_percent_increase
        )
      ],
    
    all_positive = all(
      effect_pp_for_10_percent_increase > 0
    ),
    
    all_negative = all(
      effect_pp_for_10_percent_increase < 0
    ),
    
    number_significant_10_percent = sum(
      p_value < 0.10,
      na.rm = TRUE
    ),
    
    number_significant_5_percent = sum(
      p_value < 0.05,
      na.rm = TRUE
    ),
    
    number_significant_1_percent = sum(
      p_value < 0.01,
      na.rm = TRUE
    ),
    
    number_models = .N
  ),
  by = outcome
]


leave_one_out_summary <- merge(
  leave_one_out_summary,
  baseline_values,
  by = "outcome",
  all.x = TRUE
)


leave_one_out_summary[
  ,
  maximum_absolute_deviation := pmax(
    abs(minimum_effect - baseline_effect),
    abs(maximum_effect - baseline_effect)
  )
]


fwrite(
  leave_one_out_summary,
  file.path(
    output_dir,
    "leave_one_province_out_summary.csv"
  )
)


# ------------------------------------------------------------
# 20. Leave-one-province-out figures
# ------------------------------------------------------------

plot_data <- merge(
  leave_one_out_results,
  baseline_values,
  by = "outcome",
  all.x = TRUE
)


for (current_outcome in unique(plot_data$outcome)) {
  
  current_data <- copy(
    plot_data[outcome == current_outcome]
  )
  
  
  setorder(
    current_data,
    effect_pp_for_10_percent_increase
  )
  
  
  current_data[
    ,
    excluded_province := factor(
      excluded_province,
      levels = excluded_province
    )
  ]
  
  
  baseline_line <- unique(
    current_data$baseline_effect
  )
  
  
  figure <- ggplot(
    current_data,
    aes(
      x = excluded_province,
      y = effect_pp_for_10_percent_increase
    )
  ) +
    geom_hline(
      yintercept = baseline_line,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_errorbar(
      aes(
        ymin = lower_95_pp,
        ymax = upper_95_pp
      ),
      width = 0
    ) +
    geom_point(size = 1.8) +
    coord_flip() +
    labs(
      title = paste0(
        "Leave-one-province-out estimates: ",
        current_outcome
      ),
      subtitle =
        "Dashed line indicates the full-sample estimate",
      x = "Excluded historical province",
      y =
        "Percentage-point effect of a 10% increase in exposure"
    ) +
    theme_minimal(
      base_size = 11
    )
  
  
  safe_outcome_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(current_outcome)
  )
  
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "leave_one_out_",
        safe_outcome_name,
        ".png"
      )
    ),
    plot = figure,
    width = 8,
    height = 7,
    dpi = 300
  )
}


# ------------------------------------------------------------
# 21. Print final outputs
# ------------------------------------------------------------

cat(
  "\n================ MAIN ROBUSTNESS TABLE ================\n"
)

print(main_table_wide)


cat(
  "\n================ ALTERNATIVE CLUSTERING ================\n"
)

print(clustering_results)


cat(
  "\n================ SIGN-STABILITY SUMMARY ================\n"
)

print(sign_stability)


cat(
  "\n================ LEAVE-ONE-OUT SUMMARY =================\n"
)

print(leave_one_out_summary)


cat(
  "\n================ FAILED ROBUSTNESS MODELS ===============\n"
)

print(failed_results)


cat(
  "\n================ FAILED LEAVE-ONE-OUT MODELS ============\n"
)

print(leave_one_out_failures)


cat(
  "\nAll outputs were saved to:\n",
  output_dir,
  "\n"
)