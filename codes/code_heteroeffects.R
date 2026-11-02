# ============================================================
# HETEROGENEOUS EFFECTS BY INITIAL UNIVERSITY CAPACITY
# ============================================================
#
# Province groups:
#   1. Low initial university capacity
#   2. Medium initial university capacity
#   3. High initial university capacity
#
# Initial capacity is measured using the average log exposure
# of the earliest five birth cohorts observed in the analysis.
#
# Outcomes:
#   - Horizontal mismatch
#   - Double mismatch
#   - Fully matched employment
#   - Labor-force participation
#   - Employment
#   - Unemployment among labor-force participants
#
# Specification:
#   - Group-specific slopes on log university exposure
#   - Historical-province fixed effects
#   - Birth-cohort fixed effects
#   - Survey-year fixed effects
#   - Female and urban controls
#   - Survey weights
#   - Two-way clustered SEs by province and birth cohort
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
  "heterogeneity_by_initial_capacity"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}


# ------------------------------------------------------------
# 2. Identify available variables
# ------------------------------------------------------------

metadata <- fst::metadata_fst(input_file)
available_vars <- metadata$columnNames

first_available <- function(candidates, available) {
  
  found <- candidates[candidates %in% available]
  
  if (length(found) == 0L) {
    return(NA_character_)
  }
  
  found[1L]
}


treatment_var <- first_available(
  c("log_undergrad_exposure_18_20"),
  available_vars
)

raw_exposure_var <- first_available(
  c("undergrad_exposure_18_20"),
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

sample_var <- first_available(
  c(
    "sample_main_valid_cohort",
    "sample_main",
    "baseline_exposure_sample"
  ),
  available_vars
)

lfp_var <- first_available(
  c(
    "labor_force_participation",
    "labor_force_participant",
    "in_labor_force"
  ),
  available_vars
)

employment_var <- first_available(
  c("employed", "employment"),
  available_vars
)

unemployment_var <- first_available(
  c("unemployed", "unemployment"),
  available_vars
)


# Exact mismatch outcomes
horizontal_var <-
  "horizontal_mismatch_detailed_broad"

double_var <-
  "overeducated_and_horizontal_mismatch_refined"

fully_matched_var <-
  "fully_matched_refined"

horizontal_eligibility_var <-
  "horizontal_mismatch_eligible"

joint_eligibility_var <-
  "both_refined_mismatch_available"


required_vars <- c(
  treatment_var,
  province_var,
  cohort_var,
  survey_year_var,
  weight_var,
  gender_var,
  urban_var,
  lfp_var,
  employment_var,
  unemployment_var
)

if (anyNA(required_vars)) {
  
  stop(
    "At least one required core variable was not found.\n",
    "Treatment: ", treatment_var, "\n",
    "Province: ", province_var, "\n",
    "Cohort: ", cohort_var, "\n",
    "Survey year: ", survey_year_var, "\n",
    "Weight: ", weight_var, "\n",
    "Gender: ", gender_var, "\n",
    "Urban: ", urban_var, "\n",
    "LFP: ", lfp_var, "\n",
    "Employment: ", employment_var, "\n",
    "Unemployment: ", unemployment_var
  )
}


missing_mismatch_vars <- setdiff(
  c(
    horizontal_var,
    double_var,
    fully_matched_var,
    horizontal_eligibility_var,
    joint_eligibility_var
  ),
  available_vars
)

if (length(missing_mismatch_vars) > 0L) {
  
  stop(
    "The following mismatch variables were not found: ",
    paste(missing_mismatch_vars, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 3. Read required columns
# ------------------------------------------------------------

columns_to_read <- unique(
  na.omit(
    c(
      treatment_var,
      raw_exposure_var,
      province_var,
      cohort_var,
      survey_year_var,
      weight_var,
      gender_var,
      urban_var,
      sample_var,
      lfp_var,
      employment_var,
      unemployment_var,
      horizontal_var,
      double_var,
      fully_matched_var,
      horizontal_eligibility_var,
      joint_eligibility_var
    )
  )
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
# 4. Rename variables
# ------------------------------------------------------------

setnames(dt, treatment_var, "log_exposure")
setnames(dt, province_var, "province")
setnames(dt, cohort_var, "birth_cohort")
setnames(dt, survey_year_var, "survey_year")
setnames(dt, weight_var, "survey_weight")
setnames(dt, gender_var, "gender_original")
setnames(dt, urban_var, "urban_original")

setnames(dt, lfp_var, "lfp")
setnames(dt, employment_var, "employment")
setnames(dt, unemployment_var, "unemployment")

if (!is.na(raw_exposure_var)) {
  setnames(dt, raw_exposure_var, "raw_exposure")
}

if (!is.na(sample_var)) {
  setnames(dt, sample_var, "main_sample_flag")
}

setnames(
  dt,
  c(
    horizontal_var,
    double_var,
    fully_matched_var,
    horizontal_eligibility_var,
    joint_eligibility_var
  ),
  c(
    "horizontal_mismatch",
    "double_mismatch",
    "fully_matched",
    "eligible_horizontal_original",
    "eligible_joint_original"
  )
)


# ------------------------------------------------------------
# 5. Construct controls and baseline sample
# ------------------------------------------------------------

# Project coding:
# gender == 1 means women
# gender == 2 means men
dt[
  ,
  female := as.integer(gender_original == 1)
]


# Standardize urban indicator
if (
  is.numeric(dt$urban_original) ||
  is.integer(dt$urban_original) ||
  is.logical(dt$urban_original)
) {
  
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


if ("main_sample_flag" %in% names(dt)) {
  
  dt[
    ,
    base_sample := main_sample_flag == 1
  ]
  
} else {
  
  dt[
    ,
    base_sample := TRUE
  ]
}


dt[
  ,
  base_sample :=
    base_sample &
    !is.na(log_exposure) &
    is.finite(log_exposure) &
    !is.na(province) &
    !is.na(birth_cohort) &
    !is.na(survey_year) &
    !is.na(female) &
    !is.na(urban) &
    !is.na(survey_weight) &
    is.finite(survey_weight) &
    survey_weight > 0
]


dt[
  ,
  eligible_horizontal :=
    eligible_horizontal_original == 1
]

dt[
  ,
  eligible_joint :=
    eligible_joint_original == 1
]


# ------------------------------------------------------------
# 6. Create one row per province-cohort exposure cell
# ------------------------------------------------------------

cell_validation <- dt[
  base_sample == TRUE,
  .(
    number_exposure_values = uniqueN(log_exposure),
    observations = .N
  ),
  by = .(
    province,
    birth_cohort
  )
]

problem_cells <- cell_validation[
  number_exposure_values != 1L
]

if (nrow(problem_cells) > 0L) {
  
  fwrite(
    problem_cells,
    file.path(
      output_dir,
      "problematic_province_cohort_cells.csv"
    )
  )
  
  stop(
    "Exposure is not constant within some province-cohort cells."
  )
}


exposure_cells <- unique(
  dt[
    base_sample == TRUE,
    .(
      province,
      birth_cohort,
      log_exposure
    )
  ]
)


# ------------------------------------------------------------
# 7. Define initial university capacity
# ------------------------------------------------------------

# Use the earliest five observed birth cohorts.
# This avoids hard-coding Persian-year values and works even if
# the cohort variable is stored differently.

available_cohorts <- sort(
  unique(exposure_cells$birth_cohort)
)

if (length(available_cohorts) < 5L) {
  stop("Fewer than five birth cohorts are available.")
}

initial_cohorts <- available_cohorts[1:5]

cat(
  "\nInitial-capacity cohorts:",
  paste(initial_cohorts, collapse = ", "),
  "\n"
)


initial_capacity <- exposure_cells[
  birth_cohort %in% initial_cohorts,
  .(
    initial_log_capacity =
      mean(log_exposure, na.rm = TRUE),
    
    initial_capacity_sd =
      sd(log_exposure, na.rm = TRUE),
    
    initial_cohorts_observed =
      uniqueN(birth_cohort)
  ),
  by = province
]


if (nrow(initial_capacity) !=
    uniqueN(exposure_cells$province)) {
  
  warning(
    "Some provinces do not have complete information ",
    "for the earliest five cohorts."
  )
}


# ------------------------------------------------------------
# 8. Divide provinces into three initial-capacity groups
# ------------------------------------------------------------

initial_capacity[
  ,
  capacity_rank :=
    frank(
      initial_log_capacity,
      ties.method = "average"
    )
]


initial_capacity[
  ,
  capacity_group_number :=
    pmin(
      3L,
      ceiling(
        3 * capacity_rank / .N
      )
    )
]


initial_capacity[
  ,
  initial_capacity_group :=
    factor(
      capacity_group_number,
      levels = 1:3,
      labels = c(
        "Low initial capacity",
        "Medium initial capacity",
        "High initial capacity"
      )
    )
]


# Save province assignments
setorder(
  initial_capacity,
  initial_log_capacity
)

fwrite(
  initial_capacity,
  file.path(
    output_dir,
    "province_initial_capacity_groups.csv"
  )
)


cat("\nProvince initial-capacity groups:\n")
print(
  initial_capacity[
    ,
    .(
      province,
      initial_log_capacity,
      initial_capacity_group
    )
  ]
)


# Group summary
capacity_group_summary <- initial_capacity[
  ,
  .(
    provinces = .N,
    minimum_initial_log_capacity =
      min(initial_log_capacity),
    mean_initial_log_capacity =
      mean(initial_log_capacity),
    maximum_initial_log_capacity =
      max(initial_log_capacity)
  ),
  by = initial_capacity_group
]

fwrite(
  capacity_group_summary,
  file.path(
    output_dir,
    "initial_capacity_group_summary.csv"
  )
)

print(capacity_group_summary)


# ------------------------------------------------------------
# 9. Merge province groups back to individual data
# ------------------------------------------------------------

dt <- merge(
  dt,
  initial_capacity[
    ,
    .(
      province,
      initial_log_capacity,
      initial_capacity_group
    )
  ],
  by = "province",
  all.x = TRUE,
  sort = FALSE
)


if (
  any(
    dt[
      base_sample == TRUE,
      is.na(initial_capacity_group)
    ]
  )
) {
  
  stop(
    "Some baseline observations were not assigned ",
    "to an initial-capacity group."
  )
}


# ------------------------------------------------------------
# 10. Create group-specific exposure slopes
# ------------------------------------------------------------

dt[
  ,
  exposure_low :=
    log_exposure *
    as.integer(
      initial_capacity_group ==
        "Low initial capacity"
    )
]

dt[
  ,
  exposure_medium :=
    log_exposure *
    as.integer(
      initial_capacity_group ==
        "Medium initial capacity"
    )
]

dt[
  ,
  exposure_high :=
    log_exposure *
    as.integer(
      initial_capacity_group ==
        "High initial capacity"
    )
]


# ------------------------------------------------------------
# 11. Define outcomes and samples
# ------------------------------------------------------------

outcome_info <- list(
  
  horizontal_mismatch = list(
    label = "Horizontal mismatch",
    sample_condition = function(x) {
      x$eligible_horizontal == TRUE &
        !is.na(x$horizontal_mismatch)
    }
  ),
  
  double_mismatch = list(
    label = "Double mismatch",
    sample_condition = function(x) {
      x$eligible_joint == TRUE &
        !is.na(x$double_mismatch)
    }
  ),
  
  fully_matched = list(
    label = "Fully matched employment",
    sample_condition = function(x) {
      x$eligible_joint == TRUE &
        !is.na(x$fully_matched)
    }
  ),
  
  lfp = list(
    label = "Labor-force participation",
    sample_condition = function(x) {
      !is.na(x$lfp)
    }
  ),
  
  employment = list(
    label = "Employment",
    sample_condition = function(x) {
      !is.na(x$employment)
    }
  ),
  
  unemployment = list(
    label = "Unemployment",
    sample_condition = function(x) {
      !is.na(x$unemployment) &
        x$lfp == 1
    }
  )
)


# ------------------------------------------------------------
# 12. Estimate group-specific slope models
# ------------------------------------------------------------

models <- list()
failed_models <- list()

failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  outcome_condition <-
    outcome_info[[outcome_name]]$
    sample_condition(dt)
  
  
  estimation_sample <- dt[
    base_sample == TRUE &
      outcome_condition &
      !is.na(initial_capacity_group)
  ]
  
  
  model_formula <- as.formula(
    paste0(
      outcome_name,
      " ~ exposure_low + exposure_medium + exposure_high",
      " + female + urban",
      " | province + birth_cohort + survey_year"
    )
  )
  
  
  cat(
    "\nEstimating:",
    outcome_label,
    "| N =",
    format(
      nrow(estimation_sample),
      big.mark = ","
    ),
    "\n"
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
    
    failure_counter <- failure_counter + 1L
    
    failed_models[[failure_counter]] <- data.table(
      outcome = outcome_label,
      error_message =
        conditionMessage(model_attempt)
    )
    
    next
  }
  
  
  models[[outcome_name]] <- model_attempt
}


if (length(models) == 0L) {
  stop("All initial-capacity heterogeneity models failed.")
}


# ------------------------------------------------------------
# 13. Extract group-specific effects
# ------------------------------------------------------------

effect_scale <- 100 * log(1.10)

slope_terms <- c(
  exposure_low = "Low initial capacity",
  exposure_medium = "Medium initial capacity",
  exposure_high = "High initial capacity"
)


extract_group_slopes <- function(
    model,
    outcome_id,
    outcome_label
) {
  
  beta <- coef(model)
  se_beta <- se(model)
  p_beta <- pvalue(model)
  
  
  results <- rbindlist(
    lapply(
      names(slope_terms),
      function(term_name) {
        
        if (!term_name %in% names(beta)) {
          return(NULL)
        }
        
        estimate <- unname(beta[term_name])
        standard_error <- unname(se_beta[term_name])
        p_value <- unname(p_beta[term_name])
        
        data.table(
          outcome_id = outcome_id,
          outcome = outcome_label,
          initial_capacity_group =
            unname(slope_terms[term_name]),
          term = term_name,
          
          coefficient_log_exposure =
            estimate,
          
          standard_error_log_exposure =
            standard_error,
          
          effect_pp_for_10_percent_increase =
            effect_scale * estimate,
          
          standard_error_pp =
            effect_scale * standard_error,
          
          lower_95_pp =
            effect_scale *
            (
              estimate -
                1.96 * standard_error
            ),
          
          upper_95_pp =
            effect_scale *
            (
              estimate +
                1.96 * standard_error
            ),
          
          p_value = p_value,
          observations = nobs(model)
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  results
}


group_slope_results <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      extract_group_slopes(
        model = models[[outcome_name]],
        outcome_id = outcome_name,
        outcome_label =
          outcome_info[[outcome_name]]$label
      )
    }
  ),
  use.names = TRUE,
  fill = TRUE
)


# ------------------------------------------------------------
# 14. Pairwise and joint equality tests
# ------------------------------------------------------------

pairwise_slope_test <- function(
    model,
    term_a,
    term_b,
    label_a,
    label_b
) {
  
  beta <- coef(model)
  variance_matrix <- vcov(model)
  
  difference <-
    beta[term_a] -
    beta[term_b]
  
  variance_difference <-
    variance_matrix[term_a, term_a] +
    variance_matrix[term_b, term_b] -
    2 * variance_matrix[term_a, term_b]
  
  standard_error <-
    sqrt(variance_difference)
  
  statistic <-
    difference / standard_error
  
  p_value <-
    2 * pnorm(
      abs(statistic),
      lower.tail = FALSE
    )
  
  
  data.table(
    test = paste0(
      label_a,
      " effect equals ",
      label_b,
      " effect"
    ),
    
    difference_pp =
      effect_scale *
      unname(difference),
    
    standard_error_pp =
      effect_scale *
      unname(standard_error),
    
    statistic =
      unname(statistic),
    
    p_value =
      unname(p_value)
  )
}


joint_equality_test <- function(model) {
  
  beta <- coef(model)
  variance_matrix <- vcov(model)
  
  relevant_terms <- c(
    "exposure_low",
    "exposure_medium",
    "exposure_high"
  )
  
  beta_relevant <- beta[relevant_terms]
  vcov_relevant <-
    variance_matrix[
      relevant_terms,
      relevant_terms,
      drop = FALSE
    ]
  
  
  # Restrictions:
  # low - medium = 0
  # low - high = 0
  restriction_matrix <- matrix(
    c(
      1, -1,  0,
      1,  0, -1
    ),
    nrow = 2,
    byrow = TRUE
  )
  
  
  restriction_values <-
    restriction_matrix %*%
    beta_relevant
  
  restriction_variance <-
    restriction_matrix %*%
    vcov_relevant %*%
    t(restriction_matrix)
  
  
  wald_statistic <-
    as.numeric(
      t(restriction_values) %*%
        solve(restriction_variance) %*%
        restriction_values
    )
  
  degrees_freedom <- 2L
  
  p_value <-
    pchisq(
      wald_statistic,
      df = degrees_freedom,
      lower.tail = FALSE
    )
  
  
  data.table(
    test =
      "All three initial-capacity effects are equal",
    
    difference_pp = NA_real_,
    standard_error_pp = NA_real_,
    statistic = wald_statistic,
    degrees_freedom = degrees_freedom,
    p_value = p_value
  )
}


heterogeneity_tests <- list()
test_counter <- 0L


for (outcome_name in names(models)) {
  
  model <- models[[outcome_name]]
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  
  test_list <- list(
    
    pairwise_slope_test(
      model,
      "exposure_low",
      "exposure_medium",
      "Low initial-capacity",
      "medium initial-capacity"
    ),
    
    pairwise_slope_test(
      model,
      "exposure_low",
      "exposure_high",
      "Low initial-capacity",
      "high initial-capacity"
    ),
    
    pairwise_slope_test(
      model,
      "exposure_medium",
      "exposure_high",
      "Medium initial-capacity",
      "high initial-capacity"
    ),
    
    joint_equality_test(model)
  )
  
  
  for (test_result in test_list) {
    
    test_counter <- test_counter + 1L
    
    test_result[
      ,
      `:=`(
        outcome_id = outcome_name,
        outcome = outcome_label
      )
    ]
    
    heterogeneity_tests[[test_counter]] <-
      test_result
  }
}


heterogeneity_test_results <- rbindlist(
  heterogeneity_tests,
  use.names = TRUE,
  fill = TRUE
)

setcolorder(
  heterogeneity_test_results,
  c(
    "outcome_id",
    "outcome",
    "test",
    "difference_pp",
    "standard_error_pp",
    "statistic",
    "degrees_freedom",
    "p_value"
  )
)


# ------------------------------------------------------------
# 15. Format compact results table
# ------------------------------------------------------------

group_slope_results[
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


group_slope_results[
  ,
  formatted_result := sprintf(
    "%.3f%s (%.3f)",
    effect_pp_for_10_percent_increase,
    stars,
    standard_error_pp
  )
]


group_order <- c(
  "Low initial capacity",
  "Medium initial capacity",
  "High initial capacity"
)

group_slope_results[
  ,
  initial_capacity_group :=
    factor(
      initial_capacity_group,
      levels = group_order
    )
]

setorder(
  group_slope_results,
  outcome_id,
  initial_capacity_group
)


compact_table <- dcast(
  group_slope_results,
  initial_capacity_group ~ outcome,
  value.var = "formatted_result"
)


# ------------------------------------------------------------
# 16. Save outputs
# ------------------------------------------------------------

fwrite(
  group_slope_results,
  file.path(
    output_dir,
    "initial_capacity_group_detailed_results.csv"
  )
)

fwrite(
  compact_table,
  file.path(
    output_dir,
    "initial_capacity_group_table_wide.csv"
  )
)

fwrite(
  heterogeneity_test_results,
  file.path(
    output_dir,
    "initial_capacity_group_formal_tests.csv"
  )
)


if (length(failed_models) > 0L) {
  
  failed_table <- rbindlist(
    failed_models,
    use.names = TRUE,
    fill = TRUE
  )
  
} else {
  
  failed_table <- data.table(
    outcome = character(),
    error_message = character()
  )
}


fwrite(
  failed_table,
  file.path(
    output_dir,
    "initial_capacity_group_failed_models.csv"
  )
)


# ------------------------------------------------------------
# 17. Figures
# ------------------------------------------------------------

for (
  current_outcome in
  unique(group_slope_results$outcome)
) {
  
  current_data <- copy(
    group_slope_results[
      outcome == current_outcome
    ]
  )
  
  
  figure <- ggplot(
    current_data,
    aes(
      x = initial_capacity_group,
      y = effect_pp_for_10_percent_increase
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_errorbar(
      aes(
        ymin = lower_95_pp,
        ymax = upper_95_pp
      ),
      width = 0.1
    ) +
    geom_point(size = 2.3) +
    labs(
      title = paste0(
        "University expansion and ",
        tolower(current_outcome)
      ),
      subtitle =
        "Effects by provinces' initial university capacity",
      x = "Initial university-capacity group",
      y =
        "Percentage-point effect of a 10% increase in exposure"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(
        angle = 20,
        hjust = 1
      )
    )
  
  
  safe_name <- gsub(
    "[^A-Za-z0-9]+",
    "_",
    tolower(current_outcome)
  )
  
  
  ggsave(
    filename = file.path(
      output_dir,
      paste0(
        "initial_capacity_",
        safe_name,
        ".png"
      )
    ),
    plot = figure,
    width = 7.5,
    height = 5.5,
    dpi = 300,
    bg = "white"
  )
}


# ------------------------------------------------------------
# 18. Print results
# ------------------------------------------------------------

cat(
  "\n================ GROUP-SPECIFIC EFFECTS ================\n"
)

print(compact_table)


cat(
  "\n================ HETEROGENEITY TESTS ===================\n"
)

print(heterogeneity_test_results)


cat(
  "\n================ FAILED MODELS =========================\n"
)

print(failed_table)


cat(
  "\nOutputs saved to:\n",
  output_dir,
  "\n"
)