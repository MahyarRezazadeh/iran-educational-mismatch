# ============================================================
# HETEROGENEOUS EFFECTS BY INITIAL LABOR-MARKET ABSORPTION
# ============================================================
#
# Initial absorptive capacity:
#   Province-level survey-weighted high-skill employment share
#   calculated using the earliest three LFS survey years.
#
# High-skill occupations:
#   ISCO major groups 1–3:
#     1. Managers
#     2. Professionals
#     3. Technicians and associate professionals
#
# Province groups:
#   - Low initial absorption
#   - Medium initial absorption
#   - High initial absorption
#
# Estimation sample:
#   Survey years after the three baseline years used to
#   construct initial absorptive capacity.
#
# Outcomes:
#   - Horizontal mismatch
#   - Double mismatch
#   - Fully matched employment
#   - Labor-force participation
#   - Employment
#   - Unemployment among labor-force participants
#
# Main specification:
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
  "heterogeneity_by_initial_absorption"
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

sample_var <- first_available(
  c(
    "sample_main_valid_cohort",
    "sample_main",
    "baseline_exposure_sample"
  ),
  available_vars
)


# Employment-quantity outcomes
lfp_var <- first_available(
  c(
    "labor_force_participation",
    "labor_force_participant",
    "in_labor_force"
  ),
  available_vars
)

employment_var <- first_available(
  c(
    "employed",
    "employment"
  ),
  available_vars
)

unemployment_var <- first_available(
  c(
    "unemployed",
    "unemployment"
  ),
  available_vars
)


# Use the harmonized two-digit ISCO occupation code
occupation_var <- first_available(
  c(
    "isco_2digit",
    "occupation_code_standardized",
    "main_job_title_isco_code"
  ),
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


# ------------------------------------------------------------
# 3. Validate required variables
# ------------------------------------------------------------

required_core_vars <- c(
  treatment_var,
  province_var,
  cohort_var,
  survey_year_var,
  weight_var,
  gender_var,
  urban_var,
  lfp_var,
  employment_var,
  unemployment_var,
  occupation_var
)

if (anyNA(required_core_vars)) {
  
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
    "Unemployment: ", unemployment_var, "\n",
    "Occupation: ", occupation_var
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
    paste(
      missing_mismatch_vars,
      collapse = ", "
    )
  )
}


cat("\nSelected variables:\n")
cat("Treatment:", treatment_var, "\n")
cat("Province:", province_var, "\n")
cat("Cohort:", cohort_var, "\n")
cat("Survey year:", survey_year_var, "\n")
cat("Weight:", weight_var, "\n")
cat("Gender:", gender_var, "\n")
cat("Urban:", urban_var, "\n")
cat("Sample flag:", sample_var, "\n")
cat("LFP:", lfp_var, "\n")
cat("Employment:", employment_var, "\n")
cat("Unemployment:", unemployment_var, "\n")
cat("Occupation:", occupation_var, "\n")


# ------------------------------------------------------------
# 4. Read required columns
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
      sample_var,
      lfp_var,
      employment_var,
      unemployment_var,
      occupation_var,
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
  "\nRows read:",
  format(
    nrow(dt),
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 5. Rename variables
# ------------------------------------------------------------

setnames(
  dt,
  treatment_var,
  "log_exposure"
)

setnames(
  dt,
  province_var,
  "province"
)

setnames(
  dt,
  cohort_var,
  "birth_cohort"
)

setnames(
  dt,
  survey_year_var,
  "survey_year"
)

setnames(
  dt,
  weight_var,
  "survey_weight"
)

setnames(
  dt,
  gender_var,
  "gender_original"
)

setnames(
  dt,
  urban_var,
  "urban_original"
)

setnames(
  dt,
  lfp_var,
  "lfp"
)

setnames(
  dt,
  employment_var,
  "employment"
)

setnames(
  dt,
  unemployment_var,
  "unemployment"
)

setnames(
  dt,
  occupation_var,
  "occupation_original"
)

if (!is.na(sample_var)) {
  
  setnames(
    dt,
    sample_var,
    "main_sample_flag"
  )
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
# 6. Construct controls and general sample
# ------------------------------------------------------------

# Verified project coding:
# gender == 1 means women
# gender == 2 means men
dt[
  ,
  female :=
    as.integer(
      gender_original == 1
    )
]


# Standardize urban indicator
if (
  is.numeric(dt$urban_original) ||
  is.integer(dt$urban_original) ||
  is.logical(dt$urban_original)
) {
  
  dt[
    ,
    urban :=
      as.integer(
        urban_original == 1
      )
  ]
  
} else {
  
  dt[
    ,
    urban_text :=
      tolower(
        trimws(
          as.character(
            urban_original
          )
        )
      )
  ]
  
  dt[
    ,
    urban := fifelse(
      urban_text %in%
        c(
          "urban",
          "1"
        ),
      1L,
      fifelse(
        urban_text %in%
          c(
            "rural",
            "0"
          ),
        0L,
        NA_integer_
      )
    )
  ]
}


# Existing main-sample flag, when available
if ("main_sample_flag" %in% names(dt)) {
  
  dt[
    ,
    base_sample :=
      main_sample_flag == 1
  ]
  
} else {
  
  dt[
    ,
    base_sample := TRUE
  ]
}


# General validity restrictions
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


# Outcome eligibility
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
# 7. Construct high-skill occupation indicator
# ------------------------------------------------------------

# isco_2digit may be stored as numeric or character.
# High-skill occupations are ISCO major groups 1, 2, and 3.

dt[
  ,
  occupation_text :=
    trimws(
      as.character(
        occupation_original
      )
    )
]


# Remove non-numeric characters
dt[
  ,
  occupation_digits :=
    gsub(
      "[^0-9]",
      "",
      occupation_text
    )
]


# Extract the first digit of the two-digit ISCO code
dt[
  ,
  isco_major :=
    suppressWarnings(
      as.integer(
        substr(
          occupation_digits,
          1,
          1
        )
      )
    )
]


# High skill: ISCO major groups 1–3
# Lower skill: ISCO major groups 4–9
dt[
  ,
  high_skill :=
    fifelse(
      isco_major %in% 1:3,
      1L,
      fifelse(
        isco_major %in% 4:9,
        0L,
        NA_integer_
      )
    )
]


cat(
  "\nISCO major-group coding check:\n"
)

print(
  dt[
    !is.na(isco_major),
    .N,
    by = isco_major
  ][order(isco_major)]
)


cat(
  "\nHigh-skill indicator among employed workers:\n"
)

print(
  dt[
    employment == 1 &
      !is.na(high_skill),
    .N,
    by = high_skill
  ]
)


# Save coding diagnostic
occupation_coding_summary <- dt[
  employment == 1,
  .(
    observations = .N,
    valid_high_skill =
      sum(
        !is.na(high_skill)
      ),
    missing_high_skill =
      sum(
        is.na(high_skill)
      ),
    high_skill_share_unweighted =
      mean(
        high_skill,
        na.rm = TRUE
      ),
    high_skill_share_weighted =
      weighted.mean(
        high_skill,
        survey_weight,
        na.rm = TRUE
      )
  )
]

fwrite(
  occupation_coding_summary,
  file.path(
    output_dir,
    "occupation_high_skill_coding_summary.csv"
  )
)


# ------------------------------------------------------------
# 8. Select common baseline survey years
# ------------------------------------------------------------

available_survey_years <- sort(
  unique(
    dt[
      base_sample == TRUE &
        !is.na(survey_year),
      survey_year
    ]
  )
)

if (length(available_survey_years) < 4L) {
  
  stop(
    "At least four survey years are required."
  )
}


# Earliest three LFS years define initial absorption
baseline_absorption_years <-
  available_survey_years[1:3]


# Remaining years are used in the outcome regressions
post_baseline_years <-
  available_survey_years[
    !available_survey_years %in%
      baseline_absorption_years
  ]


cat(
  "\nBaseline absorption years:",
  paste(
    baseline_absorption_years,
    collapse = ", "
  ),
  "\n"
)

cat(
  "Outcome estimation years:",
  paste(
    post_baseline_years,
    collapse = ", "
  ),
  "\n"
)


# ------------------------------------------------------------
# 9. Calculate initial high-skill employment share
# ------------------------------------------------------------

initial_absorption <- dt[
  base_sample == TRUE &
    survey_year %in%
    baseline_absorption_years &
    employment == 1 &
    !is.na(high_skill),
  .(
    initial_high_skill_share =
      weighted.mean(
        high_skill,
        survey_weight,
        na.rm = TRUE
      ),
    
    baseline_employed_observations =
      .N,
    
    baseline_weighted_employment =
      sum(
        survey_weight,
        na.rm = TRUE
      ),
    
    baseline_years_observed =
      uniqueN(
        survey_year
      )
  ),
  by = province
]


# Require all three baseline years
incomplete_baseline_provinces <-
  initial_absorption[
    baseline_years_observed < 3
  ]


if (nrow(incomplete_baseline_provinces) > 0L) {
  
  fwrite(
    incomplete_baseline_provinces,
    file.path(
      output_dir,
      "provinces_with_incomplete_absorption_baseline.csv"
    )
  )
  
  warning(
    "Some provinces do not appear in all three baseline years. ",
    "They will be excluded from the preferred analysis."
  )
}


initial_absorption_complete <-
  initial_absorption[
    baseline_years_observed == 3
  ]


cat(
  "\nProvinces with complete baseline:",
  nrow(initial_absorption_complete),
  "\n"
)


if (nrow(initial_absorption_complete) < 9L) {
  
  stop(
    "Too few provinces have complete baseline information."
  )
}


# ------------------------------------------------------------
# 10. Form low, medium, and high absorption groups
# ------------------------------------------------------------

initial_absorption_complete[
  ,
  absorption_rank :=
    frank(
      initial_high_skill_share,
      ties.method = "average"
    )
]


initial_absorption_complete[
  ,
  absorption_group_number :=
    pmin(
      3L,
      ceiling(
        3 *
          absorption_rank /
          .N
      )
    )
]


initial_absorption_complete[
  ,
  initial_absorption_group :=
    factor(
      absorption_group_number,
      levels = 1:3,
      labels = c(
        "Low initial absorption",
        "Medium initial absorption",
        "High initial absorption"
      )
    )
]


setorder(
  initial_absorption_complete,
  initial_high_skill_share
)


# Save province assignments
fwrite(
  initial_absorption_complete,
  file.path(
    output_dir,
    "province_initial_absorption_groups.csv"
  )
)


cat(
  "\nProvince absorption groups:\n"
)

print(
  initial_absorption_complete[
    ,
    .(
      province,
      initial_high_skill_share,
      baseline_employed_observations,
      baseline_years_observed,
      initial_absorption_group
    )
  ]
)


# Group-level summary
absorption_group_summary <-
  initial_absorption_complete[
    ,
    .(
      provinces = .N,
      
      minimum_initial_high_skill_share =
        min(
          initial_high_skill_share
        ),
      
      mean_initial_high_skill_share =
        mean(
          initial_high_skill_share
        ),
      
      maximum_initial_high_skill_share =
        max(
          initial_high_skill_share
        ),
      
      total_baseline_employed_observations =
        sum(
          baseline_employed_observations
        )
    ),
    by = initial_absorption_group
  ]


fwrite(
  absorption_group_summary,
  file.path(
    output_dir,
    "initial_absorption_group_summary.csv"
  )
)


# ------------------------------------------------------------
# 11. Merge absorption groups back to individual data
# ------------------------------------------------------------

dt <- merge(
  dt,
  initial_absorption_complete[
    ,
    .(
      province,
      initial_high_skill_share,
      initial_absorption_group
    )
  ],
  by = "province",
  all.x = TRUE,
  sort = FALSE
)


# Preferred estimation sample:
# exclude the three years used to define initial absorption
dt[
  ,
  post_baseline_sample :=
    base_sample == TRUE &
    survey_year %in%
    post_baseline_years &
    !is.na(
      initial_absorption_group
    )
]


cat(
  "\nPost-baseline estimation observations:",
  format(
    dt[
      post_baseline_sample == TRUE,
      .N
    ],
    big.mark = ","
  ),
  "\n"
)


# ------------------------------------------------------------
# 12. Create group-specific exposure slopes
# ------------------------------------------------------------

dt[
  ,
  exposure_low_absorption :=
    log_exposure *
    as.integer(
      initial_absorption_group ==
        "Low initial absorption"
    )
]


dt[
  ,
  exposure_medium_absorption :=
    log_exposure *
    as.integer(
      initial_absorption_group ==
        "Medium initial absorption"
    )
]


dt[
  ,
  exposure_high_absorption :=
    log_exposure *
    as.integer(
      initial_absorption_group ==
        "High initial absorption"
    )
]


# ------------------------------------------------------------
# 13. Define outcomes and samples
# ------------------------------------------------------------

outcome_info <- list(
  
  horizontal_mismatch = list(
    label = "Horizontal mismatch",
    sample_condition = function(x) {
      x$eligible_horizontal == TRUE &
        !is.na(
          x$horizontal_mismatch
        )
    }
  ),
  
  double_mismatch = list(
    label = "Double mismatch",
    sample_condition = function(x) {
      x$eligible_joint == TRUE &
        !is.na(
          x$double_mismatch
        )
    }
  ),
  
  fully_matched = list(
    label = "Fully matched employment",
    sample_condition = function(x) {
      x$eligible_joint == TRUE &
        !is.na(
          x$fully_matched
        )
    }
  ),
  
  lfp = list(
    label = "Labor-force participation",
    sample_condition = function(x) {
      !is.na(
        x$lfp
      )
    }
  ),
  
  employment = list(
    label = "Employment",
    sample_condition = function(x) {
      !is.na(
        x$employment
      )
    }
  ),
  
  unemployment = list(
    label = "Unemployment",
    sample_condition = function(x) {
      !is.na(
        x$unemployment
      ) &
        x$lfp == 1
    }
  )
)


# ------------------------------------------------------------
# 14. Estimate heterogeneous-slope models
# ------------------------------------------------------------

models <- list()
failed_models <- list()

failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  sample_condition <-
    outcome_info[[outcome_name]]$
    sample_condition(dt)
  
  
  estimation_sample <- dt[
    post_baseline_sample == TRUE &
      sample_condition
  ]
  
  
  model_formula <- as.formula(
    paste0(
      outcome_name,
      " ~ exposure_low_absorption",
      " + exposure_medium_absorption",
      " + exposure_high_absorption",
      " + female + urban",
      " | province + birth_cohort + survey_year"
    )
  )
  
  
  cat(
    "\nEstimating:",
    outcome_label,
    "| N =",
    format(
      nrow(
        estimation_sample
      ),
      big.mark = ","
    ),
    "\n"
  )
  
  
  model_attempt <- tryCatch(
    
    feols(
      fml = model_formula,
      data = estimation_sample,
      weights = ~ survey_weight,
      cluster =
        ~ province + birth_cohort,
      warn = FALSE,
      notes = FALSE
    ),
    
    error = function(e) e
  )
  
  
  if (
    inherits(
      model_attempt,
      "error"
    )
  ) {
    
    failure_counter <-
      failure_counter + 1L
    
    failed_models[[failure_counter]] <-
      data.table(
        outcome = outcome_label,
        error_message =
          conditionMessage(
            model_attempt
          )
      )
    
    next
  }
  
  
  models[[outcome_name]] <-
    model_attempt
}


if (length(models) == 0L) {
  
  stop(
    "All initial-absorption models failed."
  )
}


# ------------------------------------------------------------
# 15. Extract group-specific effects
# ------------------------------------------------------------

# Convert the coefficient on log exposure into the
# percentage-point effect of a 10 percent increase in exposure
effect_scale <- 100 * log(1.10)


slope_terms <- c(
  exposure_low_absorption =
    "Low initial absorption",
  
  exposure_medium_absorption =
    "Medium initial absorption",
  
  exposure_high_absorption =
    "High initial absorption"
)


extract_group_effects <- function(
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
        
        
        estimate <- unname(
          beta[term_name]
        )
        
        standard_error <- unname(
          se_beta[term_name]
        )
        
        p_value_current <- unname(
          p_beta[term_name]
        )
        
        
        data.table(
          outcome_id = outcome_id,
          
          outcome = outcome_label,
          
          initial_absorption_group =
            unname(
              slope_terms[term_name]
            ),
          
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
          
          p_value =
            p_value_current,
          
          observations =
            nobs(model)
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(results)
}


group_effect_results <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      extract_group_effects(
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


cat(
  "\nExtracted group-specific effects:\n"
)

print(group_effect_results)


# ------------------------------------------------------------
# 16. Formal tests of slope equality
# ------------------------------------------------------------

pairwise_effect_test <- function(
    model,
    term_a,
    term_b,
    label_a,
    label_b
) {
  
  beta <- coef(model)
  variance_matrix <- vcov(model)
  
  
  if (
    !term_a %in% names(beta) ||
    !term_b %in% names(beta)
  ) {
    
    return(
      data.table(
        test = paste0(
          label_a,
          " effect equals ",
          label_b,
          " effect"
        ),
        difference_pp = NA_real_,
        standard_error_pp = NA_real_,
        statistic = NA_real_,
        degrees_freedom = NA_integer_,
        p_value = NA_real_
      )
    )
  }
  
  
  difference <-
    beta[term_a] -
    beta[term_b]
  
  
  variance_difference <-
    variance_matrix[term_a, term_a] +
    variance_matrix[term_b, term_b] -
    2 * variance_matrix[term_a, term_b]
  
  
  standard_error <-
    sqrt(
      variance_difference
    )
  
  
  statistic <-
    difference /
    standard_error
  
  
  p_value_current <-
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
    
    degrees_freedom =
      NA_integer_,
    
    p_value =
      unname(p_value_current)
  )
}


joint_equality_test <- function(model) {
  
  relevant_terms <- c(
    "exposure_low_absorption",
    "exposure_medium_absorption",
    "exposure_high_absorption"
  )
  
  
  beta_all <- coef(model)
  
  if (!all(relevant_terms %in% names(beta_all))) {
    
    return(
      data.table(
        test =
          "All three absorption-group effects are equal",
        difference_pp = NA_real_,
        standard_error_pp = NA_real_,
        statistic = NA_real_,
        degrees_freedom = 2L,
        p_value = NA_real_
      )
    )
  }
  
  
  beta_relevant <-
    beta_all[relevant_terms]
  
  
  variance_matrix <-
    vcov(model)[
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
    variance_matrix %*%
    t(restriction_matrix)
  
  
  inverse_restriction_variance <-
    tryCatch(
      solve(restriction_variance),
      error = function(e) {
        qr.solve(restriction_variance)
      }
    )
  
  
  wald_statistic <- as.numeric(
    t(restriction_values) %*%
      inverse_restriction_variance %*%
      restriction_values
  )
  
  
  p_value_current <- pchisq(
    wald_statistic,
    df = 2,
    lower.tail = FALSE
  )
  
  
  data.table(
    test =
      "All three absorption-group effects are equal",
    
    difference_pp =
      NA_real_,
    
    standard_error_pp =
      NA_real_,
    
    statistic =
      wald_statistic,
    
    degrees_freedom =
      2L,
    
    p_value =
      p_value_current
  )
}


test_results <- list()
test_counter <- 0L


for (outcome_name in names(models)) {
  
  model <- models[[outcome_name]]
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  
  current_tests <- list(
    
    pairwise_effect_test(
      model = model,
      term_a =
        "exposure_low_absorption",
      term_b =
        "exposure_medium_absorption",
      label_a =
        "Low-absorption",
      label_b =
        "medium-absorption"
    ),
    
    pairwise_effect_test(
      model = model,
      term_a =
        "exposure_low_absorption",
      term_b =
        "exposure_high_absorption",
      label_a =
        "Low-absorption",
      label_b =
        "high-absorption"
    ),
    
    pairwise_effect_test(
      model = model,
      term_a =
        "exposure_medium_absorption",
      term_b =
        "exposure_high_absorption",
      label_a =
        "Medium-absorption",
      label_b =
        "high-absorption"
    ),
    
    joint_equality_test(
      model = model
    )
  )
  
  
  for (current_test in current_tests) {
    
    test_counter <-
      test_counter + 1L
    
    
    current_test[
      ,
      `:=`(
        outcome_id =
          outcome_name,
        
        outcome =
          outcome_label
      )
    ]
    
    
    test_results[[test_counter]] <-
      current_test
  }
}


formal_tests <- rbindlist(
  test_results,
  use.names = TRUE,
  fill = TRUE
)


setcolorder(
  formal_tests,
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


cat(
  "\nFormal heterogeneity tests:\n"
)

print(formal_tests)


# ------------------------------------------------------------
# 17. Format compact results table
# ------------------------------------------------------------

group_effect_results[
  ,
  stars := fifelse(
    is.na(p_value),
    "",
    fifelse(
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
  )
]


group_effect_results[
  ,
  formatted_result :=
    sprintf(
      "%.3f%s (%.3f)",
      effect_pp_for_10_percent_increase,
      stars,
      standard_error_pp
    )
]


absorption_group_order <- c(
  "Low initial absorption",
  "Medium initial absorption",
  "High initial absorption"
)


group_effect_results[
  ,
  initial_absorption_group :=
    factor(
      initial_absorption_group,
      levels =
        absorption_group_order
    )
]


setorder(
  group_effect_results,
  outcome_id,
  initial_absorption_group
)


compact_table <- dcast(
  group_effect_results,
  initial_absorption_group ~ outcome,
  value.var =
    "formatted_result"
)


cat(
  "\nCompact results table:\n"
)

print(compact_table)


# ------------------------------------------------------------
# 18. Save outputs
# ------------------------------------------------------------

fwrite(
  group_effect_results,
  file.path(
    output_dir,
    "initial_absorption_detailed_results.csv"
  )
)


fwrite(
  compact_table,
  file.path(
    output_dir,
    "initial_absorption_table_wide.csv"
  )
)


fwrite(
  formal_tests,
  file.path(
    output_dir,
    "initial_absorption_formal_tests.csv"
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
    outcome =
      character(),
    
    error_message =
      character()
  )
}


fwrite(
  failed_table,
  file.path(
    output_dir,
    "initial_absorption_failed_models.csv"
  )
)


# ------------------------------------------------------------
# 19. Create figures
# ------------------------------------------------------------

for (
  current_outcome in
  unique(group_effect_results$outcome)
) {
  
  current_data <- copy(
    group_effect_results[
      outcome == current_outcome
    ]
  )
  
  
  figure <- ggplot(
    current_data,
    aes(
      x =
        initial_absorption_group,
      
      y =
        effect_pp_for_10_percent_increase
    )
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_errorbar(
      aes(
        ymin =
          lower_95_pp,
        
        ymax =
          upper_95_pp
      ),
      width = 0.1
    ) +
    geom_point(
      size = 2.3
    ) +
    labs(
      title = paste0(
        "University expansion and ",
        tolower(current_outcome)
      ),
      
      subtitle =
        "Effects by initial provincial high-skill employment share",
      
      x =
        "Initial labor-market absorption group",
      
      y =
        "Percentage-point effect of a 10% increase in exposure"
    ) +
    theme_minimal(
      base_size = 11
    ) +
    theme(
      axis.text.x =
        element_text(
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
        "initial_absorption_",
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
# 20. Print final results
# ------------------------------------------------------------

cat(
  "\n================ ABSORPTION GROUPS =====================\n"
)

print(
  initial_absorption_complete[
    ,
    .(
      province,
      initial_high_skill_share,
      initial_absorption_group
    )
  ]
)


cat(
  "\n================ GROUP-SPECIFIC EFFECTS ================\n"
)

print(compact_table)


cat(
  "\n================ FORMAL TESTS ===========================\n"
)

print(formal_tests)


cat(
  "\n================ FAILED MODELS ==========================\n"
)

print(failed_table)


cat(
  "\nOutputs saved to:\n",
  output_dir,
  "\n"
)