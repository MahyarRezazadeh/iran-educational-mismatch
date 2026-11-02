# ============================================================
# NONLINEAR EXPOSURE EFFECTS FOR MAIN MISMATCH OUTCOMES
# Five province-cohort exposure bins; Q3 is the reference
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
  "nonlinear_main_mismatch_outcomes"
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
# 2. Identify variables
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


# Exact principal mismatch outcomes
horizontal_var <-
  "horizontal_mismatch_detailed_broad"

double_var <-
  "overeducated_and_horizontal_mismatch_refined"

fully_matched_var <-
  "fully_matched_refined"


# Eligibility indicators
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
  horizontal_var,
  double_var,
  fully_matched_var,
  horizontal_eligibility_var,
  joint_eligibility_var
)

if (anyNA(required_vars)) {
  
  stop(
    "One or more required variables were not found.\n",
    "Treatment: ", treatment_var, "\n",
    "Province: ", province_var, "\n",
    "Cohort: ", cohort_var, "\n",
    "Survey year: ", survey_year_var, "\n",
    "Weight: ", weight_var, "\n",
    "Gender: ", gender_var, "\n",
    "Urban: ", urban_var
  )
}

missing_named_vars <- setdiff(
  c(
    horizontal_var,
    double_var,
    fully_matched_var,
    horizontal_eligibility_var,
    joint_eligibility_var
  ),
  available_vars
)

if (length(missing_named_vars) > 0L) {
  stop(
    "These mismatch variables were not found: ",
    paste(missing_named_vars, collapse = ", ")
  )
}


# ------------------------------------------------------------
# 3. Read required columns
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

if (!is.na(sample_var)) {
  setnames(dt, sample_var, "main_sample_flag")
}


# ------------------------------------------------------------
# 5. Controls and baseline sample
# ------------------------------------------------------------

# Project coding:
# 1 = women, 2 = men
dt[, female := as.integer(gender_original == 1)]


# Standardize urban indicator
if (
  is.numeric(dt$urban_original) ||
  is.integer(dt$urban_original) ||
  is.logical(dt$urban_original)
) {
  
  dt[, urban := as.integer(urban_original == 1)]
  
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
  
  dt[, base_sample := main_sample_flag == 1]
  
} else {
  
  dt[, base_sample := TRUE]
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
# 6. Validate province-cohort exposure
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


# ------------------------------------------------------------
# 7. Construct five province-cohort exposure bins
# ------------------------------------------------------------

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


exposure_cells[
  ,
  exposure_rank :=
    frank(
      log_exposure,
      ties.method = "average"
    )
]


exposure_cells[
  ,
  exposure_quintile_number :=
    pmin(
      5L,
      ceiling(
        5 * exposure_rank / .N
      )
    )
]


exposure_cells[
  ,
  exposure_quintile :=
    factor(
      exposure_quintile_number,
      levels = 1:5,
      labels = c(
        "Q1 Lowest",
        "Q2 Lower-middle",
        "Q3 Middle",
        "Q4 Upper-middle",
        "Q5 Highest"
      )
    )
]


exposure_cells[
  ,
  exposure_quintile :=
    relevel(
      exposure_quintile,
      ref = "Q3 Middle"
    )
]


dt <- merge(
  dt,
  exposure_cells[
    ,
    .(
      province,
      birth_cohort,
      exposure_quintile
    )
  ],
  by = c(
    "province",
    "birth_cohort"
  ),
  all.x = TRUE,
  sort = FALSE
)


# ------------------------------------------------------------
# 8. Exposure-bin diagnostics
# ------------------------------------------------------------

quintile_cell_summary <- exposure_cells[
  ,
  .(
    province_cohort_cells = .N,
    minimum_log_exposure = min(log_exposure),
    mean_log_exposure = mean(log_exposure),
    median_log_exposure = median(log_exposure),
    maximum_log_exposure = max(log_exposure)
  ),
  by = exposure_quintile
]

fwrite(
  quintile_cell_summary,
  file.path(
    output_dir,
    "mismatch_exposure_quintile_cell_summary.csv"
  )
)

print(quintile_cell_summary)


# ------------------------------------------------------------
# 9. Outcome definitions and samples
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
# 10. Estimate quintile models
# ------------------------------------------------------------

models <- list()
failed_models <- list()

failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  eligibility_var <-
    outcome_info[[outcome_name]]$eligibility
  
  
  estimation_sample <- dt[
    base_sample == TRUE &
      get(eligibility_var) == TRUE &
      !is.na(get(outcome_name)) &
      !is.na(exposure_quintile)
  ]
  
  
  model_formula <- as.formula(
    paste0(
      outcome_name,
      " ~ i(exposure_quintile, ref = 'Q3 Middle')",
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
      error_message = conditionMessage(model_attempt)
    )
    
    next
  }
  
  
  models[[outcome_name]] <- model_attempt
}


if (length(models) == 0L) {
  stop("All mismatch quintile models failed.")
}


# ------------------------------------------------------------
# 11. Extract coefficient results
# ------------------------------------------------------------

extract_quintile_results <- function(
    model,
    outcome_id,
    outcome_label
) {
  
  beta <- coef(model)
  se_beta <- se(model)
  p_beta <- pvalue(model)
  
  terms <- names(beta)[
    grepl(
      "^exposure_quintile::",
      names(beta)
    )
  ]
  
  
  rbindlist(
    lapply(
      terms,
      function(term_name) {
        
        estimate <- unname(beta[term_name])
        standard_error <- unname(se_beta[term_name])
        p_value <- unname(p_beta[term_name])
        
        quintile_label <- sub(
          "^exposure_quintile::",
          "",
          term_name
        )
        
        data.table(
          outcome_id = outcome_id,
          outcome = outcome_label,
          term = term_name,
          quintile = quintile_label,
          reference_group = "Q3 Middle",
          estimate_probability = estimate,
          effect_percentage_points = 100 * estimate,
          standard_error_pp = 100 * standard_error,
          lower_95_pp =
            100 * (
              estimate - 1.96 * standard_error
            ),
          upper_95_pp =
            100 * (
              estimate + 1.96 * standard_error
            ),
          p_value = p_value,
          observations = nobs(model)
        )
      }
    )
  )
}


quintile_results <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      extract_quintile_results(
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


# Add reference rows
reference_rows <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      data.table(
        outcome_id = outcome_name,
        outcome =
          outcome_info[[outcome_name]]$label,
        term = NA_character_,
        quintile = "Q3 Middle",
        reference_group = "Q3 Middle",
        estimate_probability = 0,
        effect_percentage_points = 0,
        standard_error_pp = NA_real_,
        lower_95_pp = NA_real_,
        upper_95_pp = NA_real_,
        p_value = NA_real_,
        observations =
          nobs(models[[outcome_name]])
      )
    }
  )
)


quintile_results <- rbindlist(
  list(
    quintile_results,
    reference_rows
  ),
  use.names = TRUE,
  fill = TRUE
)


quintile_order <- c(
  "Q1 Lowest",
  "Q2 Lower-middle",
  "Q3 Middle",
  "Q4 Upper-middle",
  "Q5 Highest"
)

quintile_results[
  ,
  quintile := factor(
    quintile,
    levels = quintile_order
  )
]

setorder(
  quintile_results,
  outcome_id,
  quintile
)


# ------------------------------------------------------------
# 12. Correct formal tests
# ------------------------------------------------------------

# Test the difference between two coefficients directly
pairwise_test <- function(
    model,
    term_a,
    term_b,
    test_label
) {
  
  beta <- coef(model)
  vcov_matrix <- vcov(model)
  
  if (
    !term_a %in% names(beta) ||
    !term_b %in% names(beta)
  ) {
    
    return(
      data.table(
        test = test_label,
        difference_pp = NA_real_,
        standard_error_pp = NA_real_,
        statistic = NA_real_,
        p_value = NA_real_
      )
    )
  }
  
  
  difference <-
    beta[term_a] - beta[term_b]
  
  
  variance_difference <-
    vcov_matrix[term_a, term_a] +
    vcov_matrix[term_b, term_b] -
    2 * vcov_matrix[term_a, term_b]
  
  
  standard_error <-
    sqrt(variance_difference)
  
  
  z_statistic <-
    difference / standard_error
  
  
  p_value <-
    2 * pnorm(
      abs(z_statistic),
      lower.tail = FALSE
    )
  
  
  data.table(
    test = test_label,
    difference_pp =
      100 * unname(difference),
    standard_error_pp =
      100 * unname(standard_error),
    statistic =
      unname(z_statistic),
    p_value =
      unname(p_value)
  )
}


formal_test_results <- list()
test_counter <- 0L


for (outcome_name in names(models)) {
  
  model <- models[[outcome_name]]
  
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  coefficient_names <- names(coef(model))
  
  
  q1_term <- coefficient_names[
    grepl(
      "Q1 Lowest$",
      coefficient_names
    )
  ]
  
  q2_term <- coefficient_names[
    grepl(
      "Q2 Lower-middle$",
      coefficient_names
    )
  ]
  
  q4_term <- coefficient_names[
    grepl(
      "Q4 Upper-middle$",
      coefficient_names
    )
  ]
  
  q5_term <- coefficient_names[
    grepl(
      "Q5 Highest$",
      coefficient_names
    )
  ]
  
  
  # Joint test that all exposure-bin coefficients equal zero
  joint_result <- fixest::wald(
    model,
    keep = "exposure_quintile"
  )
  
  test_counter <- test_counter + 1L
  
  formal_test_results[[test_counter]] <- data.table(
    outcome_id = outcome_name,
    outcome = outcome_label,
    test =
      "All non-reference quintile coefficients equal zero",
    difference_pp = NA_real_,
    standard_error_pp = NA_real_,
    statistic = unname(joint_result$stat),
    p_value = unname(joint_result$p)
  )
  
  
  # Q1 versus Q5
  if (
    length(q1_term) == 1L &&
    length(q5_term) == 1L
  ) {
    
    result <- pairwise_test(
      model,
      q1_term,
      q5_term,
      "Q1 equals Q5"
    )
    
    result[
      ,
      `:=`(
        outcome_id = outcome_name,
        outcome = outcome_label
      )
    ]
    
    test_counter <- test_counter + 1L
    formal_test_results[[test_counter]] <- result
  }
  
  
  # Q4 versus Q5
  if (
    length(q4_term) == 1L &&
    length(q5_term) == 1L
  ) {
    
    result <- pairwise_test(
      model,
      q4_term,
      q5_term,
      "Q4 equals Q5"
    )
    
    result[
      ,
      `:=`(
        outcome_id = outcome_name,
        outcome = outcome_label
      )
    ]
    
    test_counter <- test_counter + 1L
    formal_test_results[[test_counter]] <- result
  }
  
  
  # Q1 versus Q2
  if (
    length(q1_term) == 1L &&
    length(q2_term) == 1L
  ) {
    
    result <- pairwise_test(
      model,
      q1_term,
      q2_term,
      "Q1 equals Q2"
    )
    
    result[
      ,
      `:=`(
        outcome_id = outcome_name,
        outcome = outcome_label
      )
    ]
    
    test_counter <- test_counter + 1L
    formal_test_results[[test_counter]] <- result
  }
}


formal_tests <- rbindlist(
  formal_test_results,
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
    "p_value"
  )
)


# ------------------------------------------------------------
# 13. Format compact table
# ------------------------------------------------------------

quintile_results[
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


quintile_results[
  ,
  formatted_result := fifelse(
    quintile == "Q3 Middle",
    "Reference",
    sprintf(
      "%.3f%s (%.3f)",
      effect_percentage_points,
      stars,
      standard_error_pp
    )
  )
]


compact_table <- dcast(
  quintile_results,
  quintile ~ outcome,
  value.var = "formatted_result"
)


# ------------------------------------------------------------
# 14. Save outputs
# ------------------------------------------------------------

fwrite(
  quintile_results,
  file.path(
    output_dir,
    "nonlinear_mismatch_quintile_detailed_results.csv"
  )
)

fwrite(
  compact_table,
  file.path(
    output_dir,
    "nonlinear_mismatch_quintile_table_wide.csv"
  )
)

fwrite(
  formal_tests,
  file.path(
    output_dir,
    "nonlinear_mismatch_formal_tests.csv"
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
    "nonlinear_mismatch_failed_models.csv"
  )
)


# ------------------------------------------------------------
# 15. Coefficient figures
# ------------------------------------------------------------

plot_results <- quintile_results[
  quintile != "Q3 Middle"
]


for (current_outcome in unique(plot_results$outcome)) {
  
  current_data <- copy(
    plot_results[
      outcome == current_outcome
    ]
  )
  
  
  figure <- ggplot(
    current_data,
    aes(
      x = quintile,
      y = effect_percentage_points
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
    geom_point(size = 2.2) +
    labs(
      title = paste0(
        "University exposure bins and ",
        tolower(current_outcome)
      ),
      subtitle =
        "Q3 is the omitted reference category",
      x =
        "Province–cohort exposure quintile",
      y =
        "Percentage-point difference relative to Q3"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x = element_text(
        angle = 25,
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
        "nonlinear_",
        safe_name,
        "_quintiles.png"
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
# 16. Print outputs
# ------------------------------------------------------------

cat(
  "\n================ QUINTILE TABLE =================\n"
)

print(compact_table)


cat(
  "\n================ FORMAL TESTS ===================\n"
)

print(formal_tests)


cat(
  "\n================ FAILED MODELS ==================\n"
)

print(failed_table)


cat(
  "\nOutputs saved to:\n",
  output_dir,
  "\n"
)