# ============================================================
# NONLINEAR EXPOSURE EFFECTS:
# FIVE PROVINCE-COHORT EXPOSURE BINS, Q3 AS REFERENCE
# ============================================================
#
# Outcomes:
#   1. Labor-force participation
#   2. Employment
#   3. Unemployment among labor-force participants
#
# Exposure bins:
#   Q1 = lowest exposure
#   Q2 = lower-middle exposure
#   Q3 = middle exposure, reference category
#   Q4 = upper-middle exposure
#   Q5 = highest exposure
#
# Specification:
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
  "nonlinear_employment_outcomes"
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
# 2. Inspect and identify variable names
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
  c(
    "log_undergrad_exposure_18_20"
  ),
  available_vars
)

raw_exposure_var <- first_available(
  c(
    "undergrad_exposure_18_20"
  ),
  available_vars
)

province_var <- first_available(
  c(
    "province_historical",
    "historical_province"
  ),
  available_vars
)

cohort_var <- first_available(
  c(
    "analysis_birth_year",
    "birth_year"
  ),
  available_vars
)

survey_year_var <- first_available(
  c(
    "year",
    "survey_year"
  ),
  available_vars
)

weight_var <- first_available(
  c(
    "weight",
    "survey_weight"
  ),
  available_vars
)

gender_var <- first_available(
  c(
    "gender",
    "sex"
  ),
  available_vars
)

urban_var <- first_available(
  c(
    "urban",
    "urban_rural_clean2",
    "urban_rural"
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


required_vars <- c(
  treatment_var,
  province_var,
  cohort_var,
  survey_year_var,
  weight_var,
  gender_var,
  lfp_var,
  employment_var,
  unemployment_var
)

if (anyNA(required_vars)) {
  
  stop(
    "One or more required variables were not found.\n",
    "Log exposure: ", treatment_var, "\n",
    "Province: ", province_var, "\n",
    "Cohort: ", cohort_var, "\n",
    "Survey year: ", survey_year_var, "\n",
    "Weight: ", weight_var, "\n",
    "Gender: ", gender_var, "\n",
    "LFP: ", lfp_var, "\n",
    "Employment: ", employment_var, "\n",
    "Unemployment: ", unemployment_var
  )
}


cat("\nVariables selected:\n")
cat("Treatment:", treatment_var, "\n")
cat("Raw exposure:", raw_exposure_var, "\n")
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


# ------------------------------------------------------------
# 3. Read only required columns
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
      unemployment_var
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

setnames(dt, lfp_var, "lfp")
setnames(dt, employment_var, "employment")
setnames(dt, unemployment_var, "unemployment")

if (!is.na(raw_exposure_var)) {
  setnames(dt, raw_exposure_var, "raw_exposure")
}

if (!is.na(urban_var)) {
  setnames(dt, urban_var, "urban_original")
}

if (!is.na(sample_var)) {
  setnames(dt, sample_var, "main_sample_flag")
}


# ------------------------------------------------------------
# 5. Construct controls and baseline sample
# ------------------------------------------------------------

# Verified project coding:
# gender == 1 means women
# gender == 2 means men
dt[
  ,
  female := as.integer(gender_original == 1)
]


# Standardize urban indicator
if ("urban_original" %in% names(dt)) {
  
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
    
    urban_text <- tolower(
      trimws(
        as.character(dt$urban_original)
      )
    )
    
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
  
  stop(
    "An urban variable is required to reproduce the baseline specification."
  )
}


# Existing main-sample flag, when available
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


# General restrictions
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


# ------------------------------------------------------------
# 6. Validate province-cohort exposure cells
# ------------------------------------------------------------

# Exposure should be constant within each province-cohort cell
cell_validation <- dt[
  base_sample == TRUE,
  .(
    number_log_exposure_values =
      uniqueN(log_exposure),
    
    minimum_log_exposure =
      min(log_exposure, na.rm = TRUE),
    
    maximum_log_exposure =
      max(log_exposure, na.rm = TRUE),
    
    observations = .N
  ),
  by = .(
    province,
    birth_cohort
  )
]


problem_cells <- cell_validation[
  number_log_exposure_values != 1L
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
    "Exposure is not constant within some province-cohort cells. ",
    "Inspect problematic_province_cohort_cells.csv."
  )
}


# ------------------------------------------------------------
# 7. Create five exposure bins at province-cohort level
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


# Rank province-cohort cells by exposure.
# ties.method = "average" gives equal exposures the same rank.
exposure_cells[
  ,
  exposure_rank :=
    frank(
      log_exposure,
      ties.method = "average"
    )
]


# Convert ranked cells into five approximately equal groups
exposure_cells[
  ,
  exposure_quintile :=
    pmin(
      5L,
      ceiling(
        5 * exposure_rank / .N
      )
    )
]


# Convert to factor with Q3 as reference
exposure_cells[
  ,
  exposure_quintile :=
    factor(
      exposure_quintile,
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


# Merge the same cell-level bins back to all individuals
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
# 8. Save exposure-bin diagnostics
# ------------------------------------------------------------

quintile_cell_summary <- exposure_cells[
  ,
  .(
    province_cohort_cells = .N,
    
    minimum_log_exposure =
      min(log_exposure),
    
    mean_log_exposure =
      mean(log_exposure),
    
    median_log_exposure =
      median(log_exposure),
    
    maximum_log_exposure =
      max(log_exposure)
  ),
  by = exposure_quintile
]


quintile_individual_summary <- dt[
  base_sample == TRUE,
  .(
    observations = .N,
    
    weighted_population =
      sum(survey_weight, na.rm = TRUE),
    
    minimum_log_exposure =
      min(log_exposure),
    
    mean_log_exposure =
      weighted.mean(
        log_exposure,
        survey_weight,
        na.rm = TRUE
      ),
    
    maximum_log_exposure =
      max(log_exposure)
  ),
  by = exposure_quintile
]


fwrite(
  quintile_cell_summary,
  file.path(
    output_dir,
    "exposure_quintile_cell_summary.csv"
  )
)

fwrite(
  quintile_individual_summary,
  file.path(
    output_dir,
    "exposure_quintile_individual_summary.csv"
  )
)


cat("\nProvince-cohort cell distribution:\n")
print(quintile_cell_summary)

cat("\nIndividual distribution:\n")
print(quintile_individual_summary)


# ------------------------------------------------------------
# 9. Define outcome-specific samples
# ------------------------------------------------------------

outcome_info <- list(
  
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
      
      # Unemployment is estimated only among
      # labor-force participants
      !is.na(x$unemployment) &
        x$lfp == 1
    }
  )
)


# ------------------------------------------------------------
# 10. Estimate quintile models
# ------------------------------------------------------------

models <- list()
failed_models <- list()

model_counter <- 0L
failure_counter <- 0L


for (outcome_name in names(outcome_info)) {
  
  outcome_label <- outcome_info[[outcome_name]]$label
  
  sample_condition <-
    outcome_info[[outcome_name]]$sample_condition(dt)
  
  
  estimation_sample <- dt[
    base_sample == TRUE &
      !is.na(exposure_quintile) &
      sample_condition
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
      error_message =
        conditionMessage(model_attempt)
    )
    
    next
  }
  
  
  model_counter <- model_counter + 1L
  models[[outcome_name]] <- model_attempt
}


if (length(models) == 0L) {
  stop("All nonlinear employment models failed.")
}


# ------------------------------------------------------------
# 11. Extract quintile coefficients
# ------------------------------------------------------------

extract_quintile_results <- function(
    model,
    outcome_name,
    outcome_label
) {
  
  coefficients <- coef(model)
  standard_errors <- se(model)
  p_values <- pvalue(model)
  
  relevant_terms <- names(coefficients)[
    grepl(
      "^exposure_quintile::",
      names(coefficients)
    )
  ]
  
  
  result <- rbindlist(
    lapply(
      relevant_terms,
      function(term_name) {
        
        estimate <- unname(
          coefficients[term_name]
        )
        
        standard_error <- unname(
          standard_errors[term_name]
        )
        
        p_value <- unname(
          p_values[term_name]
        )
        
        
        quintile_label <- sub(
          "^exposure_quintile::",
          "",
          term_name
        )
        
        
        data.table(
          outcome_id = outcome_name,
          outcome = outcome_label,
          quintile = quintile_label,
          reference_group = "Q3 Middle",
          estimate_probability = estimate,
          effect_percentage_points =
            100 * estimate,
          standard_error_pp =
            100 * standard_error,
          lower_95_pp =
            100 * (
              estimate -
                1.96 * standard_error
            ),
          upper_95_pp =
            100 * (
              estimate +
                1.96 * standard_error
            ),
          p_value = p_value,
          observations = nobs(model)
        )
      }
    )
  )
  
  result
}


quintile_results <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      extract_quintile_results(
        model = models[[outcome_name]],
        outcome_name = outcome_name,
        outcome_label =
          outcome_info[[outcome_name]]$label
      )
    }
  ),
  use.names = TRUE,
  fill = TRUE
)


# Add the Q3 reference row
reference_rows <- rbindlist(
  lapply(
    names(models),
    function(outcome_name) {
      
      data.table(
        outcome_id = outcome_name,
        outcome =
          outcome_info[[outcome_name]]$label,
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
  quintile :=
    factor(
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
# 12. Formal tests
# ------------------------------------------------------------

test_results <- list()
test_counter <- 0L


for (outcome_name in names(models)) {
  
  model <- models[[outcome_name]]
  outcome_label <-
    outcome_info[[outcome_name]]$label
  
  
  term_names <- names(coef(model))
  
  
  q1_term <- term_names[
    grepl("Q1 Lowest$", term_names)
  ]
  
  q2_term <- term_names[
    grepl("Q2 Lower-middle$", term_names)
  ]
  
  q4_term <- term_names[
    grepl("Q4 Upper-middle$", term_names)
  ]
  
  q5_term <- term_names[
    grepl("Q5 Highest$", term_names)
  ]
  
  
  # Joint test: all non-reference quintile coefficients = 0
  joint_test <- wald(
    model,
    keep = "exposure_quintile"
  )
  
  
  test_counter <- test_counter + 1L
  
  test_results[[test_counter]] <- data.table(
    outcome_id = outcome_name,
    outcome = outcome_label,
    test = "All quintile indicators jointly equal zero",
    statistic = unname(joint_test$stat),
    p_value = unname(joint_test$p)
  )
  
  
  # Q1 = Q5
  if (
    length(q1_term) == 1L &&
    length(q5_term) == 1L
  ) {
    
    result_q1_q5 <- wald(
      model,
      hypothesis = paste0(
        "`",
        q1_term,
        "` = `",
        q5_term,
        "`"
      )
    )
    
    test_counter <- test_counter + 1L
    
    test_results[[test_counter]] <- data.table(
      outcome_id = outcome_name,
      outcome = outcome_label,
      test = "Q1 equals Q5",
      statistic =
        unname(result_q1_q5$stat),
      p_value =
        unname(result_q1_q5$p)
    )
  }
  
  
  # Q4 = Q5
  if (
    length(q4_term) == 1L &&
    length(q5_term) == 1L
  ) {
    
    result_q4_q5 <- wald(
      model,
      hypothesis = paste0(
        "`",
        q4_term,
        "` = `",
        q5_term,
        "`"
      )
    )
    
    test_counter <- test_counter + 1L
    
    test_results[[test_counter]] <- data.table(
      outcome_id = outcome_name,
      outcome = outcome_label,
      test = "Q4 equals Q5",
      statistic =
        unname(result_q4_q5$stat),
      p_value =
        unname(result_q4_q5$p)
    )
  }
}


formal_tests <- rbindlist(
  test_results,
  use.names = TRUE,
  fill = TRUE
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


fwrite(
  quintile_results,
  file.path(
    output_dir,
    "nonlinear_employment_quintile_detailed_results.csv"
  )
)

fwrite(
  compact_table,
  file.path(
    output_dir,
    "nonlinear_employment_quintile_table_wide.csv"
  )
)

fwrite(
  formal_tests,
  file.path(
    output_dir,
    "nonlinear_employment_formal_tests.csv"
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
    "nonlinear_employment_failed_models.csv"
  )
)


# ------------------------------------------------------------
# 14. Coefficient figures
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
    geom_point(
      size = 2.2
    ) +
    labs(
      title = paste0(
        "University exposure bins and ",
        tolower(current_outcome)
      ),
      subtitle =
        "Q3 is the omitted reference category",
      x = "Province–cohort exposure quintile",
      y = "Percentage-point difference relative to Q3"
    ) +
    theme_minimal(
      base_size = 11
    ) +
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
# 15. Print results
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