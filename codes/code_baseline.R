# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 8: Corrected Baseline Identification and Core Outcome Regressions
#
# Main estimator:
#   Survey-weighted linear probability model
#
# Treatment:
#   Undergraduate enrollment exposure at ages 18–20
#
# Fixed effects:
#   Historical province
#   Corrected birth cohort: analysis_birth_year
#   Survey year
#
# Standard errors:
#   Two-way clustered by historical province and corrected birth cohort
#
# Samples:
#   Pooled
#   Women
#   Men
#
# Outcomes:
#   Labor-force participation
#   Employment
#   Unemployment among labor-force participants
#   Currently married
#   Ever married
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(
  scipen = 999,
  datatable.print.nrows = 100
)

required_packages <- c(
  "data.table",
  "fst",
  "fixest"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

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
  "step8_baseline_regressions_corrected"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The final dataset was not found:\n",
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
  "age",
  "gender",
  
  # Birth-year variables
  "birth_year",
  "analysis_birth_year",
  
  # Geography
  "province_historical",
  "urban_rural",
  
  # Treatment
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Outcomes
  "labor_force_participation",
  "employed",
  "unemployed",
  "currently_married",
  "ever_married",
  
  # Migration robustness variable
  "recent_cross_historical_region_move"
)

cat("\nReading regression variables...\n")

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
# STEP 3: Standardize variables
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
  urban :=
    fcase(
      urban_rural == "Urban", 1L,
      urban_rural == "Rural", 0L,
      default = NA_integer_
    )
]

dt[
  ,
  female :=
    fcase(
      gender == 1, 1L,
      gender == 2, 0L,
      default = NA_integer_
    )
]

dt[
  ,
  male :=
    fcase(
      gender == 2, 1L,
      gender == 1, 0L,
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 4: Inspect corrected cohort distribution
# ==============================================================================

cohort_distribution <- dt[
  ,
  .N,
  by = cohort
][
  order(cohort)
]

print(cohort_distribution)

fwrite(
  cohort_distribution,
  file.path(
    output_folder,
    "step8_corrected_cohort_distribution.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 5: Define valid main sample
#
# Valid corrected Persian birth cohorts:
#   1355–1383
# ==============================================================================

dt[
  ,
  valid_weight :=
    as.integer(
      !is.na(weight) &
        is.finite(weight) &
        weight > 0
    )
]

dt[
  ,
  complete_exposure_sample :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(undergrad_exposure_18_20)
    )
]

dt[
  ,
  valid_birth_cohort :=
    as.integer(
      !is.na(cohort) &
        cohort >= 1355L &
        cohort <= 1383L
    )
]

dt[
  ,
  sample_main :=
    as.integer(
      complete_exposure_sample == 1L &
        valid_weight == 1L &
        valid_birth_cohort == 1L &
        !is.na(historical_province) &
        !is.na(year) &
        !is.na(gender) &
        !is.na(urban)
    )
]

cat(
  "\nMain corrected-cohort sample:",
  format(
    dt[
      sample_main == 1L,
      .N
    ],
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 6: Define outcome-specific samples
# ==============================================================================

dt[
  ,
  sample_lfp :=
    as.integer(
      sample_main == 1L &
        !is.na(labor_force_participation)
    )
]

dt[
  ,
  sample_employment :=
    as.integer(
      sample_main == 1L &
        !is.na(employed)
    )
]

dt[
  ,
  sample_unemployment :=
    as.integer(
      sample_main == 1L &
        labor_force_participation == 1L &
        !is.na(unemployed)
    )
]

dt[
  ,
  sample_currently_married :=
    as.integer(
      sample_main == 1L &
        !is.na(currently_married)
    )
]

dt[
  ,
  sample_ever_married :=
    as.integer(
      sample_main == 1L &
        !is.na(ever_married)
    )
]


# ==============================================================================
# STEP 7: Scale treatment
#
# A coefficient on exposure_per_100 gives the change in outcome probability
# associated with an increase of 100 enrollment units per 100,000 population.
# ==============================================================================

dt[
  ,
  exposure_per_100 :=
    undergrad_exposure_18_20 / 100
]

main_exposure_mean <- dt[
  sample_main == 1L,
  weighted.mean(
    undergrad_exposure_18_20,
    weight,
    na.rm = TRUE
  )
]

main_exposure_sd <- dt[
  sample_main == 1L,
  sqrt(
    weighted.mean(
      (
        undergrad_exposure_18_20 -
          weighted.mean(
            undergrad_exposure_18_20,
            weight,
            na.rm = TRUE
          )
      )^2,
      weight,
      na.rm = TRUE
    )
  )
]

dt[
  ,
  exposure_standardized :=
    (
      undergrad_exposure_18_20 -
        main_exposure_mean
    ) /
    main_exposure_sd
]

cat(
  "\nWeighted exposure mean:",
  round(main_exposure_mean, 3),
  "\n"
)

cat(
  "Weighted exposure SD:",
  round(main_exposure_sd, 3),
  "\n"
)


# ==============================================================================
# STEP 8: Confirm treatment is constant within province × corrected cohort
# ==============================================================================

treatment_cell_check <- dt[
  sample_main == 1L,
  .(
    observations = .N,
    
    unique_exposure_values = uniqueN(
      undergrad_exposure_18_20
    ),
    
    minimum_exposure = min(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    maximum_exposure = max(
      undergrad_exposure_18_20,
      na.rm = TRUE
    )
  ),
  by = .(
    historical_province,
    cohort
  )
][
  order(
    historical_province,
    cohort
  )
]

problem_treatment_cells <- treatment_cell_check[
  unique_exposure_values > 1L |
    abs(
      maximum_exposure -
        minimum_exposure
    ) > 0.000001
]

cat(
  "\nProvince-cohort treatment cells:",
  nrow(treatment_cell_check),
  "\n"
)

cat(
  "Problem treatment cells:",
  nrow(problem_treatment_cells),
  "\n"
)

fwrite(
  treatment_cell_check,
  file.path(
    output_folder,
    "step8_treatment_cell_check_corrected.csv"
  ),
  bom = TRUE
)

fwrite(
  problem_treatment_cells,
  file.path(
    output_folder,
    "step8_problem_treatment_cells_corrected.csv"
  ),
  bom = TRUE
)

if (nrow(problem_treatment_cells) > 0L) {
  warning(
    "Treatment is still inconsistent within some corrected province-cohort cells."
  )
}


# ==============================================================================
# STEP 9: Identification comparison using labor-force participation
# ==============================================================================

lfp_data <- dt[
  sample_lfp == 1L
]

cat(
  "\nLFP sample:",
  format(nrow(lfp_data), big.mark = ","),
  "\n"
)


# Province fixed effects only
model_lfp_fe1 <- feols(
  labor_force_participation ~ exposure_per_100 |
    historical_province,
  data = lfp_data,
  weights = ~weight,
  cluster = ~historical_province + cohort
)


# Province and cohort fixed effects
model_lfp_fe2 <- feols(
  labor_force_participation ~ exposure_per_100 |
    historical_province +
    cohort,
  data = lfp_data,
  weights = ~weight,
  cluster = ~historical_province + cohort
)


# Province, cohort, and survey-year fixed effects
model_lfp_fe3 <- feols(
  labor_force_participation ~ exposure_per_100 |
    historical_province +
    cohort +
    year,
  data = lfp_data,
  weights = ~weight,
  cluster = ~historical_province + cohort
)


# Preferred pooled model with gender and urban controls
model_lfp_preferred <- feols(
  labor_force_participation ~
    exposure_per_100 +
    female +
    urban |
    historical_province +
    cohort +
    year,
  data = lfp_data,
  weights = ~weight,
  cluster = ~historical_province + cohort
)


cat(
  "\n============================================================\n",
  "LFP IDENTIFICATION COMPARISON\n",
  "============================================================\n"
)

etable(
  model_lfp_fe1,
  model_lfp_fe2,
  model_lfp_fe3,
  model_lfp_preferred,
  
  headers = c(
    "Province FE",
    "Province + Cohort FE",
    "Province + Cohort + Year FE",
    "Preferred pooled"
  ),
  
  keep = "exposure_per_100|female|urban",
  
  fitstat = ~n + r2 + wr2
)

capture.output(
  etable(
    model_lfp_fe1,
    model_lfp_fe2,
    model_lfp_fe3,
    model_lfp_preferred,
    
    headers = c(
      "Province FE",
      "Province + Cohort FE",
      "Province + Cohort + Year FE",
      "Preferred pooled"
    ),
    
    keep = "exposure_per_100|female|urban",
    
    fitstat = ~n + r2 + wr2
  ),
  
  file = file.path(
    output_folder,
    "step8_lfp_identification_comparison_corrected.txt"
  )
)


# ==============================================================================
# STEP 10: Helper function for pooled, women, and men models
# ==============================================================================

estimate_gender_models <- function(
    data,
    outcome_variable,
    sample_variable
) {
  
  analysis_data <- data[
    get(sample_variable) == 1L
  ]
  
  pooled_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ exposure_per_100 + female + urban | ",
        "historical_province + cohort + year"
      )
    ),
    data = analysis_data,
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  female_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ exposure_per_100 + urban | ",
        "historical_province + cohort + year"
      )
    ),
    data = analysis_data[
      gender == 1L
    ],
    weights = ~weight,
    cluster = ~historical_province + cohort
  )
  
  male_model <- feols(
    as.formula(
      paste0(
        outcome_variable,
        " ~ exposure_per_100 + urban | ",
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
    pooled = pooled_model,
    female = female_model,
    male = male_model
  )
}


# ==============================================================================
# STEP 11: Estimate core outcome models
# ==============================================================================

models_lfp <- estimate_gender_models(
  data = dt,
  outcome_variable = "labor_force_participation",
  sample_variable = "sample_lfp"
)

models_employment <- estimate_gender_models(
  data = dt,
  outcome_variable = "employed",
  sample_variable = "sample_employment"
)

models_unemployment <- estimate_gender_models(
  data = dt,
  outcome_variable = "unemployed",
  sample_variable = "sample_unemployment"
)

models_currently_married <- estimate_gender_models(
  data = dt,
  outcome_variable = "currently_married",
  sample_variable = "sample_currently_married"
)

models_ever_married <- estimate_gender_models(
  data = dt,
  outcome_variable = "ever_married",
  sample_variable = "sample_ever_married"
)


# ==============================================================================
# STEP 12: Display model tables
# ==============================================================================

display_model_table <- function(
    models,
    title
) {
  
  cat(
    "\n============================================================\n",
    title,
    "\n",
    "============================================================\n"
  )
  
  print(
    etable(
      models$pooled,
      models$female,
      models$male,
      
      headers = c(
        "Pooled",
        "Women",
        "Men"
      ),
      
      keep = "exposure_per_100|female|urban",
      
      fitstat = ~n + r2 + wr2
    )
  )
}


display_model_table(
  models_lfp,
  "LABOR-FORCE PARTICIPATION"
)

display_model_table(
  models_employment,
  "EMPLOYMENT"
)

display_model_table(
  models_unemployment,
  "UNEMPLOYMENT AMONG LABOR-FORCE PARTICIPANTS"
)

display_model_table(
  models_currently_married,
  "CURRENTLY MARRIED"
)

display_model_table(
  models_ever_married,
  "EVER MARRIED"
)


# ==============================================================================
# STEP 13: Extract exposure coefficients
# ==============================================================================

extract_exposure_result <- function(
    model,
    outcome_name,
    sample_name
) {
  
  coefficient_table <- as.data.table(
    coeftable(model),
    keep.rownames = "term"
  )
  
  exposure_row <- coefficient_table[
    term == "exposure_per_100"
  ]
  
  if (nrow(exposure_row) == 0L) {
    
    return(
      data.table(
        outcome = outcome_name,
        sample = sample_name,
        coefficient = NA_real_,
        standard_error = NA_real_,
        t_statistic = NA_real_,
        p_value = NA_real_,
        observations = nobs(model)
      )
    )
  }
  
  data.table(
    outcome = outcome_name,
    sample = sample_name,
    coefficient = exposure_row[[2]],
    standard_error = exposure_row[[3]],
    t_statistic = exposure_row[[4]],
    p_value = exposure_row[[5]],
    observations = nobs(model)
  )
}


model_groups <- list(
  "Labor-force participation" = models_lfp,
  "Employment" = models_employment,
  "Unemployment" = models_unemployment,
  "Currently married" = models_currently_married,
  "Ever married" = models_ever_married
)


baseline_results_list <- list()

counter <- 1L

for (outcome_name in names(model_groups)) {
  
  current_models <- model_groups[[outcome_name]]
  
  for (sample_name in c(
    "pooled",
    "female",
    "male"
  )) {
    
    baseline_results_list[[counter]] <-
      extract_exposure_result(
        model = current_models[[sample_name]],
        outcome_name = outcome_name,
        sample_name = sample_name
      )
    
    counter <- counter + 1L
  }
}


baseline_exposure_results <- rbindlist(
  baseline_results_list
)

baseline_exposure_results[
  ,
  effect_percentage_points :=
    100 * coefficient
]

baseline_exposure_results[
  ,
  lower_95 :=
    coefficient -
    1.96 * standard_error
]

baseline_exposure_results[
  ,
  upper_95 :=
    coefficient +
    1.96 * standard_error
]

baseline_exposure_results[
  ,
  `:=`(
    lower_95_percentage_points =
      100 * lower_95,
    
    upper_95_percentage_points =
      100 * upper_95
  )
]

print(
  baseline_exposure_results
)

fwrite(
  baseline_exposure_results,
  file.path(
    output_folder,
    "step8_baseline_core_outcomes_corrected.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 14: Save model tables
# ==============================================================================

save_model_table <- function(
    models,
    filename
) {
  
  capture.output(
    etable(
      models$pooled,
      models$female,
      models$male,
      
      headers = c(
        "Pooled",
        "Women",
        "Men"
      ),
      
      keep = "exposure_per_100|female|urban",
      
      fitstat = ~n + r2 + wr2
    ),
    
    file = file.path(
      output_folder,
      filename
    )
  )
}


save_model_table(
  models_lfp,
  "step8_lfp_models_corrected.txt"
)

save_model_table(
  models_employment,
  "step8_employment_models_corrected.txt"
)

save_model_table(
  models_unemployment,
  "step8_unemployment_models_corrected.txt"
)

save_model_table(
  models_currently_married,
  "step8_currently_married_models_corrected.txt"
)

save_model_table(
  models_ever_married,
  "step8_ever_married_models_corrected.txt"
)


# ==============================================================================
# STEP 15: Save sample summary
# ==============================================================================

sample_summary <- data.table(
  sample = c(
    "Main sample",
    "Labor-force participation",
    "Employment",
    "Unemployment among participants",
    "Currently married",
    "Ever married"
  ),
  
  observations = c(
    dt[sample_main == 1L, .N],
    dt[sample_lfp == 1L, .N],
    dt[sample_employment == 1L, .N],
    dt[sample_unemployment == 1L, .N],
    dt[sample_currently_married == 1L, .N],
    dt[sample_ever_married == 1L, .N]
  )
)

print(sample_summary)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "step8_sample_summary_corrected.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Final completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "CORRECTED STEP 8 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nCorrect cohort variable:\n",
  "analysis_birth_year\n"
)

cat(
  "\nPreferred fixed effects:\n",
  "historical_province + cohort + year\n"
)

cat(
  "\nPreferred clustering:\n",
  "historical_province + cohort\n"
)

cat(
  "\nMain results file:\n",
  file.path(
    output_folder,
    "step8_baseline_core_outcomes_corrected.csv"
  ),
  "\n"
)


