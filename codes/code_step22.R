# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# PURPOSE:
#   Re-estimate labor-force participation, employment, and unemployment
#   using the preferred log exposure specification.
#
# OUTPUT:
#   employment_quantity_log_exposure_results.csv
#
# Specification:
#   - Treatment: log undergraduate exposure at ages 18–20
#   - Province fixed effects
#   - Birth-cohort fixed effects
#   - Survey-year fixed effects
#   - Survey weights
#   - Two-way clustered standard errors by province and birth cohort
#   - Pooled, women, and men estimates
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
library(fixest)
library(fst)


# ==============================================================================
# STEP 1: Define project folders
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

data_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step22_employment_log_exposure"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(data_file)) {
  stop(
    paste0(
      "Data file not found:\n",
      data_file
    )
  )
}


# ==============================================================================
# STEP 2: Inspect available variables
# ==============================================================================

fst_info <- metadata_fst(data_file)

available_variables <- fst_info$columnNames

cat(
  "\nNumber of variables in dataset:",
  length(available_variables),
  "\n"
)

cat("\nUrban/rural variables:\n")

print(
  available_variables[
    grepl(
      "urban|rural",
      available_variables,
      ignore.case = TRUE
    )
  ]
)

cat("\nGender variables:\n")

print(
  available_variables[
    grepl(
      "gender|female|sex",
      available_variables,
      ignore.case = TRUE
    )
  ]
)

cat("\nExposure variables:\n")

print(
  available_variables[
    grepl(
      "log_undergrad_exposure_18_20",
      available_variables,
      ignore.case = TRUE
    )
  ]
)




# ==============================================================================
# STEP 3: Define required variables
# ==============================================================================

required_variables <- c(
  "labor_force_participation",
  "employed",
  "unemployed",
  "log_undergrad_exposure_18_20",
  "province_historical",
  "analysis_birth_year",
  "year",
  "gender",
  "urban_rural",
  "weight"
)

missing_variables <- setdiff(
  required_variables,
  available_variables
)

if (length(missing_variables) > 0L) {
  stop(
    paste0(
      "The following required variables are missing:\n",
      paste(
        missing_variables,
        collapse = "\n"
      )
    )
  )
}

# ==============================================================================
# STEP 4: Read only required variables
# ==============================================================================

dt <- as.data.table(
  read_fst(
    data_file,
    columns = required_variables
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

cat(
  "\nColumns read:\n"
)

print(names(dt))


# ==============================================================================
# STEP 5: Inspect urban/rural and gender coding
# ==============================================================================

cat(
  "\n============================================================\n",
  "URBAN/RURAL CODING\n",
  "============================================================\n"
)

print(
  dt[
    ,
    .N,
    by = urban_rural
  ][
    order(urban_rural)
  ]
)

cat(
  "\n============================================================\n",
  "GENDER CODING\n",
  "============================================================\n"
)

print(
  dt[
    ,
    .N,
    by = gender
  ][
    order(gender)
  ]
)


# ==============================================================================
# STEP 6: Construct binary urban indicator
# ==============================================================================

if (
  is.numeric(dt$urban_rural) ||
  is.integer(dt$urban_rural)
) {
  
  unique_urban_values <- sort(
    unique(
      dt[
        !is.na(urban_rural),
        urban_rural
      ]
    )
  )
  
  cat(
    "\nNumeric urban_rural values:\n"
  )
  
  print(unique_urban_values)
  
  if (
    all(
      unique_urban_values %in% c(
        0,
        1
      )
    )
  ) {
    
    # Assumes 1 = urban and 0 = rural
    dt[
      ,
      urban := as.integer(
        urban_rural
      )
    ]
    
  } else if (
    all(
      unique_urban_values %in% c(
        1,
        2
      )
    )
  ) {
    
    # Common coding:
    # 1 = Urban
    # 2 = Rural
    dt[
      ,
      urban := fcase(
        urban_rural == 1, 1L,
        urban_rural == 2, 0L,
        default = NA_integer_
      )
    ]
    
  } else {
    
    stop(
      paste0(
        "The numeric coding of urban_rural is not recognized.\n",
        "Observed values: ",
        paste(
          unique_urban_values,
          collapse = ", "
        )
      )
    )
  }
  
} else {
  
  urban_text <- tolower(
    trimws(
      as.character(
        dt$urban_rural
      )
    )
  )
  
  dt[
    ,
    urban := fcase(
      urban_text %in% c(
        "urban",
        "u"
      ),
      1L,
      
      urban_text %in% c(
        "rural",
        "r"
      ),
      0L,
      
      default = NA_integer_
    )
  ]
}

cat(
  "\nConstructed urban indicator:\n"
)

print(
  dt[
    ,
    .N,
    by = .(
      urban_rural,
      urban
    )
  ][
    order(urban_rural)
  ]
)


# ==============================================================================
# STEP 7: Construct binary female indicator
# ==============================================================================

if (
  is.numeric(dt$gender) ||
  is.integer(dt$gender)
) {
  
  unique_gender_values <- sort(
    unique(
      dt[
        !is.na(gender),
        gender
      ]
    )
  )
  
  cat(
    "\nNumeric gender values:\n"
  )
  
  print(unique_gender_values)
  
  if (
    all(
      unique_gender_values %in% c(
        0,
        1
      )
    )
  ) {
    
    # This block assumes:
    # 1 = female
    # 0 = male
    #
    # The printed diagnostic above should confirm this.
    dt[
      ,
      female := as.integer(
        gender == 1
      )
    ]
    
  } else if (
    all(
      unique_gender_values %in% c(
        1,
        2
      )
    )
  ) {
    
    # Common Iranian LFS coding:
    # 1 = male
    # 2 = female
    dt[
      ,
      female := fcase(
        gender == 2, 1L,
        gender == 1, 0L,
        default = NA_integer_
      )
    ]
    
  } else {
    
    stop(
      paste0(
        "The numeric coding of gender is not recognized.\n",
        "Observed values: ",
        paste(
          unique_gender_values,
          collapse = ", "
        )
      )
    )
  }
  
} else {
  
  gender_text <- tolower(
    trimws(
      as.character(
        dt$gender
      )
    )
  )
  
  dt[
    ,
    female := fcase(
      gender_text %in% c(
        "female",
        "woman",
        "women",
        "f"
      ),
      1L,
      
      gender_text %in% c(
        "male",
        "man",
        "men",
        "m"
      ),
      0L,
      
      default = NA_integer_
    )
  ]
}

cat(
  "\nConstructed female indicator:\n"
)

print(
  dt[
    ,
    .N,
    by = .(
      gender,
      female
    )
  ][
    order(gender)
  ]
)


# ==============================================================================
# STEP 8: Validate constructed indicators
# ==============================================================================

if (
  dt[
    !is.na(urban),
    !all(urban %in% c(0L, 1L))
  ]
) {
  stop("The constructed urban variable contains values other than 0 and 1.")
}

if (
  dt[
    !is.na(female),
    !all(female %in% c(0L, 1L))
  ]
) {
  stop("The constructed female variable contains values other than 0 and 1.")
}

if (
  dt[
    !is.na(female),
    uniqueN(female)
  ] != 2L
) {
  stop("The female indicator does not contain both women and men.")
}




# ==============================================================================
# STEP 9: Construct analysis sample
# ==============================================================================

analysis_dt <- dt[
  !is.na(log_undergrad_exposure_18_20) &
    is.finite(log_undergrad_exposure_18_20) &
    !is.na(province_historical) &
    !is.na(analysis_birth_year) &
    analysis_birth_year >= 1355 &
    analysis_birth_year <= 1383 &
    !is.na(year) &
    !is.na(female) &
    !is.na(urban) &
    !is.na(weight) &
    is.finite(weight) &
    weight > 0
]

cat(
  "\n============================================================\n",
  "ANALYSIS SAMPLE\n",
  "============================================================\n"
)

cat(
  "\nAnalysis observations:",
  format(
    nrow(analysis_dt),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Historical provinces:",
  uniqueN(analysis_dt$province_historical),
  "\n"
)

cat(
  "Birth cohorts:",
  uniqueN(analysis_dt$analysis_birth_year),
  "\n"
)

cat(
  "Survey years:",
  uniqueN(analysis_dt$year),
  "\n"
)

cat("\nBirth-cohort range:\n")

print(
  analysis_dt[
    ,
    .(
      minimum_cohort = min(analysis_birth_year),
      maximum_cohort = max(analysis_birth_year)
    )
  ]
)

cat("\nSample by gender:\n")

print(
  analysis_dt[
    ,
    .N,
    by = .(
      subgroup = fifelse(
        female == 1,
        "Women",
        "Men"
      )
    )
  ]
)

# ==============================================================================
# STEP 10: Inspect outcome distributions
# ==============================================================================

outcome_summary <- rbindlist(
  lapply(
    c(
      "labor_force_participation",
      "employed",
      "unemployed"
    ),
    function(current_outcome) {
      
      analysis_dt[
        ,
        .(
          outcome = current_outcome,
          observations_nonmissing =
            sum(
              !is.na(
                get(current_outcome)
              )
            ),
          mean =
            mean(
              get(current_outcome),
              na.rm = TRUE
            )
        )
      ]
    }
  )
)

cat(
  "\nOutcome summary:\n"
)

print(outcome_summary)


# ==============================================================================
# STEP 11: Helper functions
# ==============================================================================

conversion_factor <- 100 * log(1.10)

significance_stars <- function(p_value) {
  
  fcase(
    is.na(p_value), "",
    p_value < 0.01, "***",
    p_value < 0.05, "**",
    p_value < 0.10, "*",
    default = ""
  )
}


estimate_employment_model <- function(
    data,
    outcome,
    subgroup
) {
  
  if (subgroup == "Pooled") {
    
    estimation_data <- data[
      !is.na(get(outcome))
    ]
    
    model_formula <- as.formula(
      paste0(
        outcome,
        " ~ log_undergrad_exposure_18_20 + female + urban | ",
        "province_historical + analysis_birth_year + year"
      )
    )
    
  } else if (subgroup == "Women") {
    
    estimation_data <- data[
      female == 1 &
        !is.na(get(outcome))
    ]
    
    model_formula <- as.formula(
      paste0(
        outcome,
        " ~ log_undergrad_exposure_18_20 + urban | ",
        "province_historical + analysis_birth_year + year"
      )
    )
    
  } else if (subgroup == "Men") {
    
    estimation_data <- data[
      female == 0 &
        !is.na(get(outcome))
    ]
    
    model_formula <- as.formula(
      paste0(
        outcome,
        " ~ log_undergrad_exposure_18_20 + urban | ",
        "province_historical + analysis_birth_year + year"
      )
    )
    
  } else {
    
    stop(
      paste0(
        "Unknown subgroup: ",
        subgroup
      )
    )
  }
  
  if (nrow(estimation_data) == 0L) {
    stop(
      paste0(
        "No observations available for outcome ",
        outcome,
        " and subgroup ",
        subgroup
      )
    )
  }
  
  model <- feols(
    model_formula,
    data = estimation_data,
    weights = ~weight,
    cluster =
      ~province_historical +
      analysis_birth_year,
    warn = TRUE,
    notes = TRUE
  )
  
  treatment_name <-
    "log_undergrad_exposure_18_20"
  
  coefficient <-
    unname(
      coef(model)[
        treatment_name
      ]
    )
  
  standard_error <-
    unname(
      se(model)[
        treatment_name
      ]
    )
  
  p_value <-
    unname(
      pvalue(model)[
        treatment_name
      ]
    )
  
  effect_pp <-
    conversion_factor *
    coefficient
  
  standard_error_pp <-
    conversion_factor *
    standard_error
  
  lower_95_pp <-
    effect_pp -
    1.96 *
    standard_error_pp
  
  upper_95_pp <-
    effect_pp +
    1.96 *
    standard_error_pp
  
  data.table(
    outcome = outcome,
    subgroup = subgroup,
    
    coefficient_log_exposure =
      coefficient,
    
    standard_error_log_exposure =
      standard_error,
    
    p_value =
      p_value,
    
    effect_pp_for_10_percent_increase =
      effect_pp,
    
    standard_error_pp =
      standard_error_pp,
    
    lower_95_pp =
      lower_95_pp,
    
    upper_95_pp =
      upper_95_pp,
    
    significance =
      significance_stars(
        p_value
      ),
    
    observations =
      nobs(model),
    
    provinces =
      uniqueN(
        estimation_data$province_historical
      ),
    
    cohorts =
      uniqueN(
        estimation_data$analysis_birth_year
      ),
    
    survey_years =
      uniqueN(
        estimation_data$year
      )
  )
}


# ==============================================================================
# STEP 12: Estimate all models
# ==============================================================================

outcomes <- c(
  "labor_force_participation",
  "employed",
  "unemployed"
)

subgroups <- c(
  "Pooled",
  "Women",
  "Men"
)

employment_results <- rbindlist(
  lapply(
    outcomes,
    function(current_outcome) {
      
      rbindlist(
        lapply(
          subgroups,
          function(current_subgroup) {
            
            cat(
              "\nEstimating:",
              current_outcome,
              "-",
              current_subgroup,
              "\n"
            )
            
            estimate_employment_model(
              data = analysis_dt,
              outcome = current_outcome,
              subgroup = current_subgroup
            )
          }
        )
      )
    }
  )
)


# ==============================================================================
# STEP 13: Add readable outcome labels
# ==============================================================================

employment_results[
  ,
  outcome_label := fcase(
    outcome ==
      "labor_force_participation",
    "Labor-force participation",
    
    outcome ==
      "employed",
    "Employment",
    
    outcome ==
      "unemployed",
    "Unemployment",
    
    default =
      outcome
  )
]

employment_results[
  ,
  subgroup_order := fcase(
    subgroup == "Pooled", 1L,
    subgroup == "Women", 2L,
    subgroup == "Men", 3L,
    default = 99L
  )
]

employment_results[
  ,
  outcome_order := fcase(
    outcome ==
      "labor_force_participation",
    1L,
    
    outcome ==
      "employed",
    2L,
    
    outcome ==
      "unemployed",
    3L,
    
    default = 99L
  )
]

setorder(
  employment_results,
  outcome_order,
  subgroup_order
)


# ==============================================================================
# STEP 14: Create formatted table entries
# ==============================================================================

employment_results[
  ,
  table_entry := paste0(
    formatC(
      effect_pp_for_10_percent_increase,
      format = "f",
      digits = 3
    ),
    significance,
    " (",
    formatC(
      standard_error_pp,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

employment_table_wide <- dcast(
  employment_results,
  outcome_label +
    outcome_order ~ subgroup,
  value.var = "table_entry"
)

setorder(
  employment_table_wide,
  outcome_order
)

employment_table_wide[
  ,
  outcome_order := NULL
]

setcolorder(
  employment_table_wide,
  c(
    "outcome_label",
    "Pooled",
    "Women",
    "Men"
  )
)

setnames(
  employment_table_wide,
  "outcome_label",
  "Outcome"
)


# ==============================================================================
# STEP 15: Print results
# ==============================================================================

cat(
  "\n============================================================\n",
  "EMPLOYMENT QUANTITY RESULTS\n",
  "Effects are percentage points for a 10% increase in exposure\n",
  "============================================================\n"
)

print(
  employment_results[
    ,
    .(
      outcome_label,
      subgroup,
      effect_pp_for_10_percent_increase,
      standard_error_pp,
      p_value,
      lower_95_pp,
      upper_95_pp,
      observations
    )
  ],
  nrows = Inf
)

cat(
  "\nFormatted employment table:\n"
)

print(
  employment_table_wide,
  nrows = Inf
)


# ==============================================================================
# STEP 16: Save outputs
# ==============================================================================

results_file <- file.path(
  output_folder,
  "employment_quantity_log_exposure_results.csv"
)

table_file <- file.path(
  output_folder,
  "employment_quantity_log_exposure_table.csv"
)

sample_file <- file.path(
  output_folder,
  "employment_quantity_sample_summary.csv"
)

fwrite(
  employment_results,
  results_file,
  bom = TRUE
)

fwrite(
  employment_table_wide,
  table_file,
  bom = TRUE
)

fwrite(
  outcome_summary,
  sample_file,
  bom = TRUE
)


# ==============================================================================
# STEP 17: Validate output files
# ==============================================================================

output_validation <- data.table(
  file = c(
    results_file,
    table_file,
    sample_file
  ),
  exists = file.exists(
    c(
      results_file,
      table_file,
      sample_file
    )
  )
)

cat(
  "\n============================================================\n",
  "OUTPUT VALIDATION\n",
  "============================================================\n"
)

print(output_validation)

if (any(output_validation$exists == FALSE)) {
  stop("One or more output files were not created.")
}


# ==============================================================================
# STEP 18: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "EMPLOYMENT MODELS COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nResults file:\n",
  results_file,
  "\n"
)

cat(
  "\nFormatted table file:\n",
  table_file,
  "\n"
)

cat(
  "\nSample summary file:\n",
  sample_file,
  "\n"
)