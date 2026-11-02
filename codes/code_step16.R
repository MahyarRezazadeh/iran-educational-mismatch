# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 16: Stronger Inference and Leave-One-Province-Out Robustness
#
# Main objectives:
#   1. Wild-cluster-bootstrap inference for the central paper results
#   2. Compare conventional two-way, province-only, and bootstrap inference
#   3. Examine sensitivity to removing each historical province
#   4. Replace unstable manual field omnibus tests with fixest::wald()
#
# Central outcomes:
#   A. Aggregate horizontal mismatch
#   B. Aggregate double mismatch
#   C. Aggregate fully matched employment
#   D. Male horizontal mismatch
#   E. Male double mismatch
#   F. Male fully matched employment
#   G. Business-and-law horizontal mismatch
#   H. Business-and-law double mismatch
#
# Treatment:
#   log(undergrad_exposure_18_20)
#
# Reported effect:
#   Percentage-point change associated with a 10% increase in exposure
#
# Baseline fixed effects:
#   Historical province
#   Corrected birth cohort
#   Survey year
#
# Baseline conventional inference:
#   Two-way clustering by historical province and birth cohort
#
# Wild bootstrap:
#   Clustid includes historical province and cohort
#   Bootstrap weights are drawn at the historical-province level
#   Webb bootstrap weights
#   Null imposed
#   9,999 bootstrap replications
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

required_packages <- c(
  "data.table",
  "fst",
  "fixest",
  "fwildclusterboot"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    FUN.VALUE = logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "The following required packages are not installed:\n",
      paste(
        missing_packages,
        collapse = ", "
      ),
      "\n\nInstall them using:\n",
      "install.packages(c(",
      paste0(
        '"',
        missing_packages,
        '"',
        collapse = ", "
      ),
      "))"
    )
  )
}

library(data.table)
library(fst)
library(fixest)
library(fwildclusterboot)


# ==============================================================================
# STEP 1: Paths
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
  "step16_stronger_inference"
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
# STEP 2: User-adjustable inference settings
# ==============================================================================

bootstrap_replications <- 9999L

bootstrap_seed <- 20260714L

bootstrap_weight_type <- "webb"

bootstrap_type <- "fnw11"

bootstrap_significance_level <- 0.05

# Webb weights are often useful with a relatively limited number of clusters.
# The script bootstraps at the historical-province level.

bootstrap_cluster_variable <- "historical_province"

conventional_cluster_variables <- c(
  "historical_province",
  "cohort"
)

effect_conversion <- 100 * log(1.10)


# ==============================================================================
# STEP 3: Read required variables
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
  
  # Employment and tertiary education
  "employed",
  "tertiary_educated",
  
  # Field of study
  "isced2013_broad_code",
  "education_field_harmonized",
  
  # Horizontal mismatch
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  
  # Vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final"
)

cat("\nReading Step 16 variables...\n")

dt <- as.data.table(
  read_fst(
    input_file,
    columns = variables_to_read
  )
)

cat(
  "\nObservations:",
  format(
    nrow(dt),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Variables:",
  ncol(dt),
  "\n"
)

cat(
  "Memory size:",
  format(
    object.size(dt),
    units = "GB"
  ),
  "\n"
)


# ==============================================================================
# STEP 4: Construct analytical variables
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
    log(
      undergrad_exposure_18_20
    ),
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
    dt[
      sample_main == TRUE,
      .N
    ],
    big.mark = ","
  ),
  "\n"
)

cat(
  "Historical provinces:",
  dt[
    sample_main == TRUE,
    uniqueN(
      historical_province
    )
  ],
  "\n"
)

cat(
  "Corrected cohorts:",
  dt[
    sample_main == TRUE,
    uniqueN(cohort)
  ],
  "\n"
)


# ==============================================================================
# STEP 5: Construct field groups
# ==============================================================================

dt[
  ,
  isced_broad_code_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(
          isced2013_broad_code
        )
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
# STEP 6: Fallback field classification from labels
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

business_field <- "04 Business, administration and law"

dt[
  ,
  field_group := factor(
    field_group,
    levels = field_levels
  )
]


# ==============================================================================
# STEP 7: Define mismatch samples and outcomes
# ==============================================================================

dt[
  ,
  horizontal_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(
      horizontal_mismatch_detailed_broad
    )
]

dt[
  ,
  vertical_eligible :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(undereducated_final) &
    !is.na(vertically_matched_final)
]

dt[
  ,
  joint_eligible :=
    horizontal_eligible == TRUE &
    vertical_eligible == TRUE
]

dt[
  ,
  double_mismatch := fifelse(
    joint_eligible,
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
    joint_eligible,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 8: Validate key samples
# ==============================================================================

sample_validation <- data.table(
  sample = c(
    "Main population",
    "Horizontal mismatch eligible",
    "Joint mismatch eligible",
    "Business horizontal eligible",
    "Business joint eligible"
  ),
  
  observations = c(
    dt[
      sample_main == TRUE,
      .N
    ],
    
    dt[
      horizontal_eligible == TRUE,
      .N
    ],
    
    dt[
      joint_eligible == TRUE,
      .N
    ],
    
    dt[
      horizontal_eligible == TRUE &
        as.character(field_group) == business_field,
      .N
    ],
    
    dt[
      joint_eligible == TRUE &
        as.character(field_group) == business_field,
      .N
    ]
  ),
  
  historical_provinces = c(
    dt[
      sample_main == TRUE,
      uniqueN(
        historical_province
      )
    ],
    
    dt[
      horizontal_eligible == TRUE,
      uniqueN(
        historical_province
      )
    ],
    
    dt[
      joint_eligible == TRUE,
      uniqueN(
        historical_province
      )
    ],
    
    dt[
      horizontal_eligible == TRUE &
        as.character(field_group) == business_field,
      uniqueN(
        historical_province
      )
    ],
    
    dt[
      joint_eligible == TRUE &
        as.character(field_group) == business_field,
      uniqueN(
        historical_province
      )
    ]
  ),
  
  cohorts = c(
    dt[
      sample_main == TRUE,
      uniqueN(cohort)
    ],
    
    dt[
      horizontal_eligible == TRUE,
      uniqueN(cohort)
    ],
    
    dt[
      joint_eligible == TRUE,
      uniqueN(cohort)
    ],
    
    dt[
      horizontal_eligible == TRUE &
        as.character(field_group) == business_field,
      uniqueN(cohort)
    ],
    
    dt[
      joint_eligible == TRUE &
        as.character(field_group) == business_field,
      uniqueN(cohort)
    ]
  )
)

print(sample_validation)

fwrite(
  sample_validation,
  file.path(
    output_folder,
    "step16_sample_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Define the eight priority models
# ==============================================================================

model_definitions <- data.table(
  model_id = c(
    "aggregate_horizontal_pooled",
    "aggregate_double_pooled",
    "aggregate_fully_matched_pooled",
    "aggregate_horizontal_men",
    "aggregate_double_men",
    "aggregate_fully_matched_men",
    "business_horizontal_pooled",
    "business_double_pooled"
  ),
  
  model_label = c(
    "Horizontal mismatch: pooled",
    "Double mismatch: pooled",
    "Fully matched employment: pooled",
    "Horizontal mismatch: men",
    "Double mismatch: men",
    "Fully matched employment: men",
    "Business and law horizontal mismatch: pooled",
    "Business and law double mismatch: pooled"
  ),
  
  outcome_variable = c(
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched_joint",
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched_joint",
    "horizontal_mismatch_detailed_broad",
    "double_mismatch"
  ),
  
  eligibility_variable = c(
    "horizontal_eligible",
    "joint_eligible",
    "joint_eligible",
    "horizontal_eligible",
    "joint_eligible",
    "joint_eligible",
    "horizontal_eligible",
    "joint_eligible"
  ),
  
  subgroup = c(
    "Pooled",
    "Pooled",
    "Pooled",
    "Men",
    "Men",
    "Men",
    "Pooled",
    "Pooled"
  ),
  
  field_restriction = c(
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    business_field,
    business_field
  )
)

fwrite(
  model_definitions,
  file.path(
    output_folder,
    "step16_model_definitions.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 10: Function to prepare a model-specific dataset
# ==============================================================================

prepare_model_data <- function(
    data,
    eligibility_variable,
    outcome_variable,
    subgroup,
    field_restriction = NA_character_,
    excluded_province = NA_character_
) {
  
  model_data <- data[
    get(eligibility_variable) == TRUE &
      !is.na(
        get(outcome_variable)
      )
  ]
  
  if (subgroup == "Women") {
    
    model_data <- model_data[
      gender == 1L
    ]
  }
  
  if (subgroup == "Men") {
    
    model_data <- model_data[
      gender == 2L
    ]
  }
  
  if (!is.na(field_restriction)) {
    
    model_data <- model_data[
      as.character(field_group) ==
        field_restriction
    ]
  }
  
  if (!is.na(excluded_province)) {
    
    model_data <- model_data[
      historical_province !=
        excluded_province
    ]
  }
  
  model_data
}


# ==============================================================================
# STEP 11: Baseline-model estimation function
# ==============================================================================

estimate_baseline_model <- function(
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
# STEP 12: Extract conventional inference
# ==============================================================================

extract_conventional_results <- function(
    model,
    model_id,
    model_label,
    model_data
) {
  
  treatment_term <- "log_undergrad_exposure"
  
  coefficient <- unname(
    coef(model)[treatment_term]
  )
  
  two_way_standard_error <- unname(
    se(
      model,
      vcov = ~historical_province + cohort
    )[treatment_term]
  )
  
  two_way_p_value <- unname(
    pvalue(
      model,
      vcov = ~historical_province + cohort
    )[treatment_term]
  )
  
  province_standard_error <- unname(
    se(
      model,
      vcov = ~historical_province
    )[treatment_term]
  )
  
  province_p_value <- unname(
    pvalue(
      model,
      vcov = ~historical_province
    )[treatment_term]
  )
  
  cohort_standard_error <- unname(
    se(
      model,
      vcov = ~cohort
    )[treatment_term]
  )
  
  cohort_p_value <- unname(
    pvalue(
      model,
      vcov = ~cohort
    )[treatment_term]
  )
  
  data.table(
    model_id = model_id,
    model_label = model_label,
    
    coefficient_log_scale =
      coefficient,
    
    effect_pp_for_10_percent_increase =
      effect_conversion *
      coefficient,
    
    two_way_standard_error_pp =
      effect_conversion *
      two_way_standard_error,
    
    two_way_p_value =
      two_way_p_value,
    
    province_cluster_standard_error_pp =
      effect_conversion *
      province_standard_error,
    
    province_cluster_p_value =
      province_p_value,
    
    cohort_cluster_standard_error_pp =
      effect_conversion *
      cohort_standard_error,
    
    cohort_cluster_p_value =
      cohort_p_value,
    
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
# STEP 13: Safe extraction from a boottest object
# ==============================================================================

extract_bootstrap_p_value <- function(
    bootstrap_object
) {
  
  result <- tryCatch(
    as.numeric(
      fwildclusterboot::pval(
        bootstrap_object
      )
    ),
    error = function(e) NA_real_
  )
  
  if (
    length(result) == 0L ||
    !is.finite(result[1])
  ) {
    return(NA_real_)
  }
  
  result[1]
}


extract_bootstrap_confidence_interval <- function(
    bootstrap_object
) {
  
  result <- tryCatch(
    confint(
      bootstrap_object
    ),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    
    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }
  
  numeric_values <- as.numeric(
    result
  )
  
  numeric_values <- numeric_values[
    is.finite(numeric_values)
  ]
  
  if (length(numeric_values) < 2L) {
    
    return(
      c(
        lower = NA_real_,
        upper = NA_real_
      )
    )
  }
  
  c(
    lower = numeric_values[1],
    upper = numeric_values[2]
  )
}


# ==============================================================================
# STEP 14: Wild-cluster-bootstrap function
#
# clustid:
#   Uses both province and cohort in the bootstrap variance calculation.
#
# bootcluster:
#   Draws bootstrap weights at the historical-province level.
#
# fe:
#   Must remain NULL because regression weights are used.
# ==============================================================================

run_wild_bootstrap <- function(
    model,
    model_id,
    model_label,
    bootstrap_seed_value
) {
  
  set.seed(
    bootstrap_seed_value
  )
  
  if (
    requireNamespace(
      "dqrng",
      quietly = TRUE
    )
  ) {
    
    dqrng::dqset.seed(
      bootstrap_seed_value
    )
  }
  
  bootstrap_result <- tryCatch(
    fwildclusterboot::boottest(
      object = model,
      param = "log_undergrad_exposure",
      B = bootstrap_replications,
      clustid = c(
        "historical_province",
        "cohort"
      ),
      bootcluster =
        bootstrap_cluster_variable,
      fe = NULL,
      sign_level =
        bootstrap_significance_level,
      conf_int = TRUE,
      type =
        bootstrap_weight_type,
      impose_null = TRUE,
      bootstrap_type =
        bootstrap_type,
      p_val_type = "two-tailed",
      nthreads = 1
    ),
    error = function(e) e
  )
  
  if (inherits(
    bootstrap_result,
    "error"
  )) {
    
    return(
      list(
        results = data.table(
          model_id = model_id,
          model_label = model_label,
          bootstrap_status = "Failed",
          bootstrap_error =
            bootstrap_result$message,
          bootstrap_p_value = NA_real_,
          bootstrap_lower_95_pp = NA_real_,
          bootstrap_upper_95_pp = NA_real_
        ),
        object = NULL
      )
    )
  }
  
  bootstrap_p_value <-
    extract_bootstrap_p_value(
      bootstrap_result
    )
  
  bootstrap_confidence_interval <-
    extract_bootstrap_confidence_interval(
      bootstrap_result
    )
  
  list(
    results = data.table(
      model_id = model_id,
      model_label = model_label,
      bootstrap_status = "Completed",
      bootstrap_error = NA_character_,
      
      bootstrap_p_value =
        bootstrap_p_value,
      
      bootstrap_lower_95_pp =
        effect_conversion *
        bootstrap_confidence_interval["lower"],
      
      bootstrap_upper_95_pp =
        effect_conversion *
        bootstrap_confidence_interval["upper"],
      
      bootstrap_replications =
        bootstrap_replications,
      
      bootstrap_weight_type =
        bootstrap_weight_type,
      
      bootstrap_type =
        bootstrap_type,
      
      bootstrap_cluster =
        bootstrap_cluster_variable,
      
      null_imposed = TRUE
    ),
    
    object = bootstrap_result
  )
}


# ==============================================================================
# STEP 15: Estimate baseline models and wild-bootstrap inference
# ==============================================================================

baseline_results_list <- list()
bootstrap_results_list <- list()
baseline_models <- list()
bootstrap_objects <- list()
failed_models <- list()

for (
  model_index in
  seq_len(
    nrow(model_definitions)
  )
) {
  
  current_definition <-
    model_definitions[
      model_index
    ]
  
  current_model_id <-
    current_definition$model_id
  
  current_model_label <-
    current_definition$model_label
  
  current_outcome <-
    current_definition$outcome_variable
  
  current_eligibility <-
    current_definition$eligibility_variable
  
  current_subgroup <-
    current_definition$subgroup
  
  current_field <-
    current_definition$field_restriction
  
  cat(
    "\n============================================================\n",
    "Estimating:",
    current_model_label,
    "\n",
    "============================================================\n"
  )
  
  model_data <- prepare_model_data(
    data = dt,
    eligibility_variable =
      current_eligibility,
    outcome_variable =
      current_outcome,
    subgroup =
      current_subgroup,
    field_restriction =
      current_field
  )
  
  cat(
    "Observations:",
    format(
      nrow(model_data),
      big.mark = ","
    ),
    "\n"
  )
  
  cat(
    "Province clusters:",
    uniqueN(
      model_data$historical_province
    ),
    "\n"
  )
  
  current_model <- tryCatch(
    estimate_baseline_model(
      model_data = model_data,
      outcome_variable =
        current_outcome,
      subgroup =
        current_subgroup
    ),
    error = function(e) e
  )
  
  if (inherits(
    current_model,
    "error"
  )) {
    
    failed_models[[
      length(failed_models) + 1L
    ]] <- data.table(
      model_id =
        current_model_id,
      model_label =
        current_model_label,
      stage =
        "Baseline estimation",
      error =
        current_model$message,
      observations =
        nrow(model_data)
    )
    
    next
  }
  
  baseline_models[[
    current_model_id
  ]] <- current_model
  
  baseline_results_list[[
    length(baseline_results_list) + 1L
  ]] <- extract_conventional_results(
    model =
      current_model,
    model_id =
      current_model_id,
    model_label =
      current_model_label,
    model_data =
      model_data
  )
  
  bootstrap_output <- run_wild_bootstrap(
    model =
      current_model,
    model_id =
      current_model_id,
    model_label =
      current_model_label,
    bootstrap_seed_value =
      bootstrap_seed +
      model_index
  )
  
  bootstrap_results_list[[
    length(bootstrap_results_list) + 1L
  ]] <- bootstrap_output$results
  
  if (
    !is.null(
      bootstrap_output$object
    )
  ) {
    
    bootstrap_objects[[
      current_model_id
    ]] <- bootstrap_output$object
  }
  
  rm(
    model_data,
    current_model,
    bootstrap_output
  )
  
  invisible(gc())
}


# ==============================================================================
# STEP 16: Combine conventional and bootstrap results
# ==============================================================================

conventional_results <- rbindlist(
  baseline_results_list,
  fill = TRUE
)

wild_bootstrap_results <- rbindlist(
  bootstrap_results_list,
  fill = TRUE
)

main_inference_results <- merge(
  conventional_results,
  wild_bootstrap_results,
  by = c(
    "model_id",
    "model_label"
  ),
  all = TRUE,
  sort = FALSE
)

main_inference_results[
  ,
  two_way_significance := fcase(
    two_way_p_value < 0.01, "***",
    two_way_p_value < 0.05, "**",
    two_way_p_value < 0.10, "*",
    default = ""
  )
]

main_inference_results[
  ,
  bootstrap_significance := fcase(
    bootstrap_p_value < 0.01, "***",
    bootstrap_p_value < 0.05, "**",
    bootstrap_p_value < 0.10, "*",
    default = ""
  )
]

main_inference_results[
  ,
  robust_at_10_percent :=
    !is.na(bootstrap_p_value) &
    bootstrap_p_value < 0.10
]

main_inference_results[
  ,
  robust_at_5_percent :=
    !is.na(bootstrap_p_value) &
    bootstrap_p_value < 0.05
]

main_inference_results[
  ,
  model_order := match(
    model_id,
    model_definitions$model_id
  )
]

setorder(
  main_inference_results,
  model_order
)

main_inference_results[
  ,
  model_order := NULL
]

print(
  main_inference_results[
    ,
    .(
      model_label,
      effect_pp =
        round(
          effect_pp_for_10_percent_increase,
          3
        ),
      two_way_p =
        round(
          two_way_p_value,
          4
        ),
      province_cluster_p =
        round(
          province_cluster_p_value,
          4
        ),
      bootstrap_p =
        round(
          bootstrap_p_value,
          4
        ),
      bootstrap_lower =
        round(
          bootstrap_lower_95_pp,
          3
        ),
      bootstrap_upper =
        round(
          bootstrap_upper_95_pp,
          3
        )
    )
  ]
)

fwrite(
  main_inference_results,
  file.path(
    output_folder,
    "step16_main_inference_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Save bootstrap objects
#
# These objects allow inspection of bootstrap distributions without rerunning.
# ==============================================================================

saveRDS(
  bootstrap_objects,
  file.path(
    output_folder,
    "step16_bootstrap_objects.rds"
  )
)


# ==============================================================================
# STEP 18: Leave-one-historical-province-out analysis
#
# This re-estimates every priority model after removing one province at a time.
# Conventional two-way-clustered inference is used for these repeated models.
#
# We focus primarily on:
#   - coefficient stability
#   - sign stability
#   - whether one province drives the result
# ==============================================================================

province_list <- sort(
  unique(
    dt[
      sample_main == TRUE,
      historical_province
    ]
  )
)

leave_one_out_results <- list()
leave_one_out_failures <- list()

loo_counter <- 1L
loo_failure_counter <- 1L

for (
  model_index in
  seq_len(
    nrow(model_definitions)
  )
) {
  
  current_definition <-
    model_definitions[
      model_index
    ]
  
  current_model_id <-
    current_definition$model_id
  
  current_model_label <-
    current_definition$model_label
  
  current_outcome <-
    current_definition$outcome_variable
  
  current_eligibility <-
    current_definition$eligibility_variable
  
  current_subgroup <-
    current_definition$subgroup
  
  current_field <-
    current_definition$field_restriction
  
  cat(
    "\n============================================================\n",
    "Leave-one-province-out:",
    current_model_label,
    "\n",
    "============================================================\n"
  )
  
  for (
    current_excluded_province in
    province_list
  ) {
    
    cat(
      "Excluding:",
      current_excluded_province,
      "\n"
    )
    
    model_data <- prepare_model_data(
      data = dt,
      eligibility_variable =
        current_eligibility,
      outcome_variable =
        current_outcome,
      subgroup =
        current_subgroup,
      field_restriction =
        current_field,
      excluded_province =
        current_excluded_province
    )
    
    current_model <- tryCatch(
      estimate_baseline_model(
        model_data =
          model_data,
        outcome_variable =
          current_outcome,
        subgroup =
          current_subgroup
      ),
      error = function(e) e
    )
    
    if (inherits(
      current_model,
      "error"
    )) {
      
      leave_one_out_failures[[
        loo_failure_counter
      ]] <- data.table(
        model_id =
          current_model_id,
        model_label =
          current_model_label,
        excluded_province =
          current_excluded_province,
        error =
          current_model$message,
        observations =
          nrow(model_data)
      )
      
      loo_failure_counter <-
        loo_failure_counter + 1L
      
      next
    }
    
    treatment_term <-
      "log_undergrad_exposure"
    
    coefficient <-
      unname(
        coef(
          current_model
        )[treatment_term]
      )
    
    standard_error <-
      unname(
        se(
          current_model
        )[treatment_term]
      )
    
    p_value <-
      unname(
        pvalue(
          current_model
        )[treatment_term]
      )
    
    leave_one_out_results[[
      loo_counter
    ]] <- data.table(
      model_id =
        current_model_id,
      
      model_label =
        current_model_label,
      
      excluded_province =
        current_excluded_province,
      
      effect_pp_for_10_percent_increase =
        effect_conversion *
        coefficient,
      
      standard_error_pp =
        effect_conversion *
        standard_error,
      
      p_value =
        p_value,
      
      lower_95_pp =
        effect_conversion *
        (
          coefficient -
            1.96 *
            standard_error
        ),
      
      upper_95_pp =
        effect_conversion *
        (
          coefficient +
            1.96 *
            standard_error
        ),
      
      observations =
        nobs(
          current_model
        ),
      
      remaining_province_clusters =
        uniqueN(
          model_data$historical_province
        ),
      
      remaining_cohort_clusters =
        uniqueN(
          model_data$cohort
        )
    )
    
    loo_counter <-
      loo_counter + 1L
    
    rm(
      model_data,
      current_model
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 19: Save full leave-one-out results
# ==============================================================================

leave_one_out_table <- rbindlist(
  leave_one_out_results,
  fill = TRUE
)

setorder(
  leave_one_out_table,
  model_id,
  excluded_province
)

fwrite(
  leave_one_out_table,
  file.path(
    output_folder,
    "step16_leave_one_province_out_results.csv"
  ),
  bom = TRUE
)

if (
  length(
    leave_one_out_failures
  ) > 0L
) {
  
  leave_one_out_failure_table <-
    rbindlist(
      leave_one_out_failures,
      fill = TRUE
    )
  
} else {
  
  leave_one_out_failure_table <-
    data.table(
      model_id = character(),
      model_label = character(),
      excluded_province = character(),
      error = character(),
      observations = integer()
    )
}

fwrite(
  leave_one_out_failure_table,
  file.path(
    output_folder,
    "step16_leave_one_out_failures.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Summarize leave-one-province-out stability
# ==============================================================================

baseline_effect_lookup <- main_inference_results[
  ,
  .(
    model_id,
    baseline_effect_pp =
      effect_pp_for_10_percent_increase
  )
]

leave_one_out_summary <- leave_one_out_table[
  ,
  .(
    leave_one_out_models = .N,
    
    minimum_effect_pp =
      min(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    maximum_effect_pp =
      max(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    mean_effect_pp =
      mean(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    median_effect_pp =
      median(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    standard_deviation_effect_pp =
      sd(
        effect_pp_for_10_percent_increase,
        na.rm = TRUE
      ),
    
    share_same_sign_as_full =
      NA_real_,
    
    share_p_below_010 =
      mean(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    share_p_below_005 =
      mean(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    province_with_smallest_effect =
      excluded_province[
        which.min(
          effect_pp_for_10_percent_increase
        )
      ],
    
    province_with_largest_effect =
      excluded_province[
        which.max(
          effect_pp_for_10_percent_increase
        )
      ]
  ),
  by = .(
    model_id,
    model_label
  )
]

leave_one_out_summary <- merge(
  leave_one_out_summary,
  baseline_effect_lookup,
  by = "model_id",
  all.x = TRUE,
  sort = FALSE
)

same_sign_summary <- leave_one_out_table[
  baseline_effect_lookup,
  on = "model_id"
][
  ,
  .(
    share_same_sign_as_full =
      mean(
        sign(
          effect_pp_for_10_percent_increase
        ) ==
          sign(
            baseline_effect_pp
          ),
        na.rm = TRUE
      )
  ),
  by = model_id
]

leave_one_out_summary[
  same_sign_summary,
  on = "model_id",
  share_same_sign_as_full :=
    i.share_same_sign_as_full
]

leave_one_out_summary[
  ,
  maximum_absolute_deviation_from_baseline :=
    pmax(
      abs(
        minimum_effect_pp -
          baseline_effect_pp
      ),
      abs(
        maximum_effect_pp -
          baseline_effect_pp
      )
    )
]

leave_one_out_summary[
  ,
  relative_maximum_deviation :=
    fifelse(
      abs(
        baseline_effect_pp
      ) > 0,
      maximum_absolute_deviation_from_baseline /
        abs(
          baseline_effect_pp
        ),
      NA_real_
    )
]

leave_one_out_summary[
  ,
  model_order := match(
    model_id,
    model_definitions$model_id
  )
]

setorder(
  leave_one_out_summary,
  model_order
)

leave_one_out_summary[
  ,
  model_order := NULL
]

print(leave_one_out_summary)

fwrite(
  leave_one_out_summary,
  file.path(
    output_folder,
    "step16_leave_one_province_out_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Identify influential province exclusions
# ==============================================================================

leave_one_out_differences <- leave_one_out_table[
  baseline_effect_lookup,
  on = "model_id"
]

leave_one_out_differences[
  ,
  deviation_from_baseline_pp :=
    effect_pp_for_10_percent_increase -
    baseline_effect_pp
]

leave_one_out_differences[
  ,
  absolute_deviation_pp :=
    abs(
      deviation_from_baseline_pp
    )
]

most_influential_exclusions <- leave_one_out_differences[
  order(
    model_id,
    -absolute_deviation_pp
  ),
  head(
    .SD,
    5L
  ),
  by = model_id
]

fwrite(
  most_influential_exclusions,
  file.path(
    output_folder,
    "step16_most_influential_province_exclusions.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Native fixest field-heterogeneity models
#
# These replace the unstable manually inverted covariance-matrix omnibus tests.
# The models use:
#   - a baseline exposure coefficient for Education
#   - field-by-exposure differences
#   - field-specific province, cohort, and year fixed effects
# ==============================================================================

dt[
  ,
  field_group_interaction :=
    relevel(
      field_group,
      ref = reference_field
    )
]

dt[
  ,
  province_field :=
    interaction(
      historical_province,
      field_group_interaction,
      drop = TRUE,
      lex.order = TRUE
    )
]

dt[
  ,
  cohort_field :=
    interaction(
      cohort,
      field_group_interaction,
      drop = TRUE,
      lex.order = TRUE
    )
]

dt[
  ,
  year_field :=
    interaction(
      year,
      field_group_interaction,
      drop = TRUE,
      lex.order = TRUE
    )
]


# ==============================================================================
# STEP 23: Define outcomes for native field Wald tests
# ==============================================================================

field_wald_definitions <- data.table(
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
    "horizontal_eligible",
    "joint_eligible",
    "joint_eligible",
    "vertical_eligible"
  )
)


# ==============================================================================
# STEP 24: Estimate native Wald tests
# ==============================================================================

native_wald_results <- list()
native_wald_failures <- list()

wald_counter <- 1L
wald_failure_counter <- 1L

for (
  outcome_index in
  seq_len(
    nrow(
      field_wald_definitions
    )
  )
) {
  
  current_outcome_label <-
    field_wald_definitions$outcome[
      outcome_index
    ]
  
  current_outcome_variable <-
    field_wald_definitions$outcome_variable[
      outcome_index
    ]
  
  current_eligibility_variable <-
    field_wald_definitions$eligibility_variable[
      outcome_index
    ]
  
  for (
    current_subgroup in
    c(
      "Pooled",
      "Women",
      "Men"
    )
  ) {
    
    cat(
      "\nNative field Wald test:",
      current_outcome_label,
      "|",
      current_subgroup,
      "\n"
    )
    
    model_data <- dt[
      get(
        current_eligibility_variable
      ) == TRUE &
        !is.na(
          get(
            current_outcome_variable
          )
        ) &
        !is.na(
          field_group_interaction
        )
    ]
    
    if (
      current_subgroup == "Women"
    ) {
      
      model_data <- model_data[
        gender == 1L
      ]
    }
    
    if (
      current_subgroup == "Men"
    ) {
      
      model_data <- model_data[
        gender == 2L
      ]
    }
    
    if (
      current_subgroup == "Pooled"
    ) {
      
      field_formula <- as.formula(
        paste0(
          current_outcome_variable,
          " ~ ",
          "log_undergrad_exposure + ",
          "i(",
          "field_group_interaction, ",
          "log_undergrad_exposure, ",
          "ref = '",
          reference_field,
          "') + ",
          "female + urban | ",
          "province_field + ",
          "cohort_field + ",
          "year_field"
        )
      )
      
    } else {
      
      field_formula <- as.formula(
        paste0(
          current_outcome_variable,
          " ~ ",
          "log_undergrad_exposure + ",
          "i(",
          "field_group_interaction, ",
          "log_undergrad_exposure, ",
          "ref = '",
          reference_field,
          "') + ",
          "urban | ",
          "province_field + ",
          "cohort_field + ",
          "year_field"
        )
      )
    }
    
    field_model <- tryCatch(
      feols(
        field_formula,
        data = model_data,
        weights = ~weight,
        cluster = ~historical_province + cohort,
        notes = FALSE,
        warn = TRUE
      ),
      error = function(e) e
    )
    
    if (inherits(
      field_model,
      "error"
    )) {
      
      native_wald_failures[[
        wald_failure_counter
      ]] <- data.table(
        outcome =
          current_outcome_label,
        subgroup =
          current_subgroup,
        stage =
          "Field model estimation",
        error =
          field_model$message,
        observations =
          nrow(model_data)
      )
      
      wald_failure_counter <-
        wald_failure_counter + 1L
      
      next
    }
    
    coefficient_names <- names(
      coef(
        field_model
      )
    )
    
    interaction_names <- coefficient_names[
      grepl(
        "field_group_interaction::",
        coefficient_names,
        fixed = TRUE
      ) &
        grepl(
          "log_undergrad_exposure",
          coefficient_names,
          fixed = TRUE
        )
    ]
    
    if (
      length(
        interaction_names
      ) == 0L
    ) {
      
      native_wald_failures[[
        wald_failure_counter
      ]] <- data.table(
        outcome =
          current_outcome_label,
        subgroup =
          current_subgroup,
        stage =
          "Coefficient selection",
        error =
          "No field-by-exposure interaction coefficients were found.",
        observations =
          nrow(model_data)
      )
      
      wald_failure_counter <-
        wald_failure_counter + 1L
      
      next
    }
    
    native_wald <- tryCatch(
      fixest::wald(
        field_model,
        keep =
          "field_group_interaction::",
        vcov =
          ~historical_province + cohort,
        print = FALSE
      ),
      error = function(e) e
    )
    
    if (inherits(
      native_wald,
      "error"
    )) {
      
      native_wald_failures[[
        wald_failure_counter
      ]] <- data.table(
        outcome =
          current_outcome_label,
        subgroup =
          current_subgroup,
        stage =
          "Native Wald test",
        error =
          native_wald$message,
        observations =
          nrow(model_data)
      )
      
      wald_failure_counter <-
        wald_failure_counter + 1L
      
      next
    }
    
    native_wald_results[[
      wald_counter
    ]] <- data.table(
      outcome =
        current_outcome_label,
      
      subgroup =
        current_subgroup,
      
      number_of_tested_interactions =
        length(
          interaction_names
        ),
      
      wald_statistic =
        unname(
          native_wald["stat"]
        ),
      
      wald_p_value =
        unname(
          native_wald["p"]
        ),
      
      numerator_degrees_of_freedom =
        unname(
          native_wald["df1"]
        ),
      
      denominator_degrees_of_freedom =
        unname(
          native_wald["df2"]
        ),
      
      observations =
        nobs(
          field_model
        ),
      
      historical_province_clusters =
        uniqueN(
          model_data$historical_province
        ),
      
      cohort_clusters =
        uniqueN(
          model_data$cohort
        )
    )
    
    wald_counter <-
      wald_counter + 1L
    
    rm(
      model_data,
      field_model,
      native_wald
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 25: Save native Wald results
# ==============================================================================

native_field_wald_tests <- rbindlist(
  native_wald_results,
  fill = TRUE
)

native_field_wald_tests[
  ,
  wald_p_value_fdr :=
    p.adjust(
      wald_p_value,
      method = "BH"
    )
]

setorder(
  native_field_wald_tests,
  outcome,
  subgroup
)

print(native_field_wald_tests)

fwrite(
  native_field_wald_tests,
  file.path(
    output_folder,
    "step16_native_field_wald_tests.csv"
  ),
  bom = TRUE
)

if (
  length(
    native_wald_failures
  ) > 0L
) {
  
  native_wald_failure_table <-
    rbindlist(
      native_wald_failures,
      fill = TRUE
    )
  
} else {
  
  native_wald_failure_table <-
    data.table(
      outcome = character(),
      subgroup = character(),
      stage = character(),
      error = character(),
      observations = integer()
    )
}

fwrite(
  native_wald_failure_table,
  file.path(
    output_folder,
    "step16_native_field_wald_failures.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Create compact inference table
# ==============================================================================

compact_inference_results <- main_inference_results[
  ,
  .(
    model_label,
    
    effect_pp =
      round(
        effect_pp_for_10_percent_increase,
        3
      ),
    
    conventional_two_way_se =
      round(
        two_way_standard_error_pp,
        3
      ),
    
    conventional_two_way_p =
      round(
        two_way_p_value,
        4
      ),
    
    province_cluster_p =
      round(
        province_cluster_p_value,
        4
      ),
    
    cohort_cluster_p =
      round(
        cohort_cluster_p_value,
        4
      ),
    
    wild_bootstrap_p =
      round(
        bootstrap_p_value,
        4
      ),
    
    bootstrap_lower_95 =
      round(
        bootstrap_lower_95_pp,
        3
      ),
    
    bootstrap_upper_95 =
      round(
        bootstrap_upper_95_pp,
        3
      ),
    
    conventional_significance =
      two_way_significance,
    
    bootstrap_significance =
      bootstrap_significance,
    
    observations,
    historical_province_clusters,
    cohort_clusters
  )
]

fwrite(
  compact_inference_results,
  file.path(
    output_folder,
    "step16_compact_inference_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Overall robustness classification
# ==============================================================================

robustness_summary <- merge(
  main_inference_results,
  leave_one_out_summary[
    ,
    .(
      model_id,
      leave_one_out_models,
      minimum_effect_pp,
      maximum_effect_pp,
      share_same_sign_as_full,
      share_p_below_010,
      share_p_below_005,
      maximum_absolute_deviation_from_baseline,
      relative_maximum_deviation
    )
  ],
  by = "model_id",
  all.x = TRUE,
  sort = FALSE
)

robustness_summary[
  ,
  inference_classification := fcase(
    bootstrap_p_value < 0.05 &
      share_same_sign_as_full == 1 &
      share_p_below_010 >= 0.80,
    "Strong",
    
    bootstrap_p_value < 0.10 &
      share_same_sign_as_full == 1 &
      share_p_below_010 >= 0.60,
    "Moderate",
    
    bootstrap_p_value < 0.10,
    "Bootstrap-supported but leave-one-out sensitive",
    
    share_same_sign_as_full == 1,
    "Sign-stable but bootstrap-insignificant",
    
    default =
      "Sensitive"
  )
]

robustness_summary[
  ,
  model_order := match(
    model_id,
    model_definitions$model_id
  )
]

setorder(
  robustness_summary,
  model_order
)

robustness_summary[
  ,
  model_order := NULL
]

fwrite(
  robustness_summary,
  file.path(
    output_folder,
    "step16_overall_robustness_summary.csv"
  ),
  bom = TRUE
)

print(
  robustness_summary[
    ,
    .(
      model_label,
      effect_pp =
        round(
          effect_pp_for_10_percent_increase,
          3
        ),
      conventional_p =
        round(
          two_way_p_value,
          4
        ),
      bootstrap_p =
        round(
          bootstrap_p_value,
          4
        ),
      leave_one_out_minimum =
        round(
          minimum_effect_pp,
          3
        ),
      leave_one_out_maximum =
        round(
          maximum_effect_pp,
          3
        ),
      same_sign_share =
        round(
          share_same_sign_as_full,
          3
        ),
      inference_classification
    )
  ]
)


# ==============================================================================
# STEP 28: Save all failure logs
# ==============================================================================

if (
  length(
    failed_models
  ) > 0L
) {
  
  baseline_failure_table <-
    rbindlist(
      failed_models,
      fill = TRUE
    )
  
} else {
  
  baseline_failure_table <-
    data.table(
      model_id = character(),
      model_label = character(),
      stage = character(),
      error = character(),
      observations = integer()
    )
}

fwrite(
  baseline_failure_table,
  file.path(
    output_folder,
    "step16_baseline_and_bootstrap_failures.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 29: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 16 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step16_main_inference_results.csv\n",
  "2. step16_compact_inference_results.csv\n",
  "3. step16_leave_one_province_out_results.csv\n",
  "4. step16_leave_one_province_out_summary.csv\n",
  "5. step16_most_influential_province_exclusions.csv\n",
  "6. step16_native_field_wald_tests.csv\n",
  "7. step16_overall_robustness_summary.csv\n",
  "8. step16_bootstrap_objects.rds\n",
  "9. step16_sample_validation.csv\n"
)

cat(
  "\nFailure logs:\n",
  "1. step16_baseline_and_bootstrap_failures.csv\n",
  "2. step16_leave_one_out_failures.csv\n",
  "3. step16_native_field_wald_failures.csv\n"
)

cat(
  "\nAll outputs were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nInterpretation priorities:\n",
  "- Use wild-bootstrap p-values for the main few-cluster inference check.\n",
  "- Use leave-one-out ranges to assess province influence.\n",
  "- Use native Wald p-values for joint field heterogeneity.\n",
  "- Do not rely on the previous manually calculated omnibus statistics.\n"
)






# ==============================================================================
# STEP 16B: Corrected province-level wild-bootstrap p-values
#
# This reruns only the bootstrap inference.
# Baseline models and leave-one-out estimates do not need to be re-estimated.
# ==============================================================================

library(data.table)
library(fwildclusterboot)
library(dqrng)

bootstrap_replications <- 9999L
bootstrap_seed <- 20260714L

corrected_bootstrap_results <- list()

for (model_index in seq_len(nrow(model_definitions))) {
  
  current_model_id <-
    model_definitions$model_id[model_index]
  
  current_model_label <-
    model_definitions$model_label[model_index]
  
  current_model <-
    baseline_models[[current_model_id]]
  
  cat(
    "\n============================================================\n",
    "Corrected wild bootstrap:",
    current_model_label,
    "\n",
    "============================================================\n"
  )
  
  if (is.null(current_model)) {
    
    corrected_bootstrap_results[[model_index]] <- data.table(
      model_id = current_model_id,
      model_label = current_model_label,
      bootstrap_status = "Baseline model unavailable",
      bootstrap_p_value = NA_real_,
      bootstrap_error = NA_character_
    )
    
    next
  }
  
  current_seed <-
    bootstrap_seed + model_index
  
  set.seed(current_seed)
  dqrng::dqset.seed(current_seed)
  
  bootstrap_object <- tryCatch(
    fwildclusterboot::boottest(
      object = current_model,
      
      param =
        "log_undergrad_exposure",
      
      B =
        bootstrap_replications,
      
      # Province-level inference for the few-cluster concern.
      clustid =
        "historical_province",
      
      bootcluster =
        "historical_province",
      
      # Required because the regression uses survey weights.
      fe =
        NULL,
      
      # Calculate the p-value first; avoid fragile CI inversion.
      conf_int =
        FALSE,
      
      type =
        "webb",
      
      impose_null =
        TRUE,
      
      bootstrap_type =
        "fnw11",
      
      p_val_type =
        "two-tailed",
      
      sampling =
        "dqrng",
      
      engine =
        "R",
      
      nthreads =
        1
    ),
    
    error = function(e) e
  )
  
  if (inherits(bootstrap_object, "error")) {
    
    corrected_bootstrap_results[[model_index]] <- data.table(
      model_id = current_model_id,
      model_label = current_model_label,
      bootstrap_status = "Failed",
      bootstrap_p_value = NA_real_,
      bootstrap_error = bootstrap_object$message
    )
    
    next
  }
  
  bootstrap_p <- tryCatch(
    as.numeric(
      fwildclusterboot::pval(
        bootstrap_object
      )
    )[1],
    
    error = function(e) NA_real_
  )
  
  corrected_bootstrap_results[[model_index]] <- data.table(
    model_id = current_model_id,
    model_label = current_model_label,
    bootstrap_status = "Completed",
    bootstrap_p_value = bootstrap_p,
    bootstrap_error = NA_character_,
    bootstrap_replications = bootstrap_replications,
    bootstrap_cluster = "historical_province",
    bootstrap_weight = "webb",
    null_imposed = TRUE
  )
}

corrected_bootstrap_table <- rbindlist(
  corrected_bootstrap_results,
  fill = TRUE
)

print(corrected_bootstrap_table)

fwrite(
  corrected_bootstrap_table,
  file.path(
    output_folder,
    "step16B_corrected_wild_bootstrap_results.csv"
  ),
  bom = TRUE
)





main_inference_corrected <- merge(
  conventional_results,
  
  corrected_bootstrap_table[
    ,
    .(
      model_id,
      bootstrap_status,
      bootstrap_p_value,
      bootstrap_error
    )
  ],
  
  by = "model_id",
  all.x = TRUE,
  sort = FALSE
)

main_inference_corrected[
  ,
  bootstrap_significance := fcase(
    bootstrap_p_value < 0.01, "***",
    bootstrap_p_value < 0.05, "**",
    bootstrap_p_value < 0.10, "*",
    default = ""
  )
]

main_inference_corrected[
  ,
  bootstrap_supported_10pct :=
    bootstrap_status == "Completed" &
    !is.na(bootstrap_p_value) &
    bootstrap_p_value < 0.10
]

main_inference_corrected[
  ,
  model_order := match(
    model_id,
    model_definitions$model_id
  )
]

setorder(
  main_inference_corrected,
  model_order
)

main_inference_corrected[
  ,
  model_order := NULL
]

print(
  main_inference_corrected[
    ,
    .(
      model_label,
      
      effect_pp =
        round(
          effect_pp_for_10_percent_increase,
          3
        ),
      
      conventional_two_way_p =
        round(
          two_way_p_value,
          4
        ),
      
      province_cluster_p =
        round(
          province_cluster_p_value,
          4
        ),
      
      wild_bootstrap_p =
        round(
          bootstrap_p_value,
          4
        ),
      
      bootstrap_status
    )
  ]
)

fwrite(
  main_inference_corrected,
  file.path(
    output_folder,
    "step16B_main_inference_corrected.csv"
  ),
  bom = TRUE
)



library(fwildclusterboot)

test_model <- baseline_models[["aggregate_horizontal_pooled"]]

set.seed(20260714)
dqrng::dqset.seed(20260714)

boot_test_1 <- tryCatch(
  fwildclusterboot::boottest(
    test_model,
    param = "log_undergrad_exposure",
    B = 999,
    clustid = "historical_province",
    conf_int = FALSE
  ),
  error = function(e) e
)

print(boot_test_1)

horizontal_data <- dt[
  horizontal_eligible == TRUE &
    !is.na(horizontal_mismatch_detailed_broad)
]

test_model_clean <- feols(
  horizontal_mismatch_detailed_broad ~
    log_undergrad_exposure +
    female +
    urban |
    historical_province +
    cohort +
    year,
  
  data = horizontal_data,
  weights = ~weight,
  notes = FALSE,
  warn = TRUE
)

set.seed(20260714)
dqrng::dqset.seed(20260714)

boot_test_clean <- tryCatch(
  fwildclusterboot::boottest(
    object = test_model_clean,
    param = "log_undergrad_exposure",
    B = 999,
    clustid = "historical_province",
    conf_int = FALSE,
    type = "rademacher",
    impose_null = TRUE,
    bootstrap_type = "fnw11",
    p_val_type = "two-tailed",
    engine = "R-lean",
    sampling = "standard",
    nthreads = 1
  ),
  error = function(e) e
)

print(boot_test_clean)



install.packages("lfe")
library(lfe)
library(fwildclusterboot)

horizontal_data <- dt[
  horizontal_eligible == TRUE &
    !is.na(horizontal_mismatch_detailed_broad)
]

test_model_felm <- felm(
  horizontal_mismatch_detailed_broad ~
    log_undergrad_exposure +
    female +
    urban |
    historical_province +
    cohort +
    year |
    0 |
    historical_province,
  
  data = horizontal_data,
  weights = horizontal_data$weight
)

set.seed(20260714)
dqrng::dqset.seed(20260714)

boot_test_felm <- tryCatch(
  fwildclusterboot::boottest(
    object = test_model_felm,
    param = "log_undergrad_exposure",
    B = 999,
    clustid = "historical_province",
    bootcluster = "historical_province",
    conf_int = FALSE,
    type = "rademacher",
    impose_null = TRUE,
    p_val_type = "two-tailed",
    fe = NULL
  ),
  error = function(e) e
)

print(boot_test_felm)