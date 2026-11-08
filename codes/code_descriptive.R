# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 6: Analysis Samples, Descriptive Statistics, and Treatment Variation
#
# Input:
#   lfs_final_analysis_ready.fst
#
# Main goals:
#   1. Confirm the final dataset and key variables.
#   2. Define the regression samples for each outcome.
#   3. Produce weighted descriptive statistics.
#   4. Examine university-exposure variation across provinces and cohorts.
#   5. Prepare the dataset for baseline regressions.
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
  "fixest",
  "ggplot2"
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
library(ggplot2)


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
  "step6_descriptives"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The final analysis dataset was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Read the final dataset
# ==============================================================================

cat("\nReading final analysis-ready dataset...\n")

dt <- as.data.table(
  read_fst(input_file)
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
# STEP 3: Confirm key variables
# ==============================================================================

key_variables <- c(
  # Survey structure
  "year",
  "weight",
  "age",
  "gender",
  
  # Geography and cohort
  "historical_province",
  "birth_year",
  
  # Treatment
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Labor-market outcomes
  "labor_force_participation",
  "employed",
  "unemployed",
  
  # Marriage outcomes
  "currently_married",
  "ever_married",
  
  # Controls
  "urban",
  "education_rank",
  
  # Final vertical mismatch outcomes
  "vertical_mismatch_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final",
  
  # Refined horizontal mismatch outcomes
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict",
  
  # Eligibility variables
  "vertical_fullperiod_eligible_final",
  "horizontal_detailed_eligible"
)

variable_check <- data.table(
  variable = key_variables,
  exists = key_variables %in% names(dt)
)

print(variable_check)

missing_key_variables <- variable_check[
  exists == FALSE,
  variable
]

if (length(missing_key_variables) > 0) {
  cat(
    "\nMissing variables:\n",
    paste(missing_key_variables, collapse = "\n"),
    "\n"
  )
}




# ==============================================================================
# STEP 3B: Find exact province and urban/rural variable names
# ==============================================================================

province_candidates <- names(dt)[
  grepl(
    "province|ostan|region|historical",
    names(dt),
    ignore.case = TRUE
  )
]

urban_candidates <- names(dt)[
  grepl(
    "urban|rural|residence|area",
    names(dt),
    ignore.case = TRUE
  )
]

cat("\nProvince-related variables:\n")
print(province_candidates)

cat("\nUrban/rural-related variables:\n")
print(urban_candidates)


for (v in province_candidates) {
  cat("\n============================\n")
  cat("Variable:", v, "\n")
  print(
    dt[
      ,
      .N,
      by = v
    ][
      order(-N)
    ][
      1:min(.N, 35)
    ]
  )
}

for (v in urban_candidates) {
  cat("\n============================\n")
  cat("Variable:", v, "\n")
  print(
    dt[
      ,
      .N,
      by = v
    ][
      order(-N)
    ][
      1:min(.N, 20)
    ]
  )
}







# ==============================================================================
# STEP 3C: Create standardized analysis variables
# ==============================================================================

# Historical province used for treatment assignment and fixed effects.
dt[
  ,
  historical_province := province_historical
]

# Binary urban indicator:
#   1 = Urban
#   0 = Rural
dt[
  ,
  urban := fcase(
    urban_rural == "Urban", 1L,
    urban_rural == "Rural", 0L,
    default = NA_integer_
  )
]

# Check the standardized variables.
cat("\nHistorical province distribution:\n")

print(
  dt[
    ,
    .N,
    by = historical_province
  ][
    order(historical_province)
  ]
)

cat("\nUrban indicator distribution:\n")

print(
  dt[
    ,
    .N,
    by = .(
      urban_rural,
      urban
    )
  ][
    order(urban)
  ]
)

# Confirm expected structure.
stopifnot(
  uniqueN(dt$historical_province) == 28L,
  sum(is.na(dt$historical_province)) == 0L,
  sum(is.na(dt$urban)) == 0L
)




# ==============================================================================
# STEP 4: Define analysis samples
#
# These are indicators only. No observations are dropped from dt.
# ==============================================================================


# ------------------------------------------------------------------------------
# 4.1 Valid survey-weight indicator
# ------------------------------------------------------------------------------

dt[
  ,
  valid_weight :=
    as.integer(
      !is.na(weight) &
        is.finite(weight) &
        weight > 0
    )
]


# ------------------------------------------------------------------------------
# 4.2 Complete treatment-exposure sample
# ------------------------------------------------------------------------------

dt[
  ,
  sample_complete_exposure :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(undergrad_exposure_18_20)
    )
]


# ------------------------------------------------------------------------------
# 4.3 Main general-outcome sample
#
# Used for:
#   - Labor-force participation
#   - Employment
#   - Marriage
# ------------------------------------------------------------------------------

dt[
  ,
  sample_main :=
    as.integer(
      sample_complete_exposure == 1L &
        valid_weight == 1L &
        !is.na(historical_province) &
        !is.na(birth_year) &
        !is.na(year) &
        !is.na(age) &
        !is.na(gender) &
        !is.na(urban)
    )
]


# ------------------------------------------------------------------------------
# 4.4 Labor-force participation sample
# ------------------------------------------------------------------------------

dt[
  ,
  sample_lfp :=
    as.integer(
      sample_main == 1L &
        !is.na(labor_force_participation)
    )
]


# ------------------------------------------------------------------------------
# 4.5 Employment sample
#
# Baseline employment outcome is defined for the complete main sample.
# ------------------------------------------------------------------------------

dt[
  ,
  sample_employment :=
    as.integer(
      sample_main == 1L &
        !is.na(employed)
    )
]


# ------------------------------------------------------------------------------
# 4.6 Unemployment sample
#
# Unemployment is interpreted among labor-force participants.
# ------------------------------------------------------------------------------

dt[
  ,
  sample_unemployment :=
    as.integer(
      sample_main == 1L &
        labor_force_participation == 1L &
        !is.na(unemployed)
    )
]


# ------------------------------------------------------------------------------
# 4.7 Marriage samples
# ------------------------------------------------------------------------------

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


# ------------------------------------------------------------------------------
# 4.8 Final vertical-mismatch sample
#
# Defined among employed workers with valid occupation and education data.
# ------------------------------------------------------------------------------

dt[
  ,
  sample_vertical_mismatch :=
    as.integer(
      sample_main == 1L &
        vertical_fullperiod_eligible_final == 1L &
        !is.na(vertical_mismatch_final)
    )
]


# ------------------------------------------------------------------------------
# 4.9 Horizontal-mismatch sample
#
# Defined among tertiary-educated employed workers with classifiable fields.
# ------------------------------------------------------------------------------

dt[
  ,
  sample_horizontal_mismatch :=
    as.integer(
      sample_main == 1L &
        horizontal_detailed_eligible == 1L &
        !is.na(horizontal_mismatch_detailed_broad)
    )
]


# ------------------------------------------------------------------------------
# 4.10 Non-migrant robustness sample
#
# Respondents explicitly identified as recent cross-region movers are removed.
# Missing values generally indicate no recent migration record, not missing
# province information.
# ------------------------------------------------------------------------------

dt[
  ,
  sample_no_recent_cross_region_move :=
    as.integer(
      sample_main == 1L &
        (
          is.na(recent_cross_historical_region_move) |
            recent_cross_historical_region_move == 0L
        )
    )
]


# ==============================================================================
# STEP 5: Summarize analysis-sample sizes
# ==============================================================================

sample_summary <- data.table(
  sample = c(
    "Full final dataset",
    "Complete exposure",
    "Main analysis sample",
    "Labor-force participation",
    "Employment",
    "Unemployment among labor-force participants",
    "Currently married",
    "Ever married",
    "Final vertical mismatch",
    "Detailed horizontal mismatch",
    "Main sample excluding recent cross-region movers"
  ),
  
  observations = c(
    nrow(dt),
    
    sum(
      dt$sample_complete_exposure == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_main == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_lfp == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_employment == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_unemployment == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_currently_married == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_ever_married == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_vertical_mismatch == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_horizontal_mismatch == 1L,
      na.rm = TRUE
    ),
    
    sum(
      dt$sample_no_recent_cross_region_move == 1L,
      na.rm = TRUE
    )
  )
)

sample_summary[
  ,
  percent_of_full_dataset :=
    round(
      100 * observations / nrow(dt),
      2
    )
]

print(sample_summary)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "step6_sample_summary.csv"
  ),
  bom = TRUE
)











# ==============================================================================
# STEP 6: Efficient weighted descriptive statistics
# ==============================================================================

# Weighted standard deviation without copying the full dataset repeatedly.
weighted_sd_safe <- function(x, w) {
  
  valid <- !is.na(x) &
    !is.na(w) &
    is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (sum(valid) <= 1L) {
    return(NA_real_)
  }
  
  x_valid <- x[valid]
  w_valid <- w[valid]
  
  weighted_mean <- sum(
    x_valid * w_valid
  ) / sum(w_valid)
  
  sqrt(
    sum(
      w_valid * (x_valid - weighted_mean)^2
    ) /
      sum(w_valid)
  )
}


# Keep only the main sample once.
dt_main <- dt[
  sample_main == 1L,
  .(
    weight,
    age,
    undergrad_exposure_18_20,
    labor_force_participation,
    employed,
    currently_married,
    ever_married,
    urban
  )
]

cat(
  "\nMain sample loaded:",
  format(nrow(dt_main), big.mark = ","),
  "observations\n"
)


main_descriptive_variables <- c(
  "age",
  "undergrad_exposure_18_20",
  "labor_force_participation",
  "employed",
  "currently_married",
  "ever_married",
  "urban"
)


main_descriptives_list <- vector(
  mode = "list",
  length = length(main_descriptive_variables)
)


for (i in seq_along(main_descriptive_variables)) {
  
  v <- main_descriptive_variables[i]
  
  cat(
    "Processing:",
    v,
    "\n"
  )
  
  x <- dt_main[[v]]
  w <- dt_main$weight
  
  valid <- !is.na(x) &
    !is.na(w) &
    is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (sum(valid) == 0L) {
    
    main_descriptives_list[[i]] <- data.table(
      variable = v,
      observations = 0L,
      weighted_mean = NA_real_,
      weighted_sd = NA_real_,
      minimum = NA_real_,
      maximum = NA_real_
    )
    
  } else {
    
    main_descriptives_list[[i]] <- data.table(
      variable = v,
      
      observations = sum(valid),
      
      weighted_mean = weighted.mean(
        x[valid],
        w[valid]
      ),
      
      weighted_sd = weighted_sd_safe(
        x,
        w
      ),
      
      minimum = min(
        x[valid]
      ),
      
      maximum = max(
        x[valid]
      )
    )
  }
  
  rm(x, w, valid)
  invisible(gc())
}


main_descriptives <- rbindlist(
  main_descriptives_list
)

print(main_descriptives)


fwrite(
  main_descriptives,
  file.path(
    output_folder,
    "step6_main_weighted_descriptives.csv"
  ),
  bom = TRUE
)


cat(
  "\nStep 6 completed successfully.\n"
)






# ==============================================================================
# STEP 7: Outcome-specific weighted descriptive statistics
# ==============================================================================

# Helper function for a safe weighted mean.
weighted_mean_safe <- function(x, w) {
  
  valid <- !is.na(x) &
    !is.na(w) &
    is.finite(x) &
    is.finite(w) &
    w > 0
  
  if (sum(valid) == 0L) {
    return(NA_real_)
  }
  
  sum(
    x[valid] * w[valid]
  ) / sum(w[valid])
}


# Define each outcome and its correct analysis sample.
outcome_summary <- data.table(
  
  outcome = c(
    "Labor-force participation",
    "Employment",
    "Unemployment among labor-force participants",
    "Currently married",
    "Ever married",
    "Vertical mismatch",
    "Overeducated",
    "Undereducated",
    "Vertically matched",
    "Horizontal mismatch broad",
    "Horizontal mismatch strict"
  ),
  
  variable = c(
    "labor_force_participation",
    "employed",
    "unemployed",
    "currently_married",
    "ever_married",
    "vertical_mismatch_final",
    "overeducated_final",
    "undereducated_final",
    "vertically_matched_final",
    "horizontal_mismatch_detailed_broad",
    "horizontal_mismatch_detailed_strict"
  ),
  
  sample_indicator = c(
    "sample_lfp",
    "sample_employment",
    "sample_unemployment",
    "sample_currently_married",
    "sample_ever_married",
    "sample_vertical_mismatch",
    "sample_vertical_mismatch",
    "sample_vertical_mismatch",
    "sample_vertical_mismatch",
    "sample_horizontal_mismatch",
    "sample_horizontal_mismatch"
  )
)


# Prepare an empty list for results.
outcome_descriptives_list <- vector(
  mode = "list",
  length = nrow(outcome_summary)
)


for (i in seq_len(nrow(outcome_summary))) {
  
  outcome_name <- outcome_summary$outcome[i]
  variable_name <- outcome_summary$variable[i]
  sample_name <- outcome_summary$sample_indicator[i]
  
  cat(
    "Processing outcome:",
    outcome_name,
    "\n"
  )
  
  sample_index <- dt[[sample_name]] == 1L
  
  x <- dt[[variable_name]][sample_index]
  w <- dt$weight[sample_index]
  
  valid <- !is.na(x) &
    !is.na(w) &
    is.finite(x) &
    is.finite(w) &
    w > 0
  
  outcome_descriptives_list[[i]] <- data.table(
    outcome = outcome_name,
    variable = variable_name,
    sample = sample_name,
    
    observations = sum(valid),
    
    weighted_mean = weighted_mean_safe(
      x,
      w
    ),
    
    unweighted_mean = if (sum(!is.na(x)) > 0L) {
      mean(
        x,
        na.rm = TRUE
      )
    } else {
      NA_real_
    }
  )
  
  rm(
    sample_index,
    x,
    w,
    valid
  )
  
  invisible(gc())
}


outcome_descriptives <- rbindlist(
  outcome_descriptives_list
)

print(outcome_descriptives)


fwrite(
  outcome_descriptives,
  file.path(
    output_folder,
    "step6_outcome_weighted_descriptives.csv"
  ),
  bom = TRUE
)

cat(
  "\nStep 7 completed successfully.\n"
)








# ==============================================================================
# STEP 8: Weighted outcome rates by gender
# ==============================================================================

# Confirm gender coding:
#   1 = Female
#   2 = Male

gender_labels <- data.table(
  gender = c(1, 2),
  gender_label = c(
    "Female",
    "Male"
  )
)


# Prepare storage for gender-specific results.
gender_results_list <- vector(
  mode = "list",
  length = nrow(outcome_summary)
)


for (i in seq_len(nrow(outcome_summary))) {
  
  outcome_name <- outcome_summary$outcome[i]
  variable_name <- outcome_summary$variable[i]
  sample_name <- outcome_summary$sample_indicator[i]
  
  cat(
    "Processing by gender:",
    outcome_name,
    "\n"
  )
  
  gender_results_list[[i]] <- rbindlist(
    lapply(
      c(1L, 2L),
      function(g) {
        
        sample_index <-
          dt[[sample_name]] == 1L &
          dt$gender == g
        
        x <- dt[[variable_name]][sample_index]
        w <- dt$weight[sample_index]
        
        valid <- !is.na(x) &
          !is.na(w) &
          is.finite(x) &
          is.finite(w) &
          w > 0
        
        result <- data.table(
          outcome = outcome_name,
          variable = variable_name,
          gender = g,
          
          observations = sum(valid),
          
          weighted_mean = weighted_mean_safe(
            x,
            w
          ),
          
          unweighted_mean = if (sum(!is.na(x)) > 0L) {
            mean(
              x,
              na.rm = TRUE
            )
          } else {
            NA_real_
          }
        )
        
        rm(
          sample_index,
          x,
          w,
          valid
        )
        
        result
      }
    )
  )
  
  invisible(gc())
}


outcomes_by_gender <- rbindlist(
  gender_results_list
)


outcomes_by_gender[
  gender_labels,
  on = "gender",
  gender_label := i.gender_label
]


setcolorder(
  outcomes_by_gender,
  c(
    "outcome",
    "variable",
    "gender",
    "gender_label",
    "observations",
    "weighted_mean",
    "unweighted_mean"
  )
)


setorder(
  outcomes_by_gender,
  outcome,
  gender
)


print(outcomes_by_gender)


fwrite(
  outcomes_by_gender,
  file.path(
    output_folder,
    "step6_outcomes_by_gender.csv"
  ),
  bom = TRUE
)


cat(
  "\nStep 8 completed successfully.\n"
)