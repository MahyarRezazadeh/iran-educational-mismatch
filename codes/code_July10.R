# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 1: Create a Memory-Efficient Reduced LFS Dataset
#
# This script starts from the original large LFS Stata dataset.
#
# Main objectives:
#   1. Read only variables that may be needed for the paper.
#   2. Avoid unnecessary copies of the full dataset.
#   3. Preserve variables needed for:
#        - University-exposure assignment
#        - Labor-force participation
#        - Employment and unemployment
#        - Marriage
#        - Migration analysis
#        - Vertical mismatch
#        - Horizontal mismatch
#        - Demographic controls
#        - Survey weights
#   4. Create essential diagnostic tables.
#   5. Save one reduced dataset in FST format.
#
# IMPORTANT:
#   - Restart R before running this script.
#   - Do not run the previous script first.
#   - Do not save an RDS copy of the dataset.
#   - Run the complete script from the beginning.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear the R environment
# ==============================================================================

# Remove objects left from previous runs.
rm(list = ls())

# Ask R to release unused memory.
invisible(gc())

# Avoid scientific notation where possible.
options(scipen = 999)

# Limit data.table printing.
options(datatable.print.nrows = 50)


# ==============================================================================
# STEP 1: Install and load required packages
# ==============================================================================

required_packages <- c(
  "haven",       # Read the original Stata file
  "data.table",  # Efficient data processing
  "fst",         # Efficient storage of the reduced dataset
  "stringr"      # Basic text cleaning
)

# Identify packages that are not currently installed.
missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

# Install missing packages only.
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load required packages.
library(haven)
library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 2: Define file paths
# ==============================================================================

# Main project folder.
project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

# Original large LFS Stata file.
input_file <- file.path(
  project_folder,
  "data_ready.dta"
)

# Folder in which the reduced data and diagnostics will be saved.
output_folder <- file.path(
  project_folder,
  "LFS_reduced_outputs"
)

# Create the output folder if it does not exist.
if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}

# Confirm that the original LFS file exists.
if (!file.exists(input_file)) {
  stop(
    paste0(
      "The original LFS file was not found at:\n",
      input_file,
      "\n\nCheck the file name and project path."
    )
  )
}

cat(
  "\nOriginal LFS file found:\n",
  input_file,
  "\n"
)


# ==============================================================================
# STEP 3: Define variables to retain
#
# The list is intentionally broad enough to support the current paper and
# likely robustness exercises, but it excludes many unnecessary variables.
# ==============================================================================


# ------------------------------------------------------------------------------
# 3.1 Survey identification and design
# ------------------------------------------------------------------------------

survey_variables <- c(
  "id",
  "quarter",
  "year",
  "member_number",
  "weight"
)


# ------------------------------------------------------------------------------
# 3.2 Current geographic location
#
# province is needed to assign province-level university exposure.
# urban_rural will be used as a demographic and geographic control.
# ------------------------------------------------------------------------------

geographic_variables <- c(
  "province",
  "urban_rural"
)


# ------------------------------------------------------------------------------
# 3.3 Demographic characteristics
# ------------------------------------------------------------------------------

demographic_variables <- c(
  "birth_month",
  "birth_year",
  "age",
  "gender",
  "mar_status",
  "rel_hh",
  "nationality"
)


# ------------------------------------------------------------------------------
# 3.4 Migration and residence information
#
# These variables may help identify recent movers and their previous province.
# We retain all available migration variables before deciding on the final
# migration treatment.
# ------------------------------------------------------------------------------

migration_variables <- c(
  "previous_residence_location",
  "residence_change_reason",
  "previous_residence_province",
  "previous_residence_country",
  "residence_status",
  "residence_duration"
)


# ------------------------------------------------------------------------------
# 3.5 Education variables
#
# education_level is a readable education category.
# educ_level contains the corresponding numerical education code.
# education_field is required for horizontal mismatch.
# ------------------------------------------------------------------------------

education_variables <- c(
  "student",
  "literate",
  "education_level",
  "educ_level",
  "education_field"
)


# ------------------------------------------------------------------------------
# 3.6 Core labor-force status variables
#
# employment_status is the main cleaned employment-status variable.
# The underlying questions are retained to validate employment, unemployment,
# and labor-force participation.
# ------------------------------------------------------------------------------

labor_force_variables <- c(
  "employment_status",
  "situation_last_week",
  "worked_last_week",
  "worked_nonconventional_last_week",
  "worked_unpaid_family_business_la",
  "had_job_but_absent_last_week",
  "reason_for_job_absence_last_week",
  "wants_to_have_job_or_occupation",
  "took_action_to_search_for_job_la",
  "reason_not_job_searching_last_4_",
  "ready_to_start_job_if_found_last",
  "ever_worked_before",
  "currently_receiving_unemployment"
)


# ------------------------------------------------------------------------------
# 3.7 Main-job characteristics
#
# Occupation and industry codes are required for mismatch analysis.
# Hours, experience, insurance, and employment type may be useful as secondary
# outcomes or controls.
# ------------------------------------------------------------------------------

main_job_variables <- c(
  "main_job_title_isco_code",
  "main_job_workplace_isic_code",
  "main_job_employment_status",
  "main_job_workplace_employee_coun",
  "workplace_size",
  "has_health_insurnace",
  "main_job_duration_years",
  "main_job_duration_months",
  "total_work_experience_years",
  "total_work_experience_months",
  "main_job_days_per_week",
  "main_job_hours_per_week",
  "all_jobs_days_per_week",
  "all_jobs_hours_per_week",
  "last_week_work_hours_total"
)


# ------------------------------------------------------------------------------
# 3.8 Previous job and job-search information
#
# These variables may be useful for unemployment analysis and additional
# labor-market outcomes.
# ------------------------------------------------------------------------------

job_search_variables <- c(
  "job_search_duration_years",
  "job_search_duration_months",
  "time_since_last_job_years",
  "time_since_last_job_months",
  "last_job_title_isco_code",
  "last_job_workplace_isic_code",
  "last_job_employment_status",
  "reason_for_leaving_last_job",
  "situation_before_wanting_to_work",
  "reason_not_ready_for_work",
  "preferred_employment_status",
  "preferred_work_sector"
)


# ------------------------------------------------------------------------------
# 3.9 Secondary-job information
# ------------------------------------------------------------------------------

second_job_variables <- c(
  "had_other_job_last_week",
  "second_job_title_isco_code",
  "second_job_workplace_isic_code",
  "second_job_employment_status"
)


# ------------------------------------------------------------------------------
# 3.10 Combine all variable groups
# ------------------------------------------------------------------------------

requested_variables <- unique(
  c(
    survey_variables,
    geographic_variables,
    demographic_variables,
    migration_variables,
    education_variables,
    labor_force_variables,
    main_job_variables,
    job_search_variables,
    second_job_variables
  )
)

cat(
  "\nNumber of requested variables:",
  length(requested_variables),
  "\n"
)


# ==============================================================================
# STEP 4: Inspect the original file structure without loading observations
#
# n_max = 0 reads only the column structure. This requires little memory.
# ==============================================================================

cat(
  "\nChecking the variable names in the original LFS file...\n"
)

original_structure <- haven::read_dta(
  input_file,
  n_max = 0
)

available_variables <- names(original_structure)

cat(
  "Number of columns in the original file:",
  length(available_variables),
  "\n"
)


# ==============================================================================
# STEP 5: Check whether requested variables exist
# ==============================================================================

# Variables requested but not found in the original file.
variables_not_found <- setdiff(
  requested_variables,
  available_variables
)

# Variables that exist and can be read.
variables_to_read <- intersect(
  requested_variables,
  available_variables
)

cat(
  "\nNumber of requested variables found:",
  length(variables_to_read),
  "\n"
)

if (length(variables_not_found) > 0) {
  
  cat(
    "\nThe following variables were not found and will be skipped:\n"
  )
  
  print(variables_not_found)
}


# ------------------------------------------------------------------------------
# Save the variable-selection record.
# ------------------------------------------------------------------------------

variable_selection_table <- data.table(
  variable_name = requested_variables,
  found_in_original_file =
    requested_variables %in% available_variables
)

fwrite(
  variable_selection_table,
  file.path(
    output_folder,
    "selected_variable_list.csv"
  ),
  bom = TRUE
)


# Remove the temporary zero-row structure object.
rm(original_structure)

invisible(gc())


# ==============================================================================
# STEP 6: Read only the selected variables
#
# This is the most memory-intensive part of the script.
#
# Do not open other large datasets in R while this section is running.
# ==============================================================================

cat(
  "\nReading only the selected variables from data_ready.dta...\n"
)

lfs <- haven::read_dta(
  input_file,
  col_select = all_of(variables_to_read)
)

# Convert the object from a tibble to data.table by reference.
setDT(lfs)

cat(
  "\nReduced LFS dataset loaded successfully.\n"
)

cat(
  "Number of observations:",
  format(nrow(lfs), big.mark = ","),
  "\n"
)

cat(
  "Number of variables:",
  ncol(lfs),
  "\n"
)

# Request memory cleanup after the import.
invisible(gc())


# ==============================================================================
# STEP 7: Remove unnecessary Stata metadata
#
# Stata labels and other attributes can increase memory use.
#
# The uploaded sample indicates that important readable variables such as
# province, education_level, urban_rural, and employment_status are already
# stored as readable character variables.
#
# We therefore remove Stata-specific metadata instead of creating duplicate
# label columns.
# ==============================================================================

cat(
  "\nRemoving unnecessary Stata metadata...\n"
)

for (variable_name in names(lfs)) {
  
  # Remove variable-level Stata labels.
  attr(lfs[[variable_name]], "label") <- NULL
  
  # Remove value-label metadata while preserving the stored values.
  if (haven::is.labelled(lfs[[variable_name]])) {
    lfs[
      ,
      (variable_name) :=
        haven::zap_labels(
          get(variable_name)
        )
    ]
  }
}

# Remove dataset-level attributes that are no longer required.
attributes(lfs)$label <- NULL

invisible(gc())


# ==============================================================================
# STEP 8: Standardize basic storage types
#
# Converting logical columns to integers can improve consistency when saving
# and reading the FST file.
# ==============================================================================

logical_variables <- names(lfs)[
  vapply(
    lfs,
    is.logical,
    logical(1)
  )
]

if (length(logical_variables) > 0) {
  
  for (variable_name in logical_variables) {
    
    set(
      lfs,
      j = variable_name,
      value = as.integer(
        lfs[[variable_name]]
      )
    )
  }
}


# ==============================================================================
# STEP 9: Perform minimal character cleaning
#
# Only trim leading and trailing spaces.
#
# We do not create lowercase duplicates because that would increase the dataset
# size. More extensive cleaning will be performed later on selected variables.
# ==============================================================================

character_variables <- names(lfs)[
  vapply(
    lfs,
    is.character,
    logical(1)
  )
]

if (length(character_variables) > 0) {
  
  for (variable_name in character_variables) {
    
    cleaned_values <- str_trim(
      lfs[[variable_name]]
    )
    
    # Convert blank strings to missing values.
    cleaned_values[
      cleaned_values == ""
    ] <- NA_character_
    
    # Replace the original column without creating a permanent duplicate.
    set(
      lfs,
      j = variable_name,
      value = cleaned_values
    )
    
    rm(cleaned_values)
  }
}

invisible(gc())


# ==============================================================================
# STEP 10: Perform basic integrity checks
# ==============================================================================


# ------------------------------------------------------------------------------
# 10.1 Check the survey-year range
# ------------------------------------------------------------------------------

cat(
  "\nSurvey-year range:",
  min(lfs$year, na.rm = TRUE),
  "to",
  max(lfs$year, na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------------------------
# 10.2 Check the age range
# ------------------------------------------------------------------------------

cat(
  "Age range:",
  min(lfs$age, na.rm = TRUE),
  "to",
  max(lfs$age, na.rm = TRUE),
  "\n"
)


# ------------------------------------------------------------------------------
# 10.3 Check whether the survey weight is valid
# ------------------------------------------------------------------------------

cat(
  "Missing survey weights:",
  sum(is.na(lfs$weight)),
  "\n"
)

cat(
  "Nonpositive survey weights:",
  sum(
    lfs$weight <= 0,
    na.rm = TRUE
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 10.4 Check duplicated full rows
#
# We do not run duplicated(lfs), because checking all columns across more than
# 11 million rows would be unnecessarily memory intensive.
#
# Instead, we report duplicate counts for the available identifier structure.
# ------------------------------------------------------------------------------

possible_id_variables <- intersect(
  c(
    "id",
    "member_number",
    "year",
    "quarter"
  ),
  names(lfs)
)

if (length(possible_id_variables) > 0) {
  
  duplicate_identifier_count <- lfs[
    ,
    .N,
    by = possible_id_variables
  ][
    N > 1,
    sum(N - 1)
  ]
  
  if (length(duplicate_identifier_count) == 0 ||
      is.na(duplicate_identifier_count)) {
    duplicate_identifier_count <- 0
  }
  
  cat(
    "Duplicate observations based on available identifiers:",
    format(
      duplicate_identifier_count,
      big.mark = ","
    ),
    "\n"
  )
}

invisible(gc())


# ==============================================================================
# STEP 11: Create the variable-level diagnostic table
#
# The table reports:
#   - Storage class
#   - Number and percentage missing
#   - Number of unique nonmissing values
#
# Each column is analyzed separately to avoid making a large temporary copy.
# ==============================================================================

cat(
  "\nCreating variable-level diagnostics...\n"
)

diagnostic_list <- vector(
  mode = "list",
  length = ncol(lfs)
)

for (j in seq_along(lfs)) {
  
  variable_name <- names(lfs)[j]
  
  x <- lfs[[j]]
  
  diagnostic_list[[j]] <- data.table(
    variable_name = variable_name,
    
    variable_class = paste(
      class(x),
      collapse = ", "
    ),
    
    number_observations = length(x),
    
    number_missing = sum(
      is.na(x)
    ),
    
    percent_missing = round(
      100 * mean(
        is.na(x)
      ),
      4
    ),
    
    number_unique_nonmissing = uniqueN(
      x,
      na.rm = TRUE
    )
  )
}

variable_diagnostics <- rbindlist(
  diagnostic_list,
  use.names = TRUE,
  fill = TRUE
)

setorder(
  variable_diagnostics,
  -percent_missing,
  variable_name
)

fwrite(
  variable_diagnostics,
  file.path(
    output_folder,
    "lfs_reduced_variable_diagnostics.csv"
  ),
  bom = TRUE
)

# Remove temporary objects.
rm(
  diagnostic_list,
  variable_diagnostics,
  x
)

invisible(gc())


# ==============================================================================
# STEP 12: Create survey-coverage diagnostics
# ==============================================================================


# ------------------------------------------------------------------------------
# 12.1 Observations by survey year
# ------------------------------------------------------------------------------

cat(
  "\nCreating observations-by-year table...\n"
)

observations_by_year <- lfs[
  ,
  .(
    unweighted_observations = .N,
    
    sum_of_weights = sum(
      weight,
      na.rm = TRUE
    ),
    
    number_of_provinces = uniqueN(
      province,
      na.rm = TRUE
    ),
    
    minimum_age = min(
      age,
      na.rm = TRUE
    ),
    
    maximum_age = max(
      age,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  observations_by_year,
  file.path(
    output_folder,
    "observations_by_year.csv"
  ),
  bom = TRUE
)

rm(observations_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 12.2 Observations by year and quarter
# ------------------------------------------------------------------------------

cat(
  "Creating observations-by-year-and-quarter table...\n"
)

observations_by_year_quarter <- lfs[
  ,
  .(
    unweighted_observations = .N,
    
    sum_of_weights = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    quarter
  )
][
  order(
    year,
    quarter
  )
]

fwrite(
  observations_by_year_quarter,
  file.path(
    output_folder,
    "observations_by_year_and_quarter.csv"
  ),
  bom = TRUE
)

rm(observations_by_year_quarter)

invisible(gc())


# ------------------------------------------------------------------------------
# 12.3 Observations by year and province
# ------------------------------------------------------------------------------

cat(
  "Creating observations-by-year-and-province table...\n"
)

observations_by_year_province <- lfs[
  ,
  .(
    unweighted_observations = .N,
    
    sum_of_weights = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    province
  )
][
  order(
    year,
    province
  )
]

fwrite(
  observations_by_year_province,
  file.path(
    output_folder,
    "observations_by_year_and_province.csv"
  ),
  bom = TRUE
)

rm(observations_by_year_province)

invisible(gc())


# ==============================================================================
# STEP 13: Create core categorical-variable diagnostics
#
# These are simple frequency tables. No new variables are added to the main
# dataset.
# ==============================================================================


# ------------------------------------------------------------------------------
# 13.1 Employment status by year
# ------------------------------------------------------------------------------

cat(
  "\nCreating employment-status diagnostics...\n"
)

employment_status_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    employment_status
  )
][
  order(
    year,
    employment_status
  )
]

fwrite(
  employment_status_by_year,
  file.path(
    output_folder,
    "employment_status_by_year.csv"
  ),
  bom = TRUE
)

rm(employment_status_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 13.2 Education level by year
# ------------------------------------------------------------------------------

cat(
  "Creating education-level diagnostics...\n"
)

education_level_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    education_level
  )
][
  order(
    year,
    education_level
  )
]

fwrite(
  education_level_by_year,
  file.path(
    output_folder,
    "education_level_by_year.csv"
  ),
  bom = TRUE
)

rm(education_level_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 13.3 Marital status by year
# ------------------------------------------------------------------------------

cat(
  "Creating marital-status diagnostics...\n"
)

marital_status_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    mar_status
  )
][
  order(
    year,
    mar_status
  )
]

fwrite(
  marital_status_by_year,
  file.path(
    output_folder,
    "marital_status_by_year.csv"
  ),
  bom = TRUE
)

rm(marital_status_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 13.4 Student status by year
# ------------------------------------------------------------------------------

cat(
  "Creating student-status diagnostics...\n"
)

student_status_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    student
  )
][
  order(
    year,
    student
  )
]

fwrite(
  student_status_by_year,
  file.path(
    output_folder,
    "student_status_by_year.csv"
  ),
  bom = TRUE
)

rm(student_status_by_year)

invisible(gc())


# ==============================================================================
# STEP 14: Create migration diagnostics
#
# These variables appear to describe recent residential moves.
# At this stage, we report their original values without imposing a final
# definition of migrant or non-migrant.
# ==============================================================================


# ------------------------------------------------------------------------------
# 14.1 Previous residence location by year
# ------------------------------------------------------------------------------

cat(
  "\nCreating migration-location diagnostics...\n"
)

migration_location_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    previous_residence_location
  )
][
  order(
    year,
    previous_residence_location
  )
]

fwrite(
  migration_location_by_year,
  file.path(
    output_folder,
    "migration_location_by_year.csv"
  ),
  bom = TRUE
)

rm(migration_location_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 14.2 Residence status by year
# ------------------------------------------------------------------------------

cat(
  "Creating residence-status diagnostics...\n"
)

residence_status_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    residence_status
  )
][
  order(
    year,
    residence_status
  )
]

fwrite(
  residence_status_by_year,
  file.path(
    output_folder,
    "residence_status_by_year.csv"
  ),
  bom = TRUE
)

rm(residence_status_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 14.3 Residence duration by year
# ------------------------------------------------------------------------------

cat(
  "Creating residence-duration diagnostics...\n"
)

residence_duration_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    residence_duration
  )
][
  order(
    year,
    residence_duration
  )
]

fwrite(
  residence_duration_by_year,
  file.path(
    output_folder,
    "residence_duration_by_year.csv"
  ),
  bom = TRUE
)

rm(residence_duration_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 14.4 Reason for changing residence by year
# ------------------------------------------------------------------------------

cat(
  "Creating migration-reason diagnostics...\n"
)

residence_change_reason_by_year <- lfs[
  ,
  .N,
  by = .(
    year,
    residence_change_reason
  )
][
  order(
    year,
    residence_change_reason
  )
]

fwrite(
  residence_change_reason_by_year,
  file.path(
    output_folder,
    "residence_change_reason_by_year.csv"
  ),
  bom = TRUE
)

rm(residence_change_reason_by_year)

invisible(gc())


# ------------------------------------------------------------------------------
# 14.5 Previous province and current province
#
# Only observations with a reported previous province are included.
# ------------------------------------------------------------------------------

cat(
  "Creating previous-current-province diagnostics...\n"
)

previous_and_current_province_table <- lfs[
  !is.na(previous_residence_province),
  .N,
  by = .(
    year,
    province,
    previous_residence_province
  )
][
  order(
    year,
    province,
    previous_residence_province
  )
]

fwrite(
  previous_and_current_province_table,
  file.path(
    output_folder,
    "previous_and_current_province_table.csv"
  ),
  bom = TRUE
)

rm(previous_and_current_province_table)

invisible(gc())


# ------------------------------------------------------------------------------
# 14.6 Basic migration availability summary
#
# This summary does not classify people without migration information as
# lifetime non-movers.
# ------------------------------------------------------------------------------

cat(
  "Creating migration-availability summary...\n"
)

migration_summary_by_year <- lfs[
  ,
  .(
    observations = .N,
    
    previous_location_reported = sum(
      !is.na(previous_residence_location)
    ),
    
    percent_previous_location_reported = round(
      100 * mean(
        !is.na(previous_residence_location)
      ),
      4
    ),
    
    previous_province_reported = sum(
      !is.na(previous_residence_province)
    ),
    
    percent_previous_province_reported = round(
      100 * mean(
        !is.na(previous_residence_province)
      ),
      4
    ),
    
    residence_duration_reported = sum(
      !is.na(residence_duration)
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  migration_summary_by_year,
  file.path(
    output_folder,
    "migration_summary_by_year.csv"
  ),
  bom = TRUE
)

rm(migration_summary_by_year)

invisible(gc())


# ==============================================================================
# STEP 15: Diagnose education-field and occupation availability
#
# Horizontal mismatch requires both field of education and occupation.
# Vertical mismatch requires education level and occupation.
# ==============================================================================

cat(
  "\nCreating education-field and occupation diagnostics...\n"
)

education_field_occupation_availability <- lfs[
  ,
  .(
    observations = .N,
    
    education_field_available = sum(
      !is.na(education_field)
    ),
    
    percent_field_available = round(
      100 * mean(
        !is.na(education_field)
      ),
      4
    ),
    
    occupation_available = sum(
      !is.na(main_job_title_isco_code)
    ),
    
    percent_occupation_available = round(
      100 * mean(
        !is.na(main_job_title_isco_code)
      ),
      4
    ),
    
    field_and_occupation_available = sum(
      !is.na(education_field) &
        !is.na(main_job_title_isco_code)
    ),
    
    percent_field_and_occupation_available = round(
      100 * mean(
        !is.na(education_field) &
          !is.na(main_job_title_isco_code)
      ),
      4
    )
  ),
  by = .(
    year,
    education_level
  )
][
  order(
    year,
    education_level
  )
]

fwrite(
  education_field_occupation_availability,
  file.path(
    output_folder,
    "education_field_and_occupation_availability.csv"
  ),
  bom = TRUE
)

rm(education_field_occupation_availability)

invisible(gc())


# ==============================================================================
# STEP 16: Create a simple birth-year diagnostic
#
# The birth_year variable contains two-digit codes.
#
# We do not construct the final four-digit birth year in this first script.
# Birth-year correction and university-entry exposure will be handled in the
# next step after the reduced file is safely saved.
# ==============================================================================

cat(
  "\nCreating original birth-year diagnostic...\n"
)

birth_year_code_by_year <- lfs[
  ,
  .(
    observations = .N,
    
    birth_year_missing = sum(
      is.na(birth_year)
    ),
    
    minimum_birth_year_code = {
      x <- birth_year[!is.na(birth_year)]
      
      if (length(x) == 0) {
        NA_real_
      } else {
        min(x)
      }
    },
    
    maximum_birth_year_code = {
      x <- birth_year[!is.na(birth_year)]
      
      if (length(x) == 0) {
        NA_real_
      } else {
        max(x)
      }
    },
    
    number_of_birth_year_codes = uniqueN(
      birth_year,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  birth_year_code_by_year,
  file.path(
    output_folder,
    "birth_year_code_by_year.csv"
  ),
  bom = TRUE
)

rm(birth_year_code_by_year)

invisible(gc())


# ==============================================================================
# STEP 17: Save the reduced LFS dataset
#
# Only one FST file is saved.
#
# We deliberately do not save:
#   - An RDS file
#   - A second Stata file
#   - A CSV copy of the microdata
#
# These alternatives would be slower and require much more memory or disk space.
# ==============================================================================

cat(
  "\nSaving the reduced LFS dataset in FST format...\n"
)

fst_output_file <- file.path(
  output_folder,
  "lfs_reduced_1386_1403.fst"
)

# Compression of 30 provides a reasonable balance between speed and file size.
write_fst(
  lfs,
  fst_output_file,
  compress = 30
)

cat(
  "\nFST file saved successfully.\n"
)


# ==============================================================================
# STEP 18: Verify that the saved FST file exists
# ==============================================================================

if (!file.exists(fst_output_file)) {
  stop(
    "The FST file was not created successfully."
  )
}

saved_file_size_gb <- file.info(
  fst_output_file
)$size / (1024^3)

cat(
  "Saved file size:",
  round(saved_file_size_gb, 3),
  "GB\n"
)


# ==============================================================================
# STEP 19: Save a compact completion summary
# ==============================================================================

completion_summary <- data.table(
  item = c(
    "Input file",
    "Output file",
    "Number of observations",
    "Number of variables",
    "Minimum survey year",
    "Maximum survey year",
    "Number of provinces",
    "FST file size in GB"
  ),
  
  value = c(
    input_file,
    fst_output_file,
    format(nrow(lfs), scientific = FALSE),
    as.character(ncol(lfs)),
    as.character(min(lfs$year, na.rm = TRUE)),
    as.character(max(lfs$year, na.rm = TRUE)),
    as.character(uniqueN(lfs$province, na.rm = TRUE)),
    as.character(round(saved_file_size_gb, 3))
  )
)

fwrite(
  completion_summary,
  file.path(
    output_folder,
    "step1_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Final console report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 1 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nObservations:",
  format(nrow(lfs), big.mark = ","),
  "\n"
)

cat(
  "Variables:",
  ncol(lfs),
  "\n"
)

cat(
  "Survey years:",
  min(lfs$year, na.rm = TRUE),
  "to",
  max(lfs$year, na.rm = TRUE),
  "\n"
)

cat(
  "\nReduced dataset saved at:\n",
  fst_output_file,
  "\n"
)

cat(
  "\nDiagnostic files saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nThe next step will:\n",
  "1. Correct the two-digit birth year.\n",
  "2. Construct ages 18, 19, and 20 exposure years.\n",
  "3. Harmonize provinces.\n",
  "4. Define labor-market and marriage outcomes.\n",
  "5. investigate migration coding.\n"
)


# ==============================================================================
# STEP 21: Optional memory cleanup after successful completion
#
# Run this only after confirming that the FST file was saved.
# ==============================================================================

rm(lfs)

invisible(gc())

cat(
  "\nThe large in-memory LFS object has been removed.\n"
)








# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 2: Create the Exposure-Eligible LFS Analysis File
#
# Purpose:
#   1. Load the reduced LFS dataset created in Step 1.
#   2. Keep every existing column for future analyses.
#   3. Correct the two-digit Persian birth-year variable.
#   4. Construct university-entry years at ages 18, 19, and 20.
#   5. Retain respondents whose three exposure years fall within 1373–1403.
#   6. Create preliminary labor-market, marriage, and migration variables.
#   7. Save a smaller analysis-eligible FST file.
#
# Important:
#   - The original reduced file is not overwritten.
#   - All existing columns are preserved.
#   - Only observations are filtered.
#   - Province harmonization and exposure merging will be done later.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load required packages
# ==============================================================================

rm(list = ls())
invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 50)

required_packages <- c(
  "data.table",
  "fst",
  "stringr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_reduced_outputs",
  "lfs_reduced_1386_1403.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

output_file <- file.path(
  output_folder,
  "lfs_analysis_eligible_1386_1403.fst"
)

if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The reduced LFS file was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Load the reduced LFS dataset
#
# All columns from the reduced file are loaded and preserved.
# ==============================================================================

cat("\nReading the reduced LFS dataset...\n")

dt <- as.data.table(
  read_fst(input_file)
)

original_number_of_rows <- nrow(dt)
original_number_of_columns <- ncol(dt)
original_column_names <- names(dt)

cat(
  "\nOriginal observations:",
  format(original_number_of_rows, big.mark = ","),
  "\n"
)

cat(
  "Original columns:",
  original_number_of_columns,
  "\n"
)


# ==============================================================================
# STEP 3: Standardize core numerical variables
#
# These conversions ensure that year, age, birth year, and survey weight are
# stored numerically before constructing derived variables.
# ==============================================================================

numeric_variables <- intersect(
  c(
    "year",
    "quarter",
    "age",
    "birth_year",
    "birth_month",
    "weight",
    "mar_status",
    "employment_status",
    "residence_status",
    "residence_duration"
  ),
  names(dt)
)

for (variable_name in numeric_variables) {
  set(
    dt,
    j = variable_name,
    value = as.numeric(dt[[variable_name]])
  )
}

invisible(gc())


# ==============================================================================
# STEP 4: Construct the full four-digit Persian birth year
#
# The original birth_year variable stores only the final two digits.
#
# Examples:
#
# Survey year 1386:
#   birth_year = 66  means 1366
#   birth_year = 06  means 1306
#   birth_year = 99  means 1299
#
# Survey year 1403:
#   birth_year = 01  means 1401
#   birth_year = 99  means 1399
#
# The century is determined relative to the survey year.
# ==============================================================================

dt[
  ,
  survey_century := floor(year / 100) * 100
]

dt[
  ,
  survey_year_two_digit := year %% 100
]

dt[
  ,
  birth_year_two_digit := birth_year
]

dt[
  !is.na(birth_year_two_digit) &
    !is.na(year),
  reported_birth_year_full :=
    fifelse(
      birth_year_two_digit <= survey_year_two_digit,
      survey_century + birth_year_two_digit,
      survey_century - 100 + birth_year_two_digit
    )
]


# ==============================================================================
# STEP 5: Construct an alternative birth year from survey year and age
#
# This provides an independent check on the reported birth-year code.
#
# The two measures may differ by one year because a respondent may or may not
# have had their birthday by the interview date.
# ==============================================================================

dt[
  !is.na(year) &
    !is.na(age),
  birth_year_from_age := year - age
]

dt[
  ,
  birth_year_difference :=
    reported_birth_year_full - birth_year_from_age
]


# ==============================================================================
# STEP 6: Assess birth-year consistency
#
# A difference of zero or one year in either direction is treated as acceptable.
#
# Larger discrepancies are flagged.
# ==============================================================================

dt[
  ,
  birth_year_consistent :=
    fcase(
      is.na(reported_birth_year_full) |
        is.na(birth_year_from_age),
      NA_integer_,
      
      abs(birth_year_difference) <= 1,
      1L,
      
      abs(birth_year_difference) > 1,
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 7: Select the birth year used for analysis
#
# Baseline rule:
#
#   1. Use the corrected reported birth year when it is consistent with age.
#   2. Otherwise, use survey year minus age.
#
# This prevents implausible birth-year codes from causing incorrect exposure
# assignment.
# ==============================================================================

dt[
  ,
  analysis_birth_year :=
    fcase(
      birth_year_consistent == 1L,
      reported_birth_year_full,
      
      !is.na(birth_year_from_age),
      birth_year_from_age,
      
      !is.na(reported_birth_year_full),
      reported_birth_year_full,
      
      default = NA_real_
    )
]


# ==============================================================================
# STEP 8: Create age-quality indicators
#
# The source includes a small number of implausible ages.
#
# We do not delete them immediately. Instead, we create indicators so that the
# filtering rule is transparent.
# ==============================================================================

dt[
  ,
  age_nonmissing :=
    as.integer(
      !is.na(age)
    )
]

dt[
  ,
  age_plausible :=
    as.integer(
      !is.na(age) &
        age >= 0 &
        age <= 100
    )
]

dt[
  ,
  adult_age_18_plus :=
    as.integer(
      !is.na(age) &
        age >= 18
    )
]


# ==============================================================================
# STEP 9: Construct university-entry exposure years
#
# Exposure will later be defined as the average undergraduate enrollment per
# 100,000 population in the individual's assigned province when the person was:
#
#   - Age 18
#   - Age 19
#   - Age 20
# ==============================================================================

dt[
  ,
  exposure_year_age18 := analysis_birth_year + 18
]

dt[
  ,
  exposure_year_age19 := analysis_birth_year + 19
]

dt[
  ,
  exposure_year_age20 := analysis_birth_year + 20
]


# ==============================================================================
# STEP 10: Identify respondents with complete exposure-year coverage
#
# The exposure panel covers Persian years 1373–1403.
#
# Baseline eligibility requires:
#
#   exposure year at age 18 >= 1373
#   exposure year at age 20 <= 1403
#
# This ensures that all three exposure years are available.
# ==============================================================================

dt[
  ,
  complete_exposure_year_coverage :=
    as.integer(
      !is.na(exposure_year_age18) &
        !is.na(exposure_year_age19) &
        !is.na(exposure_year_age20) &
        exposure_year_age18 >= 1373 &
        exposure_year_age20 <= 1403
    )
]


# ==============================================================================
# STEP 11: Create the final observation-level eligibility indicator
#
# The analysis-eligible dataset includes respondents who:
#
#   1. Are age 18 or older.
#   2. Have a plausible age no greater than 100.
#   3. Have complete exposure-year coverage at ages 18–20.
#
# No columns are removed.
# ==============================================================================

dt[
  ,
  analysis_sample_eligible :=
    as.integer(
      adult_age_18_plus == 1L &
        age_plausible == 1L &
        complete_exposure_year_coverage == 1L
    )
]


# ==============================================================================
# STEP 12: Create preliminary labor-market outcomes
#
# Based on the Stata value labels:
#
#   employment_status = 1  Employed
#   employment_status = 2  Inactive
#   employment_status = 3  Unemployed
#
# These variables will be checked again before regressions.
# ==============================================================================


# ------------------------------------------------------------------------------
# 12.1 Labor-force participation
#
# Employed and unemployed respondents participate in the labor force.
# Inactive respondents do not.
# ------------------------------------------------------------------------------

dt[
  ,
  labor_force_participation :=
    fcase(
      employment_status %in% c(1, 3),
      1L,
      
      employment_status == 2,
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 12.2 Employment-to-population indicator
#
# This equals one for employed respondents and zero for unemployed or inactive
# respondents.
# ------------------------------------------------------------------------------

dt[
  ,
  employed :=
    fcase(
      employment_status == 1,
      1L,
      
      employment_status %in% c(2, 3),
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 12.3 Unemployment indicator
#
# This variable is defined only among labor-force participants.
#
#   1 = unemployed
#   0 = employed
#   NA = inactive or employment status missing
# ------------------------------------------------------------------------------

dt[
  ,
  unemployed :=
    fcase(
      employment_status == 3,
      1L,
      
      employment_status == 1,
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 13: Create preliminary marriage outcomes
#
# Based on the Stata value labels:
#
#   mar_status = 1  Married
#   mar_status = 2  Divorced
#   mar_status = 3  Never married
#   mar_status = 4  Widowed
# ==============================================================================


# ------------------------------------------------------------------------------
# 13.1 Readable marital-status category
# ------------------------------------------------------------------------------

dt[
  ,
  marital_status_clean :=
    fcase(
      mar_status == 1,
      "Married",
      
      mar_status == 2,
      "Divorced",
      
      mar_status == 3,
      "Never_Married",
      
      mar_status == 4,
      "Widowed",
      
      default = NA_character_
    )
]


# ------------------------------------------------------------------------------
# 13.2 Currently married
# ------------------------------------------------------------------------------

dt[
  ,
  currently_married :=
    fcase(
      mar_status == 1,
      1L,
      
      mar_status %in% c(2, 3, 4),
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 13.3 Ever married
#
# Married, divorced, and widowed respondents are treated as ever married.
# ------------------------------------------------------------------------------

dt[
  ,
  ever_married :=
    fcase(
      mar_status %in% c(1, 2, 4),
      1L,
      
      mar_status == 3,
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 14: Create recent-migration variables
#
# The LFS migration questions identify recent residential changes.
#
# They do not establish whether an individual has ever migrated during their
# lifetime.
# ==============================================================================


# ------------------------------------------------------------------------------
# 14.1 Clean the previous-residence-location text
# ------------------------------------------------------------------------------

dt[
  ,
  previous_residence_location_clean :=
    str_to_lower(
      str_squish(
        as.character(previous_residence_location)
      )
    )
]


# ------------------------------------------------------------------------------
# 14.2 Create a readable recent-migration category
# ------------------------------------------------------------------------------

dt[
  ,
  recent_migration_type :=
    fcase(
      
      previous_residence_location_clean %in%
        c(
          "same_city_other_place",
          "same city other place"
        ),
      "Within_City",
      
      previous_residence_location_clean %in%
        c(
          "same_province_other_city",
          "same province other city",
          "same_province_other_village",
          "same province other village"
        ),
      "Within_Province",
      
      previous_residence_location_clean %in%
        c(
          "other_province_other_city",
          "other province other city",
          "other_province_other_village",
          "other province other village"
        ),
      "Interprovincial",
      
      previous_residence_location_clean == "abroad",
      "International",
      
      is.na(previous_residence_location_clean),
      "No_Recent_Move_Reported",
      
      default = "Other_or_Unknown"
    )
]


# ------------------------------------------------------------------------------
# 14.3 Indicator for any reported recent move
# ------------------------------------------------------------------------------

dt[
  ,
  recent_move_reported :=
    as.integer(
      recent_migration_type %in%
        c(
          "Within_City",
          "Within_Province",
          "Interprovincial",
          "International",
          "Other_or_Unknown"
        )
    )
]


# ------------------------------------------------------------------------------
# 14.4 Recent interprovincial migration
# ------------------------------------------------------------------------------

dt[
  ,
  recent_interprovincial_migrant :=
    as.integer(
      recent_migration_type == "Interprovincial"
    )
]


# ------------------------------------------------------------------------------
# 14.5 Recent within-province migration
# ------------------------------------------------------------------------------

dt[
  ,
  recent_within_province_migrant :=
    as.integer(
      recent_migration_type == "Within_Province"
    )
]


# ------------------------------------------------------------------------------
# 14.6 Recent within-city migration
# ------------------------------------------------------------------------------

dt[
  ,
  recent_within_city_migrant :=
    as.integer(
      recent_migration_type == "Within_City"
    )
]


# ------------------------------------------------------------------------------
# 14.7 Recent international migration
# ------------------------------------------------------------------------------

dt[
  ,
  recent_international_migrant :=
    as.integer(
      recent_migration_type == "International"
    )
]


# ==============================================================================
# STEP 15: Create clean province text variables
#
# These variables prepare current and previous province names for the later
# province-harmonization step.
# ==============================================================================

dt[
  ,
  current_province_clean :=
    str_to_lower(
      str_squish(
        as.character(province)
      )
    )
]

dt[
  ,
  previous_province_clean :=
    str_to_lower(
      str_squish(
        as.character(previous_residence_province)
      )
    )
]

dt[
  ,
  current_previous_province_different :=
    fcase(
      !is.na(current_province_clean) &
        !is.na(previous_province_clean),
      as.integer(
        current_province_clean != previous_province_clean
      ),
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 16: Produce a pre-filter sample summary
# ==============================================================================

pre_filter_summary <- data.table(
  measure = c(
    "Total observations",
    "Adults age 18 or older",
    "Plausible age",
    "Complete exposure-year coverage",
    "Final analysis-eligible observations"
  ),
  
  value = c(
    nrow(dt),
    
    sum(
      dt$adult_age_18_plus == 1,
      na.rm = TRUE
    ),
    
    sum(
      dt$age_plausible == 1,
      na.rm = TRUE
    ),
    
    sum(
      dt$complete_exposure_year_coverage == 1,
      na.rm = TRUE
    ),
    
    sum(
      dt$analysis_sample_eligible == 1,
      na.rm = TRUE
    )
  )
)

fwrite(
  pre_filter_summary,
  file.path(
    output_folder,
    "step2_pre_filter_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Create eligibility diagnostics by survey year
# ==============================================================================

eligibility_by_year <- dt[
  ,
  .(
    total_observations = .N,
    
    adults_age_18_plus = sum(
      adult_age_18_plus == 1,
      na.rm = TRUE
    ),
    
    complete_exposure_coverage = sum(
      complete_exposure_year_coverage == 1,
      na.rm = TRUE
    ),
    
    analysis_eligible = sum(
      analysis_sample_eligible == 1,
      na.rm = TRUE
    ),
    
    percent_analysis_eligible = round(
      100 * mean(
        analysis_sample_eligible == 1,
        na.rm = TRUE
      ),
      3
    ),
    
    minimum_eligible_age = {
      x <- age[analysis_sample_eligible == 1]
      
      if (length(x) == 0 || all(is.na(x))) {
        NA_real_
      } else {
        min(x, na.rm = TRUE)
      }
    },
    
    maximum_eligible_age = {
      x <- age[analysis_sample_eligible == 1]
      
      if (length(x) == 0 || all(is.na(x))) {
        NA_real_
      } else {
        max(x, na.rm = TRUE)
      }
    }
  ),
  by = year
][
  order(year)
]

fwrite(
  eligibility_by_year,
  file.path(
    output_folder,
    "step2_eligibility_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 18: Create birth-year consistency diagnostics
# ==============================================================================

birth_year_diagnostics <- dt[
  ,
  .(
    observations = .N,
    
    reported_birth_year_missing = sum(
      is.na(reported_birth_year_full)
    ),
    
    age_based_birth_year_missing = sum(
      is.na(birth_year_from_age)
    ),
    
    exact_match = sum(
      birth_year_difference == 0,
      na.rm = TRUE
    ),
    
    difference_one_year = sum(
      abs(birth_year_difference) == 1,
      na.rm = TRUE
    ),
    
    difference_more_than_one_year = sum(
      abs(birth_year_difference) > 1,
      na.rm = TRUE
    ),
    
    percent_difference_more_than_one_year = round(
      100 * mean(
        abs(birth_year_difference) > 1,
        na.rm = TRUE
      ),
      4
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  birth_year_diagnostics,
  file.path(
    output_folder,
    "step2_birth_year_diagnostics.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Create migration diagnostics before filtering
# ==============================================================================

migration_diagnostics <- dt[
  ,
  .(
    observations = .N,
    
    recent_move_reported = sum(
      recent_move_reported == 1,
      na.rm = TRUE
    ),
    
    recent_interprovincial_migrants = sum(
      recent_interprovincial_migrant == 1,
      na.rm = TRUE
    ),
    
    recent_within_province_migrants = sum(
      recent_within_province_migrant == 1,
      na.rm = TRUE
    ),
    
    recent_within_city_migrants = sum(
      recent_within_city_migrant == 1,
      na.rm = TRUE
    ),
    
    recent_international_migrants = sum(
      recent_international_migrant == 1,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  migration_diagnostics,
  file.path(
    output_folder,
    "step2_migration_diagnostics.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Filter observations while keeping every column
#
# Only observations with analysis_sample_eligible = 1 are retained.
#
# No original or newly created columns are removed.
# ==============================================================================

cat("\nFiltering to exposure-eligible adult respondents...\n")

dt <- dt[
  analysis_sample_eligible == 1
]

setDT(dt)

invisible(gc())


# ==============================================================================
# STEP 21: Confirm that all original columns were preserved
# ==============================================================================

missing_original_columns <- setdiff(
  original_column_names,
  names(dt)
)

if (length(missing_original_columns) > 0) {
  stop(
    paste0(
      "Some original columns were unexpectedly removed:\n",
      paste(
        missing_original_columns,
        collapse = ", "
      )
    )
  )
}

cat(
  "\nAll",
  original_number_of_columns,
  "original columns were preserved.\n"
)

cat(
  "Additional derived columns:",
  ncol(dt) - original_number_of_columns,
  "\n"
)


# ==============================================================================
# STEP 22: Create a retained-age table by survey year
# ==============================================================================

retained_age_range_by_year <- dt[
  ,
  .(
    observations = .N,
    
    minimum_age = min(
      age,
      na.rm = TRUE
    ),
    
    maximum_age = max(
      age,
      na.rm = TRUE
    ),
    
    minimum_birth_year = min(
      analysis_birth_year,
      na.rm = TRUE
    ),
    
    maximum_birth_year = max(
      analysis_birth_year,
      na.rm = TRUE
    ),
    
    minimum_exposure_year_age18 = min(
      exposure_year_age18,
      na.rm = TRUE
    ),
    
    maximum_exposure_year_age20 = max(
      exposure_year_age20,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  retained_age_range_by_year,
  file.path(
    output_folder,
    "step2_retained_age_range_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Create preliminary outcome summaries
#
# These are descriptive checks, not regression estimates.
# ==============================================================================

preliminary_outcome_summary <- dt[
  ,
  .(
    observations = .N,
    
    weighted_labor_force_participation = weighted.mean(
      labor_force_participation,
      weight,
      na.rm = TRUE
    ),
    
    weighted_employment_population_rate = weighted.mean(
      employed,
      weight,
      na.rm = TRUE
    ),
    
    weighted_unemployment_rate = weighted.mean(
      unemployed,
      weight,
      na.rm = TRUE
    ),
    
    weighted_currently_married_share = weighted.mean(
      currently_married,
      weight,
      na.rm = TRUE
    ),
    
    weighted_ever_married_share = weighted.mean(
      ever_married,
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    gender
  )
][
  order(
    year,
    gender
  )
]

fwrite(
  preliminary_outcome_summary,
  file.path(
    output_folder,
    "step2_preliminary_outcome_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Save the exposure-eligible analysis file
#
# This file keeps:
#
#   - Every original column from the reduced LFS file
#   - Corrected birth-year variables
#   - Exposure-year variables
#   - Eligibility indicators
#   - Preliminary labor-market outcomes
#   - Preliminary marriage outcomes
#   - Recent-migration variables
#   - Clean province text variables
# ==============================================================================

cat(
  "\nSaving the exposure-eligible analysis dataset...\n"
)

write_fst(
  dt,
  output_file,
  compress = 30
)

if (!file.exists(output_file)) {
  stop(
    "The analysis-eligible FST file was not saved successfully."
  )
}


# ==============================================================================
# STEP 25: Save a completion summary
# ==============================================================================

output_file_size_gb <- file.info(
  output_file
)$size / (1024^3)

completion_summary <- data.table(
  item = c(
    "Input observations",
    "Output observations",
    "Observations removed",
    "Percent retained",
    "Original columns preserved",
    "Total output columns",
    "Derived columns added",
    "Minimum survey year",
    "Maximum survey year",
    "Output file size in GB",
    "Output file"
  ),
  
  value = c(
    format(
      original_number_of_rows,
      scientific = FALSE
    ),
    
    format(
      nrow(dt),
      scientific = FALSE
    ),
    
    format(
      original_number_of_rows - nrow(dt),
      scientific = FALSE
    ),
    
    as.character(
      round(
        100 * nrow(dt) / original_number_of_rows,
        3
      )
    ),
    
    as.character(
      original_number_of_columns
    ),
    
    as.character(
      ncol(dt)
    ),
    
    as.character(
      ncol(dt) - original_number_of_columns
    ),
    
    as.character(
      min(dt$year, na.rm = TRUE)
    ),
    
    as.character(
      max(dt$year, na.rm = TRUE)
    ),
    
    as.character(
      round(
        output_file_size_gb,
        3
      )
    ),
    
    output_file
  )
)

fwrite(
  completion_summary,
  file.path(
    output_folder,
    "step2_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Print the final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 2 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nOriginal observations:",
  format(
    original_number_of_rows,
    big.mark = ","
  ),
  "\n"
)

cat(
  "Retained observations:",
  format(
    nrow(dt),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Percent retained:",
  round(
    100 * nrow(dt) / original_number_of_rows,
    3
  ),
  "%\n"
)

cat(
  "\nOriginal columns preserved:",
  original_number_of_columns,
  "\n"
)

cat(
  "Total output columns:",
  ncol(dt),
  "\n"
)

cat(
  "Derived columns added:",
  ncol(dt) - original_number_of_columns,
  "\n"
)

cat(
  "\nOutput file size:",
  round(
    output_file_size_gb,
    3
  ),
  "GB\n"
)

cat(
  "\nAnalysis-ready file saved at:\n",
  output_file,
  "\n"
)

cat(
  "\nImportant diagnostic files:\n",
  "1. step2_completion_summary.csv\n",
  "2. step2_birth_year_diagnostics.csv\n",
  "3. step2_eligibility_by_year.csv\n",
  "4. step2_retained_age_range_by_year.csv\n",
  "5. step2_migration_diagnostics.csv\n",
  "6. step2_preliminary_outcome_summary.csv\n"
)


# ==============================================================================
# STEP 27: Remove the large in-memory object after saving
# ==============================================================================

rm(dt)

invisible(gc())

cat(
  "\nThe in-memory analysis dataset has been removed after saving.\n"
)






### reading labels




library(haven)
library(data.table)

input_file <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)/data_ready.dta"

output_file <- paste0(
  "E:/sfu/PhD/Summer2026/paper(HES and LFS)/LFS_reduced_outputs/",
  "stata_value_label_codebook.csv"
)

# Read only a few rows because value labels are stored as metadata.
sample_data <- read_dta(
  input_file,
  n_max = 10
)

labelled_variables <- names(sample_data)[
  vapply(
    sample_data,
    haven::is.labelled,
    logical(1)
  )
]

codebook <- rbindlist(
  lapply(
    labelled_variables,
    function(variable_name) {
      
      labels <- attr(
        sample_data[[variable_name]],
        "labels"
      )
      
      if (is.null(labels)) {
        return(NULL)
      }
      
      data.table(
        variable = variable_name,
        code = as.numeric(labels),
        label = names(labels)
      )
    }
  ),
  fill = TRUE
)

fwrite(
  codebook,
  output_file,
  bom = TRUE
)

cat(
  "\nCodebook saved at:\n",
  output_file,
  "\n"
)




## correcting the mistakes

library(data.table)
library(fst)

input_file <- paste0(
  "E:/sfu/PhD/Summer2026/paper(HES and LFS)/LFS_analysis_outputs/",
  "lfs_analysis_eligible_1386_1403.fst"
)

dt <- as.data.table(
  read_fst(input_file)
)

is.data.table(dt)

dt[
  ,
  marital_status_clean :=
    fcase(
      mar_status == 1, "Never_Married",
      mar_status == 2, "Divorced",
      mar_status == 3, "Married",
      mar_status == 4, "Widowed",
      default = NA_character_
    )
]

dt[
  ,
  currently_married :=
    fcase(
      mar_status == 3, 1L,
      mar_status %in% c(1, 2, 4), 0L,
      default = NA_integer_
    )
]

dt[
  ,
  ever_married :=
    fcase(
      mar_status %in% c(2, 3, 4), 1L,
      mar_status == 1, 0L,
      default = NA_integer_
    )
]

write_fst(
  dt,
  input_file,
  compress = 30
)

dt[
  ,
  .N,
  by = .(
    mar_status,
    marital_status_clean,
    currently_married,
    ever_married
  )
][
  order(mar_status)
]






















# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 3: Harmonize LFS Provinces to Consistent Historical Regions
#
# Purpose:
#   1. Load the exposure-eligible LFS analysis dataset.
#   2. Map the 31 current Iranian provinces to 28 historical regions.
#   3. Match the geographical definition used in the university-exposure panel.
#   4. Harmonize both current province and previous-residence province.
#   5. Validate that every province maps correctly.
#   6. Save a new province-harmonized analysis file.
#
# Historical harmonization:
#
#   Alborz + Tehran
#       -> Tehran historical region
#
#   North Khorasan + Razavi Khorasan + South Khorasan
#       -> Khorasan historical region
#
# All other provinces retain their original names.
#
# Important:
#   - This script does not yet merge university exposure.
#   - The existing analysis file is not overwritten.
#   - All original and derived columns are retained.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load required packages
# ==============================================================================

rm(list = ls())

invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 100)

required_packages <- c(
  "data.table",
  "fst",
  "stringr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

# Input LFS file created in Step 2.
input_lfs_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_analysis_eligible_1386_1403.fst"
)

# Final exposure panel created previously.
exposure_file <- file.path(
  project_folder,
  "final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv"
)

# Folder for Step 3 outputs.
output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

# New harmonized LFS output file.
output_lfs_file <- file.path(
  output_folder,
  "lfs_step3_historical_provinces_1386_1403.fst"
)


# ------------------------------------------------------------------------------
# Confirm that both required files exist.
# ------------------------------------------------------------------------------

if (!file.exists(input_lfs_file)) {
  stop(
    paste0(
      "The Step 2 LFS file was not found:\n",
      input_lfs_file
    )
  )
}

if (!file.exists(exposure_file)) {
  stop(
    paste0(
      "The exposure panel was not found:\n",
      exposure_file
    )
  )
}


# ==============================================================================
# STEP 2: Load the exposure-eligible LFS dataset
# ==============================================================================

cat(
  "\nReading the exposure-eligible LFS dataset...\n"
)

dt <- as.data.table(
  read_fst(input_lfs_file)
)

original_number_of_rows <- nrow(dt)
original_number_of_columns <- ncol(dt)
original_column_names <- names(dt)

cat(
  "\nLFS observations:",
  format(nrow(dt), big.mark = ","),
  "\n"
)

cat(
  "LFS columns:",
  ncol(dt),
  "\n"
)


# ==============================================================================
# STEP 3: Load only the required columns from the exposure panel
#
# The exposure panel is small, so reading it does not create a memory problem.
# ==============================================================================

cat(
  "\nReading the province structure from the exposure panel...\n"
)

exposure_panel <- fread(
  exposure_file,
  encoding = "UTF-8",
  select = c(
    "exposure_year",
    "province_historical"
  )
)

# Standardize storage types.
exposure_panel[
  ,
  exposure_year := as.numeric(exposure_year)
]

exposure_panel[
  ,
  province_historical :=
    str_squish(
      as.character(province_historical)
    )
]


# ==============================================================================
# STEP 4: Identify the historical regions used in the exposure panel
# ==============================================================================

exposure_historical_provinces <- sort(
  unique(
    exposure_panel$province_historical
  )
)

cat(
  "\nNumber of historical regions in exposure panel:",
  length(exposure_historical_provinces),
  "\n"
)

print(exposure_historical_provinces)

if (length(exposure_historical_provinces) != 28) {
  stop(
    paste0(
      "The exposure panel does not contain exactly 28 historical regions.\n",
      "Number found: ",
      length(exposure_historical_provinces)
    )
  )
}


# ==============================================================================
# STEP 5: Inspect the current LFS province names
# ==============================================================================

lfs_current_provinces <- sort(
  unique(
    as.character(dt$province)
  )
)

cat(
  "\nNumber of current LFS provinces:",
  length(lfs_current_provinces),
  "\n"
)

print(lfs_current_provinces)

if (length(lfs_current_provinces) != 31) {
  warning(
    paste0(
      "The LFS does not contain exactly 31 current provinces.\n",
      "Number found: ",
      length(lfs_current_provinces)
    )
  )
}


# ==============================================================================
# STEP 6: Create an explicit province crosswalk
#
# This table maps each current LFS province into the corresponding historical
# province used by the exposure panel.
#
# The crosswalk is written explicitly so that it can be inspected, documented,
# and shared with collaborators.
# ==============================================================================

province_crosswalk <- data.table(
  
  province_current = c(
    "Alborz",
    "Ardabil",
    "Bushehr",
    "Chaharmahal_and_Bakhtiari",
    "East_Azerbaijan",
    "Fars",
    "Gilan",
    "Golestan",
    "Hamadan",
    "Hormozgan",
    "Ilam",
    "Isfahan",
    "Kerman",
    "Kermanshah",
    "Khuzestan",
    "Kohgiluyeh_and_Boyer_Ahmad",
    "Kurdistan",
    "Lorestan",
    "Markazi",
    "Mazandaran",
    "North_Khorasan",
    "Qazvin",
    "Qom",
    "Razavi_Khorasan",
    "Semnan",
    "Sistan_and_Baluchestan",
    "South_Khorasan",
    "Tehran",
    "West_Azerbaijan",
    "Yazd",
    "Zanjan"
  ),
  
  province_historical = c(
    "Tehran",
    "Ardabil",
    "Bushehr",
    "Chaharmahal_and_Bakhtiari",
    "East_Azerbaijan",
    "Fars",
    "Gilan",
    "Golestan",
    "Hamadan",
    "Hormozgan",
    "Ilam",
    "Isfahan",
    "Kerman",
    "Kermanshah",
    "Khuzestan",
    "Kohgiluyeh_and_Boyer_Ahmad",
    "Kurdistan",
    "Lorestan",
    "Markazi",
    "Mazandaran",
    "Khorasan",
    "Qazvin",
    "Qom",
    "Khorasan",
    "Semnan",
    "Sistan_and_Baluchestan",
    "Khorasan",
    "Tehran",
    "West_Azerbaijan",
    "Yazd",
    "Zanjan"
  )
)


# ==============================================================================
# STEP 7: Validate the province crosswalk itself
# ==============================================================================


# ------------------------------------------------------------------------------
# 7.1 Confirm that every current province appears only once in the crosswalk.
# ------------------------------------------------------------------------------

duplicate_current_provinces <- province_crosswalk[
  ,
  .N,
  by = province_current
][
  N > 1
]

if (nrow(duplicate_current_provinces) > 0) {
  print(duplicate_current_provinces)
  
  stop(
    "At least one current province appears more than once in the crosswalk."
  )
}


# ------------------------------------------------------------------------------
# 7.2 Confirm that all 31 LFS provinces appear in the crosswalk.
# ------------------------------------------------------------------------------

lfs_provinces_not_in_crosswalk <- setdiff(
  lfs_current_provinces,
  province_crosswalk$province_current
)

if (length(lfs_provinces_not_in_crosswalk) > 0) {
  
  cat(
    "\nLFS provinces missing from the crosswalk:\n"
  )
  
  print(lfs_provinces_not_in_crosswalk)
  
  stop(
    "Some LFS provinces do not have a historical-region mapping."
  )
}


# ------------------------------------------------------------------------------
# 7.3 Confirm that the crosswalk does not contain provinces absent from LFS.
# ------------------------------------------------------------------------------

crosswalk_provinces_not_in_lfs <- setdiff(
  province_crosswalk$province_current,
  lfs_current_provinces
)

if (length(crosswalk_provinces_not_in_lfs) > 0) {
  
  cat(
    "\nCrosswalk provinces not found in the LFS:\n"
  )
  
  print(crosswalk_provinces_not_in_lfs)
}


# ------------------------------------------------------------------------------
# 7.4 Confirm that all historical regions exist in the exposure panel.
# ------------------------------------------------------------------------------

crosswalk_regions_not_in_exposure <- setdiff(
  unique(province_crosswalk$province_historical),
  exposure_historical_provinces
)

if (length(crosswalk_regions_not_in_exposure) > 0) {
  
  cat(
    "\nHistorical regions in the crosswalk but not in exposure panel:\n"
  )
  
  print(crosswalk_regions_not_in_exposure)
  
  stop(
    "The historical province crosswalk does not match the exposure panel."
  )
}


# ------------------------------------------------------------------------------
# 7.5 Confirm that the mapping produces exactly 28 historical regions.
# ------------------------------------------------------------------------------

number_historical_regions_crosswalk <- uniqueN(
  province_crosswalk$province_historical
)

cat(
  "\nHistorical regions produced by crosswalk:",
  number_historical_regions_crosswalk,
  "\n"
)

if (number_historical_regions_crosswalk != 28) {
  stop(
    paste0(
      "The crosswalk does not produce exactly 28 historical regions.\n",
      "Number produced: ",
      number_historical_regions_crosswalk
    )
  )
}


# ==============================================================================
# STEP 8: Save the province crosswalk
#
# This file should be retained with the project replication materials.
# ==============================================================================

fwrite(
  province_crosswalk,
  file.path(
    output_folder,
    "step3_province_crosswalk_31_to_28.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Merge the crosswalk into the LFS dataset
#
# The merge is many-to-one:
#
#   Many individuals
#       -> One historical province mapping
#
# sort = FALSE attempts to preserve the original observation order.
# ==============================================================================

cat(
  "\nApplying the historical province crosswalk...\n"
)

dt[
  ,
  province_current_original :=
    as.character(province)
]

dt <- merge(
  dt,
  province_crosswalk,
  by.x = "province_current_original",
  by.y = "province_current",
  all.x = TRUE,
  sort = FALSE
)

setDT(dt)


# ==============================================================================
# STEP 10: Validate the current-province merge
# ==============================================================================


# ------------------------------------------------------------------------------
# 10.1 Confirm that the number of observations did not change.
# ------------------------------------------------------------------------------

if (nrow(dt) != original_number_of_rows) {
  stop(
    paste0(
      "The province merge changed the number of LFS observations.\n",
      "Before merge: ",
      original_number_of_rows,
      "\nAfter merge: ",
      nrow(dt)
    )
  )
}


# ------------------------------------------------------------------------------
# 10.2 Confirm that no historical province is missing.
# ------------------------------------------------------------------------------

number_missing_historical_province <- dt[
  is.na(province_historical),
  .N
]

cat(
  "\nObservations with missing historical province:",
  format(
    number_missing_historical_province,
    big.mark = ","
  ),
  "\n"
)

if (number_missing_historical_province > 0) {
  
  print(
    dt[
      is.na(province_historical),
      .N,
      by = province_current_original
    ][
      order(-N)
    ]
  )
  
  stop(
    "Some LFS observations were not assigned to a historical province."
  )
}


# ------------------------------------------------------------------------------
# 10.3 Confirm that the resulting historical regions match the exposure panel.
# ------------------------------------------------------------------------------

lfs_historical_provinces <- sort(
  unique(
    dt$province_historical
  )
)

lfs_regions_not_in_exposure <- setdiff(
  lfs_historical_provinces,
  exposure_historical_provinces
)

if (length(lfs_regions_not_in_exposure) > 0) {
  
  print(lfs_regions_not_in_exposure)
  
  stop(
    "Some historical LFS regions do not appear in the exposure panel."
  )
}


# ==============================================================================
# STEP 11: Create a historical mapping for previous province
#
# previous_residence_province is only available for recent movers.
#
# We map it to the same historical geography so that later robustness analyses
# can assign recent movers according to their previous province.
# ==============================================================================

previous_province_crosswalk <- copy(
  province_crosswalk
)

setnames(
  previous_province_crosswalk,
  old = c(
    "province_current",
    "province_historical"
  ),
  new = c(
    "previous_province_current",
    "previous_province_historical"
  )
)

dt[
  ,
  previous_province_current :=
    as.character(previous_residence_province)
]

dt <- merge(
  dt,
  previous_province_crosswalk,
  by = "previous_province_current",
  all.x = TRUE,
  sort = FALSE
)

setDT(dt)


# ==============================================================================
# STEP 12: Validate the previous-province mapping
#
# Missing historical previous province is expected when no previous province
# was reported.
#
# It is not acceptable when a current previous-province value exists but the
# crosswalk fails to map it.
# ==============================================================================

unmapped_reported_previous_provinces <- dt[
  !is.na(previous_province_current) &
    is.na(previous_province_historical),
  .N,
  by = previous_province_current
][
  order(-N)
]

if (nrow(unmapped_reported_previous_provinces) > 0) {
  
  cat(
    "\nReported previous provinces that could not be mapped:\n"
  )
  
  print(unmapped_reported_previous_provinces)
  
  stop(
    "Some reported previous provinces could not be harmonized."
  )
}


# ==============================================================================
# STEP 13: Create a harmonized province-change indicator
#
# This indicator compares historical regions rather than current province names.
#
# Example:
#   A move from Tehran to Alborz is not classified as a cross-historical-region
#   move because both belong to the historical Tehran region.
#
# Similarly:
#   A move between North, Razavi, and South Khorasan is not classified as a
#   cross-historical-region move.
# ==============================================================================

dt[
  ,
  recent_cross_historical_region_move :=
    fcase(
      
      !is.na(previous_province_historical) &
        !is.na(province_historical),
      
      as.integer(
        previous_province_historical != province_historical
      ),
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 14: Create province mapping diagnostics
# ==============================================================================


# ------------------------------------------------------------------------------
# 14.1 Current province to historical-region counts
# ------------------------------------------------------------------------------

current_province_mapping_diagnostic <- dt[
  ,
  .(
    observations = .N,
    sum_of_weights = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    province_current_original,
    province_historical
  )
][
  order(
    province_historical,
    province_current_original
  )
]

fwrite(
  current_province_mapping_diagnostic,
  file.path(
    output_folder,
    "step3_current_province_mapping_diagnostic.csv"
  ),
  bom = TRUE
)


# ------------------------------------------------------------------------------
# 14.2 Historical-region counts by survey year
# ------------------------------------------------------------------------------

historical_region_by_year <- dt[
  ,
  .(
    observations = .N,
    sum_of_weights = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    province_historical
  )
][
  order(
    year,
    province_historical
  )
]

fwrite(
  historical_region_by_year,
  file.path(
    output_folder,
    "step3_historical_region_by_year.csv"
  ),
  bom = TRUE
)


# ------------------------------------------------------------------------------
# 14.3 Number of historical regions observed in each survey year
# ------------------------------------------------------------------------------

historical_region_coverage_by_year <- dt[
  ,
  .(
    observations = .N,
    
    number_historical_regions = uniqueN(
      province_historical,
      na.rm = TRUE
    ),
    
    number_current_provinces = uniqueN(
      province_current_original,
      na.rm = TRUE
    ),
    
    missing_historical_region = sum(
      is.na(province_historical)
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  historical_region_coverage_by_year,
  file.path(
    output_folder,
    "step3_historical_region_coverage_by_year.csv"
  ),
  bom = TRUE
)


# ------------------------------------------------------------------------------
# 14.4 Previous province mapping diagnostics
# ------------------------------------------------------------------------------

previous_province_mapping_diagnostic <- dt[
  !is.na(previous_province_current),
  .(
    observations = .N
  ),
  by = .(
    previous_province_current,
    previous_province_historical
  )
][
  order(
    previous_province_historical,
    previous_province_current
  )
]

fwrite(
  previous_province_mapping_diagnostic,
  file.path(
    output_folder,
    "step3_previous_province_mapping_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Recreate migration diagnostics for the eligible sample
#
# Earlier migration diagnostics were calculated before the exposure-eligibility
# filter. This table is based only on the current Step 3 analysis sample.
# ==============================================================================

eligible_sample_migration_diagnostic <- dt[
  ,
  .(
    observations = .N,
    
    any_recent_move = sum(
      recent_move_reported == 1,
      na.rm = TRUE
    ),
    
    recent_interprovincial_movers = sum(
      recent_interprovincial_migrant == 1,
      na.rm = TRUE
    ),
    
    previous_province_reported = sum(
      !is.na(previous_province_current)
    ),
    
    cross_historical_region_movers = sum(
      recent_cross_historical_region_move == 1,
      na.rm = TRUE
    ),
    
    same_historical_region_movers = sum(
      recent_cross_historical_region_move == 0,
      na.rm = TRUE
    ),
    
    percent_cross_historical_region_movers = round(
      100 * mean(
        recent_cross_historical_region_move == 1,
        na.rm = TRUE
      ),
      4
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  eligible_sample_migration_diagnostic,
  file.path(
    output_folder,
    "step3_eligible_sample_migration_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Confirm that every original Step 2 column remains available
# ==============================================================================

missing_original_columns <- setdiff(
  original_column_names,
  names(dt)
)

if (length(missing_original_columns) > 0) {
  
  print(missing_original_columns)
  
  stop(
    "Some original Step 2 columns were unexpectedly lost."
  )
}

cat(
  "\nAll original Step 2 columns were preserved.\n"
)

cat(
  "New Step 3 columns added:",
  ncol(dt) - original_number_of_columns,
  "\n"
)


# ==============================================================================
# STEP 17: Save the province-harmonized analysis dataset
# ==============================================================================

cat(
  "\nSaving the province-harmonized LFS dataset...\n"
)

write_fst(
  dt,
  output_lfs_file,
  compress = 30
)

if (!file.exists(output_lfs_file)) {
  stop(
    "The Step 3 FST file was not saved successfully."
  )
}

output_file_size_gb <- file.info(
  output_lfs_file
)$size / (1024^3)


# ==============================================================================
# STEP 18: Save a Step 3 completion summary
# ==============================================================================

step3_completion_summary <- data.table(
  
  item = c(
    "Input observations",
    "Output observations",
    "Input columns",
    "Output columns",
    "New columns added",
    "Current LFS provinces",
    "Historical regions",
    "Missing historical province observations",
    "Output file size in GB",
    "Output file"
  ),
  
  value = c(
    as.character(original_number_of_rows),
    as.character(nrow(dt)),
    as.character(original_number_of_columns),
    as.character(ncol(dt)),
    as.character(
      ncol(dt) - original_number_of_columns
    ),
    as.character(
      uniqueN(
        dt$province_current_original,
        na.rm = TRUE
      )
    ),
    as.character(
      uniqueN(
        dt$province_historical,
        na.rm = TRUE
      )
    ),
    as.character(
      sum(
        is.na(dt$province_historical)
      )
    ),
    as.character(
      round(
        output_file_size_gb,
        3
      )
    ),
    output_lfs_file
  )
)

fwrite(
  step3_completion_summary,
  file.path(
    output_folder,
    "step3_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Print final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 3 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
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
  "Current provinces:",
  uniqueN(
    dt$province_current_original,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Historical regions:",
  uniqueN(
    dt$province_historical,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Missing historical province assignments:",
  sum(
    is.na(dt$province_historical)
  ),
  "\n"
)

cat(
  "\nOutput file size:",
  round(
    output_file_size_gb,
    3
  ),
  "GB\n"
)

cat(
  "\nProvince-harmonized dataset saved at:\n",
  output_lfs_file,
  "\n"
)

cat(
  "\nImportant Step 3 diagnostic files:\n",
  "1. step3_completion_summary.csv\n",
  "2. step3_province_crosswalk_31_to_28.csv\n",
  "3. step3_current_province_mapping_diagnostic.csv\n",
  "4. step3_historical_region_coverage_by_year.csv\n",
  "5. step3_previous_province_mapping_diagnostic.csv\n",
  "6. step3_eligible_sample_migration_diagnostic.csv\n"
)


# ==============================================================================
# STEP 20: Remove large objects from memory after saving
# ==============================================================================

rm(
  dt,
  exposure_panel,
  province_crosswalk,
  previous_province_crosswalk
)

invisible(gc())

cat(
  "\nLarge in-memory objects were removed after saving.\n"
)

















# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 4: Merge Undergraduate University Exposure at Ages 18, 19, and 20
#
# Purpose:
#   1. Load the province-harmonized LFS dataset from Step 3.
#   2. Load the historical province-year university exposure panel.
#   3. Validate the exposure panel before merging.
#   4. Treat false zero undergraduate enrollment values as missing.
#   5. Merge undergraduate exposure when respondents were ages 18, 19, and 20.
#   6. Construct the baseline three-year average exposure measure.
#   7. Construct alternative exposure measures for robustness checks.
#   8. Produce detailed merge and missingness diagnostics.
#   9. Save the core analysis dataset.
#
# Baseline treatment:
#
#   Average undergraduate/professional enrollment per 100,000 population
#   in the respondent's historical province when the respondent was ages
#   18, 19, and 20.
#
# Important:
#   - The false zero values in the exposure panel represent missing information.
#   - The baseline exposure requires all three years to be nonmissing.
#   - A two-of-three-years average is retained only for robustness analysis.
#   - The Step 3 dataset is not overwritten.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load required packages
# ==============================================================================

rm(list = ls())

invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 100)

required_packages <- c(
  "data.table",
  "fst"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

# Province-harmonized LFS file created in Step 3.
input_lfs_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_step3_historical_provinces_1386_1403.fst"
)

# Historical province-year university exposure panel.
exposure_file <- file.path(
  project_folder,
  "final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv"
)

# Output folder.
output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

# Core analysis dataset created in this step.
output_lfs_file <- file.path(
  output_folder,
  "lfs_step4_exposure_merged_core_analysis.fst"
)

if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}

if (!file.exists(input_lfs_file)) {
  stop(
    paste0(
      "The Step 3 LFS file was not found:\n",
      input_lfs_file
    )
  )
}

if (!file.exists(exposure_file)) {
  stop(
    paste0(
      "The exposure panel was not found:\n",
      exposure_file
    )
  )
}


# ==============================================================================
# STEP 2: Load the Step 3 LFS dataset
# ==============================================================================

cat(
  "\nReading the Step 3 province-harmonized LFS dataset...\n"
)

dt <- as.data.table(
  read_fst(input_lfs_file)
)

input_number_of_rows <- nrow(dt)
input_number_of_columns <- ncol(dt)
input_column_names <- names(dt)

cat(
  "\nLFS observations:",
  format(input_number_of_rows, big.mark = ","),
  "\n"
)

cat(
  "LFS columns:",
  input_number_of_columns,
  "\n"
)


# ==============================================================================
# STEP 3: Confirm that required LFS variables are available
# ==============================================================================

required_lfs_variables <- c(
  "province_historical",
  "exposure_year_age18",
  "exposure_year_age19",
  "exposure_year_age20",
  "analysis_birth_year",
  "year",
  "age",
  "weight"
)

missing_lfs_variables <- setdiff(
  required_lfs_variables,
  names(dt)
)

if (length(missing_lfs_variables) > 0) {
  stop(
    paste0(
      "The following required LFS variables are missing:\n",
      paste(
        missing_lfs_variables,
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# STEP 4: Standardize the LFS merge variables
# ==============================================================================

dt[
  ,
  province_historical :=
    trimws(
      as.character(province_historical)
    )
]

dt[
  ,
  exposure_year_age18 :=
    as.integer(exposure_year_age18)
]

dt[
  ,
  exposure_year_age19 :=
    as.integer(exposure_year_age19)
]

dt[
  ,
  exposure_year_age20 :=
    as.integer(exposure_year_age20)
]


# ==============================================================================
# STEP 5: Load only the needed exposure-panel variables
#
# We retain:
#
#   - Historical province
#   - Exposure year
#   - Undergraduate/professional enrollment count
#   - Population
#   - Undergraduate/professional enrollment per 100,000
#   - Clean undergraduate/professional exposure measure
#
# The graduate enrollment measures are not merged because ages of entry into
# master's and doctoral programs vary substantially.
# ==============================================================================

cat(
  "\nReading the undergraduate exposure panel...\n"
)

exposure <- fread(
  exposure_file,
  encoding = "UTF-8",
  select = c(
    "exposure_year",
    "province_historical",
    "enroll_undergrad_professional",
    "population_000s",
    "undergrad_professional_per_100k",
    "undergrad_professional_per_100k_clean"
  )
)

setDT(exposure)


# ==============================================================================
# STEP 6: Standardize exposure-panel variables
# ==============================================================================

exposure[
  ,
  exposure_year := as.integer(exposure_year)
]

exposure[
  ,
  province_historical :=
    trimws(
      as.character(province_historical)
    )
]

exposure[
  ,
  enroll_undergrad_professional :=
    as.numeric(enroll_undergrad_professional)
]

exposure[
  ,
  population_000s :=
    as.numeric(population_000s)
]

exposure[
  ,
  undergrad_professional_per_100k :=
    as.numeric(undergrad_professional_per_100k)
]

exposure[
  ,
  undergrad_professional_per_100k_clean :=
    as.numeric(undergrad_professional_per_100k_clean)
]


# ==============================================================================
# STEP 7: Enforce the missing-value rule for false zero exposure
#
# The zero undergraduate enrollment observations are treated as missing because
# they represent unavailable data rather than genuine zero university access.
#
# We apply the rule both to:
#
#   - Undergraduate enrollment count
#   - Undergraduate enrollment per 100,000
#   - Clean undergraduate exposure
# ==============================================================================

exposure[
  is.na(enroll_undergrad_professional) |
    enroll_undergrad_professional <= 0,
  `:=`(
    enroll_undergrad_professional = NA_real_,
    undergrad_professional_per_100k = NA_real_,
    undergrad_professional_per_100k_clean = NA_real_
  )
]


# ==============================================================================
# STEP 8: Validate the exposure panel
# ==============================================================================


# ------------------------------------------------------------------------------
# 8.1 Check year coverage.
# ------------------------------------------------------------------------------

minimum_exposure_year <- min(
  exposure$exposure_year,
  na.rm = TRUE
)

maximum_exposure_year <- max(
  exposure$exposure_year,
  na.rm = TRUE
)

cat(
  "\nExposure-panel years:",
  minimum_exposure_year,
  "to",
  maximum_exposure_year,
  "\n"
)

if (
  minimum_exposure_year != 1373 ||
  maximum_exposure_year != 1403
) {
  warning(
    "The exposure-panel year range differs from the expected 1373–1403."
  )
}


# ------------------------------------------------------------------------------
# 8.2 Check the number of historical regions.
# ------------------------------------------------------------------------------

number_exposure_regions <- uniqueN(
  exposure$province_historical,
  na.rm = TRUE
)

cat(
  "Historical regions in exposure panel:",
  number_exposure_regions,
  "\n"
)

if (number_exposure_regions != 28) {
  stop(
    paste0(
      "The exposure panel does not contain exactly 28 historical regions.\n",
      "Number found: ",
      number_exposure_regions
    )
  )
}


# ------------------------------------------------------------------------------
# 8.3 Check for duplicate province-year observations.
# ------------------------------------------------------------------------------

duplicate_exposure_keys <- exposure[
  ,
  .N,
  by = .(
    province_historical,
    exposure_year
  )
][
  N > 1
]

if (nrow(duplicate_exposure_keys) > 0) {
  print(duplicate_exposure_keys)
  
  stop(
    "The exposure panel contains duplicate historical province-year records."
  )
}


# ------------------------------------------------------------------------------
# 8.4 Check whether all LFS historical provinces exist in the exposure panel.
# ------------------------------------------------------------------------------

lfs_regions_not_in_exposure <- setdiff(
  unique(dt$province_historical),
  unique(exposure$province_historical)
)

if (length(lfs_regions_not_in_exposure) > 0) {
  cat(
    "\nLFS historical regions absent from the exposure panel:\n"
  )
  
  print(lfs_regions_not_in_exposure)
  
  stop(
    "Some LFS historical regions cannot be matched to the exposure panel."
  )
}


# ------------------------------------------------------------------------------
# 8.5 Count false-zero or missing undergraduate exposure observations.
# ------------------------------------------------------------------------------

exposure_missingness_diagnostic <- exposure[
  ,
  .(
    province_year_observations = .N,
    
    missing_undergraduate_count = sum(
      is.na(enroll_undergrad_professional)
    ),
    
    missing_undergraduate_per_100k = sum(
      is.na(undergrad_professional_per_100k_clean)
    ),
    
    nonmissing_undergraduate_per_100k = sum(
      !is.na(undergrad_professional_per_100k_clean)
    )
  )
]

cat(
  "\nExposure-panel missingness after treating false zeros as missing:\n"
)

print(exposure_missingness_diagnostic)


# ==============================================================================
# STEP 9: Save the cleaned exposure panel used for merging
#
# This provides an exact record of the treatment data used in the analysis.
# ==============================================================================

fwrite(
  exposure,
  file.path(
    output_folder,
    "step4_clean_undergraduate_exposure_panel.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 10: Prepare age-18 exposure lookup table
# ==============================================================================

exposure_age18 <- exposure[
  ,
  .(
    province_historical,
    exposure_year_age18 = exposure_year,
    
    undergrad_enrollment_age18 =
      enroll_undergrad_professional,
    
    population_000s_age18 =
      population_000s,
    
    undergrad_per_100k_age18_raw =
      undergrad_professional_per_100k,
    
    undergrad_per_100k_age18 =
      undergrad_professional_per_100k_clean
  )
]

setkey(
  exposure_age18,
  province_historical,
  exposure_year_age18
)


# ==============================================================================
# STEP 11: Merge age-18 exposure using an update join
#
# An update join is used because it is more memory efficient than repeatedly
# creating full merged copies of the 4.1-million-observation LFS dataset.
# ==============================================================================

cat(
  "\nMerging undergraduate exposure at age 18...\n"
)

dt[
  exposure_age18,
  on = .(
    province_historical,
    exposure_year_age18
  ),
  `:=`(
    undergrad_enrollment_age18 =
      i.undergrad_enrollment_age18,
    
    population_000s_age18 =
      i.population_000s_age18,
    
    undergrad_per_100k_age18_raw =
      i.undergrad_per_100k_age18_raw,
    
    undergrad_per_100k_age18 =
      i.undergrad_per_100k_age18
  )
]

rm(exposure_age18)

invisible(gc())


# ==============================================================================
# STEP 12: Prepare and merge age-19 exposure
# ==============================================================================

exposure_age19 <- exposure[
  ,
  .(
    province_historical,
    exposure_year_age19 = exposure_year,
    
    undergrad_enrollment_age19 =
      enroll_undergrad_professional,
    
    population_000s_age19 =
      population_000s,
    
    undergrad_per_100k_age19_raw =
      undergrad_professional_per_100k,
    
    undergrad_per_100k_age19 =
      undergrad_professional_per_100k_clean
  )
]

setkey(
  exposure_age19,
  province_historical,
  exposure_year_age19
)

cat(
  "Merging undergraduate exposure at age 19...\n"
)

dt[
  exposure_age19,
  on = .(
    province_historical,
    exposure_year_age19
  ),
  `:=`(
    undergrad_enrollment_age19 =
      i.undergrad_enrollment_age19,
    
    population_000s_age19 =
      i.population_000s_age19,
    
    undergrad_per_100k_age19_raw =
      i.undergrad_per_100k_age19_raw,
    
    undergrad_per_100k_age19 =
      i.undergrad_per_100k_age19
  )
]

rm(exposure_age19)

invisible(gc())


# ==============================================================================
# STEP 13: Prepare and merge age-20 exposure
# ==============================================================================

exposure_age20 <- exposure[
  ,
  .(
    province_historical,
    exposure_year_age20 = exposure_year,
    
    undergrad_enrollment_age20 =
      enroll_undergrad_professional,
    
    population_000s_age20 =
      population_000s,
    
    undergrad_per_100k_age20_raw =
      undergrad_professional_per_100k,
    
    undergrad_per_100k_age20 =
      undergrad_professional_per_100k_clean
  )
]

setkey(
  exposure_age20,
  province_historical,
  exposure_year_age20
)

cat(
  "Merging undergraduate exposure at age 20...\n"
)

dt[
  exposure_age20,
  on = .(
    province_historical,
    exposure_year_age20
  ),
  `:=`(
    undergrad_enrollment_age20 =
      i.undergrad_enrollment_age20,
    
    population_000s_age20 =
      i.population_000s_age20,
    
    undergrad_per_100k_age20_raw =
      i.undergrad_per_100k_age20_raw,
    
    undergrad_per_100k_age20 =
      i.undergrad_per_100k_age20
  )
]

rm(exposure_age20)

invisible(gc())


# ==============================================================================
# STEP 14: Verify that the exposure merge did not change the number of rows
# ==============================================================================

if (nrow(dt) != input_number_of_rows) {
  stop(
    paste0(
      "The exposure merge changed the number of observations.\n",
      "Before merge: ",
      input_number_of_rows,
      "\nAfter merge: ",
      nrow(dt)
    )
  )
}


# ==============================================================================
# STEP 15: Create age-specific exposure availability indicators
# ==============================================================================

dt[
  ,
  exposure_age18_available :=
    as.integer(
      !is.na(undergrad_per_100k_age18)
    )
]

dt[
  ,
  exposure_age19_available :=
    as.integer(
      !is.na(undergrad_per_100k_age19)
    )
]

dt[
  ,
  exposure_age20_available :=
    as.integer(
      !is.na(undergrad_per_100k_age20)
    )
]

dt[
  ,
  number_exposure_years_available :=
    exposure_age18_available +
    exposure_age19_available +
    exposure_age20_available
]


# ==============================================================================
# STEP 16: Construct the baseline three-year average exposure
#
# The baseline requires valid exposure in all three years.
#
# We avoid rowMeans() on the full dataset because direct arithmetic is more
# memory efficient.
# ==============================================================================

dt[
  ,
  undergrad_exposure_18_20 :=
    fifelse(
      number_exposure_years_available == 3L,
      
      (
        undergrad_per_100k_age18 +
          undergrad_per_100k_age19 +
          undergrad_per_100k_age20
      ) / 3,
      
      NA_real_
    )
]


# ==============================================================================
# STEP 17: Construct the log of the baseline average exposure
#
# We take the logarithm after averaging the three annual exposure values:
#
#   log(average exposure)
#
# This differs from averaging the logs of the three annual values.
# ==============================================================================

dt[
  ,
  log_undergrad_exposure_18_20 :=
    fifelse(
      !is.na(undergrad_exposure_18_20) &
        undergrad_exposure_18_20 > 0,
      
      log(undergrad_exposure_18_20),
      
      NA_real_
    )
]


# ==============================================================================
# STEP 18: Construct a two-of-three-years robustness measure
#
# This measure is not the baseline treatment.
#
# It averages the available annual exposure values when at least two of the
# three ages have valid exposure.
# ==============================================================================

dt[
  ,
  sum_available_undergrad_exposure :=
    fifelse(
      is.na(undergrad_per_100k_age18),
      0,
      undergrad_per_100k_age18
    ) +
    fifelse(
      is.na(undergrad_per_100k_age19),
      0,
      undergrad_per_100k_age19
    ) +
    fifelse(
      is.na(undergrad_per_100k_age20),
      0,
      undergrad_per_100k_age20
    )
]

dt[
  ,
  undergrad_exposure_18_20_two_of_three :=
    fifelse(
      number_exposure_years_available >= 2L,
      
      sum_available_undergrad_exposure /
        number_exposure_years_available,
      
      NA_real_
    )
]

dt[
  ,
  log_undergrad_exposure_18_20_two_of_three :=
    fifelse(
      !is.na(undergrad_exposure_18_20_two_of_three) &
        undergrad_exposure_18_20_two_of_three > 0,
      
      log(undergrad_exposure_18_20_two_of_three),
      
      NA_real_
    )
]


# ==============================================================================
# STEP 19: Create exposure completeness indicators
# ==============================================================================

dt[
  ,
  complete_undergrad_exposure :=
    as.integer(
      number_exposure_years_available == 3L
    )
]

dt[
  ,
  at_least_two_undergrad_exposure_years :=
    as.integer(
      number_exposure_years_available >= 2L
    )
]


# ==============================================================================
# STEP 20: Create a baseline estimation-sample indicator
#
# This indicator requires:
#
#   - Valid three-year undergraduate exposure
#   - Nonmissing historical province
#   - Positive and nonmissing survey weight
#
# Outcome-specific restrictions will be added later.
# ==============================================================================

dt[
  ,
  baseline_exposure_sample :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(province_historical) &
        !is.na(weight) &
        weight > 0
    )
]


# ==============================================================================
# STEP 21: Remove the temporary exposure-sum variable
#
# The final two-of-three measure has already been constructed, so this temporary
# variable is no longer needed.
# ==============================================================================

dt[
  ,
  sum_available_undergrad_exposure := NULL
]


# ==============================================================================
# STEP 22: Create observation-level merge diagnostics
# ==============================================================================

merge_availability_summary <- dt[
  ,
  .(
    observations = .N,
    
    age18_exposure_available = sum(
      exposure_age18_available == 1L
    ),
    
    age19_exposure_available = sum(
      exposure_age19_available == 1L
    ),
    
    age20_exposure_available = sum(
      exposure_age20_available == 1L
    ),
    
    all_three_exposures_available = sum(
      complete_undergrad_exposure == 1L
    ),
    
    at_least_two_exposures_available = sum(
      at_least_two_undergrad_exposure_years == 1L
    ),
    
    no_exposure_available = sum(
      number_exposure_years_available == 0L
    ),
    
    percent_all_three_available = round(
      100 * mean(
        complete_undergrad_exposure == 1L
      ),
      4
    ),
    
    percent_at_least_two_available = round(
      100 * mean(
        at_least_two_undergrad_exposure_years == 1L
      ),
      4
    )
  )
]

fwrite(
  merge_availability_summary,
  file.path(
    output_folder,
    "step4_overall_exposure_merge_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Create merge diagnostics by survey year
# ==============================================================================

merge_diagnostic_by_survey_year <- dt[
  ,
  .(
    observations = .N,
    
    complete_three_year_exposure = sum(
      complete_undergrad_exposure == 1L
    ),
    
    at_least_two_year_exposure = sum(
      at_least_two_undergrad_exposure_years == 1L
    ),
    
    missing_all_three_exposure = sum(
      number_exposure_years_available == 0L
    ),
    
    percent_complete_three_year_exposure = round(
      100 * mean(
        complete_undergrad_exposure == 1L
      ),
      4
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  merge_diagnostic_by_survey_year,
  file.path(
    output_folder,
    "step4_exposure_merge_by_survey_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Create merge diagnostics by historical province
# ==============================================================================

merge_diagnostic_by_province <- dt[
  ,
  .(
    observations = .N,
    
    complete_three_year_exposure = sum(
      complete_undergrad_exposure == 1L
    ),
    
    incomplete_three_year_exposure = sum(
      complete_undergrad_exposure == 0L
    ),
    
    percent_complete_three_year_exposure = round(
      100 * mean(
        complete_undergrad_exposure == 1L
      ),
      4
    ),
    
    minimum_complete_exposure = {
      x <- undergrad_exposure_18_20[
        !is.na(undergrad_exposure_18_20)
      ]
      
      if (length(x) == 0) {
        NA_real_
      } else {
        min(x)
      }
    },
    
    maximum_complete_exposure = {
      x <- undergrad_exposure_18_20[
        !is.na(undergrad_exposure_18_20)
      ]
      
      if (length(x) == 0) {
        NA_real_
      } else {
        max(x)
      }
    },
    
    mean_complete_exposure = mean(
      undergrad_exposure_18_20,
      na.rm = TRUE
    )
  ),
  by = province_historical
][
  order(province_historical)
]

fwrite(
  merge_diagnostic_by_province,
  file.path(
    output_folder,
    "step4_exposure_merge_by_historical_province.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Create merge diagnostics by birth cohort
# ==============================================================================

merge_diagnostic_by_birth_cohort <- dt[
  ,
  .(
    observations = .N,
    
    complete_three_year_exposure = sum(
      complete_undergrad_exposure == 1L
    ),
    
    percent_complete_three_year_exposure = round(
      100 * mean(
        complete_undergrad_exposure == 1L
      ),
      4
    ),
    
    mean_undergraduate_exposure = mean(
      undergrad_exposure_18_20,
      na.rm = TRUE
    )
  ),
  by = analysis_birth_year
][
  order(analysis_birth_year)
]

fwrite(
  merge_diagnostic_by_birth_cohort,
  file.path(
    output_folder,
    "step4_exposure_merge_by_birth_cohort.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Identify the exact province-years responsible for missing exposure
#
# This table is useful because missing exposure should arise from the known
# false-zero province-year observations rather than failed province matching.
# ==============================================================================

missing_exposure_keys <- rbindlist(
  list(
    
    dt[
      is.na(undergrad_per_100k_age18),
      .(
        age_at_exposure = 18L,
        province_historical,
        exposure_year = exposure_year_age18,
        affected_observations = .N
      ),
      by = .(
        province_historical,
        exposure_year_age18
      )
    ][
      ,
      exposure_year_age18 := NULL
    ],
    
    dt[
      is.na(undergrad_per_100k_age19),
      .(
        age_at_exposure = 19L,
        province_historical,
        exposure_year = exposure_year_age19,
        affected_observations = .N
      ),
      by = .(
        province_historical,
        exposure_year_age19
      )
    ][
      ,
      exposure_year_age19 := NULL
    ],
    
    dt[
      is.na(undergrad_per_100k_age20),
      .(
        age_at_exposure = 20L,
        province_historical,
        exposure_year = exposure_year_age20,
        affected_observations = .N
      ),
      by = .(
        province_historical,
        exposure_year_age20
      )
    ][
      ,
      exposure_year_age20 := NULL
    ]
  ),
  
  use.names = TRUE,
  fill = TRUE
)

setorder(
  missing_exposure_keys,
  age_at_exposure,
  province_historical,
  exposure_year
)

fwrite(
  missing_exposure_keys,
  file.path(
    output_folder,
    "step4_missing_exposure_keys.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Create exposure distribution diagnostics
# ==============================================================================

exposure_distribution <- dt[
  complete_undergrad_exposure == 1L,
  .(
    observations = .N,
    
    minimum = min(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    percentile_1 = quantile(
      undergrad_exposure_18_20,
      probs = 0.01,
      na.rm = TRUE,
      names = FALSE
    ),
    
    percentile_10 = quantile(
      undergrad_exposure_18_20,
      probs = 0.10,
      na.rm = TRUE,
      names = FALSE
    ),
    
    percentile_25 = quantile(
      undergrad_exposure_18_20,
      probs = 0.25,
      na.rm = TRUE,
      names = FALSE
    ),
    
    median = median(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    mean = mean(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    percentile_75 = quantile(
      undergrad_exposure_18_20,
      probs = 0.75,
      na.rm = TRUE,
      names = FALSE
    ),
    
    percentile_90 = quantile(
      undergrad_exposure_18_20,
      probs = 0.90,
      na.rm = TRUE,
      names = FALSE
    ),
    
    percentile_99 = quantile(
      undergrad_exposure_18_20,
      probs = 0.99,
      na.rm = TRUE,
      names = FALSE
    ),
    
    maximum = max(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    standard_deviation = sd(
      undergrad_exposure_18_20,
      na.rm = TRUE
    )
  )
]

fwrite(
  exposure_distribution,
  file.path(
    output_folder,
    "step4_exposure_distribution.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 28: Confirm preservation of all Step 3 columns
# ==============================================================================

missing_input_columns <- setdiff(
  input_column_names,
  names(dt)
)

if (length(missing_input_columns) > 0) {
  print(missing_input_columns)
  
  stop(
    "Some Step 3 columns were unexpectedly lost."
  )
}

cat(
  "\nAll Step 3 columns were preserved.\n"
)

cat(
  "New Step 4 columns added:",
  ncol(dt) - input_number_of_columns,
  "\n"
)


# ==============================================================================
# STEP 29: Save the core analysis dataset
# ==============================================================================

cat(
  "\nSaving the exposure-merged core analysis dataset...\n"
)

write_fst(
  dt,
  output_lfs_file,
  compress = 30
)

if (!file.exists(output_lfs_file)) {
  stop(
    "The Step 4 core analysis file was not saved successfully."
  )
}

output_file_size_gb <- file.info(
  output_lfs_file
)$size / (1024^3)


# ==============================================================================
# STEP 30: Save a completion summary
# ==============================================================================

step4_completion_summary <- data.table(
  
  item = c(
    "Input observations",
    "Output observations",
    "Input columns",
    "Output columns",
    "New columns added",
    "Complete three-year exposure observations",
    "Incomplete three-year exposure observations",
    "Percent with complete three-year exposure",
    "At least two-year exposure observations",
    "Baseline exposure variable",
    "Log baseline exposure variable",
    "Output file size in GB",
    "Output file"
  ),
  
  value = c(
    as.character(input_number_of_rows),
    
    as.character(nrow(dt)),
    
    as.character(input_number_of_columns),
    
    as.character(ncol(dt)),
    
    as.character(
      ncol(dt) - input_number_of_columns
    ),
    
    as.character(
      sum(
        dt$complete_undergrad_exposure == 1L
      )
    ),
    
    as.character(
      sum(
        dt$complete_undergrad_exposure == 0L
      )
    ),
    
    as.character(
      round(
        100 * mean(
          dt$complete_undergrad_exposure == 1L
        ),
        4
      )
    ),
    
    as.character(
      sum(
        dt$at_least_two_undergrad_exposure_years == 1L
      )
    ),
    
    "undergrad_exposure_18_20",
    
    "log_undergrad_exposure_18_20",
    
    as.character(
      round(
        output_file_size_gb,
        3
      )
    ),
    
    output_lfs_file
  )
)

fwrite(
  step4_completion_summary,
  file.path(
    output_folder,
    "step4_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 31: Print final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 4 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
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
  "Complete three-year exposure:",
  format(
    sum(
      dt$complete_undergrad_exposure == 1L
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Percent with complete exposure:",
  round(
    100 * mean(
      dt$complete_undergrad_exposure == 1L
    ),
    4
  ),
  "%\n"
)

cat(
  "\nBaseline exposure variable:\n",
  "undergrad_exposure_18_20\n"
)

cat(
  "\nLog baseline exposure variable:\n",
  "log_undergrad_exposure_18_20\n"
)

cat(
  "\nOutput columns:",
  ncol(dt),
  "\n"
)

cat(
  "Output file size:",
  round(
    output_file_size_gb,
    3
  ),
  "GB\n"
)

cat(
  "\nCore analysis dataset saved at:\n",
  output_lfs_file,
  "\n"
)

cat(
  "\nImportant Step 4 diagnostic files:\n",
  "1. step4_completion_summary.csv\n",
  "2. step4_overall_exposure_merge_summary.csv\n",
  "3. step4_exposure_merge_by_survey_year.csv\n",
  "4. step4_exposure_merge_by_historical_province.csv\n",
  "5. step4_exposure_merge_by_birth_cohort.csv\n",
  "6. step4_missing_exposure_keys.csv\n",
  "7. step4_exposure_distribution.csv\n",
  "8. step4_clean_undergraduate_exposure_panel.csv\n"
)


# ==============================================================================
# STEP 32: Remove large objects from memory after saving
# ==============================================================================

rm(
  dt,
  exposure
)

invisible(gc())

cat(
  "\nLarge in-memory objects were removed after saving.\n"
)




# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 5A: Diagnose Education and Occupation Codes for Mismatch Construction
#
# Purpose:
#   1. Load the Step 4 core analysis dataset.
#   2. Inspect education-level coding.
#   3. Inspect education-field coding.
#   4. Inspect the number of digits and structure of ISCO occupation codes.
#   5. Identify the sample available for vertical mismatch.
#   6. Identify the sample available for horizontal mismatch.
#
# Important:
#   - This script does not change or overwrite the Step 4 dataset.
#   - It does not yet classify workers as matched or mismatched.
#   - Its purpose is to ensure that the final mismatch definitions are based on
#     the actual coding structure of the full LFS file.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load packages
# ==============================================================================

rm(list = ls())

invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 100)

required_packages <- c(
  "data.table",
  "fst",
  "stringr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_step4_exposure_merged_core_analysis.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "mismatch_diagnostics"
)

if (!dir.exists(output_folder)) {
  dir.create(
    output_folder,
    recursive = TRUE
  )
}

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The Step 4 core analysis file was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Load only variables needed for mismatch diagnostics
#
# Reading only selected columns reduces memory use substantially.
# ==============================================================================

variables_needed <- c(
  "year",
  "quarter",
  "age",
  "gender",
  "weight",
  "province_historical",
  "employment_status",
  "employed",
  "education_level",
  "educ_level",
  "education_field",
  "main_job_title_isco_code",
  "main_job_workplace_isic_code",
  "main_job_employment_status",
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure"
)

available_columns <- metadata_fst(input_file)$columnNames

missing_variables <- setdiff(
  variables_needed,
  available_columns
)

if (length(missing_variables) > 0) {
  stop(
    paste0(
      "The following required variables are missing:\n",
      paste(missing_variables, collapse = ", ")
    )
  )
}

cat("\nReading mismatch-related variables...\n")

dt <- as.data.table(
  read_fst(
    input_file,
    columns = variables_needed
  )
)

cat(
  "\nObservations:",
  format(nrow(dt), big.mark = ","),
  "\n"
)

cat(
  "Columns read:",
  ncol(dt),
  "\n"
)


# ==============================================================================
# STEP 3: Standardize the main codes
# ==============================================================================

dt[
  ,
  educ_level_num := suppressWarnings(
    as.integer(educ_level)
  )
]

dt[
  ,
  education_field_num := suppressWarnings(
    as.integer(education_field)
  )
]

dt[
  ,
  occupation_code_original :=
    str_squish(
      as.character(main_job_title_isco_code)
    )
]

dt[
  occupation_code_original %in% c(
    "",
    "NA",
    "NaN",
    "."
  ),
  occupation_code_original := NA_character_
]


# ==============================================================================
# STEP 4: Remove decimal artifacts from occupation codes
#
# Some numeric codes may be imported as text such as "2142.0".
# This step removes a trailing decimal zero without changing the code itself.
# ==============================================================================

dt[
  ,
  occupation_code_clean :=
    str_replace(
      occupation_code_original,
      "\\.0+$",
      ""
    )
]

dt[
  ,
  occupation_code_clean :=
    str_replace_all(
      occupation_code_clean,
      "[^0-9]",
      ""
    )
]

dt[
  occupation_code_clean == "",
  occupation_code_clean := NA_character_
]


# ==============================================================================
# STEP 5: Calculate occupation-code length
# ==============================================================================

dt[
  ,
  occupation_code_digits :=
    fifelse(
      !is.na(occupation_code_clean),
      nchar(occupation_code_clean),
      NA_integer_
    )
]


# ==============================================================================
# STEP 6: Create candidate ISCO aggregation levels
#
# These variables are provisional until the code structure is verified.
#
# ISCO major group:
#   First digit
#
# ISCO sub-major group:
#   First two digits
#
# ISCO minor group:
#   First three digits
#
# ISCO unit group:
#   First four digits
# ==============================================================================

dt[
  ,
  isco_1digit :=
    fifelse(
      !is.na(occupation_code_clean) &
        nchar(occupation_code_clean) >= 1,
      substr(occupation_code_clean, 1, 1),
      NA_character_
    )
]

dt[
  ,
  isco_2digit :=
    fifelse(
      !is.na(occupation_code_clean) &
        nchar(occupation_code_clean) >= 2,
      substr(occupation_code_clean, 1, 2),
      NA_character_
    )
]

dt[
  ,
  isco_3digit :=
    fifelse(
      !is.na(occupation_code_clean) &
        nchar(occupation_code_clean) >= 3,
      substr(occupation_code_clean, 1, 3),
      NA_character_
    )
]

dt[
  ,
  isco_4digit :=
    fifelse(
      !is.na(occupation_code_clean) &
        nchar(occupation_code_clean) >= 4,
      substr(occupation_code_clean, 1, 4),
      NA_character_
    )
]


# ==============================================================================
# STEP 7: Create readable education-level labels
#
# These values come from the Stata codebook.
#
# Codes 1 and 2 have labels "54" and "81" and should remain unresolved until
# their substantive meaning is verified.
# ==============================================================================

dt[
  ,
  education_level_clean :=
    fcase(
      educ_level_num == 1L, "Unresolved_Code_54",
      educ_level_num == 2L, "Unresolved_Code_81",
      educ_level_num == 3L, "Bachelors",
      educ_level_num == 4L, "Doctoral",
      educ_level_num == 5L, "Higher_Secondary",
      educ_level_num == 6L, "Lower_Secondary",
      educ_level_num == 7L, "Masters",
      educ_level_num == 8L, "Pre_University",
      educ_level_num == 9L, "Primary",
      educ_level_num == 10L, "Short_Cycle_Tertiary",
      educ_level_num == 11L, "Unofficial",
      default = NA_character_
    )
]


# ==============================================================================
# STEP 8: Identify tertiary-educated respondents
#
# Baseline tertiary categories:
#   Short-cycle tertiary
#   Bachelor's
#   Master's
#   Doctoral
# ==============================================================================

dt[
  ,
  tertiary_educated :=
    as.integer(
      educ_level_num %in%
        c(
          3L,  # Bachelor's
          4L,  # Doctoral
          7L,  # Master's
          10L  # Short-cycle tertiary
        )
    )
]


# ==============================================================================
# STEP 9: Create an ordered education rank
#
# This rank is used only to inspect vertical mismatch.
#
# It is not interpreted as exact years of schooling.
# ==============================================================================

dt[
  ,
  education_rank :=
    fcase(
      educ_level_num == 9L,  1L, # Primary
      educ_level_num == 6L,  2L, # Lower secondary
      educ_level_num == 5L,  3L, # Higher secondary
      educ_level_num == 8L,  4L, # Pre-university
      educ_level_num == 10L, 5L, # Short-cycle tertiary
      educ_level_num == 3L,  6L, # Bachelor's
      educ_level_num == 7L,  7L, # Master's
      educ_level_num == 4L,  8L, # Doctoral
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 10: Define potential mismatch samples
# ==============================================================================

dt[
  ,
  employed_with_occupation :=
    as.integer(
      employed == 1L &
        !is.na(occupation_code_clean)
    )
]

dt[
  ,
  vertical_mismatch_eligible :=
    as.integer(
      employed == 1L &
        !is.na(occupation_code_clean) &
        !is.na(education_rank)
    )
]

dt[
  ,
  horizontal_mismatch_eligible :=
    as.integer(
      employed == 1L &
        tertiary_educated == 1L &
        !is.na(occupation_code_clean) &
        !is.na(education_field_num)
    )
]


# ==============================================================================
# STEP 11: Education-level frequency table
# ==============================================================================

education_level_frequency <- dt[
  ,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    employed_observations = sum(
      employed == 1L,
      na.rm = TRUE
    ),
    occupation_available = sum(
      !is.na(occupation_code_clean)
    ),
    field_available = sum(
      !is.na(education_field_num)
    )
  ),
  by = .(
    educ_level_num,
    education_level,
    education_level_clean
  )
][
  order(educ_level_num)
]

fwrite(
  education_level_frequency,
  file.path(
    output_folder,
    "step5a_education_level_frequency.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Occupation-code digit structure by year
# ==============================================================================

occupation_digits_by_year <- dt[
  employed == 1L,
  .N,
  by = .(
    year,
    occupation_code_digits
  )
][
  order(
    year,
    occupation_code_digits
  )
]

occupation_digits_by_year[
  ,
  percent_within_year :=
    round(
      100 * N / sum(N),
      4
    ),
  by = year
]

fwrite(
  occupation_digits_by_year,
  file.path(
    output_folder,
    "step5a_occupation_code_digits_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Most common original occupation codes
# ==============================================================================

occupation_code_frequency <- dt[
  employed == 1L &
    !is.na(occupation_code_clean),
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    occupation_code_original,
    occupation_code_clean,
    occupation_code_digits,
    isco_1digit,
    isco_2digit,
    isco_3digit,
    isco_4digit
  )
][
  order(-observations)
]

fwrite(
  occupation_code_frequency,
  file.path(
    output_folder,
    "step5a_occupation_code_frequency.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 14: ISCO major-group coverage
# ==============================================================================

isco_major_group_by_year <- dt[
  employed == 1L,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    education_available = sum(
      !is.na(education_rank)
    ),
    education_field_available = sum(
      !is.na(education_field_num)
    )
  ),
  by = .(
    year,
    isco_1digit
  )
][
  order(
    year,
    isco_1digit
  )
]

fwrite(
  isco_major_group_by_year,
  file.path(
    output_folder,
    "step5a_isco_major_group_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: ISCO two-digit group coverage
# ==============================================================================

isco_2digit_frequency <- dt[
  employed == 1L &
    !is.na(isco_2digit),
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    number_of_education_levels = uniqueN(
      education_rank,
      na.rm = TRUE
    ),
    number_of_fields = uniqueN(
      education_field_num,
      na.rm = TRUE
    )
  ),
  by = isco_2digit
][
  order(isco_2digit)
]

fwrite(
  isco_2digit_frequency,
  file.path(
    output_folder,
    "step5a_isco_2digit_frequency.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Education-field frequency among tertiary graduates
# ==============================================================================

education_field_frequency <- dt[
  tertiary_educated == 1L,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    employed_observations = sum(
      employed == 1L,
      na.rm = TRUE
    ),
    occupation_available = sum(
      !is.na(occupation_code_clean)
    ),
    horizontal_mismatch_eligible = sum(
      horizontal_mismatch_eligible == 1L
    )
  ),
  by = .(
    education_field_num
  )
][
  order(education_field_num)
]

fwrite(
  education_field_frequency,
  file.path(
    output_folder,
    "step5a_education_field_frequency.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Field-by-occupation table
#
# This table will be used to design and validate the horizontal mismatch
# crosswalk.
# ==============================================================================

field_by_occupation <- dt[
  horizontal_mismatch_eligible == 1L,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    education_field_num,
    isco_1digit,
    isco_2digit,
    isco_3digit,
    isco_4digit
  )
][
  order(
    education_field_num,
    -observations
  )
]

fwrite(
  field_by_occupation,
  file.path(
    output_folder,
    "step5a_field_by_occupation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 18: Mismatch sample sizes by year
# ==============================================================================

mismatch_sample_by_year <- dt[
  ,
  .(
    total_observations = .N,
    
    employed = sum(
      employed == 1L,
      na.rm = TRUE
    ),
    
    employed_with_occupation = sum(
      employed_with_occupation == 1L,
      na.rm = TRUE
    ),
    
    vertical_mismatch_eligible = sum(
      vertical_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    
    tertiary_educated = sum(
      tertiary_educated == 1L,
      na.rm = TRUE
    ),
    
    horizontal_mismatch_eligible = sum(
      horizontal_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    
    percent_vertical_eligible_among_employed = round(
      100 *
        sum(vertical_mismatch_eligible == 1L) /
        sum(employed == 1L),
      4
    ),
    
    percent_horizontal_eligible_among_tertiary_employed = {
      denominator <- sum(
        employed == 1L &
          tertiary_educated == 1L
      )
      
      if (denominator == 0) {
        NA_real_
      } else {
        round(
          100 *
            sum(horizontal_mismatch_eligible == 1L) /
            denominator,
          4
        )
      }
    }
  ),
  by = year
][
  order(year)
]

fwrite(
  mismatch_sample_by_year,
  file.path(
    output_folder,
    "step5a_mismatch_sample_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 19: Education distributions within occupations
#
# This table will support the realized-matches vertical mismatch method.
#
# We initially use three-digit occupation groups because they provide more
# occupational detail than one- or two-digit groups while generally retaining
# larger cell sizes than four-digit groups.
# ==============================================================================

education_by_isco3 <- dt[
  vertical_mismatch_eligible == 1L &
    !is.na(isco_3digit),
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    isco_3digit,
    education_rank,
    education_level_clean
  )
][
  order(
    year,
    isco_3digit,
    education_rank
  )
]

fwrite(
  education_by_isco3,
  file.path(
    output_folder,
    "step5a_education_by_isco3_and_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 20: Cell-size diagnostics for potential realized-match groups
#
# Vertical mismatch should not be calculated from very small occupation cells.
#
# We inspect cell sizes for:
#   - ISCO 2-digit by broad period
#   - ISCO 3-digit by broad period
#   - ISCO 4-digit by broad period
#
# Broad periods reduce sensitivity to yearly sampling variation.
# ==============================================================================

dt[
  ,
  survey_period :=
    fcase(
      year >= 1386 & year <= 1391, "1386_1391",
      year >= 1392 & year <= 1397, "1392_1397",
      year >= 1398 & year <= 1403, "1398_1403",
      default = NA_character_
    )
]

cell_size_isco2 <- dt[
  vertical_mismatch_eligible == 1L &
    !is.na(isco_2digit),
  .N,
  by = .(
    survey_period,
    isco_2digit
  )
]

cell_size_isco3 <- dt[
  vertical_mismatch_eligible == 1L &
    !is.na(isco_3digit),
  .N,
  by = .(
    survey_period,
    isco_3digit
  )
]

cell_size_isco4 <- dt[
  vertical_mismatch_eligible == 1L &
    !is.na(isco_4digit),
  .N,
  by = .(
    survey_period,
    isco_4digit
  )
]

cell_size_summary <- rbindlist(
  list(
    cell_size_isco2[
      ,
      .(
        occupation_level = "ISCO_2digit",
        number_cells = .N,
        minimum_cell_size = min(N),
        percentile_10 = quantile(
          N,
          0.10,
          names = FALSE
        ),
        median_cell_size = median(N),
        mean_cell_size = mean(N),
        percent_cells_below_30 =
          100 * mean(N < 30),
        percent_cells_below_50 =
          100 * mean(N < 50)
      )
    ],
    
    cell_size_isco3[
      ,
      .(
        occupation_level = "ISCO_3digit",
        number_cells = .N,
        minimum_cell_size = min(N),
        percentile_10 = quantile(
          N,
          0.10,
          names = FALSE
        ),
        median_cell_size = median(N),
        mean_cell_size = mean(N),
        percent_cells_below_30 =
          100 * mean(N < 30),
        percent_cells_below_50 =
          100 * mean(N < 50)
      )
    ],
    
    cell_size_isco4[
      ,
      .(
        occupation_level = "ISCO_4digit",
        number_cells = .N,
        minimum_cell_size = min(N),
        percentile_10 = quantile(
          N,
          0.10,
          names = FALSE
        ),
        median_cell_size = median(N),
        mean_cell_size = mean(N),
        percent_cells_below_30 =
          100 * mean(N < 30),
        percent_cells_below_50 =
          100 * mean(N < 50)
      )
    ]
  ),
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  cell_size_summary,
  file.path(
    output_folder,
    "step5a_vertical_mismatch_cell_size_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Save completion summary
# ==============================================================================

completion_summary <- data.table(
  item = c(
    "Input observations",
    "Employed observations",
    "Employed with occupation",
    "Vertical mismatch eligible",
    "Tertiary educated",
    "Horizontal mismatch eligible",
    "Minimum survey year",
    "Maximum survey year",
    "Output folder"
  ),
  
  value = c(
    as.character(nrow(dt)),
    
    as.character(
      sum(
        dt$employed == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$employed_with_occupation == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$vertical_mismatch_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$tertiary_educated == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$horizontal_mismatch_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      min(dt$year, na.rm = TRUE)
    ),
    
    as.character(
      max(dt$year, na.rm = TRUE)
    ),
    
    output_folder
  )
)

fwrite(
  completion_summary,
  file.path(
    output_folder,
    "step5a_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 5A COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
)

cat(
  "\nEmployed observations:",
  format(
    sum(dt$employed == 1L, na.rm = TRUE),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Vertical mismatch eligible:",
  format(
    sum(
      dt$vertical_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Horizontal mismatch eligible:",
  format(
    sum(
      dt$horizontal_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "\nDiagnostic files saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nUpload these files:\n",
  "1. step5a_completion_summary.csv\n",
  "2. step5a_education_level_frequency.csv\n",
  "3. step5a_occupation_code_digits_by_year.csv\n",
  "4. step5a_occupation_code_frequency.csv\n",
  "5. step5a_isco_2digit_frequency.csv\n",
  "6. step5a_education_field_frequency.csv\n",
  "7. step5a_mismatch_sample_by_year.csv\n",
  "8. step5a_vertical_mismatch_cell_size_summary.csv\n"
)


# ==============================================================================
# STEP 23: Remove the diagnostic dataset from memory
# ==============================================================================

rm(
  dt,
  education_level_frequency,
  occupation_digits_by_year,
  occupation_code_frequency,
  isco_major_group_by_year,
  isco_2digit_frequency,
  education_field_frequency,
  field_by_occupation,
  mismatch_sample_by_year,
  education_by_isco3,
  cell_size_isco2,
  cell_size_isco3,
  cell_size_isco4,
  cell_size_summary
)

invisible(gc())

cat(
  "\nMismatch diagnostic objects were removed from memory.\n"
)







# ==============================================================================
# STEP 5B-PREP: Export education-field codes and any available labels
# ==============================================================================

rm(list = ls())
invisible(gc())

library(haven)
library(data.table)

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "data_ready.dta"
)

output_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "mismatch_diagnostics",
  "education_field_codebook_diagnostic.csv"
)

# Read only education-related variables.
field_data <- read_dta(
  input_file,
  col_select = any_of(
    c(
      "education_field",
      "education_field_label",
      "education_level",
      "educ_level"
    )
  )
)

setDT(field_data)

cat(
  "\nVariables read:\n"
)

print(names(field_data))


# Check whether education_field contains attached Stata value labels.
field_labels <- attr(
  field_data$education_field,
  "labels"
)

cat(
  "\nAttached value labels for education_field:\n"
)

print(field_labels)


# Create a frequency table of observed field codes.
field_frequency <- field_data[
  !is.na(education_field),
  .(
    observations = .N
  ),
  by = education_field
][
  order(education_field)
]


# Add Stata labels where they exist.
if (!is.null(field_labels)) {
  
  label_lookup <- data.table(
    education_field = as.numeric(field_labels),
    education_field_label_from_stata = names(field_labels)
  )
  
  field_frequency <- merge(
    field_frequency,
    label_lookup,
    by = "education_field",
    all.x = TRUE,
    sort = TRUE
  )
}


# If a separate readable field variable exists, include its values.
if ("education_field_label" %in% names(field_data)) {
  
  readable_field_values <- field_data[
    !is.na(education_field),
    .(
      education_field_label_in_data =
        paste(
          sort(
            unique(
              as.character(education_field_label)
            )
          ),
          collapse = " | "
        )
    ),
    by = education_field
  ]
  
  field_frequency <- merge(
    field_frequency,
    readable_field_values,
    by = "education_field",
    all.x = TRUE,
    sort = TRUE
  )
}


fwrite(
  field_frequency,
  output_file,
  bom = TRUE
)

cat(
  "\nEducation-field diagnostic saved at:\n",
  output_file,
  "\n"
)

rm(field_data)
invisible(gc())








# ==============================================================================
# STEP 5B-PREP 2: Identify Education-Field Classification by Survey Year
#
# Purpose:
#   1. Link the internal education_field values to their Stata labels.
#   2. Determine which years use ISCED-F 2013 and which use ISCED-1997.
#   3. Identify transition years or mixed coding.
# ==============================================================================

rm(list = ls())
invisible(gc())

library(data.table)
library(fst)

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_step4_exposure_merged_core_analysis.fst"
)

codebook_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "mismatch_diagnostics",
  "education_field_codebook_diagnostic.csv"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "mismatch_diagnostics"
)

# Read only the variables needed for this diagnostic.
dt <- as.data.table(
  read_fst(
    input_file,
    columns = c(
      "year",
      "employment_status",
      "employed",
      "educ_level",
      "education_field",
      "weight"
    )
  )
)

# Read the field-code mapping extracted from the original Stata file.
field_codebook <- fread(
  codebook_file,
  encoding = "UTF-8"
)

# Standardize data types.
dt[
  ,
  education_field := as.integer(education_field)
]

field_codebook[
  ,
  education_field := as.integer(education_field)
]

field_codebook[
  ,
  education_field_label :=
    as.character(education_field_label_from_stata)
]


# ==============================================================================
# Classify each field label by coding system
# ==============================================================================

field_codebook[
  ,
  field_classification_system :=
    fcase(
      
      # ISCED-F 2013 broad, narrow, or detailed codes.
      grepl(
        "^(00|01|02|03|04|05|06|07|08|09|10|[0-9]{4})$",
        education_field_label
      ),
      "ISCED_F_2013",
      
      # ISCED-1997 two-digit field codes.
      education_field_label %in%
        c(
          "11", "14", "18",
          "21", "22", "26",
          "30", "31", "32", "34", "38",
          "42", "44", "46", "48",
          "50", "52", "54", "58",
          "62", "64", "66",
          "70", "72", "76", "78",
          "81", "84", "85", "86", "89",
          "99"
        ),
      "ISCED_1997",
      
      education_field_label %in%
        c("X0", "X000", "x0"),
      "Unknown_or_nonstandard",
      
      default = "Needs_review"
    )
]


# ==============================================================================
# Merge the labels into the analytical data
# ==============================================================================

dt[
  field_codebook,
  on = "education_field",
  `:=`(
    education_field_label =
      i.education_field_label,
    
    field_classification_system =
      i.field_classification_system
  )
]


# ==============================================================================
# Classification-system frequency by year
# ==============================================================================

classification_by_year <- dt[
  !is.na(education_field),
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    field_classification_system
  )
][
  order(
    year,
    field_classification_system
  )
]

classification_by_year[
  ,
  percent_within_year :=
    round(
      100 * observations /
        sum(observations),
      4
    ),
  by = year
]

fwrite(
  classification_by_year,
  file.path(
    output_folder,
    "step5b_field_classification_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# Individual field-label frequency by year
# ==============================================================================

field_labels_by_year <- dt[
  !is.na(education_field),
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    year,
    education_field,
    education_field_label,
    field_classification_system
  )
][
  order(
    year,
    field_classification_system,
    education_field_label
  )
]

fwrite(
  field_labels_by_year,
  file.path(
    output_folder,
    "step5b_field_labels_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# Check for unmapped or unclear labels
# ==============================================================================

unresolved_field_labels <- dt[
  !is.na(education_field) &
    (
      is.na(field_classification_system) |
        field_classification_system %in%
        c(
          "Unknown_or_nonstandard",
          "Needs_review"
        )
    ),
  .(
    observations = .N
  ),
  by = .(
    education_field,
    education_field_label,
    field_classification_system
  )
][
  order(-observations)
]

fwrite(
  unresolved_field_labels,
  file.path(
    output_folder,
    "step5b_unresolved_field_labels.csv"
  ),
  bom = TRUE
)

cat(
  "\n============================================================\n",
  "FIELD CLASSIFICATION DIAGNOSTIC COMPLETED\n",
  "============================================================\n",
  "\nUpload these files:\n",
  "1. step5b_field_classification_by_year.csv\n",
  "2. step5b_unresolved_field_labels.csv\n"
)

rm(dt, field_codebook)
invisible(gc())








# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 5C: Construct Vertical and Horizontal Education Mismatch
#
# Purpose:
#   1. Load the Step 4 core analysis dataset.
#   2. Decode and harmonize education-field classifications.
#   3. Standardize occupation codes to the two-digit ISCO level.
#   4. Construct vertical mismatch using a weighted realized-matches method.
#   5. Construct strict and broad horizontal mismatch indicators.
#   6. Construct combined mismatch outcomes.
#   7. Save the final outcome-ready analysis dataset.
#
# Main vertical-mismatch definition:
#   Compare each worker's education rank with the survey-weighted modal
#   education rank in the worker's:
#
#       ISCO two-digit occupation × broad survey period
#
# Main horizontal-mismatch definition:
#   Compare the worker's harmonized field of education with a documented set
#   of related ISCO two-digit occupations.
#
# Important:
#   - Mismatch is defined only among employed respondents with valid information.
#   - General programmes and unknown fields are not classified horizontally.
#   - The Step 4 core dataset is not overwritten.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load packages
# ==============================================================================

rm(list = ls())

invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 100)

required_packages <- c(
  "data.table",
  "fst",
  "stringr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_step4_exposure_merged_core_analysis.fst"
)

field_codebook_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "mismatch_diagnostics",
  "education_field_codebook_diagnostic.csv"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

output_file <- file.path(
  output_folder,
  "lfs_step5_all_outcomes_analysis_ready.fst"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The Step 4 core dataset was not found:\n",
      input_file
    )
  )
}

if (!file.exists(field_codebook_file)) {
  stop(
    paste0(
      "The education-field codebook was not found:\n",
      field_codebook_file
    )
  )
}


# ==============================================================================
# STEP 2: Load the Step 4 core dataset
# ==============================================================================

cat("\nReading the Step 4 core dataset...\n")

dt <- as.data.table(
  read_fst(input_file)
)

input_rows <- nrow(dt)
input_columns <- ncol(dt)
input_column_names <- names(dt)

cat(
  "\nObservations:",
  format(input_rows, big.mark = ","),
  "\n"
)

cat(
  "Input columns:",
  input_columns,
  "\n"
)


# ==============================================================================
# STEP 3: Confirm that required variables exist
# ==============================================================================

required_variables <- c(
  "year",
  "weight",
  "employed",
  "employment_status",
  "educ_level",
  "education_field",
  "main_job_title_isco_code",
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure"
)

missing_variables <- setdiff(
  required_variables,
  names(dt)
)

if (length(missing_variables) > 0) {
  stop(
    paste0(
      "Required variables are missing:\n",
      paste(missing_variables, collapse = ", ")
    )
  )
}


# ==============================================================================
# STEP 4: Load the education-field codebook
#
# education_field in the microdata is an internal numeric category.
# The attached Stata label identifies the actual ISCED field code.
# ==============================================================================

field_codebook <- fread(
  field_codebook_file,
  encoding = "UTF-8"
)

required_codebook_variables <- c(
  "education_field",
  "education_field_label_from_stata"
)

missing_codebook_variables <- setdiff(
  required_codebook_variables,
  names(field_codebook)
)

if (length(missing_codebook_variables) > 0) {
  stop(
    paste0(
      "Required codebook variables are missing:\n",
      paste(missing_codebook_variables, collapse = ", ")
    )
  )
}

field_codebook[
  ,
  education_field := as.integer(education_field)
]

field_codebook[
  ,
  education_field_code :=
    str_squish(
      as.character(education_field_label_from_stata)
    )
]

field_codebook <- unique(
  field_codebook[
    ,
    .(
      education_field,
      education_field_code
    )
  ]
)


# ==============================================================================
# STEP 5: Check that the education-field codebook has unique keys
# ==============================================================================

duplicate_field_codes <- field_codebook[
  ,
  .N,
  by = education_field
][
  N > 1
]

if (nrow(duplicate_field_codes) > 0) {
  print(duplicate_field_codes)
  
  stop(
    "The education-field codebook contains duplicate internal field codes."
  )
}


# ==============================================================================
# STEP 6: Merge the actual ISCED field code into the analytical dataset
# ==============================================================================

dt[
  ,
  education_field := as.integer(education_field)
]

dt[
  field_codebook,
  on = "education_field",
  education_field_code := i.education_field_code
]


# ==============================================================================
# STEP 7: Identify the field-classification system
#
# ISCED-1997 generally uses two-digit codes such as:
#   14, 21, 34, 48, 52, 72
#
# ISCED-F 2013 generally uses:
#   two-digit broad fields: 01, 02, ..., 10
#   four-digit detailed fields: 0111, 0411, 0711, etc.
# ==============================================================================

isced1997_codes <- c(
  "11", "14", "18",
  "21", "22", "26",
  "30", "31", "32", "34", "38",
  "42", "44", "46", "48",
  "50", "52", "54", "58",
  "62", "64", "66",
  "70", "72", "76", "78",
  "81", "84", "85", "86", "89",
  "99"
)

dt[
  ,
  education_field_system :=
    fcase(
      
      education_field_code %in% isced1997_codes,
      "ISCED_1997",
      
      str_detect(
        education_field_code,
        "^(00|01|02|03|04|05|06|07|08|09|10)$"
      ),
      "ISCED_F_2013",
      
      str_detect(
        education_field_code,
        "^[0-9]{4}$"
      ),
      "ISCED_F_2013",
      
      education_field_code %in%
        c(
          "X0",
          "X000",
          "x0"
        ),
      "Unknown_or_nonstandard",
      
      is.na(education_field_code),
      NA_character_,
      
      default = "Needs_review"
    )
]


# ==============================================================================
# STEP 8: Construct a harmonized broad education field
#
# Common categories:
#
#   00 General programmes
#   01 Education
#   02 Arts and humanities
#   03 Social sciences, journalism, and information
#   04 Business, administration, and law
#   05 Natural sciences, mathematics, and statistics
#   06 Information and communication technology
#   07 Engineering, manufacturing, and construction
#   08 Agriculture, forestry, fisheries, and veterinary
#   09 Health and welfare
#   10 Services
#
# Older ISCED-1997 fields are mapped into these common categories.
# ==============================================================================


# ------------------------------------------------------------------------------
# 8.1 Create the broad ISCED-F prefix
#
# For a four-digit code such as 0711, the broad field is 07.
# For an existing two-digit code such as 07, it remains 07.
# ------------------------------------------------------------------------------

dt[
  ,
  isced2013_broad_code :=
    fifelse(
      education_field_system == "ISCED_F_2013" &
        !is.na(education_field_code),
      substr(education_field_code, 1, 2),
      NA_character_
    )
]


# ------------------------------------------------------------------------------
# 8.2 Harmonize old and new field classifications
# ------------------------------------------------------------------------------

dt[
  ,
  education_field_harmonized :=
    fcase(
      
      # ------------------------------------------------------------------------
      # General programmes and qualifications
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "00",
      "General_Programmes",
      
      education_field_system == "ISCED_1997" &
        education_field_code == "11",
      "General_Programmes",
      
      
      # ------------------------------------------------------------------------
      # Education
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "01",
      "Education",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "14",
          "18"
        ),
      "Education",
      
      
      # ------------------------------------------------------------------------
      # Arts and humanities
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "02",
      "Arts_Humanities",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "21",
          "22",
          "26"
        ),
      "Arts_Humanities",
      
      
      # ------------------------------------------------------------------------
      # Social sciences, journalism, and information
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "03",
      "Social_Sciences_Journalism_Information",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "30",
          "31",
          "32"
        ),
      "Social_Sciences_Journalism_Information",
      
      
      # ------------------------------------------------------------------------
      # Business, administration, and law
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "04",
      "Business_Administration_Law",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "34",
          "38"
        ),
      "Business_Administration_Law",
      
      
      # ------------------------------------------------------------------------
      # Natural sciences, mathematics, and statistics
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "05",
      "Natural_Sciences_Mathematics_Statistics",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "42",
          "44",
          "46"
        ),
      "Natural_Sciences_Mathematics_Statistics",
      
      
      # ------------------------------------------------------------------------
      # Information and communication technology
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "06",
      "Information_Communication_Technology",
      
      education_field_system == "ISCED_1997" &
        education_field_code == "48",
      "Information_Communication_Technology",
      
      
      # ------------------------------------------------------------------------
      # Engineering, manufacturing, and construction
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "07",
      "Engineering_Manufacturing_Construction",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "50",
          "52",
          "54",
          "58"
        ),
      "Engineering_Manufacturing_Construction",
      
      
      # ------------------------------------------------------------------------
      # Agriculture, forestry, fisheries, and veterinary
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "08",
      "Agriculture_Forestry_Fisheries_Veterinary",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "62",
          "64",
          "66"
        ),
      "Agriculture_Forestry_Fisheries_Veterinary",
      
      
      # ------------------------------------------------------------------------
      # Health and welfare
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "09",
      "Health_Welfare",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "70",
          "72",
          "76"
        ),
      "Health_Welfare",
      
      
      # ------------------------------------------------------------------------
      # Services
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_F_2013" &
        isced2013_broad_code == "10",
      "Services",
      
      education_field_system == "ISCED_1997" &
        education_field_code %in%
        c(
          "78",
          "81",
          "84",
          "85",
          "86",
          "89"
        ),
      "Services",
      
      
      # ------------------------------------------------------------------------
      # Unspecified and nonstandard categories remain unresolved
      # ------------------------------------------------------------------------
      
      education_field_system == "ISCED_1997" &
        education_field_code == "99",
      "Unspecified",
      
      education_field_system %in%
        c(
          "Unknown_or_nonstandard",
          "Needs_review"
        ),
      "Unspecified",
      
      default = NA_character_
    )
]


# ==============================================================================
# STEP 9: Create the ordered education hierarchy
#
# The ranking is ordinal and is not interpreted as exact years of schooling.
#
# Codes confirmed from the Stata value-label codebook:
#
#   9  Primary
#   6  Lower secondary
#   5  Higher secondary
#   8  Pre-university
#   10 Short-cycle tertiary
#   3  Bachelor's
#   7  Master's
#   4  Doctoral
#
# Unofficial and unresolved categories remain missing.
# ==============================================================================

dt[
  ,
  educ_level_num :=
    suppressWarnings(
      as.integer(educ_level)
    )
]

dt[
  ,
  education_rank :=
    fcase(
      educ_level_num == 9L,  1L,
      educ_level_num == 6L,  2L,
      educ_level_num == 5L,  3L,
      educ_level_num == 8L,  4L,
      educ_level_num == 10L, 5L,
      educ_level_num == 3L,  6L,
      educ_level_num == 7L,  7L,
      educ_level_num == 4L,  8L,
      default = NA_integer_
    )
]

dt[
  ,
  education_level_ordered :=
    fcase(
      education_rank == 1L, "Primary",
      education_rank == 2L, "Lower_Secondary",
      education_rank == 3L, "Higher_Secondary",
      education_rank == 4L, "Pre_University",
      education_rank == 5L, "Short_Cycle_Tertiary",
      education_rank == 6L, "Bachelors",
      education_rank == 7L, "Masters",
      education_rank == 8L, "Doctoral",
      default = NA_character_
    )
]

dt[
  ,
  tertiary_educated :=
    as.integer(
      education_rank %in%
        c(
          5L,
          6L,
          7L,
          8L
        )
    )
]


# ==============================================================================
# STEP 10: Standardize the occupation code
#
# Most years use four-digit ISCO codes, while 1402–1403 mainly contain
# two-digit codes.
#
# We construct a consistent two-digit occupation code.
# ==============================================================================

dt[
  ,
  occupation_code_text :=
    str_squish(
      as.character(main_job_title_isco_code)
    )
]

dt[
  ,
  occupation_code_text :=
    str_replace(
      occupation_code_text,
      "\\.0+$",
      ""
    )
]

dt[
  ,
  occupation_code_text :=
    str_replace_all(
      occupation_code_text,
      "[^0-9]",
      ""
    )
]

dt[
  occupation_code_text == "",
  occupation_code_text := NA_character_
]


# ------------------------------------------------------------------------------
# 10.1 Record the original number of digits
# ------------------------------------------------------------------------------

dt[
  ,
  occupation_code_digits :=
    fifelse(
      !is.na(occupation_code_text),
      nchar(occupation_code_text),
      NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 10.2 Recover leading zero for armed-forces codes where possible
#
# Examples:
#   A stored value of 111 may represent 0111 in four-digit coding.
#   A stored value of 1 may represent 01 in two-digit coding.
# ------------------------------------------------------------------------------

dt[
  ,
  occupation_code_standardized :=
    fcase(
      
      year <= 1401 &
        !is.na(occupation_code_text) &
        nchar(occupation_code_text) == 3,
      str_pad(
        occupation_code_text,
        width = 4,
        side = "left",
        pad = "0"
      ),
      
      year >= 1402 &
        !is.na(occupation_code_text) &
        nchar(occupation_code_text) == 1,
      str_pad(
        occupation_code_text,
        width = 2,
        side = "left",
        pad = "0"
      ),
      
      !is.na(occupation_code_text),
      occupation_code_text,
      
      default = NA_character_
    )
]


# ------------------------------------------------------------------------------
# 10.3 Construct consistent two-digit ISCO group
# ------------------------------------------------------------------------------

dt[
  ,
  isco_2digit :=
    fifelse(
      !is.na(occupation_code_standardized) &
        nchar(occupation_code_standardized) >= 2,
      substr(
        occupation_code_standardized,
        1,
        2
      ),
      NA_character_
    )
]

dt[
  !str_detect(
    isco_2digit,
    "^[0-9]{2}$"
  ),
  isco_2digit := NA_character_
]


# ==============================================================================
# STEP 11: Create broad survey periods for vertical mismatch
#
# Broad periods allow occupational education requirements to change gradually
# while maintaining sufficiently large occupation cells.
# ==============================================================================

dt[
  ,
  vertical_mismatch_period :=
    fcase(
      year >= 1386 & year <= 1391,
      "1386_1391",
      
      year >= 1392 & year <= 1397,
      "1392_1397",
      
      year >= 1398 & year <= 1403,
      "1398_1403",
      
      default = NA_character_
    )
]


# ==============================================================================
# STEP 12: Identify the vertical-mismatch estimation sample
# ==============================================================================

dt[
  ,
  vertical_mismatch_eligible :=
    as.integer(
      employed == 1L &
        !is.na(education_rank) &
        !is.na(isco_2digit) &
        !is.na(vertical_mismatch_period) &
        !is.na(weight) &
        weight > 0
    )
]


# ==============================================================================
# STEP 13: Calculate weighted education distributions by occupation and period
#
# For every ISCO two-digit occupation × broad period, calculate the total
# survey weight associated with each education rank.
# ==============================================================================

weighted_education_distribution <- dt[
  vertical_mismatch_eligible == 1L,
  .(
    weighted_count = sum(
      weight,
      na.rm = TRUE
    ),
    
    unweighted_count = .N
  ),
  by = .(
    vertical_mismatch_period,
    isco_2digit,
    education_rank
  )
]


# ==============================================================================
# STEP 14: Determine weighted modal education in each occupation-period cell
#
# If two education levels have exactly the same weighted count, the lower rank
# is selected. Exact ties should be uncommon.
# ==============================================================================

setorder(
  weighted_education_distribution,
  vertical_mismatch_period,
  isco_2digit,
  -weighted_count,
  education_rank
)

occupation_required_education <- weighted_education_distribution[
  ,
  .SD[1],
  by = .(
    vertical_mismatch_period,
    isco_2digit
  )
]

setnames(
  occupation_required_education,
  old = c(
    "education_rank",
    "weighted_count",
    "unweighted_count"
  ),
  new = c(
    "occupation_modal_education_rank",
    "modal_education_weight",
    "modal_education_observations"
  )
)


# ==============================================================================
# STEP 15: Add total cell size and modal share
# ==============================================================================

occupation_cell_sizes <- dt[
  vertical_mismatch_eligible == 1L,
  .(
    occupation_period_observations = .N,
    
    occupation_period_weight = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    vertical_mismatch_period,
    isco_2digit
  )
]

occupation_required_education[
  occupation_cell_sizes,
  on = .(
    vertical_mismatch_period,
    isco_2digit
  ),
  `:=`(
    occupation_period_observations =
      i.occupation_period_observations,
    
    occupation_period_weight =
      i.occupation_period_weight
  )
]

occupation_required_education[
  ,
  occupation_modal_education_share :=
    modal_education_weight /
    occupation_period_weight
]


# ==============================================================================
# STEP 16: Save the realized education-requirement table
#
# This table is part of the replication materials.
# ==============================================================================

fwrite(
  occupation_required_education,
  file.path(
    output_folder,
    "step5_vertical_required_education_by_isco2_period.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 17: Merge occupation-specific required education into the microdata
# ==============================================================================

dt[
  occupation_required_education,
  on = .(
    vertical_mismatch_period,
    isco_2digit
  ),
  `:=`(
    occupation_modal_education_rank =
      i.occupation_modal_education_rank,
    
    occupation_period_observations =
      i.occupation_period_observations,
    
    occupation_modal_education_share =
      i.occupation_modal_education_share
  )
]


# ==============================================================================
# STEP 18: Construct vertical mismatch outcomes
#
# Outcomes are defined only for eligible employed workers.
#
#   education rank > modal rank: overeducated
#   education rank = modal rank: vertically matched
#   education rank < modal rank: undereducated
# ==============================================================================

dt[
  ,
  vertical_mismatch_status :=
    fcase(
      
      vertical_mismatch_eligible == 1L &
        education_rank >
        occupation_modal_education_rank,
      "Overeducated",
      
      vertical_mismatch_eligible == 1L &
        education_rank ==
        occupation_modal_education_rank,
      "Vertically_Matched",
      
      vertical_mismatch_eligible == 1L &
        education_rank <
        occupation_modal_education_rank,
      "Undereducated",
      
      default = NA_character_
    )
]

dt[
  ,
  overeducated :=
    fcase(
      vertical_mismatch_status == "Overeducated",
      1L,
      
      vertical_mismatch_status %in%
        c(
          "Vertically_Matched",
          "Undereducated"
        ),
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  undereducated :=
    fcase(
      vertical_mismatch_status == "Undereducated",
      1L,
      
      vertical_mismatch_status %in%
        c(
          "Vertically_Matched",
          "Overeducated"
        ),
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  vertically_matched :=
    fcase(
      vertical_mismatch_status == "Vertically_Matched",
      1L,
      
      vertical_mismatch_status %in%
        c(
          "Overeducated",
          "Undereducated"
        ),
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  any_vertical_mismatch :=
    fcase(
      vertical_mismatch_status %in%
        c(
          "Overeducated",
          "Undereducated"
        ),
      1L,
      
      vertical_mismatch_status ==
        "Vertically_Matched",
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 19: Construct a stricter cell-quality indicator
#
# The main sample uses all valid ISCO two-digit occupation-period cells.
#
# This robustness indicator requires:
#   - At least 50 observations in the cell
#   - Modal education share of at least 20%
# ==============================================================================

dt[
  ,
  vertical_mismatch_reliable_cell :=
    as.integer(
      vertical_mismatch_eligible == 1L &
        occupation_period_observations >= 50 &
        occupation_modal_education_share >= 0.20
    )
]


# ==============================================================================
# STEP 20: Create the horizontal field-to-occupation crosswalk
#
# The strict crosswalk contains occupations most directly connected to each
# education field.
#
# The broad crosswalk also accepts related managerial, technical, clerical,
# craft, and service occupations.
#
# This is a transparent researcher-defined crosswalk, not an official
# UNESCO-ILO determination.
# ==============================================================================

horizontal_crosswalk <- data.table(
  
  education_field_harmonized = c(
    "Education",
    "Arts_Humanities",
    "Social_Sciences_Journalism_Information",
    "Business_Administration_Law",
    "Natural_Sciences_Mathematics_Statistics",
    "Information_Communication_Technology",
    "Engineering_Manufacturing_Construction",
    "Agriculture_Forestry_Fisheries_Veterinary",
    "Health_Welfare",
    "Services"
  ),
  
  strict_isco2 = c(
    "23",
    "26",
    "26",
    "24,26,33,34",
    "21,31",
    "25,35",
    "21,31,71,72,73,74,75,81,82,83",
    "61,62,63,92",
    "22,32,53",
    "51,52,53,54"
  ),
  
  broad_isco2 = c(
    "13,23",
    "26,34,73",
    "24,26,34",
    "11,12,13,14,24,26,33,34,41,42,43,44",
    "21,31",
    "25,35",
    "13,21,31,71,72,73,74,75,81,82,83,93",
    "13,61,62,63,92",
    "13,22,32,53",
    "14,51,52,53,54,91,94,95"
  )
)

fwrite(
  horizontal_crosswalk,
  file.path(
    output_folder,
    "step5_horizontal_field_occupation_crosswalk.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Identify the horizontal-mismatch eligible sample
#
# Horizontal mismatch is defined only for:
#
#   - Employed workers
#   - Tertiary-educated respondents
#   - Valid harmonized education field
#   - Valid two-digit occupation
#
# General programmes and unspecified fields are excluded.
# ==============================================================================

classifiable_horizontal_fields <- horizontal_crosswalk[
  ,
  education_field_harmonized
]

dt[
  ,
  horizontal_mismatch_eligible :=
    as.integer(
      employed == 1L &
        tertiary_educated == 1L &
        education_field_harmonized %in%
        classifiable_horizontal_fields &
        !is.na(isco_2digit)
    )
]


# ==============================================================================
# STEP 22: Merge strict and broad occupation lists into the microdata
# ==============================================================================

dt[
  horizontal_crosswalk,
  on = "education_field_harmonized",
  `:=`(
    horizontal_strict_isco2_list =
      i.strict_isco2,
    
    horizontal_broad_isco2_list =
      i.broad_isco2
  )
]


# ==============================================================================
# STEP 23: Determine whether occupation is related to field
#
# Commas are placed around both the occupation list and occupation code so that
# code "21" does not accidentally match part of another code.
# ==============================================================================

dt[
  ,
  horizontal_match_strict :=
    fcase(
      
      horizontal_mismatch_eligible == 1L &
        str_detect(
          paste0(
            ",",
            horizontal_strict_isco2_list,
            ","
          ),
          paste0(
            ",",
            isco_2digit,
            ","
          )
        ),
      1L,
      
      horizontal_mismatch_eligible == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_mismatch_strict :=
    fcase(
      horizontal_match_strict == 1L,
      0L,
      
      horizontal_match_strict == 0L,
      1L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_match_broad :=
    fcase(
      
      horizontal_mismatch_eligible == 1L &
        str_detect(
          paste0(
            ",",
            horizontal_broad_isco2_list,
            ","
          ),
          paste0(
            ",",
            isco_2digit,
            ","
          )
        ),
      1L,
      
      horizontal_mismatch_eligible == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_mismatch_broad :=
    fcase(
      horizontal_match_broad == 1L,
      0L,
      
      horizontal_match_broad == 0L,
      1L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 24: Construct combined mismatch outcomes
#
# Combined outcomes are defined only where both vertical and horizontal
# mismatch are observed.
# ==============================================================================

dt[
  ,
  both_mismatch_measures_available :=
    as.integer(
      !is.na(any_vertical_mismatch) &
        !is.na(horizontal_mismatch_strict)
    )
]

dt[
  ,
  both_vertical_and_horizontal_mismatch :=
    fcase(
      both_mismatch_measures_available == 1L &
        any_vertical_mismatch == 1L &
        horizontal_mismatch_strict == 1L,
      1L,
      
      both_mismatch_measures_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  either_vertical_or_horizontal_mismatch :=
    fcase(
      both_mismatch_measures_available == 1L &
        (
          any_vertical_mismatch == 1L |
            horizontal_mismatch_strict == 1L
        ),
      1L,
      
      both_mismatch_measures_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  overeducated_and_horizontally_mismatched :=
    fcase(
      both_mismatch_measures_available == 1L &
        overeducated == 1L &
        horizontal_mismatch_strict == 1L,
      1L,
      
      both_mismatch_measures_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  fully_matched_strict :=
    fcase(
      both_mismatch_measures_available == 1L &
        vertically_matched == 1L &
        horizontal_match_strict == 1L,
      1L,
      
      both_mismatch_measures_available == 1L,
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 25: Produce vertical-mismatch diagnostics
# ==============================================================================

vertical_mismatch_by_year <- dt[
  vertical_mismatch_eligible == 1L,
  .(
    observations = .N,
    
    weighted_overeducation_rate =
      weighted.mean(
        overeducated,
        weight,
        na.rm = TRUE
      ),
    
    weighted_undereducation_rate =
      weighted.mean(
        undereducated,
        weight,
        na.rm = TRUE
      ),
    
    weighted_vertical_match_rate =
      weighted.mean(
        vertically_matched,
        weight,
        na.rm = TRUE
      ),
    
    reliable_cell_share =
      mean(
        vertical_mismatch_reliable_cell == 1L,
        na.rm = TRUE
      )
  ),
  by = year
][
  order(year)
]

fwrite(
  vertical_mismatch_by_year,
  file.path(
    output_folder,
    "step5_vertical_mismatch_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Produce horizontal-mismatch diagnostics
# ==============================================================================

horizontal_mismatch_by_year <- dt[
  horizontal_mismatch_eligible == 1L,
  .(
    observations = .N,
    
    weighted_horizontal_mismatch_strict =
      weighted.mean(
        horizontal_mismatch_strict,
        weight,
        na.rm = TRUE
      ),
    
    weighted_horizontal_mismatch_broad =
      weighted.mean(
        horizontal_mismatch_broad,
        weight,
        na.rm = TRUE
      ),
    
    weighted_horizontal_match_strict =
      weighted.mean(
        horizontal_match_strict,
        weight,
        na.rm = TRUE
      ),
    
    weighted_horizontal_match_broad =
      weighted.mean(
        horizontal_match_broad,
        weight,
        na.rm = TRUE
      )
  ),
  by = year
][
  order(year)
]

fwrite(
  horizontal_mismatch_by_year,
  file.path(
    output_folder,
    "step5_horizontal_mismatch_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Produce horizontal mismatch by harmonized field
# ==============================================================================

horizontal_mismatch_by_field <- dt[
  horizontal_mismatch_eligible == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    strict_mismatch_rate =
      weighted.mean(
        horizontal_mismatch_strict,
        weight,
        na.rm = TRUE
      ),
    
    broad_mismatch_rate =
      weighted.mean(
        horizontal_mismatch_broad,
        weight,
        na.rm = TRUE
      )
  ),
  by = education_field_harmonized
][
  order(education_field_harmonized)
]

fwrite(
  horizontal_mismatch_by_field,
  file.path(
    output_folder,
    "step5_horizontal_mismatch_by_field.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 28: Produce sample-size diagnostics
# ==============================================================================

mismatch_sample_summary <- dt[
  ,
  .(
    total_observations = .N,
    
    employed_observations =
      sum(
        employed == 1L,
        na.rm = TRUE
      ),
    
    vertical_mismatch_eligible =
      sum(
        vertical_mismatch_eligible == 1L,
        na.rm = TRUE
      ),
    
    reliable_vertical_cells =
      sum(
        vertical_mismatch_reliable_cell == 1L,
        na.rm = TRUE
      ),
    
    horizontal_mismatch_eligible =
      sum(
        horizontal_mismatch_eligible == 1L,
        na.rm = TRUE
      ),
    
    both_mismatch_measures_available =
      sum(
        both_mismatch_measures_available == 1L,
        na.rm = TRUE
      ),
    
    unresolved_field_observations =
      sum(
        education_field_harmonized %in%
          c(
            "Unspecified"
          ),
        na.rm = TRUE
      ),
    
    field_code_not_harmonized =
      sum(
        !is.na(education_field) &
          is.na(education_field_harmonized)
      )
  )
]

fwrite(
  mismatch_sample_summary,
  file.path(
    output_folder,
    "step5_mismatch_sample_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 29: Produce harmonized field diagnostics
# ==============================================================================

harmonized_field_diagnostic <- dt[
  !is.na(education_field),
  .(
    observations = .N,
    
    employed_observations =
      sum(
        employed == 1L,
        na.rm = TRUE
      ),
    
    tertiary_employed_observations =
      sum(
        tertiary_educated == 1L &
          employed == 1L,
        na.rm = TRUE
      )
  ),
  by = .(
    education_field_system,
    education_field_code,
    education_field_harmonized
  )
][
  order(
    education_field_system,
    education_field_harmonized,
    education_field_code
  )
]

fwrite(
  harmonized_field_diagnostic,
  file.path(
    output_folder,
    "step5_harmonized_education_field_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 30: Confirm that no rows or prior columns were lost
# ==============================================================================

if (nrow(dt) != input_rows) {
  stop(
    paste0(
      "The number of rows changed unexpectedly.\n",
      "Before: ",
      input_rows,
      "\nAfter: ",
      nrow(dt)
    )
  )
}

missing_input_columns <- setdiff(
  input_column_names,
  names(dt)
)

if (length(missing_input_columns) > 0) {
  print(missing_input_columns)
  
  stop(
    "Some Step 4 columns were unexpectedly lost."
  )
}



# ==============================================================================
# STEP 31: Save and verify the final outcome-ready dataset
# ==============================================================================

cat(
  "\nSaving the dataset containing all main outcomes...\n"
)

# Remove any incomplete file left by the failed write attempt.
if (file.exists(output_file)) {
  file.remove(output_file)
}

# Release unused memory before writing.
invisible(gc())

# Use stronger compression to reduce required disk space.
write_fst(
  dt,
  output_file,
  compress = 70
)

# Confirm that the file was created.
if (!file.exists(output_file)) {
  stop(
    "The Step 5 dataset was not created."
  )
}

# Confirm that the saved file is not empty.
output_file_size_bytes <- file.info(output_file)$size

if (
  is.na(output_file_size_bytes) ||
  output_file_size_bytes <= 0
) {
  stop(
    "The Step 5 file exists but appears to be empty or corrupted."
  )
}

# Confirm that fst can read the file metadata.
saved_metadata <- metadata_fst(output_file)

output_file_size_gb <- output_file_size_bytes / (1024^3)

cat(
  "\nStep 5 dataset saved successfully.\n"
)

cat(
  "Saved file size:",
  round(output_file_size_gb, 3),
  "GB\n"
)

cat(
  "Saved rows:",
  format(saved_metadata$nrOfRows, big.mark = ","),
  "\n"
)

cat(
  "Saved columns:",
  saved_metadata$nrOfCols,
  "\n"
)


# ==============================================================================
# STEP 32: Save completion summary
# ==============================================================================

step5_completion_summary <- data.table(
  
  item = c(
    "Input observations",
    "Output observations",
    "Input columns",
    "Output columns",
    "New columns added",
    "Vertical mismatch eligible",
    "Horizontal mismatch eligible",
    "Both mismatch measures available",
    "Baseline vertical mismatch variable",
    "Baseline horizontal mismatch variable",
    "Broad horizontal robustness variable",
    "Output file size in GB",
    "Output file"
  ),
  
  value = c(
    as.character(input_rows),
    as.character(nrow(dt)),
    as.character(input_columns),
    as.character(ncol(dt)),
    as.character(ncol(dt) - input_columns),
    
    as.character(
      sum(
        dt$vertical_mismatch_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$horizontal_mismatch_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$both_mismatch_measures_available == 1L,
        na.rm = TRUE
      )
    ),
    
    "any_vertical_mismatch",
    "horizontal_mismatch_strict",
    "horizontal_mismatch_broad",
    
    as.character(
      round(
        output_file_size_gb,
        3
      )
    ),
    
    output_file
  )
)

fwrite(
  step5_completion_summary,
  file.path(
    output_folder,
    "step5_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 33: Print final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 5 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
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
  "Vertical mismatch eligible:",
  format(
    sum(
      dt$vertical_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Horizontal mismatch eligible:",
  format(
    sum(
      dt$horizontal_mismatch_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Both mismatch measures available:",
  format(
    sum(
      dt$both_mismatch_measures_available == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "\nBaseline mismatch variables:\n",
  "1. any_vertical_mismatch\n",
  "2. overeducated\n",
  "3. undereducated\n",
  "4. horizontal_mismatch_strict\n"
)

cat(
  "\nBroad horizontal robustness variable:\n",
  "horizontal_mismatch_broad\n"
)

cat(
  "\nFinal outcome-ready dataset saved at:\n",
  output_file,
  "\n"
)

cat(
  "\nUpload these diagnostic files:\n",
  "1. step5_completion_summary.csv\n",
  "2. step5_mismatch_sample_summary.csv\n",
  "3. step5_vertical_mismatch_by_year.csv\n",
  "4. step5_horizontal_mismatch_by_year.csv\n",
  "5. step5_horizontal_mismatch_by_field.csv\n",
  "6. step5_harmonized_education_field_diagnostic.csv\n",
  "7. step5_vertical_required_education_by_isco2_period.csv\n",
  "8. step5_horizontal_field_occupation_crosswalk.csv\n"
)


# ==============================================================================
# STEP 34: Remove large objects after saving
# ==============================================================================

rm(
  dt,
  field_codebook,
  weighted_education_distribution,
  occupation_required_education,
  occupation_cell_sizes,
  horizontal_crosswalk
)

invisible(gc())

cat(
  "\nLarge in-memory objects were removed after saving.\n"
)






# ==============================================================================
# PROJECT: University Expansion and Labor-Market Outcomes in Iran
# STEP 5D: Refine Vertical and Horizontal Mismatch Measures
#
# Purpose:
#   1. Load the outcome-ready dataset created in Step 5.
#   2. Preserve all existing variables.
#   3. Add a full-period vertical mismatch measure.
#   4. Retain the existing period-specific vertical measure as robustness.
#   5. Construct a more detailed harmonized education-field classification.
#   6. Apply detailed strict and broad field-to-occupation crosswalks.
#   7. Save a refined final analysis dataset.
#
# Preferred vertical measure:
#   Weighted modal education rank within each two-digit ISCO occupation,
#   calculated over all survey years 1386–1403.
#
# Preferred horizontal measure:
#   Detailed education field compared with related two-digit ISCO occupations.
#
# Important:
#   - No observations are removed.
#   - No existing Step 5 variables are overwritten.
#   - The existing period-specific mismatch variables remain available.
# ==============================================================================


# ==============================================================================
# STEP 0: Clear memory and load required packages
# ==============================================================================

rm(list = ls())

invisible(gc())

options(scipen = 999)
options(datatable.print.nrows = 100)

required_packages <- c(
  "data.table",
  "fst",
  "stringr"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

library(data.table)
library(fst)
library(stringr)


# ==============================================================================
# STEP 1: Define file paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

input_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_step5_all_outcomes_analysis_ready.fst"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs"
)

output_file <- file.path(
  output_folder,
  "lfs_step5d_refined_mismatch_analysis_ready.fst"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "The Step 5 dataset was not found:\n",
      input_file
    )
  )
}


# ==============================================================================
# STEP 2: Load the Step 5 dataset
# ==============================================================================

cat(
  "\nReading the Step 5 outcome-ready dataset...\n"
)

dt <- as.data.table(
  read_fst(input_file)
)

input_rows <- nrow(dt)
input_columns <- ncol(dt)
input_column_names <- names(dt)

cat(
  "\nObservations:",
  format(input_rows, big.mark = ","),
  "\n"
)

cat(
  "Input columns:",
  input_columns,
  "\n"
)


# ==============================================================================
# STEP 3: Confirm required variables
# ==============================================================================

required_variables <- c(
  "year",
  "weight",
  "employed",
  "educ_level",
  "education_rank",
  "education_field_code",
  "education_field_system",
  "education_field_harmonized",
  "isco_2digit",
  "vertical_mismatch_eligible",
  "any_vertical_mismatch",
  "overeducated",
  "undereducated",
  "vertically_matched"
)

missing_variables <- setdiff(
  required_variables,
  names(dt)
)

if (length(missing_variables) > 0) {
  stop(
    paste0(
      "Required variables are missing:\n",
      paste(missing_variables, collapse = ", ")
    )
  )
}


# ==============================================================================
# PART A: FULL-PERIOD VERTICAL MISMATCH
# ==============================================================================


# ==============================================================================
# STEP 4: Define eligibility for the full-period vertical measure
#
# This uses the same education and occupation information as Step 5, but the
# required education level is calculated over all years rather than separately
# within three broad periods.
# ==============================================================================

dt[
  ,
  vertical_fullperiod_eligible :=
    as.integer(
      employed == 1L &
        !is.na(education_rank) &
        !is.na(isco_2digit) &
        !is.na(weight) &
        weight > 0
    )
]


# ==============================================================================
# STEP 5: Calculate weighted education distributions by occupation
# ==============================================================================

vertical_full_distribution <- dt[
  vertical_fullperiod_eligible == 1L,
  .(
    weighted_count = sum(
      weight,
      na.rm = TRUE
    ),
    
    unweighted_count = .N
  ),
  by = .(
    isco_2digit,
    education_rank
  )
]


# ==============================================================================
# STEP 6: Select the weighted modal education rank for each occupation
#
# In an exact tie, the lower education rank is selected.
# ==============================================================================

setorder(
  vertical_full_distribution,
  isco_2digit,
  -weighted_count,
  education_rank
)

vertical_full_requirement <- vertical_full_distribution[
  ,
  .SD[1],
  by = isco_2digit
]

setnames(
  vertical_full_requirement,
  old = c(
    "education_rank",
    "weighted_count",
    "unweighted_count"
  ),
  new = c(
    "occupation_modal_rank_fullperiod",
    "modal_weight_fullperiod",
    "modal_observations_fullperiod"
  )
)


# ==============================================================================
# STEP 7: Add total occupation cell sizes and modal shares
# ==============================================================================

vertical_full_cell_sizes <- dt[
  vertical_fullperiod_eligible == 1L,
  .(
    occupation_observations_fullperiod = .N,
    
    occupation_weight_fullperiod = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = isco_2digit
]

vertical_full_requirement[
  vertical_full_cell_sizes,
  on = "isco_2digit",
  `:=`(
    occupation_observations_fullperiod =
      i.occupation_observations_fullperiod,
    
    occupation_weight_fullperiod =
      i.occupation_weight_fullperiod
  )
]

vertical_full_requirement[
  ,
  modal_share_fullperiod :=
    modal_weight_fullperiod /
    occupation_weight_fullperiod
]


# ==============================================================================
# STEP 8: Save the full-period occupational requirement table
# ==============================================================================

fwrite(
  vertical_full_requirement,
  file.path(
    output_folder,
    "step5d_vertical_required_education_fullperiod.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Merge the full-period occupation requirement into microdata
# ==============================================================================

dt[
  vertical_full_requirement,
  on = "isco_2digit",
  `:=`(
    occupation_modal_rank_fullperiod =
      i.occupation_modal_rank_fullperiod,
    
    occupation_observations_fullperiod =
      i.occupation_observations_fullperiod,
    
    modal_share_fullperiod =
      i.modal_share_fullperiod
  )
]


# ==============================================================================
# STEP 10: Construct refined vertical mismatch status
# ==============================================================================

dt[
  ,
  vertical_status_fullperiod :=
    fcase(
      
      vertical_fullperiod_eligible == 1L &
        education_rank >
        occupation_modal_rank_fullperiod,
      "Overeducated",
      
      vertical_fullperiod_eligible == 1L &
        education_rank ==
        occupation_modal_rank_fullperiod,
      "Vertically_Matched",
      
      vertical_fullperiod_eligible == 1L &
        education_rank <
        occupation_modal_rank_fullperiod,
      "Undereducated",
      
      default = NA_character_
    )
]


# ------------------------------------------------------------------------------
# 10.1 Overeducation
# ------------------------------------------------------------------------------

dt[
  ,
  overeducated_fullperiod :=
    fcase(
      vertical_status_fullperiod == "Overeducated",
      1L,
      
      vertical_status_fullperiod %in%
        c(
          "Vertically_Matched",
          "Undereducated"
        ),
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 10.2 Undereducation
# ------------------------------------------------------------------------------

dt[
  ,
  undereducated_fullperiod :=
    fcase(
      vertical_status_fullperiod == "Undereducated",
      1L,
      
      vertical_status_fullperiod %in%
        c(
          "Vertically_Matched",
          "Overeducated"
        ),
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 10.3 Vertical match
# ------------------------------------------------------------------------------

dt[
  ,
  vertically_matched_fullperiod :=
    fcase(
      vertical_status_fullperiod == "Vertically_Matched",
      1L,
      
      vertical_status_fullperiod %in%
        c(
          "Overeducated",
          "Undereducated"
        ),
      0L,
      
      default = NA_integer_
    )
]


# ------------------------------------------------------------------------------
# 10.4 Any vertical mismatch
# ------------------------------------------------------------------------------

dt[
  ,
  vertical_mismatch_fullperiod :=
    fcase(
      vertical_status_fullperiod %in%
        c(
          "Overeducated",
          "Undereducated"
        ),
      1L,
      
      vertical_status_fullperiod ==
        "Vertically_Matched",
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# STEP 11: Create a full-period cell-quality indicator
# ==============================================================================

dt[
  ,
  vertical_fullperiod_reliable_cell :=
    as.integer(
      vertical_fullperiod_eligible == 1L &
        occupation_observations_fullperiod >= 50 &
        modal_share_fullperiod >= 0.20
    )
]


# ==============================================================================
# PART B: DETAILED HORIZONTAL MISMATCH
# ==============================================================================


# ==============================================================================
# STEP 12: Standardize the education-field code
# ==============================================================================

dt[
  ,
  education_field_code :=
    str_squish(
      as.character(education_field_code)
    )
]


# ==============================================================================
# STEP 13: Construct a detailed harmonized field
#
# The newer ISCED-F 2013 codes allow more detail than the older ISCED-1997
# classification. Where detailed information is unavailable, the observation is
# assigned to a broader category rather than discarded.
# ==============================================================================

dt[
  ,
  education_field_detailed :=
    fcase(
      
      # ------------------------------------------------------------------------
      # General programmes
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "00",
          "0011",
          "0021",
          "0031",
          "11"
        ),
      "General_Programmes",
      
      
      # ------------------------------------------------------------------------
      # Education and teacher training
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "01",
          "0111",
          "0112",
          "0113",
          "0114",
          "14",
          "18"
        ),
      "Education",
      
      
      # ------------------------------------------------------------------------
      # Arts and design
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0211",
          "0212",
          "0213",
          "0214",
          "0215",
          "21"
        ),
      "Arts_Design",
      
      
      # ------------------------------------------------------------------------
      # Humanities, religion, history, philosophy, and languages
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0220",
          "0221",
          "0222",
          "0223",
          "0231",
          "0232",
          "22",
          "26"
        ),
      "Humanities_Languages",
      
      
      # Broad arts and humanities records that cannot be separated
      education_field_code == "02",
      "Arts_Humanities_Broad",
      
      
      # ------------------------------------------------------------------------
      # Social and behavioural sciences
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0311",
          "0312",
          "0313",
          "0314",
          "31"
        ),
      "Social_Behavioural_Sciences",
      
      
      # ------------------------------------------------------------------------
      # Journalism, library, and information
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0321",
          "0322",
          "32"
        ),
      "Journalism_Information",
      
      
      # Broad social-science records
      education_field_code %in%
        c(
          "03",
          "30"
        ),
      "Social_Information_Broad",
      
      
      # ------------------------------------------------------------------------
      # Business and administration
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0411",
          "0412",
          "0413",
          "0414",
          "0415",
          "0416",
          "0417",
          "34"
        ),
      "Business_Administration",
      
      
      # ------------------------------------------------------------------------
      # Law
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0421",
          "38"
        ),
      "Law",
      
      
      # Broad business and law records
      education_field_code == "04",
      "Business_Law_Broad",
      
      
      # ------------------------------------------------------------------------
      # Biological and environmental sciences
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0510",
          "0511",
          "0512",
          "0521",
          "0522",
          "42"
        ),
      "Biology_Environment",
      
      
      # ------------------------------------------------------------------------
      # Physical sciences
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0531",
          "0532",
          "0533",
          "44"
        ),
      "Physical_Sciences",
      
      
      # ------------------------------------------------------------------------
      # Mathematics and statistics
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0541",
          "0542",
          "46"
        ),
      "Mathematics_Statistics",
      
      
      # Broad natural-science records
      education_field_code %in%
        c(
          "05"
        ),
      "Natural_Sciences_Broad",
      
      
      # ------------------------------------------------------------------------
      # Information and communication technology
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "06",
          "0611",
          "0612",
          "0613",
          "0614",
          "48"
        ),
      "Information_Communication_Technology",
      
      
      # ------------------------------------------------------------------------
      # Engineering
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0711",
          "0712",
          "0713",
          "0714",
          "0715",
          "0716",
          "0717",
          "0718",
          "0719",
          "52"
        ),
      "Engineering",
      
      
      # ------------------------------------------------------------------------
      # Manufacturing and processing
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0721",
          "0722",
          "0723",
          "0724",
          "0725",
          "54"
        ),
      "Manufacturing_Processing",
      
      
      # ------------------------------------------------------------------------
      # Architecture and construction
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0731",
          "0732",
          "58"
        ),
      "Architecture_Construction",
      
      
      # Broad engineering records
      education_field_code %in%
        c(
          "07",
          "0740",
          "50"
        ),
      "Engineering_Broad",
      
      
      # ------------------------------------------------------------------------
      # Agriculture
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0811",
          "0812",
          "62"
        ),
      "Agriculture",
      
      
      # ------------------------------------------------------------------------
      # Forestry and fisheries
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0821",
          "0831"
        ),
      "Forestry_Fisheries",
      
      
      # ------------------------------------------------------------------------
      # Veterinary
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0841",
          "64"
        ),
      "Veterinary",
      
      
      # Broad agriculture records
      education_field_code %in%
        c(
          "08",
          "66"
        ),
      "Agriculture_Broad",
      
      
      # ------------------------------------------------------------------------
      # Health
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0911",
          "0912",
          "0913",
          "0914",
          "0915",
          "0916",
          "0917",
          "0918",
          "72"
        ),
      "Health",
      
      
      # ------------------------------------------------------------------------
      # Welfare and social services
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "0922",
          "0923",
          "76"
        ),
      "Welfare_Social_Services",
      
      
      # Broad health and welfare records
      education_field_code %in%
        c(
          "09",
          "70",
          "78"
        ),
      "Health_Welfare_Broad",
      
      
      # ------------------------------------------------------------------------
      # Personal services
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "1011",
          "1012",
          "1013",
          "1014",
          "1015",
          "81"
        ),
      "Personal_Services",
      
      
      # ------------------------------------------------------------------------
      # Hygiene and occupational health services
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "1021",
          "1022",
          "85"
        ),
      "Environmental_Occupational_Services",
      
      
      # ------------------------------------------------------------------------
      # Security services
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "1031",
          "1032",
          "86"
        ),
      "Security_Services",
      
      
      # ------------------------------------------------------------------------
      # Transport services
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "1041",
          "84"
        ),
      "Transport_Services",
      
      
      # Broad or residual service categories
      education_field_code %in%
        c(
          "10",
          "89"
        ),
      "Services_Broad",
      
      
      # ------------------------------------------------------------------------
      # Unknown and unspecified fields
      # ------------------------------------------------------------------------
      
      education_field_code %in%
        c(
          "99",
          "X0",
          "X000",
          "x0"
        ),
      "Unspecified",
      
      default = NA_character_
    )
]


# ==============================================================================
# STEP 14: Create detailed field-to-occupation crosswalk
#
# strict_isco2:
#   Occupations directly associated with the field.
#
# broad_isco2:
#   Strict matches plus related managerial, technical, administrative,
#   teaching, service, or production occupations.
#
# This crosswalk is researcher-defined and must be documented in the paper.
# ==============================================================================

horizontal_detailed_crosswalk <- data.table(
  
  education_field_detailed = c(
    "Education",
    "Arts_Design",
    "Humanities_Languages",
    "Arts_Humanities_Broad",
    "Social_Behavioural_Sciences",
    "Journalism_Information",
    "Social_Information_Broad",
    "Business_Administration",
    "Law",
    "Business_Law_Broad",
    "Biology_Environment",
    "Physical_Sciences",
    "Mathematics_Statistics",
    "Natural_Sciences_Broad",
    "Information_Communication_Technology",
    "Engineering",
    "Manufacturing_Processing",
    "Architecture_Construction",
    "Engineering_Broad",
    "Agriculture",
    "Forestry_Fisheries",
    "Veterinary",
    "Agriculture_Broad",
    "Health",
    "Welfare_Social_Services",
    "Health_Welfare_Broad",
    "Personal_Services",
    "Environmental_Occupational_Services",
    "Security_Services",
    "Transport_Services",
    "Services_Broad"
  ),
  
  strict_isco2 = c(
    "23",
    "26,73",
    "23,26",
    "23,26,73",
    "24,26,34",
    "26,44",
    "24,26,34,44",
    "11,12,13,14,24,33,41,42,43,44",
    "26,34",
    "11,12,13,14,24,26,33,34,41,42,43,44",
    "21,31",
    "21,31",
    "21,31",
    "21,31",
    "25,35",
    "21,31",
    "21,31,72,73,75,81,82",
    "21,31,71",
    "21,31,71,72,73,74,75,81,82,83",
    "21,31,61,92",
    "21,31,62,92",
    "22,32",
    "21,22,31,32,61,62,63,92",
    "22,32,53",
    "26,34,53",
    "22,26,32,34,53",
    "51,52,53",
    "21,31,32,75,81,91,96",
    "54",
    "83",
    "51,52,53,54,83,91,94,95,96"
  ),
  
  broad_isco2 = c(
    "13,23",
    "14,23,26,73",
    "23,24,26,34",
    "14,23,24,26,34,73",
    "11,12,13,23,24,26,34",
    "23,24,26,44",
    "11,12,13,23,24,26,34,44",
    "11,12,13,14,24,26,33,34,41,42,43,44",
    "11,12,24,26,34",
    "11,12,13,14,24,26,33,34,41,42,43,44",
    "13,21,23,31",
    "13,21,23,31",
    "13,21,23,24,31",
    "13,21,23,24,31",
    "13,21,25,31,35",
    "13,21,31,71,72,73,74,75,81,82,83",
    "13,21,31,71,72,73,74,75,81,82,93",
    "13,21,31,71,75",
    "13,21,31,71,72,73,74,75,81,82,83,93",
    "13,21,31,61,92",
    "13,21,31,61,62,63,92",
    "13,22,32",
    "13,21,22,31,32,61,62,63,92",
    "13,22,32,53",
    "13,22,26,32,34,53",
    "13,22,26,32,34,53",
    "14,51,52,53,54",
    "13,21,31,32,71,75,81,91,96",
    "13,54",
    "13,31,43,83",
    "13,14,31,43,51,52,53,54,83,91,94,95,96"
  )
)


# ==============================================================================
# STEP 15: Save the detailed crosswalk
# ==============================================================================

fwrite(
  horizontal_detailed_crosswalk,
  file.path(
    output_folder,
    "step5d_detailed_horizontal_crosswalk.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 16: Expand comma-separated occupation lists
#
# The long format allows an efficient data.table join rather than applying
# string matching separately to more than four million observations.
# ==============================================================================

strict_crosswalk_long <- horizontal_detailed_crosswalk[
  ,
  .(
    isco_2digit = unlist(
      strsplit(
        strict_isco2,
        split = ",",
        fixed = TRUE
      )
    )
  ),
  by = education_field_detailed
]

strict_crosswalk_long[
  ,
  strict_match_detailed := 1L
]

strict_crosswalk_long <- unique(
  strict_crosswalk_long[
    ,
    .(
      education_field_detailed,
      isco_2digit,
      strict_match_detailed
    )
  ]
)


broad_crosswalk_long <- horizontal_detailed_crosswalk[
  ,
  .(
    isco_2digit = unlist(
      strsplit(
        broad_isco2,
        split = ",",
        fixed = TRUE
      )
    )
  ),
  by = education_field_detailed
]

broad_crosswalk_long[
  ,
  broad_match_detailed := 1L
]

broad_crosswalk_long <- unique(
  broad_crosswalk_long[
    ,
    .(
      education_field_detailed,
      isco_2digit,
      broad_match_detailed
    )
  ]
)


# ==============================================================================
# STEP 17: Define eligibility for detailed horizontal mismatch
# ==============================================================================

classifiable_detailed_fields <- horizontal_detailed_crosswalk[
  ,
  education_field_detailed
]

dt[
  ,
  horizontal_detailed_eligible :=
    as.integer(
      employed == 1L &
        tertiary_educated == 1L &
        education_field_detailed %in%
        classifiable_detailed_fields &
        !is.na(isco_2digit)
    )
]


# ==============================================================================
# STEP 18: Merge strict and broad detailed match indicators
# ==============================================================================

dt[
  strict_crosswalk_long,
  on = .(
    education_field_detailed,
    isco_2digit
  ),
  strict_match_detailed :=
    i.strict_match_detailed
]

dt[
  broad_crosswalk_long,
  on = .(
    education_field_detailed,
    isco_2digit
  ),
  broad_match_detailed :=
    i.broad_match_detailed
]


# ==============================================================================
# STEP 19: Convert join results into clean horizontal outcomes
# ==============================================================================

dt[
  ,
  horizontal_match_detailed_strict :=
    fcase(
      horizontal_detailed_eligible == 1L &
        strict_match_detailed == 1L,
      1L,
      
      horizontal_detailed_eligible == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_mismatch_detailed_strict :=
    fcase(
      horizontal_match_detailed_strict == 1L,
      0L,
      
      horizontal_match_detailed_strict == 0L,
      1L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_match_detailed_broad :=
    fcase(
      horizontal_detailed_eligible == 1L &
        broad_match_detailed == 1L,
      1L,
      
      horizontal_detailed_eligible == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  horizontal_mismatch_detailed_broad :=
    fcase(
      horizontal_match_detailed_broad == 1L,
      0L,
      
      horizontal_match_detailed_broad == 0L,
      1L,
      
      default = NA_integer_
    )
]


# Remove temporary join indicators.
dt[
  ,
  c(
    "strict_match_detailed",
    "broad_match_detailed"
  ) := NULL
]


# ==============================================================================
# PART C: COMBINED REFINED MISMATCH OUTCOMES
# ==============================================================================


# ==============================================================================
# STEP 20: Identify respondents with both refined mismatch measures
# ==============================================================================

dt[
  ,
  both_refined_mismatch_available :=
    as.integer(
      !is.na(vertical_mismatch_fullperiod) &
        !is.na(horizontal_mismatch_detailed_broad)
    )
]


# ==============================================================================
# STEP 21: Construct combined refined outcomes
# ==============================================================================

dt[
  ,
  both_refined_vertical_horizontal_mismatch :=
    fcase(
      both_refined_mismatch_available == 1L &
        vertical_mismatch_fullperiod == 1L &
        horizontal_mismatch_detailed_broad == 1L,
      1L,
      
      both_refined_mismatch_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  either_refined_vertical_horizontal_mismatch :=
    fcase(
      both_refined_mismatch_available == 1L &
        (
          vertical_mismatch_fullperiod == 1L |
            horizontal_mismatch_detailed_broad == 1L
        ),
      1L,
      
      both_refined_mismatch_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  fully_matched_refined :=
    fcase(
      both_refined_mismatch_available == 1L &
        vertically_matched_fullperiod == 1L &
        horizontal_match_detailed_broad == 1L,
      1L,
      
      both_refined_mismatch_available == 1L,
      0L,
      
      default = NA_integer_
    )
]

dt[
  ,
  overeducated_and_horizontal_mismatch_refined :=
    fcase(
      both_refined_mismatch_available == 1L &
        overeducated_fullperiod == 1L &
        horizontal_mismatch_detailed_broad == 1L,
      1L,
      
      both_refined_mismatch_available == 1L,
      0L,
      
      default = NA_integer_
    )
]


# ==============================================================================
# PART D: DIAGNOSTICS
# ==============================================================================


# ==============================================================================
# STEP 22: Compare period-specific and full-period vertical measures
# ==============================================================================

vertical_comparison_by_year <- dt[
  vertical_fullperiod_eligible == 1L,
  .(
    observations = .N,
    
    overeducation_fullperiod = weighted.mean(
      overeducated_fullperiod,
      weight,
      na.rm = TRUE
    ),
    
    overeducation_period_specific = weighted.mean(
      overeducated,
      weight,
      na.rm = TRUE
    ),
    
    undereducation_fullperiod = weighted.mean(
      undereducated_fullperiod,
      weight,
      na.rm = TRUE
    ),
    
    undereducation_period_specific = weighted.mean(
      undereducated,
      weight,
      na.rm = TRUE
    ),
    
    vertical_match_fullperiod = weighted.mean(
      vertically_matched_fullperiod,
      weight,
      na.rm = TRUE
    ),
    
    vertical_match_period_specific = weighted.mean(
      vertically_matched,
      weight,
      na.rm = TRUE
    ),
    
    classifications_agree = weighted.mean(
      vertical_mismatch_fullperiod ==
        any_vertical_mismatch,
      weight,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  vertical_comparison_by_year,
  file.path(
    output_folder,
    "step5d_vertical_comparison_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Refined horizontal mismatch by year
# ==============================================================================

horizontal_refined_by_year <- dt[
  horizontal_detailed_eligible == 1L,
  .(
    observations = .N,
    
    strict_mismatch_rate = weighted.mean(
      horizontal_mismatch_detailed_strict,
      weight,
      na.rm = TRUE
    ),
    
    broad_mismatch_rate = weighted.mean(
      horizontal_mismatch_detailed_broad,
      weight,
      na.rm = TRUE
    )
  ),
  by = year
][
  order(year)
]

fwrite(
  horizontal_refined_by_year,
  file.path(
    output_folder,
    "step5d_horizontal_refined_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Refined horizontal mismatch by detailed field
# ==============================================================================

horizontal_refined_by_field <- dt[
  horizontal_detailed_eligible == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    strict_mismatch_rate = weighted.mean(
      horizontal_mismatch_detailed_strict,
      weight,
      na.rm = TRUE
    ),
    
    broad_mismatch_rate = weighted.mean(
      horizontal_mismatch_detailed_broad,
      weight,
      na.rm = TRUE
    )
  ),
  by = education_field_detailed
][
  order(education_field_detailed)
]

fwrite(
  horizontal_refined_by_field,
  file.path(
    output_folder,
    "step5d_horizontal_refined_by_field.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Detailed field frequency and coverage
# ==============================================================================

detailed_field_diagnostic <- dt[
  !is.na(education_field_code),
  .(
    observations = .N,
    
    employed = sum(
      employed == 1L,
      na.rm = TRUE
    ),
    
    tertiary_employed = sum(
      employed == 1L &
        tertiary_educated == 1L,
      na.rm = TRUE
    ),
    
    horizontal_eligible = sum(
      horizontal_detailed_eligible == 1L,
      na.rm = TRUE
    )
  ),
  by = .(
    education_field_system,
    education_field_code,
    education_field_detailed
  )
][
  order(
    education_field_detailed,
    education_field_code
  )
]

fwrite(
  detailed_field_diagnostic,
  file.path(
    output_folder,
    "step5d_detailed_field_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Overall refined mismatch summary
# ==============================================================================

refined_mismatch_summary <- dt[
  ,
  .(
    total_observations = .N,
    
    vertical_fullperiod_eligible = sum(
      vertical_fullperiod_eligible == 1L,
      na.rm = TRUE
    ),
    
    vertical_fullperiod_reliable = sum(
      vertical_fullperiod_reliable_cell == 1L,
      na.rm = TRUE
    ),
    
    horizontal_detailed_eligible = sum(
      horizontal_detailed_eligible == 1L,
      na.rm = TRUE
    ),
    
    both_refined_measures_available = sum(
      both_refined_mismatch_available == 1L,
      na.rm = TRUE
    ),
    
    detailed_field_unclassified = sum(
      !is.na(education_field_code) &
        is.na(education_field_detailed),
      na.rm = TRUE
    )
  )
]

fwrite(
  refined_mismatch_summary,
  file.path(
    output_folder,
    "step5d_refined_mismatch_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Confirm that no observations or prior columns were lost
# ==============================================================================

if (nrow(dt) != input_rows) {
  stop(
    paste0(
      "The number of observations changed unexpectedly.\n",
      "Before: ",
      input_rows,
      "\nAfter: ",
      nrow(dt)
    )
  )
}

missing_input_columns <- setdiff(
  input_column_names,
  names(dt)
)

if (length(missing_input_columns) > 0) {
  print(missing_input_columns)
  
  stop(
    "Some Step 5 variables were unexpectedly lost."
  )
}


# ==============================================================================
# STEP 28: Safely save the refined final dataset
#
# Stronger compression is used because the earlier Step 5 save encountered a
# disk-space error.
# ==============================================================================

cat(
  "\nSaving the refined mismatch dataset...\n"
)

# Remove an incomplete file from any previous failed attempt.
if (file.exists(output_file)) {
  file.remove(output_file)
}

invisible(gc())

write_fst(
  dt,
  output_file,
  compress = 70
)

if (!file.exists(output_file)) {
  stop(
    "The Step 5D dataset was not created."
  )
}

output_file_size_bytes <- file.info(
  output_file
)$size

if (
  is.na(output_file_size_bytes) ||
  output_file_size_bytes <= 0
) {
  stop(
    "The output file appears empty or corrupted."
  )
}

# Verify that FST can read the saved metadata.
saved_metadata <- metadata_fst(
  output_file
)

output_file_size_gb <- output_file_size_bytes / (1024^3)

cat(
  "\nStep 5D dataset saved successfully.\n"
)

cat(
  "Saved file size:",
  round(output_file_size_gb, 3),
  "GB\n"
)


# ==============================================================================
# STEP 29: Save completion summary
# ==============================================================================

step5d_completion_summary <- data.table(
  
  item = c(
    "Input observations",
    "Output observations",
    "Input columns",
    "Output columns",
    "New columns added",
    "Full-period vertical eligible",
    "Detailed horizontal eligible",
    "Both refined measures available",
    "Preferred vertical variable",
    "Preferred horizontal variable",
    "Strict horizontal robustness variable",
    "Period-specific vertical robustness variable",
    "Output file size in GB",
    "Output file"
  ),
  
  value = c(
    as.character(input_rows),
    as.character(nrow(dt)),
    as.character(input_columns),
    as.character(ncol(dt)),
    as.character(ncol(dt) - input_columns),
    
    as.character(
      sum(
        dt$vertical_fullperiod_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$horizontal_detailed_eligible == 1L,
        na.rm = TRUE
      )
    ),
    
    as.character(
      sum(
        dt$both_refined_mismatch_available == 1L,
        na.rm = TRUE
      )
    ),
    
    "vertical_mismatch_fullperiod",
    "horizontal_mismatch_detailed_broad",
    "horizontal_mismatch_detailed_strict",
    "any_vertical_mismatch",
    
    as.character(
      round(
        output_file_size_gb,
        3
      )
    ),
    
    output_file
  )
)

fwrite(
  step5d_completion_summary,
  file.path(
    output_folder,
    "step5d_completion_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 30: Print final report
# ==============================================================================

cat(
  "\n============================================================\n"
)

cat(
  "STEP 5D COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================================\n"
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
  "Full-period vertical eligible:",
  format(
    sum(
      dt$vertical_fullperiod_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "Detailed horizontal eligible:",
  format(
    sum(
      dt$horizontal_detailed_eligible == 1L,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "\nPreferred mismatch variables:\n",
  "1. vertical_mismatch_fullperiod\n",
  "2. overeducated_fullperiod\n",
  "3. undereducated_fullperiod\n",
  "4. horizontal_mismatch_detailed_broad\n"
)

cat(
  "\nRobustness variables:\n",
  "1. any_vertical_mismatch\n",
  "2. horizontal_mismatch_detailed_strict\n",
  "3. horizontal_mismatch_broad\n"
)

cat(
  "\nRefined final dataset saved at:\n",
  output_file,
  "\n"
)

cat(
  "\nUpload these diagnostic files:\n",
  "1. step5d_completion_summary.csv\n",
  "2. step5d_refined_mismatch_summary.csv\n",
  "3. step5d_vertical_comparison_by_year.csv\n",
  "4. step5d_horizontal_refined_by_year.csv\n",
  "5. step5d_horizontal_refined_by_field.csv\n",
  "6. step5d_detailed_field_diagnostic.csv\n",
  "7. step5d_vertical_required_education_fullperiod.csv\n",
  "8. step5d_detailed_horizontal_crosswalk.csv\n"
)


# ==============================================================================
# STEP 31: Remove large objects after saving
# ==============================================================================

rm(
  dt,
  vertical_full_distribution,
  vertical_full_requirement,
  vertical_full_cell_sizes,
  horizontal_detailed_crosswalk,
  strict_crosswalk_long,
  broad_crosswalk_long
)

invisible(gc())

cat(
  "\nLarge in-memory objects were removed after saving.\n"
)









