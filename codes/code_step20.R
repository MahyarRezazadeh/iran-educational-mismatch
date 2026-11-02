# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 20A: Identify Exposure Variables for Timing and Placebo Tests
#
# Purpose:
#   Locate variables related to:
#   - undergraduate exposure
#   - enrollment exposure
#   - exposure at alternative ages
#   - province-year university expansion
#   - treatment completeness and coverage
# ==============================================================================


# ==============================================================================
# STEP 0: Clear environment and load package
# ==============================================================================

rm(list = ls())
invisible(gc())

library(fst)


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
  "step20_timing_placebo"
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
# STEP 2: Retrieve all variable names
# ==============================================================================

all_variable_names <- names(
  read_fst(
    input_file,
    from = 1,
    to = 1
  )
)

cat(
  "\nTotal variables:",
  length(all_variable_names),
  "\n"
)


# ==============================================================================
# STEP 3: Search for treatment and exposure variables
# ==============================================================================

exposure_variables <- all_variable_names[
  grepl(
    paste0(
      "exposure|enrollment|enrolment|undergrad|graduate|",
      "university|college|higher_education|highereducation|",
      "capacity|admission|student|expansion"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 4: Search specifically for age-window variables
# ==============================================================================

age_window_variables <- all_variable_names[
  grepl(
    paste0(
      "15_17|15to17|15\\.17|",
      "16_18|16to18|16\\.18|",
      "17_19|17to19|17\\.19|",
      "18_20|18to20|18\\.20|",
      "19_21|19to21|19\\.21|",
      "20_22|20to22|20\\.22|",
      "21_23|21to23|21\\.23|",
      "22_24|22to24|22\\.24|",
      "23_25|23to25|23\\.25|",
      "age|ages|window|future|placebo|lead|lag"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 5: Search for province-year enrollment source variables
# ==============================================================================

province_year_source_variables <- all_variable_names[
  grepl(
    paste0(
      "province.*year|year.*province|",
      "province.*enroll|enroll.*province|",
      "province.*student|student.*province|",
      "province.*university|university.*province|",
      "historical_province|province_historical|",
      "birth_year|cohort|calendar_year|persian_year"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 6: Search for treatment-completeness variables
# ==============================================================================

completeness_variables <- all_variable_names[
  grepl(
    paste0(
      "complete|coverage|available|valid|missing|",
      "exposure_years|number_of_years|n_years"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 7: Combine and sort all candidates
# ==============================================================================

all_candidate_variables <- sort(
  unique(
    c(
      exposure_variables,
      age_window_variables,
      province_year_source_variables,
      completeness_variables
    )
  )
)


# ==============================================================================
# STEP 8: Print results
# ==============================================================================

cat(
  "\n============================================================\n",
  "EXPOSURE AND TREATMENT VARIABLES\n",
  "============================================================\n"
)

print(exposure_variables)

cat(
  "\n============================================================\n",
  "AGE-WINDOW VARIABLES\n",
  "============================================================\n"
)

print(age_window_variables)

cat(
  "\n============================================================\n",
  "PROVINCE-YEAR SOURCE VARIABLES\n",
  "============================================================\n"
)

print(province_year_source_variables)

cat(
  "\n============================================================\n",
  "COMPLETENESS AND COVERAGE VARIABLES\n",
  "============================================================\n"
)

print(completeness_variables)

cat(
  "\n============================================================\n",
  "ALL UNIQUE CANDIDATES\n",
  "============================================================\n"
)

print(all_candidate_variables)


# ==============================================================================
# STEP 9: Save results
# ==============================================================================

writeLines(
  exposure_variables,
  file.path(
    output_folder,
    "step20A_exposure_variables.txt"
  )
)

writeLines(
  age_window_variables,
  file.path(
    output_folder,
    "step20A_age_window_variables.txt"
  )
)

writeLines(
  province_year_source_variables,
  file.path(
    output_folder,
    "step20A_province_year_source_variables.txt"
  )
)

writeLines(
  completeness_variables,
  file.path(
    output_folder,
    "step20A_completeness_variables.txt"
  )
)

writeLines(
  all_candidate_variables,
  file.path(
    output_folder,
    "step20A_all_candidate_variables.txt"
  )
)


# ==============================================================================
# STEP 10: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 20A COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nFiles saved in:\n",
  output_folder,
  "\n\n"
)

cat(
  "Upload these files:\n",
  "1. step20A_exposure_variables.txt\n",
  "2. step20A_age_window_variables.txt\n",
  "3. step20A_province_year_source_variables.txt\n",
  "4. step20A_completeness_variables.txt\n",
  "5. step20A_all_candidate_variables.txt\n"
)





# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 20B: Locate the Province-Year University Enrollment Source File
#
# Purpose:
#   Find the original province-year enrollment dataset used to construct:
#   - undergrad_per_100k_age18
#   - undergrad_per_100k_age19
#   - undergrad_per_100k_age20
#
# Once found, the next step will reconstruct exposure at:
#   - ages 15–17: pre-university timing test
#   - ages 18–20: preferred treatment
#   - ages 21–23: later university-age window
#   - ages 24–26: future/post-education placebo
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
library(fst)
library(haven)


# ==============================================================================
# STEP 1: Define paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step20_timing_placebo"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!dir.exists(project_folder)) {
  stop(
    paste0(
      "Project folder not found:\n",
      project_folder
    )
  )
}


# ==============================================================================
# STEP 2: List potentially relevant data files
#
# Search recursively for common data formats.
# Temporary files and Step 20 outputs are excluded.
# ==============================================================================

all_files <- list.files(
  project_folder,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)

candidate_files <- all_files[
  grepl(
    "\\.(csv|fst|dta|rds|xlsx|xls)$",
    all_files,
    ignore.case = TRUE
  )
]

candidate_files <- candidate_files[
  !grepl(
    "step20_timing_placebo",
    candidate_files,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 3: Score file names for likely relevance
# ==============================================================================

file_inventory <- data.table(
  full_path = candidate_files
)

file_inventory[
  ,
  file_name := basename(full_path)
]

file_inventory[
  ,
  extension := tolower(
    tools::file_ext(full_path)
  )
]

file_inventory[
  ,
  file_size_mb := round(
    file.info(full_path)$size /
      1024^2,
    3
  )
]

file_inventory[
  ,
  modified_time := file.info(full_path)$mtime
]

file_inventory[
  ,
  relevance_score :=
    5L * grepl(
      "enroll|enrol",
      full_path,
      ignore.case = TRUE
    ) +
    4L * grepl(
      "undergrad|university|student|higher",
      full_path,
      ignore.case = TRUE
    ) +
    3L * grepl(
      "province",
      full_path,
      ignore.case = TRUE
    ) +
    2L * grepl(
      "population|per.?100|capacity",
      full_path,
      ignore.case = TRUE
    ) +
    1L * grepl(
      "treatment|exposure|expansion",
      full_path,
      ignore.case = TRUE
    )
]

setorder(
  file_inventory,
  -relevance_score,
  file_size_mb
)

fwrite(
  file_inventory,
  file.path(
    output_folder,
    "step20B_project_file_inventory.csv"
  ),
  bom = TRUE
)

cat(
  "\n============================================================\n",
  "MOST LIKELY SOURCE FILES BASED ON FILE NAMES\n",
  "============================================================\n"
)

print(
  file_inventory[
    relevance_score > 0,
    .(
      relevance_score,
      file_name,
      extension,
      file_size_mb,
      full_path
    )
  ][1:min(.N, 100)]
)


# ==============================================================================
# STEP 4: Safe function to retrieve column names
#
# Only column names are retrieved when possible.
# This avoids loading large datasets into memory.
# ==============================================================================

get_column_names <- function(
    file_path,
    extension
) {
  
  result <- tryCatch(
    {
      
      if (extension == "fst") {
        
        return(
          names(
            read_fst(
              file_path,
              from = 1,
              to = 1
            )
          )
        )
      }
      
      if (extension == "csv") {
        
        return(
          names(
            fread(
              file_path,
              nrows = 0,
              showProgress = FALSE
            )
          )
        )
      }
      
      if (extension == "dta") {
        
        return(
          names(
            read_dta(
              file_path,
              n_max = 1
            )
          )
        )
      }
      
      if (extension == "rds") {
        
        object <- readRDS(
          file_path
        )
        
        if (
          is.data.frame(object) ||
          is.data.table(object)
        ) {
          return(names(object))
        }
        
        return(character())
      }
      
      if (
        extension %in% c(
          "xlsx",
          "xls"
        )
      ) {
        
        if (!requireNamespace(
          "readxl",
          quietly = TRUE
        )) {
          return(
            "READXL_PACKAGE_NOT_INSTALLED"
          )
        }
        
        return(
          names(
            readxl::read_excel(
              file_path,
              n_max = 1
            )
          )
        )
      }
      
      character()
    },
    
    error = function(e) {
      paste0(
        "ERROR: ",
        conditionMessage(e)
      )
    }
  )
  
  result
}


# ==============================================================================
# STEP 5: Inspect likely files for relevant column names
#
# To keep this efficient, inspect:
#   - every file with a positive relevance score;
#   - at most the first 100 such files.
# ==============================================================================

files_to_inspect <- file_inventory[
  relevance_score > 0
][
  1:min(.N, 100)
]

column_results <- vector(
  mode = "list",
  length = nrow(files_to_inspect)
)

for (
  file_index in
  seq_len(nrow(files_to_inspect))
) {
  
  current_path <-
    files_to_inspect$full_path[file_index]
  
  current_extension <-
    files_to_inspect$extension[file_index]
  
  cat(
    "\nInspecting",
    file_index,
    "of",
    nrow(files_to_inspect),
    ":\n",
    current_path,
    "\n"
  )
  
  current_columns <- get_column_names(
    file_path = current_path,
    extension = current_extension
  )
  
  column_results[[file_index]] <- data.table(
    full_path = current_path,
    file_name = basename(current_path),
    extension = current_extension,
    variable_name = current_columns
  )
}

column_inventory <- rbindlist(
  column_results,
  fill = TRUE
)


# ==============================================================================
# STEP 6: Flag variables needed to reconstruct exposure
# ==============================================================================

column_inventory[
  ,
  variable_relevance_score :=
    5L * grepl(
      "undergrad|enroll|enrol|student",
      variable_name,
      ignore.case = TRUE
    ) +
    4L * grepl(
      "province|ostan",
      variable_name,
      ignore.case = TRUE
    ) +
    4L * grepl(
      "^year$|calendar_year|persian_year|survey_year",
      variable_name,
      ignore.case = TRUE
    ) +
    3L * grepl(
      "population|per.?100|rate",
      variable_name,
      ignore.case = TRUE
    ) +
    2L * grepl(
      "university|higher|degree|level",
      variable_name,
      ignore.case = TRUE
    )
]

relevant_columns <- column_inventory[
  variable_relevance_score > 0
]

setorder(
  relevant_columns,
  full_path,
  -variable_relevance_score,
  variable_name
)

fwrite(
  column_inventory,
  file.path(
    output_folder,
    "step20B_all_inspected_columns.csv"
  ),
  bom = TRUE
)

fwrite(
  relevant_columns,
  file.path(
    output_folder,
    "step20B_relevant_source_columns.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 7: Summarize each candidate file
# ==============================================================================

candidate_summary <- relevant_columns[
  ,
  .(
    number_relevant_variables = .N,
    
    has_province_variable =
      any(
        grepl(
          "province|ostan",
          variable_name,
          ignore.case = TRUE
        )
      ),
    
    has_year_variable =
      any(
        grepl(
          "^year$|calendar_year|persian_year|survey_year",
          variable_name,
          ignore.case = TRUE
        )
      ),
    
    has_enrollment_variable =
      any(
        grepl(
          "undergrad|enroll|enrol|student",
          variable_name,
          ignore.case = TRUE
        )
      ),
    
    has_population_or_rate =
      any(
        grepl(
          "population|per.?100|rate",
          variable_name,
          ignore.case = TRUE
        )
      ),
    
    relevant_variables =
      paste(
        unique(variable_name),
        collapse = " | "
      )
  ),
  by = .(
    full_path,
    file_name,
    extension
  )
]

candidate_summary[
  ,
  likely_complete_source :=
    has_province_variable &
    has_year_variable &
    has_enrollment_variable
]

candidate_summary[
  ,
  source_score :=
    4L * likely_complete_source +
    1L * has_population_or_rate +
    log1p(number_relevant_variables)
]

setorder(
  candidate_summary,
  -source_score,
  -number_relevant_variables
)

fwrite(
  candidate_summary,
  file.path(
    output_folder,
    "step20B_candidate_source_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 8: Print likely source datasets
# ==============================================================================

cat(
  "\n============================================================\n",
  "LIKELY PROVINCE-YEAR ENROLLMENT SOURCE DATASETS\n",
  "============================================================\n"
)

print(
  candidate_summary[
    likely_complete_source == TRUE,
    .(
      file_name,
      extension,
      number_relevant_variables,
      has_population_or_rate,
      relevant_variables,
      full_path
    )
  ]
)


# ==============================================================================
# STEP 9: Print fallback candidates
# ==============================================================================

cat(
  "\n============================================================\n",
  "OTHER POSSIBLE SOURCE DATASETS\n",
  "============================================================\n"
)

print(
  candidate_summary[
    likely_complete_source == FALSE
  ][
    1:min(.N, 30),
    .(
      file_name,
      extension,
      number_relevant_variables,
      has_province_variable,
      has_year_variable,
      has_enrollment_variable,
      has_population_or_rate,
      relevant_variables,
      full_path
    )
  ]
)


# ==============================================================================
# STEP 10: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 20B COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nUpload these files:\n",
  "1. step20B_candidate_source_summary.csv\n",
  "2. step20B_relevant_source_columns.csv\n",
  "3. step20B_project_file_inventory.csv\n"
)

cat(
  "\nFiles were saved in:\n",
  output_folder,
  "\n"
)







# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 20C: Exposure Timing and Future-Exposure Placebo Tests
#
# Exposure windows:
#   Ages 15–17: pre-university window
#   Ages 18–20: preferred university-entry window
#   Ages 21–23: later university-age window
#   Ages 24–26: post-entry/future-exposure placebo window
#
# Central outcomes:
#   - Horizontal mismatch, broad
#   - Double mismatch
#   - Fully matched employment
#   - High-skill occupation among tertiary-employed workers
#   - Tertiary-educated lower-skill employment among all employed workers
#
# Two comparison approaches:
#   A. Maximum available sample for each exposure window
#   B. Common sample with all four exposure windows observed
#
# Important:
#   Exposure windows are likely highly correlated because university capacity
#   evolves persistently over time. The future-window coefficient is therefore
#   a demanding falsification test, not a mechanically clean placebo.
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
library(fst)
library(fixest)


# ==============================================================================
# STEP 1: Define paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

individual_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

exposure_panel_file <- file.path(
  project_folder,
  "LFS_reduced_outputs",
  "final_enrollment_population_exposure_panel_1373_1403_cleanzeros.rds"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step20_timing_placebo"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(individual_file)) {
  stop(
    paste0(
      "Individual analysis file not found:\n",
      individual_file
    )
  )
}

if (!file.exists(exposure_panel_file)) {
  stop(
    paste0(
      "Province-year exposure panel not found:\n",
      exposure_panel_file
    )
  )
}


# ==============================================================================
# STEP 2: Read and inspect province-year exposure panel
# ==============================================================================

exposure_panel <- as.data.table(
  readRDS(exposure_panel_file)
)

cat(
  "\nExposure panel dimensions:",
  nrow(exposure_panel),
  "rows and",
  ncol(exposure_panel),
  "variables\n"
)

required_exposure_variables <- c(
  "province_historical",
  "exposure_year",
  "undergrad_professional_per_100k_clean"
)

missing_exposure_variables <- setdiff(
  required_exposure_variables,
  names(exposure_panel)
)

if (length(missing_exposure_variables) > 0L) {
  stop(
    paste0(
      "Missing required exposure variables:\n",
      paste(
        missing_exposure_variables,
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# STEP 3: Clean province-year exposure panel
# ==============================================================================

exposure_panel <- exposure_panel[
  ,
  .(
    province_historical =
      as.character(province_historical),
    
    exposure_year =
      as.integer(exposure_year),
    
    undergrad_rate =
      as.numeric(
        undergrad_professional_per_100k_clean
      )
  )
]

exposure_panel <- exposure_panel[
  !is.na(province_historical) &
    !is.na(exposure_year) &
    !is.na(undergrad_rate) &
    is.finite(undergrad_rate) &
    undergrad_rate > 0
]

duplicate_exposure_cells <- exposure_panel[
  ,
  .N,
  by = .(
    province_historical,
    exposure_year
  )
][
  N > 1L
]

if (nrow(duplicate_exposure_cells) > 0L) {
  stop(
    "The source exposure panel contains duplicate province-year cells."
  )
}

setkey(
  exposure_panel,
  province_historical,
  exposure_year
)

exposure_panel_validation <- exposure_panel[
  ,
  .(
    observations = .N,
    provinces = uniqueN(province_historical),
    first_year = min(exposure_year),
    last_year = max(exposure_year),
    minimum_rate = min(undergrad_rate),
    maximum_rate = max(undergrad_rate),
    missing_rate = sum(is.na(undergrad_rate))
  )
]

print(exposure_panel_validation)

fwrite(
  exposure_panel_validation,
  file.path(
    output_folder,
    "step20C_exposure_panel_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 4: Read individual-level variables
# ==============================================================================

variables_to_read <- c(
  # Identifiers, controls, treatment, and weights
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Employment and education
  "employed",
  "tertiary_educated",
  
  # Occupation
  "isco_2digit",
  
  # Horizontal mismatch
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  
  # Preferred vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "vertically_matched_final"
)

dt <- as.data.table(
  read_fst(
    individual_file,
    columns = variables_to_read
  )
)

cat(
  "\nIndividual observations:",
  format(nrow(dt), big.mark = ","),
  "\n"
)


# ==============================================================================
# STEP 5: Construct common analytical variables
# ==============================================================================

dt[
  ,
  historical_province :=
    as.character(province_historical)
]

dt[
  ,
  cohort :=
    as.integer(analysis_birth_year)
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
  valid_weight :=
    !is.na(weight) &
    is.finite(weight) &
    weight > 0
]

dt[
  ,
  base_controls_available :=
    valid_weight &
    !is.na(historical_province) &
    !is.na(cohort) &
    cohort >= 1355L &
    cohort <= 1383L &
    !is.na(year) &
    !is.na(gender) &
    !is.na(female) &
    !is.na(urban)
]


# ==============================================================================
# STEP 6: Create province-cohort treatment cells
#
# Treatment is constant within historical province × birth cohort.
# Construct it once at that level and merge it back to individuals.
# ==============================================================================

treatment_cells <- unique(
  dt[
    base_controls_available == TRUE,
    .(
      historical_province,
      cohort
    )
  ]
)

setnames(
  treatment_cells,
  "historical_province",
  "province_historical"
)


# ==============================================================================
# STEP 7: Corrected helper function to construct an exposure window
# ==============================================================================

construct_exposure_window <- function(
    treatment_data,
    exposure_data,
    start_age,
    end_age,
    window_name
) {
  
  age_sequence <- seq.int(
    start_age,
    end_age
  )
  
  expanded <- treatment_data[
    rep(
      seq_len(.N),
      each = length(age_sequence)
    )
  ]
  
  expanded[
    ,
    exposure_age :=
      rep(
        age_sequence,
        times = nrow(treatment_data)
      )
  ]
  
  expanded[
    ,
    exposure_year :=
      cohort + exposure_age
  ]
  
  expanded <- merge(
    expanded,
    exposure_data,
    by = c(
      "province_historical",
      "exposure_year"
    ),
    all.x = TRUE,
    sort = FALSE
  )
  
  result <- expanded[
    ,
    .(
      years_required =
        length(age_sequence),
      
      years_available =
        sum(!is.na(undergrad_rate)),
      
      complete =
        all(!is.na(undergrad_rate)),
      
      exposure =
        if (
          all(!is.na(undergrad_rate))
        ) {
          mean(undergrad_rate)
        } else {
          NA_real_
        }
    ),
    by = .(
      province_historical,
      cohort
    )
  ]
  
  # Give every window-specific variable a unique name.
  setnames(
    result,
    old = c(
      "years_required",
      "years_available",
      "complete",
      "exposure"
    ),
    new = c(
      paste0("required_", window_name),
      paste0("n_", window_name),
      paste0("complete_", window_name),
      window_name
    )
  )
  
  result[
    ,
    paste0("log_", window_name) :=
      fifelse(
        get(window_name) > 0,
        log(get(window_name)),
        NA_real_
      )
  ]
  
  return(result)
}


# ==============================================================================
# STEP 8: Reconstruct all four exposure windows
# ==============================================================================

exposure_15_17 <- construct_exposure_window(
  treatment_data = treatment_cells,
  exposure_data = exposure_panel,
  start_age = 15L,
  end_age = 17L,
  window_name = "exposure_15_17"
)

exposure_18_20 <- construct_exposure_window(
  treatment_data = treatment_cells,
  exposure_data = exposure_panel,
  start_age = 18L,
  end_age = 20L,
  window_name = "exposure_18_20_rebuilt"
)

exposure_21_23 <- construct_exposure_window(
  treatment_data = treatment_cells,
  exposure_data = exposure_panel,
  start_age = 21L,
  end_age = 23L,
  window_name = "exposure_21_23"
)

exposure_24_26 <- construct_exposure_window(
  treatment_data = treatment_cells,
  exposure_data = exposure_panel,
  start_age = 24L,
  end_age = 26L,
  window_name = "exposure_24_26"
)


# ==============================================================================
# STEP 9: Combine treatment windows safely
# ==============================================================================

treatment_windows <- Reduce(
  function(x, y) {
    
    merge(
      x,
      y,
      by = c(
        "province_historical",
        "cohort"
      ),
      all = TRUE,
      sort = FALSE
    )
    
  },
  list(
    exposure_15_17,
    exposure_18_20,
    exposure_21_23,
    exposure_24_26
  )
)

# Confirm that no duplicated names remain.
duplicated_names <- names(treatment_windows)[
  duplicated(names(treatment_windows))
]

if (length(duplicated_names) > 0L) {
  stop(
    paste0(
      "Duplicated variable names remain in treatment_windows: ",
      paste(
        duplicated_names,
        collapse = ", "
      )
    )
  )
}

treatment_windows[
  ,
  complete_all_windows :=
    complete_exposure_15_17 == TRUE &
    complete_exposure_18_20_rebuilt == TRUE &
    complete_exposure_21_23 == TRUE &
    complete_exposure_24_26 == TRUE
]

cat(
  "\nTreatment-window dataset created successfully.\n",
  "Rows:",
  nrow(treatment_windows),
  "\nVariables:",
  ncol(treatment_windows),
  "\n"
)

print(names(treatment_windows))

# ==============================================================================
# STEP 10: Validate rebuilt preferred treatment
# ==============================================================================

preferred_treatment_validation <- unique(
  dt[
    base_controls_available == TRUE &
      complete_undergrad_exposure == 1L &
      !is.na(undergrad_exposure_18_20),
    .(
      historical_province,
      cohort,
      existing_exposure_18_20 =
        undergrad_exposure_18_20
    )
  ]
)

setnames(
  preferred_treatment_validation,
  "historical_province",
  "province_historical"
)

preferred_treatment_validation <- merge(
  preferred_treatment_validation,
  treatment_windows[
    ,
    .(
      province_historical,
      cohort,
      exposure_18_20_rebuilt,
      complete_exposure_18_20_rebuilt
    )
  ],
  by = c(
    "province_historical",
    "cohort"
  ),
  all.x = TRUE
)

preferred_treatment_validation[
  ,
  absolute_difference :=
    abs(
      existing_exposure_18_20 -
        exposure_18_20_rebuilt
    )
]

preferred_validation_summary <- preferred_treatment_validation[
  ,
  .(
    treatment_cells = .N,
    
    complete_rebuilt_cells =
      sum(
        complete_exposure_18_20_rebuilt,
        na.rm = TRUE
      ),
    
    maximum_absolute_difference =
      max(
        absolute_difference,
        na.rm = TRUE
      ),
    
    mean_absolute_difference =
      mean(
        absolute_difference,
        na.rm = TRUE
      ),
    
    correlation =
      cor(
        existing_exposure_18_20,
        exposure_18_20_rebuilt,
        use = "complete.obs"
      )
  )
]

print(preferred_validation_summary)

fwrite(
  preferred_validation_summary,
  file.path(
    output_folder,
    "step20C_preferred_treatment_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Validate window coverage by cohort
# ==============================================================================

window_coverage_by_cohort <- treatment_windows[
  ,
  .(
    province_cells = .N,
    
    complete_15_17 =
      sum(
        complete_exposure_15_17,
        na.rm = TRUE
      ),
    
    complete_18_20 =
      sum(
        complete_exposure_18_20_rebuilt,
        na.rm = TRUE
      ),
    
    complete_21_23 =
      sum(
        complete_exposure_21_23,
        na.rm = TRUE
      ),
    
    complete_24_26 =
      sum(
        complete_exposure_24_26,
        na.rm = TRUE
      ),
    
    complete_all =
      sum(
        complete_all_windows,
        na.rm = TRUE
      )
  ),
  by = cohort
]

setorder(
  window_coverage_by_cohort,
  cohort
)

print(window_coverage_by_cohort)

fwrite(
  window_coverage_by_cohort,
  file.path(
    output_folder,
    "step20C_window_coverage_by_cohort.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Save treatment-cell dataset
# ==============================================================================

fwrite(
  treatment_windows,
  file.path(
    output_folder,
    "step20C_province_cohort_exposure_windows.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Merge exposure windows into individual data
# ==============================================================================

dt <- merge(
  dt,
  treatment_windows,
  by.x = c(
    "historical_province",
    "cohort"
  ),
  by.y = c(
    "province_historical",
    "cohort"
  ),
  all.x = TRUE,
  sort = FALSE
)


# ==============================================================================
# STEP 14: Clean occupation and construct skill outcomes
# ==============================================================================

dt[
  ,
  isco2_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isco_2digit)
      )
    )
  )
]

dt[
  ,
  isco_major_group := fcase(
    isco2_clean >= 10L &
      isco2_clean <= 99L,
    as.integer(
      floor(isco2_clean / 10)
    ),
    
    isco2_clean >= 1L &
      isco2_clean <= 9L,
    isco2_clean,
    
    default = NA_integer_
  )
]

dt[
  !isco_major_group %in% 1:9,
  isco_major_group := NA_integer_
]


# ==============================================================================
# STEP 15: Construct central outcomes and eligible samples
# ==============================================================================

dt[
  ,
  horizontal_sample :=
    base_controls_available == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(horizontal_mismatch_detailed_broad)
]

dt[
  ,
  joint_sample :=
    horizontal_sample == TRUE &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(vertically_matched_final)
]

dt[
  ,
  double_mismatch := fifelse(
    joint_sample,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched := fifelse(
    joint_sample,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  graduate_occupation_sample :=
    base_controls_available == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    !is.na(isco_major_group)
]

dt[
  ,
  graduate_high_skill := fifelse(
    graduate_occupation_sample,
    as.integer(
      isco_major_group %in% 1:3
    ),
    NA_integer_
  )
]

dt[
  ,
  all_employed_occupation_sample :=
    base_controls_available == TRUE &
    employed == 1L &
    !is.na(tertiary_educated) &
    !is.na(isco_major_group)
]

dt[
  ,
  tertiary_lower_skill := fifelse(
    all_employed_occupation_sample,
    as.integer(
      tertiary_educated == 1L &
        isco_major_group %in% 4:9
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 16: Define outcomes and exposure windows
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Horizontal mismatch",
    "Double mismatch",
    "Fully matched",
    "High-skill occupation among tertiary-employed workers",
    "Tertiary-educated lower-skill employment"
  ),
  
  outcome_variable = c(
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched",
    "graduate_high_skill",
    "tertiary_lower_skill"
  ),
  
  sample_variable = c(
    "horizontal_sample",
    "joint_sample",
    "joint_sample",
    "graduate_occupation_sample",
    "all_employed_occupation_sample"
  )
)

window_definitions <- data.table(
  exposure_window = c(
    "Ages 15–17",
    "Ages 18–20",
    "Ages 21–23",
    "Ages 24–26"
  ),
  
  treatment_variable = c(
    "log_exposure_15_17",
    "log_exposure_18_20_rebuilt",
    "log_exposure_21_23",
    "log_exposure_24_26"
  ),
  
  completeness_variable = c(
    "complete_exposure_15_17",
    "complete_exposure_18_20_rebuilt",
    "complete_exposure_21_23",
    "complete_exposure_24_26"
  ),
  
  timing_order = 1:4
)


# ==============================================================================
# STEP 17: Exposure correlation matrix on common treatment cells
# ==============================================================================

common_treatment_cells <- treatment_windows[
  complete_all_windows == TRUE
]

exposure_correlation_matrix <- cor(
  common_treatment_cells[
    ,
    .(
      log_exposure_15_17,
      log_exposure_18_20_rebuilt,
      log_exposure_21_23,
      log_exposure_24_26
    )
  ],
  use = "complete.obs"
)

exposure_correlation_table <- as.data.table(
  exposure_correlation_matrix,
  keep.rownames = "exposure_window"
)

print(exposure_correlation_table)

fwrite(
  exposure_correlation_table,
  file.path(
    output_folder,
    "step20C_exposure_window_correlations.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 18: Regression helper
# ==============================================================================

estimate_timing_model <- function(
    model_data,
    outcome_variable,
    treatment_variable,
    subgroup
) {
  
  controls <- if (
    subgroup == "Pooled"
  ) {
    "female + urban"
  } else {
    "urban"
  }
  
  regression_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      treatment_variable,
      " + ",
      controls,
      " | historical_province + cohort + year"
    )
  )
  
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
# STEP 19: Result-extraction helper
# ==============================================================================

extract_timing_result <- function(
    model,
    outcome,
    exposure_window,
    treatment_variable,
    subgroup,
    sample_type,
    model_data
) {
  
  coefficient <- unname(
    coef(model)[treatment_variable]
  )
  
  standard_error <- unname(
    se(model)[treatment_variable]
  )
  
  p_value <- unname(
    pvalue(model)[treatment_variable]
  )
  
  confidence_interval <- confint(
    model,
    parm = treatment_variable,
    level = 0.95
  )
  
  conversion_factor <- 100 * log(1.10)
  
  data.table(
    outcome = outcome,
    exposure_window = exposure_window,
    treatment_variable = treatment_variable,
    subgroup = subgroup,
    sample_type = sample_type,
    
    effect_pp_for_10_percent_increase =
      conversion_factor * coefficient,
    
    standard_error_pp =
      conversion_factor * standard_error,
    
    lower_95_pp =
      conversion_factor *
      confidence_interval[1, 1],
    
    upper_95_pp =
      conversion_factor *
      confidence_interval[1, 2],
    
    p_value = p_value,
    
    significance = fcase(
      p_value < 0.01, "***",
      p_value < 0.05, "**",
      p_value < 0.10, "*",
      default = ""
    ),
    
    observations = nobs(model),
    
    provinces =
      uniqueN(model_data$historical_province),
    
    cohorts =
      uniqueN(model_data$cohort),
    
    minimum_cohort =
      min(model_data$cohort),
    
    maximum_cohort =
      max(model_data$cohort),
    
    survey_years =
      uniqueN(model_data$year)
  )
}


# ==============================================================================
# STEP 20: Estimate separate-window models
#
# Each window is estimated:
#   A. On its maximum complete sample
#   B. On the common sample where all four windows are complete
# ==============================================================================

results <- list()
failures <- list()

result_counter <- 1L
failure_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome <-
    outcome_definitions$outcome[outcome_index]
  
  current_outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_sample_variable <-
    outcome_definitions$sample_variable[outcome_index]
  
  for (
    window_index in
    seq_len(nrow(window_definitions))
  ) {
    
    current_window <-
      window_definitions$exposure_window[window_index]
    
    current_treatment <-
      window_definitions$treatment_variable[window_index]
    
    current_completeness <-
      window_definitions$completeness_variable[window_index]
    
    for (
      current_subgroup in
      c(
        "Pooled",
        "Women",
        "Men"
      )
    ) {
      
      for (
        current_sample_type in
        c(
          "Maximum available sample",
          "Common four-window sample"
        )
      ) {
        
        model_data <- dt[
          get(current_sample_variable) == TRUE &
            !is.na(get(current_outcome_variable)) &
            get(current_completeness) == TRUE &
            !is.na(get(current_treatment))
        ]
        
        if (
          current_sample_type ==
          "Common four-window sample"
        ) {
          model_data <- model_data[
            complete_all_windows == TRUE
          ]
        }
        
        if (current_subgroup == "Women") {
          model_data <- model_data[
            gender == 1L
          ]
        }
        
        if (current_subgroup == "Men") {
          model_data <- model_data[
            gender == 2L
          ]
        }
        
        cat(
          "\nEstimating:",
          current_outcome,
          "|",
          current_window,
          "|",
          current_subgroup,
          "|",
          current_sample_type,
          "\n"
        )
        
        if (
          nrow(model_data) < 500L ||
          uniqueN(model_data$historical_province) < 10L ||
          uniqueN(model_data$cohort) < 8L
        ) {
          
          failures[[failure_counter]] <- data.table(
            outcome = current_outcome,
            exposure_window = current_window,
            subgroup = current_subgroup,
            sample_type = current_sample_type,
            reason = "Insufficient observations or identifying clusters",
            observations = nrow(model_data)
          )
          
          failure_counter <- failure_counter + 1L
          next
        }
        
        current_model <- tryCatch(
          estimate_timing_model(
            model_data = model_data,
            outcome_variable =
              current_outcome_variable,
            treatment_variable =
              current_treatment,
            subgroup =
              current_subgroup
          ),
          error = function(e) e
        )
        
        if (inherits(current_model, "error")) {
          
          failures[[failure_counter]] <- data.table(
            outcome = current_outcome,
            exposure_window = current_window,
            subgroup = current_subgroup,
            sample_type = current_sample_type,
            reason = current_model$message,
            observations = nrow(model_data)
          )
          
          failure_counter <- failure_counter + 1L
          next
        }
        
        results[[result_counter]] <-
          extract_timing_result(
            model = current_model,
            outcome = current_outcome,
            exposure_window = current_window,
            treatment_variable =
              current_treatment,
            subgroup =
              current_subgroup,
            sample_type =
              current_sample_type,
            model_data =
              model_data
          )
        
        result_counter <- result_counter + 1L
        
        rm(
          model_data,
          current_model
        )
        
        invisible(gc())
      }
    }
  }
}


# ==============================================================================
# STEP 21: Combine and adjust results
# ==============================================================================

timing_results <- rbindlist(
  results,
  fill = TRUE
)

timing_results <- merge(
  timing_results,
  window_definitions[
    ,
    .(
      exposure_window,
      timing_order
    )
  ],
  by = "exposure_window",
  all.x = TRUE
)

setorder(
  timing_results,
  outcome,
  subgroup,
  sample_type,
  timing_order
)

timing_results[
  ,
  p_value_fdr_within_outcome :=
    p.adjust(
      p_value,
      method = "BH"
    ),
  by = .(
    outcome,
    subgroup,
    sample_type
  )
]

timing_results[
  ,
  survives_fdr_10 :=
    p_value_fdr_within_outcome < 0.10
]

fwrite(
  timing_results,
  file.path(
    output_folder,
    "step20C_timing_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Compact results
# ==============================================================================

compact_results <- timing_results[
  ,
  .(
    outcome,
    exposure_window,
    timing_order,
    subgroup,
    sample_type,
    
    estimate =
      round(
        effect_pp_for_10_percent_increase,
        3
      ),
    
    standard_error =
      round(
        standard_error_pp,
        3
      ),
    
    lower_95 =
      round(
        lower_95_pp,
        3
      ),
    
    upper_95 =
      round(
        upper_95_pp,
        3
      ),
    
    p_value =
      round(
        p_value,
        4
      ),
    
    fdr =
      round(
        p_value_fdr_within_outcome,
        4
      ),
    
    significance,
    survives_fdr_10,
    observations,
    provinces,
    cohorts,
    minimum_cohort,
    maximum_cohort,
    survey_years
  )
]

compact_results[
  ,
  table_entry := paste0(
    formatC(
      estimate,
      format = "f",
      digits = 3
    ),
    significance,
    " (",
    formatC(
      standard_error,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step20C_timing_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Common-sample pooled table
# ==============================================================================

common_pooled_results <- compact_results[
  subgroup == "Pooled" &
    sample_type == "Common four-window sample"
]

common_pooled_table <- dcast(
  common_pooled_results,
  exposure_window + timing_order ~ outcome,
  value.var = "table_entry"
)

setorder(
  common_pooled_table,
  timing_order
)

fwrite(
  common_pooled_table,
  file.path(
    output_folder,
    "step20C_common_sample_pooled_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Common-sample gender table
# ==============================================================================

common_gender_results <- compact_results[
  sample_type == "Common four-window sample" &
    outcome %in% c(
      "Horizontal mismatch",
      "Double mismatch",
      "Fully matched",
      "High-skill occupation among tertiary-employed workers"
    )
]

common_gender_table <- dcast(
  common_gender_results,
  outcome + exposure_window + timing_order ~ subgroup,
  value.var = "table_entry"
)

setorder(
  common_gender_table,
  outcome,
  timing_order
)

fwrite(
  common_gender_table,
  file.path(
    output_folder,
    "step20C_common_sample_gender_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Timing-pattern summary
# ==============================================================================

timing_pattern_summary <- timing_results[
  sample_type == "Common four-window sample",
  .(
    preferred_effect =
      effect_pp_for_10_percent_increase[
        exposure_window == "Ages 18–20"
      ],
    
    pre_effect =
      effect_pp_for_10_percent_increase[
        exposure_window == "Ages 15–17"
      ],
    
    later_effect =
      effect_pp_for_10_percent_increase[
        exposure_window == "Ages 21–23"
      ],
    
    future_effect =
      effect_pp_for_10_percent_increase[
        exposure_window == "Ages 24–26"
      ],
    
    preferred_p_value =
      p_value[
        exposure_window == "Ages 18–20"
      ],
    
    future_p_value =
      p_value[
        exposure_window == "Ages 24–26"
      ]
  ),
  by = .(
    outcome,
    subgroup
  )
]

timing_pattern_summary[
  ,
  preferred_larger_than_pre :=
    abs(preferred_effect) >
    abs(pre_effect)
]

timing_pattern_summary[
  ,
  preferred_larger_than_later :=
    abs(preferred_effect) >
    abs(later_effect)
]

timing_pattern_summary[
  ,
  preferred_larger_than_future :=
    abs(preferred_effect) >
    abs(future_effect)
]

fwrite(
  timing_pattern_summary,
  file.path(
    output_folder,
    "step20C_timing_pattern_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Failed-model log
# ==============================================================================

if (length(failures) > 0L) {
  
  failed_models <- rbindlist(
    failures,
    fill = TRUE
  )
  
} else {
  
  failed_models <- data.table(
    outcome = character(),
    exposure_window = character(),
    subgroup = character(),
    sample_type = character(),
    reason = character(),
    observations = integer()
  )
}

fwrite(
  failed_models,
  file.path(
    output_folder,
    "step20C_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Print central common-sample results
# ==============================================================================

cat(
  "\n============================================================\n",
  "COMMON-SAMPLE TIMING RESULTS\n",
  "============================================================\n"
)

print(
  timing_results[
    sample_type == "Common four-window sample" &
      subgroup == "Pooled",
    .(
      outcome,
      exposure_window,
      
      effect_pp =
        round(
          effect_pp_for_10_percent_increase,
          3
        ),
      
      standard_error =
        round(
          standard_error_pp,
          3
        ),
      
      p_value =
        round(
          p_value,
          4
        ),
      
      FDR =
        round(
          p_value_fdr_within_outcome,
          4
        ),
      
      observations,
      minimum_cohort,
      maximum_cohort
    )
  ]
)


# ==============================================================================
# STEP 28: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 20C COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step20C_exposure_panel_validation.csv\n",
  "2. step20C_preferred_treatment_validation.csv\n",
  "3. step20C_window_coverage_by_cohort.csv\n",
  "4. step20C_province_cohort_exposure_windows.csv\n",
  "5. step20C_exposure_window_correlations.csv\n",
  "6. step20C_timing_regression_results.csv\n",
  "7. step20C_timing_compact_results.csv\n",
  "8. step20C_common_sample_pooled_table_wide.csv\n",
  "9. step20C_common_sample_gender_table_wide.csv\n",
  "10. step20C_timing_pattern_summary.csv\n",
  "11. step20C_failed_models.csv\n"
)

cat(
  "\nInterpretation rules:\n",
  "- First confirm that rebuilt ages 18–20 exposure matches the existing treatment.\n",
  "- Compare windows primarily on the common four-window sample.\n",
  "- Do not treat nearby age windows as independent treatments.\n",
  "- Check the exposure correlation matrix before interpreting timing differences.\n",
  "- A significant future-window coefficient is not automatically fatal if the\n",
  "  windows are extremely correlated, but it weakens a sharp age-specific claim.\n",
  "- The strongest timing evidence would be a larger ages 18–20 estimate and a\n",
  "  small or insignificant ages 24–26 estimate.\n"
)





# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 20D: Joint Preferred-Window and Future-Exposure Models
#
# Purpose:
#   Estimate exposure at ages 18–20 and ages 24–26 simultaneously.
#
# Main questions:
#   1. Does ages 18–20 exposure remain predictive after controlling for
#      subsequent university expansion?
#   2. Does ages 24–26 exposure retain independent explanatory power?
#   3. Are the two exposure coefficients statistically different?
#
# Outcomes:
#   - Horizontal mismatch
#   - Double mismatch
#   - Fully matched employment
#   - High-skill occupation among tertiary-employed workers
#   - Tertiary-educated lower-skill employment among all employed workers
#
# Sample:
#   Common sample with complete exposure at ages 18–20 and 24–26.
#
# Fixed effects:
#   Historical province
#   Corrected birth cohort
#   Survey year
#
# Controls:
#   Pooled: female and urban
#   Gender-specific: urban
#
# Inference:
#   Survey-weighted linear probability models
#   Two-way clustered SE by historical province and cohort
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
library(fst)
library(fixest)


# ==============================================================================
# STEP 1: Define paths
# ==============================================================================

project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"

individual_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "lfs_final_analysis_ready.fst"
)

exposure_window_file <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step20_timing_placebo",
  "step20C_province_cohort_exposure_windows.csv"
)

output_folder <- file.path(
  project_folder,
  "LFS_analysis_outputs",
  "step20_timing_placebo"
)

dir.create(
  output_folder,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(individual_file)) {
  stop(
    paste0(
      "Individual analysis file not found:\n",
      individual_file
    )
  )
}

if (!file.exists(exposure_window_file)) {
  stop(
    paste0(
      "Step 20C exposure-window file not found:\n",
      exposure_window_file
    )
  )
}


# ==============================================================================
# STEP 2: Read province-cohort exposure windows
# ==============================================================================

treatment_windows <- fread(
  exposure_window_file,
  showProgress = FALSE
)

required_treatment_variables <- c(
  "province_historical",
  "cohort",
  "log_exposure_18_20_rebuilt",
  "log_exposure_24_26",
  "complete_exposure_18_20_rebuilt",
  "complete_exposure_24_26"
)

missing_treatment_variables <- setdiff(
  required_treatment_variables,
  names(treatment_windows)
)

if (length(missing_treatment_variables) > 0L) {
  stop(
    paste0(
      "Missing treatment-window variables:\n",
      paste(
        missing_treatment_variables,
        collapse = ", "
      )
    )
  )
}

treatment_windows[
  ,
  province_historical :=
    as.character(province_historical)
]

treatment_windows[
  ,
  cohort :=
    as.integer(cohort)
]

treatment_windows[
  ,
  complete_pair :=
    complete_exposure_18_20_rebuilt == TRUE &
    complete_exposure_24_26 == TRUE &
    !is.na(log_exposure_18_20_rebuilt) &
    !is.na(log_exposure_24_26)
]

pair_treatment_cells <- treatment_windows[
  complete_pair == TRUE,
  .(
    province_historical,
    cohort,
    log_exposure_18_20_rebuilt,
    log_exposure_24_26
  )
]


# ==============================================================================
# STEP 3: Validate treatment-cell structure
# ==============================================================================

duplicate_cells <- pair_treatment_cells[
  ,
  .N,
  by = .(
    province_historical,
    cohort
  )
][
  N > 1L
]

if (nrow(duplicate_cells) > 0L) {
  stop(
    "Duplicate province-cohort treatment cells were found."
  )
}

treatment_validation <- pair_treatment_cells[
  ,
  .(
    treatment_cells = .N,
    
    provinces =
      uniqueN(province_historical),
    
    cohorts =
      uniqueN(cohort),
    
    minimum_cohort =
      min(cohort),
    
    maximum_cohort =
      max(cohort),
    
    exposure_correlation =
      cor(
        log_exposure_18_20_rebuilt,
        log_exposure_24_26,
        use = "complete.obs"
      )
  )
]

print(treatment_validation)

fwrite(
  treatment_validation,
  file.path(
    output_folder,
    "step20D_treatment_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 4: Read individual-level variables
# ==============================================================================

variables_to_read <- c(
  # Identifiers, controls, and weights
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  
  # Employment and education
  "employed",
  "tertiary_educated",
  
  # Occupation
  "isco_2digit",
  
  # Horizontal mismatch
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  
  # Preferred vertical mismatch
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "vertically_matched_final"
)

dt <- as.data.table(
  read_fst(
    individual_file,
    columns = variables_to_read
  )
)

cat(
  "\nIndividual observations:",
  format(nrow(dt), big.mark = ","),
  "\n"
)


# ==============================================================================
# STEP 5: Construct analytical variables
# ==============================================================================

dt[
  ,
  historical_province :=
    as.character(province_historical)
]

dt[
  ,
  cohort :=
    as.integer(analysis_birth_year)
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
  valid_weight :=
    !is.na(weight) &
    is.finite(weight) &
    weight > 0
]

dt[
  ,
  base_controls_available :=
    valid_weight &
    !is.na(historical_province) &
    !is.na(cohort) &
    cohort >= 1355L &
    cohort <= 1383L &
    !is.na(year) &
    !is.na(gender) &
    !is.na(female) &
    !is.na(urban)
]


# ==============================================================================
# STEP 6: Merge pairwise exposure variables
# ==============================================================================

dt <- merge(
  dt,
  pair_treatment_cells,
  by.x = c(
    "historical_province",
    "cohort"
  ),
  by.y = c(
    "province_historical",
    "cohort"
  ),
  all.x = TRUE,
  sort = FALSE
)

dt[
  ,
  pair_exposure_available :=
    !is.na(log_exposure_18_20_rebuilt) &
    !is.na(log_exposure_24_26)
]


# ==============================================================================
# STEP 7: Clean occupation codes
# ==============================================================================

dt[
  ,
  isco2_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isco_2digit)
      )
    )
  )
]

dt[
  ,
  isco_major_group := fcase(
    isco2_clean >= 10L &
      isco2_clean <= 99L,
    as.integer(
      floor(isco2_clean / 10)
    ),
    
    isco2_clean >= 1L &
      isco2_clean <= 9L,
    isco2_clean,
    
    default = NA_integer_
  )
]

dt[
  !isco_major_group %in% 1:9,
  isco_major_group := NA_integer_
]


# ==============================================================================
# STEP 8: Construct eligible samples
# ==============================================================================

dt[
  ,
  horizontal_sample :=
    base_controls_available == TRUE &
    pair_exposure_available == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    horizontal_detailed_eligible == 1L &
    !is.na(horizontal_mismatch_detailed_broad)
]

dt[
  ,
  joint_sample :=
    horizontal_sample == TRUE &
    vertical_fullperiod_eligible_final == 1L &
    !is.na(overeducated_final) &
    !is.na(vertically_matched_final)
]

dt[
  ,
  graduate_occupation_sample :=
    base_controls_available == TRUE &
    pair_exposure_available == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    !is.na(isco_major_group)
]

dt[
  ,
  all_employed_occupation_sample :=
    base_controls_available == TRUE &
    pair_exposure_available == TRUE &
    employed == 1L &
    !is.na(tertiary_educated) &
    !is.na(isco_major_group)
]


# ==============================================================================
# STEP 9: Construct outcomes
# ==============================================================================

dt[
  ,
  double_mismatch := fifelse(
    joint_sample,
    as.integer(
      overeducated_final == 1L &
        horizontal_mismatch_detailed_broad == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  fully_matched := fifelse(
    joint_sample,
    as.integer(
      vertically_matched_final == 1L &
        horizontal_mismatch_detailed_broad == 0L
    ),
    NA_integer_
  )
]

dt[
  ,
  graduate_high_skill := fifelse(
    graduate_occupation_sample,
    as.integer(
      isco_major_group %in% 1:3
    ),
    NA_integer_
  )
]

dt[
  ,
  tertiary_lower_skill := fifelse(
    all_employed_occupation_sample,
    as.integer(
      tertiary_educated == 1L &
        isco_major_group %in% 4:9
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 10: Validate outcome construction
# ==============================================================================

outcome_validation <- dt[
  ,
  .(
    horizontal_sample =
      sum(
        horizontal_sample,
        na.rm = TRUE
      ),
    
    joint_sample =
      sum(
        joint_sample,
        na.rm = TRUE
      ),
    
    graduate_occupation_sample =
      sum(
        graduate_occupation_sample,
        na.rm = TRUE
      ),
    
    all_employed_occupation_sample =
      sum(
        all_employed_occupation_sample,
        na.rm = TRUE
      ),
    
    provinces =
      uniqueN(
        historical_province[
          pair_exposure_available == TRUE
        ]
      ),
    
    cohorts =
      uniqueN(
        cohort[
          pair_exposure_available == TRUE
        ]
      ),
    
    minimum_cohort =
      min(
        cohort[
          pair_exposure_available == TRUE
        ],
        na.rm = TRUE
      ),
    
    maximum_cohort =
      max(
        cohort[
          pair_exposure_available == TRUE
        ],
        na.rm = TRUE
      )
  )
]

print(outcome_validation)

fwrite(
  outcome_validation,
  file.path(
    output_folder,
    "step20D_outcome_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Define outcomes
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Horizontal mismatch",
    "Double mismatch",
    "Fully matched",
    "High-skill occupation among tertiary-employed workers",
    "Tertiary-educated lower-skill employment"
  ),
  
  outcome_variable = c(
    "horizontal_mismatch_detailed_broad",
    "double_mismatch",
    "fully_matched",
    "graduate_high_skill",
    "tertiary_lower_skill"
  ),
  
  sample_variable = c(
    "horizontal_sample",
    "joint_sample",
    "joint_sample",
    "graduate_occupation_sample",
    "all_employed_occupation_sample"
  )
)


# ==============================================================================
# STEP 12: Estimate joint exposure model
# ==============================================================================

estimate_joint_model <- function(
    model_data,
    outcome_variable,
    subgroup
) {
  
  controls <- if (
    subgroup == "Pooled"
  ) {
    "female + urban"
  } else {
    "urban"
  }
  
  regression_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      "log_exposure_18_20_rebuilt + ",
      "log_exposure_24_26 + ",
      controls,
      " | historical_province + cohort + year"
    )
  )
  
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
# STEP 13: Helper for coefficient extraction
# ==============================================================================

extract_single_coefficient <- function(
    model,
    term
) {
  
  coefficient <- unname(
    coef(model)[term]
  )
  
  standard_error <- unname(
    se(model)[term]
  )
  
  p_value <- unname(
    pvalue(model)[term]
  )
  
  confidence_interval <- confint(
    model,
    parm = term,
    level = 0.95
  )
  
  conversion_factor <- 100 * log(1.10)
  
  data.table(
    coefficient_log_scale =
      coefficient,
    
    standard_error_log_scale =
      standard_error,
    
    effect_pp_for_10_percent_increase =
      conversion_factor * coefficient,
    
    standard_error_pp =
      conversion_factor * standard_error,
    
    lower_95_pp =
      conversion_factor *
      confidence_interval[1, 1],
    
    upper_95_pp =
      conversion_factor *
      confidence_interval[1, 2],
    
    p_value =
      p_value
  )
}


# ==============================================================================
# STEP 14: Helper for coefficient-difference test
#
# Tests:
#   H0: beta_18_20 - beta_24_26 = 0
# ==============================================================================

extract_difference_test <- function(model) {
  
  preferred_term <-
    "log_exposure_18_20_rebuilt"
  
  future_term <-
    "log_exposure_24_26"
  
  model_coefficients <- coef(model)
  model_vcov <- vcov(model)
  
  beta_difference <-
    unname(
      model_coefficients[preferred_term] -
        model_coefficients[future_term]
    )
  
  difference_variance <-
    model_vcov[
      preferred_term,
      preferred_term
    ] +
    model_vcov[
      future_term,
      future_term
    ] -
    2 * model_vcov[
      preferred_term,
      future_term
    ]
  
  difference_variance <- max(
    difference_variance,
    0
  )
  
  difference_se <-
    sqrt(difference_variance)
  
  difference_t <- if (
    difference_se > 0
  ) {
    beta_difference / difference_se
  } else {
    NA_real_
  }
  
  # Conservative degrees of freedom based on the smaller clustering dimension.
  degrees_freedom <- min(
    uniqueN(model$fixef_id$historical_province),
    uniqueN(model$fixef_id$cohort)
  ) - 1L
  
  difference_p <- if (
    !is.na(difference_t) &&
    degrees_freedom > 0
  ) {
    2 * pt(
      abs(difference_t),
      df = degrees_freedom,
      lower.tail = FALSE
    )
  } else {
    NA_real_
  }
  
  critical_value <- qt(
    0.975,
    df = degrees_freedom
  )
  
  conversion_factor <-
    100 * log(1.10)
  
  data.table(
    coefficient_difference_log_scale =
      beta_difference,
    
    difference_standard_error_log_scale =
      difference_se,
    
    difference_effect_pp =
      conversion_factor * beta_difference,
    
    difference_standard_error_pp =
      conversion_factor * difference_se,
    
    difference_lower_95_pp =
      conversion_factor *
      (
        beta_difference -
          critical_value * difference_se
      ),
    
    difference_upper_95_pp =
      conversion_factor *
      (
        beta_difference +
          critical_value * difference_se
      ),
    
    difference_t_statistic =
      difference_t,
    
    difference_p_value =
      difference_p,
    
    difference_degrees_freedom =
      degrees_freedom
  )
}


# ==============================================================================
# STEP 15: Estimate all outcomes and subgroups
# ==============================================================================

joint_results <- list()
failed_models <- list()

result_counter <- 1L
failure_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome <-
    outcome_definitions$outcome[outcome_index]
  
  current_outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_sample_variable <-
    outcome_definitions$sample_variable[outcome_index]
  
  for (
    current_subgroup in
    c(
      "Pooled",
      "Women",
      "Men"
    )
  ) {
    
    cat(
      "\nEstimating:",
      current_outcome,
      "|",
      current_subgroup,
      "\n"
    )
    
    model_data <- dt[
      get(current_sample_variable) == TRUE &
        !is.na(get(current_outcome_variable)) &
        !is.na(log_exposure_18_20_rebuilt) &
        !is.na(log_exposure_24_26)
    ]
    
    if (current_subgroup == "Women") {
      model_data <- model_data[
        gender == 1L
      ]
    }
    
    if (current_subgroup == "Men") {
      model_data <- model_data[
        gender == 2L
      ]
    }
    
    if (
      nrow(model_data) < 500L ||
      uniqueN(model_data$historical_province) < 10L ||
      uniqueN(model_data$cohort) < 8L
    ) {
      
      failed_models[[failure_counter]] <-
        data.table(
          outcome =
            current_outcome,
          
          subgroup =
            current_subgroup,
          
          reason =
            "Insufficient observations or identifying clusters",
          
          observations =
            nrow(model_data)
        )
      
      failure_counter <-
        failure_counter + 1L
      
      next
    }
    
    current_model <- tryCatch(
      estimate_joint_model(
        model_data =
          model_data,
        
        outcome_variable =
          current_outcome_variable,
        
        subgroup =
          current_subgroup
      ),
      error = function(e) e
    )
    
    if (inherits(current_model, "error")) {
      
      failed_models[[failure_counter]] <-
        data.table(
          outcome =
            current_outcome,
          
          subgroup =
            current_subgroup,
          
          reason =
            current_model$message,
          
          observations =
            nrow(model_data)
        )
      
      failure_counter <-
        failure_counter + 1L
      
      next
    }
    
    preferred_result <-
      extract_single_coefficient(
        model =
          current_model,
        
        term =
          "log_exposure_18_20_rebuilt"
      )
    
    future_result <-
      extract_single_coefficient(
        model =
          current_model,
        
        term =
          "log_exposure_24_26"
      )
    
    difference_result <-
      extract_difference_test(
        current_model
      )
    
    joint_results[[result_counter]] <-
      cbind(
        data.table(
          outcome =
            current_outcome,
          
          subgroup =
            current_subgroup,
          
          observations =
            nobs(current_model),
          
          provinces =
            uniqueN(
              model_data$historical_province
            ),
          
          cohorts =
            uniqueN(
              model_data$cohort
            ),
          
          minimum_cohort =
            min(model_data$cohort),
          
          maximum_cohort =
            max(model_data$cohort),
          
          survey_years =
            uniqueN(model_data$year)
        ),
        
        preferred_result[
          ,
          setnames(
            copy(.SD),
            names(.SD),
            paste0(
              "preferred_",
              names(.SD)
            )
          )
        ],
        
        future_result[
          ,
          setnames(
            copy(.SD),
            names(.SD),
            paste0(
              "future_",
              names(.SD)
            )
          )
        ],
        
        difference_result
      )
    
    result_counter <-
      result_counter + 1L
    
    rm(
      model_data,
      current_model
    )
    
    invisible(gc())
  }
}


# ==============================================================================
# STEP 16: Combine results
# ==============================================================================

joint_exposure_results <- rbindlist(
  joint_results,
  fill = TRUE
)

setorder(
  joint_exposure_results,
  outcome,
  subgroup
)


# ==============================================================================
# STEP 17: Add significance indicators
# ==============================================================================

joint_exposure_results[
  ,
  preferred_significance := fcase(
    preferred_p_value < 0.01, "***",
    preferred_p_value < 0.05, "**",
    preferred_p_value < 0.10, "*",
    default = ""
  )
]

joint_exposure_results[
  ,
  future_significance := fcase(
    future_p_value < 0.01, "***",
    future_p_value < 0.05, "**",
    future_p_value < 0.10, "*",
    default = ""
  )
]

joint_exposure_results[
  ,
  difference_significance := fcase(
    difference_p_value < 0.01, "***",
    difference_p_value < 0.05, "**",
    difference_p_value < 0.10, "*",
    default = ""
  )
]


# ==============================================================================
# STEP 18: Multiple-testing adjustments
#
# Adjust across the five outcomes within each subgroup.
# Separate adjustments are calculated for:
#   - preferred exposure coefficient
#   - future exposure coefficient
#   - preferred-minus-future difference test
# ==============================================================================

joint_exposure_results[
  ,
  preferred_p_value_fdr :=
    p.adjust(
      preferred_p_value,
      method = "BH"
    ),
  by = subgroup
]

joint_exposure_results[
  ,
  future_p_value_fdr :=
    p.adjust(
      future_p_value,
      method = "BH"
    ),
  by = subgroup
]

joint_exposure_results[
  ,
  difference_p_value_fdr :=
    p.adjust(
      difference_p_value,
      method = "BH"
    ),
  by = subgroup
]


# ==============================================================================
# STEP 19: Add interpretation indicators
# ==============================================================================

joint_exposure_results[
  ,
  preferred_remains_significant_10 :=
    preferred_p_value < 0.10
]

joint_exposure_results[
  ,
  future_remains_significant_10 :=
    future_p_value < 0.10
]

joint_exposure_results[
  ,
  preferred_differs_from_future_10 :=
    difference_p_value < 0.10
]

joint_exposure_results[
  ,
  preferred_larger_absolute_effect :=
    abs(
      preferred_effect_pp_for_10_percent_increase
    ) >
    abs(
      future_effect_pp_for_10_percent_increase
    )
]


# ==============================================================================
# STEP 20: Save complete results
# ==============================================================================

fwrite(
  joint_exposure_results,
  file.path(
    output_folder,
    "step20D_joint_exposure_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 21: Construct compact results
# ==============================================================================

compact_results <- joint_exposure_results[
  ,
  .(
    outcome,
    subgroup,
    
    preferred_effect =
      round(
        preferred_effect_pp_for_10_percent_increase,
        3
      ),
    
    preferred_se =
      round(
        preferred_standard_error_pp,
        3
      ),
    
    preferred_p =
      round(
        preferred_p_value,
        4
      ),
    
    preferred_fdr =
      round(
        preferred_p_value_fdr,
        4
      ),
    
    preferred_significance,
    
    future_effect =
      round(
        future_effect_pp_for_10_percent_increase,
        3
      ),
    
    future_se =
      round(
        future_standard_error_pp,
        3
      ),
    
    future_p =
      round(
        future_p_value,
        4
      ),
    
    future_fdr =
      round(
        future_p_value_fdr,
        4
      ),
    
    future_significance,
    
    preferred_minus_future =
      round(
        difference_effect_pp,
        3
      ),
    
    difference_se =
      round(
        difference_standard_error_pp,
        3
      ),
    
    difference_p =
      round(
        difference_p_value,
        4
      ),
    
    difference_fdr =
      round(
        difference_p_value_fdr,
        4
      ),
    
    difference_significance,
    
    preferred_remains_significant_10,
    future_remains_significant_10,
    preferred_differs_from_future_10,
    preferred_larger_absolute_effect,
    
    observations,
    provinces,
    cohorts,
    minimum_cohort,
    maximum_cohort,
    survey_years
  )
]

compact_results[
  ,
  preferred_entry := paste0(
    formatC(
      preferred_effect,
      format = "f",
      digits = 3
    ),
    preferred_significance,
    " (",
    formatC(
      preferred_se,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

compact_results[
  ,
  future_entry := paste0(
    formatC(
      future_effect,
      format = "f",
      digits = 3
    ),
    future_significance,
    " (",
    formatC(
      future_se,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

compact_results[
  ,
  difference_entry := paste0(
    formatC(
      preferred_minus_future,
      format = "f",
      digits = 3
    ),
    difference_significance,
    " (",
    formatC(
      difference_se,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

fwrite(
  compact_results,
  file.path(
    output_folder,
    "step20D_joint_exposure_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Pooled publication-style table
# ==============================================================================

pooled_table <- compact_results[
  subgroup == "Pooled",
  .(
    outcome,
    
    ages_18_20 =
      preferred_entry,
    
    ages_24_26 =
      future_entry,
    
    preferred_minus_future =
      difference_entry,
    
    preferred_p,
    future_p,
    difference_p,
    preferred_fdr,
    future_fdr,
    difference_fdr,
    observations
  )
]

fwrite(
  pooled_table,
  file.path(
    output_folder,
    "step20D_pooled_joint_exposure_table.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Gender comparison table
# ==============================================================================

gender_table_long <- compact_results[
  ,
  .(
    outcome,
    subgroup,
    preferred_entry,
    future_entry,
    difference_entry
  )
]

preferred_gender_table <- dcast(
  gender_table_long,
  outcome ~ subgroup,
  value.var = "preferred_entry"
)

setnames(
  preferred_gender_table,
  old = intersect(
    c(
      "Pooled",
      "Women",
      "Men"
    ),
    names(preferred_gender_table)
  ),
  new = paste0(
    intersect(
      c(
        "Pooled",
        "Women",
        "Men"
      ),
      names(preferred_gender_table)
    ),
    "_ages_18_20"
  )
)

future_gender_table <- dcast(
  gender_table_long,
  outcome ~ subgroup,
  value.var = "future_entry"
)

setnames(
  future_gender_table,
  old = intersect(
    c(
      "Pooled",
      "Women",
      "Men"
    ),
    names(future_gender_table)
  ),
  new = paste0(
    intersect(
      c(
        "Pooled",
        "Women",
        "Men"
      ),
      names(future_gender_table)
    ),
    "_ages_24_26"
  )
)

difference_gender_table <- dcast(
  gender_table_long,
  outcome ~ subgroup,
  value.var = "difference_entry"
)

setnames(
  difference_gender_table,
  old = intersect(
    c(
      "Pooled",
      "Women",
      "Men"
    ),
    names(difference_gender_table)
  ),
  new = paste0(
    intersect(
      c(
        "Pooled",
        "Women",
        "Men"
      ),
      names(difference_gender_table)
    ),
    "_difference"
  )
)

gender_joint_table <- Reduce(
  function(x, y) {
    merge(
      x,
      y,
      by = "outcome",
      all = TRUE
    )
  },
  list(
    preferred_gender_table,
    future_gender_table,
    difference_gender_table
  )
)

fwrite(
  gender_joint_table,
  file.path(
    output_folder,
    "step20D_gender_joint_exposure_table.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Summary of joint-model evidence
# ==============================================================================

joint_evidence_summary <- joint_exposure_results[
  ,
  .(
    outcomes = .N,
    
    preferred_p_below_010 =
      sum(
        preferred_p_value < 0.10,
        na.rm = TRUE
      ),
    
    preferred_fdr_below_010 =
      sum(
        preferred_p_value_fdr < 0.10,
        na.rm = TRUE
      ),
    
    future_p_below_010 =
      sum(
        future_p_value < 0.10,
        na.rm = TRUE
      ),
    
    future_fdr_below_010 =
      sum(
        future_p_value_fdr < 0.10,
        na.rm = TRUE
      ),
    
    difference_p_below_010 =
      sum(
        difference_p_value < 0.10,
        na.rm = TRUE
      ),
    
    difference_fdr_below_010 =
      sum(
        difference_p_value_fdr < 0.10,
        na.rm = TRUE
      ),
    
    preferred_larger_absolute =
      sum(
        preferred_larger_absolute_effect,
        na.rm = TRUE
      )
  ),
  by = subgroup
]

print(joint_evidence_summary)

fwrite(
  joint_evidence_summary,
  file.path(
    output_folder,
    "step20D_joint_evidence_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Save failed models
# ==============================================================================

if (length(failed_models) > 0L) {
  
  failed_model_table <- rbindlist(
    failed_models,
    fill = TRUE
  )
  
} else {
  
  failed_model_table <- data.table(
    outcome = character(),
    subgroup = character(),
    reason = character(),
    observations = integer()
  )
}

fwrite(
  failed_model_table,
  file.path(
    output_folder,
    "step20D_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Print central results
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 20D JOINT EXPOSURE RESULTS\n",
  "============================================================\n"
)

print(
  compact_results[
    ,
    .(
      outcome,
      subgroup,
      preferred_effect,
      preferred_se,
      preferred_p,
      future_effect,
      future_se,
      future_p,
      preferred_minus_future,
      difference_se,
      difference_p,
      observations
    )
  ]
)


# ==============================================================================
# STEP 27: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 20D COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step20D_treatment_validation.csv\n",
  "2. step20D_outcome_validation.csv\n",
  "3. step20D_joint_exposure_regression_results.csv\n",
  "4. step20D_joint_exposure_compact_results.csv\n",
  "5. step20D_pooled_joint_exposure_table.csv\n",
  "6. step20D_gender_joint_exposure_table.csv\n",
  "7. step20D_joint_evidence_summary.csv\n",
  "8. step20D_failed_models.csv\n"
)

cat(
  "\nInterpretation priorities:\n",
  "- Determine whether ages 18–20 exposure remains significant after controlling\n",
  "  for ages 24–26 exposure.\n",
  "- Determine whether the future exposure coefficient becomes small or insignificant.\n",
  "- Use the preferred-minus-future test to assess whether the coefficients differ.\n",
  "- Do not interpret insignificant coefficient differences as proof of equality.\n",
  "- Give greater weight to occupational outcomes, where Step 20C showed the\n",
  "  cleanest timing pattern.\n"
)