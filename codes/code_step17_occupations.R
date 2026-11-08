# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 17A: Identify Variables for Occupational-Destination Analysis
#
# Purpose:
#   Search the final analysis-ready dataset for variables related to:
#   - occupation and ISCO classification
#   - occupation skill level
#   - industry and sector
#   - public/private employment
#   - employee/self-employment status
#   - job type and employment arrangement
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
  "step17_occupational_destination"
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
  "\nTotal variables in final dataset:",
  length(all_variable_names),
  "\n"
)


# ==============================================================================
# STEP 3: Search for occupation-related variables
# ==============================================================================

occupation_variables <- all_variable_names[
  grepl(
    paste0(
      "occupation|occup|isco|job_title|jobtitle|",
      "professional|manager|technician|associate|",
      "clerical|service|sales|agric|craft|trade|",
      "operator|machine|elementary|skill"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 4: Search for industry and sector variables
# ==============================================================================

industry_sector_variables <- all_variable_names[
  grepl(
    paste0(
      "industry|sector|isic|economic_activity|activity_code|",
      "public|private|government|state|cooperative|",
      "manufactur|service_sector|agriculture_sector"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 5: Search for employment-status and job-type variables
# ==============================================================================

employment_type_variables <- all_variable_names[
  grepl(
    paste0(
      "employment_type|employment_status|employment_relation|",
      "employee|employer|self_employ|selfemploy|",
      "wage|salary|salaried|contract|permanent|temporary|",
      "formal|informal|main_job|second_job|last_job|",
      "work_status|job_status|occupation_status"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 6: Search for education and mismatch variables needed for the analysis
# ==============================================================================

education_mismatch_variables <- all_variable_names[
  grepl(
    paste0(
      "tertiary|education_field|isced|educ_level|",
      "horizontal|vertical|overeduc|undereduc|",
      "fully_matched|double_mismatch|both_refined"
    ),
    all_variable_names,
    ignore.case = TRUE
  )
]


# ==============================================================================
# STEP 7: Combine and remove duplicates
# ==============================================================================

all_candidate_variables <- unique(
  c(
    occupation_variables,
    industry_sector_variables,
    employment_type_variables,
    education_mismatch_variables
  )
)

all_candidate_variables <- sort(
  all_candidate_variables
)


# ==============================================================================
# STEP 8: Print candidate variables by category
# ==============================================================================

cat(
  "\n============================================================\n",
  "OCCUPATION-RELATED VARIABLES\n",
  "============================================================\n"
)

print(occupation_variables)

cat(
  "\n============================================================\n",
  "INDUSTRY AND SECTOR VARIABLES\n",
  "============================================================\n"
)

print(industry_sector_variables)

cat(
  "\n============================================================\n",
  "EMPLOYMENT-TYPE VARIABLES\n",
  "============================================================\n"
)

print(employment_type_variables)

cat(
  "\n============================================================\n",
  "EDUCATION AND MISMATCH VARIABLES\n",
  "============================================================\n"
)

print(education_mismatch_variables)

cat(
  "\n============================================================\n",
  "ALL UNIQUE CANDIDATE VARIABLES\n",
  "============================================================\n"
)

print(all_candidate_variables)


# ==============================================================================
# STEP 9: Save variable lists
# ==============================================================================

writeLines(
  occupation_variables,
  file.path(
    output_folder,
    "step17A_occupation_variables.txt"
  )
)

writeLines(
  industry_sector_variables,
  file.path(
    output_folder,
    "step17A_industry_sector_variables.txt"
  )
)

writeLines(
  employment_type_variables,
  file.path(
    output_folder,
    "step17A_employment_type_variables.txt"
  )
)

writeLines(
  education_mismatch_variables,
  file.path(
    output_folder,
    "step17A_education_mismatch_variables.txt"
  )
)

writeLines(
  all_candidate_variables,
  file.path(
    output_folder,
    "step17A_all_candidate_variables.txt"
  )
)


# ==============================================================================
# STEP 10: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 17A COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nCandidate-variable lists were saved in:\n",
  output_folder,
  "\n"
)


# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 17B: Inspect Occupation, Employment-Status, and Industry Codes
#
# Purpose:
#   Confirm the coding structure before constructing occupational destinations.
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
  "step17_occupational_destination"
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
# STEP 2: Read variables required for diagnostics
# ==============================================================================

variables_to_read <- c(
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  "employed",
  "tertiary_educated",
  
  # Occupation variables
  "isco_2digit",
  "isco_2digit_before_final_validation",
  "occupation_code_standardized",
  "occupation_code_digits",
  "occupation_code_text",
  "main_job_title_isco_code",
  
  # Employment-status variables
  "main_job_employment_status",
  "employment_status",
  
  # Industry and workplace variables
  "main_job_workplace_isic_code",
  "main_job_workplace_employee_coun",
  "preferred_work_sector",
  
  # Mismatch variables
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  "horizontal_mismatch_detailed_strict",
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final"
)

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


# ==============================================================================
# STEP 3: Construct basic analytical indicators
# ==============================================================================

dt[
  ,
  cohort := as.integer(
    analysis_birth_year
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
  sample_main :=
    complete_undergrad_exposure == 1L &
    !is.na(undergrad_exposure_18_20) &
    undergrad_exposure_18_20 > 0 &
    valid_weight &
    !is.na(cohort) &
    cohort >= 1355L &
    cohort <= 1383L &
    !is.na(province_historical) &
    !is.na(year) &
    !is.na(gender)
]

dt[
  ,
  graduate_employed :=
    sample_main == TRUE &
    tertiary_educated == 1L &
    employed == 1L
]


# ==============================================================================
# STEP 4: Inspect ISCO two-digit values
# ==============================================================================

isco_diagnostic <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    isco_2digit,
    isco_2digit_before_final_validation,
    occupation_code_digits,
    occupation_code_standardized
  )
]

setorder(
  isco_diagnostic,
  isco_2digit,
  occupation_code_digits
)

print(isco_diagnostic)

fwrite(
  isco_diagnostic,
  file.path(
    output_folder,
    "step17B_isco_code_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 5: Inspect one-digit ISCO major groups
#
# This is only a diagnostic. The final coding will be written after confirming
# the format of isco_2digit.
# ==============================================================================

dt[
  ,
  isco_2digit_text := as.character(
    isco_2digit
  )
]

dt[
  ,
  isco_2digit_numeric := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        isco_2digit_text
      )
    )
  )
]

dt[
  ,
  isco_major_group_candidate := fifelse(
    !is.na(isco_2digit_numeric),
    as.integer(
      floor(
        isco_2digit_numeric / 10
      )
    ),
    NA_integer_
  )
]

isco_major_group_diagnostic <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    minimum_isco2 = min(
      isco_2digit_numeric,
      na.rm = TRUE
    ),
    maximum_isco2 = max(
      isco_2digit_numeric,
      na.rm = TRUE
    )
  ),
  by = isco_major_group_candidate
]

setorder(
  isco_major_group_diagnostic,
  isco_major_group_candidate
)

print(isco_major_group_diagnostic)

fwrite(
  isco_major_group_diagnostic,
  file.path(
    output_folder,
    "step17B_isco_major_group_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 6: Inspect occupation-code text
# ==============================================================================

occupation_text_diagnostic <- dt[
  graduate_employed == TRUE &
    !is.na(occupation_code_text),
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    occupation_code_text,
    isco_2digit
  )
]

setorder(
  occupation_text_diagnostic,
  -observations
)

fwrite(
  occupation_text_diagnostic,
  file.path(
    output_folder,
    "step17B_occupation_text_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 7: Inspect employment-status coding
# ==============================================================================

main_job_status_diagnostic <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = main_job_employment_status
]

setorder(
  main_job_status_diagnostic,
  -observations
)

print(main_job_status_diagnostic)

fwrite(
  main_job_status_diagnostic,
  file.path(
    output_folder,
    "step17B_main_job_employment_status.csv"
  ),
  bom = TRUE
)


employment_status_diagnostic <- dt[
  sample_main == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = employment_status
]

setorder(
  employment_status_diagnostic,
  -observations
)

print(employment_status_diagnostic)

fwrite(
  employment_status_diagnostic,
  file.path(
    output_folder,
    "step17B_employment_status.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 8: Inspect industry coding
# ==============================================================================

industry_diagnostic <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = main_job_workplace_isic_code
]

setorder(
  industry_diagnostic,
  main_job_workplace_isic_code
)

print(industry_diagnostic)

fwrite(
  industry_diagnostic,
  file.path(
    output_folder,
    "step17B_industry_code_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 9: Inspect workplace-size coding
# ==============================================================================

workplace_size_diagnostic <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = main_job_workplace_employee_coun
]

setorder(
  workplace_size_diagnostic,
  -observations
)

print(workplace_size_diagnostic)

fwrite(
  workplace_size_diagnostic,
  file.path(
    output_folder,
    "step17B_workplace_size_diagnostic.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 10: Inspect preferred-work-sector coding
# ==============================================================================

preferred_sector_diagnostic <- dt[
  sample_main == TRUE,
  .(
    observations = .N,
    weighted_population = sum(
      weight,
      na.rm = TRUE
    )
  ),
  by = preferred_work_sector
]

setorder(
  preferred_sector_diagnostic,
  -observations
)

print(preferred_sector_diagnostic)

fwrite(
  preferred_sector_diagnostic,
  file.path(
    output_folder,
    "step17B_preferred_work_sector.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 11: Sample coverage by year
# ==============================================================================

occupation_year_coverage <- dt[
  graduate_employed == TRUE,
  .(
    observations = .N,
    
    valid_isco2 = sum(
      !is.na(isco_2digit)
    ),
    
    valid_isco2_share = mean(
      !is.na(isco_2digit)
    ),
    
    valid_main_job_status = sum(
      !is.na(main_job_employment_status)
    ),
    
    valid_main_job_status_share = mean(
      !is.na(main_job_employment_status)
    ),
    
    valid_industry = sum(
      !is.na(main_job_workplace_isic_code)
    ),
    
    valid_industry_share = mean(
      !is.na(main_job_workplace_isic_code)
    )
  ),
  by = year
]

setorder(
  occupation_year_coverage,
  year
)

print(occupation_year_coverage)

fwrite(
  occupation_year_coverage,
  file.path(
    output_folder,
    "step17B_variable_coverage_by_year.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 12: Summary validation
# ==============================================================================

validation_summary <- data.table(
  measure = c(
    "Graduate-employed observations",
    "Valid ISCO two-digit",
    "Valid main-job employment status",
    "Valid main-job industry",
    "Valid workplace size"
  ),
  
  observations = c(
    dt[
      graduate_employed == TRUE,
      .N
    ],
    
    dt[
      graduate_employed == TRUE &
        !is.na(isco_2digit),
      .N
    ],
    
    dt[
      graduate_employed == TRUE &
        !is.na(main_job_employment_status),
      .N
    ],
    
    dt[
      graduate_employed == TRUE &
        !is.na(main_job_workplace_isic_code),
      .N
    ],
    
    dt[
      graduate_employed == TRUE &
        !is.na(main_job_workplace_employee_coun),
      .N
    ]
  )
)

validation_summary[
  ,
  share_of_graduate_employed :=
    observations /
    observations[measure == "Graduate-employed observations"]
]

print(validation_summary)

fwrite(
  validation_summary,
  file.path(
    output_folder,
    "step17B_validation_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 13: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 17B COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nUpload these outputs:\n",
  "1. step17B_isco_code_diagnostic.csv\n",
  "2. step17B_isco_major_group_diagnostic.csv\n",
  "3. step17B_main_job_employment_status.csv\n",
  "4. step17B_industry_code_diagnostic.csv\n",
  "5. step17B_variable_coverage_by_year.csv\n",
  "6. step17B_validation_summary.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)










# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
# STEP 17: Occupational-Destination Analysis
#
# Main question:
#   Does university expansion redirect tertiary-educated workers away from
#   professional/high-skill occupations and toward lower-skill occupations?
#
# Sample:
#   Tertiary-educated employed workers with valid harmonized ISCO codes.
#
# Main treatment:
#   log(undergrad_exposure_18_20)
#
# Reported effect:
#   Percentage-point effect associated with a 10% increase in exposure.
#
# Fixed effects:
#   Historical province
#   Corrected birth cohort
#   Survey year
#
# Controls:
#   Pooled models: female and urban
#   Gender-specific models: urban
#
# Inference:
#   Survey-weighted linear probability models
#   Two-way clustering by historical province and corrected birth cohort
#
# Heterogeneity:
#   Pooled
#   Women
#   Men
#   Business, administration, and law
#   Engineering, manufacturing, and construction
#   ICT, reported cautiously because of shorter field coverage
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

library(data.table)
library(fst)
library(fixest)


# ==============================================================================
# STEP 1: Define project paths
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
  "step17_occupational_destination"
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
# STEP 2: Read required variables
# ==============================================================================

variables_to_read <- c(
  # Identifiers, controls, and weights
  "year",
  "weight",
  "gender",
  "analysis_birth_year",
  "province_historical",
  "urban_rural",
  
  # Treatment
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  
  # Employment and education
  "employed",
  "tertiary_educated",
  
  # Occupation
  "isco_2digit",
  "isco_2digit_before_final_validation",
  "occupation_code_standardized",
  "occupation_code_digits",
  
  # Field of study
  "isced2013_broad_code",
  "education_field_harmonized",
  
  # Main mismatch variables
  "horizontal_detailed_eligible",
  "horizontal_mismatch_detailed_broad",
  "vertical_fullperiod_eligible_final",
  "overeducated_final",
  "undereducated_final",
  "vertically_matched_final"
)

cat("\nReading Step 17 variables...\n")

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
# STEP 3: Construct analytical variables
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
  sex := fcase(
    gender == 1L, "Women",
    gender == 2L, "Men",
    default = NA_character_
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
    log(undergrad_exposure_18_20),
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
    dt[sample_main == TRUE, .N],
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 4: Clean the harmonized ISCO two-digit code
#
# The diagnostic revealed both:
#   - normal two-digit codes such as 11, 21, 31, ..., 96
#   - a limited number of single-digit codes such as 1
#
# Coding rule:
#   - two-digit values 10–99: major group is floor(code / 10)
#   - single-digit values 1–9: treat the value itself as the major group
#   - zero and invalid values are treated as missing
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

# Retain only standard civilian ISCO major groups 1–9.
dt[
  !isco_major_group %in% 1:9,
  isco_major_group := NA_integer_
]


# ==============================================================================
# STEP 5: Label major occupation groups
# ==============================================================================

dt[
  ,
  isco_major_label := fcase(
    isco_major_group == 1L,
    "1 Managers",
    
    isco_major_group == 2L,
    "2 Professionals",
    
    isco_major_group == 3L,
    "3 Technicians and associate professionals",
    
    isco_major_group == 4L,
    "4 Clerical support workers",
    
    isco_major_group == 5L,
    "5 Service and sales workers",
    
    isco_major_group == 6L,
    "6 Skilled agricultural workers",
    
    isco_major_group == 7L,
    "7 Craft and related trades workers",
    
    isco_major_group == 8L,
    "8 Plant and machine operators",
    
    isco_major_group == 9L,
    "9 Elementary occupations",
    
    default = NA_character_
  )
]


# ==============================================================================
# STEP 6: Define graduate-employed occupation sample
# ==============================================================================

dt[
  ,
  occupation_sample :=
    sample_main == TRUE &
    employed == 1L &
    tertiary_educated == 1L &
    !is.na(isco_major_group)
]

occupation_sample_validation <- dt[
  ,
  .(
    graduate_employed =
      sum(
        sample_main == TRUE &
          employed == 1L &
          tertiary_educated == 1L,
        na.rm = TRUE
      ),
    
    valid_occupation_sample =
      sum(
        occupation_sample,
        na.rm = TRUE
      ),
    
    occupation_coverage =
      sum(
        occupation_sample,
        na.rm = TRUE
      ) /
      sum(
        sample_main == TRUE &
          employed == 1L &
          tertiary_educated == 1L,
        na.rm = TRUE
      ),
    
    historical_provinces =
      uniqueN(
        historical_province[
          occupation_sample == TRUE
        ]
      ),
    
    cohorts =
      uniqueN(
        cohort[
          occupation_sample == TRUE
        ]
      ),
    
    survey_years =
      uniqueN(
        year[
          occupation_sample == TRUE
        ]
      )
  )
]

print(occupation_sample_validation)

fwrite(
  occupation_sample_validation,
  file.path(
    output_folder,
    "step17_occupation_sample_validation.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 7: Construct occupational-destination outcomes
#
# High-skill definition:
#   ISCO major groups 1–3:
#   managers, professionals, and technicians/associate professionals.
#
# Lower-skill definition:
#   ISCO major groups 4–9.
# ==============================================================================

dt[
  ,
  occupation_manager := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 1L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_professional := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 2L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_technician := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 3L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_high_skill := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group %in% 1:3
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_clerical := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 4L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_service_sales := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 5L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_skilled_agriculture := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 6L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_craft_operator := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group %in% c(
        7L,
        8L
      )
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_elementary := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group == 9L
    ),
    NA_integer_
  )
]

dt[
  ,
  occupation_lower_skill := fifelse(
    occupation_sample,
    as.integer(
      isco_major_group %in% 4:9
    ),
    NA_integer_
  )
]


# ==============================================================================
# STEP 8: Validate occupation outcomes
# ==============================================================================

occupation_outcome_validation <- dt[
  occupation_sample == TRUE,
  .(
    observations = .N,
    
    invalid_high_low_sum =
      sum(
        occupation_high_skill +
          occupation_lower_skill != 1L,
        na.rm = TRUE
      ),
    
    invalid_major_group_sum =
      sum(
        occupation_manager +
          occupation_professional +
          occupation_technician +
          occupation_clerical +
          occupation_service_sales +
          occupation_skilled_agriculture +
          occupation_craft_operator +
          occupation_elementary != 1L,
        na.rm = TRUE
      )
  )
]

print(occupation_outcome_validation)

fwrite(
  occupation_outcome_validation,
  file.path(
    output_folder,
    "step17_occupation_outcome_validation.csv"
  ),
  bom = TRUE
)

if (
  occupation_outcome_validation$invalid_high_low_sum > 0L ||
  occupation_outcome_validation$invalid_major_group_sum > 0L
) {
  stop(
    "Occupational-destination categories are not internally exhaustive."
  )
}


# ==============================================================================
# STEP 9: Reconstruct broad field groups
# ==============================================================================

dt[
  ,
  isced_broad_code_clean := suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        as.character(isced2013_broad_code)
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
# STEP 10: Fallback field classification using harmonized labels
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


# ==============================================================================
# STEP 11: Define outcome list
# ==============================================================================

outcome_definitions <- data.table(
  outcome = c(
    "Managers",
    "Professionals",
    "Technicians and associate professionals",
    "High-skill occupations",
    "Clerical support occupations",
    "Service and sales occupations",
    "Skilled agricultural occupations",
    "Craft and machine-operation occupations",
    "Elementary occupations",
    "Lower-skill occupations"
  ),
  
  outcome_variable = c(
    "occupation_manager",
    "occupation_professional",
    "occupation_technician",
    "occupation_high_skill",
    "occupation_clerical",
    "occupation_service_sales",
    "occupation_skilled_agriculture",
    "occupation_craft_operator",
    "occupation_elementary",
    "occupation_lower_skill"
  ),
  
  outcome_family = c(
    "Detailed",
    "Primary",
    "Detailed",
    "Primary",
    "Detailed",
    "Detailed",
    "Detailed",
    "Detailed",
    "Detailed",
    "Primary"
  )
)


# ==============================================================================
# STEP 12: Define analysis groups
# ==============================================================================

group_definitions <- data.table(
  analysis_group = c(
    "Pooled",
    "Women",
    "Men",
    "Business and law",
    "Engineering",
    "ICT"
  ),
  
  gender_restriction = c(
    NA_integer_,
    1L,
    2L,
    NA_integer_,
    NA_integer_,
    NA_integer_
  ),
  
  field_restriction = c(
    NA_character_,
    NA_character_,
    NA_character_,
    "04 Business, administration and law",
    "07 Engineering, manufacturing and construction",
    "06 Information and communication technologies"
  )
)


# ==============================================================================
# STEP 13: Weighted-mean helper
# ==============================================================================

safe_weighted_mean <- function(
    outcome,
    weights
) {
  
  valid <- (
    !is.na(outcome) &
      !is.na(weights) &
      is.finite(weights) &
      weights > 0
  )
  
  if (!any(valid)) {
    return(NA_real_)
  }
  
  weighted.mean(
    outcome[valid],
    weights[valid]
  )
}


# ==============================================================================
# STEP 14: Descriptive occupation shares
# ==============================================================================

descriptive_results <- list()
descriptive_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  current_outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  for (
    group_index in
    seq_len(nrow(group_definitions))
  ) {
    
    current_group <-
      group_definitions$analysis_group[group_index]
    
    current_gender <-
      group_definitions$gender_restriction[group_index]
    
    current_field <-
      group_definitions$field_restriction[group_index]
    
    current_data <- dt[
      occupation_sample == TRUE &
        !is.na(get(current_outcome_variable))
    ]
    
    if (!is.na(current_gender)) {
      current_data <- current_data[
        gender == current_gender
      ]
    }
    
    if (!is.na(current_field)) {
      current_data <- current_data[
        field_group == current_field
      ]
    }
    
    descriptive_results[[descriptive_counter]] <-
      data.table(
        outcome =
          current_outcome_name,
        
        outcome_variable =
          current_outcome_variable,
        
        analysis_group =
          current_group,
        
        weighted_rate =
          safe_weighted_mean(
            current_data[[current_outcome_variable]],
            current_data$weight
          ),
        
        weighted_rate_percent =
          100 *
          safe_weighted_mean(
            current_data[[current_outcome_variable]],
            current_data$weight
          ),
        
        observations =
          nrow(current_data),
        
        historical_provinces =
          uniqueN(
            current_data$historical_province
          ),
        
        cohorts =
          uniqueN(
            current_data$cohort
          ),
        
        survey_years =
          uniqueN(
            current_data$year
          )
      )
    
    descriptive_counter <-
      descriptive_counter + 1L
  }
}

occupation_descriptives <- rbindlist(
  descriptive_results,
  fill = TRUE
)

setorder(
  occupation_descriptives,
  outcome,
  analysis_group
)

fwrite(
  occupation_descriptives,
  file.path(
    output_folder,
    "step17_occupation_descriptive_rates.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 15: Model-estimation helper
# ==============================================================================

estimate_occupation_model <- function(
    model_data,
    outcome_variable,
    analysis_group
) {
  
  if (
    analysis_group %in% c(
      "Pooled",
      "Business and law",
      "Engineering",
      "ICT"
    )
  ) {
    
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
# STEP 16: Result-extraction helper
# ==============================================================================

extract_occupation_result <- function(
    model,
    outcome_name,
    outcome_family,
    analysis_group,
    model_data
) {
  
  treatment_term <-
    "log_undergrad_exposure"
  
  coefficient <-
    unname(
      coef(model)[treatment_term]
    )
  
  standard_error <-
    unname(
      se(model)[treatment_term]
    )
  
  p_value <-
    unname(
      pvalue(model)[treatment_term]
    )
  
  confidence_interval <- confint(
    model,
    parm = treatment_term,
    level = 0.95
  )
  
  conversion_factor <-
    100 * log(1.10)
  
  data.table(
    outcome =
      outcome_name,
    
    outcome_family =
      outcome_family,
    
    analysis_group =
      analysis_group,
    
    coefficient_log_scale =
      coefficient,
    
    standard_error_log_scale =
      standard_error,
    
    effect_pp_for_10_percent_increase =
      conversion_factor *
      coefficient,
    
    standard_error_pp =
      conversion_factor *
      standard_error,
    
    lower_95_pp =
      conversion_factor *
      confidence_interval[1, 1],
    
    upper_95_pp =
      conversion_factor *
      confidence_interval[1, 2],
    
    p_value =
      p_value,
    
    significance = fcase(
      p_value < 0.01, "***",
      p_value < 0.05, "**",
      p_value < 0.10, "*",
      default = ""
    ),
    
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
# STEP 17: Estimate occupational-destination models
# ==============================================================================

regression_results <- list()
failed_models <- list()

result_counter <- 1L
failure_counter <- 1L

for (
  outcome_index in
  seq_len(nrow(outcome_definitions))
) {
  
  current_outcome_name <-
    outcome_definitions$outcome[outcome_index]
  
  current_outcome_variable <-
    outcome_definitions$outcome_variable[outcome_index]
  
  current_outcome_family <-
    outcome_definitions$outcome_family[outcome_index]
  
  for (
    group_index in
    seq_len(nrow(group_definitions))
  ) {
    
    current_group <-
      group_definitions$analysis_group[group_index]
    
    current_gender <-
      group_definitions$gender_restriction[group_index]
    
    current_field <-
      group_definitions$field_restriction[group_index]
    
    cat(
      "\nEstimating:",
      current_outcome_name,
      "|",
      current_group,
      "\n"
    )
    
    model_data <- dt[
      occupation_sample == TRUE &
        !is.na(get(current_outcome_variable))
    ]
    
    if (!is.na(current_gender)) {
      model_data <- model_data[
        gender == current_gender
      ]
    }
    
    if (!is.na(current_field)) {
      model_data <- model_data[
        field_group == current_field
      ]
    }
    
    if (
      nrow(model_data) < 500L ||
      uniqueN(model_data$historical_province) < 5L ||
      uniqueN(model_data$cohort) < 5L ||
      uniqueN(model_data$year) < 3L
    ) {
      
      failed_models[[failure_counter]] <-
        data.table(
          outcome =
            current_outcome_name,
          
          analysis_group =
            current_group,
          
          reason =
            "Insufficient observations or identifying variation",
          
          observations =
            nrow(model_data),
          
          historical_provinces =
            uniqueN(
              model_data$historical_province
            ),
          
          cohorts =
            uniqueN(
              model_data$cohort
            ),
          
          survey_years =
            uniqueN(
              model_data$year
            )
        )
      
      failure_counter <-
        failure_counter + 1L
      
      next
    }
    
    current_model <- tryCatch(
      estimate_occupation_model(
        model_data =
          model_data,
        
        outcome_variable =
          current_outcome_variable,
        
        analysis_group =
          current_group
      ),
      error = function(e) e
    )
    
    if (inherits(current_model, "error")) {
      
      failed_models[[failure_counter]] <-
        data.table(
          outcome =
            current_outcome_name,
          
          analysis_group =
            current_group,
          
          reason =
            current_model$message,
          
          observations =
            nrow(model_data),
          
          historical_provinces =
            uniqueN(
              model_data$historical_province
            ),
          
          cohorts =
            uniqueN(
              model_data$cohort
            ),
          
          survey_years =
            uniqueN(
              model_data$year
            )
        )
      
      failure_counter <-
        failure_counter + 1L
      
      next
    }
    
    regression_results[[result_counter]] <-
      extract_occupation_result(
        model =
          current_model,
        
        outcome_name =
          current_outcome_name,
        
        outcome_family =
          current_outcome_family,
        
        analysis_group =
          current_group,
        
        model_data =
          model_data
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
# STEP 18: Combine regression results
# ==============================================================================

occupation_regression_results <- rbindlist(
  regression_results,
  fill = TRUE
)

setorder(
  occupation_regression_results,
  outcome,
  analysis_group
)


# ==============================================================================
# STEP 19: Add false-discovery-rate adjustments
#
# Adjustments are calculated within each analysis group across:
#   1. Primary outcomes only
#   2. All ten occupation outcomes
# ==============================================================================

occupation_regression_results[
  ,
  p_value_fdr_all_outcomes :=
    p.adjust(
      p_value,
      method = "BH"
    ),
  by = analysis_group
]

occupation_regression_results[
  ,
  p_value_fdr_primary :=
    NA_real_
]

for (
  current_group in
  unique(
    occupation_regression_results$analysis_group
  )
) {
  
  selected_rows <- which(
    occupation_regression_results$analysis_group ==
      current_group &
      occupation_regression_results$outcome_family ==
      "Primary"
  )
  
  occupation_regression_results[
    selected_rows,
    p_value_fdr_primary :=
      p.adjust(
        p_value,
        method = "BH"
      )
  ]
}

occupation_regression_results[
  ,
  significant_fdr_primary_10 :=
    !is.na(p_value_fdr_primary) &
    p_value_fdr_primary < 0.10
]

occupation_regression_results[
  ,
  significant_fdr_all_10 :=
    !is.na(p_value_fdr_all_outcomes) &
    p_value_fdr_all_outcomes < 0.10
]


# ==============================================================================
# STEP 20: Reliability indicators
# ==============================================================================

occupation_regression_results[
  ,
  minimum_required_observations := fcase(
    analysis_group == "Pooled",
    5000L,
    
    analysis_group %in% c(
      "Women",
      "Men"
    ),
    2000L,
    
    default =
      3000L
  )
]

occupation_regression_results[
  ,
  reliable_sample :=
    observations >= minimum_required_observations &
    historical_province_clusters >= 15L &
    cohort_clusters >= 15L &
    survey_years >= 5L
]


# ==============================================================================
# STEP 21: Save complete regression results
# ==============================================================================

fwrite(
  occupation_regression_results,
  file.path(
    output_folder,
    "step17_occupation_regression_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 22: Compact publication-style results
# ==============================================================================

compact_results <- occupation_regression_results[
  ,
  .(
    outcome,
    outcome_family,
    analysis_group,
    
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
    
    p_value_fdr_primary =
      round(
        p_value_fdr_primary,
        4
      ),
    
    p_value_fdr_all =
      round(
        p_value_fdr_all_outcomes,
        4
      ),
    
    significance,
    observations,
    historical_province_clusters,
    cohort_clusters,
    survey_years,
    reliable_sample
  )
]

compact_results[
  ,
  estimate_with_stars :=
    paste0(
      formatC(
        estimate,
        format = "f",
        digits = 3
      ),
      significance
    )
]

compact_results[
  ,
  standard_error_parentheses :=
    paste0(
      "(",
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
    "step17_occupation_compact_results.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 23: Main pooled and gender table
# ==============================================================================

main_results <- compact_results[
  analysis_group %in% c(
    "Pooled",
    "Women",
    "Men"
  ) &
    outcome_family == "Primary" &
    reliable_sample == TRUE,
  .(
    outcome,
    analysis_group,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    p_value_fdr_primary,
    observations
  )
]

main_results[
  ,
  table_entry := paste0(
    estimate_with_stars,
    " ",
    standard_error_parentheses
  )
]

main_table_wide <- dcast(
  main_results,
  outcome ~ analysis_group,
  value.var = "table_entry"
)

fwrite(
  main_table_wide,
  file.path(
    output_folder,
    "step17_main_occupation_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 24: Field-mechanism table
# ==============================================================================

field_mechanism_results <- compact_results[
  analysis_group %in% c(
    "Business and law",
    "Engineering",
    "ICT"
  ) &
    outcome_family == "Primary" &
    reliable_sample == TRUE,
  .(
    outcome,
    analysis_group,
    estimate_with_stars,
    standard_error_parentheses,
    p_value,
    p_value_fdr_primary,
    observations,
    survey_years
  )
]

field_mechanism_results[
  ,
  table_entry := paste0(
    estimate_with_stars,
    " ",
    standard_error_parentheses
  )
]

field_mechanism_table_wide <- dcast(
  field_mechanism_results,
  outcome ~ analysis_group,
  value.var = "table_entry"
)

fwrite(
  field_mechanism_table_wide,
  file.path(
    output_folder,
    "step17_field_occupation_mechanism_table_wide.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 25: Results surviving FDR correction
# ==============================================================================

results_surviving_fdr <- occupation_regression_results[
  reliable_sample == TRUE &
    (
      significant_fdr_primary_10 == TRUE |
        significant_fdr_all_10 == TRUE
    )
][
  order(
    analysis_group,
    p_value_fdr_all_outcomes
  )
]

fwrite(
  results_surviving_fdr,
  file.path(
    output_folder,
    "step17_results_surviving_FDR.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 26: Save failed-model log
# ==============================================================================

if (length(failed_models) > 0L) {
  
  failed_model_table <- rbindlist(
    failed_models,
    fill = TRUE
  )
  
} else {
  
  failed_model_table <- data.table(
    outcome = character(),
    analysis_group = character(),
    reason = character(),
    observations = integer(),
    historical_provinces = integer(),
    cohorts = integer(),
    survey_years = integer()
  )
}

fwrite(
  failed_model_table,
  file.path(
    output_folder,
    "step17_failed_models.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 27: Diagnostic summary
# ==============================================================================

diagnostic_summary <- occupation_regression_results[
  ,
  .(
    models_estimated = .N,
    
    reliable_models =
      sum(
        reliable_sample,
        na.rm = TRUE
      ),
    
    conventional_p_below_010 =
      sum(
        p_value < 0.10,
        na.rm = TRUE
      ),
    
    conventional_p_below_005 =
      sum(
        p_value < 0.05,
        na.rm = TRUE
      ),
    
    primary_FDR_below_010 =
      sum(
        p_value_fdr_primary < 0.10,
        na.rm = TRUE
      ),
    
    all_outcome_FDR_below_010 =
      sum(
        p_value_fdr_all_outcomes < 0.10,
        na.rm = TRUE
      )
  ),
  by = analysis_group
]

print(diagnostic_summary)

fwrite(
  diagnostic_summary,
  file.path(
    output_folder,
    "step17_diagnostic_summary.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 28: Print primary results
# ==============================================================================

cat(
  "\n============================================================\n",
  "PRIMARY OCCUPATIONAL-DESTINATION RESULTS\n",
  "============================================================\n"
)

print(
  occupation_regression_results[
    outcome_family == "Primary",
    .(
      outcome,
      analysis_group,
      
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
      
      FDR_primary =
        round(
          p_value_fdr_primary,
          4
        ),
      
      observations,
      survey_years,
      reliable_sample
    )
  ]
)


# ==============================================================================
# STEP 29: Completion message
# ==============================================================================

cat(
  "\n============================================================\n",
  "STEP 17 COMPLETED\n",
  "============================================================\n"
)

cat(
  "\nMain outputs:\n",
  "1. step17_occupation_sample_validation.csv\n",
  "2. step17_occupation_outcome_validation.csv\n",
  "3. step17_occupation_descriptive_rates.csv\n",
  "4. step17_occupation_regression_results.csv\n",
  "5. step17_occupation_compact_results.csv\n",
  "6. step17_main_occupation_table_wide.csv\n",
  "7. step17_field_occupation_mechanism_table_wide.csv\n",
  "8. step17_results_surviving_FDR.csv\n",
  "9. step17_failed_models.csv\n",
  "10. step17_diagnostic_summary.csv\n"
)

cat(
  "\nAll files were saved in:\n",
  output_folder,
  "\n"
)

cat(
  "\nInterpretation priorities:\n",
  "- Focus first on professionals, high-skill, and lower-skill occupations.\n",
  "- Examine whether effects are concentrated among men.\n",
  "- Use business and law as the main field-mechanism analysis.\n",
  "- Treat ICT estimates cautiously because of shorter field coverage.\n",
  "- Use detailed occupation categories to explain the aggregate shift.\n"
)