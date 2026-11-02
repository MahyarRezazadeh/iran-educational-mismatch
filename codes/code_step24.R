# ==============================================================================
# PROJECT: University Expansion and Educational Mismatch in Iran
#
# PURPOSE:
#   Identify the two-digit occupations in which the exposure-induced increase
#   in double mismatch is concentrated, separately for women and men.
#
# MAIN OUTCOME FOR OCCUPATION j:
#   1 if the worker is:
#       - overeducated under the preferred vertical benchmark,
#       - horizontally mismatched under the preferred broad crosswalk, and
#       - employed in two-digit ISCO occupation j;
#   0 otherwise within the common joint-mismatch sample.
#
# IMPORTANT:
#   All occupation-specific outcomes are estimated on exactly the same sample.
#   Therefore, their coefficients should sum to the coefficient from the
#   corresponding total double-mismatch regression.
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
library(ggplot2)


# ==============================================================================
# STEP 1: Define file paths
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
  "step24_occupation_specific_double_mismatch"
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
# STEP 2: Define and verify required variables
# ==============================================================================

required_variables <- c(
  "isco_2digit",
  "tertiary_educated",
  "employed",
  "overeducated_final",
  "horizontal_mismatch_detailed_broad",
  "log_undergrad_exposure_18_20",
  "province_historical",
  "analysis_birth_year",
  "year",
  "gender",
  "urban_rural",
  "weight"
)

available_variables <- metadata_fst(data_file)$columnNames

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
# STEP 3: Read required variables
# ==============================================================================

dt <- as.data.table(
  read_fst(
    data_file,
    columns = required_variables
  )
)

cat(
  "\nRows read:",
  format(nrow(dt), big.mark = ","),
  "\n"
)


# ==============================================================================
# STEP 4: Construct gender and urban indicators
# ==============================================================================

# Confirmed coding:
# gender = 1: women
# gender = 2: men

dt[
  ,
  female := fcase(
    gender == 1, 1L,
    gender == 2, 0L,
    default = NA_integer_
  )
]

dt[
  ,
  urban := fcase(
    tolower(trimws(as.character(urban_rural))) == "urban", 1L,
    tolower(trimws(as.character(urban_rural))) == "rural", 0L,
    default = NA_integer_
  )
]

cat("\nGender mapping:\n")

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

cat("\nUrban mapping:\n")

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
# STEP 5: Clean two-digit ISCO codes while preserving missing values
# ==============================================================================

dt[
  ,
  isco_numeric :=
    suppressWarnings(
      as.integer(
        trimws(
          as.character(isco_2digit)
        )
      )
    )
]

dt[
  ,
  isco_2digit_clean := fifelse(
    is.na(isco_numeric),
    NA_character_,
    sprintf(
      "%02d",
      isco_numeric
    )
  )
]

dt[
  ,
  isco_numeric := NULL
]

cat(
  "\nMissing cleaned occupation codes:",
  dt[is.na(isco_2digit_clean), .N],
  "\n"
)


# ==============================================================================
# STEP 6: ISCO-08 two-digit occupation labels
# ==============================================================================

isco_crosswalk <- data.table(
  isco_2digit_clean = c(
    "00", "01",
    "11", "12", "13", "14",
    "21", "22", "23", "24", "25", "26",
    "31", "32", "33", "34", "35",
    "41", "42", "43", "44",
    "51", "52", "53", "54",
    "61", "62", "63",
    "71", "72", "73", "74", "75",
    "81", "82", "83",
    "91", "92", "93", "94", "95", "96"
  ),
  
  occupation_label = c(
    "Armed-forces occupations, unspecified",
    "Commissioned and non-commissioned armed-forces occupations",
    
    "Chief executives, senior officials, and legislators",
    "Administrative and commercial managers",
    "Production and specialized-services managers",
    "Hospitality, retail, and other-services managers",
    
    "Science and engineering professionals",
    "Health professionals",
    "Teaching professionals",
    "Business and administration professionals",
    "ICT professionals",
    "Legal, social, and cultural professionals",
    
    "Science and engineering associate professionals",
    "Health associate professionals",
    "Business and administration associate professionals",
    "Legal, social, cultural, and related associate professionals",
    "Information and communications technicians",
    
    "General and keyboard clerks",
    "Customer-services clerks",
    "Numerical and material-recording clerks",
    "Other clerical-support workers",
    
    "Personal-service workers",
    "Sales workers",
    "Personal-care workers",
    "Protective-services workers",
    
    "Market-oriented skilled agricultural workers",
    "Skilled forestry, fishery, and hunting workers",
    "Subsistence farmers, fishers, hunters, and gatherers",
    
    "Building and related-trades workers",
    "Metal, machinery, and related-trades workers",
    "Handicraft and printing workers",
    "Electrical and electronic-trades workers",
    "Food-processing, woodworking, garment, and related workers",
    
    "Stationary plant and machine operators",
    "Assemblers",
    "Drivers and mobile-plant operators",
    
    "Cleaners and helpers",
    "Agricultural, forestry, and fishery laborers",
    "Laborers in mining, construction, manufacturing, and transport",
    "Food-preparation assistants",
    "Street and related sales and service workers",
    "Refuse workers and other elementary workers"
  )
)


# ==============================================================================
# STEP 7: Construct the common joint-mismatch analysis sample
# ==============================================================================

# Do not restrict the sample separately for each occupation.
# Every occupation-specific regression must use the same observations.

analysis_dt <- dt[
  tertiary_educated == 1 &
    employed == 1 &
    !is.na(overeducated_final) &
    !is.na(horizontal_mismatch_detailed_broad) &
    !is.na(isco_2digit_clean) &
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

analysis_dt[
  ,
  double_mismatch :=
    as.integer(
      overeducated_final == 1 &
        horizontal_mismatch_detailed_broad == 1
    )
]

cat(
  "\n============================================================\n",
  "COMMON JOINT-MISMATCH SAMPLE\n",
  "============================================================\n"
)

cat(
  "\nObservations:",
  format(nrow(analysis_dt), big.mark = ","),
  "\n"
)

cat(
  "Women:",
  format(analysis_dt[female == 1, .N], big.mark = ","),
  "\n"
)

cat(
  "Men:",
  format(analysis_dt[female == 0, .N], big.mark = ","),
  "\n"
)

cat(
  "Double-mismatched observations:",
  format(analysis_dt[double_mismatch == 1, .N], big.mark = ","),
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

# The validated joint sample was previously approximately 392,777.
# A small difference should be investigated before interpretation.

if (
  abs(
    nrow(analysis_dt) -
    392777
  ) > 5000
) {
  warning(
    paste0(
      "The joint sample differs materially from 392,777. ",
      "Check the sample construction before interpreting results."
    )
  )
}


# ==============================================================================
# STEP 8: Check that occupation categories are exhaustive
# ==============================================================================

unmatched_codes <- setdiff(
  sort(unique(analysis_dt$isco_2digit_clean)),
  isco_crosswalk$isco_2digit_clean
)

if (length(unmatched_codes) > 0L) {
  stop(
    paste0(
      "These occupation codes are missing from the crosswalk:\n",
      paste(
        unmatched_codes,
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# STEP 9: Descriptive distribution of double mismatch by occupation and gender
# ==============================================================================

double_mismatch_descriptives <- analysis_dt[
  double_mismatch == 1,
  .(
    double_mismatch_observations = .N,
    weighted_double_mismatch =
      sum(weight)
  ),
  by = .(
    isco_2digit_clean,
    subgroup = fifelse(
      female == 1,
      "Women",
      "Men"
    )
  )
]

double_mismatch_descriptives[
  ,
  weighted_share_within_gender :=
    weighted_double_mismatch /
    sum(weighted_double_mismatch),
  by = subgroup
]

double_mismatch_descriptives <- merge(
  double_mismatch_descriptives,
  isco_crosswalk,
  by = "isco_2digit_clean",
  all.x = TRUE
)

setorder(
  double_mismatch_descriptives,
  subgroup,
  -weighted_share_within_gender
)

cat(
  "\n============================================================\n",
  "DESCRIPTIVE DISTRIBUTION OF DOUBLE MISMATCH\n",
  "============================================================\n"
)

print(
  double_mismatch_descriptives,
  nrows = Inf
)


# ==============================================================================
# STEP 10: Helper functions
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


extract_treatment_result <- function(
    model,
    outcome_name,
    occupation_code,
    subgroup
) {
  
  treatment_name <- "log_undergrad_exposure_18_20"
  
  beta <- unname(
    coef(model)[treatment_name]
  )
  
  standard_error <- unname(
    se(model)[treatment_name]
  )
  
  p_value <- unname(
    pvalue(model)[treatment_name]
  )
  
  effect_pp <-
    conversion_factor *
    beta
  
  standard_error_pp <-
    conversion_factor *
    standard_error
  
  data.table(
    outcome = outcome_name,
    isco_2digit_clean = occupation_code,
    subgroup = subgroup,
    coefficient_log_exposure = beta,
    standard_error_log_exposure =
      standard_error,
    p_value = p_value,
    effect_pp_10_percent = effect_pp,
    standard_error_pp_10_percent =
      standard_error_pp,
    lower_95 =
      effect_pp -
      1.96 * standard_error_pp,
    upper_95 =
      effect_pp +
      1.96 * standard_error_pp,
    significance =
      significance_stars(p_value),
    model_observations =
      nobs(model)
  )
}


# ==============================================================================
# STEP 11: Estimate the total double-mismatch models
# ==============================================================================

estimate_total_double_mismatch <- function(
    data,
    subgroup
) {
  
  if (subgroup == "Pooled") {
    
    estimation_data <- data
    
    model_formula <- (
      double_mismatch ~
        log_undergrad_exposure_18_20 +
        female +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Women") {
    
    estimation_data <- data[
      female == 1
    ]
    
    model_formula <- (
      double_mismatch ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Men") {
    
    estimation_data <- data[
      female == 0
    ]
    
    model_formula <- (
      double_mismatch ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else {
    
    stop(
      paste0(
        "Unknown subgroup: ",
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
    notes = FALSE
  )
  
  extract_treatment_result(
    model = model,
    outcome_name = "Total double mismatch",
    occupation_code = NA_character_,
    subgroup = subgroup
  )
}


subgroups <- c(
  "Pooled",
  "Women",
  "Men"
)

total_double_results <- rbindlist(
  lapply(
    subgroups,
    function(current_subgroup) {
      
      estimate_total_double_mismatch(
        data = analysis_dt,
        subgroup = current_subgroup
      )
    }
  )
)

cat(
  "\n============================================================\n",
  "TOTAL DOUBLE-MISMATCH RESULTS\n",
  "============================================================\n"
)

print(total_double_results)


# ==============================================================================
# STEP 12: Estimate occupation-specific double-mismatch models
# ==============================================================================

estimate_occupation_specific_double_mismatch <- function(
    data,
    occupation_code,
    subgroup
) {
  
  estimation_data <- copy(data)
  
  estimation_data[
    ,
    occupation_specific_double :=
      as.integer(
        double_mismatch == 1 &
          isco_2digit_clean ==
          occupation_code
      )
  ]
  
  if (subgroup == "Pooled") {
    
    model_formula <- (
      occupation_specific_double ~
        log_undergrad_exposure_18_20 +
        female +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Women") {
    
    estimation_data <- estimation_data[
      female == 1
    ]
    
    model_formula <- (
      occupation_specific_double ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else if (subgroup == "Men") {
    
    estimation_data <- estimation_data[
      female == 0
    ]
    
    model_formula <- (
      occupation_specific_double ~
        log_undergrad_exposure_18_20 +
        urban |
        province_historical +
        analysis_birth_year +
        year
    )
    
  } else {
    
    stop(
      paste0(
        "Unknown subgroup: ",
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
    notes = FALSE
  )
  
  extract_treatment_result(
    model = model,
    outcome_name =
      "Occupation-specific double mismatch",
    occupation_code = occupation_code,
    subgroup = subgroup
  )
}


occupation_codes <- sort(
  unique(
    analysis_dt$isco_2digit_clean
  )
)

occupation_results <- rbindlist(
  lapply(
    occupation_codes,
    function(current_occupation) {
      
      cat(
        "\nEstimating occupation:",
        current_occupation,
        "\n"
      )
      
      rbindlist(
        lapply(
          subgroups,
          function(current_subgroup) {
            
            cat(
              "  ",
              current_subgroup,
              "\n"
            )
            
            estimate_occupation_specific_double_mismatch(
              data = analysis_dt,
              occupation_code =
                current_occupation,
              subgroup =
                current_subgroup
            )
          }
        )
      )
    }
  )
)


# ==============================================================================
# STEP 13: Merge labels and descriptive information
# ==============================================================================

occupation_results <- merge(
  occupation_results,
  isco_crosswalk,
  by = "isco_2digit_clean",
  all.x = TRUE
)

occupation_results <- merge(
  occupation_results,
  double_mismatch_descriptives[
    ,
    .(
      isco_2digit_clean,
      subgroup,
      double_mismatch_observations,
      weighted_double_mismatch,
      weighted_share_within_gender
    )
  ],
  by = c(
    "isco_2digit_clean",
    "subgroup"
  ),
  all.x = TRUE
)

occupation_results[
  is.na(double_mismatch_observations),
  double_mismatch_observations := 0L
]

occupation_results[
  is.na(weighted_double_mismatch),
  weighted_double_mismatch := 0
]

occupation_results[
  is.na(weighted_share_within_gender),
  weighted_share_within_gender := 0
]


# ==============================================================================
# STEP 14: Validate the decomposition
# ==============================================================================

# Since occupation-specific double-mismatch outcomes sum to total double
# mismatch for every observation, their estimated coefficients should also sum
# to the total coefficient when the sample and regressors are identical.

coefficient_decomposition <- occupation_results[
  ,
  .(
    sum_occupation_effects =
      sum(effect_pp_10_percent)
  ),
  by = subgroup
]

coefficient_decomposition <- merge(
  coefficient_decomposition,
  total_double_results[
    ,
    .(
      subgroup,
      total_double_effect =
        effect_pp_10_percent
    )
  ],
  by = "subgroup",
  all.x = TRUE
)

coefficient_decomposition[
  ,
  decomposition_difference :=
    sum_occupation_effects -
    total_double_effect
]

cat(
  "\n============================================================\n",
  "COEFFICIENT DECOMPOSITION CHECK\n",
  "============================================================\n"
)

print(coefficient_decomposition)

if (
  any(
    abs(
      coefficient_decomposition$
      decomposition_difference
    ) > 0.000001
  )
) {
  warning(
    paste0(
      "Occupation-specific coefficients do not sum exactly to the total ",
      "double-mismatch coefficient. Check that all occupations use the ",
      "same sample and that all valid occupation codes are included."
    )
  )
}


# ==============================================================================
# STEP 15: Calculate contribution shares
# ==============================================================================

occupation_results <- merge(
  occupation_results,
  total_double_results[
    ,
    .(
      subgroup,
      total_double_effect =
        effect_pp_10_percent
    )
  ],
  by = "subgroup",
  all.x = TRUE
)

occupation_results[
  ,
  contribution_share_percent :=
    fifelse(
      total_double_effect != 0,
      100 *
        effect_pp_10_percent /
        total_double_effect,
      NA_real_
    )
]


# ==============================================================================
# STEP 16: Define transparent eligibility for top-occupation ranking
# ==============================================================================

# Ranking very small occupations can elevate unstable point estimates.
# The default threshold requires:
#   - at least 100 observed double-mismatched workers in that gender; and
#   - at least 0.5% of weighted double mismatch in that gender.
#
# These thresholds can be changed before inspecting the ranking.

minimum_double_mismatch_observations <- 100L
minimum_weighted_share <- 0.005
number_of_top_occupations <- 5L

occupation_results[
  ,
  ranking_eligible :=
    double_mismatch_observations >=
    minimum_double_mismatch_observations &
    weighted_share_within_gender >=
    minimum_weighted_share
]


# ==============================================================================
# STEP 17: Select the top five positive contributors for women and men
# ==============================================================================

top_gender_results <- occupation_results[
  subgroup %in% c(
    "Women",
    "Men"
  ) &
    ranking_eligible == TRUE &
    effect_pp_10_percent > 0
][
  order(
    subgroup,
    -effect_pp_10_percent
  ),
  head(
    .SD,
    number_of_top_occupations
  ),
  by = subgroup
]

top_gender_results[
  ,
  rank_within_gender :=
    frank(
      -effect_pp_10_percent,
      ties.method = "first"
    ),
  by = subgroup
]

setorder(
  top_gender_results,
  subgroup,
  rank_within_gender
)

cat(
  "\n============================================================\n",
  "TOP FIVE OCCUPATIONS BY GENDER\n",
  "============================================================\n"
)

print(
  top_gender_results[
    ,
    .(
      subgroup,
      rank_within_gender,
      isco_2digit_clean,
      occupation_label,
      effect_pp_10_percent,
      standard_error_pp_10_percent,
      p_value,
      contribution_share_percent,
      double_mismatch_observations,
      weighted_share_within_gender
    )
  ],
  nrows = Inf
)


# ==============================================================================
# STEP 18: Create formatted top-five table
# ==============================================================================

top_gender_results[
  ,
  formatted_effect := paste0(
    formatC(
      effect_pp_10_percent,
      format = "f",
      digits = 3
    ),
    significance,
    " (",
    formatC(
      standard_error_pp_10_percent,
      format = "f",
      digits = 3
    ),
    ")"
  )
]

top_table <- top_gender_results[
  ,
  .(
    Gender = subgroup,
    Rank = rank_within_gender,
    ISCO = isco_2digit_clean,
    Occupation = occupation_label,
    `Effect (SE)` = formatted_effect,
    `Contribution share (%)` =
      round(
        contribution_share_percent,
        1
      ),
    `Descriptive mismatch share (%)` =
      round(
        100 *
          weighted_share_within_gender,
        1
      )
  )
]

print(
  top_table,
  nrows = Inf
)


# ==============================================================================
# STEP 19: Create separate figures for women and men
# ==============================================================================

create_gender_figure <- function(
    results_data,
    selected_gender
) {
  
  plot_data <- copy(
    results_data[
      subgroup ==
        selected_gender
    ]
  )
  
  plot_data[
    ,
    occupation_plot_label := factor(
      occupation_label,
      levels = rev(
        occupation_label[
          order(rank_within_gender)
        ]
      )
    )
  ]
  
  ggplot(
    plot_data,
    aes(
      x = effect_pp_10_percent,
      y = occupation_plot_label
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed"
    ) +
    geom_errorbarh(
      aes(
        xmin = lower_95,
        xmax = upper_95
      ),
      height = 0.15
    ) +
    geom_point(
      size = 2.5
    ) +
    labs(
      title = paste0(
        "Occupational destinations of double mismatch: ",
        selected_gender
      ),
      subtitle =
        paste0(
          "Top positive occupation-specific effects; ",
          "10% increase in university exposure"
        ),
      x =
        "Percentage-point effect",
      y =
        NULL
    ) +
    theme_minimal(
      base_size = 11
    ) +
    theme(
      panel.grid.minor =
        element_blank()
    )
}


women_figure <- create_gender_figure(
  results_data = top_gender_results,
  selected_gender = "Women"
)

men_figure <- create_gender_figure(
  results_data = top_gender_results,
  selected_gender = "Men"
)

print(women_figure)
print(men_figure)


# ==============================================================================
# STEP 20: Save all outputs
# ==============================================================================

sample_summary <- data.table(
  measure = c(
    "Joint sample observations",
    "Women observations",
    "Men observations",
    "Double-mismatched observations",
    "Historical provinces",
    "Birth cohorts",
    "Survey years"
  ),
  
  value = c(
    nrow(analysis_dt),
    analysis_dt[female == 1, .N],
    analysis_dt[female == 0, .N],
    analysis_dt[double_mismatch == 1, .N],
    uniqueN(analysis_dt$province_historical),
    uniqueN(analysis_dt$analysis_birth_year),
    uniqueN(analysis_dt$year)
  )
)

fwrite(
  sample_summary,
  file.path(
    output_folder,
    "occupation_double_mismatch_sample_summary.csv"
  ),
  bom = TRUE
)

fwrite(
  total_double_results,
  file.path(
    output_folder,
    "total_double_mismatch_results.csv"
  ),
  bom = TRUE
)

fwrite(
  double_mismatch_descriptives,
  file.path(
    output_folder,
    "double_mismatch_occupation_descriptives.csv"
  ),
  bom = TRUE
)

fwrite(
  occupation_results,
  file.path(
    output_folder,
    "all_occupation_specific_double_mismatch_results.csv"
  ),
  bom = TRUE
)

fwrite(
  coefficient_decomposition,
  file.path(
    output_folder,
    "occupation_coefficient_decomposition_check.csv"
  ),
  bom = TRUE
)

fwrite(
  top_gender_results,
  file.path(
    output_folder,
    "top_five_occupations_by_gender_detailed.csv"
  ),
  bom = TRUE
)

fwrite(
  top_table,
  file.path(
    output_folder,
    "top_five_occupations_by_gender_table.csv"
  ),
  bom = TRUE
)

ggsave(
  filename = file.path(
    output_folder,
    "top_double_mismatch_occupations_women.png"
  ),
  plot = women_figure,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    output_folder,
    "top_double_mismatch_occupations_men.png"
  ),
  plot = men_figure,
  width = 9,
  height = 5.5,
  dpi = 300
)


# ==============================================================================
# STEP 21: Final output validation
# ==============================================================================

expected_files <- c(
  "occupation_double_mismatch_sample_summary.csv",
  "total_double_mismatch_results.csv",
  "double_mismatch_occupation_descriptives.csv",
  "all_occupation_specific_double_mismatch_results.csv",
  "occupation_coefficient_decomposition_check.csv",
  "top_five_occupations_by_gender_detailed.csv",
  "top_five_occupations_by_gender_table.csv",
  "top_double_mismatch_occupations_women.png",
  "top_double_mismatch_occupations_men.png"
)

output_validation <- data.table(
  file = file.path(
    output_folder,
    expected_files
  )
)

output_validation[
  ,
  exists := file.exists(file)
]

cat(
  "\n============================================================\n",
  "OUTPUT VALIDATION\n",
  "============================================================\n"
)

print(output_validation)

if (any(output_validation$exists == FALSE)) {
  stop(
    "One or more expected output files were not created."
  )
}

cat(
  "\n============================================================\n",
  "ANALYSIS COMPLETED\n",
  "============================================================\n",
  "\nOutput folder:\n",
  output_folder,
  "\n"
)