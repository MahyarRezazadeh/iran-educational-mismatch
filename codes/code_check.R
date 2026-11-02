# ==============================================================================
# PROJECT: University Expansion and Individual Outcomes in Iran
# STEP 7: Treatment Variation and Identification Diagnostics
#
# Main goals:
#   1. Examine treatment variation across provinces and birth cohorts.
#   2. Separate within-province and between-province variation.
#   3. Check treatment trends by survey year and gender.
#   4. Create exposure quartiles for descriptive comparisons.
#   5. Prepare the variables needed for baseline regressions.
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
  "step7_treatment_variation"
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
# STEP 2: Read only variables needed for treatment diagnostics
# ==============================================================================

variables_to_read <- c(
  "year",
  "weight",
  "age",
  "gender",
  "birth_year",
  "province_historical",
  "urban_rural",
  "undergrad_exposure_18_20",
  "complete_undergrad_exposure",
  "labor_force_participation",
  "employed",
  "unemployed",
  "currently_married",
  "ever_married",
  "vertical_mismatch_final",
  "horizontal_mismatch_detailed_broad",
  "vertical_fullperiod_eligible_final",
  "horizontal_detailed_eligible",
  "recent_cross_historical_region_move"
)

cat("\nReading variables for treatment diagnostics...\n")

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
# STEP 3: Standardized variables and main sample
# ==============================================================================

dt[
  ,
  historical_province := province_historical
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
    as.integer(
      !is.na(weight) &
        is.finite(weight) &
        weight > 0
    )
]

dt[
  ,
  sample_complete_exposure :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(undergrad_exposure_18_20)
    )
]

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

dt[
  ,
  sample_vertical_mismatch :=
    as.integer(
      sample_main == 1L &
        vertical_fullperiod_eligible_final == 1L &
        !is.na(vertical_mismatch_final)
    )
]

dt[
  ,
  sample_horizontal_mismatch :=
    as.integer(
      sample_main == 1L &
        horizontal_detailed_eligible == 1L &
        !is.na(horizontal_mismatch_detailed_broad)
    )
]

cat(
  "\nMain sample:",
  format(
    sum(dt$sample_main == 1L, na.rm = TRUE),
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# RECREATE MAIN SAMPLE FOR STEP 7
# ==============================================================================

dt[
  ,
  historical_province := province_historical
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
    as.integer(
      !is.na(weight) &
        is.finite(weight) &
        weight > 0
    )
]

dt[
  ,
  sample_complete_exposure :=
    as.integer(
      complete_undergrad_exposure == 1L &
        !is.na(undergrad_exposure_18_20)
    )
]

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

cat(
  "\nMain sample:",
  format(
    sum(dt$sample_main == 1L, na.rm = TRUE),
    big.mark = ","
  ),
  "\n"
)





# ==============================================================================
# STEP 3B: Restrict to valid analytical birth cohorts
# ==============================================================================

dt[
  ,
  valid_birth_cohort :=
    as.integer(
      !is.na(birth_year) &
        birth_year >= 55L &
        birth_year <= 83L
    )
]

dt[
  ,
  sample_main_valid_cohort :=
    as.integer(
      sample_main == 1L &
        valid_birth_cohort == 1L
    )
]

cat(
  "\nInvalid cohort observations excluded:",
  format(
    dt[
      sample_main == 1L &
        valid_birth_cohort == 0L,
      .N
    ],
    big.mark = ","
  ),
  "\n"
)

cat(
  "Final valid-cohort main sample:",
  format(
    dt[
      sample_main_valid_cohort == 1L,
      .N
    ],
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 4: Exposure distribution in the main sample
# ==============================================================================

exposure_summary <- dt[
  sample_main == 1L,
  .(
    observations = .N,
    
    weighted_mean = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    ),
    
    weighted_sd = sqrt(
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
    ),
    
    minimum = min(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    p10 = quantile(
      undergrad_exposure_18_20,
      probs = 0.10,
      na.rm = TRUE
    ),
    
    p25 = quantile(
      undergrad_exposure_18_20,
      probs = 0.25,
      na.rm = TRUE
    ),
    
    median = median(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    p75 = quantile(
      undergrad_exposure_18_20,
      probs = 0.75,
      na.rm = TRUE
    ),
    
    p90 = quantile(
      undergrad_exposure_18_20,
      probs = 0.90,
      na.rm = TRUE
    ),
    
    maximum = max(
      undergrad_exposure_18_20,
      na.rm = TRUE
    )
  )
]

print(exposure_summary)

fwrite(
  exposure_summary,
  file.path(
    output_folder,
    "step7_exposure_summary.csv"
  ),
  bom = TRUE
)

# ==============================================================================
# STEP 5: Exposure by birth cohort
# ==============================================================================

exposure_by_cohort <- dt[
  sample_main == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    weighted_mean_exposure = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    ),
    
    unweighted_mean_exposure = mean(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    exposure_sd = sd(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    minimum_exposure = min(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    maximum_exposure = max(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    provinces = uniqueN(
      historical_province
    )
  ),
  by = birth_year
][
  order(birth_year)
]

print(exposure_by_cohort)

fwrite(
  exposure_by_cohort,
  file.path(
    output_folder,
    "step7_exposure_by_birth_cohort.csv"
  ),
  bom = TRUE
)


# ==============================================================================
# STEP 6: Exposure by historical province
# ==============================================================================

exposure_by_province <- dt[
  sample_main == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    weighted_mean_exposure = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    ),
    
    unweighted_mean_exposure = mean(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    exposure_sd = sd(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    minimum_exposure = min(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    maximum_exposure = max(
      undergrad_exposure_18_20,
      na.rm = TRUE
    ),
    
    birth_cohorts = uniqueN(
      birth_year
    )
  ),
  by = historical_province
][
  order(weighted_mean_exposure)
]

print(exposure_by_province)

fwrite(
  exposure_by_province,
  file.path(
    output_folder,
    "step7_exposure_by_province.csv"
  ),
  bom = TRUE
)



# ==============================================================================
# STEP 7: Province-by-cohort exposure panel
#
# This is the core treatment-variation table.
# ==============================================================================

province_cohort_exposure <- dt[
  sample_main == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    exposure = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    historical_province,
    birth_year
  )
][
  order(
    historical_province,
    birth_year
  )
]

fwrite(
  province_cohort_exposure,
  file.path(
    output_folder,
    "step7_province_cohort_exposure.csv"
  ),
  bom = TRUE
)

cat(
  "\nProvince-cohort cells:",
  format(
    nrow(province_cohort_exposure),
    big.mark = ","
  ),
  "\n"
)


# ==============================================================================
# STEP 8: Decompose treatment variation
#
# Total variation is separated into:
#   - Variation explained by province fixed effects
#   - Variation explained by cohort fixed effects
#   - Residual province-by-cohort variation
# ==============================================================================

variation_model <- feols(
  exposure ~ 1 |
    historical_province +
    birth_year,
  data = province_cohort_exposure,
  weights = ~weighted_population,
  
  # Keep singleton province-cohort observations.
  fixef.rm = "none"
)

# Confirm that residuals exist for every province-cohort cell.
stopifnot(
  length(resid(variation_model)) ==
    nrow(province_cohort_exposure)
)

province_cohort_exposure[
  ,
  exposure_residualized :=
    resid(variation_model)
]


# Weighted overall exposure mean.
overall_exposure_mean <- weighted.mean(
  province_cohort_exposure$exposure,
  province_cohort_exposure$weighted_population,
  na.rm = TRUE
)


# Total weighted variance.
total_variance <- weighted.mean(
  (
    province_cohort_exposure$exposure -
      overall_exposure_mean
  )^2,
  province_cohort_exposure$weighted_population,
  na.rm = TRUE
)


# Residual weighted variance after province and cohort fixed effects.
residual_variance <- weighted.mean(
  province_cohort_exposure$exposure_residualized^2,
  province_cohort_exposure$weighted_population,
  na.rm = TRUE
)


variation_decomposition <- data.table(
  measure = c(
    "Province-cohort cells",
    "Total weighted variance",
    "Residual variance after province and cohort fixed effects",
    "Residual share of total variance",
    "Explained share of total variance",
    "Residual standard deviation"
  ),
  
  value = c(
    nrow(province_cohort_exposure),
    total_variance,
    residual_variance,
    residual_variance / total_variance,
    1 - residual_variance / total_variance,
    sqrt(residual_variance)
  )
)

print(variation_decomposition)

fwrite(
  variation_decomposition,
  file.path(
    output_folder,
    "step7_variation_decomposition.csv"
  ),
  bom = TRUE
)

# Overwrite the province-cohort file to include residualized exposure.
fwrite(
  province_cohort_exposure,
  file.path(
    output_folder,
    "step7_province_cohort_exposure.csv"
  ),
  bom = TRUE
)




# ==============================================================================
# STEP 9: Corrected province-by-cohort exposure panel
# ==============================================================================

province_cohort_exposure_valid <- dt[
  sample_main_valid_cohort == 1L,
  .(
    observations = .N,
    
    weighted_population = sum(
      weight,
      na.rm = TRUE
    ),
    
    exposure = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    )
  ),
  by = .(
    historical_province,
    birth_year
  )
][
  order(
    historical_province,
    birth_year
  )
]

cat(
  "\nValid province-cohort cells:",
  nrow(province_cohort_exposure_valid),
  "\n"
)

fwrite(
  province_cohort_exposure_valid,
  file.path(
    output_folder,
    "step7_province_cohort_exposure_valid.csv"
  ),
  bom = TRUE
)





# ==============================================================================
# STEP 10: Corrected variation decomposition
# ==============================================================================

variation_model_valid <- feols(
  exposure ~ 1 |
    historical_province +
    birth_year,
  data = province_cohort_exposure_valid,
  weights = ~weighted_population,
  fixef.rm = "none"
)

stopifnot(
  length(resid(variation_model_valid)) ==
    nrow(province_cohort_exposure_valid)
)

province_cohort_exposure_valid[
  ,
  exposure_residualized :=
    resid(variation_model_valid)
]

overall_mean_valid <- weighted.mean(
  province_cohort_exposure_valid$exposure,
  province_cohort_exposure_valid$weighted_population,
  na.rm = TRUE
)

total_variance_valid <- weighted.mean(
  (
    province_cohort_exposure_valid$exposure -
      overall_mean_valid
  )^2,
  province_cohort_exposure_valid$weighted_population,
  na.rm = TRUE
)

residual_variance_valid <- weighted.mean(
  province_cohort_exposure_valid$exposure_residualized^2,
  province_cohort_exposure_valid$weighted_population,
  na.rm = TRUE
)

variation_decomposition_valid <- data.table(
  measure = c(
    "Valid province-cohort cells",
    "Total weighted variance",
    "Residual variance after province and cohort fixed effects",
    "Residual share of total variance",
    "Explained share of total variance",
    "Residual standard deviation"
  ),
  
  value = c(
    nrow(province_cohort_exposure_valid),
    total_variance_valid,
    residual_variance_valid,
    residual_variance_valid / total_variance_valid,
    1 - residual_variance_valid / total_variance_valid,
    sqrt(residual_variance_valid)
  )
)

print(variation_decomposition_valid)

fwrite(
  variation_decomposition_valid,
  file.path(
    output_folder,
    "step7_variation_decomposition_valid.csv"
  ),
  bom = TRUE
)

# Save the corrected panel again with residualized exposure included.
fwrite(
  province_cohort_exposure_valid,
  file.path(
    output_folder,
    "step7_province_cohort_exposure_valid.csv"
  ),
  bom = TRUE
)





# ==============================================================================
# STEP 11: Residual treatment variation by province
# ==============================================================================

residual_variation_by_province <- province_cohort_exposure_valid[
  ,
  .(
    cohort_cells = .N,
    
    weighted_population = sum(
      weighted_population,
      na.rm = TRUE
    ),
    
    mean_exposure = weighted.mean(
      exposure,
      weighted_population,
      na.rm = TRUE
    ),
    
    residual_sd = sqrt(
      weighted.mean(
        exposure_residualized^2,
        weighted_population,
        na.rm = TRUE
      )
    ),
    
    mean_absolute_residual = weighted.mean(
      abs(exposure_residualized),
      weighted_population,
      na.rm = TRUE
    ),
    
    minimum_residual = min(
      exposure_residualized,
      na.rm = TRUE
    ),
    
    maximum_residual = max(
      exposure_residualized,
      na.rm = TRUE
    )
  ),
  by = historical_province
][
  order(-residual_sd)
]

print(residual_variation_by_province)

fwrite(
  residual_variation_by_province,
  file.path(
    output_folder,
    "step7_residual_variation_by_province.csv"
  ),
  bom = TRUE
)






# ==============================================================================
# STEP 12: Residual treatment variation by birth cohort
# ==============================================================================

residual_variation_by_cohort <- province_cohort_exposure_valid[
  ,
  .(
    province_cells = .N,
    
    weighted_population = sum(
      weighted_population,
      na.rm = TRUE
    ),
    
    mean_exposure = weighted.mean(
      exposure,
      weighted_population,
      na.rm = TRUE
    ),
    
    residual_sd = sqrt(
      weighted.mean(
        exposure_residualized^2,
        weighted_population,
        na.rm = TRUE
      )
    ),
    
    mean_absolute_residual = weighted.mean(
      abs(exposure_residualized),
      weighted_population,
      na.rm = TRUE
    ),
    
    minimum_residual = min(
      exposure_residualized,
      na.rm = TRUE
    ),
    
    maximum_residual = max(
      exposure_residualized,
      na.rm = TRUE
    )
  ),
  by = birth_year
][
  order(birth_year)
]

print(residual_variation_by_cohort)

fwrite(
  residual_variation_by_cohort,
  file.path(
    output_folder,
    "step7_residual_variation_by_cohort.csv"
  ),
  bom = TRUE
)






# ==============================================================================
# STEP 13: Treatment trend by birth cohort
# ==============================================================================

cohort_plot_data <- dt[
  sample_main_valid_cohort == 1L,
  .(
    weighted_mean_exposure = weighted.mean(
      undergrad_exposure_18_20,
      weight,
      na.rm = TRUE
    )
  ),
  by = birth_year
][
  order(birth_year)
]

p_cohort <- ggplot(
  cohort_plot_data,
  aes(
    x = birth_year,
    y = weighted_mean_exposure
  )
) +
  geom_line(
    linewidth = 0.8
  ) +
  geom_point(
    size = 1.8
  ) +
  labs(
    title = "University Exposure by Birth Cohort",
    x = "Birth cohort",
    y = "Weighted mean exposure at ages 18–20"
  ) +
  theme_minimal()

ggsave(
  file.path(
    output_folder,
    "step7_exposure_by_birth_cohort.png"
  ),
  plot = p_cohort,
  width = 8,
  height = 5,
  dpi = 300
)




# ==============================================================================
# STEP 14: Province-specific exposure trends
# ==============================================================================

p_province <- ggplot(
  province_cohort_exposure_valid,
  aes(
    x = birth_year,
    y = exposure,
    group = historical_province
  )
) +
  geom_line(
    alpha = 0.45,
    linewidth = 0.5
  ) +
  labs(
    title = "University Exposure by Province and Birth Cohort",
    x = "Birth cohort",
    y = "Exposure at ages 18–20"
  ) +
  theme_minimal()

ggsave(
  file.path(
    output_folder,
    "step7_province_cohort_exposure_trends.png"
  ),
  plot = p_province,
  width = 10,
  height = 6,
  dpi = 300
)





# ==============================================================================
# STEP 15: Sensitivity of identifying variation to influential provinces
# ==============================================================================

calculate_variation_decomposition <- function(
    panel_data,
    specification_name
) {
  
  model <- feols(
    exposure ~ 1 |
      historical_province +
      birth_year,
    data = panel_data,
    weights = ~weighted_population,
    fixef.rm = "none"
  )
  
  panel_copy <- copy(panel_data)
  
  panel_copy[
    ,
    residual_exposure := resid(model)
  ]
  
  overall_mean <- weighted.mean(
    panel_copy$exposure,
    panel_copy$weighted_population,
    na.rm = TRUE
  )
  
  total_variance <- weighted.mean(
    (
      panel_copy$exposure -
        overall_mean
    )^2,
    panel_copy$weighted_population,
    na.rm = TRUE
  )
  
  residual_variance <- weighted.mean(
    panel_copy$residual_exposure^2,
    panel_copy$weighted_population,
    na.rm = TRUE
  )
  
  data.table(
    specification = specification_name,
    
    provinces = uniqueN(
      panel_copy$historical_province
    ),
    
    province_cohort_cells = nrow(
      panel_copy
    ),
    
    total_variance = total_variance,
    
    residual_variance = residual_variance,
    
    residual_share = residual_variance /
      total_variance,
    
    residual_sd = sqrt(
      residual_variance
    )
  )
}


variation_all_provinces <- calculate_variation_decomposition(
  province_cohort_exposure_valid,
  "All provinces"
)


variation_without_semnan <- calculate_variation_decomposition(
  province_cohort_exposure_valid[
    historical_province != "Semnan"
  ],
  "Exclude Semnan"
)


variation_without_semnan_qazvin <- calculate_variation_decomposition(
  province_cohort_exposure_valid[
    !historical_province %in%
      c(
        "Semnan",
        "Qazvin"
      )
  ],
  "Exclude Semnan and Qazvin"
)


variation_influence_check <- rbindlist(
  list(
    variation_all_provinces,
    variation_without_semnan,
    variation_without_semnan_qazvin
  )
)

print(
  variation_influence_check
)

fwrite(
  variation_influence_check,
  file.path(
    output_folder,
    "step7_variation_influence_check.csv"
  ),
  bom = TRUE
)



# ==============================================================================
# STEP 16: Province contribution to residual treatment variance
# ==============================================================================

residual_variation_by_province[
  ,
  residual_variance_contribution :=
    residual_sd^2 *
    weighted_population
]

residual_variation_by_province[
  ,
  residual_variance_share :=
    residual_variance_contribution /
    sum(
      residual_variance_contribution,
      na.rm = TRUE
    )
]

residual_variation_by_province[
  ,
  cumulative_residual_variance_share :=
    cumsum(
      residual_variance_share
    )
]

print(
  residual_variation_by_province[
    ,
    .(
      historical_province,
      residual_sd,
      residual_variance_share,
      cumulative_residual_variance_share
    )
  ]
)

fwrite(
  residual_variation_by_province,
  file.path(
    output_folder,
    "step7_residual_variation_by_province_with_shares.csv"
  ),
  bom = TRUE
)