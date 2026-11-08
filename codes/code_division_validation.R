# ==============================================================================
# DIVISION-LEVEL REFRAMING -- STEP 25C
# Validation checks not already covered elsewhere in the codebase:
#   (a) missing-data / coverage by province-year in the exposure panel
#   (b) distribution / outlier screen on exposure values
#   (c) a simple check for reporting/classification breaks over time
#   (d) confirmation that province identifiers match 1:1 between the raw
#       exposure panel and the LFS-side historical-province harmonization
# Leave-one-province-out (code_robutness.R), influential-province variance
# decomposition (code_check.R / step7), and Tehran-exclusion robustness
# (code_Exclude_Tehran.R) already exist and are not rebuilt here -- this
# script only fills the specific gaps identified in the codebase inventory.
# ==============================================================================

rm(list = ls())
invisible(gc())
options(scipen = 999, datatable.print.nrows = 100)

library(data.table)

project_folder <- "E:/MismatchPipeline"
raw_panel_file <- file.path(
  project_folder,
  "final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv"
)
capacity_groups_file <- file.path(
  project_folder, "LFS_analysis_outputs", "step25_division_level_capacity",
  "step25A_province_baseline_capacity_groups.csv"
)

output_folder <- file.path(project_folder, "LFS_analysis_outputs", "step25_division_level_capacity")
onedrive_output_folder <- paste0(
  "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/",
  "Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/",
  "outputs/step25_division_level_capacity"
)

raw_panel <- fread(raw_panel_file, encoding = "UTF-8")
capacity_groups <- fread(capacity_groups_file, encoding = "UTF-8")


# ------------------------------------------------------------
# (a) Province identifier consistency check
# ------------------------------------------------------------
panel_provinces <- sort(unique(raw_panel$province_historical))
group_provinces <- sort(unique(capacity_groups$province_historical))

identifier_check <- data.table(
  check = "Province identifiers match between raw exposure panel and capacity-group classification",
  n_panel_provinces = length(panel_provinces),
  n_group_provinces = length(group_provinces),
  n_mismatched = length(union(panel_provinces, group_provinces)) - length(intersect(panel_provinces, group_provinces)),
  passes = length(setdiff(panel_provinces, group_provinces)) == 0 &&
    length(setdiff(group_provinces, panel_provinces)) == 0
)
print(identifier_check)


# ------------------------------------------------------------
# (b) Coverage by province-year: is the panel balanced?
# ------------------------------------------------------------
expected_years <- 1373:1403
coverage_by_province <- raw_panel[
  , .(
      n_years_observed = uniqueN(exposure_year),
      n_years_expected = length(expected_years),
      min_year = min(exposure_year), max_year = max(exposure_year),
      n_missing_enrollment = sum(is.na(enroll_undergrad_professional)),
      n_zero_enrollment = sum(enroll_undergrad_professional == 0, na.rm = TRUE)
    ),
  by = province_historical
][
  , balanced := n_years_observed == n_years_expected
]
setorder(coverage_by_province, n_years_observed)

cat("\nProvince-year coverage summary:\n")
print(coverage_by_province)
cat(
  "\nProvinces with unbalanced coverage (missing at least one year 1373-1403): ",
  coverage_by_province[balanced == FALSE, .N], " of ", nrow(coverage_by_province), "\n", sep = ""
)

fwrite(coverage_by_province, file.path(output_folder, "step25C_province_year_coverage.csv"), bom = TRUE)
fwrite(coverage_by_province, file.path(onedrive_output_folder, "step25C_province_year_coverage.csv"), bom = TRUE)


# ------------------------------------------------------------
# (c) Distribution / outlier screen on per-capita exposure
# ------------------------------------------------------------
exposure_distribution <- raw_panel[
  , .(
      mean = mean(undergrad_professional_per_100k, na.rm = TRUE),
      sd = sd(undergrad_professional_per_100k, na.rm = TRUE),
      p1 = quantile(undergrad_professional_per_100k, 0.01, na.rm = TRUE),
      p5 = quantile(undergrad_professional_per_100k, 0.05, na.rm = TRUE),
      p50 = quantile(undergrad_professional_per_100k, 0.50, na.rm = TRUE),
      p95 = quantile(undergrad_professional_per_100k, 0.95, na.rm = TRUE),
      p99 = quantile(undergrad_professional_per_100k, 0.99, na.rm = TRUE),
      min = min(undergrad_professional_per_100k, na.rm = TRUE),
      max = max(undergrad_professional_per_100k, na.rm = TRUE)
    )
]
print(exposure_distribution)

# Flag province-years more than 5 SD from that province's own mean (implausible
# within-province jump, as opposed to legitimate cross-province level differences)
raw_panel[, `:=`(
  province_mean = mean(undergrad_professional_per_100k, na.rm = TRUE),
  province_sd = sd(undergrad_professional_per_100k, na.rm = TRUE)
), by = province_historical]
raw_panel[, within_province_z := (undergrad_professional_per_100k - province_mean) / province_sd]

outlier_flags <- raw_panel[
  abs(within_province_z) > 5,
  .(province_historical, exposure_year, undergrad_professional_per_100k, within_province_z)
]
setorder(outlier_flags, -within_province_z)
cat("\nWithin-province outlier flags (|z| > 5 relative to that province's own 1373-1403 mean):\n")
print(outlier_flags)

fwrite(exposure_distribution, file.path(output_folder, "step25C_exposure_distribution.csv"), bom = TRUE)
fwrite(outlier_flags, file.path(output_folder, "step25C_within_province_outlier_flags.csv"), bom = TRUE)
fwrite(outlier_flags, file.path(onedrive_output_folder, "step25C_within_province_outlier_flags.csv"), bom = TRUE)


# ------------------------------------------------------------
# (d) Reporting/classification-break check: year-over-year national growth
# ------------------------------------------------------------
# A genuine reporting-system change (e.g. a new institution type added to the
# ministry's count, or a coding-scheme revision) should show up as an
# implausible single-year national jump unrelated to the secular expansion
# trend. This is a screen, not a formal structural-break test.
national_annual <- raw_panel[
  , .(national_enrollment = sum(enroll_undergrad_professional, na.rm = TRUE)),
  by = exposure_year
][order(exposure_year)]
national_annual[, pct_change := national_enrollment / shift(national_enrollment) - 1]

cat("\nYear-over-year national enrollment growth (screen for reporting breaks):\n")
print(national_annual)

large_jumps <- national_annual[abs(pct_change) > 0.5]
cat("\nYears with >50% year-over-year national change (candidate reporting breaks):\n")
print(large_jumps)

fwrite(national_annual, file.path(output_folder, "step25C_national_annual_growth_screen.csv"), bom = TRUE)
fwrite(national_annual, file.path(onedrive_output_folder, "step25C_national_annual_growth_screen.csv"), bom = TRUE)


# ------------------------------------------------------------
# (e) Consolidated validation summary
# ------------------------------------------------------------
validation_summary <- data.table(
  check = c(
    "Province identifiers consistent across sources",
    "Panel years span full 1373-1403 range",
    "Any province with unbalanced (incomplete) year coverage",
    "Any within-province exposure outlier (|z| > 5)",
    "Any single-year national enrollment change > 50%"
  ),
  result = c(
    identifier_check$passes,
    (min(raw_panel$exposure_year) == 1373 && max(raw_panel$exposure_year) == 1403),
    coverage_by_province[balanced == FALSE, .N] > 0,
    nrow(outlier_flags) > 0,
    nrow(large_jumps) > 0
  )
)
print(validation_summary)
fwrite(validation_summary, file.path(output_folder, "step25C_validation_summary.csv"), bom = TRUE)
fwrite(validation_summary, file.path(onedrive_output_folder, "step25C_validation_summary.csv"), bom = TRUE)

cat("\n\nStep 25C complete.\n")
