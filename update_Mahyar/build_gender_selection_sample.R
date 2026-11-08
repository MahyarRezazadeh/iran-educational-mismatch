# ==============================================================================
# Build the analysis sample for: "Is the rise in women's education leading to
# more selective employment (i.e., a widening gender gap driven by who
# selects into work), rather than more women working?"
#
# Source: the paper's final construction-pipeline output
# (lfs_step5d_refined_mismatch_analysis_ready.fst), the same file that was
# exported as update_Mahyar/data_ready.dta. Re-used here directly (fst is
# much faster to read than the .dta) so the sample is provably built from
# the exact same verified data.
#
# Output: update_Mahyar/gender_selection_sample.dta
# ==============================================================================

library(haven)
library(fst)
library(data.table)

in_file  <- "E:/MismatchPipeline/LFS_analysis_outputs/lfs_step5d_refined_mismatch_analysis_ready.fst"
out_dta  <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_sample.dta"

needed_cols <- c(
  "id", "quarter", "year", "member_number",
  "province_historical", "analysis_birth_year", "urban_rural",
  "gender", "age", "mar_status", "rel_hh", "weight",
  "log_undergrad_exposure_18_20", "undergrad_exposure_18_20",
  "complete_undergrad_exposure", "baseline_exposure_sample",
  "analysis_sample_eligible",
  "labor_force_participation", "employed", "unemployed",
  "main_job_hours_per_week", "all_jobs_hours_per_week",
  "educ_level", "educ_level_num", "education_rank", "tertiary_educated",
  "main_job_title_isco_code", "isco_2digit",
  "currently_married", "ever_married"
)

cat("\nReading pipeline output (fst)...\n")
dt <- as.data.table(read_fst(in_file, columns = needed_cols))
cat("Rows:", format(nrow(dt), big.mark = ","), " Cols:", ncol(dt), "\n")

# ------------------------------------------------------------------------
# Reconstruct household-composition variables (exclusion-restriction
# candidates for a Heckman selection model): presence/number of young
# children in the household, household size. Rebuilt here via id x year x
# quarter grouping on rel_hh (1/2/3 = child codes, matching the
# construction used earlier when data_ready.dta was assembled), because
# code_July10.R's Step 1 variable-selection list drops these before they
# reach the final analysis file.
# ------------------------------------------------------------------------
dt[, household_size := .N, by = .(id, year, quarter)]
dt[, child_member := rel_hh %in% c(1, 2, 3)]
dt[, has_child := as.integer(any(child_member)), by = .(id, year, quarter)]
dt[, num_kids := sum(rel_hh %in% c(1, 2, 3)), by = .(id, year, quarter)]
dt[, child_member := NULL]

# ------------------------------------------------------------------------
# Core sample restrictions: the paper's own baseline exposure-eligible
# sample, restricted to valid weight/province/cohort/gender/urban (the
# same restrictions used for Table 1 in the paper), broad labor-market
# sample so employment/LFP are always defined.
# ------------------------------------------------------------------------
dt[, cohort := as.integer(analysis_birth_year)]
dt[, female := fcase(gender == 1, 1L, gender == 2, 0L, default = NA_integer_)]
dt[, urban  := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]

sample <- dt[
  analysis_sample_eligible == 1L &
    baseline_exposure_sample == 1L &
    !is.na(log_undergrad_exposure_18_20) &
    !is.na(weight) & weight > 0 &
    !is.na(cohort) & cohort >= 1355L & cohort <= 1383L &
    !is.na(province_historical) & !is.na(female) & !is.na(urban)
]

cat("\nAnalysis sample:", format(nrow(sample), big.mark = ","), "\n")
cat("Women:", format(sum(sample$female == 1L), big.mark = ","),
    " Men:", format(sum(sample$female == 0L), big.mark = ","), "\n")

# Weekly hours: prefer all-jobs hours, fall back to main-job hours; only
# meaningful among the employed.
sample[, weekly_hours := fifelse(!is.na(all_jobs_hours_per_week), all_jobs_hours_per_week, main_job_hours_per_week)]
sample[employed != 1L, weekly_hours := NA_real_]

cat("\nWeighted employment rate by gender:\n")
print(sample[, .(emp_rate = weighted.mean(employed, weight, na.rm = TRUE)), by = female])

cat("\nWeighted mean weekly hours (employed) by gender:\n")
print(sample[employed == 1L, .(mean_hours = weighted.mean(weekly_hours, weight, na.rm = TRUE)), by = female])

cat("\nSaving gender_selection_sample.dta ...\n")
write_dta(sample, out_dta)
cat("Saved:", out_dta, "\n")
cat("Final rows:", format(nrow(sample), big.mark = ","), " cols:", ncol(sample), "\n")
