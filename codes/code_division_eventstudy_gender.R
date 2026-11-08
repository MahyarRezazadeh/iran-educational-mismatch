# ==============================================================================
# DIVISION-LEVEL REFRAMING -- STEP 25B
# (1) Event-study / pre-trends check using the NEW baseline-quartile capacity
#     groups (Low vs High, per Section 2 of the brief), extending the existing
#     median-split design in update_Mahyar/event_study.R.
# (2) Gender heterogeneity: formal exposure x female interaction test,
#     extended to the mismatch outcomes (existing code_Robustness.R Step 12
#     only covers LFP/employment/unemployment/marriage, not mismatch).
#
# Preserves exactly: cohort definition (analysis_birth_year, 1355-1383),
# outcome construction (from the companion paper's fst pipeline), province +
# cohort + year FE, survey weights, two-way clustering by province x cohort.
# ==============================================================================

rm(list = ls())
invisible(gc())
options(scipen = 999, datatable.print.nrows = 100)

library(data.table)
library(fst)
library(fixest)

project_folder <- "E:/MismatchPipeline"
analysis_input_file <- file.path(project_folder, "LFS_analysis_outputs", "lfs_final_analysis_ready.fst")
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

needed_cols <- c(
  "province_historical", "analysis_birth_year", "year", "gender", "urban_rural", "weight",
  "log_undergrad_exposure_18_20", "undergrad_exposure_18_20",
  "labor_force_participation", "employed", "unemployed",
  "both_refined_vertical_horizontal_mismatch",
  "both_refined_mismatch_available",
  "overeducated_and_horizontal_mismatch_refined",
  "fully_matched_refined",
  "horizontal_mismatch_detailed_broad", "horizontal_detailed_eligible"
)

cat("Reading analysis file (selected columns)...\n")
dt <- as.data.table(read_fst(analysis_input_file, columns = needed_cols))
cat("Rows:", format(nrow(dt), big.mark = ","), "\n")

dt[, exposure := log_undergrad_exposure_18_20]
dt[, cohort := analysis_birth_year]
dt[, female := fcase(gender == 1, 1L, gender == 2, 0L, default = NA_integer_)]
dt[, urban := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]

capacity_groups <- fread(capacity_groups_file, encoding = "UTF-8")
dt <- merge(dt, capacity_groups[, .(province_historical, capacity_group, capacity_quartile)],
            by = "province_historical", all.x = TRUE)

ref_cohort <- min(dt$cohort, na.rm = TRUE)
cat("Reference cohort:", ref_cohort, "\n")


# ==============================================================================
# PART 1: Event study, baseline-quartile Low vs High capacity groups
# ==============================================================================
# Sample restricted to Low (Q1) and High (Q4) baseline-capacity provinces only
# -- the natural comparison implied by a quartile design; middle provinces
# (Q2-Q3) are not part of either group and are excluded from this check (they
# remain in every other regression in the paper, which uses the full
# continuous exposure measure, not the group indicator).

dt[, high_capacity := fcase(
  capacity_group == "High capacity (top quartile)", 1L,
  capacity_group == "Low capacity (bottom quartile)", 0L,
  default = NA_integer_
)]

run_event_study <- function(outcome_var, sample_filter_expr, label, fname) {
  cat("\n\n=== Event study (baseline-quartile groups):", label, "===\n")
  sub <- dt[
    eval(sample_filter_expr) & !is.na(get(outcome_var)) & !is.na(exposure) &
      !is.na(cohort) & !is.na(province_historical) & !is.na(year) &
      !is.na(female) & !is.na(urban) & !is.na(weight) & !is.na(high_capacity)
  ]
  cat("N =", format(nrow(sub), big.mark = ","), "\n")

  fml <- as.formula(paste0(
    outcome_var, " ~ i(cohort, high_capacity, ref = ", ref_cohort, ") + female + urban | ",
    "province_historical + cohort + year"
  ))

  m <- feols(fml, data = sub, weights = ~weight, cluster = ~province_historical + cohort)
  print(summary(m))

  saveRDS(m, file.path(output_folder, paste0("step25B_event_study_", fname, ".rds")))
  capture.output(summary(m), file = file.path(output_folder, paste0("step25B_event_study_", fname, ".txt")))

  coefs <- as.data.table(coeftable(m), keep.rownames = "term")
  fwrite(coefs, file.path(output_folder, paste0("step25B_event_study_", fname, "_coefs.csv")), bom = TRUE)
  fwrite(coefs, file.path(onedrive_output_folder, paste0("step25B_event_study_", fname, "_coefs.csv")), bom = TRUE)

  png(file.path(output_folder, paste0("figure_D5_event_study_", fname, ".png")), width = 2200, height = 1200, res = 220)
  iplot(m,
    xlab = "Birth cohort",
    main = paste0(
      "Event study: ", label,
      "\n(baseline-period bottom vs. top quartile provinces, ref. cohort ", ref_cohort, ")"
    ),
    ci.lty = 1, pt.pch = 16
  )
  abline(h = 0, lty = 2, col = "grey40")
  dev.off()
  file.copy(
    file.path(output_folder, paste0("figure_D5_event_study_", fname, ".png")),
    file.path(onedrive_output_folder, paste0("figure_D5_event_study_", fname, ".png")),
    overwrite = TRUE
  )
  cat("Saved figure_D5_event_study_", fname, ".png\n", sep = "")

  m
}

m_double <- run_event_study(
  "overeducated_and_horizontal_mismatch_refined",
  quote(both_refined_mismatch_available == 1),
  "Double mismatch (refined, over-educated & horizontally mismatched)",
  "double_mismatch"
)

m_horiz <- run_event_study(
  "horizontal_mismatch_detailed_broad",
  quote(horizontal_detailed_eligible == 1),
  "Horizontal mismatch (detailed, broad crosswalk)",
  "horizontal_mismatch"
)

m_fully <- run_event_study(
  "fully_matched_refined",
  quote(both_refined_mismatch_available == 1),
  "Fully matched employment (refined)",
  "fully_matched"
)


# ==============================================================================
# PART 2: Gender interaction (exposure x female), extended to mismatch outcomes
# ==============================================================================
# Same formula pattern as code_Robustness.R Step 12 (exposure_per_100 * female
# | province + cohort + year), applied here to the three mismatch outcomes,
# which that script does not cover. Uses the CONTINUOUS log-exposure measure
# (not the capacity group), consistent with the paper's main specification.

gender_interaction_results <- list()

run_gender_interaction <- function(outcome_var, sample_filter_expr, label, fname) {
  cat("\n\n=== Gender interaction (exposure x female):", label, "===\n")
  sub <- dt[
    eval(sample_filter_expr) & !is.na(get(outcome_var)) & !is.na(exposure) &
      !is.na(cohort) & !is.na(province_historical) & !is.na(year) &
      !is.na(female) & !is.na(urban) & !is.na(weight)
  ]
  cat("N =", format(nrow(sub), big.mark = ","), "\n")

  fml <- as.formula(paste0(
    outcome_var, " ~ exposure * female + urban | province_historical + cohort + year"
  ))
  m <- feols(fml, data = sub, weights = ~weight, cluster = ~province_historical + cohort)
  print(summary(m))

  ct <- coeftable(m)
  beta_male <- ct["exposure", "Estimate"]
  se_male <- ct["exposure", "Std. Error"]
  beta_diff <- ct["exposure:female", "Estimate"]
  se_diff <- ct["exposure:female", "Std. Error"]
  z_diff <- beta_diff / se_diff
  p_diff <- 2 * pnorm(abs(z_diff), lower.tail = FALSE)

  vc <- vcov(m)
  beta_female <- beta_male + beta_diff
  se_female <- sqrt(vc["exposure", "exposure"] + vc["exposure:female", "exposure:female"] +
    2 * vc["exposure", "exposure:female"])

  result <- data.table(
    outcome = label, N = nrow(sub),
    beta_male = beta_male, se_male = se_male,
    beta_female = beta_female, se_female = se_female,
    beta_diff_female_minus_male = beta_diff, se_diff = se_diff,
    z_diff = z_diff, p_diff = p_diff
  )
  print(result)

  saveRDS(m, file.path(output_folder, paste0("step25B_gender_interaction_", fname, ".rds")))
  gender_interaction_results[[fname]] <<- result
}

run_gender_interaction(
  "overeducated_and_horizontal_mismatch_refined",
  quote(both_refined_mismatch_available == 1),
  "Double mismatch (refined)", "double_mismatch"
)
run_gender_interaction(
  "horizontal_mismatch_detailed_broad",
  quote(horizontal_detailed_eligible == 1),
  "Horizontal mismatch (detailed, broad)", "horizontal_mismatch"
)
run_gender_interaction(
  "fully_matched_refined",
  quote(both_refined_mismatch_available == 1),
  "Fully matched employment (refined)", "fully_matched"
)

gender_interaction_table <- rbindlist(gender_interaction_results)
fwrite(gender_interaction_table, file.path(output_folder, "step25B_gender_interaction_mismatch_results.csv"), bom = TRUE)
fwrite(gender_interaction_table, file.path(onedrive_output_folder, "step25B_gender_interaction_mismatch_results.csv"), bom = TRUE)

cat("\n\nStep 25B complete.\n")
print(gender_interaction_table)
