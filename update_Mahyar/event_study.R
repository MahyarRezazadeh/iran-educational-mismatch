library(haven)
library(data.table)
library(fixest)

path <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/data_ready.dta"
out_dir <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar"

cat("Reading data_ready.dta (selected columns only, to limit memory use)...\n")
needed_cols <- c("province_historical", "analysis_birth_year", "year", "gender", "urban_rural",
                  "weight", "log_undergrad_exposure_18_20",
                  "overed_horiz_mismatched", "both_mismatch_measures_available",
                  "horizontal_mismatch_broad", "horizontal_mismatch_eligible")
dt <- as.data.table(read_dta(path, col_select = tidyselect::all_of(needed_cols)))
cat("Rows:", format(nrow(dt), big.mark=","), "\n")

dt[, exposure := log_undergrad_exposure_18_20]
dt[, cohort := analysis_birth_year]
cat("gender values:", paste(unique(dt$gender), collapse=" | "), "\n")
cat("urban_rural values:", paste(unique(dt$urban_rural), collapse=" | "), "\n")

dt[, female := fcase(gender == 1, 1L, gender == 2, 0L, default = NA_integer_)]
dt[, urban := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]

# time-invariant province exposure ranking (average across sample), median split
prov_exp <- dt[!is.na(exposure), .(mean_exposure = mean(exposure, na.rm = TRUE)), by = province_historical]
med_exp <- median(prov_exp$mean_exposure)
prov_exp[, high_exposure := as.integer(mean_exposure > med_exp)]
cat("\nProvince high/low exposure split (median =", round(med_exp,3), "):\n")
print(prov_exp[order(-mean_exposure)])

dt <- merge(dt, prov_exp[, .(province_historical, high_exposure)], by = "province_historical")

# reference cohort: earliest cohort with reasonable N
cohort_counts <- dt[!is.na(cohort), .N, by = cohort][order(cohort)]
cat("\nCohort range:", min(cohort_counts$cohort), "-", max(cohort_counts$cohort), "\n")
ref_cohort <- min(cohort_counts$cohort)

run_event_study <- function(outcome_var, sample_filter_expr, label, fname) {
  cat("\n\n=== Event study:", label, "===\n")
  sub <- dt[eval(sample_filter_expr) & !is.na(get(outcome_var)) & !is.na(exposure) &
              !is.na(cohort) & !is.na(province_historical) & !is.na(year) &
              !is.na(female) & !is.na(urban) & !is.na(weight) & !is.na(high_exposure)]
  cat("N =", format(nrow(sub), big.mark=","), "\n")

  fml <- as.formula(paste0(
    outcome_var, " ~ i(cohort, high_exposure, ref = ", ref_cohort, ") + female + urban | ",
    "province_historical + cohort + year"
  ))

  m <- feols(fml, data = sub, weights = ~weight, cluster = ~province_historical + cohort)
  print(summary(m))
  saveRDS(m, file.path(out_dir, "gender_selection_results", paste0("event_study_", fname, ".rds")))
  capture.output(summary(m), file = file.path(out_dir, "gender_selection_results", paste0("event_study_", fname, ".txt")))

  png(file.path(out_dir, paste0("figure_event_study_", fname, ".png")), width = 2000, height = 1100, res = 200)
  iplot(m, xlab = "Birth cohort", main = paste0("Event study: ", label, "\n(high- vs low-exposure provinces, ref. cohort ", ref_cohort, ")"),
        ci.lty = 1, pt.pch = 16)
  abline(h = 0, lty = 2, col = "grey40")
  dev.off()
  cat("Saved figure_event_study_", fname, ".png\n", sep="")

  m
}

m_double <- run_event_study("overed_horiz_mismatched",
                             quote(both_mismatch_measures_available == 1),
                             "Double mismatch (overeducated & horizontally mismatched)",
                             "double_mismatch")

m_horiz <- run_event_study("horizontal_mismatch_broad",
                            quote(horizontal_mismatch_eligible == 1),
                            "Horizontal mismatch (broad crosswalk)",
                            "horizontal_mismatch")

cat("\n\nDONE.\n")
