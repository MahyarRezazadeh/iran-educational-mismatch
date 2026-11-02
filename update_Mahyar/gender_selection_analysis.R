# ==============================================================================
# Gender gap and selection-into-employment analysis
#
# Research question: as university exposure rises, does the gender gap in
# employment/hours widen because employment becomes more SELECTIVE for women
# (i.e., only increasingly well-educated women work), rather than because
# women's employment fails to respond to education gains?
#
# Data: update_Mahyar/gender_selection_sample.dta (built by
# build_gender_selection_sample.R from the paper's verified pipeline output).
#
# Models:
#  1. Triple-interaction linear probability model of employment:
#     employed ~ exposure*female*tertiary + controls | province+cohort+year
#  2. Same for weekly hours, conditional on employment.
#  3. Selection-margin model: does employment increasingly select on
#     education for women as exposure rises?
#     education_rank ~ employed*exposure*female + controls | FE
#  4. Two-step Heckman (feglm probit selection + Inverse Mills Ratio),
#     estimated separately by gender, with number of children as the
#     exclusion restriction, interacted with exposure to test whether the
#     selection correction becomes more important for women where exposure
#     is high.
#
# All models: survey-weighted, province+cohort+year FE, two-way clustered
# SE by province x cohort (matching the paper's baseline specification).
# ==============================================================================

library(haven)
library(data.table)
library(fixest)

path <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_sample.dta"
out_dir <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_results"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

cat("\nReading gender_selection_sample.dta...\n")
dt <- as.data.table(read_dta(path))
cat("Rows:", format(nrow(dt), big.mark = ","), " Cols:", ncol(dt), "\n")

dt[, historical_province := province_historical]
dt[, urban := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]
dt[, tertiary := as.integer(tertiary_educated)]
dt[, exposure := log_undergrad_exposure_18_20]

# ==============================================================================
# 1. Descriptives: employment rate & hours by gender x exposure quartile
# ==============================================================================

exposure_breaks <- unique(quantile(dt$exposure, probs = seq(0, 1, 0.25), na.rm = TRUE))
dt[, exposure_q := cut(exposure, breaks = exposure_breaks, include.lowest = TRUE, labels = FALSE)]

desc_by_q_gender <- dt[
  !is.na(exposure_q),
  .(
    emp_rate = weighted.mean(employed, weight, na.rm = TRUE),
    lfp_rate = weighted.mean(labor_force_participation, weight, na.rm = TRUE),
    mean_hours = weighted.mean(weekly_hours, weight, na.rm = TRUE),
    tertiary_share = weighted.mean(tertiary, weight, na.rm = TRUE),
    n = .N
  ),
  by = .(female, exposure_q)
][order(female, exposure_q)]

cat("\n--- Employment/hours/tertiary share by gender x exposure quartile ---\n")
print(desc_by_q_gender)
fwrite(desc_by_q_gender, file.path(out_dir, "desc_by_exposure_quartile_gender.csv"))

desc_by_province_gender <- dt[
  ,
  .(
    emp_rate = weighted.mean(employed, weight, na.rm = TRUE),
    mean_hours = weighted.mean(weekly_hours, weight, na.rm = TRUE),
    mean_exposure = weighted.mean(exposure, weight, na.rm = TRUE),
    tertiary_share = weighted.mean(tertiary, weight, na.rm = TRUE),
    n = .N
  ),
  by = .(female, historical_province)
][order(historical_province, female)]

cat("\n--- Employment/hours by gender x province (first 10 rows) ---\n")
print(head(desc_by_province_gender, 10))
fwrite(desc_by_province_gender, file.path(out_dir, "desc_by_province_gender.csv"))

# Selection margin descriptive: education rank among employed vs full eligible
# pool, by gender x exposure quartile.
selection_margin_desc <- dt[
  !is.na(exposure_q),
  .(
    edu_rank_all     = weighted.mean(education_rank, weight, na.rm = TRUE),
    edu_rank_employed = weighted.mean(fifelse(employed == 1L, education_rank, NA_real_), weight, na.rm = TRUE)
  ),
  by = .(female, exposure_q)
][, selection_gap := edu_rank_employed - edu_rank_all][order(female, exposure_q)]

cat("\n--- Selection margin (education rank: employed - all) by gender x exposure quartile ---\n")
print(selection_margin_desc)
fwrite(selection_margin_desc, file.path(out_dir, "selection_margin_by_quartile.csv"))


# ==============================================================================
# 2. Model 1: triple-interaction employment model
# ==============================================================================

cat("\n\n=== MODEL 1: Employment, triple interaction (exposure x female x tertiary) ===\n")

m1 <- feols(
  employed ~ exposure * female * tertiary + urban |
    historical_province + cohort + year,
  data = dt, weights = ~weight,
  cluster = ~historical_province + cohort
)
print(summary(m1))
saveRDS(m1, file.path(out_dir, "model1_employment_triple_interaction.rds"))
capture.output(summary(m1), file = file.path(out_dir, "model1_employment_triple_interaction.txt"))

cat("\n\n=== MODEL 1b: Weekly hours (conditional on employed), triple interaction ===\n")

m1b <- feols(
  weekly_hours ~ exposure * female * tertiary + urban |
    historical_province + cohort + year,
  data = dt[employed == 1L], weights = ~weight,
  cluster = ~historical_province + cohort
)
print(summary(m1b))
saveRDS(m1b, file.path(out_dir, "model1b_hours_triple_interaction.rds"))
capture.output(summary(m1b), file = file.path(out_dir, "model1b_hours_triple_interaction.txt"))


# ==============================================================================
# 3. Model 2: selection-margin model
#    Does employment increasingly select on education for women as exposure
#    rises? (education_rank ~ employed*exposure*female)
# ==============================================================================

cat("\n\n=== MODEL 2: Selection margin (education_rank ~ employed x exposure x female) ===\n")

m2 <- feols(
  education_rank ~ employed * exposure * female + urban |
    historical_province + cohort + year,
  data = dt, weights = ~weight,
  cluster = ~historical_province + cohort
)
print(summary(m2))
saveRDS(m2, file.path(out_dir, "model2_selection_margin.rds"))
capture.output(summary(m2), file = file.path(out_dir, "model2_selection_margin.txt"))


# ==============================================================================
# 4. Model 3: two-step Heckman selection correction, by gender
#    Exclusion restriction: num_kids (affects employment decision via
#    childcare demands, not hours conditional on already having decided to
#    work) interacted with exposure.
# ==============================================================================

cat("\n\n=== MODEL 3: Two-step Heckman selection correction, by gender ===\n")

run_heckman <- function(data_subset, label) {

  cat("\n---", label, "---\n")

  sel_data <- data_subset[!is.na(num_kids) & !is.na(exposure) & !is.na(tertiary) & !is.na(urban)]

  probit_fe <- feglm(
    employed ~ exposure + tertiary + urban + num_kids + num_kids:exposure |
      historical_province + cohort + year,
    data = sel_data, family = binomial(link = "probit"),
    weights = ~weight
  )

  lin_pred <- predict(probit_fe, type = "link")
  sel_data[, xb := lin_pred]
  sel_data[, mills := dnorm(xb) / pnorm(xb)]

  outcome_data <- sel_data[employed == 1L & is.finite(mills)]

  hours_model <- feols(
    weekly_hours ~ exposure + tertiary + urban + mills |
      historical_province + cohort + year,
    data = outcome_data, weights = ~weight,
    cluster = ~historical_province + cohort
  )

  list(probit = probit_fe, hours = hours_model, n_selection = nrow(sel_data), n_outcome = nrow(outcome_data))
}

heckman_women <- run_heckman(dt[female == 1L], "WOMEN")
heckman_men   <- run_heckman(dt[female == 0L], "MEN")

cat("\nWomen - selection (probit) coefficients:\n")
print(summary(heckman_women$probit))
cat("\nWomen - hours equation (with Inverse Mills Ratio):\n")
print(summary(heckman_women$hours))

cat("\nMen - selection (probit) coefficients:\n")
print(summary(heckman_men$probit))
cat("\nMen - hours equation (with Inverse Mills Ratio):\n")
print(summary(heckman_men$hours))

capture.output(
  {
    cat("WOMEN selection equation:\n"); print(summary(heckman_women$probit))
    cat("\nWOMEN hours equation:\n"); print(summary(heckman_women$hours))
    cat("\n\nMEN selection equation:\n"); print(summary(heckman_men$probit))
    cat("\nMEN hours equation:\n"); print(summary(heckman_men$hours))
  },
  file = file.path(out_dir, "model3_heckman_by_gender.txt")
)

saveRDS(
  list(women = heckman_women, men = heckman_men),
  file.path(out_dir, "model3_heckman_by_gender.rds")
)

cat("\n\nDONE. All results saved to:\n", out_dir, "\n")
