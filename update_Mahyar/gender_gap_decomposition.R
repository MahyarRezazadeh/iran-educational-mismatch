library(haven)
library(data.table)
library(fixest)

path    <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_sample.dta"
out_dir <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_results"
models  <- readRDS(file.path(out_dir, "model3_heckman_by_gender.rds"))

dt <- as.data.table(read_dta(path))
dt[, historical_province := province_historical]
dt[, urban := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]
dt[, tertiary := as.integer(tertiary_educated)]
dt[, exposure := log_undergrad_exposure_18_20]

rebuild_sel <- function(data_subset) {
  data_subset[!is.na(num_kids) & !is.na(exposure) & !is.na(tertiary) & !is.na(urban)]
}

sel_w <- rebuild_sel(dt[female == 1L])
sel_m <- rebuild_sel(dt[female == 0L])

# ==============================================================================
# PART A: participation-margin (employment-rate) counterfactual, women's model
#   Ask: holding women's own province/cohort/year context (i.e. the fixed
#   effects already embedded in her fitted index) and her own characteristics
#   fixed, what would her predicted employment probability be if the SLOPES
#   on (exposure, tertiary, urban, num_kids, exposure:num_kids) matched men's
#   estimated slopes instead of women's own estimated slopes?
# ==============================================================================

cf_w <- copy(sel_w)
xb_actual_w <- predict(models$women$probit, newdata = cf_w, type = "link")

b_w <- coef(models$women$probit)
b_m <- coef(models$men$probit)
regs <- c("exposure", "tertiary", "urban", "num_kids")

Xw <- as.matrix(cf_w[, ..regs])
Xw <- cbind(Xw, "exposure:num_kids" = cf_w$exposure * cf_w$num_kids)

own_contrib_w   <- as.numeric(Xw %*% b_w[colnames(Xw)])
mens_contrib_w  <- as.numeric(Xw %*% b_m[colnames(Xw)])

xb_cf_w <- xb_actual_w - own_contrib_w + mens_contrib_w

cf_w[, `:=`(p_actual = pnorm(xb_actual_w), p_cf = pnorm(xb_cf_w))]

emp_rate_women_actual   <- weighted.mean(cf_w$p_actual, cf_w$weight)
emp_rate_women_actual_raw <- weighted.mean(cf_w$employed, cf_w$weight)
emp_rate_women_cf        <- weighted.mean(cf_w$p_cf, cf_w$weight)
emp_rate_men_actual_raw  <- weighted.mean(sel_m$employed, sel_m$weight)

raw_gap        <- emp_rate_men_actual_raw - emp_rate_women_actual_raw
cf_closed      <- emp_rate_women_cf - emp_rate_women_actual
share_of_gap   <- cf_closed / raw_gap

cat("\n=== PART A: participation-margin counterfactual (women given men's slopes) ===\n")
cat(sprintf("Actual employment rate, women: %.4f (fitted: %.4f)\n", emp_rate_women_actual_raw, emp_rate_women_actual))
cat(sprintf("Actual employment rate, men:   %.4f\n", emp_rate_men_actual_raw))
cat(sprintf("Raw gender gap in employment rate (men - women): %.4f\n", raw_gap))
cat(sprintf("Counterfactual employment rate, women (men's slopes on exposure/tertiary/urban/num_kids, women's own FE+X otherwise): %.4f\n", emp_rate_women_cf))
cat(sprintf("Gap closed by counterfactual: %.4f percentage points (%.1f%% of raw gap)\n", cf_closed, 100 * share_of_gap))

# ==============================================================================
# PART B: Oaxaca-Blinder decomposition of the hours-worked gap among the
#   employed, using the Heckman-corrected hours equations (mills = selection
#   channel).
# ==============================================================================

xb_actual_m <- predict(models$men$probit, newdata = sel_m, type = "link")
sel_m[, mills := dnorm(xb_actual_m) / pnorm(xb_actual_m)]
cf_w[, mills := dnorm(xb_actual_w) / pnorm(xb_actual_w)]

out_w <- cf_w[employed == 1L & is.finite(mills)]
out_m <- sel_m[employed == 1L & is.finite(mills)]

hours_vars <- c("exposure", "tertiary", "urban", "mills")
Xbar_w <- sapply(hours_vars, function(v) weighted.mean(out_w[[v]], out_w$weight, na.rm = TRUE))
Xbar_m <- sapply(hours_vars, function(v) weighted.mean(out_m[[v]], out_m$weight, na.rm = TRUE))

beta_w <- coef(models$women$hours)[hours_vars]
beta_m <- coef(models$men$hours)[hours_vars]

hours_mean_w <- weighted.mean(out_w$weekly_hours, out_w$weight, na.rm = TRUE)
hours_mean_m <- weighted.mean(out_m$weekly_hours, out_m$weight, na.rm = TRUE)
raw_hours_gap <- hours_mean_m - hours_mean_w

endowment_k  <- (Xbar_m - Xbar_w) * beta_m
coefficient_k <- Xbar_w * (beta_m - beta_w)

decomp <- data.table(
  variable = hours_vars,
  Xbar_men = Xbar_m, Xbar_women = Xbar_w,
  beta_men = beta_m, beta_women = beta_w,
  endowment_component = endowment_k,
  coefficient_component = coefficient_k,
  total_component = endowment_k + coefficient_k
)

cat("\n\n=== PART B: Oaxaca-Blinder decomposition of the employed hours gap (men - women) ===\n")
cat(sprintf("Raw mean weekly hours, men (employed):   %.2f\n", hours_mean_m))
cat(sprintf("Raw mean weekly hours, women (employed): %.2f\n", hours_mean_w))
cat(sprintf("Raw hours gap (men - women): %.2f\n", raw_hours_gap))
print(decomp)

total_endow <- sum(endowment_k)
total_coef  <- sum(coefficient_k)
total_modeled <- total_endow + total_coef

cat(sprintf("\nSum of endowment (characteristics) components: %.3f\n", total_endow))
cat(sprintf("Sum of coefficient (differential-response/selection) components: %.3f\n", total_coef))
cat(sprintf("Total modeled gap (endowment+coefficient, 4 regressors only): %.3f (%.1f%% of raw gap; remainder reflects FE/residual not decomposed here)\n",
            total_modeled, 100 * total_modeled / raw_hours_gap))
cat(sprintf("Mills (selection) channel alone: endowment=%.3f, coefficient=%.3f, total=%.3f (%.1f%% of raw hours gap; %.1f%% of total modeled gap)\n",
            endowment_k["mills"], coefficient_k["mills"], endowment_k["mills"] + coefficient_k["mills"],
            100 * (endowment_k["mills"] + coefficient_k["mills"]) / raw_hours_gap,
            100 * (endowment_k["mills"] + coefficient_k["mills"]) / total_modeled))

fwrite(decomp, file.path(out_dir, "gender_gap_decomposition_hours.csv"))

results_summary <- data.table(
  quantity = c("emp_rate_women_actual","emp_rate_men_actual","raw_employment_gap",
               "emp_rate_women_counterfactual_mens_slopes","employment_gap_closed_by_cf",
               "share_of_employment_gap_closed",
               "hours_mean_men","hours_mean_women","raw_hours_gap",
               "hours_gap_mills_channel_total","share_hours_gap_from_mills"),
  value = c(emp_rate_women_actual_raw, emp_rate_men_actual_raw, raw_gap,
            emp_rate_women_cf, cf_closed, share_of_gap,
            hours_mean_m, hours_mean_w, raw_hours_gap,
            endowment_k["mills"] + coefficient_k["mills"],
            (endowment_k["mills"] + coefficient_k["mills"]) / raw_hours_gap)
)
fwrite(results_summary, file.path(out_dir, "gender_gap_decomposition_summary.csv"))

capture.output({
  cat("=== PART A: participation-margin counterfactual (women given men's slopes) ===\n")
  cat(sprintf("Actual employment rate, women: %.4f\n", emp_rate_women_actual_raw))
  cat(sprintf("Actual employment rate, men:   %.4f\n", emp_rate_men_actual_raw))
  cat(sprintf("Raw gender gap in employment rate (men - women): %.4f\n", raw_gap))
  cat(sprintf("Counterfactual employment rate, women (men's slopes): %.4f\n", emp_rate_women_cf))
  cat(sprintf("Gap closed by counterfactual: %.4f pp (%.1f%% of raw gap)\n", cf_closed, 100*share_of_gap))
  cat("\n=== PART B: Oaxaca-Blinder decomposition of employed hours gap ===\n")
  print(decomp)
  cat(sprintf("\nTotal modeled gap: %.3f (%.1f%% of raw hours gap of %.2f)\n", total_modeled, 100*total_modeled/raw_hours_gap, raw_hours_gap))
  cat(sprintf("Mills/selection channel: %.3f (%.1f%% of raw hours gap)\n", endowment_k["mills"]+coefficient_k["mills"], 100*(endowment_k["mills"]+coefficient_k["mills"])/raw_hours_gap))
}, file = file.path(out_dir, "gender_gap_decomposition_report.txt"))

cat("\n\nDONE.\n")
