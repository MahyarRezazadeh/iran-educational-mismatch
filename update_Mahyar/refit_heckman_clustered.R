library(haven)
library(data.table)
library(fixest)

path    <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_sample.dta"
out_dir <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/gender_selection_results"

dt <- as.data.table(read_dta(path))
dt[, historical_province := province_historical]
dt[, urban := fcase(urban_rural == "Urban", 1L, urban_rural == "Rural", 0L, default = NA_integer_)]
dt[, tertiary := as.integer(tertiary_educated)]
dt[, exposure := log_undergrad_exposure_18_20]

run_heckman <- function(data_subset, label) {
  cat("\n---", label, "---\n")
  sel_data <- data_subset[!is.na(num_kids) & !is.na(exposure) & !is.na(tertiary) & !is.na(urban)]

  probit_fe <- feglm(
    employed ~ exposure + tertiary + urban + num_kids + num_kids:exposure |
      historical_province + cohort + year,
    data = sel_data, family = binomial(link = "probit"),
    weights = ~weight,
    cluster = ~historical_province + cohort
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

cat("\nWomen - selection (probit, clustered SE) coefficients:\n")
print(summary(heckman_women$probit))
cat("\nWomen - hours equation (with Inverse Mills Ratio):\n")
print(summary(heckman_women$hours))

cat("\nMen - selection (probit, clustered SE) coefficients:\n")
print(summary(heckman_men$probit))
cat("\nMen - hours equation (with Inverse Mills Ratio):\n")
print(summary(heckman_men$hours))

capture.output(
  {
    cat("WOMEN selection equation (two-way clustered SE: province x cohort):\n"); print(summary(heckman_women$probit))
    cat("\nWOMEN hours equation:\n"); print(summary(heckman_women$hours))
    cat("\n\nMEN selection equation (two-way clustered SE: province x cohort):\n"); print(summary(heckman_men$probit))
    cat("\nMEN hours equation:\n"); print(summary(heckman_men$hours))
  },
  file = file.path(out_dir, "model3_heckman_by_gender.txt")
)

saveRDS(
  list(women = heckman_women, men = heckman_men),
  file.path(out_dir, "model3_heckman_by_gender.rds")
)

cat("\n\nDONE. Overwrote model3_heckman_by_gender.{rds,txt} with clustered-SE version.\n")
