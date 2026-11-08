# rev_setup.R  -- one-time: slim.dta -> slim.fst with analysis-ready derived vars
suppressPackageStartupMessages({library(haven); library(data.table); library(fst)})
setDTthreads(0)
t0 <- Sys.time()
d <- as.data.table(read_dta("C:/MismatchRev/slim.dta"))
cat("loaded", nrow(d), "x", ncol(d), "in", round(difftime(Sys.time(),t0,units="secs")), "s\n")

# strip haven labels -> plain
d[, (names(d)) := lapply(.SD, function(x){ if(inherits(x,"haven_labelled")) as.numeric(x) else x })]

# derived, mirroring code_baseline.R / pipeline conventions
d[, female  := fifelse(gender==1L,1L, fifelse(gender==2L,0L,NA_integer_))]
d[, male    := fifelse(gender==2L,1L, fifelse(gender==1L,0L,NA_integer_))]
d[, urban   := fifelse(urban_rural=="Urban",1L, fifelse(urban_rural=="Rural",0L,NA_integer_))]
d[, cohort  := as.integer(analysis_birth_year)]
d[, prov    := as.character(province_historical)]
d[, yr      := as.integer(year)]
d[, w       := as.numeric(weight)]
d[, logexp  := as.numeric(log_undergrad_exposure_18_20)]

# baseline analysis sample (complete 3-yr exposure + valid cohort 1355-1383 + valid weight + geo)
d[, samp_main := as.integer(
      complete_undergrad_exposure==1L & is.finite(undergrad_exposure_18_20) &
      is.finite(w) & w>0 &
      is.finite(cohort) & cohort>=1355L & cohort<=1383L &
      !is.na(prov) & !is.na(yr) & !is.na(gender) & !is.na(urban))]

cat("samp_main N =", d[samp_main==1L,.N], "\n")
cat("provinces =", d[samp_main==1L, uniqueN(prov)], " cohorts =", d[samp_main==1L, uniqueN(cohort)],
    " years =", d[samp_main==1L, uniqueN(yr)], "\n")

# conversion factor for a 10% increase
CF <- 100*log(1.10)
cat("conversion factor 100*ln(1.1) =", CF, "\n")

write_fst(d, "C:/MismatchRev/slim.fst", compress=60)
cat("wrote slim.fst; total", round(difftime(Sys.time(),t0,units="mins"),1), "min\n")
