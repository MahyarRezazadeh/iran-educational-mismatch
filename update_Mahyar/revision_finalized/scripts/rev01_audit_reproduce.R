# rev01_audit_reproduce.R -- verify manuscript Tables 1 & 2 against slim.fst
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4)
options(width=200)
CF <- 100*log(1.10)

cols <- c("logexp","w","female","male","urban","cohort","prov","yr","samp_main","gender",
  "undergrad_exposure_18_20",
  "labor_force_participation","employed","unemployed",
  "overeducated_fullperiod","undereducated_fullperiod","vertically_matched_fullperiod",
  "vertical_mismatch_fullperiod","vertical_fullperiod_eligible","vert_fullperiod_reliable_cell",
  "overeducated","undereducated","any_vertical_mismatch","vertical_mismatch_eligible","vertical_mismatch_reliable_cell",
  "horizontal_mismatch_eligible","horizontal_mismatch_broad","horizontal_mismatch_strict",
  "horizontal_detailed_eligible","horiz_mismatch_detail_broad","horiz_mismatch_detail_strict",
  "both_mismatch_measures_available","overed_horiz_mismatched","fully_matched_strict",
  "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))
d <- d[samp_main==1L]
cat("rows:", nrow(d), "\n")

fit <- function(dat, y, extra_ctrl="female + urban"){
  f <- as.formula(sprintf("%s ~ logexp + %s | prov + cohort + yr", y, extra_ctrl))
  m <- feols(f, data=dat, weights=~w, cluster=~prov+cohort, notes=FALSE)
  ct <- coeftable(m)["logexp",]
  data.table(coef=ct[1], se=ct[2], p=ct[4], effect_pp=CF*ct[1], se_pp=CF*ct[2],
             N=m$nobs, ymean=weighted.mean(dat[[y]], dat$w, na.rm=TRUE))
}
row <- function(lbl, dat, y){
  r <- rbind(
    cbind(grp="Pooled", fit(dat, y, "female + urban")),
    cbind(grp="Women",  fit(dat[gender==1L], y, "urban")),
    cbind(grp="Men",    fit(dat[gender==2L], y, "urban")))
  r[, outcome:=lbl][]
}

res <- list()

## ---- Table 1: employment quantity ----
res$lfp  <- row("LFP",          d[!is.na(labor_force_participation)], "labor_force_participation")
res$emp  <- row("Employment",   d[!is.na(employed)],                  "employed")
res$unemp<- row("Unemployment", d[labor_force_participation==1L & !is.na(unemployed)], "unemployed")

## ---- Table 2: mismatch ----
# Final preferred vertical = full-period benchmark + reliable cell
dv <- d[vertical_fullperiod_eligible==1L & vert_fullperiod_reliable_cell==1L]
res$overed  <- row("Overeducation (pref)",  dv[!is.na(overeducated_fullperiod)],  "overeducated_fullperiod")
res$undered <- row("Undereducation (pref)", dv[!is.na(undereducated_fullperiod)], "undereducated_fullperiod")
res$anyvert <- row("Any vertical (pref)",   dv[!is.na(vertical_mismatch_fullperiod)], "vertical_mismatch_fullperiod")

# Horizontal broad -- try two eligibility bases
dh1 <- d[horizontal_mismatch_eligible==1L & !is.na(horizontal_mismatch_broad)]
dh2 <- d[horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
res$horiz_a <- row("Horiz broad [elig/broad]",  dh1, "horizontal_mismatch_broad")
res$horiz_b <- row("Horiz broad [detailed]",    dh2, "horiz_mismatch_detail_broad")

# Double mismatch / fully matched -- refined (preferred)
dj <- d[both_refined_mismatch_available==1L]
res$double_ref <- row("Double mismatch (refined)", dj[!is.na(overed_horiz_mism_refined)], "overed_horiz_mism_refined")
res$fully_ref  <- row("Fully matched (refined)",   dj[!is.na(fully_matched_refined)],     "fully_matched_refined")
# full-period (unrestricted) joint
dj2 <- d[both_mismatch_measures_available==1L]
res$double_fp <- row("Double mismatch (full-period)", dj2[!is.na(overed_horiz_mismatched)], "overed_horiz_mismatched")
res$fully_fp  <- row("Fully matched (strict/fp)",     dj2[!is.na(fully_matched_strict)],    "fully_matched_strict")

out <- rbindlist(res)
setcolorder(out, c("outcome","grp","effect_pp","se_pp","p","N","ymean","coef","se"))
out[, `:=`(effect_pp=round(effect_pp,3), se_pp=round(se_pp,3), p=signif(p,3),
           ymean=round(ymean,4), coef=signif(coef,4), se=signif(se,4))]
print(out)
fwrite(out, "C:/MismatchRev/out/rev01_reproduce.csv")
