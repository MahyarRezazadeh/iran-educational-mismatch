suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=200)
CF <- 100*log(1.10)
cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","gender","employed",
 "tertiary_educated",
 "vertical_fullperiod_eligible","vert_fullperiod_reliable_cell","occ_observations_fullperiod",
 "overeducated_fullperiod","undereducated_fullperiod","vertical_mismatch_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","both_mismatch_measures_available",
 "overed_horiz_mism_refined","fully_matched_refined","overed_horiz_mismatched")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]

cat("--- vert_fullperiod_reliable_cell tab (employed & fp-eligible) ---\n")
print(d[employed==1L & vertical_fullperiod_eligible==1L, table(vert_fullperiod_reliable_cell, useNA="ifany")])
cat("--- occ_observations_fullperiod quantiles ---\n")
print(quantile(d[employed==1L & vertical_fullperiod_eligible==1L, occ_observations_fullperiod], c(0,.01,.05,.1,.25,.5,1), na.rm=TRUE))

base <- d[samp_main==1L & employed==1L & vertical_fullperiod_eligible==1L &
          !is.na(overeducated_fullperiod)]
cat("\nN fp-eligible employed nonmiss overed_fp:", nrow(base), "\n")
for(thr in c(0,20,25,30,50,100)){
  s <- base[occ_observations_fullperiod>=thr | is.na(occ_observations_fullperiod)]
  s2<- base[occ_observations_fullperiod>=thr]
  cat(sprintf("thr>=%3d : N(incl NA)=%d  N(excl NA)=%d\n", thr, nrow(s), nrow(s2)))
}
cat("target Final preferred overeducation pooled N = 1619190 ; full-period N = 1633647\n\n")

# reliable_cell==1 restriction
r <- base[vert_fullperiod_reliable_cell==1L]
cat("overed_fp | reliable_cell==1 : N =", nrow(r), "\n")
m <- feols(overeducated_fullperiod ~ logexp + female + urban | prov+cohort+yr, r, weights=~w, cluster=~prov+cohort, notes=FALSE)
cat("  effect_pp =", round(CF*coef(m)["logexp"],4), "\n")

# joint refined vs full-period counts
cat("\nboth_refined_mismatch_available==1 :", d[both_refined_mismatch_available==1L,.N], "\n")
cat("both_mismatch_measures_available==1 :", d[both_mismatch_measures_available==1L,.N], "\n")
cat("refined & reliable_cell==1          :", d[both_refined_mismatch_available==1L & vert_fullperiod_reliable_cell==1L,.N], "\n")
dj <- d[both_refined_mismatch_available==1L & vert_fullperiod_reliable_cell==1L & !is.na(overed_horiz_mism_refined)]
cat("double refined&reliable N =", nrow(dj), "(target 392777)\n")
m2 <- feols(overed_horiz_mism_refined ~ logexp + female + urban | prov+cohort+yr, dj, weights=~w, cluster=~prov+cohort, notes=FALSE)
cat("  double effect_pp =", round(CF*coef(m2)["logexp"],4), " p=", signif(coeftable(m2)["logexp",4],3), "\n")
