# rev05c -- minimal wild cluster bootstrap: 3 joint outcomes, province clustering, 199 reps
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(scipen=999); set.seed(20260909)
CF <- 100*log(1.10)
cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","employed",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]
d[, `:=`(prov=factor(prov), cohort=factor(cohort), yr=factor(yr))]

wcb <- function(dat, y, cv, B=199){
  dat <- dat[!is.na(get(y))]
  cl <- as.formula(paste0("~",cv))
  f1 <- as.formula(sprintf("%s ~ logexp + female + urban | prov + cohort + yr", y))
  f0 <- as.formula(sprintf("%s ~ female + urban | prov + cohort + yr", y))
  m1 <- feols(f1, dat, weights=~w, notes=FALSE, warn=FALSE, mem.clean=TRUE)
  s1 <- summary(m1, cluster=cl)$coeftable["logexp",]; t_obs <- unname(s1[1]/s1[2])
  m0 <- feols(f0, dat, weights=~w, notes=FALSE, warn=FALSE, mem.clean=TRUE)
  f0hat <- as.numeric(predict(m0)); r0 <- as.numeric(resid(m0)); g <- as.character(dat[[cv]])
  ug <- unique(g); tb <- numeric(B)
  for(b in 1:B){
    ww <- sample(c(-1,1), length(ug), TRUE); names(ww) <- ug
    dat[, yb := f0hat + r0*ww[g]]
    mb <- feols(yb ~ logexp + female + urban | prov + cohort + yr, dat, weights=~w, notes=FALSE, warn=FALSE, mem.clean=TRUE)
    sb <- summary(mb, cluster=cl)$coeftable["logexp",]; tb[b] <- unname(sb[1]/sb[2])
    if(b %% 50 == 0){ cat(y, cv, b, "\n"); flush.console() }
  }
  data.table(outcome=y, cluster=cv, est_pp=round(CF*s1[1],3), boot_p=round((1+sum(abs(tb)>=abs(t_obs)))/(B+1),4), B=B)
}
S_h <- d[employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
S_j <- d[both_refined_mismatch_available==1L]
res <- rbindlist(list(
  wcb(S_h,"horiz_mismatch_detail_broad","prov"),
  wcb(S_j,"overed_horiz_mism_refined","prov"),
  wcb(S_j,"fully_matched_refined","prov"),
  wcb(S_h,"horiz_mismatch_detail_broad","cohort"),
  wcb(S_j,"overed_horiz_mism_refined","cohort"),
  wcb(S_j,"fully_matched_refined","cohort")
))
print(res); fwrite(res, "C:/MismatchRev/out/rev05_wildboot.csv"); cat("DONE\n")
