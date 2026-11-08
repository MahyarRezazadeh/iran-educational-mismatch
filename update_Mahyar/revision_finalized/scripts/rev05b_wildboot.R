# rev05b -- leaner wild cluster bootstrap (Rademacher, imposed null)
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=200, scipen=999)
CF <- 100*log(1.10); OUT <- "C:/MismatchRev/out"; set.seed(20260909)
cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","employed",
 "vertical_fullperiod_eligible","overeducated_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]

wcb <- function(dat, y, clustervar, B){
  dat <- dat[!is.na(get(y))]
  cl <- as.formula(paste0("~",clustervar))
  f1 <- as.formula(sprintf("%s ~ logexp + female + urban | prov + cohort + yr", y))
  f0 <- as.formula(sprintf("%s ~ female + urban | prov + cohort + yr", y))
  m1 <- feols(f1, dat, weights=~w, notes=FALSE, warn=FALSE)
  s1 <- summary(m1, cluster=cl)$coeftable["logexp",]
  t_obs <- unname(s1[1]/s1[2]); est_pp <- CF*s1[1]
  m0 <- feols(f0, dat, weights=~w, notes=FALSE, warn=FALSE)
  dat[, `:=`(f0=predict(m0), r0=resid(m0))]
  g <- dat[[clustervar]]; ug <- unique(g); ng <- length(ug)
  tb <- numeric(B)
  for(b in 1:B){
    ww <- sample(c(-1,1), ng, TRUE); names(ww) <- as.character(ug)
    dat[, yb := f0 + r0*ww[as.character(g)]]
    mb <- feols(yb ~ logexp + female + urban | prov + cohort + yr, dat, weights=~w, notes=FALSE, warn=FALSE)
    sb <- summary(mb, cluster=cl)$coeftable["logexp",]
    tb[b] <- unname(sb[1]/sb[2])
  }
  data.table(cluster=clustervar, n_clusters=ng, est_pp=round(est_pp,3),
             analytic_p=signif(s1[4],3), boot_p=round((1+sum(abs(tb)>=abs(t_obs)))/(B+1),4), B=B)
}
S_h <- d[employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
S_j <- d[both_refined_mismatch_available==1L]
S_o <- d[employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod)]
res <- list()
for(cv in c("prov","cohort")){
  cat(cv," horiz\n"); res[[length(res)+1]] <- cbind(outcome="Horizontal mismatch", wcb(S_h,"horiz_mismatch_detail_broad",cv,999))
  cat(cv," double\n"); res[[length(res)+1]] <- cbind(outcome="Double mismatch", wcb(S_j,"overed_horiz_mism_refined",cv,999))
  cat(cv," fully\n"); res[[length(res)+1]] <- cbind(outcome="Fully matched employment", wcb(S_j,"fully_matched_refined",cv,999))
  cat(cv," overed\n"); res[[length(res)+1]] <- cbind(outcome="Overeducation", wcb(S_o,"overeducated_fullperiod",cv,299))
}
R <- rbindlist(res); print(R); fwrite(R, file.path(OUT,"rev05_wildboot.csv")); cat("DONE\n")
