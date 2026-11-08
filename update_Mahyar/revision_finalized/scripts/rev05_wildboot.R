# rev05 -- wild cluster bootstrap (Webb 6-point weights, imposed null), fixed
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=200, scipen=999)
CF <- 100*log(1.10); OUT <- "C:/MismatchRev/out"
set.seed(20260909)

cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","gender","employed",
 "vertical_fullperiod_eligible","overeducated_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]

webb <- c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2))

wcb <- function(dat, y, clustervar, B=999){
  dat <- dat[!is.na(get(y))]
  yv <- dat[[y]]
  X  <- dat[, .(logexp, female, urban)]
  fe_full <- ~ prov + cohort + yr
  cl <- as.formula(paste0("~", clustervar))
  f1 <- as.formula(sprintf("%s ~ logexp + female + urban | prov + cohort + yr", y))
  f0 <- as.formula(sprintf("%s ~ female + urban | prov + cohort + yr", y))
  m1 <- feols(f1, data=dat, weights=~w, notes=FALSE, warn=FALSE)
  s1 <- summary(m1, cluster=cl)$coeftable["logexp",]
  t_obs <- unname(s1["Estimate"]/s1["Std. Error"])
  # restricted fit (impose logexp=0)
  m0 <- feols(f0, data=dat, weights=~w, notes=FALSE, warn=FALSE)
  fit0 <- predict(m0); res0 <- resid(m0)
  g <- dat[[clustervar]]; ug <- unique(g); ng <- length(ug)
  dat[, `:=`(fit0=fit0, res0=res0)]
  tb <- numeric(B)
  for(b in 1:B){
    ww <- sample(webb, ng, replace=TRUE); names(ww) <- as.character(ug)
    dat[, yb := fit0 + res0*ww[as.character(g)]]
    mb <- feols(yb ~ logexp + female + urban | prov + cohort + yr, data=dat, weights=~w, notes=FALSE, warn=FALSE)
    sb <- summary(mb, cluster=cl)$coeftable["logexp",]
    tb[b] <- unname(sb["Estimate"]/sb["Std. Error"])
  }
  data.table(cluster=clustervar, n_clusters=ng, t_obs=round(t_obs,3),
             boot_p=round((1+sum(abs(tb)>=abs(t_obs)))/(B+1),4), B=B)
}

S_o <- d[employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod)]
S_h <- d[employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
S_j <- d[both_refined_mismatch_available==1L]

jobs <- list(
  list(S=S_o, y="overeducated_fullperiod",   lbl="Overeducation"),
  list(S=S_h, y="horiz_mismatch_detail_broad",lbl="Horizontal mismatch"),
  list(S=S_j, y="overed_horiz_mism_refined",  lbl="Double mismatch"),
  list(S=S_j, y="fully_matched_refined",      lbl="Fully matched employment")
)
res <- list()
for(j in jobs) for(cv in c("prov","cohort")){
  cat("running", j$lbl, cv, "...\n"); flush.console()
  res[[length(res)+1]] <- cbind(outcome=j$lbl, wcb(j$S, j$y, cv))
}
R <- rbindlist(res)
print(R); fwrite(R, file.path(OUT,"rev05_wildboot.csv"))
cat("DONE rev05\n")
