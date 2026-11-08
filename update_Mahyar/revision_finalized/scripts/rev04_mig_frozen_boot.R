# rev04 -- migration/Table6 verification, frozen occ benchmark, wild bootstrap
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=220, scipen=999)
CF <- 100*log(1.10); OUT <- "C:/MismatchRev/out"
set.seed(20260909)

cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","gender","employed","age",
 "education_rank","tertiary_educated","isco_2digit","occupation_code_digits",
 "vertical_fullperiod_eligible","overeducated_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad","horiz_mismatch_detail_strict",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined",
 "recent_cross_hist_region_move","current_previous_province_diff","recent_interprovincial_migrant")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]

est <- function(dat,y,rhs="logexp + female + urban",fe="prov + cohort + yr"){
  m <- feols(as.formula(sprintf("%s ~ %s | %s",y,rhs,fe)), dat, weights=~w, cluster=~prov+cohort, notes=FALSE, warn=FALSE)
  ct <- coeftable(m)["logexp",]
  data.table(effect_pp=round(CF*ct[1],3), se_pp=round(CF*ct[2],3), p=signif(ct[4],3), N=m$nobs)
}

########## 1. MIGRATION / TABLE 6 ##########
S_h <- d[employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
S_j <- d[both_refined_mismatch_available==1L]
cat("migration flag coverage (joint sample):\n")
print(S_j[, .(.N,
   cross_hist_move = sum(recent_cross_hist_region_move==1L, na.rm=TRUE),
   prev_prov_diff  = sum(current_previous_province_diff==1L, na.rm=TRUE),
   interprov_mig   = sum(recent_interprovincial_migrant==1L, na.rm=TRUE),
   na_crosshist    = sum(is.na(recent_cross_hist_region_move)))])
mig <- rbindlist(list(
 cbind(spec="Baseline", outcome="Horizontal", est(S_h,"horiz_mismatch_detail_broad")),
 cbind(spec="Baseline", outcome="Double",     est(S_j,"overed_horiz_mism_refined")),
 cbind(spec="Baseline", outcome="Fully matched", est(S_j,"fully_matched_refined")),
 cbind(spec="Excl. cross-hist movers", outcome="Horizontal", est(S_h[recent_cross_hist_region_move==0L | is.na(recent_cross_hist_region_move)],"horiz_mismatch_detail_broad")),
 cbind(spec="Excl. cross-hist movers", outcome="Double",     est(S_j[recent_cross_hist_region_move==0L | is.na(recent_cross_hist_region_move)],"overed_horiz_mism_refined")),
 cbind(spec="Excl. cross-hist movers", outcome="Fully matched", est(S_j[recent_cross_hist_region_move==0L | is.na(recent_cross_hist_region_move)],"fully_matched_refined")),
 cbind(spec="Excl. Tehran", outcome="Horizontal", est(S_h[prov!="Tehran"],"horiz_mismatch_detail_broad")),
 cbind(spec="Excl. Tehran", outcome="Double",     est(S_j[prov!="Tehran"],"overed_horiz_mism_refined")),
 cbind(spec="Excl. Tehran", outcome="Fully matched", est(S_j[prov!="Tehran"],"fully_matched_refined")),
 cbind(spec="Excl. Tehran + movers", outcome="Horizontal", est(S_h[prov!="Tehran" & (recent_cross_hist_region_move==0L|is.na(recent_cross_hist_region_move))],"horiz_mismatch_detail_broad")),
 cbind(spec="Excl. Tehran + movers", outcome="Double",     est(S_j[prov!="Tehran" & (recent_cross_hist_region_move==0L|is.na(recent_cross_hist_region_move))],"overed_horiz_mism_refined")),
 cbind(spec="Excl. Tehran + movers", outcome="Fully matched", est(S_j[prov!="Tehran" & (recent_cross_hist_region_move==0L|is.na(recent_cross_hist_region_move))],"fully_matched_refined"))
))
cat("\n=== MIGRATION / TABLE 6 (reproducible full-period benchmark) ===\n"); print(mig)
fwrite(mig, file.path(OUT,"rev04_migration.csv"))

########## 2. FROZEN OCCUPATION BENCHMARK ##########
# modal education_rank by isco_2digit using EARLY years only (1386-1391), applied to all obs
emp <- d[employed==1L & !is.na(isco_2digit) & isco_2digit!="" & !is.na(education_rank)]
early <- emp[yr <= 1391]
mode_fun <- function(v,wt){ a <- tapply(wt, v, sum); as.integer(names(a)[which.max(a)]) }
bench <- early[, .(req_frozen = mode_fun(education_rank, w), n_cell = .N), by=isco_2digit]
cat("\nfrozen benchmark: cells =", nrow(bench), "; min cell n =", min(bench$n_cell), "\n")
bench_ok <- bench[n_cell >= 200]
emp <- merge(emp, bench_ok[,.(isco_2digit,req_frozen)], by="isco_2digit", all.x=TRUE)
emp[, overed_frozen := fifelse(!is.na(req_frozen), as.integer(education_rank > req_frozen), NA_integer_)]
emp[, undered_frozen := fifelse(!is.na(req_frozen), as.integer(education_rank < req_frozen), NA_integer_)]
# join horizontal (broad) for double mismatch on tertiary sample
emp[, double_frozen := fifelse(tertiary_educated==1L & horizontal_detailed_eligible==1L &
        !is.na(horiz_mismatch_detail_broad) & !is.na(overed_frozen),
        as.integer(overed_frozen==1L & horiz_mismatch_detail_broad==1L), NA_integer_)]
emp[, fully_frozen := fifelse(tertiary_educated==1L & horizontal_detailed_eligible==1L &
        !is.na(horiz_mismatch_detail_broad) & !is.na(overed_frozen),
        as.integer(overed_frozen==0L & undered_frozen==0L & horiz_mismatch_detail_broad==0L), NA_integer_)]
frozen <- rbindlist(list(
  cbind(outcome="Overeducation (frozen early-year benchmark)", est(emp[!is.na(overed_frozen)],"overed_frozen")),
  cbind(outcome="Undereducation (frozen)",                     est(emp[!is.na(undered_frozen)],"undered_frozen")),
  cbind(outcome="Double mismatch (frozen vertical + broad horiz)", est(emp[!is.na(double_frozen)],"double_frozen")),
  cbind(outcome="Fully matched (frozen)",                      est(emp[!is.na(fully_frozen)],"fully_frozen"))
))
cat("\n=== FROZEN BENCHMARK (modal educ rank by ISCO-2 from survey years 1386-1391) ===\n"); print(frozen)
fwrite(frozen, file.path(OUT,"rev04_frozen_benchmark.csv"))

########## 3. WILD CLUSTER BOOTSTRAP (Rademacher, restricted null) ##########
wcb <- function(dat, y, clustervar, B=999){
  dat <- copy(dat)[!is.na(get(y))]
  f_full <- as.formula(sprintf("%s ~ logexp + female + urban | prov + cohort + yr", y))
  m0 <- feols(f_full, dat, weights=~w, notes=FALSE, warn=FALSE)
  t_obs <- coeftable(m0)["logexp","t value"]  # default iid t; we compare to boot dist
  ct <- summary(m0, cluster=~prov+cohort)$coeftable["logexp",]
  t_cl <- ct["Estimate"]/ct["Std. Error"]
  # restricted model: impose logexp coef = 0
  f_r <- as.formula(sprintf("%s ~ female + urban | prov + cohort + yr", y))
  mr <- feols(f_r, dat, weights=~w, notes=FALSE, warn=FALSE)
  dat[, res_r := resid(mr)]
  dat[, fit_r := predict(mr)]
  g <- dat[[clustervar]]; ug <- unique(g)
  tb <- numeric(B)
  for(b in 1:B){
    dsign <- sample(c(-1,1), length(ug), replace=TRUE)
    names(dsign) <- as.character(ug)
    yb <- dat$fit_r + dat$res_r * dsign[as.character(g)]
    dat[, yb := yb]
    mb <- feols(update(f_full, yb ~ .), dat, weights=~w, notes=FALSE, warn=FALSE)
    cb <- summary(mb, cluster=as.formula(paste0("~",clustervar)))$coeftable["logexp",]
    tb[b] <- cb["Estimate"]/cb["Std. Error"]
  }
  p <- (1 + sum(abs(tb) >= abs(t_cl))) / (B+1)
  data.table(cluster=clustervar, t_cluster=round(t_cl,3), boot_p=round(p,4), B=B)
}
S_o <- d[employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod)]
boot <- rbindlist(list(
  cbind(outcome="Horizontal mismatch", wcb(S_h,"horiz_mismatch_detail_broad","prov")),
  cbind(outcome="Horizontal mismatch", wcb(S_h,"horiz_mismatch_detail_broad","cohort")),
  cbind(outcome="Double mismatch",     wcb(S_j,"overed_horiz_mism_refined","prov")),
  cbind(outcome="Double mismatch",     wcb(S_j,"overed_horiz_mism_refined","cohort")),
  cbind(outcome="Fully matched",       wcb(S_j,"fully_matched_refined","prov")),
  cbind(outcome="Fully matched",       wcb(S_j,"fully_matched_refined","cohort")),
  cbind(outcome="Overeducation",       wcb(S_o,"overeducated_fullperiod","prov")),
  cbind(outcome="Overeducation",       wcb(S_o,"overeducated_fullperiod","cohort"))
))
cat("\n=== WILD CLUSTER BOOTSTRAP (Rademacher, imposed null, 999 reps) ===\n"); print(boot)
fwrite(boot, file.path(OUT,"rev04_wildboot.csv"))
cat("\nDONE rev04\n")
