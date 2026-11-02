# rev06_r2.R -- second-round corrections: age restrictions, residualized magnitude, proper gender interaction
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=230, scipen=999)
CF <- 100*log(1.10); OUT <- "C:/MismatchRev/out"

cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","gender","age","employed",
 "tertiary_educated","education_rank",
 "labor_force_participation","unemployed",
 "vertical_fullperiod_eligible","overeducated_fullperiod","undereducated_fullperiod","vertical_mismatch_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]
d[, cc := cohort - 1369L]
d[, femf := factor(female)]

grab <- function(m, term="logexp"){
  ct <- tryCatch(coeftable(m)[term,], error=function(e) rep(NA_real_,4))
  data.table(eff_pp=round(CF*ct[1],3), se_pp=round(CF*ct[2],3), p=signif(ct[4],3), N=tryCatch(m$nobs,error=function(e)NA))
}
fitb <- function(dat,y,rhs="logexp + female + urban",fe="prov + cohort + yr"){
  tryCatch(feols(as.formula(sprintf("%s ~ %s | %s",y,rhs,fe)), dat, weights=~w, cluster=~prov+cohort, notes=FALSE, warn=FALSE), error=function(e) NULL)
}

## outcome -> (sample subset expression)
outs <- list(
 list(y="tertiary_educated", lbl="P(tertiary)", S=quote(TRUE)),
 list(y="education_rank",     lbl="Education rank", S=quote(TRUE)),
 list(y="labor_force_participation", lbl="LFP", S=quote(!is.na(labor_force_participation))),
 list(y="employed",           lbl="Employment", S=quote(!is.na(employed))),
 list(y="unemployed",         lbl="Unemployment", S=quote(labor_force_participation==1L & !is.na(unemployed))),
 list(y="overeducated_fullperiod",  lbl="Overeducation", S=quote(employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod))),
 list(y="undereducated_fullperiod", lbl="Undereducation", S=quote(employed==1L & vertical_fullperiod_eligible==1L & !is.na(undereducated_fullperiod))),
 list(y="vertical_mismatch_fullperiod", lbl="Any vertical mismatch", S=quote(employed==1L & vertical_fullperiod_eligible==1L & !is.na(vertical_mismatch_fullperiod))),
 list(y="horiz_mismatch_detail_broad", lbl="Horizontal mismatch", S=quote(employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad))),
 list(y="overed_horiz_mism_refined",  lbl="Double mismatch", S=quote(both_refined_mismatch_available==1L)),
 list(y="fully_matched_refined",      lbl="Fully matched employment", S=quote(both_refined_mismatch_available==1L))
)

########################################################################
## PHASE 2: age-restriction robustness table
########################################################################
age_cuts <- list("Baseline (all ages)"=0L, "Age >= 21"=21L, "Age >= 25"=25L, "Age >= 30"=30L)
resA <- list()
for(o in outs){
  base <- d[eval(o$S)]
  for(nm in names(age_cuts)){
    s <- base[age >= age_cuts[[nm]]]
    m <- fitb(s, o$y)
    resA[[length(resA)+1]] <- cbind(outcome=o$lbl, age_sample=nm, grab(m))
  }
}
A <- rbindlist(resA)
fwrite(A, file.path(OUT,"rev06_age_restrictions.csv")); cat("=== PHASE 2: age restrictions ===\n"); print(A)

########################################################################
## PHASE 7: residualized-exposure magnitudes
########################################################################
wsd <- function(x,w){ m<-sum(x*w)/sum(w); sqrt(sum(w*(x-m)^2)/sum(w)) }
wq <- function(x,w,probs){ o<-order(x); cw<-cumsum(w[o])/sum(w); approx(cw, x[o], probs, rule=2)$y }
# residualize on baseline FE, weighted, within each outcome's estimation sample -- use the double-mismatch joint sample as the representative "central results" sample, plus the broad sample
mag_rows <- list()
for(tag in c("broad main sample","joint-mismatch sample","vertical-mismatch sample")){
  s <- switch(tag,
    "broad main sample" = d,
    "joint-mismatch sample" = d[both_refined_mismatch_available==1L],
    "vertical-mismatch sample" = d[employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod)])
  fe_res <- feols(logexp ~ 1 | prov + cohort + yr, s, weights=~w, notes=FALSE)
  s[, res_exp := resid(fe_res)]
  raw_sd <- s[, wsd(logexp,w)]; raw_iqr <- diff(s[, wq(logexp,w,c(.25,.75))])
  r_sd  <- s[, wsd(res_exp,w)]; r_iqr  <- diff(s[, wq(res_exp,w,c(.25,.75))])
  mag_rows[[tag]] <- data.table(sample=tag, raw_sd=round(raw_sd,3), raw_iqr=round(raw_iqr,3),
                                resid_sd=round(r_sd,3), resid_iqr=round(r_iqr,3),
                                resid_var_share=round((r_sd/raw_sd)^2,3))
}
M <- rbindlist(mag_rows)
cat("\n=== PHASE 7: exposure variance (raw vs residualized on prov+cohort+year FE) ===\n"); print(M)
# per-residualized-SD / IQR effects for central outcomes, using the joint sample residualized SD/IQR
rsd_joint  <- M[sample=="joint-mismatch sample", resid_sd]
riqr_joint <- M[sample=="joint-mismatch sample", resid_iqr]
rsd_vert   <- M[sample=="vertical-mismatch sample", resid_sd]
cent <- outs[c(6,9,10,11)]  # overed, horiz, double, fully
mag2 <- list()
for(o in cent){
  s <- d[eval(o$S)]
  m <- fitb(s, o$y)
  b <- coef(m)["logexp"]  # per-unit-log coefficient
  rs <- if(o$lbl=="Overeducation") rsd_vert else rsd_joint
  ri <- if(o$lbl=="Overeducation") M[sample=="vertical-mismatch sample", resid_iqr] else riqr_joint
  ym <- s[, sum(get(o$y)*w,na.rm=TRUE)/sum(w[!is.na(get(o$y))])]
  mag2[[o$lbl]] <- data.table(outcome=o$lbl,
     eff_10pct_pp = round(CF*b,3),
     eff_1residSD_pp = round(100*b*rs,3),
     eff_residIQR_pp = round(100*b*ri,3),
     outcome_mean = round(ym,3))
}
M2 <- rbindlist(mag2)
cat("\n=== PHASE 7: magnitudes using RESIDUALIZED exposure ===\n"); print(M2)
fwrite(M, file.path(OUT,"rev06_magnitude_variance.csv"))
fwrite(M2, file.path(OUT,"rev06_magnitude_effects.csv"))

########################################################################
## PHASE 8: properly specified pooled gender-interaction test
##   Y ~ logexp*female + urban*female | prov^female + cohort^female + yr^female
########################################################################
gi_outs <- outs[c(6,7,8,9,10,11,4,3)]  # overed, undered, anyvert, horiz, double, fully, emp, lfp
resG <- list()
for(o in gi_outs){
  s <- d[eval(o$S)]
  m <- tryCatch(feols(as.formula(sprintf(
        "%s ~ logexp + logexp:female + urban + urban:female | prov^female + cohort^female + yr^female", o$y)),
        data=s, weights=~w, cluster=~prov+cohort, notes=FALSE, warn=FALSE), error=function(e){message("GI fail ",o$lbl,": ",conditionMessage(e)); NULL})
  # also the men-only and women-only comparable regressions (same FE structure per gender)
  mm <- fitb(s[gender==2L], o$y, "logexp + urban")
  mw <- fitb(s[gender==1L], o$y, "logexp + urban")
  if(is.null(m)){ resG[[o$lbl]] <- data.table(outcome=o$lbl, male_sep=mm$eff_pp, female_sep=mw$eff_pp); next }
  ct <- coeftable(m)
  male_e <- ct["logexp",]; diff_e <- ct["logexp:female",]
  L <- tryCatch(fixest::hypotheses(m, "logexp + logexp:female = 0"), error=function(e) NULL)
  resG[[o$lbl]] <- data.table(outcome=o$lbl,
     male_interact_pp = round(CF*male_e[1],3),
     female_interact_pp = if(!is.null(L)) round(CF*L$estimate,3) else NA_real_,
     diff_Fm_pp = round(CF*diff_e[1],3),
     diff_se_pp = round(CF*diff_e[2],3),
     diff_p = signif(diff_e[4],4),
     male_sep_pp = mm$eff_pp, female_sep_pp = mw$eff_pp, N=m$nobs)
}
G <- rbindlist(resG, fill=TRUE)
cat("\n=== PHASE 8: properly specified gender interaction (Female x exposure, x FE, x urban) ===\n"); print(G)
fwrite(G, file.path(OUT,"rev06_gender_interaction_proper.csv"))
cat("\nDONE rev06\n")
