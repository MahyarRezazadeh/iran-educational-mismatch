# rev02_priority1.R -- Priority-1 identification analyses (referee sections 3,4,5,6,13,16)
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=220, scipen=999)
CF <- 100*log(1.10)
OUT <- "C:/MismatchRev/out"

cols <- c("logexp","w","female","male","urban","cohort","prov","yr","samp_main","gender","employed",
 "undergrad_exposure_18_20","tertiary_educated","education_rank","educ_level_num","literate",
 "labor_force_participation","unemployed",
 "vertical_fullperiod_eligible","overeducated_fullperiod","undereducated_fullperiod","vertical_mismatch_fullperiod",
 "horizontal_detailed_eligible","horiz_mismatch_detail_broad",
 "both_refined_mismatch_available","overed_horiz_mism_refined","fully_matched_refined",
 "recent_cross_hist_region_move")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]
d[, cc := cohort - 1369L]                    # centered cohort (approx mid-sample) for trends
setDT(d)

## tidy extractor
grab <- function(m, term="logexp"){
  ct <- tryCatch(coeftable(m)[term,], error=function(e) rep(NA_real_,4))
  data.table(coef=ct[1], se=ct[2], p=ct[4], effect_pp=CF*ct[1], se_pp=CF*ct[2], N=tryCatch(m$nobs, error=function(e) NA))
}
runspec <- function(dat, y, rhs, fe, label, spec, term="logexp"){
  f <- as.formula(sprintf("%s ~ %s | %s", y, rhs, fe))
  m <- tryCatch(feols(f, data=dat, weights=~w, cluster=~prov+cohort, notes=FALSE, warn=FALSE),
                error=function(e){message("FAIL ",label," ",spec,": ",conditionMessage(e)); NULL})
  if(is.null(m)) return(data.table(outcome=label, spec=spec, coef=NA,se=NA,p=NA,effect_pp=NA,se_pp=NA,N=NA))
  cbind(outcome=label, spec=spec, grab(m, term))
}

# sample definitions (reproducible full-period benchmark)
S_lm    <- d
S_emp   <- d[!is.na(employed)]
S_lfp   <- d[!is.na(labor_force_participation)]
S_unemp <- d[labor_force_participation==1L & !is.na(unemployed)]
S_vert  <- d[employed==1L & vertical_fullperiod_eligible==1L & !is.na(overeducated_fullperiod)]
S_horiz <- d[employed==1L & horizontal_detailed_eligible==1L & !is.na(horiz_mismatch_detail_broad)]
S_joint <- d[both_refined_mismatch_available==1L]

outcomes <- list(
  list(S=S_lfp,  y="labor_force_participation",    lbl="LFP"),
  list(S=S_emp,  y="employed",                      lbl="Employment"),
  list(S=S_unemp,y="unemployed",                    lbl="Unemployment"),
  list(S=S_vert, y="overeducated_fullperiod",       lbl="Overeducation"),
  list(S=S_vert, y="undereducated_fullperiod",      lbl="Undereducation"),
  list(S=S_vert, y="vertical_mismatch_fullperiod",  lbl="Any vertical mismatch"),
  list(S=S_horiz,y="horiz_mismatch_detail_broad",   lbl="Horizontal mismatch"),
  list(S=S_joint,y="overed_horiz_mism_refined",     lbl="Double mismatch"),
  list(S=S_joint,y="fully_matched_refined",         lbl="Fully matched employment")
)

########################################################################
## (A) BASELINE + robustness FE ladder  (sections 4 & 5)
########################################################################
resA <- list()
for(o in outcomes){
  S <- o$S; y <- o$y; lbl <- o$lbl
  resA[[paste0(lbl,"_base")]]   <- runspec(S, y, "logexp + female + urban", "prov + cohort + yr", lbl, "1. Baseline (prov+cohort+year FE)")
  resA[[paste0(lbl,"_pxy")]]    <- runspec(S, y, "logexp + female + urban", "prov^yr + cohort",   lbl, "2. + Province x survey-year FE")
  resA[[paste0(lbl,"_trend")]]  <- runspec(S, y, "logexp + female + urban", "prov + cohort + yr + prov[cc]", lbl, "3. + Province-specific linear cohort trend")
  resA[[paste0(lbl,"_both")]]   <- runspec(S, y, "logexp + female + urban", "prov^yr + cohort + prov[cc]",   lbl, "4. + Province x year FE AND cohort trend")
}
A <- rbindlist(resA)
A[, `:=`(effect_pp=round(effect_pp,3), se_pp=round(se_pp,3), p=signif(p,3), coef=signif(coef,5), se=signif(se,5))]
fwrite(A, file.path(OUT,"rev02A_fe_ladder.csv")); cat("=== A: FE ladder ===\n"); print(A)

########################################################################
## (B) FIRST STAGE: exposure -> educational attainment  (section 3)
########################################################################
resB <- list()
resB$tert  <- runspec(S_lm, "tertiary_educated", "logexp + female + urban", "prov + cohort + yr", "P(tertiary educated)", "broad sample")
resB$tertP <- runspec(S_lm, "tertiary_educated", "logexp + urban", "prov + cohort + yr", "P(tertiary), women", "women", term="logexp")
# gender split
resB$tertW <- cbind(outcome="P(tertiary educated)", spec="women",
                    grab(feols(tertiary_educated ~ logexp + urban | prov+cohort+yr, S_lm[gender==1L], weights=~w, cluster=~prov+cohort, notes=FALSE)))
resB$tertM <- cbind(outcome="P(tertiary educated)", spec="men",
                    grab(feols(tertiary_educated ~ logexp + urban | prov+cohort+yr, S_lm[gender==2L], weights=~w, cluster=~prov+cohort, notes=FALSE)))
resB$rank  <- runspec(S_lm, "education_rank", "logexp + female + urban", "prov + cohort + yr", "Education rank (1-8)", "broad sample")
resB$lit   <- runspec(S_lm, "literate", "logexp + female + urban", "prov + cohort + yr", "P(literate)", "broad sample")
# robustness of first stage to stronger FE
resB$tert_pxy   <- runspec(S_lm, "tertiary_educated", "logexp + female + urban", "prov^yr + cohort", "P(tertiary educated)", "+ prov x year FE")
resB$tert_trend <- runspec(S_lm, "tertiary_educated", "logexp + female + urban", "prov + cohort + yr + prov[cc]", "P(tertiary educated)", "+ cohort trend")
B <- rbindlist(resB, fill=TRUE)
B[, `:=`(effect_pp=round(effect_pp,3), se_pp=round(se_pp,3), p=signif(p,3), coef=signif(coef,5), se=signif(se,5),
         ymean=NA_real_)]
B[outcome=="P(tertiary educated)" & spec=="broad sample", ymean:=S_lm[,weighted.mean(tertiary_educated,w,na.rm=TRUE)]]
B[outcome=="Education rank (1-8)", ymean:=S_lm[,weighted.mean(education_rank,w,na.rm=TRUE)]]
fwrite(B, file.path(OUT,"rev02B_first_stage.csv")); cat("\n=== B: first stage ===\n"); print(B)

########################################################################
## (C) FORMAL GENDER INTERACTION TESTS  (section 13)
########################################################################
resC <- list()
gi_outcomes <- outcomes[c(4,5,7,8,9)]  # overed, undered, horiz, double, fully
for(o in gi_outcomes){
  m <- tryCatch(feols(as.formula(sprintf("%s ~ logexp*female + urban | prov + cohort + yr", o$y)),
              data=o$S, weights=~w, cluster=~prov+cohort, notes=FALSE), error=function(e) NULL)
  if(is.null(m)){ next }
  ct <- coeftable(m)
  male_e   <- ct["logexp",]                      # female==0 baseline (male)
  diff_e   <- ct["logexp:female",]
  # implied female effect via linear combination
  fem <- tryCatch({ lh <- summary(m, lean=FALSE); NULL}, error=function(e) NULL)
  L <- tryCatch(fixest::hypotheses(m, "logexp + logexp:female = 0"), error=function(e) NULL)
  resC[[o$lbl]] <- data.table(outcome=o$lbl,
      male_effect_pp = round(CF*male_e[1],3), male_p=signif(male_e[4],3),
      female_minus_male_pp = round(CF*diff_e[1],3), interaction_se_pp=round(CF*diff_e[2],3),
      interaction_p = signif(diff_e[4],4),
      female_effect_pp = if(!is.null(L)) round(CF*L$estimate,3) else NA_real_,
      female_p = if(!is.null(L)) signif(L$p.value,3) else NA_real_,
      N=m$nobs)
}
C <- rbindlist(resC, fill=TRUE)
fwrite(C, file.path(OUT,"rev02C_gender_interactions.csv")); cat("\n=== C: gender interaction tests ===\n"); print(C)

########################################################################
## (D) UNCONDITIONAL (population-level) mismatch outcomes  (section 6)
########################################################################
# denominator = broad main sample (all individuals w/ valid exposure etc.)
d[, u_tert_emp_horiz  := as.integer(!is.na(horiz_mismatch_detail_broad) & horiz_mismatch_detail_broad==1L)]
d[, u_tert_emp_double := as.integer(both_refined_mismatch_available==1L & !is.na(overed_horiz_mism_refined) & overed_horiz_mism_refined==1L)]
d[, u_tert_emp_full   := as.integer(both_refined_mismatch_available==1L & !is.na(fully_matched_refined) & fully_matched_refined==1L)]
d[, u_tert_emp        := as.integer(tertiary_educated==1L & employed==1L)]
resD <- list()
for(v in c("u_tert_emp","u_tert_emp_horiz","u_tert_emp_double","u_tert_emp_full")){
  resD[[paste0(v,"_base")]] <- cbind(runspec(d, v, "logexp + female + urban", "prov + cohort + yr", v, "1. Baseline FE"),
                                     ymean=d[,weighted.mean(get(v),w,na.rm=TRUE)])
  resD[[paste0(v,"_pxy")]]  <- cbind(runspec(d, v, "logexp + female + urban", "prov^yr + cohort", v, "2. + prov x year FE"),
                                     ymean=NA_real_)
}
D <- rbindlist(resD, fill=TRUE)
D[, `:=`(effect_pp=round(effect_pp,3), se_pp=round(se_pp,3), p=signif(p,3), ymean=round(ymean,4), coef=signif(coef,5), se=signif(se,5))]
fwrite(D, file.path(OUT,"rev02D_unconditional.csv")); cat("\n=== D: unconditional population outcomes ===\n"); print(D)

########################################################################
## (E) ECONOMIC MAGNITUDES  (section 16)
########################################################################
wmean <- function(x,w) sum(x*w)/sum(w)
wsd   <- function(x,w){ m<-wmean(x,w); sqrt(sum(w*(x-m)^2)/sum(w)) }
ind_sd  <- d[, wsd(logexp,w)]
ind_iqr <- diff(Hmisc_wtd_q <- {
  o <- order(d$logexp); lx<-d$logexp[o]; cw<-cumsum(d$w[o])/sum(d$w); approx(cw,lx,c(.25,.75))$y })
# province-cohort cell level
cell <- d[, .(logexp=logexp[1], w=sum(w)), by=.(prov,cohort)]
cell_sd  <- cell[, wsd(logexp,w)]
cell_iqr <- { o<-order(cell$logexp); lx<-cell$logexp[o]; cw<-cumsum(cell$w[o])/sum(cell$w); diff(approx(cw,lx,c(.25,.75))$y) }
mag <- data.table(
  quantity=c("SD log-exposure (individual, wtd)","IQR log-exposure (individual, wtd)",
             "SD log-exposure (prov-cohort cell, wtd)","IQR log-exposure (prov-cohort cell, wtd)",
             "10% increase in exposure = delta log", "conversion factor 100*ln(1.1)"),
  value=c(ind_sd, ind_iqr, cell_sd, cell_iqr, log(1.10), CF))
fwrite(mag, file.path(OUT,"rev02E_magnitudes.csv")); cat("\n=== E: magnitude inputs ===\n"); print(mag)

########################################################################
## (F) PROVINCE x COHORT (x YEAR) CELL-LEVEL ROBUSTNESS  (section 15)
########################################################################
resF <- list()
for(o in outcomes){
  S <- o$S; y <- o$y; lbl <- o$lbl
  csub <- S[!is.na(get(y)), .(ybar=wmean(get(y),w), logexp=logexp[1], nw=sum(w), n=.N,
                              female=wmean(female,w), urban=wmean(urban,w)), by=.(prov,cohort,yr)]
  m <- tryCatch(feols(ybar ~ logexp + female + urban | prov + cohort + yr, csub, weights=~nw,
                      cluster=~prov+cohort, notes=FALSE), error=function(e) NULL)
  if(!is.null(m)) resF[[lbl]] <- cbind(outcome=lbl, spec=sprintf("prov x cohort x year cells (n=%d)",nrow(csub)), grab(m))
}
F <- rbindlist(resF, fill=TRUE)
F[, `:=`(effect_pp=round(effect_pp,3), se_pp=round(se_pp,3), p=signif(p,3), coef=signif(coef,5), se=signif(se,5))]
fwrite(F, file.path(OUT,"rev02F_cell_level.csv")); cat("\n=== F: cell-level ===\n"); print(F)

cat("\nDONE rev02\n")
