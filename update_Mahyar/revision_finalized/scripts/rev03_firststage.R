# rev03_firststage.R -- deep dive: does exposure predict educational attainment?
suppressPackageStartupMessages({library(data.table); library(fst); library(fixest)})
setDTthreads(0); setFixest_nthreads(4); options(width=220, scipen=999)
CF <- 100*log(1.10)
cols <- c("logexp","w","female","urban","cohort","prov","yr","samp_main","gender","age",
 "tertiary_educated","education_rank","educ_level_num","literate","student","employed")
d <- as.data.table(read_fst("C:/MismatchRev/slim.fst", columns=cols))[samp_main==1L]
d[, cc := cohort - 1369L]
wm <- function(x,w) sum(x*w,na.rm=TRUE)/sum(w[!is.na(x)])

g <- function(dat,y,rhs,fe,term="logexp"){
  m <- feols(as.formula(sprintf("%s ~ %s | %s",y,rhs,fe)), dat, weights=~w, cluster=~prov+cohort, notes=FALSE, warn=FALSE)
  ct <- coeftable(m)[term,]
  data.table(coef=signif(ct[1],4), se=signif(ct[2],4), p=signif(ct[4],3), eff_pp=round(CF*ct[1],3), N=m$nobs)
}
res <- list()
add <- function(lbl,...) res[[length(res)+1]] <<- cbind(row=lbl, ...)

## vary FE structure, full sample
add("tertiary | prov FE only",           g(d,"tertiary_educated","logexp+female+urban","prov"))
add("tertiary | prov+cohort",            g(d,"tertiary_educated","logexp+female+urban","prov+cohort"))
add("tertiary | prov+cohort+year (base)",g(d,"tertiary_educated","logexp+female+urban","prov+cohort+yr"))
add("tertiary | prov+year (no cohort)",  g(d,"tertiary_educated","logexp+female+urban","prov+yr"))

## age restrictions (post-university completion), baseline FE
for(a in c(23,25,28,30)){
  add(sprintf("tertiary, age>=%d",a), g(d[age>=a],"tertiary_educated","logexp+female+urban","prov+cohort+yr"))
}
## cohort restriction: fully post-expansion observation (older cohorts, seen at 25+)
add("tertiary, age>=25 & cohort 1355-1375", g(d[age>=25 & cohort<=1375],"tertiary_educated","logexp+female+urban","prov+cohort+yr"))
add("tertiary, age 25-40",                   g(d[age>=25 & age<=40],"tertiary_educated","logexp+female+urban","prov+cohort+yr"))

## other attainment outcomes, age>=25
add("education_rank, age>=25", g(d[age>=25],"education_rank","logexp+female+urban","prov+cohort+yr"))
add("educ_level_num, age>=25", g(d[age>=25],"educ_level_num","logexp+female+urban","prov+cohort+yr"))
add("literate, age>=25",       g(d[age>=25],"literate","logexp+female+urban","prov+cohort+yr"))

## gender-split, age>=25
add("tertiary women, age>=25", g(d[age>=25 & gender==1L],"tertiary_educated","logexp+urban","prov+cohort+yr"))
add("tertiary men, age>=25",   g(d[age>=25 & gender==2L],"tertiary_educated","logexp+urban","prov+cohort+yr"))

R <- rbindlist(res, fill=TRUE)
print(R)
fwrite(R, "C:/MismatchRev/out/rev03_first_stage_deep.csv")

## descriptive: tertiary share by exposure quartile (within prov, residualized)
d25 <- d[age>=25]
d25[, logexp_r := resid(feols(logexp ~ 1 | prov+cohort+yr, d25, weights=~w))]
d25[, q := cut(logexp_r, quantile(logexp_r, 0:4/4, na.rm=TRUE), include.lowest=TRUE, labels=1:4)]
cat("\ntertiary share by residualized-exposure quartile (age>=25):\n")
print(d25[, .(tertiary=round(wm(tertiary_educated,w),4), n=.N), by=q][order(q)])
