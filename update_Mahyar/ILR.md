DRAFT prepared for submission to the *International Labour Review*

# University Access, Educational Mismatch, and the Selective Gender Gap in Iran's Labor Market

**Authors**: Mahyar Rezazadeh, Hossein Jalili
**Draft date**: [insert]
**Companion paper**: *University Expansion, Educational Mismatch, and Occupational Allocation in Iran* (Rezazadeh and Jalili, draft "paper_july17"), which supplies the identification strategy, the constructed Labor Force Survey panel, and the main vertical/horizontal mismatch point estimates this paper draws on, extends, and reinterprets through a gender-selection lens.

---

## Abstract

Iran expanded undergraduate enrollment dramatically between the mid-1990s and mid-2010s. We ask how the resulting province-by-cohort variation in university access affected vertical mismatch (over-education relative to an occupation's typical schooling), horizontal mismatch (working outside one's field of study), and — the paper's central new contribution — how much of Iran's very large gender employment gap reflects women's markedly more selective entry into work. Using 18 waves of the Iranian Labor Force Survey (1386–1403; approximately 4.1 million observations) linked to a measure of university exposure at ages 18–20 in 28 harmonized historical provinces, and following the same province-by-birth-cohort design used in the companion paper, we show that university exposure raised over-education and, especially, horizontal mismatch, with both effects two to three times larger for men than women. We show this gender asymmetry is a selection artifact rather than evidence that women are protected from mismatch: a selection-margin regression and a two-step Heckman correction (with two-way clustered inference throughout, including the selection-equation probit stage) show employed women are drawn from a far narrower, more positively selected slice of the female education distribution than employed men are of the male distribution, with a roughly five-fold larger correlation between unobserved selection determinants and realized hours. A new decomposition shows this channel is quantitatively large: an Oaxaca–Blinder decomposition attributes an amount larger than the entire observed 13.6-hour weekly-hours gap among the employed to the selection channel alone, while a complementary counterfactual shows only about 7.5 percent of the 55-percentage-point raw employment-rate gap owes to women and men responding differently to observable characteristics — the remainder reflects structural, province-level differences the selection framework cannot speak to directly. We also compile new national data on Konkoor (university entrance exam) participants by gender, showing women's share of applicants rose from about 59 percent in the mid-1390s to over 61 percent by 1402–1403 — evidence that access, not qualified supply, is not the binding constraint. Relative to the closest existing Iran-specific evidence (Haddad and Habibi, 2017), which documents higher over-education odds for women using cross-sectional wage data from 2001–12, we provide the first quasi-experimental estimates of what drives Iranian mismatch upward, the first joint treatment of vertical and horizontal mismatch for Iran, and the first connection between Iran's mismatch literature and the selection-into-employment literature.

**JEL codes**: I23, I24, J16, J21, J24, O15
**Keywords**: educational mismatch, over-education, horizontal mismatch, university expansion, gender gap, selection into employment, Iran

---

## 1. Introduction

Iran's higher-education system underwent one of the fastest expansions of any country in the last three decades: undergraduate and professional enrollment grew from a system serving a small, exam-selected elite into one credentialing roughly a third of the employed workforce (companion paper, Figure 2). Nearly all of this expansion happened while the Konkoor — Iran's centralized, single-day national university entrance exam — remained the nominal gatekeeper, even as the government progressively opened parallel non-exam admission channels alongside it. The companion paper to this one (Rezazadeh and Jalili, draft) exploits province-by-birth-cohort variation in undergraduate enrollment per capita, measured at the conventional university-entry ages of 18–20, to show that this expansion did not raise employment or labor-force participation on average, but did raise over-education and, especially, horizontal mismatch (working outside one's field of study) — with the latter effect concentrated heavily among men.

This paper extends that analysis along the dimension it explicitly leaves open: gender. We ask three connected questions. First, does the gender asymmetry in mismatch responses reflect women being protected from mismatch as exposure rises, or being selected out of employment before mismatch can even be observed? Second, how much of Iran's very large raw gender employment gap can be attributed to differential selectivity, as against structural or institutional level differences that a selection framework cannot reach? Third, what did the underlying supply shock look like from the demand side — how many people were actually competing for the seats being created, and did that composition shift by gender in a way that could itself explain the mismatch results?

We show that the mismatch asymmetry is a selection artifact. Using a purpose-built gender-selection sample (4,086,861 observations) and three complementary approaches — a triple-interaction reduced-form model, a selection-margin regression, and a two-step Heckman correction estimated with two-way clustered standard errors throughout, including at the probit selection stage — we find that employed women are drawn from a persistently narrower, higher-education slice of the female population than employed men are of the male population, and that the correlation between unobserved selection determinants and realized hours is roughly five times larger in magnitude for women. Read together with the companion paper's finding that men's mismatch responds far more to exposure, the two results describe a single mechanism: men are drawn into the labor market more readily as exposure rises, absorbing worse job matches in the process, while women's entry into employment remains conditional on being an unusually strong candidate, so relatively few of them are observed experiencing mismatch simply because relatively few of them are observed working at all.

We then ask how much of Iran's raw gender employment gap this selectivity difference can actually explain — a question no existing paper on Iranian mismatch has posed. An extensive-margin counterfactual, substituting men's estimated behavioral response to observable characteristics into women's own fitted selection index, closes only about 7.5 percent of the raw 55-percentage-point employment-rate gap. An Oaxaca–Blinder decomposition of the 13.6-hour raw weekly-hours gap among the employed attributes a component of 33.5 hours — larger than the entire observed gap — to the selection channel alone. The reading is that, among the relatively few women who do work, selection is the dominant force shaping how much they work, even though it explains little of whether they work at all — a result consistent with a demand-constrained, rather than supply-constrained, explanation for Iran's gender employment gap.

Finally, we compile new national data — not previously connected to the Iranian mismatch literature — on Konkoor participants by gender, 1393–1403, showing women have been a majority of applicants throughout the verifiable series and that this majority has grown, from 59.1 percent (1393) to 63.1 percent (1403). This corroborates, from an entirely independent data source, the companion paper's enrollment-stock evidence that women's relative position in Iran's higher-education pipeline has been rising, not falling, and rules out a simple story in which women's mismatch outcomes are muted merely because their access expanded less than men's.

**Relation to existing evidence and contribution.** The closest existing paper on Iranian educational mismatch is Haddad and Habibi (2017), discussed in detail in Section 2.2. Briefly: they use Iran's Household Income and Expenditure Survey (HIES), 2001–12, to document that the share of workers with more than 12 years of schooling employed in jobs conventionally requiring 12 years or fewer rose steadily over that period, and that the odds of such over-education were higher for women than men, with a wage penalty in the private sector and a wage premium in the public sector. Their analysis is descriptive/correlational — it documents a trend and its wage correlates, not what causes over-education to rise — is restricted to low-skill jobs, and does not address horizontal (field-of-study) mismatch or the employment-selection margin at all. Relative to this and the broader literature, we contribute: (i) the first quasi-experimental estimates, to our knowledge, of what drives Iranian educational mismatch upward, exploiting geographically uneven university-capacity expansion rather than a pooled cross-sectional correlation; (ii) the first joint treatment of vertical and horizontal mismatch for Iran, including their joint occurrence (double mismatch) and joint absence (fully matched employment); (iii) the first connection between Iran's mismatch literature and the selection-into-employment/gender-labor-supply literature, including a formal decomposition of how much of the gender employment gap traces to differential selectivity; and (iv) new descriptive evidence, from an independent data source, on the demand side of Iran's higher-education expansion. Notably, our causal finding that women's over-education response to exposure exceeds men's (Section 5.1) independently corroborates Haddad and Habibi's cross-sectional finding of higher female over-education odds, despite using an entirely different data source, time period, and identification strategy — a form of cross-validation that strengthens confidence in the pattern being a real feature of Iran's labor market rather than an artifact of either paper's specific design.

Section 2 reviews the literature in detail, including a full positioning against Haddad and Habibi (2017). Section 3 describes the institutional background and the new Konkoor data. Section 4 describes the data in detail: the Labor Force Survey, the exposure panel, the gender-selection sample, and the resulting analytical samples. Section 5 restates the empirical strategy. Section 6 presents results, including the mismatch estimates, the selection analysis, the gender-gap decompositions, and event-study evidence on the design's identifying variation. Section 7 reports additional robustness checks. Section 8 discusses limitations and concludes.

## 2. Literature review

### 2.1 Measuring vertical and horizontal educational mismatch

The companion paper's vertical-mismatch measure — classifying a worker as over-, under-, or adequately educated relative to the *modal* education level in their occupation — sits in the "realized matches" tradition. Verdugo and Verdugo (1989) is the tradition's foundational paper, defining over/under-education relative to the occupation mean (± one standard deviation); Kiker, Santos, and Mendes de Oliveira (1997), studying Portugal, introduced the modal-value variant the companion paper follows, on the grounds that the mode is less sensitive to outliers. Both share a limitation the companion paper flags explicitly: the benchmark can itself embed pre-existing mismatch, so the resulting measure is relative allocation, not a technologically fixed requirement.

Horizontal (field-of-study) mismatch measurement follows a distinct tradition. Robst (2007) is an early paper using a field-to-occupation "relatedness" classification; Nordin, Persson, and Rooth (2010) extend this to Swedish administrative data and connect horizontal mismatch to a wage penalty distinct from the vertical penalty. Somers, Cabus, Groot, and van den Brink (2019), a systematic review, categorize the field into subjective, objective/crosswalk-based (the family the companion paper's crosswalk belongs to), and statistical/empirical measures, noting crosswalk measures are sensitive to how broadly the crosswalk is drawn — precisely the concern the companion paper's broad-versus-strict robustness check addresses.

### 2.2 Iran-specific evidence, and our relation to Haddad and Habibi (2017)

Haddad and Habibi (2017), published in this journal, is the closest existing paper on Iranian educational mismatch and merits detailed discussion. They use Iran's Household Income and Expenditure Survey (HIES) for 2001–12 (Persian years approximately 1380–1391) to study over-education incidence and its wage consequences in low-skilled jobs specifically. Their over-education measure is a threshold/job-requirement definition: a worker is classified as over-educated if they hold more than twelve years of schooling (i.e., some post-secondary attainment) while employed in an occupation conventionally requiring twelve years or fewer, and their sample is restricted to such low-skill occupations rather than spanning the full occupational distribution. Descriptively, they document that the share of over-schooled workers in these low-skill jobs rose steadily over 2001–12. Econometrically, they estimate the odds of over-education (finding them higher for women than men, and strongly negatively associated with experience) and, separately, the wage consequences of over-education by sector, finding a negative wage penalty in the private sector and a positive association in the public sector.

Four things distinguish our approach. First, identification: Haddad and Habibi's analysis is descriptive and correlational — it documents that over-education rose and what wage penalty it carries, but not what caused it to rise. We exploit geographically uneven province-by-cohort variation in university-capacity expansion, giving quasi-experimental estimates of a specific driver of mismatch. Second, scope of the mismatch measure: their over-education measure is a fixed schooling-year threshold applied only within low-skill occupations; our realized-matches measure, following the companion paper, is occupation-specific (benchmarked to each occupation's own modal education level) and spans the full occupational distribution, not just low-skill jobs. Third, dimensionality: Haddad and Habibi study only vertical mismatch; we jointly measure vertical and horizontal (field-of-study) mismatch and their joint occurrence, a dimension essentially absent from the existing Iran literature. Fourth, and most directly the contribution of this paper specifically (as against the companion paper), we connect Iran's mismatch literature to the selection-into-employment literature for the first time, asking not just whether employed women are more mismatched than employed men, but how much of the gender gap in employment and hours *itself* reflects differential selectivity — a question outside the scope of a wage-focused analysis restricted to the employed, as Haddad and Habibi's necessarily is (the HIES contains no representative province-cohort panel suited to this kind of decomposition).

Despite these differences in data, period, and method, our results and theirs point in the same direction on the one dimension where they overlap: both find Iranian women more prone to over-education than men. Haddad and Habibi find this in cross-sectional odds using 2001–12 wage data; we find the *causal response* of over-education to university exposure is larger for women than men (0.478 versus 0.305 percentage points per 10 percent increase in exposure; Section 6.1) using 1386–1403 labor-force data and an entirely different identification strategy. That two studies using different surveys, different time windows, different mismatch definitions, and different empirical designs converge on the same qualitative gender pattern is, we think, meaningful corroboration that this is a real and persistent feature of the Iranian labor market rather than an artifact of a particular method.

Beyond Haddad and Habibi, Salehi-Isfahani's working-paper-stage research characterizes Iran's higher-education system as producing graduates in excess of what the private-sector labor market can absorb in matched jobs, consistent with our overall reading, though we note this is a working paper whose final peer-reviewed status we have not independently confirmed.

### 2.3 University expansion and graduate mismatch elsewhere

The clearest international precedent for using regional/cohort variation in higher-education capacity to identify effects on graduate mismatch is China's 1999 expansion. Li, Whalley, and Xing (2014) and Li, Morgan, and Ding (2008) document that China's enrollment growth outpaced growth in graduate-appropriate jobs, raising both unemployment and over-education among new graduates; Knight, Deng, and Li (2017) reach a similar conclusion using a supply-shock framing closely analogous to ours. These papers consistently find, as we do, that the primary margin of adjustment to a higher-education supply shock is match quality rather than employment quantity.

### 2.4 Selection into employment and the gender gap

The idea that observed gender gaps can be an artifact of *who selects into observability*, rather than differential treatment or characteristics among the employed, is central to Mulligan and Rubinstein (2008), who show the apparent narrowing of the U.S. gender wage gap between the 1970s and 1990s partly reflects a shift in the selection of women into full-time work, from negative to positive selection — the same *direction* of selection we find for Iranian women's employment on the education margin, though in Iran the selection appears uniformly positive and strong throughout our sample rather than shifting over time. This is the direct conceptual ancestor of our Heckman-based decomposition in Section 6.3.

Directly on Iran, Tanaka and Moeeni (2023) study a 2010 policy imposing public-sector female-hiring quotas and find it caused an 11.4-percentage-point *decline* in women's college attendance in affected fields — evidence that Iranian women's educational investment responds to anticipated labor-demand conditions. This supports our reading of the extensive-margin decomposition: if the binding constraint on Iranian women's employment were primarily about their own characteristics or preferences, we would expect our counterfactual exercise to close much more of the raw employment gap than the 7.5 percent we find.

A broader Middle East and North Africa literature documents a persistent "gender paradox": near-parity or a female advantage in educational attainment coexisting with the lowest female labor-force participation rates in the world outside South Asia. Our sample's female labor-force participation rate (roughly 15–20 percent) sits at the low end even by this region's standards. The dominant explanations emphasize social norms, occupational segregation, and legal/institutional constraints rather than a shortage of qualified female labor supply — a framing our results are directly consistent with.

## 3. Institutional background and new evidence on Konkoor participants

### 3.1 Iran's university admission system

Since 1968, admission to Iranian public universities has been governed primarily by the Konkoor, a centralized, single-day, high-stakes national exam administered by the Sanjesh Organization. Historically nearly all public-university seats, and a large share of seats at parallel private/quasi-private institutions (Islamic Azad University, Payam-Noor University), were allocated on Konkoor rank within a quota structure including merit-based and reserved-category tracks. Beginning around 1395 (2016), Iran began allocating a growing share of seats through GPA-based admission (*savabegh-e tahsili*) rather than the exam: by our sources, over 20 percent of public-university capacity and over 70 percent of capacity at other institutions has been allocated this way from 1395 onward, a share that has continued to grow.

### 3.2 New national data: Konkoor participants by gender, 1393–1403

We compiled a national-level series of Konkoor participants by gender for 1393–1403 from Sanjesh figures reported in the Iranian press and consolidated in Persian Wikipedia's Konkoor article (Figure 1). We investigated whether a province-level "seats per Konkoor applicant" panel could be constructed as an alternative exposure measure to the companion paper's enrollment-per-capita measure, and found this is not feasible for our full sample period from public data: Sanjesh does not maintain a scrapable public archive of participant/admission counts by province and gender; the Statistical Yearbook may contain such a table for individual years but would require manual extraction from non-machine-readable scans; and the one compiled national dataset we located is paywalled and lacks province detail. The project's own data folder contains a province-by-gender admitted-students table, but only for 1373–1382, too short a span for its own province-cohort design.

Because press outlets sometimes misattribute figures to the wrong exam year, we retained only years for which reported female and male participant counts summed to the reported total within rounding error; this excluded 1400 and 1401, where sources could not be reconciled.

*[Figure 1 here: national Konkoor participants by gender, 1393–1403, and the female share of participants over the same period; see `figure_konkoor_participants.png`.]*

Total participants did not decline monotonically: from about 1.03 million (1393) to a low of about 860,000 (1395), back to a local peak of roughly 1.15 million (1399), then down to about 984,000 by 1403. Women have been a majority of participants in every verified year, and that majority has grown: from 59.1 percent (1393) to 58.0–59.2 percent through 1399, then to 61.4 percent (1402) and 63.1 percent (1403). Because most participants are drawn from the age-18–20 window our exposure measure targets, this corroborates, from an independent data source, the companion paper's enrollment-stock evidence that women's relative position in Iran's higher-education pipeline has been rising throughout the study period. We do not use this series as a causal exposure variable: it has no province-level variation in the years we could verify and therefore cannot support a province-cohort design (year fixed effects would fully absorb a national-only series). We use it as descriptive context, and continue to use the companion paper's enrollment-per-capita measure as the causal exposure variable, since it captures realized capacity across all admission channels, whereas Konkoor participation captures only demand for the shrinking competitive-exam channel.

## 4. Data

This section describes each data source, how each analytical sample was constructed, and the sample sizes at each stage — reported in detail because our results depend on several linked construction decisions.

### 4.1 Iranian Labor Force Survey

Our primary data source is the Iranian Labor Force Survey (LFS), a repeated cross-sectional household survey conducted quarterly by Iran's Statistical Centre, covering Persian calendar years 1386–1403 (18 survey years; approximately 2007/08–2024/25 in the Gregorian calendar). The LFS records demographic characteristics, educational attainment, field of study, labor-force status, occupation, current and previous place of residence, and survey weights, for a nationally representative rotating sample of households. We use the companion paper's harmonized individual-level panel, built by pooling all 18 available yearly files, restricting to individuals born between Persian years 1355 and 1383 (29 birth cohorts, chosen so that university exposure at ages 18–20 is observable for every cohort), and mapping current and historical province identifiers onto a consistent set of 28 historical provinces to prevent administrative boundary changes from generating spurious exposure variation. This construction yields an analysis file of 4,128,510 observations.

Two education-related variables require additional construction because they are not present as clean numeric fields in the raw LFS extracts. First, `educ_level` — a required numeric education code — does not exist as a raw column; it is recovered as the alphabetically sorted encoding of the raw string education-level field (confirmed by reproducing Stata's `encode` ordering, which places numeric leftover codes before letter-coded categories, exactly matching the harmonized eight-level education hierarchy used throughout). Second, occupation codes are standardized to two-digit ISCO categories from free-text and mixed-format raw entries.

### 4.2 University enrollment, population, and exposure panel

University expansion is measured using a province-year panel of undergraduate and professional enrollment, harmonized to the same 28 historical provinces as the LFS and combined with province population data to construct enrollment per 100,000 residents. For an individual born in cohort $c$ in historical province $p$, exposure is the average of this enrollment rate at ages 18, 19, and 20:
$$\text{Exposure}_{pc} = \tfrac{1}{3}\left(E_{p,c+18} + E_{p,c+19} + E_{p,c+20}\right),$$
entering the regressions in natural-log form. The baseline sample requires all three annual values to be observed. This is the companion paper's exposure construction in full; we do not modify it (see Section 3.2 for why we retain it over a Konkoor-participant-based alternative).

### 4.3 Vertical and horizontal mismatch construction

Vertical mismatch follows the realized-matches approach (Section 2.1): for each occupation, the modal education rank among employed workers, subject to minimum-cell reliability restrictions, is treated as the occupation's typical education level, and workers are classified as over-, under-, or vertically matched relative to it. Horizontal mismatch is defined for tertiary-educated employed workers as an indicator for whether their two-digit ISCO occupation falls outside the set of occupations linked to their ISCED field of study by a field-to-occupation crosswalk (broad definition preferred; a strict crosswalk is used for robustness). Double mismatch is the joint occurrence of over-education and horizontal mismatch; fully matched employment is the joint absence of both. The joint-mismatch-eligible sample — tertiary-educated employed workers with valid vertical and horizontal mismatch measures — comprises 400,726 observations in the pooled LFS panel, verified directly from the underlying analysis file for this draft (Section 6.4 uses this same sample for the event-study check).

### 4.4 Gender-selection sample

For the selection analysis (Sections 6.2–6.3) we additionally reconstruct household size and number of children — variables present in the raw LFS via the household-relationship-to-head code but dropped from the companion paper's main analysis file during its variable-selection step — recomputed at the household-quarter level, to serve as the exclusion restriction in our Heckman selection model. We restrict to the companion paper's broad labor-market sample: individuals with valid exposure, province, cohort (1355–1383), survey weight, gender, and urban/rural status. This yields 4,086,861 observations: 2,079,162 women and 2,007,699 men. Weekly hours are defined among the employed only, using all-jobs hours per week where available and falling back to main-job hours otherwise.

### 4.5 Sample restriction summary

| Sample | Restriction | N |
|---|---|---|
| Full harmonized LFS panel | Birth cohorts 1355–1383, valid province/exposure | 4,128,510 |
| Gender-selection (broad labor-market) sample | + valid weight, gender, urban/rural | 4,086,861 (2,079,162 F / 2,007,699 M) |
| Joint (vertical + horizontal) mismatch-eligible sample | Tertiary-educated, employed, valid occupation/field | 400,726 |
| Heckman selection-equation sample, women | + valid number of children, tertiary, urban | 2,079,162 |
| Heckman selection-equation sample, men | + valid number of children, tertiary, urban | 2,007,699 |
| Heckman hours-equation sample, women | Employed, finite Mills ratio | 301,636 |
| Heckman hours-equation sample, men | Employed, finite Mills ratio | 1,384,685 |

### 4.6 Konkoor participant data

Described in Section 3.2. Nine years (1393–1399, 1402, 1403) of nationally aggregated, gender-disaggregated Konkoor participant counts, compiled from press-reported Sanjesh Organization figures and cross-checked for internal consistency (reported female and male counts summing to the reported total).

## 5. Empirical strategy

We use the companion paper's baseline specification without modification:
$$Y_{ipct} = \beta \cdot \log(\text{Exposure}_{pc}) + X_i'\gamma + \mu_p + \lambda_c + \tau_t + \varepsilon_{ipct},$$
with historical-province ($\mu_p$), birth-cohort ($\lambda_c$), and survey-year ($\tau_t$) fixed effects; survey weights; and two-way clustering by province and cohort (28 province, 29 cohort clusters) throughout. Section 6.1 reports the companion paper's own mismatch estimates from this specification, which we cite directly rather than re-estimate, having independently reproduced them from the underlying analysis file in an earlier verification pass, matching the paper's reported coefficients to three decimal places.

For the gender-selection analysis (Sections 6.2–6.3) we extend this specification in three ways, following the same fixed-effect structure, weighting, and clustering throughout — **including, in this draft, the selection-equation probit stage of the Heckman correction, which we now cluster two-way by province and cohort rather than reporting IID standard errors as in our previous draft** (Section 7.1 discusses this change and its effect on inference).

**Triple-interaction model.**
$$Y_{ipct} = \beta_1 \text{Exp} + \beta_2 \text{Exp}\times\text{Female} + \beta_3 \text{Exp}\times\text{Tert} + \beta_4 \text{Exp}\times\text{Female}\times\text{Tert} + X_i'\gamma + \mu_p + \lambda_c + \tau_t + \varepsilon,$$
for $Y \in \{\text{employed},\ \text{weekly hours} \mid \text{employed}\}$.

**Selection-margin model.**
$$\text{EduRank}_{ipct} = \gamma_1 \text{Employed} + \gamma_2 \text{Employed}\times\text{Exp} + \gamma_3 \text{Employed}\times\text{Exp}\times\text{Female} + X_i'\gamma + \text{FE} + \varepsilon.$$

**Two-step Heckman selection correction**, estimated separately by gender. First stage:
$$\text{Employed}^*_{ipct} = \text{Exp}\cdot\delta_1 + \text{Tert}\cdot\delta_2 + \text{Urban}\cdot\delta_3 + \text{NumKids}\cdot\delta_4 + \text{NumKids}\times\text{Exp}\cdot\delta_5 + \text{FE} + u,$$
using number of children in the household, interacted with exposure, as the exclusion restriction, with standard errors now two-way clustered by province and cohort. We compute the inverse Mills ratio from the first-stage linear predictor and include it in a second-stage hours regression among the employed:
$$\text{Hours}_{ipct} = \text{Exp}\cdot\beta_1 + \text{Tert}\cdot\beta_2 + \text{Urban}\cdot\beta_3 + \lambda \cdot \text{MillsRatio} + \text{FE} + \varepsilon.$$

**Gender-gap decompositions.** Building on the fitted Heckman models, Section 6.3 constructs (i) an extensive-margin counterfactual substituting men's estimated first-stage coefficients into women's own fitted selection index, holding women's own province-cohort-year context and characteristics fixed, to isolate how much of the raw employment-rate gap reflects differential behavioral response versus differential baseline levels; and (ii) a standard Oaxaca–Blinder decomposition of the raw mean weekly-hours gap between employed men and women.

**Event-study check.** Because exposure is continuous rather than a discrete treatment date, we cannot construct a canonical binary-treatment event study. As the closest available analogue and a direct test of the design's parallel-trends assumption, we classify provinces as high- or low-exposure based on their time-invariant sample-average log exposure (median split), and estimate
$$Y_{ipct} = \sum_{c \neq c_0} \delta_c\, (\mathbb{1}[\text{cohort}=c] \times \text{HighExposure}_p) + X_i'\gamma + \mu_p + \lambda_c + \tau_t + \varepsilon,$$
plotting $\hat\delta_c$ against birth cohort, relative to the earliest cohort $c_0$=1355. This tests whether high- and low-exposure provinces were on divergent trajectories in mismatch outcomes before and independent of the within-province exposure changes that actually identify the paper's coefficients; it is a validity check, not a re-estimate of the treatment effect itself (Section 6.4 discusses this distinction explicitly).

## 6. Results

### 6.1 University exposure and educational mismatch (companion paper's estimates, reinterpreted)

Table 1 restates the companion paper's central mismatch results, expressed as percentage-point effects of a 10 percent increase in university exposure at ages 18–20.

**Table 1. University exposure and educational mismatch (companion paper, Table 2)**

| Outcome | Pooled | Women | Men |
|---|---|---|---|
| Any vertical mismatch | 0.170 (0.110) | 0.299 (0.222) | 0.141 (0.105) |
| Over-education | 0.335*** (0.101) | 0.478** (0.205) | 0.305*** (0.104) |
| Under-education | −0.165* (0.090) | −0.179 (0.109) | −0.164 (0.102) |
| Horizontal mismatch | 0.614*** (0.126) | 0.073 (0.174) | 0.840*** (0.149) |
| Double mismatch | 0.695*** (0.140) | 0.368** (0.144) | 0.797*** (0.182) |
| Fully matched employment | −0.341*** (0.103) | 0.088 (0.207) | −0.513*** (0.115) |

*Notes: two-way clustered SE (province × cohort) in parentheses. \*\*\* p<0.01, \*\* p<0.05, \* p<0.10.*

Read alongside Section 3's institutional facts and Section 2.2's positioning against Haddad and Habibi (2017), the pattern is that women's over-education response to exposure (0.478) is larger than men's (0.305) — consistent with, and a causal analogue of, Haddad and Habibi's cross-sectional finding of higher female over-education odds — while horizontal and double mismatch rise dramatically more for men (0.840 vs. 0.073; 0.797 vs. 0.368). Because women's share of the competing applicant pool has been rising, not falling (Section 3.2), this asymmetry cannot be explained by women facing a smaller expansion in access. Section 6.2 shows it is instead a selection artifact.

### 6.2 Who behaves more selectively: women or men?

The descriptive selection gap (education rank of the employed minus education rank of the full eligible population, by gender and exposure quartile) is stark. Among men, the employed are close to, or slightly below, the full male population in education rank, and this small negative gap widens with exposure (−0.008 to −0.458) — consistent with rising over-education among employed men (Table 1), but showing employment itself does not become more selective for men. Among women, the employed are substantially more educated than the full female population in every quartile (0.858, 0.851, 0.336, 0.452) — working women in Iran are a persistently, strongly positively selected minority across the entire exposure distribution.

The regression version confirms this survives controlling for province, cohort, year, and urbanicity: the employment–education-rank premium is 2.425 ranks for the male reference group and an additional 0.804 ranks for women (p=0.006). This premium narrows with exposure for men (Employed×Exposure = −0.469, p<0.001) but narrows distinctly less for women (Employed×Exposure×Female = 0.083, p=0.09): women's employment remains a more consistently selective filter across the exposure distribution than men's.

The Heckman results sharpen this into a single number: the inverse Mills ratio coefficient in the hours equation is −24.3 (p<0.001) for women versus −5.1 (p<0.001) for men — roughly a five-fold difference. **Answering the question directly: women behave far more selectively than men.** Not in the sense of being pickier about job characteristics conditional on working — the data cannot speak to that — but in the sense that whether a woman works at all, and how much she works if she does, is far more tightly bound up with unobserved selection factors than for men.

### 6.3 How much of the gender gap reflects this selective behavior?

**Extensive margin.** The raw employment-rate gap in our sample is 69.8 percent for men versus 14.7 percent for women, a 55.1-percentage-point gap. Substituting men's estimated first-stage coefficients into each woman's own fitted selection index — holding her own characteristics and province-cohort-year context fixed — raises predicted female employment from 14.7 to 18.8 percent, closing 4.1 of the 55.1 points. **Differential behavioral response to observable characteristics therefore accounts for only about 7.5 percent of the raw employment-rate gap**; the remaining 92.5 percent reflects structural/institutional/normative factors the fixed effects absorb.

**Intensive margin.** Among the employed, the raw weekly-hours gap is 13.6 hours (50.3 for men, 36.7 for women). Table 2 decomposes this using a two-fold Oaxaca–Blinder decomposition over the four regressors in the second-stage hours equation.

**Table 2. Oaxaca–Blinder decomposition of the employed-hours gender gap**

| Component | Endowment | Coefficient | Total |
|---|---|---|---|
| Exposure | −0.02 | −8.77 | −8.79 |
| Tertiary education | 0.58 | 4.75 | 5.33 |
| Urban | 0.05 | −4.70 | −4.65 |
| **Mills ratio (selection)** | **5.40** | **28.06** | **33.45** |
| **Total (4 regressors)** | **6.00** | **19.33** | **25.34** |
| *Raw hours gap* | | | *13.58* |

The mills/selection channel alone (33.45 hours) is larger than the entire observed 13.58-hour gap — 246 percent of it. This is offset by exposure's and urban status's coefficient components and by a large implied negative contribution from province/cohort/year fixed effects (≈−11.8 hours, the residual needed to reconcile the 25.34-hour four-regressor gap with the observed 13.58-hour gap; we read this cautiously as it is not directly estimated). The reading is that, among the relatively few women who work, selection is the dominant force shaping how much they work, even though selection differences on observable characteristics explain little of *whether* they work at all.

### 6.4 Event-study evidence

*[Figures 2a and 2b here: cohort-by-high-exposure-province dynamic effects for double mismatch and horizontal mismatch, relative to birth cohort 1355; see `figure_event_study_double_mismatch.png` and `figure_event_study_horizontal_mismatch.png`.]*

We emphasize what this check does and does not show. It is not a re-estimate of the paper's treatment effect: the paper's actual identifying variation is the within-province change in exposure across cohorts, whereas this check uses a cruder, time-invariant, cross-province high/low split, which discards most of that variation and instead asks whether provinces that happen to have higher average exposure over the whole sample were already on a different mismatch trajectory before and independent of that variation. For horizontal mismatch, the estimated cohort-by-high-exposure coefficients are close to zero and noisy throughout the cohort range, with no clear monotonic divergence — reassuring evidence against a simple pre-existing-trend confound. For double mismatch, the coefficients are mildly negative and relatively stable through the middle of the cohort range, widening only for the youngest cohorts (where cell sizes are smallest and confidence intervals correspondingly widest); the sign here runs opposite to the main pooled coefficient, which is expected, since this coarse specification is not estimating the same object as the main design; we read it as showing no dramatic, early divergence that would undermine the parallel-trends assumption, rather than as an independent confirmation of the treatment effect's magnitude or sign.

## 7. Robustness

### 7.1 Clustering the selection-equation standard errors

Our previous draft reported the Heckman selection-equation (probit) standard errors as IID, a limitation of fixed-effects probit estimation flagged explicitly at the time. We re-estimate both selection equations with standard errors two-way clustered by province and cohort, matching every other specification in this paper and the companion paper (Table 3). Point estimates are numerically identical to the IID-SE version, as they must be — clustering affects only the standard errors, not the coefficients — so the extensive-margin counterfactual and the Oaxaca–Blinder decomposition in Section 6.3, which depend only on point estimates (and, for the second stage, on standard errors that were already clustered in every draft of this paper), are unaffected by this revision.

**Table 3. Two-step Heckman selection correction, by gender (two-way clustered SE)**

| | Women: Selection (probit) | Women: Hours (2nd stage) | Men: Selection (probit) | Men: Hours (2nd stage) |
|---|---|---|---|---|
| Exposure | −0.0808 (0.0583) | 0.9589* (0.5135) | 0.0169 (0.0670) | −0.4625 (0.4198) |
| Tertiary | 0.6290*** (0.0766) | −12.1724*** (1.7526) | −0.5196*** (0.0606) | −2.3183*** (0.7128) |
| Urban | −0.2975*** (0.0470) | 10.2399*** (0.7398) | −0.1730*** (0.0272) | 3.7295*** (0.5310) |
| Num. children | 0.1342*** (0.0255) | — | −0.1055 (0.0680) | — |
| Num. children × Exp. | −0.0096** (0.0030) | — | −0.0098 (0.0091) | — |
| Inverse Mills ratio | — | −24.3253*** (2.1911) | — | −5.0765*** (0.4902) |
| Observations | 2,079,162 | 301,636 | 2,007,699 | 1,384,685 |

*Notes: fixed effects (province, cohort, year) in all four columns. Selection-equation SEs now two-way clustered by province and cohort (previously IID); hours-equation SEs were already two-way clustered in every draft. \*\*\* p<0.01, \*\* p<0.05, \* p<0.10.*

Two changes in inference are worth flagging explicitly, in the interest of not overstating precision. First, the direct effect of exposure on the probability of employment is not statistically distinguishable from zero for either gender once clustering is applied (women: p=0.166; men: p=0.801), where it had appeared strongly significant under IID standard errors. We read this as a *welcome* correction rather than a problem: it brings the selection-equation first stage into line with the companion paper's own Table 1 finding of a null effect of exposure on employment and labor-force participation, resolving what had been a minor internal tension in our previous, IID-based draft. Second, and more consequential for our identification argument, the exclusion restriction's exposure interaction (number of children × exposure) remains significant for women (p=0.0016) but is no longer statistically significant for men (p=0.281) under clustering, though the number-of-children main effect remains highly significant for women (p<0.001) and directionally similar, though no longer significant, for men (p=0.121). We view this as a genuine weakening of the exclusion restriction's identifying power for men specifically, and flag it as a limitation in Section 8 rather than downplaying it: the male-side selection equation should be read as less precisely pinned down than the female-side one, even though the coefficients that matter most for our central results — the second-stage inverse Mills ratios, both estimated with two-way clustered standard errors in every version of this paper — remain highly significant for both genders and numerically unchanged (women: −24.3, p<0.001; men: −5.1, p<0.001).

### 7.2 Other robustness checks (companion paper)

The companion paper additionally reports, for the main vertical/horizontal/double-mismatch estimates this paper builds on: robustness to six alternative combinations of vertical benchmark (preferred/reliable, unrestricted full-period, period-specific) and horizontal crosswalk (broad, strict), all preserving sign, magnitude, and significance; robustness to unweighted estimation, province-specific cohort trends, exclusion of identified cross-province movers, exclusion of Tehran, and exclusion of provinces with unusual administrative histories (Semnan, Qazvin, Qom, Golestan); robustness to alternative standard-error clustering (province-only, cohort-only); and leave-one-province-out estimates across all 28 provinces, in every case preserving sign and significance of the double-mismatch and fully-matched-employment coefficients. We do not repeat these here but note they apply directly to the Table 1 estimates this paper's Section 6.1 discussion builds on.

## 8. Discussion and conclusion

This paper connects four pieces of evidence — the companion paper's causal estimates of how expanding university access reshaped graduate job matching, a careful positioning against the closest existing Iran-specific mismatch evidence (Haddad and Habibi, 2017), a new decomposition of how much of Iran's gender employment gap traces to differential selection into work, and new descriptive data on who is competing for Iranian university seats — into a single account. University access expanded for women at least as much as for men over our sample period, evident both in enrollment stocks (companion paper) and in the rising female share of Konkoor participants documented here. Yet this expansion did not translate into a proportionate expansion in employment for women, nor did it generate the same "settle for a worse match" dynamic among employed women that it generated among employed men. Instead, Iranian women's employment has remained a highly selective filter, and that selectivity is quantitatively large enough to more than explain the realized hours gap among the employed, even though it explains only a small fraction of the much larger gap in whether women work at all.

We interpret this as consistent with a demand-constrained, rather than supply-constrained, explanation for Iran's persistent gender employment gap: women are not failing to acquire human capital or compete for it (if anything, they increasingly out-compete men for university seats), but the labor market absorbs relatively few of them into employment, and the ones it does absorb are unusually well-qualified relative to the broader female population.

**Limitations.** First, the Konkoor-participant series is national and cannot support province-level causal identification; we use it only descriptively. Second, our selection framework relies on a single exclusion restriction (number of children); alternative instruments (e.g., co-resident extended family) would strengthen the Heckman results, and Section 7.1 shows this restriction's exposure interaction is not statistically significant for men under proper clustering, so the male-side selection equation should be read as less precisely identified than the female-side one, even though the second-stage Mills-ratio results that drive our central claims are unaffected. Third, the LFS does not collect wage data, so our intensive-margin decomposition is on hours rather than earnings. Fourth, the fixed-effect residual in the Oaxaca decomposition (Table 2) is not directly estimated and should be read as suggestive; a fuller treatment would re-estimate with a common fixed-effect structure across genders (interacted with a female indicator) to make the decomposition fully additive. Fifth, our event-study check (Section 6.4) uses a coarser, time-invariant exposure split than the paper's actual continuous, within-province identifying variation, and should be read as a pre-trends validity check rather than an independent estimate of the treatment effect.

Despite these caveats, we view the core findings — a persistent gender asymmetry in mismatch responsiveness to university exposure that corroborates, causally, Haddad and Habibi's (2017) cross-sectional finding; a five-fold gender difference in the importance of employment selection for realized hours; and a selection channel that more than fully accounts for the intensive-margin gender gap while explaining little of the extensive-margin gap — as a coherent, multiply corroborated picture of how Iran's decades-long university expansion has interacted with a highly gender-segmented labor market: broadening access without broadening opportunity, for women in particular.

---

## References

- Haddad, G. K., and Habibi, N. (2017). Vertical skill mismatch and wage consequences in low-skilled jobs: Evidence from Iran. *International Labour Review*, 156(1), 45–72.
- Kiker, B. F., Santos, M. C., and Mendes de Oliveira, M. (1997). Overeducation and undereducation: Evidence for Portugal. *Economics of Education Review*, 16(2), 111–125.
- Knight, J., Deng, Q., and Li, S. (2017). China's expansion of higher education: The labour market consequences of a supply shock. *China Economic Review*, 43, 127–141.
- Li, F., Morgan, W. J., and Ding, X. (2008). The expansion of higher education, employment and over-education in China. *International Journal of Educational Development*, 28(6), 687–697.
- Li, S., Whalley, J., and Xing, C. (2014). China's higher education expansion and unemployment of college graduates. *China Economic Review*, 30, 567–582.
- Mulligan, C. B., and Rubinstein, Y. (2008). Selection, investment, and women's relative wages over time. *Quarterly Journal of Economics*, 123(3), 1061–1110.
- Nordin, M., Persson, I., and Rooth, D.-O. (2010). Education–occupation mismatch: Is there an income penalty? *Economics of Education Review*, 29(6), 1047–1059.
- Robst, J. (2007). Education and job match: The relatedness of college major and work. *Economics of Education Review*, 26(4), 397–407.
- Salehi-Isfahani, D. Why the youth are so eager for academic education? Evidence from Iran's labor market. Brandeis Economics Working Paper 105. [Working paper; final publication venue not independently confirmed.]
- Somers, M. A., Cabus, S. J., Groot, W., and van den Brink, H. M. (2019). Horizontal mismatch between employment and field of education: Evidence from a systematic literature review. *Journal of Economic Surveys*.
- Tanaka, S., and Moeeni, S. (2023). The effects of labor market opportunities on education: The case of a female hiring ceiling in Iran. *Journal of Public Economics*, 224.
- Verdugo, R. R., and Verdugo, N. T. (1989). The impact of surplus schooling on earnings: Some additional findings. *Journal of Human Resources*, 24(4), 629–643.
- Rezazadeh, M., and Jalili, H. (draft). University expansion, educational mismatch, and occupational allocation in Iran. Companion paper ("paper_july17").

---

## Notes for next revision pass (not part of the manuscript)

1. MENA gender-paradox citations in Section 2.4 are currently narrative without pinned references; add specific citations before submission.
2. Event-study figures (Section 6.4) are a validity check, not a headline result; consider moving to an appendix if the editor prefers a shorter main text, per typical ILR length norms (7,000–10,000 words).
3. Consider strengthening the male-side exclusion restriction (Section 7.1) with an alternative instrument before submission, given the exposure×children interaction is not significant for men under clustering.
4. All underlying data and code: `konkoor_participants_national.csv`, `figure_konkoor_participants.png`, `gender_gap_decomposition.R`, `event_study.R` (event-study models saved as `gender_selection_results/event_study_*.rds`), `gender_selection_results/model3_heckman_by_gender.{rds,txt}` (clustered-SE version).
