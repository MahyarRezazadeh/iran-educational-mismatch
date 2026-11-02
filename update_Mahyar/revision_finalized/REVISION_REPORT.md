# REVISION_REPORT.md — *University Expansion, Educational Mismatch, and Occupational Allocation in Iran*
### Second-round revision for MDPI *Economies* (Special Issue "Returns to Education in Changing Economies")

Companion documents: `empirical_audit.md` (Phase-1 reproduction & spec audit), `GEOGRAPHY_AUDIT.md`,
`LITERATURE_REVISION.md`, `revision_blocked_tasks.md`, `CHANGELOG_REVISED_PAPER.md`.
All numbers below are produced by code in `scripts/` (`rev01`–`rev06`); outputs in `results/`.

---

## Executive summary

**What changed.** The empirical core is unchanged; the revision (i) corrects two genuine mistakes
— the economic-magnitude calculation used the raw SD of log exposure rather than the residualized
(identifying) SD, and the gender-difference test did not match the nuisance structure of the
separate-sex regressions; (ii) adds a full age-restriction robustness table (all outcomes, post-treatment
samples); (iii) makes the geographic-exposure definition explicit and honest; (iv) updates the
literature and the framing of the contribution; and (v) rewrites the abstract, magnitude discussion,
and gender claims to match the corrected evidence. MDPI back-matter (author contributions, data
availability, reproducibility statement) was added.

**Is the paper stronger?** Yes. The corrected gender test *strengthens* the male/female claim for
horizontal mismatch (now p = 0.001, previously reported as "not significant" under a mis-specified
test). The corrected magnitudes are smaller and honest. The age-restriction table shows the central
results are not an artefact of pre-treatment observations. The identification is now described
precisely rather than as "known local access at ages 18–20."

**Which original claims survive.**
- Horizontal mismatch, double mismatch, and fully-matched-employment relationships with exposure —
  **survive** province × year FE, province-specific cohort trends, age ≥ 25/30, alternative
  benchmarks and crosswalks, migration/Tehran exclusions, cell-level estimation, and one-way /
  leave-one-out inference.
- The male/female difference for **horizontal mismatch** and **fully matched employment** —
  **survives** the properly specified test (p = 0.001 and p = 0.019).
- Null effects on employment, LFP, unemployment, and individual tertiary attainment — **survive**.
- Business/administration/law as the field with the most robust deterioration — **survives** FDR.
- Occupational-decomposition additivity — **survives** (exact to 1e-13).

**Which original claims do NOT survive.**
- "Overeducation rises by 0.335 pp" as a *headline, equally weighted* result — **downgraded**.
  It is smaller under the reproducible benchmark (0.330), attenuates to ≈ 0.25 at age ≥ 25, and
  **does not survive province-specific cohort trends** (flips to −0.06, n.s.). Now presented as a
  compositional shift within vertical mismatch, secondary to the field/joint-mismatch results.
- "The male/female difference for double mismatch is significant (p = 0.01)" — **downgraded** to
  "significant only at the 10 percent level (p = 0.09)" under the properly specified test.
- "A one-SD increase in exposure raises double mismatch by ~5.5 pp" — **corrected** to ~1.0 pp
  using the residualized SD; the raw-SD figure overstated the identifying variation ~5×.
- "Treatment is the local access the individual faced at ages 18–20" — **corrected**; exposure is
  a province–cohort measure assigned using harmonized *current* province of residence.

---

## Referee issue 1 — Identification / exogeneity of exposure

**Action.** (a) Traced the full code path from raw LFS geography to exposure (`GEOGRAPHY_AUDIT.md`).
(b) FE ladder: baseline → province × survey-year FE → province-specific linear cohort trends → both
(`rev02A`). (c) Cell-level (province × cohort × year) WLS (`rev02F`). (d) Variance decomposition of
log exposure.
**Result.** Province + cohort + year FE absorb **96%** of the variance of log exposure (not the 92%
previously cited from a prov+cohort-only decomposition). Horizontal / double / fully-matched results
are stable under province × year FE (0.614→0.568, 0.688→0.661, −0.346→−0.322) and *larger* under
province-specific cohort trends (0.748, 0.882, −0.258, all p < 0.01). Cell-level estimates match the
individual-level estimates, so precision is not an N artefact. Overeducation collapses under
province-specific cohort trends.
**Implication.** The field/joint-mismatch design is credible for a reduced-form / conditional-association
reading; causal language is used sparingly and the manuscript now leads with "is associated with" /
"predicts." Exposure is not claimed to be a clean capacity shock.

## Referee issue 2 — Null education first stage

**Action.** Re-estimated tertiary attainment, attainment rank, and literacy on log exposure with the
FE ladder and by age (≥ 23/25/28/30) and gender (`rev02B`, `rev03`). Also the project's own
`step14_gender_interaction_results.csv`.
**Result.** Once province + cohort FE are included, exposure does **not** predict tertiary attainment:
−0.15 pp (p = 0.36) baseline; −0.05 (p = 0.71) at age ≥ 25; null for both genders; attainment rank
+0.006 rank points (n.s.). Province-FE-only gives +0.79 pp*** (endogenous cross-province level).
**Implication.** New Section 5.2 ("University exposure and educational attainment") reports this as a
central result. Section 3.1's clause on "changes in educational attainment" was removed. The estimated
relationship is framed as the effect of the *local graduate-market environment a cohort faced*
(graduate density, competition), not of individual schooling. Population-level (unconditional)
mismatch estimates (Appendix Table A4) are near zero, consistent with this.
COMPLETED.

## Referee issue 3 — Treatment timing / age restrictions

**Action.** Age-at-survey is `age` (already in the data). Re-estimated all nine outcomes on
baseline / age ≥ 21 / ≥ 25 / ≥ 30 (`rev06`, Appendix Table A10).
**Result.** Horizontal mismatch (0.614 → 0.605 → 0.621 → 0.590), double mismatch (0.688 → 0.680 →
0.679 → 0.684) and fully matched (−0.346 → … → −0.346) are **essentially identical** across all age
restrictions. Overeducation attenuates (0.330 → 0.309 → 0.249** → 0.220**) but stays significant.
Employment, LFP, unemployment, tertiary attainment remain small and insignificant.
**Implication.** The central results are not driven by pre-treatment observations. The horizontal /
joint-mismatch samples are already restricted to employed tertiary workers, who are necessarily
past ages 18–20; the age table confirms this. The manuscript now states that outcomes are observed
post-treatment and that age ≥ 25 preserves the central results.
COMPLETED.

## Referee issue 4 — Gender heterogeneity test specification

**Action.** Built a pooled interaction model that interacts `female` with log exposure, with
province, cohort, and survey-year FE, and with the urban control — so the specification
reproduces the separate-sex regressions exactly. The coefficient on `logexp × female` is then a
clean between-gender test (`rev06` Phase 8, Appendix Table A7).
**Result.** The properly specified test differs from the earlier (mis-specified) one:
| Outcome | Male | Female | Diff (F−M) | p |
|---|---|---|---|---|
| Overeducation | 0.300 | 0.479 | +0.179 (0.213) | 0.41 |
| Undereducation | −0.157 | −0.181 | −0.024 (0.144) | 0.87 |
| Any vertical | 0.142 | 0.297 | +0.155 (0.192) | 0.43 |
| **Horizontal mismatch** | 0.840 | 0.073 | **−0.767 (0.213)** | **0.001** |
| Double mismatch | 0.784 | 0.367 | −0.417 (0.236) | 0.089 |
| **Fully matched employment** | −0.519 | 0.094 | **+0.613 (0.245)** | **0.019** |
| Employment | −0.052 | −0.167 | −0.115 (0.268) | 0.67 |
| LFP | 0.078 | −0.161 | −0.239 (0.255) | 0.36 |
**Implication.** For **horizontal mismatch** and **fully matched employment** the male/female
difference *is* statistically significant — so the earlier softening (based on p = 0.32 from a test
that did not match the separate-sex nuisance structure) was **reversed**. Double mismatch is now
"significant only at the 10 percent level." Overeducation and the employment outcomes show no
significant gender difference. Abstract, §5.4, §5.7, and the Conclusion were rewritten accordingly.
COMPLETED.

## Referee issue 5 — Economic-magnitude calculation (residualized exposure)

**Action.** Residualized log exposure on the identifying FE (survey-weighted) and computed the SD
and IQR of the *residuals* on the relevant samples (`rev06` Phase 7).
**Result.** Raw weighted SD ≈ 0.75, raw IQR ≈ 1.3. **Residualized SD ≈ 0.14, residualized IQR ≈
0.19** (the FE absorb ≈ 96% of the variance). Per-residualized-SD effects: horizontal +0.93 pp,
double +1.04 pp, fully matched −0.52 pp, overeducation +0.49 pp.
**Implication.** The §5.4 magnitude paragraph and Appendix Table A2 were rewritten to use the
residualized moments. The previously reported "≈ 5 pp per SD" figures overstated the identifying
variation by roughly a factor of five and have been removed.
COMPLETED.

## Referee issue 6 — Stronger FE / province-specific cohort trends and vertical-mismatch fragility

**Action.** See issue 1. Manuscript §5.7.2 + Table 6.
**Result.** Field / joint-mismatch results survive and strengthen; overeducation and undereducation
lose significance and change sign under province-specific linear cohort trends (a demanding spec
with 29 cohorts — presented as a lower bound).
**Implication.** Overeducation downgraded in abstract, §5.4, §5.7.2, and Conclusion; horizontal and
double mismatch are the central findings.
COMPLETED.

## Referee issue 7 — Exposure measure decomposition, placebo windows, supply-side treatment

**Action / result.**
- **Alternative age windows / numerator vs denominator / placebo windows:** the project's
  `step20_timing_placebo` outputs already contain exposure at ages 15–17, 18–20, 21–23, 24–26 and a
  joint model. The ages-18–20 window is the strongest; a "future" window (ages 24–26) is comparatively
  clean. Earlier placebo windows (ages 10–15) are **not valid** here: the enrolment panel starts in
  1373, so for the older cohorts (born 1355–1360) the ages-10–15 window falls before the panel begins.
  Documented rather than fabricated. PARTIALLY COMPLETED (existing outputs surfaced; no new
  numerator/denominator split run this pass — logged in `revision_blocked_tasks.md`).
- **Supply-side treatment (university openings / seats / capacity):** confirmed in
  `empirical_audit.md` that Iranian public data contain **no** province-year "available seats" or
  admission-capacity series; only enrolment and graduates. `data/universities.xlsx` and
  `university_list.xlsx` are institution lists without dated openings usable as a province-year panel.
  A campus-count treatment is not reliably constructable. NOT FEASIBLE WITH AVAILABLE DATA — the
  manuscript language was already changed to "exposure to local higher-education opportunity" /
  "university enrollment exposure" rather than a capacity shock.

## Referee issue 8 — Confounding / pre-trends

**Action.** Searched the repo for province-cohort predetermined covariates. `data/panel_data_1373_1403.xlsx`
and `population_panel_1373_1403.xlsx` contain enrolment and population; there is **no** consistent
province-year series for unemployment, GDP/income, sector composition, or secondary completion at
the historical-province level across 1373–1403.
**Result.** Cannot run a formal "exposure predicts predetermined province-cohort conditions"
regression with genuine covariates. The FE ladder (issue 1/6) is the substitute: province × year FE
absorb *any* contemporaneous province condition common to all cohorts, and province-specific cohort
trends absorb smooth differential province paths. The field/joint-mismatch results survive both.
PARTIALLY COMPLETED — no external province covariates available; logged.

## Referee issue 9 — Vertical-mismatch robustness and the occupation benchmark

**Action.** Documented the benchmark construction (`empirical_audit.md` §3). Ran preferred / unrestricted
full-period / period-specific / **frozen early-year** benchmarks (`rev01`, `rev04`; Table 5 + Appendix
Table A5).
**Result.** Double mismatch 0.68–0.70 across all six preferred/full-period/period-specific
combinations; frozen benchmark (modal education by ISCO-2 from survey years 1386–91 only) gives
overeducation 0.366***, double 0.705***, fully matched −0.298** — the results are not mechanically
driven by later expansion. A machine-readable *occupation × required-rank × cell-count* appendix
table is **not** produced this pass (the `*_final` reliability variables were not exported to
`data_ready.dta`; see `revision_blocked_tasks.md`). PARTIALLY COMPLETED.

## Referee issue 10 — Horizontal-mismatch crosswalk reproducibility

**Action.** Located the crosswalk: it is stored *row-by-row* in `data_ready.dta` as
`horizontal_broad_isco2_list` / `horizontal_strict_isco2_list` (the set of ISCO-2 codes deemed
related to each worker's ISCED field) and constructed in `code_July10.R` block 5.
**Result.** The crosswalk is **author-constructed** (broad ISCED fields → two-digit ISCO groups); no
external ILO/OECD source. Broad and strict differ in inclusiveness. Broad vs strict on identical
samples: double mismatch 0.688 (broad) vs 0.679 (strict); the difference is classification, not
sample. A standalone `appendix_crosswalk_broad.csv` / `_strict.csv` extraction is **not** produced
this pass and is logged. Table A9 reports field-level N and FDR. PARTIALLY COMPLETED.

## Referee issue 11 — Occupational decomposition and multiple testing

**Action.** Verified additivity (`occupation_coefficient_decomposition_check.csv`: Σ = aggregate to
1e-13). The main-text Table 3 reports the five largest positive contributors per gender with
conventional stars, labelled as descriptive.
**Result / gap.** A *full* occupation-set appendix table with BH q-values and bootstrapped
contribution-share uncertainty is **not** produced this pass. The existing `step24` output has all
occupation coefficients/SEs but no FDR column. Logged. PARTIALLY COMPLETED.

## Referee issue 12 — Field-of-study heterogeneity

**Action.** Reproduced from `step15B`; Table A9 reports N, within-outcome FDR, and cross-field
differences.
**Result.** Only business/administration/law survives FDR on both horizontal and double mismatch
(FDR p = 0.009 / 0.008; difference from education +1.31*** / +1.47***). ICT survives the
within-outcome adjustment only (FDR p ≈ 0.10) and is observed over 7 years — flagged as suggestive.
A field × survey-year coverage table is **not** added this pass; the ICT caveat is in the table
note and text. COMPLETED (core); coverage table logged.

## Referee issue 13 — Few-cluster inference

**Action.** One-way clustering by province (28) and by cohort (29) from
`step16_main_inference_results.csv`; leave-one-province-out range from
`step16_leave_one_province_out_summary.csv`; a restricted wild cluster bootstrap was implemented
three ways (`rev04`, `rev05*`). Appendix Table A6.
**Result.** Horizontal and double mismatch: p < 0.001 under every one-way clustering; significant at
5% in all 28 leave-one-province-out samples. Fully matched: p ≤ 0.003; significant in 27/28. The
wild bootstrap **did not converge** in usable time on the full samples on the available hardware
(8 GB RAM; `fwildclusterboot`/`boottest` are not available for R 4.6.1). The one-way + leave-one-out
results serve as the few-cluster check and are reported honestly.
PARTIALLY COMPLETED — logged; `boottest` after `reghdfe` on adequate hardware would close it.

## Referee issue 14 — Cell-level / effective sample robustness

**Action.** Province × cohort × survey-year WLS with cell population weights (`rev02F`, Table A8).
**Result.** Cell-level and individual-level estimates match (horizontal 0.590 vs 0.614; double 0.639
vs 0.688; fully matched −0.315 vs −0.346; SEs comparable). Precision is not an artefact of the
individual N relative to ~11,000 cells / 28–29 clusters. COMPLETED.

## Referee issue 15 — Literature and contribution

See `LITERATURE_REVISION.md`. Five working papers already cited were updated to their published
versions; five new papers were verified against publisher pages and incorporated: Keshavarz Haddad
& Habibi (2017), Li, Morgan & Ding (2008), Karimi & Taghvatalab (2020), Moeeni & Tanaka (2023),
Su (2025). The contribution is reframed away from "expansion causes overeducation" toward
province–cohort evidence on vertical *and* horizontal mismatch, their joint occurrence, occupational
allocation, field heterogeneity, and gender-specific selection. COMPLETED.

## Referee issues 16–19 — Claims/evidence match, abstract, MDPI material, table QC

- **Causal language / abstract:** rewritten to lead with association/prediction; abstract, magnitude
  discussion, gender claims, and Conclusion updated. Keywords + JEL added. COMPLETED.
- **MDPI back-matter:** Author Contributions, Funding, IRB/consent (not applicable), Data
  Availability, Conflicts of Interest, and a Reproducibility statement added. COMPLETED.
- **Table QC:** the "10 percent increase" uses `100·ln(1.10)` throughout (verified in code and notes).
  Fixed the confirmed Table 5 star typo (`0.695*` → `0.695***`). Table A7 rebuilt. All new tables are
  generated from `rev06`. Persian vs Gregorian years: the figures use Persian survey years on the
  x-axis (1386–1403); the text gives Gregorian equivalents (2007–2024). COMPLETED.
- **MDPI document class:** NOT converted to the `Definitions`/MDPI LaTeX class this pass — the draft
  stays on the current `article` + elsarticle-harv setup. Logged.

---

## Final scientific decision (acting as the referee)

**Verdict: ACCEPTABLE AFTER REVISION**, conditional on the framing being kept at the level the
evidence supports (association / reduced-form for the central results, explicit null first stage,
downgraded overeducation).

1. **Does the age ≥ 25 sample preserve the central results?** Yes — horizontal (0.621***), double
   (0.679***), fully matched (−0.331***) are essentially unchanged; overeducation attenuates to
   0.249** but stays significant.
2. **Are horizontal and double mismatch robust to province-specific cohort trends?** Yes — both are
   if anything larger (0.748, 0.882). Overeducation is not.
3. **Does the correctly specified gender test show significant male/female differences?** Yes for
   horizontal mismatch (p = 0.001) and fully matched employment (p = 0.019); only at 10% for double
   mismatch (p = 0.09); no for overeducation and the employment outcomes.
4. **Does exposure predict tertiary attainment or graduate labor-market pressure?** It does **not**
   predict individual tertiary attainment (−0.15 pp, p = 0.36). Population-level graduate-mismatch
   incidence is small (+0.04 pp, p ≈ 0.10). Province-cohort "graduate pressure" was not constructed
   this pass (logged).
5. **What does geographic exposure measure?** A province–birth-cohort average of undergraduate/
   professional enrolment per 100,000 residents, assigned using the respondent's harmonized *current*
   province of residence and the calendar years the cohort was 18–20. It is not birthplace or
   residence at age 18, and pre-window migration is unobserved (≈ 0.3% of the sample are identified
   movers).
6. **Is the treatment exogenous enough for causal language?** Not cleanly. The design supports a
   credible reduced-form / conditional-association interpretation for the field and joint-mismatch
   outcomes; the manuscript now uses causal verbs sparingly and never for the first-stage or
   overeducation results.
7. **Correct one-SD magnitude (residualized):** horizontal +0.9 pp, double +1.0 pp, fully matched
   −0.5 pp, overeducation +0.5 pp.
8. **Which results survive few-cluster inference?** Horizontal and double mismatch (p < 0.001 under
   one-way clustering both ways; significant in every leave-one-province-out sample). Fully matched
   (p ≤ 0.003; 27/28 leave-one-out). Wild bootstrap not converged.
9. **Which heterogeneity results survive multiple testing?** Field: only business/administration/law
   (FDR both outcomes). Occupation decomposition: additive by construction; treated as descriptive,
   full FDR table pending.
10. **Genuinely novel contribution:** province–cohort evidence from Iran that a large university
    expansion is associated with a shift in the *composition* of education–job matching among
    employed graduates — rising field and joint (vertical + horizontal) mismatch and falling full
    matching — concentrated among men and in specific occupations and fields, with essentially no
    change in employment quantities or individual attainment, in a setting where female employment
    is highly selective.
