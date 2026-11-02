# Empirical audit — *University Expansion, Educational Mismatch, and Occupational Allocation in Iran*

Prepared for the identification/referee revision. All numbers below were produced by re-running
the regressions on the project's own analysis file, not copied from the manuscript.

## 0. Project map

| Component | Location |
|---|---|
| Current (finalised) manuscript source | Overleaf `Mismatch_in_Iran` (`6a7030b268033c6d0a4332fe`): `main.tex` (preamble + abstract), `body.tex` (all text), `references.bib`, `fig1–4.png`. A byte-identical copy is in `revision_finalized/overleaf_current/`. |
| Analysis-ready microdata | `update_Mahyar/data_ready.dta` — 4,128,510 obs × 185 vars (Persian LFS years 1386–1403, birth cohorts 1355–1383, 28 harmonised historical provinces). |
| Construction pipeline | `codes/code_July10.R` (5 mega-blocks: reduce → filter → harmonise provinces → merge exposure → build mismatch, incl. step 5d "refined" benchmark). Downstream regression scripts `code_step14.R … code_step24.R`, `code_baseline.R`, `code_mismatch.R`, `code_finalmismatch.R`, `code_step19.R`. |
| Enrolment / exposure panel | `data/final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv`; also `data/universities.xlsx`, `data/university_list.xlsx`, `data/population_panel_1373_1403.xlsx`. |
| Authoritative result tables | `outputs/step14…step21`. Table 2 of the paper = `step19_alternative_mismatch_regression_results.csv` (overeducation/horizontal) + `step14_selection_joint_regression_results.csv` (double / fully matched). |
| Pipeline working dir (`E:/MismatchPipeline`, `E:/sfu/...`) | **No longer present** — the E: drive is not mounted in the current environment. All revision work runs from `data_ready.dta` on C:. |

Empirical pipeline: raw per-year LFS CSV → `creating_sample.do` (sibling `LFS project-May2026`) →
`data_ready.dta` → `code_July10.R` block 1–5 builds exposure + vertical/horizontal/joint
mismatch → `code_step14/17/18/19/20` run the regressions → `code_step21.R` assembles tables/figures →
manuscript.

## 1. Specification: manuscript vs. code

**Manuscript (eq. 6, §5.1):**
`Y_ipct = β·log(Exposure_pc) + X_i'γ + μ_p + λ_c + τ_t + ε_ipct`,
survey-weighted LPM, `X` = female + urban (pooled) / urban (by gender),
two-way clustered SE by historical province and birth cohort, 28 province + 29 cohort clusters,
10%-effect = `100·ln(1.10)·β̂`.

**Code (`code_step19.R`, `code_step14.R`, `code_baseline.R`):**
`feols(y ~ log_undergrad_exposure_18_20 + female + urban | province_historical + analysis_birth_year + year,
weights = ~weight, cluster = ~province_historical + analysis_birth_year)`.

| Audit check | Result |
|---|---|
| A. Coded spec = manuscript spec | **Yes.** Treatment `log_undergrad_exposure_18_20`; FE `province_historical + analysis_birth_year + year`; controls female + urban; weights `weight`; cluster province + cohort. |
| B. Exposure constant within province × cohort | **Yes.** 0 of 804 non-empty province×cohort cells have any within-cell variation in `log_undergrad_exposure_18_20` (tol 1e-9). |
| C. Log transform | **Yes.** `log_undergrad_exposure_18_20 = ln(undergrad_exposure_18_20)`, defined only where exposure > 0; baseline sample requires all three annual values (`complete_undergrad_exposure == 1`). |
| D. 10%-increase conversion | **Correct.** `100·ln(1.10) = 9.53101798`. For 0/1 LPM outcomes this is the pp change; code applies exactly this factor. |
| E. Clustering | **As stated.** `cluster = ~province_historical + analysis_birth_year`; 28 province clusters, 29 cohort clusters (a handful of the 812 province×cohort cells are empty → 804 populated). |
| F. Survey weights | **Yes.** `weights = ~weight`; sample drops non-positive/missing weights. |
| G. Sample restrictions | Main sample = complete 3-year exposure + weight > 0 + cohort ∈ [1355,1383] + non-missing province/year/gender/urban → **N = 4,086,861** (manuscript "≈ 4.1 million" ✓; 28 provinces, 29 cohorts, 18 years ✓). Outcome samples add: employed + occupation code (vertical), + tertiary + field + ISCO-2 (horizontal), + both (joint). |

No coded specification was found to contradict the manuscript. No code was changed for
stylistic reasons.

## 2. Reproduction of headline numbers

Re-estimated directly from `data_ready.dta` (script `rev01_audit_reproduce.R`).

### Table 1 (employment) — **exact reproduction**

| Outcome | Pooled (paper) | Pooled (rerun) | Women | Men |
|---|---|---|---|---|
| Labour-force participation | −0.035 (0.057) | −0.035 (0.057) | −0.161 / −0.161 | 0.078 / 0.078 |
| Employment | −0.103 (0.085) | −0.103 (0.085) | −0.167 / −0.167 | −0.052 / −0.052 |
| Unemployment | 0.228 (0.147) | 0.228 (0.147) | 0.229 / 0.229 | 0.213 / 0.213 |

### Table 2 (mismatch)

| Outcome | Paper (pooled) | Rerun (pooled) | Notes |
|---|---|---|---|
| Overeducation | 0.335*** | **0.330*** (0.101), p = 0.0028 | = step19 "Full-period vertical" row exactly; "Final preferred" (0.335) needs `*_final` vars — see §3. |
| Undereducation | −0.165* | **−0.159*** (0.093), p = 0.097 | matches step19 "Full-period vertical". |
| Any vertical mismatch | 0.170 | **0.171** (0.110), n.s. | exact. |
| Horizontal mismatch (broad) | 0.614*** | **0.614*** (0.126), p = 4.5e-5 | **exact**; women 0.073 n.s., men 0.840*** — exact. |
| Double mismatch | 0.695*** | **0.688*** (0.140), p = 3.9e-5 | = paper's own Table 5 "unrestricted full-period + broad horizontal" row (0.688***) exactly. |
| Fully matched employment | −0.341*** | **−0.346*** (0.102), p = 0.0022 | = paper's Table 5 "unrestricted full-period" row (−0.346***) exactly. |

### Table 5 / Table 6 — reproduced

- **Table 6 (migration/Tehran)** reproduces to 3 decimals: baseline 0.614 / 0.688 / −0.346; excl. movers
  0.616 / 0.689 / −0.344; excl. Tehran 0.662 / 0.735 / −0.323; excl. both 0.664 / 0.738 / −0.324 —
  all identical to the manuscript (with the same 0.688↔0.695 benchmark offset on double mismatch).
- **Table 5** rows reproduce exactly for the "unrestricted full-period" and "period-specific"
  families; see §3 for the "preferred" family.

## 3. The one reproduction gap: the "final preferred reliable" vertical benchmark

The manuscript's *headline* vertical numbers (overeducation 0.335, double mismatch 0.695, fully
matched −0.341) come from `code_step19.R`, which reads variables `overeducated_final`,
`vertical_fullperiod_eligible_final`, etc. from `lfs_final_analysis_ready.fst`. **Those `*_final`
variables were not exported into `data_ready.dta`** (the 185-var export renamed the *joint* refined
outcomes to `*_refined` but omitted the standalone `*_final` vertical variables and the
`*_final` eligibility flag). In `data_ready.dta`, `vert_fullperiod_reliable_cell` is identically 1
and `occ_observations_fullperiod` has a minimum cell size of 483, so the reliability restriction
that distinguishes "final preferred" (N = 1,619,190 / 392,777) from "unrestricted full-period"
(N = 1,633,647 / 400,726) cannot be reconstructed from the export.

**Consequence:** the reproducible benchmark from `data_ready.dta` is the *unrestricted full-period*
one. It gives overeducation 0.330 (paper 0.335), double mismatch 0.688 (paper 0.695), fully
matched −0.346 (paper −0.341) — i.e. **the paper's own Table 5 robustness rows, reproduced
exactly.** The manuscript's Table 5 already shows the double-mismatch estimate is 0.679–0.703
across all six benchmark/crosswalk combinations, so the headline claims do **not** need to change.

All new analyses in this revision are therefore run on the *unrestricted full-period* benchmark
and reported as deltas from that benchmark's baseline (which is itself in the paper). Recovering
`lfs_final_analysis_ready.fst` (or re-exporting the `*_final` variables from `code_July10.R`
block 5d) would let every new table be stated on the exact "preferred" benchmark; this is logged
in `revision_blocked_tasks.md`.

## 4. New identification evidence (summary — full numbers in `revision_summary.md`)

| Check | Overeducation | Horizontal | Double | Fully matched | Employment |
|---|---|---|---|---|---|
| Baseline (prov + cohort + year FE) | 0.330*** | 0.614*** | 0.688*** | −0.346*** | ≈0 n.s. |
| **+ Province × survey-year FE** | 0.338*** | 0.568*** | 0.661*** | −0.322*** | ≈0 n.s. |
| **+ Province-specific linear cohort trend** | **−0.062 n.s.** | 0.748*** | 0.882*** | −0.258*** | ≈0 n.s. |
| + both | −0.102 n.s. | 0.739*** | 0.869*** | −0.258** | ≈0 n.s. |
| Frozen occ. benchmark (modal educ rank, survey yrs 1386–91 only) | 0.366*** | — | 0.705*** | −0.298** | — |
| Province × cohort × year cell-level WLS | 0.339*** | 0.590*** | 0.639*** | −0.315*** | ≈0 n.s. |

- **Province × survey-year FE:** all results robust (absorbs contemporaneous province labour-market conditions).
- **Province-specific cohort trends:** horizontal / double / fully-matched **robust (and larger)**;
  **overeducation and undereducation lose significance and change sign.** Reported honestly — the
  paper's field/occupational-allocation results survive, the vertical-mismatch result is fragile to
  differential province cohort trends (a demanding specification with only 29 cohorts).
- **Cell-level:** SEs comparable to individual-level → precision is not an artefact of N.
- **First stage — `tertiary_educated ~ log(exposure)`:** province FE only → +0.79 pp*** (cross-province,
  endogenous); **add cohort + year FE → −0.15 pp, p = 0.36**. Null for every age restriction
  (≥23/25/28/30), both genders, and the education-rank and literacy outcomes. The project's own
  `step14_gender_interaction_results.csv` shows the same ("Tertiary educated": −0.14/−0.18 pp,
  n.s.). Exposure captures the *local graduate-market environment a cohort faced*, not that
  cohort's own attainment → §3.1's clause "including changes in educational attainment" is softened.
- **Formal gender-interaction tests** (`Y ~ logexp*female`): double mismatch **p = 0.008** (men
  larger — supported), fully matched **p = 0.038** (supported), horizontal mismatch **p = 0.32
  (not significant)**, overeducation p = 0.71, undereducation p < 0.001. → the "horizontal mismatch
  is concentrated among men" wording is softened to "the point estimate is larger for men, though
  the gender difference is not statistically significant."
- **Unconditional (population-level) mismatch:** incidence of tertiary-employed horizontal
  mismatch +0.044 pp (p = 0.10), double +0.041 pp (p = 0.14) — much smaller than the conditional
  effects and mostly insignificant, consistent with the null first stage. Added as a bound; the
  conditional measures remain primary.
- **Occupational decomposition additivity** (`occupation_coefficient_decomposition_check.csv`):
  Σ occupation-specific effects = aggregate to 1e-13 (men 0.7969, pooled 0.6952, women 0.3676). ✓
- **Cross-field tests** (`step15B_formal_differences_from_education.csv`): business/law vs education
  = **+1.311 pp*** (horizontal, FDR p = 0.048), +1.473 pp*** (double, FDR p = 0.008)** — matches
  the manuscript's §5.5 claim exactly; already computed in the repo, surfaced to the appendix.

## 5. Known typos / inconsistencies found

1. **Table 5 (`tab:robdef`), row 1, double mismatch:** manuscript prints `0.695* (0.140)`. The
   regression output has p = 3.2e-5 → should be `0.695***`. Every other row in the table is `***`.
   **Fixed in `draft_finalized`.** (Referee "known issue" — confirmed.)
2. **`references.bib` line 1:** `ven@article{blundell2022,` — stray `ven` prefix before `@article`
   (a paste artefact). BibTeX tolerates it silently but it is malformed. **Fixed** (`@article{blundell2022,`).
3. **`@unpublished{li_nd}` / `year = {n.d.}`** and six `@techreport` entries (Cassidy & Gaulke 2023,
   Chen & Ebadi 2023, Dang 2024, Redaelli 2024, Domadenik 2013, Leuven 2011, Rudakov 2019): checked
   for published versions — see `revision_summary.md` §Literature.
4. §4.3 of the source docx contained a duplicated paragraph ("The joint mismatch sample requires…");
   it appears once in `body.tex` — no action needed.
5. Abstract (in `main.tex`, not edited): "The increases in horizontal and double mismatch are
   considerably larger among men" — formally supported for **double** mismatch (p = 0.008), **not**
   for horizontal mismatch (p = 0.32). Flagged for the authors; `main.tex` left untouched per instruction.
