# Tasks that could not be completed with the current data / environment

> Round-2 additions (MDPI *Economies*) are in section 6 below; sections 1–5 carry over from round 1.

## 1. Exact "final preferred reliable" vertical benchmark

**What is missing.** The manuscript's headline vertical numbers (overeducation 0.335, double
mismatch 0.695, fully matched −0.341) are produced by `code_step19.R` / `code_step14.R` from
variables `overeducated_final`, `undereducated_final`, `vertically_matched_final`,
`vertical_mismatch_final`, `vertical_fullperiod_eligible_final`, read from
`LFS_analysis_outputs/lfs_final_analysis_ready.fst`. **Those `*_final` variables were not exported
into `data_ready.dta`** (only the *joint* refined outcomes were, renamed to `*_refined`), and the
`E:` drive holding `lfs_final_analysis_ready.fst` is not mounted in this environment.

**Why it cannot be reconstructed from `data_ready.dta`.** In the export, `vert_fullperiod_reliable_cell`
is identically 1 and `occ_observations_fullperiod` has a minimum of 483, so the cell-reliability
restriction that distinguishes "final preferred" (N = 1,619,190 / 392,777) from "unrestricted
full-period" (N = 1,633,647 / 400,726) is not identifiable from the exported columns.

**Closest defensible alternative implemented.** All new analyses use the *unrestricted full-period*
benchmark, whose baseline (overeducation 0.330, double 0.688, fully matched −0.346) is **the
manuscript's own Table 5 robustness row and reproduces exactly**. Table 5 already shows the
double-mismatch estimate is 0.679–0.703 across all six benchmark/crosswalk combinations, so no
headline claim changes. Every new appendix table notes that the preferred-benchmark point
estimates differ by less than 0.01 pp.

**To close it.** Re-run `code_July10.R` block 5d (or locate `lfs_final_analysis_ready.fst`) and
re-export the five `*_final` variables into `data_ready.dta`; then the new appendix tables can be
re-estimated on the exact preferred benchmark by changing the outcome/eligibility variable names
in `rev02_priority1.R`.

## 2. Student mobility / cross-province university attendance

**What is missing.** The LFS records province of residence (current and previous) but not where an
individual attended university, and the enrollment/exposure panel is province-year with no
origin–destination flows or inter-province distances. There is no sub-province (shahrestan)
identifier anywhere in the pipeline.

**Consequence.** Referee item 9 (neighbouring-province exposure, regional exposure, distance-weighted
exposure, own- vs other-province exposure) cannot be implemented without fabricating an adjacency
matrix or coordinates. Not done.

**What was done instead.** §3.1 and the new §5.2 state explicitly that admission is organized through
the national Konkoor examination, so a province's enrollment rate need not reflect the attainment
or university location of its birth cohort — this is offered as the reason the first stage is null,
and as a limitation of interpreting exposure as *local* opportunity.

## 3. Full field-to-occupation crosswalk appendix table

**Status: partial.** The broad and strict crosswalks are stored row-by-row in `data_ready.dta`
(`horizontal_broad_isco2_list`, `horizontal_strict_isco2_list` — the set of ISCO-2 codes deemed
related to each worker's ISCED field) and are constructed in `code_July10.R` block 5. They are
author-constructed (mapping ISCED-2013 broad fields to two-digit ISCO groups), not taken from an
external ILO/OECD source. Appendix Table A9 reports field sample sizes, FDR-adjusted p-values and
cross-field tests, and the manuscript states the crosswalk is author-constructed, but a full
field-by-ISCO2 matrix was **not** typeset as a table. To add it: extract the unique
`education_field_harmonized × horizontal_broad_isco2_list` pairs from `data_ready.dta` (about 10
fields × up to ~15 ISCO-2 codes each) and format as an appendix table.

## 4. Wild cluster bootstrap

**Status: attempted, not converged.** A restricted (imposed-null) wild cluster bootstrap by
province and by cohort was implemented (`rev05_wildboot.R`, `rev05b_wildboot.R`, `rev05c_wildboot.R`,
Rademacher and Webb weights). `fwildclusterboot` / `boottest` are not available for R 4.6.1, so
the bootstrap runs a manual `feols` refit per replication. On this machine (8 GB RAM, ~1 GB free
during the run) each replication takes seconds and 999 (or even 199) replications × 6–8 models did
not finish in a usable time; the loop was memory-bound.

**What was used instead (Appendix Table A6).** One-way cluster-robust p-values by province
(28 clusters) and by cohort (29 clusters), both < 0.001 for horizontal and double mismatch and
≤ 0.008 for overeducation (from the repo's `step16_main_inference_results.csv`), plus the
leave-one-province-out coefficient range from `step16_leave_one_province_out_summary.csv` (every
one of the 28 leave-one-out samples keeps the sign and 5% significance for horizontal and double
mismatch; 27/28 for fully matched). These are legitimate few-cluster checks.

**To close it.** Run `boottest` (via a compatible R build) or Stata `boottest` after `reghdfe` on
`data_ready.dta`; ~10 minutes on adequate hardware.

## 5. Re-running the full construction pipeline

`code_July10.R` (11k lines, 5 blocks) rebuilds exposure and all mismatch measures from raw LFS
CSVs. It was **not** re-run: it needs the raw per-year CSVs and several hours, its hard-coded
`E:/sfu/...` paths do not exist here, and the verified `data_ready.dta` already contains its
output. The revision works entirely from `data_ready.dta`.

---

# 6. Round-2 items not completed this pass (MDPI *Economies*)

## 6.1 MDPI document class
The draft stays on `\documentclass[11pt]{article}` + `elsarticle-harv` bibliography. It was **not**
converted to the MDPI `Definitions/mdpi` LaTeX class / `mdpi.cls` + `chicago.bst`. MDPI back-matter
(Author Contributions, Funding, IRB/Consent, Data Availability, Conflicts of Interest,
Reproducibility statement) has been added to the `article` layout so the conversion is mechanical
when the journal template is dropped in.

## 6.2 Typeset field-to-occupation crosswalk table (Phase 10)
Still open — same as section 3 above. `appendix_crosswalk_broad.csv` / `_strict.csv` were not
extracted from `data_ready.dta` and no field x ISCO-2 matrix table was added. Table A9 gives
field-level N and FDR p-values in place of the raw crosswalk.

## 6.3 Full occupation-set decomposition table with BH q-values + bootstrapped shares (Phase 11)
Table 3 reports the five largest positive contributors per gender with conventional stars, labelled
descriptive, and additivity is verified to 1e-13. A complete occupation x coefficient x FDR-q x
bootstrapped-contribution-share appendix table was **not** produced. `step24` has the coefficients
and SEs but no FDR column.

## 6.4 Field x survey-year coverage table (Phase 12)
Not added. The ICT caveat ("observed over seven survey years") is stated in the Table A9 note, the
field-heterogeneity text, and REVISION_REPORT.md issue 12, but a field x year availability matrix
was not typeset.

## 6.5 Exposure numerator/denominator split; new placebo windows; supply-side treatment (Phase 5)
- Numerator (enrolment) vs denominator (population) split of log exposure: not run this pass.
- New placebo windows beyond the project's existing `step20_timing_placebo` (ages 15-17, 18-20,
  21-23, 24-26): none added. Ages 10-15 placebo is invalid for the older cohorts (enrolment panel
  starts 1373) and was not fabricated.
- Supply-side university-opening / seats / capacity treatment: **not feasible** — Iranian public
  data have no province-year admission-capacity or dated-openings series (only enrolment and
  graduates). Confirmed in `empirical_audit.md`. Manuscript language uses "exposure to local
  higher-education opportunity", not a capacity shock.

## 6.6 Wild cluster bootstrap convergence
Unchanged from section 4 — attempted three ways, memory-bound on 8 GB RAM, `fwildclusterboot` /
`boottest` unavailable for R 4.6.1. One-way clustering (province: 28, cohort: 29) and
leave-one-province-out (Appendix Table A6) serve as the few-cluster check. Close with Stata
`boottest` after `reghdfe`, ~10 min on adequate hardware.

## 6.7 Province-cohort "graduate pressure" first stage (Phase 4, partial)
The national descriptive graduate-pressure ratio (Figure 3, ~1.0 -> ~1.45-1.50) is in the paper.
A province x cohort graduate-density / graduate-pressure variable regressed on exposure was **not**
constructed — there is no province-year graduate-stock series at the historical-province level.
Population-level (unconditional) mismatch incidence (Appendix Table A4, +0.04 pp, p ~ 0.10) is the
substitute evidence that exposure does not move aggregate graduate outcomes.

## 6.8 External province-cohort predetermined covariates (Phase 8 / pre-trends)
No consistent historical-province series for unemployment, GDP/income, sector composition, or
secondary completion across 1373-1403 exists in the pipeline, so a formal "exposure predicts
predetermined province-cohort conditions" regression with real covariates cannot be run. The FE
ladder (province x survey-year FE; province-specific linear cohort trends; Table 6 / Appendix
Table A_feladder) is the substitute and the field/joint-mismatch results survive it.

## 6.9 REVISION_AUDIT.md as a separate file
Not created as a distinct file. `empirical_audit.md` (round 1, Phase-1 reproduction and
specification audit) plays this role and is cross-referenced from REVISION_REPORT.md.
