# Revision summary — identification / referee response

**Deliverable:** `draft_finalized/draft_finalized.pdf` (36 pp, compiles clean, 0 undefined
references or citations). Sources: `draft_finalized.tex` (copy of `main.tex`, abstract byte-identical),
`body_finalized.tex`, `appendix_finalized.tex`, `references.bib`, `fig1–4.png`.
**`main.tex` and `body.tex` on Overleaf were not touched.** All new estimates come from
re-running regressions on `data_ready.dta` (scripts in `scripts/`, outputs in `results/`).

The revision keeps the manuscript's voice, structure, notation, table style, and citation style.
Textual changes are confined to: one clause in §3.1, one new 2-paragraph subsection (§5.2),
two sentences in §5.1, three sentences in §5.4, one magnitude paragraph in §5.4, one new
robustness subsubsection (§5.7.2) with one table, one sentence in the §5.7.3 migration paragraph,
two sentences in the Conclusion, one significance star in Table 5, and a new appendix (9 tables).

## Issue-by-issue

| # | Referee item | Action taken | File / script | Main result | Manuscript location | Status |
|---|---|---|---|---|---|---|
| 0 | Audit project before editing | Full recursive audit; pipeline mapped; backup of current Overleaf source | `empirical_audit.md`, `overleaf_current/` | Coded spec = manuscript spec on all 7 checks | — | Completed |
| 2 | Verify existing model | Re-estimated Tables 1, 2, 5, 6 | `rev01_audit_reproduce.R` | Table 1 exact; Tables 2/5/6 exact on unrestricted full-period benchmark | §Audit | Completed |
| 3 | First-stage / treatment validation | `tertiary`, `education_rank`, `literacy` on log exposure; age & gender splits; FE variations | `rev02_priority1.R`, `rev03_firststage.R` | **Null**: −0.15 pp (p=0.36); null at every age cut and both genders | New §5.2, Table A3 | Completed |
| 4 | Province × survey-year FE | Added `prov^yr + cohort` FE for all main outcomes | `rev02_priority1.R` (block A) | All results robust (horiz 0.614→0.568, double 0.688→0.661, fully −0.346→−0.322; overed 0.330→0.338) | §5.7.2, Table 6 | Completed |
| 5 | Province-specific cohort trends | Added `prov[cohort]` varying slopes | `rev02_priority1.R` (block A) | Horiz/double/fully **robust and larger**; **overeducation & undereducation lose significance, flip sign** | §5.7.2, Table 6; Conclusion | Completed |
| 6 | Post-treatment selection / unconditional outcomes | Built population-level indicators (tertiary×employed×mismatch over the full sample) | `rev02_priority1.R` (block D) | Population incidence effects small (+0.04 pp, p≈0.10–0.14) — much below conditional estimates; conditional measures kept primary | §5.2 (ref), Table A4 | Completed |
| 7 | Decompose the mechanism | Reduced-form chain shown across exposure → attainment (null) → employment (null) → allocation (positive) → mismatch (positive); no formal mediation claimed | new §5.2 + existing §5.3–5.5 | Adjustment is in allocation, not attainment or employment | §5.2, Conclusion | Completed |
| 8 | Treatment measurement | Confirmed only enrollment (not seats/capacity) exists; language already accurate ("exposure to local higher-education opportunity"); §3.1 clause on attainment softened | `empirical_audit.md`; body §3.1 | No treatment change; one clause softened | §3.1 | Completed |
| 9 | Student mobility / spatial exposure | No sub-province geography and no cross-province attendance flows in the LFS or the exposure panel | `revision_blocked_tasks.md` #2 | Cannot implement; §3.1/§5.2 note that Konkoor is national and enrollment need not be local | §3.1, §5.2 | Not feasible with current data |
| 10 | Historical province & migration | Verified variable assignment; reproduced Table 6 exactly; added coverage figure (~0.3% of joint sample are identified movers) | `rev04_mig_frozen_boot.R` | Table 6 exact; movers are 0.3% of sample | §5.7.3 | Completed |
| 11 | Frozen vertical benchmark | Modal education rank by ISCO-2 from survey years 1386–91 only, applied to all | `rev04_mig_frozen_boot.R` | Overeducation 0.366\*\*\*, double 0.705\*\*\*, fully −0.298\*\* — main results robust | Table A5 | Completed |
| 12 | Horizontal-mismatch crosswalk | Located `horizontal_broad/strict_isco2_list` (row-level crosswalk in `data_ready.dta`) and the construction in `code_July10.R`; broad/strict robustness already in Table 5 | `empirical_audit.md` | Crosswalk is author-constructed (ISCED broad → ISCO-2); documented | — | Partially completed — see blocked #3 |
| 13 | Formal gender-difference tests | `Y ~ logexp*female` interaction models | `rev02_priority1.R` (block C) | Double p=0.008 ✓, fully matched p=0.038 ✓, **horizontal p=0.32 (n.s.)**, overed p=0.71 | §5.4, Table A7 | Completed |
| 14 | Small-cluster inference | One-way clustering (province, cohort) from repo; leave-one-province-out range; wild bootstrap attempted | `rev05*_wildboot.R`; `step16` outputs | Horiz/double/fully significant under every one-way and leave-one-out sample; wild bootstrap did not converge on this hardware | Table A6 | Partially completed — see blocked #4 |
| 15 | Province × cohort (× year) cell-level | Collapsed to prov×cohort×year cells, WLS with population weights | `rev02_priority1.R` (block F) | Estimates match individual-level (horiz 0.590, double 0.639, fully −0.315) — precision not an N artefact | Table A8 | Completed |
| 16 | Economic magnitudes | SD (0.76) and IQR (1.32) of log exposure; per-SD and per-IQR effects; outcome means + N added to appendix | `rev02_priority1.R` (block E) | 1 SD → +4.9 pp horiz, +5.5 pp double, −2.8 pp fully | §5.4 paragraph, Table A1 | Completed |
| 17 | Table verification / known typo | Re-checked every Table 1–6 cell; verified the flagged Table 5 issue | `rev01`, `rev04` | **Table 5 double-mismatch preferred row: `0.695*` → `0.695***`** (p=3.2e-5); `references.bib` `ven@article` typo fixed | Table 5; references.bib | Completed |
| 18 | Appendix | New 9-table appendix in the manuscript's format | `appendix_finalized.tex` | A1 samples/means, A2 exposure, A3 first stage, A4 unconditional, A5 frozen benchmark, A6 few-cluster, A7 gender tests, A8 cell-level, A9 field detail | Appendix A | Completed |
| 19 | Field-of-study heterogeneity | Verified all field estimates; surfaced N, FDR p-values, and formal cross-field tests | `step15B` outputs | Business/law vs education = +1.31 pp\*\*\* (horiz), +1.47 pp\*\*\* (double) — matches §5.5 exactly | §5.5 (ref), Table A9 | Completed |
| 20 | Occupational decomposition additivity | Checked Σ occupation effects = aggregate | `occupation_coefficient_decomposition_check.csv` | Additive to 1e-13 (men 0.797, women 0.368) — verified | §5.4 (unchanged) | Completed |
| 21 | Literature / citation updates | (a) Web-checked working papers for published versions; (b) targeted search for missing Iran-mismatch and expansion→mismatch literature; verified every `\citet` key resolves (20 cited = 20 in `.bib`, 0 undefined, 0 unused) | `references_finalized.bib`, `body_finalized.tex` §2 | 5 entries updated: Cassidy & Gaulke → *Econ. Inquiry* 62(2), 2024; Li et al. → *JHR* 60(1), 2025 (was `n.d.`); Chen & Ebadi → *JDE* 178, 2026; Rudakov et al. → *J. Educ. Work* 35(6–7), 2022; Leuven & Oosterbeek → *Handbook Econ. Educ.* 4, 2011. **2 entries added** to fill gaps a referee would flag: Keshavarz Haddad & Habibi (2017, *Int. Labour Review* — the Iran-specific over-education/wage study, incl. higher female odds) and Li, Morgan & Ding (2008, *Int. J. Educ. Dev.* — expansion → over-education in China, the "expansion can itself raise mismatch" link) | §2 (2 sentences added), references_finalized.bib | Completed |
| 22 | Reframe contribution | No "first ever" claim present; existing three-literature framing kept verbatim | — | No change needed | Introduction (unchanged) | Completed |
| 23 | Causal-language audit | Existing language already hedged ("associated with", "the estimates indicate", "reduced-form"); added first-stage caveat to §3.1 and Conclusion; no broad softening (results are robust) | body §3.1, Conclusion | Minimal, targeted | §3.1, Conclusion | Completed |
| 24–25 | Preserve style / interpretations | Every edit is an insert or a ≤2-sentence change; interpretations retained verbatim where results unchanged | body_finalized.tex diff | — | throughout | Completed |
| 26 | Compilation / QC | pdflatex + bibtex + pdflatex ×2 | — | 36 pp, 0 undefined refs/citations, no bad overfull boxes | — | Completed |

## Results that materially changed

1. **First stage is null.** The manuscript's §3.1 previously listed "changes in educational
   attainment" as a channel of the exposure effect. Re-estimation shows exposure does **not**
   predict individual tertiary attainment once province and cohort fixed effects are included
   (−0.15 pp, p = 0.36; null at every age restriction and for both genders). The project's own
   `step14_gender_interaction_results.csv` contains the same result. §3.1 is reworded to say the
   effects operate through the *composition of the graduate workforce and competition among
   graduates* rather than through attainment levels, and a new §5.2 documents this.

2. **Overeducation (vertical mismatch) is fragile to province-specific cohort trends.** Adding
   a province-specific linear cohort trend drives the pooled overeducation estimate from
   0.330\*\*\* to −0.062 (n.s.) and undereducation from −0.159\* to 0.052 (n.s.). The
   horizontal-mismatch, double-mismatch, and fully-matched-employment estimates *survive and
   strengthen* under the same specification, and all results are stable under province × survey-year
   fixed effects. The Conclusion now notes that the within-vertical compositional shift should be
   treated more cautiously than the field-mismatch results. This is a demanding specification
   (29 cohorts, 28 province-specific trends), so it is presented as a lower bound.

3. **The gender difference in horizontal mismatch is not statistically significant.** The
   manuscript reported a large male estimate (0.840\*\*\*) and a small insignificant female
   estimate (0.073) for horizontal mismatch. A formal interaction test gives p = 0.32, so §5.4
   now states the difference in point estimates "should be interpreted with caution." The gender
   differences for **double mismatch (p = 0.01)** and **fully matched employment (p = 0.04)** *are*
   significant, and the manuscript's claims for those outcomes are unchanged.

4. **Table 5 significance correction.** The preferred-benchmark double-mismatch estimate is
   `0.695***` (p = 3.2e-5), not `0.695*` as printed.

Everything else — Table 1, the main mismatch magnitudes, the occupational decomposition, the
field-of-study results, the migration/Tehran robustness, the descriptive figures — reproduces and
is unchanged.

## Note for the authors on the abstract (`main.tex`, not edited)

The abstract states "The increases in horizontal and double mismatch are considerably larger among
men." This is formally supported for **double** mismatch (interaction p = 0.01) but **not** for
**horizontal** mismatch (p = 0.32). A minimal fix would be "The increase in double mismatch is
larger among men, and employed women exhibit substantially better observed education–job matching."
Left to the authors since `main.tex` was out of scope.
