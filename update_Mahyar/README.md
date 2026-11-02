# update_Mahyar contents

## Main paper deliverable
- `data_ready.dta` — full analysis-ready dataset (4,128,510 obs x 185 vars) reproducing the
  Mismatch Paper's results. See `variable_rename_map.csv` for the 16 variables shortened to fit
  Stata's 32-character name limit.

## Gender-gap / selection-into-employment extension
- `build_gender_selection_sample.R` — builds `gender_selection_sample.dta` from the pipeline's
  fst output (same underlying data as `data_ready.dta`); reconstructs household composition
  variables (num_kids, has_child, household_size) needed for the Heckman exclusion restriction.
- `gender_selection_sample.dta` — the analysis sample (4,086,861 obs x 37 vars).
- `gender_selection_analysis.R` — all models: descriptive cross-tabs, triple-interaction
  employment/hours models, selection-margin regression, two-step Heckman by gender.
- `gender_selection_results/` — raw output: CSVs, .txt regression printouts, and saved .rds
  model objects for every model in the analysis script.
- `draft_paper_JDE_short.md` — draft paper (Markdown source), written for the Journal of
  Development Economics "Short Paper" track (top-recommended venue; see draft's introduction
  for the journal-fit rationale and runner-up venues).
- `draft_paper_exhibits.md` — the 4 exhibit tables referenced in the draft, with full
  coefficients/SEs, as Markdown tables.
- `draft_paper_JDE_short.tex` — full LaTeX source (self-contained single file; paste directly
  into a new Overleaf project). Includes all 4 exhibit tables typeset with booktabs, numbered
  equations for the empirical strategy, and a manual bibliography (placeholder entries for the
  4 comparison papers found via literature search still need full citation details — marked
  `[Author(s)]` in the .tex).
- `draft_paper_JDE_short.pdf` — compiled PDF of the LaTeX source (pdflatex, MiKTeX, 2 passes;
  10 pages, compiles cleanly with no errors or undefined references).

## Still to do before submission (flagged in the draft's "Notes for next revision pass")
1. Cluster or bootstrap the Heckman probit selection-equation standard errors (currently IID).
2. Add an alternative exclusion-restriction robustness check (e.g., co-resident grandparents).
3. Complete full citations in the References section (4 placeholder entries in the .tex/.md).
4. Author affiliations/title-page details in the .tex are placeholders — update before submission.
