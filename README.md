# More Education, Worse Matches? University Expansion and Occupational Allocation in Iran

## Purpose

This repository contains the R code for a study of how the expansion of university places across Iranian provinces affected the labour-market outcomes of people exposed to it. Exposure is measured by province-year undergraduate enrolment relative to population at ages 18 to 20, merged to individual records from the Iranian Labor Force Survey (LFS) by historical province and birth cohort. The scripts build the analysis file, construct vertical, horizontal and double educational-mismatch measures, and estimate survey-weighted linear probability models of participation, employment, unemployment, mismatch and occupational destination, with robustness, heterogeneity, placebo and leave-one-province-out checks. A later set of scripts studies gender differences in selection into employment.

## Status

Working paper. This repository holds code, regression printouts and LaTeX manuscript drafts. It is not a complete replication package: the data are not included and there is no single script that runs the whole pipeline.

## Authors

Hossein Jalili and Mahyar Rezazadeh.

Some older drafts in `update_Mahyar/` list the authors in a different order and carry different working titles. The order above is the current one.

## Data

The data are **not** included in this repository.

| Data | Use | Access |
|---|---|---|
| Iranian Labor Force Survey microdata, Statistical Center of Iran | individual outcomes, education, field of study, occupation, province, birth year, survey weights | The manuscript draft states that the microdata cannot be redistributed by the authors and must be obtained from the Statistical Center of Iran. |
| Province-year university enrolment and population panel, Iranian years 1373 to 1403 (`final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv`) | exposure measure | Compiled by the authors from official statistical yearbooks (per the manuscript draft). Available from the authors on request. |
| Field-to-occupation crosswalk and field codebook | horizontal mismatch | Constructed by the authors. Available from the authors on request. |
| National enrolment by broad field (Ministry of Science) | descriptive figure in `code_division_field_trend.R` | Source described in the script header. |

The code expects a Stata file named `data_ready.dta` as its starting point, and later scripts read intermediate `.fst` files written by earlier steps.

## Software

- R. The revision scripts state "R >= 4.4 (developed on 4.6.1)". Versions for the other scripts are not recorded.
- Packages (from `library()` calls and `pkg::` references): data.table, fst, fixest, haven, ggplot2, stringr, scales, readxl, dqrng, fwildclusterboot, lfe, tidyselect, png. Package versions not recorded. See `environment.txt`.
- `run_all.R` notes that fwildclusterboot was not available for R 4.6.1 in the authors' setup and that the wild cluster bootstrap scripts (`rev05*.R`) are not part of the default run.
- LaTeX (pdflatex and bibtex) to compile the drafts.

## Folder map

```
codes/                       main analysis pipeline (R)
outputs/                     regression printouts and variable lists (.txt) from steps 8, 17 and 20
update_Mahyar/               gender-selection extension scripts, results printouts,
                             manuscript drafts (.tex/.md) and a presentation (.tex)
update_Mahyar/gender_selection_results/   regression printouts for the extension
update_Mahyar/revision_finalized/         later manuscript draft and scripts
  draft_finalized/           LaTeX manuscript draft and appendix
  overleaf_current/          copy of an earlier LaTeX draft
  scripts/                   rev_setup.R, rev01 to rev06 scripts and run_all.R
```

## How to run

There is no master script for the main pipeline in `codes/`. The order below is **inferred** from the step numbers in each script's header.

1. `code_July10.R`: Steps 1 to 5 (reduced LFS file, exposure-eligible sample, historical-province harmonisation, merge of exposure at ages 18 to 20, education and occupation code diagnostics for mismatch).
2. `code_descriptive.R` (Step 6), `code_check.R` (Step 7), `code_baseline.R` (Step 8), `code_Robustness.R` (Step 9).
3. `code_mismatch.R` (Step 10), `code_finalmismatch.R` (Step 11), `code_graphs.R` (Step 13).
4. `code_mismatchtypes.R` (Step 14A), `code_mismatchfield.R` (Step 15), `code_step16.R` to `code_step21.R` (Steps 16 to 21), then `code_step22.R`, `code_step23.R`, `code_step24.R`.
5. Scripts without a step number, which appear to be robustness and heterogeneity checks run on the analysis file: `code_Exclude_Tehran.R`, `code_heteroabsorb.R`, `code_heteroeffects.R`, `code_nonlinear_LFP.R`, `code_nonlinear_mainoutputs.R`, `code_robutness.R`.
6. Division-level scripts, Steps 25A to 25D: `code_division_capacity_groups.R`, `code_division_eventstudy_gender.R`, `code_division_validation.R`, `code_division_field_trend.R`.

Gender-selection extension (`update_Mahyar/`), inferred order: `build_gender_selection_sample.R`, `gender_selection_analysis.R`, `refit_heckman_clustered.R`, `gender_gap_decomposition.R`, `event_study.R`.

Revision analysis (`update_Mahyar/revision_finalized/scripts/`): this part has an entry script. Create the slim extract from `data_ready.dta` first (the folder README describes a Stata step), set the environment variable `MISMATCH_DATA_DIR` to its folder, then run

```
Rscript run_all.R            # all steps
Rscript run_all.R rev06      # one step by prefix
```

Before running anything, edit the folder paths at the top of each script (see Known limitations).

## Expected running time

Not recorded. `run_all.R` prints the time of each step when it runs, but no run log is included.

## Known limitations

- Hard-coded absolute paths in most scripts, for example:
  - `codes/code_baseline.R`: `project_folder <- "E:/sfu/PhD/Summer2026/paper(HES and LFS)"`
  - `codes/code_division_validation.R`: `project_folder <- "E:/MismatchPipeline"` and a path under `C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/`
  - `update_Mahyar/gender_selection_analysis.R`: `path <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/..."`
  - `update_Mahyar/revision_finalized/scripts/rev_setup.R`: reads `C:/MismatchRev/slim.dta` directly, even though `run_all.R` offers `MISMATCH_DATA_DIR`.
- `code_July10.R` is about 11,000 lines and contains several steps that each restart with their own setup. It is not split into separate files.
- Some file names have spelling variants (`code_Robustness.R` and `code_robutness.R` are different scripts).
- Several scripts search the data for candidate variables (for example Steps 14A, 17A and 20A) and print lists; their outputs in `outputs/` are diagnostics, not paper tables.
- `outputs/` has two versions of the Step 8 printouts (`step8_baseline_regressions/` and `step8_baseline_regressions_corrected/`). The scripts do not say which version the current draft uses.
- No package versions or session information are recorded.
