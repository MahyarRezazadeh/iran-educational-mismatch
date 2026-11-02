# revision_finalized — identification / referee revision (2026-09-09)

Minimal-edit revision of the current Overleaf draft `Mismatch_in_Iran`
(https://www.overleaf.com/project/6a7030b268033c6d0a4332fe). `main.tex` and `body.tex` on
Overleaf were **not** modified.

## Contents

| Path | What it is |
|---|---|
| `draft_finalized/draft_finalized.pdf` | **Compiled revised manuscript** (36 pp) |
| `draft_finalized/draft_finalized.tex` | copy of `main.tex`; abstract byte-identical; `\input{body_finalized}` + `\input{appendix_finalized}` |
| `draft_finalized/body_finalized.tex` | `body.tex` + the minimal referee edits (see `revision_summary.md` for the exact list) |
| `draft_finalized/appendix_finalized.tex` | new 9-table appendix (A1–A9) |
| `draft_finalized/references.bib` | `references.bib` + 5 working-paper→published updates + the `ven@article` typo fix |
| `overleaf_current/` | pristine copy of the current Overleaf source (downloaded 2026-09-09) |
| `empirical_audit.md` | project map, spec audit (checks A–G), reproduction of Tables 1/2/5/6 |
| `revision_summary.md` | issue-by-issue table + "Results that materially changed" |
| `revision_blocked_tasks.md` | 5 items that could not be completed and why |
| `scripts/rev*.R` | all new analysis scripts (run against `../data_ready.dta` via a slim extract) |
| `results/rev*.csv` | raw regression output behind every new number |

## To compile

```
cd draft_finalized
pdflatex draft_finalized
bibtex   draft_finalized
pdflatex draft_finalized
pdflatex draft_finalized
```

## To reproduce the new numbers

1. In Stata: run `scripts/extract`-equivalent (`use <60 vars> using data_ready.dta`, `compress`,
   `save C:/MismatchRev/slim.dta`).
2. `Rscript scripts/rev_setup.R` → `slim.fst`.
3. `Rscript scripts/rev01_audit_reproduce.R` (reproduction), `rev02_priority1.R` (FE ladder,
   first stage, gender tests, unconditional, magnitudes, cell-level), `rev03_firststage.R`
   (first-stage deep dive), `rev04_mig_frozen_boot.R` (migration, frozen benchmark).

R 4.6.1 + data.table, fst, fixest, haven. All scripts assume the slim extract on `C:\MismatchRev\`.
