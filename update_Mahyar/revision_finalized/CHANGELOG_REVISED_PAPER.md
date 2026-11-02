# CHANGELOG_REVISED_PAPER.md

`draft_finalized` = `main.tex` (untouched original) → `draft_finalized.tex` + `body_finalized.tex`
+ `appendix_finalized.tex` + `references_finalized.bib`. The Overleaf project `Mismatch_in_Iran`
holds both the original files and the `_finalized` files side by side.

## Round 1 (JDE-style referee response)
- Reproduced Tables 1/2/5/6; new §5.2 (null first stage); new §5.7.2 + Table 6 (province × year FE,
  province-specific cohort trends); softened horizontal-mismatch gender wording (later reversed —
  see round 2); Table 5 star fix `0.695*` → `0.695***`; magnitude paragraph (later corrected);
  migration coverage figure; 9-table appendix (A1–A9); 5 bib entries → published versions;
  `ven@article` typo fix; +2 bib entries (Haddad & Habibi 2017, Li/Morgan/Ding 2008).

## Round 2 (MDPI *Economies* referee brief)

### Corrections to round-1 material
| Location | Before | After | Why |
|---|---|---|---|
| §5.4 magnitude paragraph; Appendix Table A2 | "SD of log exposure 0.76 … 1 SD → +4.9 pp horiz, +5.5 pp double" | residualized SD ≈ 0.14, IQR ≈ 0.19; 1 resid-SD → +0.9 pp horiz, +1.0 pp double, −0.5 pp fully, +0.5 pp overed | raw SD overstated the identifying variation ~5×; FE absorb 96% of variance |
| §5.1 | "absorb about 92 percent of the variance" | "about 96 percent … residualized SD about 0.14 log points" | prov+cohort+year decomposition, not prov+cohort only |
| §5.4 horizontal-mismatch gender sentence; Appendix Table A7 | "interaction model does not reject equality (p = 0.32) … interpret with caution" | "rejects equality … difference −0.767, p = 0.001" | earlier test did not interact FE/urban with gender; properly specified test (Female × exposure, × FE, × urban) reproduces separate-sex regressions and is significant |
| §5.4 double-mismatch gender sentence | "gender difference significant (p = 0.01)" | "significant only at the 10 percent level (p = 0.09)" | properly specified test |
| Conclusion gender paragraph | "considerably larger among men" (generic) | itemized by outcome per formal test | match evidence |

### New material (round 2)
| Location | Change |
|---|---|
| Abstract (`draft_finalized.tex`) | fully rewritten: association/prediction language, residualized identification (96%), null attainment + employment, corrected magnitudes, formal gender test, overeducation downgraded; + Keywords + JEL codes |
| §3.1 | +1 sentence citing Karimi & Taghvatalab (2020) (county university-opening exposure precedent) |
| §2 first strand | +Su (2025) (China expansion → underemployment, male-skewed) alongside Li/Morgan/Ding |
| §2 third strand | +Moeeni & Tanaka (2023) (2010 female hiring ceiling → college attendance/major choice) |
| §5.7.2 | +1 paragraph on age-restriction robustness pointing to new Table A10 |
| §5.1 robustness list | + "restrictions to older (post-treatment) workers" |
| Appendix Table A2 | Panel B split into raw vs residualized SD/IQR; Panel C updated to 96% / 0.04 |
| Appendix Table A7 | rebuilt: 5 columns (male, female, F−M diff, p); proper gender-interacted spec; note explains it reproduces separate-sex regressions |
| Appendix Table A10 | **new** — age-restriction robustness, all 9 outcomes × {baseline, ≥21, ≥25, ≥30} |
| Appendix §"Reproducibility statement" | **new** — software, pipeline, crosswalk, data access |
| Back matter (`draft_finalized.tex`, after body) | **new** — Author Contributions, Funding, IRB/Consent (N/A), Data Availability Statement, Conflicts of Interest |
| `references_finalized.bib` | +3 entries: `karimi2020`, `moeeni2023`, `su2025` (25 total) |
| `scripts/run_all.R` | **new** — master reproducibility entry point; ordered pipeline `rev_setup` → `rev01` → `rev01b` → `rev02` → `rev03` → `rev04` → `rev06`; `rev05*` (wild bootstrap) deliberately excluded |

### Compilation
`draft_finalized.pdf` — 36 pp, `pdflatex` + `bibtex` + `pdflatex` ×2, MiKTeX. 0 undefined
references, 0 undefined citations. Minor pre-existing 8 pt overfull hbox in the Table 3 lead-in
paragraph (unchanged from the original).

### Figures de-duplicated (round 2, follow-up)
Every figure had its title/subtitle/notes **baked into the PNG** and repeated in the LaTeX
`\caption{}`. Fixed: `fig1.png`–`fig4.png` were cropped (`scripts/cropfigs.R`, R `png` package) to
remove the in-image bold title, the fig1/fig2 subtitles, and the fig1/fig2/fig4 "Notes:" bands —
keeping only the plot area, axis labels, legends, and the A/B/C/D panel headers. All removed text
was folded into self-contained `\caption{}` commands in `body_finalized.tex` (fig1–fig4), and the
separate `\begin{quote}…\end{quote}` notes block after fig3 was deleted (folded into its caption).
Cropped PNGs replace the originals in `draft_finalized/` and on Overleaf.

### Pushed to Overleaf (project `Mismatch_in_Iran`, 6a7030b268033c6d0a4332fe)
All four `_finalized` files were replaced in the live project via the editor:
`draft_finalized.tex`, `references_finalized.bib`, `appendix_finalized.tex` (byte-exact to local),
`body_finalized.tex` (SHA-256 `d281e6a3…53ad` verified against the local file before insert; the
local file's leading UTF-8 BOM is dropped online, content otherwise identical). Overleaf recompile:
**36 pp, 0 errors, 0 warnings, 0 undefined refs/citations** — only cosmetic overfull/underfull
`\hbox` info messages (Table 3 lead-in; author-contributions line; Table A7 header ≈55 pt; two
underfull lines in the appendix). Main document is set to `draft_finalized.tex`.

### Not done this pass (see `revision_blocked_tasks.md` and `REVISION_REPORT.md`)
- MDPI document-class conversion
- `appendix_crosswalk_broad.csv` / `_strict.csv` extraction + typeset crosswalk table
- full occupation-set decomposition table with BH q-values + bootstrapped contribution shares
- field × survey-year coverage table
- exposure numerator/denominator split; new placebo windows; supply-side (university-opening) treatment
- wild cluster bootstrap convergence (hardware-limited)
- exact "final preferred reliable" vertical benchmark (`*_final` variables not in `data_ready.dta`)
