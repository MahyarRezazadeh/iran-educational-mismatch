# LITERATURE_REVISION.md

Every entry below was verified against a publisher / DOI / RePEc page (not a search snippet).
`references_finalized.bib` now has 25 entries; every `\citet` key in `body_finalized.tex` resolves
(0 undefined, 0 unused).

## Working papers updated to published versions

| Key | Now cited as |
|---|---|
| `cassidy2023` | Cassidy & Gaulke (2024), *Economic Inquiry* 62(2), 607–632 |
| `li_nd` | Li, Liang & Wu (2025), *Journal of Human Resources* 60(1), 289–325 (was `year = {n.d.}`) |
| `chen2023` | Chen & Ebadi (2026), *Journal of Development Economics* 178, 103572 |
| `rudakov2019` | Rudakov, Figueiredo, Teixeira & Roshchin (2022), *Journal of Education and Work* 35(6–7), 680–699 |
| `leuven2011` | Leuven & Oosterbeek (2011), *Handbook of the Economics of Education*, vol. 4, 283–326 |

## New papers incorporated

### `haddad2017` — Keshavarz Haddad & Habibi (2017), *International Labour Review* 156(1), 45–72
- **What it does:** documents over-education in Iran using the Household Income and Expenditure
  Survey, 2001–2012.
- **Design:** descriptive/cross-sectional; realized-match modal-education benchmark; wage regressions.
- **Outcome:** incidence of over-education (post-secondary workers in low-skill jobs) and its wage
  consequences.
- **How this paper differs:** province–cohort variation from a specific expansion rather than a
  cross-sectional snapshot; adds horizontal and joint mismatch and occupational allocation.
- **Used in text:** §2 (Iran-specific over-education evidence; the paper's own finding of higher
  female over-education odds is noted).

### `li2008` — Li, Morgan & Ding (2008), *Int. J. Educational Development* 28(6), 687–697
- **What it does:** examines the labor market for Chinese graduates following the late-1990s
  expansion; models employment probability and over-education.
- **Outcome:** over-education among graduates.
- **How this paper differs:** identification from within-province cross-cohort variation; jointly
  studies field mismatch.
- **Used in text:** §2 (expansion → over-education, first strand).

### `su2025` — Su (2025), *Education Economics* (advance online), DOI 10.1080/09645292.2025.2525862
- **What it does:** estimates the effect of China's college expansion on graduate underemployment
  (overqualification).
- **Finding:** expansion raises underemployment; **the adverse effect is larger for men.**
- **How this paper differs:** Iran; horizontal as well as vertical mismatch; formal between-gender
  test. The male-skewed effect in Su (2025) parallels this paper's finding for horizontal mismatch.
- **Used in text:** §2 (first strand, alongside `li2008`).

### `karimi2020` — Karimi & Taghvatalab (2020), *Quarterly Review of Economics and Finance* 78, 22–33
- **What it does:** uses Iran's large-scale university-opening program in small and medium counties
  during the 2000s; exposure = a university opening in the county of residence at ages 18–22.
- **Outcome:** likelihood of being married at ages 28–32 (down to −5.9 pp).
- **How this paper differs:** this is the closest existing design; it uses **county-level university
  openings** (a supply-side, finer-geography measure). The present paper uses **province-year
  enrolment per capita** because the LFS has no sub-province identifier and no dated-openings panel.
  This is stated in §3.1 and `GEOGRAPHY_AUDIT.md`. Karimi & Taghvatalab study marriage; this paper
  studies education–job mismatch.
- **Used in text:** §3.1 (institutional background / exposure design precedent).

### `moeeni2023` — Moeeni & Tanaka (2023), *Journal of Public Economics* 224, 104896
- **What it does:** exploits Iran's 2010 female public-sector hiring ceiling.
- **Finding:** women's college attendance fell 11.4 pp; women shifted away from majors whose
  employment prospects worsened.
- **How this paper differs:** studies mismatch outcomes among the already-educated rather than
  enrolment/major choice; but Moeeni & Tanaka is direct evidence that female labor demand shapes
  women's education decisions in Iran — relevant to the selection interpretation of the gender
  results.
- **Used in text:** §2 (third strand, Iran gender paragraph).

## Contribution reframing (implemented in Introduction §1 and §2)

Away from: "higher education expansion can cause overeducation."
Toward: province–cohort evidence from Iran on **both vertical and horizontal mismatch, their joint
occurrence, occupational allocation, field-of-study heterogeneity, and gender-specific labor-market
selection**, with the field/joint-mismatch results as the robust core and the vertical-mismatch
(overeducation) result explicitly secondary.

## Not added
- Kiker, Santos & Mendes de Oliveira (1997) — classic realized-match methodology reference; optional.
- Salehi-Isfahani (Iran higher-education working paper) — venue not confirmed; excluded per the
  never-cite-unverified rule.
