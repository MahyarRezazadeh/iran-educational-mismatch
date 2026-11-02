# Exhibits for "Educated but Excluded" draft

Source for all exhibits: `update_Mahyar/gender_selection_results/` (raw output) built from
`update_Mahyar/gender_selection_sample.dta` (N = 4,086,861; 2,079,162 women, 2,007,699 men).
All models: survey-weighted, historical-province + birth-cohort + survey-year fixed effects,
two-way clustered standard errors (province x cohort) unless noted. Significance: *** p<0.01,
** p<0.05, * p<0.10.

---

## Exhibit 1. Employment, hours, and education by gender and exposure quartile

| Gender | Exposure quartile | Employment rate | Mean weekly hours | Tertiary share |
|---|---|---:|---:|---:|
| Men   | Q1 (lowest)  | 0.844 | 49.7 | 0.199 |
| Men   | Q2           | 0.805 | 51.4 | 0.269 |
| Men   | Q3           | 0.585 | 49.2 | 0.310 |
| Men   | Q4 (highest) | 0.545 | 50.1 | 0.385 |
| Women | Q1 (lowest)  | 0.182 | 34.3 | 0.184 |
| Women | Q2           | 0.167 | 37.7 | 0.295 |
| Women | Q3           | 0.119 | 36.1 | 0.331 |
| Women | Q4 (highest) | 0.114 | 39.0 | 0.406 |

---

## Exhibit 2. Triple-interaction models: employment and hours

|  | (1) Employed | (2) Weekly hours (employed only) |
|---|---:|---:|
| Exposure | -0.0367** (0.0135) | -0.9895** (0.3590) |
| Female | -1.2328*** (0.0568) | -23.8557*** (2.2373) |
| Tertiary | 0.7073*** (0.1039) | -7.0151*** (0.9125) |
| Urban | -0.0569*** (0.0070) | 3.6015*** (0.5003) |
| Exposure x Female | 0.0930*** (0.0089) | 1.4680*** (0.3997) |
| Exposure x Tertiary | -0.1340*** (0.0157) | 0.5715*** (0.1689) |
| Female x Tertiary | 0.1361 (0.0837) | 4.7541* (2.4971) |
| Exposure x Female x Tertiary | 0.0254* (0.0144) | -0.0860 (0.3841) |
| Observations | 4,086,861 | 1,686,321 |
| Adj. R2 | 0.407 | 0.136 |
| Fixed effects | Province, cohort, year | Province, cohort, year |

Note: outcome (1) is a 0/1 indicator, linear probability model. Outcome (2) restricted to employed
individuals.

---

## Exhibit 3. The selection margin: education rank of employed vs. eligible population

**Panel A. Descriptive selection gap** (mean education rank of employed minus mean education
rank of the full eligible population, by cell; education rank runs 1-8)

| Gender | Exposure quartile | Education rank, all eligible | Education rank, employed | Selection gap |
|---|---|---:|---:|---:|
| Men   | Q1 | 3.070 | 3.063 | -0.008 |
| Men   | Q2 | 3.522 | 3.444 | -0.078 |
| Men   | Q3 | 3.727 | 3.362 | -0.365 |
| Men   | Q4 | 4.078 | 3.620 | -0.458 |
| Women | Q1 | 3.005 | 3.863 | +0.858 |
| Women | Q2 | 3.708 | 4.559 | +0.851 |
| Women | Q3 | 3.842 | 4.178 | +0.336 |
| Women | Q4 | 4.222 | 4.674 | +0.452 |

**Panel B. Regression: education rank ~ employed x exposure x female**

| | Education rank |
|---|---:|
| Employed | 2.4247*** (0.4222) |
| Exposure | 0.3092*** (0.0865) |
| Female | 0.0573 (0.2324) |
| Urban | 1.2320*** (0.0532) |
| Employed x Exposure | -0.4694*** (0.0615) |
| Employed x Female | 0.8041*** (0.2668) |
| Exposure x Female | -0.0676* (0.0341) |
| Employed x Exposure x Female | 0.0827* (0.0470) |
| Observations | 3,891,434 |
| Adj. R2 | 0.174 |
| Fixed effects | Province, cohort, year |

---

## Exhibit 4. Two-step Heckman selection correction, by gender

|  | Women: selection (probit) | Women: hours (2nd stage) | Men: selection (probit) | Men: hours (2nd stage) |
|---|---:|---:|---:|---:|
| Exposure | -0.0808*** (0.0007) | 0.9589* (0.5135) | 0.0169*** (0.0007) | -0.4625 (0.4198) |
| Tertiary | 0.6290*** (0.0002) | -12.1724*** (1.7526) | -0.5196*** (0.0002) | -2.3183*** (0.7128) |
| Urban | -0.2975*** (0.0003) | 10.2399*** (0.7398) | -0.1730*** (0.0002) | 3.7295*** (0.5310) |
| Num. children | 0.1342*** (0.0007) | - | -0.1055*** (0.0006) | - |
| Num. children x Exposure | -0.0096*** (0.0001) | - | -0.0098*** (0.0001) | - |
| Inverse Mills ratio | - | -24.3253*** (2.1911) | - | -5.0765*** (0.4902) |
| Observations | 2,079,162 | 301,636 | 2,007,699 | 1,384,685 |
| Fixed effects | Province, cohort, year | Province, cohort, year | Province, cohort, year | Province, cohort, year |

Note: selection-equation standard errors are currently IID (fixed-effects probit); to be replaced
with clustered/bootstrap SEs before submission. Exclusion restriction: number of children in the
household (reconstructed from `rel_hh`), interacted with local university exposure.
