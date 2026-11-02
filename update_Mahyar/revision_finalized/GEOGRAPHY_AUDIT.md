# GEOGRAPHY_AUDIT.md — what "historical province" means and what the exposure geography measures

## Raw geographic variables in the LFS microdata (`data_ready.dta`)

| Variable | Content |
|---|---|
| `province` / `province_current_original` / `current_province_clean` | **current** province of residence at the survey date (string, 31 contemporary provinces) |
| `previous_residence_province` / `previous_province_clean` | province of **previous** residence, when the respondent reports a move |
| `previous_residence_location` / `prev_residence_location_clean` | previous residence at a finer (settlement) level, string |
| `residence_change_reason` / `recent_migration_type` | reason and type of the most recent move |
| `recent_move_reported`, `recent_interprovincial_migrant`, `recent_within_province_migrant`, `recent_within_city_migrant`, `recent_international_migrant` | flags derived from the previous-residence fields |
| `residence_duration` | how long the respondent has lived at the current location |

There is **no birthplace variable** and **no sub-province (shahrestan/county) identifier** anywhere in the microdata. The finest geography is the province.

## Harmonization to 28 historical provinces

`code_July10.R` block 3 maps the contemporary province identifiers (which changed over 1373--1403 as provinces were split — Qazvin, Qom, Golestan, Alborz, South/North Khorasan) to a **consistent set of 28 historical provinces** (`province_historical`, `previous_province_historical`). This prevents an administrative split from appearing as a change in enrolment per capita.

## Which geography is used to assign treatment

`province_historical` is the harmonized version of **`current_province_clean`** — i.e. **current (survey-date) province of residence, harmonized to 1373 boundaries.** It is **not** birthplace, not province at age 18, and not a reconstructed origin. Confirmed by tracing `code_July10.R`: `province_historical` is built from the current-province string via the historical-province lookup; the previous-province fields feed only `previous_province_historical` and the migration flags.

Exposure `undergrad_exposure_18_20` = mean of `undergrad_per_100k_ageXX` for XX in {18,19,20}, where the province-year enrolment/population panel is joined on `province_historical` and the calendar years when the person's `analysis_birth_year` cohort was 18/19/20.

## What migration can and cannot be observed

- **Observable:** whether the respondent's *current* province differs from their *previous* province, and whether the most recent move was recent (the LFS asks about residence "X years ago"). `recent_cross_hist_region_move` and `current_previous_province_diff` flag identified cross-historical-province movers — **about 0.3 percent of the joint-mismatch sample.**
- **Not observable:** the full migration history. Someone who moved provinces *before* the LFS reference window, or who moved and then moved back, is not flagged. Province of residence **at ages 18--20 cannot be established** for anyone whose most recent move predates the survey's recall window.

## Implication for the manuscript (implemented)

The manuscript now states that exposure is "assigned using an individual's historical province" (= harmonized current residence) and, in the migration subsubsection, that the LFS "identifies recent cross-province movers but not migration that occurred before the survey window," with the 0.3 percent coverage figure. It does **not** claim that treatment is the local access an individual actually faced at ages 18--20. The Konkoor discussion in Section 3.1 and Section 5.2 makes explicit that a province's enrolment rate need not reflect where its birth cohort attended university or whether they became graduates. This is consistent with the null first stage.

## Migration sensitivity checks implemented (Table 7 in the manuscript)

1. Baseline. 2. Exclude identified cross-historical-province movers. 3. Exclude Tehran. 4. Exclude Tehran + identified movers. All reproduce to 3 decimals; central results unchanged.

A "known stable-residence" sample (respondents demonstrably resident in the current province before university-entry age) **cannot be constructed**: `residence_duration` is coarse and not consistently coded across waves, and there is no age-at-move variable that would place a move relative to age 18. Logged as NOT FEASIBLE.
