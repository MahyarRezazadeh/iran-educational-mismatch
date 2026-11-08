# ==============================================================================
# DIVISION-LEVEL REFRAMING -- STEP 25A
# Baseline-period capacity-group classification and descriptive trend figures
#
# "Division" = historical province (28 units), the finest geography available
# anywhere in the LFS microdata or the exposure panel (see project memory /
# ILR.tex Data section for why: raw LFS respondent records only carry a
# Province field, no county/shahrestan identifier).
#
# Exposure measure = undergraduate + professional enrollment per 100,000
# population (companion paper's existing construction, unmodified). Two
# alternative capacity measures requested by the brief -- "available seats"
# and "empty seats" -- were investigated and are NOT constructible: no
# Iranian data source (Statistical Yearbook, Ministry of Science, Sanjesh)
# publishes admission-capacity by province in any panel form; only
# enrollment/admission counts exist. This is disclosed in the paper's Data
# section rather than fabricated.
#
# Capacity-group definition (per explicit brief, Section 2):
#   - predetermined, based on capacity NEAR THE BEGINNING of the sample
#   - bottom quartile = low-capacity, top quartile = high-capacity
#   - fixed for the whole panel, never redefined by year
# This differs deliberately from two existing-but-not-quite-matching designs
# already in the codebase: code_heteroeffects.R uses a TERTILE split on the
# same first-5-cohort baseline window, and code_graphs.R uses a QUARTILE
# split but on the FULL-SAMPLE mean exposure (not baseline-period). This
# script combines "quartile" (from code_graphs.R) with "baseline-period"
# (from code_heteroeffects.R) exactly as newly specified.
# ==============================================================================

rm(list = ls())
invisible(gc())
options(scipen = 999, datatable.print.nrows = 100)

library(data.table)
library(fst)
library(ggplot2)

project_folder <- "E:/MismatchPipeline"

analysis_input_file <- file.path(
  project_folder, "LFS_analysis_outputs", "lfs_final_analysis_ready.fst"
)

# Raw calendar-year province exposure panel (built in code_July10.R Step 4).
# This is the ONLY source with a genuine exposure_year calendar axis; the LFS
# analysis file only carries survey_year (when the respondent was surveyed,
# years later) and birth cohort, not the raw enrollment-year time series.
raw_panel_file <- file.path(
  project_folder,
  "final_enrollment_population_exposure_panel_1373_1403_cleanzeros_utf8.csv"
)

output_folder <- file.path(
  project_folder, "LFS_analysis_outputs", "step25_division_level_capacity"
)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

onedrive_output_folder <- paste0(
  "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/",
  "Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/",
  "outputs/step25_division_level_capacity"
)
dir.create(onedrive_output_folder, recursive = TRUE, showWarnings = FALSE)


# ==============================================================================
# STEP 1: Baseline-period province classification (bottom/top quartile)
# ==============================================================================
# Uses the analysis file's own province x cohort exposure cells (same
# undergrad_exposure_18_20 measure as the main specification), restricted to
# the earliest 5 birth cohorts observed -- identical baseline-window length to
# code_heteroeffects.R's existing tertile design, so the two are comparable.

dt <- as.data.table(read_fst(
  analysis_input_file,
  columns = c(
    "province_historical", "analysis_birth_year",
    "undergrad_exposure_18_20", "log_undergrad_exposure_18_20"
  )
))

setnames(dt, "analysis_birth_year", "cohort")

province_cohort_exposure <- unique(
  dt[
    !is.na(undergrad_exposure_18_20) & !is.na(province_historical),
    .(province_historical, cohort, undergrad_exposure_18_20, log_undergrad_exposure_18_20)
  ]
)

earliest_cohorts <- sort(unique(province_cohort_exposure$cohort))[1:5]
cat("Baseline cohorts used for capacity-group classification:\n")
print(earliest_cohorts)

baseline_capacity <- province_cohort_exposure[
  cohort %in% earliest_cohorts,
  .(baseline_log_exposure = mean(log_undergrad_exposure_18_20, na.rm = TRUE),
    baseline_exposure = mean(undergrad_exposure_18_20, na.rm = TRUE),
    n_baseline_cohorts = .N),
  by = province_historical
]

n_provinces <- nrow(baseline_capacity)
baseline_capacity[
  , capacity_rank := frank(baseline_log_exposure, ties.method = "average")
]
baseline_capacity[
  , capacity_quartile := pmin(4L, ceiling(4 * capacity_rank / n_provinces))
]
baseline_capacity[
  , capacity_group := fcase(
      capacity_quartile == 1L, "Low capacity (bottom quartile)",
      capacity_quartile == 4L, "High capacity (top quartile)",
      default = "Middle (Q2-Q3)"
    )
]
setorder(baseline_capacity, -baseline_log_exposure)

cat("\nBaseline capacity-group classification (fixed for entire panel):\n")
print(baseline_capacity[, .(province_historical, baseline_exposure, capacity_quartile, capacity_group)])

fwrite(
  baseline_capacity,
  file.path(output_folder, "step25A_province_baseline_capacity_groups.csv"),
  bom = TRUE
)
fwrite(
  baseline_capacity,
  file.path(onedrive_output_folder, "step25A_province_baseline_capacity_groups.csv"),
  bom = TRUE
)


# ==============================================================================
# STEP 2: Raw calendar-year province panel + population-normalized enrollment
# ==============================================================================

raw_panel <- fread(raw_panel_file, encoding = "UTF-8")

raw_panel <- merge(
  raw_panel,
  baseline_capacity[, .(province_historical, capacity_group, capacity_quartile)],
  by = "province_historical",
  all.x = TRUE
)

if (raw_panel[is.na(capacity_group), .N] > 0) {
  warning(
    "Some provinces in the raw exposure panel did not match the baseline ",
    "capacity classification -- check province name harmonization."
  )
  print(unique(raw_panel[is.na(capacity_group), province_historical]))
}

fwrite(
  raw_panel,
  file.path(output_folder, "step25A_raw_panel_with_capacity_groups.csv"),
  bom = TRUE
)


# ==============================================================================
# STEP 3: National-average annual series + group-average annual series
# ==============================================================================
# "Enrollment" here is undergrad+professional enrollment (the measure used
# throughout the main specification). enrollment_total (incl. masters/PhD) is
# also plotted as a supplementary series since it exists in the same panel.

annual_national <- raw_panel[
  , .(
      enroll_ug_prof = sum(enroll_undergrad_professional, na.rm = TRUE),
      enroll_total = sum(enrollment_total, na.rm = TRUE),
      population_000s = sum(population_000s, na.rm = TRUE)
    ),
  by = exposure_year
][
  , `:=`(
      series = "National average",
      enroll_ug_prof_per_100k = enroll_ug_prof / population_000s * 100,
      enroll_total_per_100k = enroll_total / population_000s * 100
    )
]

annual_by_group <- raw_panel[
  capacity_group != "Middle (Q2-Q3)",
  .(
      enroll_ug_prof = sum(enroll_undergrad_professional, na.rm = TRUE),
      enroll_total = sum(enrollment_total, na.rm = TRUE),
      population_000s = sum(population_000s, na.rm = TRUE)
    ),
  by = .(exposure_year, series = capacity_group)
][
  , `:=`(
      enroll_ug_prof_per_100k = enroll_ug_prof / population_000s * 100,
      enroll_total_per_100k = enroll_total / population_000s * 100
    )
]

annual_combined <- rbindlist(list(annual_national, annual_by_group), use.names = TRUE)
setorder(annual_combined, series, exposure_year)

fwrite(
  annual_combined,
  file.path(output_folder, "step25A_annual_trends_by_capacity_group.csv"),
  bom = TRUE
)
fwrite(
  annual_combined,
  file.path(onedrive_output_folder, "step25A_annual_trends_by_capacity_group.csv"),
  bom = TRUE
)

series_colors <- c(
  "Low capacity (bottom quartile)" = "#B23A48",
  "High capacity (top quartile)" = "#1B5299",
  "National average" = "#555555"
)
series_linetypes <- c(
  "Low capacity (bottom quartile)" = "solid",
  "High capacity (top quartile)" = "solid",
  "National average" = "dashed"
)

make_trend_figure <- function(data, y_var, y_label, title, file_stub) {
  p <- ggplot(data, aes(x = exposure_year, y = .data[[y_var]], color = series, linetype = series)) +
    geom_line(linewidth = 0.9) +
    scale_color_manual(values = series_colors) +
    scale_linetype_manual(values = series_linetypes) +
    labs(
      x = "Year (1373-1403)", y = y_label, color = NULL, linetype = NULL,
      title = title,
      caption = paste0(
        "Capacity groups are fixed, based on average log enrollment exposure ",
        "over birth cohorts ", earliest_cohorts[1], "-", earliest_cohorts[5],
        " (bottom/top quartile of 28 historical provinces)."
      )
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 8, hjust = 0)
    )

  ggsave(file.path(output_folder, paste0(file_stub, ".png")), p, width = 8, height = 5.5, dpi = 300)
  ggsave(file.path(output_folder, paste0(file_stub, ".pdf")), p, width = 8, height = 5.5)
  ggsave(file.path(onedrive_output_folder, paste0(file_stub, ".png")), p, width = 8, height = 5.5, dpi = 300)
  invisible(p)
}

make_trend_figure(
  annual_combined, "enroll_ug_prof",
  "Undergraduate + professional enrollment (persons)",
  "Figure D1: University enrollment by division capacity group (levels)",
  "figure_D1_enrollment_levels"
)
make_trend_figure(
  annual_combined, "enroll_ug_prof_per_100k",
  "Undergraduate + professional enrollment per 100,000 residents",
  "Figure D2: University enrollment by division capacity group (per capita)",
  "figure_D2_enrollment_per_capita"
)
make_trend_figure(
  annual_combined, "enroll_total",
  "Total enrollment, all degree levels (persons)",
  "Figure D3: Total enrollment (incl. masters/PhD) by division capacity group (levels)",
  "figure_D3_total_enrollment_levels"
)
make_trend_figure(
  annual_combined, "enroll_total_per_100k",
  "Total enrollment per 100,000 residents, all degree levels",
  "Figure D4: Total enrollment (incl. masters/PhD) by division capacity group (per capita)",
  "figure_D4_total_enrollment_per_capita"
)

cat("\nStep 25A complete. Outputs written to:\n", output_folder, "\nand\n", onedrive_output_folder, "\n")
