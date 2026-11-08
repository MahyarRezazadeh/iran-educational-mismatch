# run_all.R  --  master reproducibility entry point for the revision analysis
# --------------------------------------------------------------------------
# University Expansion, Educational Mismatch, and Occupational Allocation in Iran
# Second-round revision (MDPI Economies).
#
# Software: R >= 4.4 (developed on 4.6.1). Packages: data.table, fixest, haven, fst.
# NOT required / NOT available for R 4.6.1: fwildclusterboot, boottest (wild
# cluster bootstrap was attempted in rev05*.R and did not converge on the
# available hardware -- see REVISION_REPORT.md issue 13 and revision_blocked_tasks.md).
#
# Inputs (restricted -- obtain from the corresponding author / Statistical Center of Iran):
#   <DATA>/slim.dta            individual LFS analysis extract (from code_July10.R)
#   <DATA>/data_ready.dta      published-benchmark extract used for Tables 1/2/5/6
# The province-year enrolment/population exposure panel and the field-to-occupation
# crosswalks are embedded in those extracts (see GEOGRAPHY_AUDIT.md, empirical_audit.md).
#
# Outputs: ../results/*.csv  (one file per analysis block; consumed when typesetting
#          body_finalized.tex / appendix_finalized.tex).
#
# Usage:
#   Rscript run_all.R                      # runs every block
#   Rscript run_all.R rev06                # runs a single block by prefix
# --------------------------------------------------------------------------

args    <- commandArgs(trailingOnly = TRUE)
here    <- normalizePath(dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))))
if (length(here) == 0 || !nzchar(here)) here <- getwd()
setwd(here)

## ---- config -------------------------------------------------------------
## Point DATA_DIR at the folder holding slim.dta / data_ready.dta.
DATA_DIR <- Sys.getenv("MISMATCH_DATA_DIR", unset = "C:/MismatchRev")
RESULTS  <- normalizePath(file.path(here, "..", "results"), mustWork = FALSE)
dir.create(RESULTS, showWarnings = FALSE, recursive = TRUE)
options(MISMATCH_DATA_DIR = DATA_DIR, MISMATCH_RESULTS = RESULTS)

## ---- pipeline (in order) ----------------------------------------------
steps <- c(
  rev_setup            = "rev_setup.R",            # slim.dta -> slim.fst, derived vars, samp_main
  rev01_reproduce      = "rev01_audit_reproduce.R",# reproduce Tables 1 & 2
  rev01b_findfinal     = "rev01b_findfinal.R",     # locate *_final reliability vars (documents the gap)
  rev02_priority1      = "rev02_priority1.R",       # FE ladder, first stage, gender, unconditional, magnitudes, cell-level
  rev03_firststage     = "rev03_firststage.R",      # first-stage deep dive by age / gender / FE
  rev04_mig_frozen     = "rev04_mig_frozen_boot.R", # migration/Tehran, frozen benchmark, (bootstrap attempt)
  rev06_r2             = "rev06_r2.R"               # Phase 2 age restrictions, Phase 7 residualized magnitude, Phase 8 proper gender interaction
)
## rev05*_wildboot.R are intentionally NOT in the default pipeline: the wild
## cluster bootstrap did not converge in usable time. Run them manually if you
## have boottest/reghdfe on adequate hardware.

sel <- if (length(args)) steps[grepl(paste(args, collapse = "|"), names(steps))] else steps
if (!length(sel)) stop("No matching step for: ", paste(args, collapse = " "))

for (nm in names(sel)) {
  f <- file.path(here, sel[[nm]])
  message(sprintf("\n===== %s  (%s) =====", nm, basename(f)))
  if (!file.exists(f)) { message("  SKIP (missing): ", f); next }
  t0 <- Sys.time()
  ok <- tryCatch({ source(f, local = new.env(), echo = FALSE); TRUE },
                 error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  message(sprintf("  %s in %.1f s", if (ok) "done" else "FAILED",
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

message("\nAll requested steps finished. CSV outputs in: ", RESULTS)
message("Typeset with: pdflatex draft_finalized -> bibtex draft_finalized -> pdflatex x2")
