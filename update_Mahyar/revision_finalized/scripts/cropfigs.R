suppressMessages(library(png))
SRC <- "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/update_Mahyar/revision_finalized/draft_finalized"
OUT <- "C:/MismatchRev/figs_cropped"
dir.create(OUT, showWarnings = FALSE)

# per-image [top,bottom] rows to KEEP (1-indexed, inclusive), from profile.R analysis
spec <- list(
  "fig1.png" = c(96,  992),
  "fig2.png" = c(126, 1002),
  "fig3.png" = c(62,  2052),
  "fig4.png" = c(196, 4412)
)

for (f in names(spec)) {
  a <- readPNG(file.path(SRC, f))
  if (length(dim(a)) == 2) a <- array(a, dim = c(dim(a), 1))
  H <- dim(a)[1]
  r <- spec[[f]]
  top <- max(1, r[1]); bot <- min(H, r[2])
  b <- a[top:bot, , , drop = FALSE]
  if (dim(b)[3] == 1) b <- b[, , 1]
  writePNG(b, file.path(OUT, f))
  cat(sprintf("%-9s  %d rows -> %d rows  (kept %d-%d)\n", f, H, bot - top + 1, top, bot))
}
