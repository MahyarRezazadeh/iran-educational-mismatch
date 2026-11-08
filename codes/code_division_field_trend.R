# ==============================================================================
# DIVISION-LEVEL REFRAMING -- STEP 25D
# National enrollment trend by broad field of study (Ministry of Science's own
# field taxonomy: Medicine, Humanities, Basic Sciences, Technical/Engineering,
# Agriculture/Veterinary, Arts), NATIONAL LEVEL ONLY.
#
# WHY THIS PROXY, NOT LITERAL TAJROBI/RIAZI/ENSANI:
# A genuine multi-year national admitted-students-by-Konkoor-exam-group
# (Tajrobi/Riazi/Ensani) series does not exist in any consistent, verifiable
# public source -- only scattered single-source press citations for isolated
# years, mixing different metrics (participants vs. admitted, exam-only vs.
# GPA-track-inclusive) that are not safely comparable across years.
#
# Instead, this uses the Ministry of Science's own official "broad field of
# study" breakdown, hand-extracted from three Statistical Yearbook editions
# (1391, 1393, 1394), each of which publishes this field split for only its
# own most-recent year. All three points use the IDENTICAL definition
# ("All degrees", "Both sexes", "Including Islamic Azad University" --
# verified arithmetically for 1391-92 by summing the Yearbook's separate
# Azad-only and non-Azad tables and confirming the total matches exactly).
# Humanities is a close proxy for Ensani; Basic Sciences + Technical/
# Engineering are the closest (imperfect, not 1:1 separable) proxy for the
# Riazi/Tajrobi split. This is disclosed explicitly in the paper text, not
# just this comment.
# ==============================================================================

library(data.table)
library(ggplot2)

project_folder <- "E:/MismatchPipeline"
output_folder <- file.path(project_folder, "LFS_analysis_outputs", "step25_division_level_capacity")
onedrive_output_folder <- paste0(
  "C:/Users/mahya/OneDrive - Simon Fraser University (1sfu)/Desktop/",
  "Iran Labor Market Project/DID Paper- With Hossein Jalili/Mismatch Paper/",
  "outputs/step25_division_level_capacity"
)

dt <- fread(file.path(project_folder, "field_trend_data.csv"))
dt[, field := factor(field, levels = c(
  "Humanities", "Technical and engineering", "Basic sciences",
  "Medicine", "Arts", "Agriculture and veterinary"
))]

fwrite(dt, file.path(output_folder, "step25D_field_trend_national.csv"), bom = TRUE)
fwrite(dt, file.path(onedrive_output_folder, "step25D_field_trend_national.csv"), bom = TRUE)

field_colors <- c(
  "Humanities" = "#1B5299", "Technical and engineering" = "#B23A48",
  "Basic sciences" = "#2E8B57", "Medicine" = "#7B4EA3",
  "Arts" = "#D98E04", "Agriculture and veterinary" = "#555555"
)

p <- ggplot(dt, aes(x = year_label, y = students, color = field)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  scale_color_manual(values = field_colors) +
  scale_x_continuous(breaks = unique(dt$year_label)) +
  labs(
    x = "Persian academic year (start)", y = "Enrolled students (all degree levels, both sexes)",
    color = NULL,
    title = "Figure D6: National enrollment by broad field of study, 1391-1394",
    caption = paste0(
      "National totals only (no division-level breakdown available). Ministry of Science's own\n",
      "broad-field taxonomy, not literal Konkoor exam groups (Tajrobi/Riazi/Ensani) -- see Section 4.2.\n",
      "Humanities is the closest proxy for Ensani. Source: Iran Statistical Yearbooks 1391, 1393, 1394,\n",
      "Table 17.26/15.36+15.37 (\"Including Islamic Azad University\"), hand-extracted, arithmetic-verified."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.caption = element_text(size = 7.5, hjust = 0)
  )

ggsave(file.path(output_folder, "figure_D6_field_trend_national.png"), p, width = 8.5, height = 5.5, dpi = 300)
ggsave(file.path(output_folder, "figure_D6_field_trend_national.pdf"), p, width = 8.5, height = 5.5)
ggsave(file.path(onedrive_output_folder, "figure_D6_field_trend_national.png"), p, width = 8.5, height = 5.5, dpi = 300)

cat("Step 25D complete.\n")
print(dt)
