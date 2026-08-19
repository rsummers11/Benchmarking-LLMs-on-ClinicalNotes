# Regenerates the F1-by-comorbidity figure (Figure S1 / f1_with_diagnosis) from
# performance_by_diagnosis.csv, covering all 4 model families (Llama, Qwen, Claude, GPT).
# Font-family patch as in regenerate_figure1.R.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))

final_data_dir <- "your_path_here/final_data"
plots_dir <- "your_path_here/plots"

fix_svg_font <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- gsub('"Liberation Serif"', '"Times New Roman"', lines, fixed = TRUE)
  writeLines(lines, path)
}

merged <- read_csv(file.path(final_data_dir, "performance_by_diagnosis.csv"), show_col_types = FALSE)

# Point-range, not bars -- with 4 families dodged per diagnosis, bars were too crowded; chosen
# over a faceted-by-architecture point-range layout too, since direct adjacency (all 4 on one
# shared scale) mattered more than the visual separation a facet split would give.
p <- ggplot(merged, aes(x = diagnosis_label, y = 100 * F1, color = model_family)) +
  geom_pointrange(aes(ymin = F1_LCL * 100, ymax = F1_UCL * 100), position = position_dodge(width = 0.6), size = 0.35, linewidth = 0.6) +
  scale_x_discrete(name = "Elixhauser comorbidity") +
  scale_y_continuous(name = "F1 score (%)") +
  coord_flip() +
  theme_minimal() +
  theme(legend.title = element_blank(), text = element_text(family = "Times New Roman", size = 14))

svg_path <- file.path(plots_dir, "figureS1_f1_with_diagnosis_bootstrap.svg")
ggsave(svg_path, plot = p, width = 9, height = 9, units = "in")
fix_svg_font(svg_path)

pdf_tmp <- file.path(plots_dir, "figureS1_f1_with_diagnosis_bootstrap.pdf.tmp")
pdf_path <- file.path(plots_dir, "figureS1_f1_with_diagnosis_bootstrap.pdf")
ggsave(pdf_tmp, plot = p, width = 9, height = 9, units = "in", device = cairo_pdf)
file.rename(pdf_tmp, pdf_path)

cat("Wrote figures to", plots_dir, "\n")
