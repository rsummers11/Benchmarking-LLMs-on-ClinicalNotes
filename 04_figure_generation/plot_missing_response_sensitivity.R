# Plots recall/precision/F1 vs. each model's missing-response rate, under the "drop"/"impute"/
# "worst_case" methods from missing_response_sensitivity.py.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(ggrepel))

results_dir <- "your_path_here/results"
plots_dir <- "your_path_here/plots"

fix_svg_font <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- gsub('"Liberation Serif"', '"Times New Roman"', lines, fixed = TRUE)
  writeLines(lines, path)
}

df <- read_csv(file.path(results_dir, "missing_response_sensitivity_by_model.csv"), show_col_types = FALSE)

impute_long <- df %>%
  select(model_name, model_family, pct_missing,
         Recall = recall_delta_pp, Precision = precision_delta_pp, F1 = f1_delta_pp) %>%
  pivot_longer(c(Recall, Precision, F1), names_to = "metric", values_to = "delta_pp") %>%
  mutate(method = "Impute (missing = false negative)")

worst_long <- df %>%
  select(model_name, model_family, pct_missing,
         Recall = recall_worst_delta_pp, Precision = precision_worst_delta_pp, F1 = f1_worst_delta_pp) %>%
  pivot_longer(c(Recall, Precision, F1), names_to = "metric", values_to = "delta_pp") %>%
  mutate(method = "Worst case (missing = always wrong)")

df_long <- bind_rows(impute_long, worst_long) %>%
  mutate(metric = factor(metric, levels = c("Recall", "Precision", "F1")),
         method = factor(method, levels = c("Impute (missing = false negative)", "Worst case (missing = always wrong)")),
         flag = pct_missing > 10)

p <- ggplot(df_long, aes(x = pct_missing, y = delta_pp, color = model_family)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(size = 2.2, alpha = 0.8) +
  ggrepel::geom_text_repel(data = df_long %>% filter(flag), aes(label = model_name),
                           size = 2.8, family = "Times New Roman", show.legend = FALSE, max.overlaps = 20) +
  facet_grid(method ~ metric) +
  labs(x = "Missing/unparseable responses (%)",
       y = "Change vs. current (drop-missing) method (percentage points)",
       color = "Model family") +
  theme_gray() +
  theme(text = element_text(family = "Times New Roman", size = 12))

svg_path <- file.path(plots_dir, "figureS_missing_response_sensitivity.svg")
ggsave(svg_path, plot = p, width = 11, height = 7, units = "in")
fix_svg_font(svg_path)

# Write to a temp file and rename over the target, to avoid an intermittent overwrite failure.
pdf_path <- file.path(plots_dir, "figureS_missing_response_sensitivity.pdf")
pdf_tmp <- file.path(plots_dir, "figureS_missing_response_sensitivity.pdf.tmp")
ggsave(pdf_tmp, plot = p, width = 11, height = 7, units = "in", device = cairo_pdf)
file.rename(pdf_tmp, pdf_path)

cat("Wrote figures to", plots_dir, "\n")
