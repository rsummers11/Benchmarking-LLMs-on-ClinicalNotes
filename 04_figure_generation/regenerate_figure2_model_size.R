# Regenerates Figure 2 (performance vs. model size, open-weights models only) from the
# Table_9/12/14 CSVs (three panels: recall, precision, F1), Times New Roman font-patched as usual.

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
save_plot <- function(p, name, width, height) {
  svg_path <- file.path(plots_dir, paste0(name, ".svg"))
  ggsave(svg_path, plot = p, width = width, height = height, units = "in")
  fix_svg_font(svg_path)
  pdf_tmp <- file.path(plots_dir, paste0(name, ".pdf.tmp"))
  pdf_path <- file.path(plots_dir, paste0(name, ".pdf"))
  ggsave(pdf_tmp, plot = p, width = width, height = height, units = "in", device = cairo_pdf)
  file.rename(pdf_tmp, pdf_path)
}

# --- per-model overlay points (raw model-level means, matching the Rmd) ---
combined_all <- read_csv(file.path(final_data_dir, "performance_results_by_model.csv"), show_col_types = FALSE)
model_meta <- read_csv(file.path(final_data_dir, "models_run.csv"), show_col_types = FALSE) %>%
  select(model, model_size, model_family) %>% distinct()
combined_all_with_model_sizes <- combined_all %>%
  inner_join(model_meta, by = "model") %>%
  filter(model_family %in% c("Llama", "Qwen")) %>%
  mutate(model_size = as.numeric(as.character(model_size)))

recall_by_size <- read_csv(file.path(final_data_dir, "Table_9_Recall_by_log10_Model_Size(Open Models).csv"), show_col_types = FALSE) %>%
  mutate(model_size = 10^log10_model_size)
precision_by_size <- read_csv(file.path(final_data_dir, "Table_12_Precision_by_log10_Model_Size(Open Models).csv"), show_col_types = FALSE) %>%
  mutate(model_size = 10^log10_model_size)
f1_by_size <- read_csv(file.path(final_data_dir, "Table_14_F1_Score_by_log10_Model_Size(Open Models).csv"), show_col_types = FALSE) %>%
  mutate(model_size = 10^log10_model_size)

p_recall <- ggplot(recall_by_size, aes(x = model_size, y = prob * 100, color = model_family, fill = model_family)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = 100 * asymp.LCL, ymax = 100 * asymp.UCL), color = NA, alpha = 0.2) +
  geom_point(mapping = aes(x = model_size, y = 100 * Recall_mean, color = model_family, size = model_size), data = combined_all_with_model_sizes) +
  labs(x = "Model Size in Billions", y = "Recall (%)") + scale_x_log10() +
  guides(color = guide_legend(title = "model family"), fill = "none", size = guide_legend(title = "model size")) +
  theme_gray() + theme(text = element_text(family = "Times New Roman", size = 14))
save_plot(p_recall, "recall_by_model_sizev2", 7, 4)

p_precision <- ggplot(precision_by_size, aes(x = model_size, y = 100 * prob, color = model_family, fill = model_family)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = 100 * asymp.LCL, ymax = 100 * asymp.UCL), color = NA, alpha = 0.5) +
  geom_point(mapping = aes(x = model_size, y = 100 * Precision_mean, color = model_family, size = model_size), data = combined_all_with_model_sizes) +
  labs(x = "Model Size in Billions", y = "Precision (%)") + scale_x_log10() + ylim(0, 100) +
  guides(color = guide_legend(title = "model family"), fill = "none", size = guide_legend(title = "model size")) +
  theme_gray() + theme(text = element_text(family = "Times New Roman", size = 14))
save_plot(p_precision, "precision_by_model_sizev2", 7, 4)

p_f1 <- ggplot(f1_by_size, aes(x = model_size, y = F1 * 100, color = model_family, fill = model_family)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = F1_LCL * 100, ymax = F1_UCL * 100), color = NA, alpha = 0.2) +
  geom_point(mapping = aes(x = model_size, y = 100 * F1_mean, color = model_family, size = model_size), data = combined_all_with_model_sizes) +
  labs(x = "Model Size in Billions", y = "F1 (%)") + scale_x_log10() + ylim(0, 100) +
  guides(color = guide_legend(title = "model family"), fill = "none", size = guide_legend(title = "model size")) +
  theme_gray() + theme(text = element_text(family = "Times New Roman", size = 14))
save_plot(p_f1, "F1_by_model_size", 7, 4)

cat("Wrote figure2 (recall/precision/F1 by model size) to", plots_dir, "\n")
