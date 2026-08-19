# Regenerates Figure 1 (performance by model family, closed-source + open-weights) from the
# Table_1/3/5 (closed) and Table_7/10/13 (open) CSVs, writing svg + pdf to plots/. Patches the
# SVG's font-family back to "Times New Roman" post-hoc, since svglite bakes in the
# fontconfig-resolved name ("Liberation Serif") instead of the requested font.

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

make_family_plot <- function(recall, precision, f1, response_col, prob_expr) {
  recall$metric <- "Recall"
  precision$metric <- "Precision"
  metrics <- bind_rows(recall, precision)

  f1_reformat <- tibble(model_family = f1$model_family, response = f1$F1, SE = f1$se_F1,
                         asymp.LCL = f1$F1_LCL, asymp.UCL = f1$F1_UCL, metric = "F1")
  metrics <- bind_rows(metrics, f1_reformat)
  metrics$metric <- factor(metrics$metric, levels = c("Recall", "Precision", "F1"))
  metrics$model_family <- as.factor(metrics$model_family)
  metrics <- metrics %>% rename(performance_metric = metric)

  ggplot(metrics, aes(x = model_family, y = response * 100, color = model_family)) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    geom_errorbar(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100),
                  position = position_dodge(width = 0.5), width = 0.2) +
    labs(title = "", x = "Model family", y = "Metric value (%)") +
    facet_wrap(~performance_metric) +
    theme_gray() + scale_y_continuous(limits = c(0, 100)) +
    theme(legend.title = element_blank(), text = element_text(family = "Times New Roman", size = 14))
}

# --- Closed-source models ---
recall_closed <- read_csv(file.path(final_data_dir, "Table_1_Recall_by_Model_Family(Closed Models).csv"), show_col_types = FALSE)
precision_closed <- read_csv(file.path(final_data_dir, "Table_3_Precision_by_Model_Family(Closed Models).csv"), show_col_types = FALSE)
f1_closed <- read_csv(file.path(final_data_dir, "Table_5_F1_Score_by_Model_Family(Closed Models).csv"), show_col_types = FALSE)

p_closed <- make_family_plot(recall_closed, precision_closed, f1_closed)
svg_closed <- file.path(plots_dir, "figure1_performance_by_model_family_closed.svg")
ggsave(svg_closed, plot = p_closed, width = 6, height = 5, units = "in")
fix_svg_font(svg_closed)
ggsave(file.path(plots_dir, "figure1_performance_by_model_family_closed.pdf"), plot = p_closed, width = 6, height = 5, units = "in", device = cairo_pdf)

# --- Open-weights models ---
recall_open <- read_csv(file.path(final_data_dir, "Table_7_Recall_by_Model_Family(Open Models).csv"), show_col_types = FALSE)
precision_open <- read_csv(file.path(final_data_dir, "Table_10_Precision_by_Model_Family(Open Models).csv"), show_col_types = FALSE)
f1_open <- read_csv(file.path(final_data_dir, "Table_13_F1_Score_by_Model_Family(Open Models).csv"), show_col_types = FALSE)

recall_open$performance_metric <- "Recall"
precision_open$performance_metric <- "Precision"
f1_open_reformat <- tibble(model_family = f1_open$model_family, prob = f1_open$F1, SE = f1_open$se_F1,
                            asymp.LCL = f1_open$F1_LCL, asymp.UCL = f1_open$F1_UCL, performance_metric = "F1")
open_metrics <- bind_rows(recall_open, precision_open, f1_open_reformat)
open_metrics$performance_metric <- factor(open_metrics$performance_metric, levels = c("Recall", "Precision", "F1"))

p_open <- ggplot(open_metrics, aes(x = model_family, y = prob * 100, color = model_family)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), position = position_dodge(width = 0.5), width = 0.2) +
  labs(title = "", x = "Model family", y = "Metric value (%)") +
  facet_wrap(~performance_metric) +
  theme_gray() + scale_y_continuous(limits = c(0, 100)) +
  theme(legend.title = element_blank(), text = element_text(family = "Times New Roman", size = 14))

svg_open <- file.path(plots_dir, "figure1_performance_by_model_family_open.svg")
ggsave(svg_open, plot = p_open, width = 6, height = 5, units = "in")
fix_svg_font(svg_open)
ggsave(file.path(plots_dir, "figure1_performance_by_model_family_open.pdf"), plot = p_open, width = 6, height = 5, units = "in", device = cairo_pdf)

cat("Wrote figures to", plots_dir, "\n")
