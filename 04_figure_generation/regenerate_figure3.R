# Regenerates Figure 3 (performance vs. model release recency, closed-source + open-weights)
# from the Table_2/4/6 (closed) and Table_8/11/15 (open) release-duration CSVs plus DAT.RData
# and performance_results_by_model.csv, writing svg + pdf to plots/. Same font-family patch as
# regenerate_figure1.R.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))

final_data_dir <- "your_path_here/final_data"
plots_dir <- "your_path_here/plots"
reference_date <- "2025-06-06" # must match pre_process.R

fix_svg_font <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- gsub('"Liberation Serif"', '"Times New Roman"', lines, fixed = TRUE)
  writeLines(lines, path)
}

load(file.path(final_data_dir, "DAT.RData"))
combined_all <- read_csv(file.path(final_data_dir, "performance_results_by_model.csv"), show_col_types = FALSE)

make_recency_plot <- function(recall_file, precision_file, f1_file, open_source_flag) {
  recall_with_time <- read_csv(file.path(final_data_dir, recall_file), show_col_types = FALSE)
  precision_with_time <- read_csv(file.path(final_data_dir, precision_file), show_col_types = FALSE)
  f1_with_time <- read_csv(file.path(final_data_dir, f1_file), show_col_types = FALSE)

  recall_with_time$performance_metric <- "Recall"
  precision_with_time$performance_metric <- "Precision"
  f1_with_time$performance_metric <- "F1"
  recall_with_time <- recall_with_time %>% select(duration_since_release, model_family, prob, asymp.LCL, asymp.UCL, performance_metric)
  precision_with_time <- precision_with_time %>% select(duration_since_release, model_family, prob, asymp.LCL, asymp.UCL, performance_metric)
  f1_with_time <- f1_with_time %>% rename(prob = F1, se = se_F1, asymp.LCL = F1_LCL, asymp.UCL = F1_UCL) %>%
    select(duration_since_release, model_family, prob, asymp.LCL, asymp.UCL, performance_metric)
  all_metrics_with_time <- rbind(recall_with_time, precision_with_time, f1_with_time)
  all_metrics_with_time <- all_metrics_with_time %>% mutate(estimated_date_of_release = as.Date(reference_date) - duration_since_release)

  model_dates <- DAT %>% filter(open_source == open_source_flag) %>% select(model, date_of_release, model_family) %>% unique
  combined_with_dates <- inner_join(combined_all, model_dates)
  combined_with_dates_long <- combined_with_dates %>%
    select(model, Recall_mean, Precision_mean, F1_mean, date_of_release, model_family) %>%
    rename(Recall = Recall_mean, Precision = Precision_mean, F1 = F1_mean) %>%
    pivot_longer(c(Recall, Precision, F1), names_to = "performance_metric", values_to = "prob") %>%
    mutate(date_of_release = as.Date(date_of_release))

  ggplot(all_metrics_with_time, aes(x = estimated_date_of_release, y = 100 * prob, color = model_family, fill = model_family)) +
    geom_line() +
    geom_ribbon(aes(ymin = asymp.LCL * 100, ymax = asymp.UCL * 100), color = NA, alpha = 0.2) +
    xlab("Date of model release") +
    scale_y_continuous(limits = c(0, 100), name = "Metric value (%)") +
    geom_point(mapping = aes(x = date_of_release, y = 100 * prob, color = model_family), data = combined_with_dates_long) +
    facet_wrap(~factor(performance_metric, levels = c("Recall", "Precision", "F1")), nrow = 1) +
    theme_gray() +
    theme(legend.title = element_blank(), text = element_text(family = "Times New Roman", size = 14))
}

# --- Closed-source models ---
p_closed <- make_recency_plot(
  "Table_2_Recall_by_Release_Duration(Closed Models).csv",
  "Table_4_Precision_by_Release_Duration(Closed Models).csv",
  "Table_6_F1_Score_by_Release_Duration(Closed Models).csv",
  "no"
)
svg_closed <- file.path(plots_dir, "figure3_performance_by_release_recency_closed.svg")
ggsave(svg_closed, plot = p_closed, width = 8, height = 4, units = "in")
fix_svg_font(svg_closed)
ggsave(file.path(plots_dir, "figure3_performance_by_release_recency_closed.pdf"), plot = p_closed, width = 8, height = 4, units = "in", device = cairo_pdf)

# --- Open-weights models ---
p_open <- make_recency_plot(
  "Table_8_Recall_by_Release_Duration(Open Models).csv",
  "Table_11_Precision_by_Release_Duration(Open Models).csv",
  "Table_15_F1_Score_by_Release_Duration(Open Models).csv",
  "yes"
)
svg_open <- file.path(plots_dir, "figure3_performance_by_release_recency_open.svg")
ggsave(svg_open, plot = p_open, width = 8, height = 4, units = "in")
fix_svg_font(svg_open)
ggsave(file.path(plots_dir, "figure3_performance_by_release_recency_open.pdf"), plot = p_open, width = 8, height = 4, units = "in", device = cairo_pdf)

cat("Wrote figures to", plots_dir, "\n")
