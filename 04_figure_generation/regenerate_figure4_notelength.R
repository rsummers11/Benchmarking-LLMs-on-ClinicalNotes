# Regenerates Figure 4 (performance vs. note length, closed-source + open-weights) from the
# Table_16/17 CSVs, Times New Roman font-patched as usual.

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

make_notelength_plot <- function(csv_name) {
  pred_data <- read_csv(file.path(final_data_dir, csv_name), show_col_types = FALSE)
  recall_portion <- pred_data %>% select(model_family, note_length, recall, recall_asymp.LCL, recall_asymp.UCL) %>%
    rename(metric_value = recall, asymp.LCL = recall_asymp.LCL, asymp.UCL = recall_asymp.UCL) %>% mutate(performance_metric = "Recall")
  precision_portion <- pred_data %>% select(model_family, note_length, precision, precision_asymp.LCL, precision_asymp.UCL) %>%
    rename(metric_value = precision, asymp.LCL = precision_asymp.LCL, asymp.UCL = precision_asymp.UCL) %>% mutate(performance_metric = "Precision")
  f1_portion <- pred_data %>% select(model_family, note_length, F1, F1_LCL, F1_UCL) %>%
    rename(metric_value = F1, asymp.LCL = F1_LCL, asymp.UCL = F1_UCL) %>% mutate(performance_metric = "F1")
  combined <- rbind(recall_portion, precision_portion, f1_portion) %>%
    mutate(metric_value = 100 * metric_value, asymp.LCL = 100 * asymp.LCL, asymp.UCL = 100 * asymp.UCL)

  ggplot(combined, aes(x = note_length / 1000, y = metric_value, color = model_family, fill = model_family)) +
    geom_line() +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), color = NA, alpha = 0.2) +
    facet_wrap(~factor(performance_metric, levels = c("Recall", "Precision", "F1"))) +
    scale_x_continuous(name = "Note length (in thousand words)") +
    scale_y_continuous(name = "Metric value (%)", limits = c(0, 100)) +
    theme(legend.title = element_blank(), text = element_text(family = "Times New Roman", size = 14))
}

p_closed <- make_notelength_plot("Table_16_Recall_and_Precision_by_NoteLength(Closed Models).csv")
save_plot(p_closed, "performance_with_notelength_closedv2", 8, 4)

p_open <- make_notelength_plot("Table_17_Recall_and_Precision_by_NoteLength(Open Models).csv")
save_plot(p_open, "performance_with_notelength_openv2", 8, 4)

cat("Wrote figure4 (performance by notelength, closed + open) to", plots_dir, "\n")
