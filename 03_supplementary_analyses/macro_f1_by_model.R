# Computes macro-F1 (averaged across the 29 comorbidities) per model, from GPT-o3-mini_binary vs.
# ground truth `gt`. Unparseable judge responses are dropped and reported as an unparsed count/
# percentage per model, rather than imputed.

suppressMessages(library(arrow))
suppressMessages(library(dplyr))

final_data_dir <- "your_path_here/final_data"
results_dir <- "your_path_here/results"

ds <- open_dataset(file.path(final_data_dir, "all_data.parquet"))

counts <- ds %>%
  mutate(
    gt_i = as.integer(gt),
    pred_i = as.integer(`GPT-o3-mini_binary`)
  ) %>%
  group_by(model, diagnosis) %>%
  summarise(
    TP = sum(gt_i * pred_i, na.rm = TRUE),
    FP = sum((1L - gt_i) * pred_i, na.rm = TRUE),
    FN = sum(gt_i * (1L - pred_i), na.rm = TRUE),
    n_total = n(),
    n_unparsed = sum(as.integer(is.na(pred_i))),
    .groups = "drop"
  ) %>%
  collect()

# Model metadata (sizes/names).
meta <- read.csv(file.path(final_data_dir, "models_run.csv"), stringsAsFactors = FALSE)

counts <- counts %>%
  mutate(
    precision = ifelse((TP + FP) == 0, 0, TP / (TP + FP)),
    recall    = ifelse((TP + FN) == 0, 0, TP / (TP + FN)),
    f1        = ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  )

macro_f1 <- counts %>%
  group_by(model) %>%
  summarise(
    n_diagnoses = n(),
    macro_precision = mean(precision),
    macro_recall = mean(recall),
    macro_F1 = mean(f1),
    n_unparsed = sum(n_unparsed),
    n_rows = sum(n_total),
    pct_unparsed = 100 * n_unparsed / n_rows,
    .groups = "drop"
  ) %>%
  left_join(meta, by = "model") %>%
  arrange(desc(macro_F1))

write.csv(macro_f1, file.path(results_dir, "macro_f1_by_model.csv"), row.names = FALSE)
write.csv(counts, file.path(results_dir, "precision_recall_f1_by_model_diagnosis.csv"), row.names = FALSE)

cat("\n=== Macro-F1 by model (sorted desc) ===\n")
print(as.data.frame(macro_f1), digits = 3)
