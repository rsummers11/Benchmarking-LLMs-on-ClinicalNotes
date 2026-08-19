# Computes bootstrap confidence intervals for F1 from the GLM with diagnosis as a covariate
# (the by-comorbidity figure), resampling notes and refitting recall/precision each replicate.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(emmeans))
suppressMessages(library(doParallel))

results_dir <- "your_path_here/results"
N_BOOT <- 2000 # matches the manuscript's own bootstrap convention (Table 2's per-model F1 CIs)
SEED <- 42

open_models <- read_csv(file.path(results_dir, "diagnosis_bootstrap_input.csv"), show_col_types = FALSE) %>%
  filter(!is.na(`GPT-o3-mini_binary`)) %>%
  mutate(
    log10_model_size = log10(model_size),
    diagnosis = factor(diagnosis),
    model_family = factor(model_family),
    dataset = factor(dataset),
    pred_true = `GPT-o3-mini_binary`
  )

cat("open_models rows:", nrow(open_models), "\n")
unique_notes <- unique(open_models$note_id)
cat("unique notes:", length(unique_notes), "\n")
note_index <- split(seq_len(nrow(open_models)), open_models$note_id)

fit_f1_by_diagnosis <- function(idx) {
  data <- open_models[idx, ]

  recall_data <- data %>% filter(gt)
  recall_model <- glm(
    pred_true ~ model_family + log10_model_size + note_length + duration_since_release + dataset + diagnosis,
    data = recall_data, family = binomial
  )
  recall_emm <- as.data.frame(emmeans(recall_model, ~ model_family + diagnosis, type = "response")) %>%
    select(model_family, diagnosis, recall = prob)

  precision_data <- data %>% filter(pred_true)
  precision_model <- glm(
    gt ~ model_family + log10_model_size + note_length + duration_since_release + dataset + diagnosis,
    data = precision_data, family = binomial
  )
  precision_emm <- as.data.frame(emmeans(precision_model, ~ model_family + diagnosis, type = "response")) %>%
    select(model_family, diagnosis, precision = prob)

  merged <- inner_join(recall_emm, precision_emm, by = c("model_family", "diagnosis")) %>%
    mutate(F1 = 2 * precision * recall / (precision + recall))
  merged
}

cat("\nFitting on original (non-resampled) data for point estimates...\n")
t0 <- Sys.time()
point_est <- fit_f1_by_diagnosis(seq_len(nrow(open_models)))
cat("done in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")
print(head(point_est))

cat("\nStarting bootstrap (", N_BOOT, "reps, parallel)...\n")
n_cores <- min(32, max(1, detectCores() - 1)) # cap workers -- forking duplicates the ~450k-row frame per worker
cat("using", n_cores, "cores (fork-based, no clusterExport needed)\n")
registerDoParallel(cores = n_cores)

set.seed(SEED)
note_samples <- lapply(seq_len(N_BOOT), function(i) sample(unique_notes, length(unique_notes), replace = TRUE))

t0 <- Sys.time()
boot_results <- foreach(b = seq_len(N_BOOT), .packages = c("dplyr", "emmeans"), .errorhandling = "remove") %dopar% {
  chosen <- note_samples[[b]]
  idx <- unlist(note_index[as.character(chosen)], use.names = FALSE)
  tryCatch(fit_f1_by_diagnosis(idx), error = function(e) NULL)
}
cat("bootstrap done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min;",
    sum(sapply(boot_results, is.null)), "failed reps out of", N_BOOT, "\n")

boot_results <- boot_results[!sapply(boot_results, is.null)]
all_boot <- bind_rows(boot_results, .id = "rep")

ci_summary <- all_boot %>%
  group_by(model_family, diagnosis) %>%
  summarise(
    F1_LCL_boot = quantile(F1, 0.025, na.rm = TRUE),
    F1_UCL_boot = quantile(F1, 0.975, na.rm = TRUE),
    n_reps = n(),
    .groups = "drop"
  )

final <- point_est %>% left_join(ci_summary, by = c("model_family", "diagnosis"))
write_csv(final, file.path(results_dir, "performance_by_diagnosis_bootstrap.csv"))
cat("\nWrote performance_by_diagnosis_bootstrap.csv\n")
print(final %>% arrange(F1) %>% head(10))
