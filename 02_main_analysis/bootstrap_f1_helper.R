# Nonparametric bootstrap for F1 confidence intervals, used throughout precisionrecall_v2.R and
# analysis_report_v2.Rmd. Resamples notes (note_id, the true per-patient unit) with replacement,
# refits the recall and precision GLMs on each resample using their own formula(), and re-predicts
# at the given reference grid.
#
# Usage:
#   library(doParallel)
#   ci <- bootstrap_f1_ci(base_data, recall_model, precision_model,
#                          recall_filter, precision_filter, recall_grid, precision_grid)
#   pred_data$F1_LCL <- ci$F1_LCL
#   pred_data$F1_UCL <- ci$F1_UCL
#
# base_data must contain a `note_id` column (the resampling unit) plus every column referenced
# by recall_model/precision_model's formulas. recall_filter/precision_filter are one-argument
# functions selecting the recall-model-fitting and precision-model-fitting subsets from base_data
# (e.g. `function(d) d %>% filter(gt == "True")`). recall_grid/precision_grid are data.frames at
# which to predict (same row count and row order expected, so recall/precision combine 1:1 into
# F1 -- exactly what the surrounding code already builds for the point estimate).

bootstrap_f1_ci <- function(base_data, recall_model, precision_model,
                             recall_filter, precision_filter,
                             recall_grid, precision_grid,
                             n_boot = 2000, seed = 42, n_cores = NULL) {
  stopifnot("note_id" %in% names(base_data))
  recall_formula <- formula(recall_model)
  precision_formula <- formula(precision_model)

  unique_notes <- unique(base_data$note_id)
  note_index <- split(seq_len(nrow(base_data)), base_data$note_id)

  if (is.null(n_cores)) n_cores <- min(32, max(1, parallel::detectCores() - 1))
  doParallel::registerDoParallel(cores = n_cores)

  set.seed(seed)
  note_samples <- lapply(seq_len(n_boot), function(i) sample(unique_notes, length(unique_notes), replace = TRUE))

  results <- foreach::foreach(b = seq_len(n_boot), .errorhandling = "remove") %dopar% {
    idx <- unlist(note_index[as.character(note_samples[[b]])], use.names = FALSE)
    resampled <- base_data[idx, ]
    tryCatch({
      r_data <- recall_filter(resampled)
      p_data <- precision_filter(resampled)
      r_m <- glm(recall_formula, data = r_data, family = binomial)
      p_m <- glm(precision_formula, data = p_data, family = binomial)
      r_pred <- predict(r_m, newdata = recall_grid, type = "response")
      p_pred <- predict(p_m, newdata = precision_grid, type = "response")
      2 * p_pred * r_pred / (p_pred + r_pred)
    }, error = function(e) NULL)
  }
  results <- results[!sapply(results, is.null)]
  mat <- do.call(rbind, results)

  list(
    F1_LCL = apply(mat, 2, quantile, probs = 0.025, na.rm = TRUE),
    F1_UCL = apply(mat, 2, quantile, probs = 0.975, na.rm = TRUE),
    # SD of the bootstrap F1 draws, provided for compatibility only -- use the percentile CI
    # above, since bootstrap distributions aren't assumed symmetric/normal.
    F1_SE = apply(mat, 2, sd, na.rm = TRUE),
    n_reps = nrow(mat)
  )
}
