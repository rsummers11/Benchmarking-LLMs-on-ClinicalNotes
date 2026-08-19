# Bootstraps F1 confidence intervals for the model-family/duration/size/notelength tables
# (closed and open models): resamples notes with replacement, refits all 4 GLMs (closed/open x
# recall/precision) per replicate, and re-predicts at the original grids to get percentile CIs.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(doParallel))

results_dir <- "your_path_here/results"
final_data_dir <- "your_path_here/final_data"
N_BOOT <- 2000
SEED <- 42
N_GRID <- 100

all_data <- read_csv(file.path(results_dir, "all_models_bootstrap_input.csv"), show_col_types = FALSE) %>%
  filter(!is.na(`GPT-o3-mini_binary`)) %>%
  mutate(
    pred_true = `GPT-o3-mini_binary`,
    log10_model_size = log10(model_size)
  )

closed_data <- all_data %>% filter(open_source == "no") %>% mutate(model_family = factor(model_family))
open_data <- all_data %>% filter(open_source == "yes") %>% mutate(model_family = factor(model_family))
cat("closed rows:", nrow(closed_data), " open rows:", nrow(open_data), "\n")

unique_notes <- unique(all_data$note_id)
cat("unique notes:", length(unique_notes), "\n")
closed_note_index <- split(seq_len(nrow(closed_data)), closed_data$note_id)
open_note_index <- split(seq_len(nrow(open_data)), open_data$note_id)

closed_families <- levels(closed_data$model_family)
open_families <- levels(open_data$model_family)

# --- reference means, computed SEPARATELY for the recall-fitting (gt==True) and
# precision-fitting (pred_true==True) subsets, matching precisionrecall_v2.R exactly ---
closed_recall_sub <- closed_data %>% filter(gt)
closed_precision_sub <- closed_data %>% filter(pred_true)
open_recall_sub <- open_data %>% filter(gt)
open_precision_sub <- open_data %>% filter(pred_true)

ref <- function(df, col) mean(df[[col]])
rng <- function(df, col) c(min(df[[col]]), max(df[[col]]))

# sanity check: recall and precision subsets must cover the same models (needed for grid alignment)
stopifnot(isTRUE(all.equal(rng(closed_recall_sub, "duration_since_release"), rng(closed_precision_sub, "duration_since_release"))))
stopifnot(isTRUE(all.equal(rng(open_recall_sub, "duration_since_release"), rng(open_precision_sub, "duration_since_release"))))
stopifnot(isTRUE(all.equal(rng(open_recall_sub, "log10_model_size"), rng(open_precision_sub, "log10_model_size"))))

closed_duration_seq <- seq(rng(closed_recall_sub, "duration_since_release")[1], rng(closed_recall_sub, "duration_since_release")[2], length.out = N_GRID)
closed_notelength_seq <- seq(min(closed_data$note_length), max(closed_data$note_length), length.out = N_GRID) # Table 16 uses the full closed_models range
open_duration_seq <- seq(rng(open_recall_sub, "duration_since_release")[1], rng(open_recall_sub, "duration_since_release")[2], length.out = N_GRID)
open_size_seq <- seq(rng(open_recall_sub, "log10_model_size")[1], rng(open_recall_sub, "log10_model_size")[2], length.out = N_GRID)
open_notelength_seq <- seq(min(open_recall_sub$note_length), max(open_recall_sub$note_length), length.out = N_GRID)

make_grid <- function(families, fixed) {
  g <- expand.grid(model_family = families, stringsAsFactors = FALSE)
  for (n in names(fixed)) g[[n]] <- fixed[[n]]
  g
}
make_grid_seq <- function(families, seq_name, seq_vals, fixed) {
  g <- expand.grid(model_family = families, x = seq_vals, stringsAsFactors = FALSE)
  names(g)[names(g) == "x"] <- seq_name
  for (n in names(fixed)) g[[n]] <- fixed[[n]]
  g
}

# --- Closed: recall grids (ref = recall subset means) and precision grids (ref = precision subset means) ---
cr_nl <- ref(closed_recall_sub, "note_length"); cr_dur <- ref(closed_recall_sub, "duration_since_release")
cp_nl <- ref(closed_precision_sub, "note_length"); cp_dur <- ref(closed_precision_sub, "duration_since_release")

g_closed_family_r <- make_grid(closed_families, list(note_length = cr_nl, duration_since_release = cr_dur))
g_closed_family_p <- make_grid(closed_families, list(note_length = cp_nl, duration_since_release = cp_dur))
g_closed_duration_r <- make_grid_seq(closed_families, "duration_since_release", closed_duration_seq, list(note_length = cr_nl))
g_closed_duration_p <- make_grid_seq(closed_families, "duration_since_release", closed_duration_seq, list(note_length = cp_nl))
# Table 16 (closed notelength) shares ONE pred_data grid across recall_preds and precision_preds,
# with duration_since_release held at the mean of the full (unfiltered) closed-model subset.
closed_notelength_dur_ref <- mean(closed_data$duration_since_release)
g_closed_notelength_r <- make_grid_seq(closed_families, "note_length", closed_notelength_seq, list(duration_since_release = closed_notelength_dur_ref))
g_closed_notelength_p <- g_closed_notelength_r

# --- Open: same idea, plus log10_model_size ---
or_nl <- ref(open_recall_sub, "note_length"); or_dur <- ref(open_recall_sub, "duration_since_release"); or_sz <- ref(open_recall_sub, "log10_model_size")
op_nl <- ref(open_precision_sub, "note_length"); op_dur <- ref(open_precision_sub, "duration_since_release"); op_sz <- ref(open_precision_sub, "log10_model_size")

g_open_family_r <- make_grid(open_families, list(log10_model_size = or_sz, note_length = or_nl, duration_since_release = or_dur))
g_open_family_p <- make_grid(open_families, list(log10_model_size = op_sz, note_length = op_nl, duration_since_release = op_dur))
g_open_duration_r <- make_grid_seq(open_families, "duration_since_release", open_duration_seq, list(log10_model_size = or_sz, note_length = or_nl))
g_open_duration_p <- make_grid_seq(open_families, "duration_since_release", open_duration_seq, list(log10_model_size = op_sz, note_length = op_nl))
g_open_size_r <- make_grid_seq(open_families, "log10_model_size", open_size_seq, list(note_length = or_nl, duration_since_release = or_dur))
g_open_size_p <- make_grid_seq(open_families, "log10_model_size", open_size_seq, list(note_length = op_nl, duration_since_release = op_dur))
# Table 17 (open notelength) similarly shares ONE pred_data grid across recall_preds and
# precision_preds, with the shared reference at the mean of the precision-fitting subset.
g_open_notelength_r <- make_grid_seq(open_families, "note_length", open_notelength_seq, list(log10_model_size = op_sz, duration_since_release = op_dur))
g_open_notelength_p <- g_open_notelength_r

fit_all <- function(closed_idx, open_idx) {
  cd <- closed_data[closed_idx, ]
  od <- open_data[open_idx, ]

  closed_recall_m <- glm(pred_true ~ model_family + note_length + duration_since_release, data = cd %>% filter(gt), family = binomial)
  closed_precision_m <- glm(gt ~ model_family + note_length + duration_since_release, data = cd %>% filter(pred_true), family = binomial)
  open_recall_m <- glm(pred_true ~ model_family + log10_model_size + note_length + duration_since_release, data = od %>% filter(gt), family = binomial)
  open_precision_m <- glm(gt ~ model_family + log10_model_size + note_length + duration_since_release, data = od %>% filter(pred_true), family = binomial)

  pr <- function(m, g) predict(m, newdata = g, type = "response")
  f1 <- function(p, r) 2 * p * r / (p + r)

  list(
    closed_family = f1(pr(closed_precision_m, g_closed_family_p), pr(closed_recall_m, g_closed_family_r)),
    closed_duration = f1(pr(closed_precision_m, g_closed_duration_p), pr(closed_recall_m, g_closed_duration_r)),
    closed_notelength = f1(pr(closed_precision_m, g_closed_notelength_p), pr(closed_recall_m, g_closed_notelength_r)),
    open_family = f1(pr(open_precision_m, g_open_family_p), pr(open_recall_m, g_open_family_r)),
    open_duration = f1(pr(open_precision_m, g_open_duration_p), pr(open_recall_m, g_open_duration_r)),
    open_size = f1(pr(open_precision_m, g_open_size_p), pr(open_recall_m, g_open_size_r)),
    open_notelength = f1(pr(open_precision_m, g_open_notelength_p), pr(open_recall_m, g_open_notelength_r))
  )
}

cat("\nFitting on original (non-resampled) data for point estimates...\n")
t0 <- Sys.time()
point_est <- fit_all(seq_len(nrow(closed_data)), seq_len(nrow(open_data)))
cat("done in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

cat("\nStarting bootstrap (", N_BOOT, "reps, parallel)...\n")
n_cores <- min(32, max(1, detectCores() - 1))
cat("using", n_cores, "cores\n")
registerDoParallel(cores = n_cores)

set.seed(SEED)
note_samples <- lapply(seq_len(N_BOOT), function(i) sample(unique_notes, length(unique_notes), replace = TRUE))

t0 <- Sys.time()
boot_results <- foreach(b = seq_len(N_BOOT), .errorhandling = "remove") %dopar% {
  chosen <- note_samples[[b]]
  closed_idx <- unlist(closed_note_index[as.character(chosen)], use.names = FALSE)
  open_idx <- unlist(open_note_index[as.character(chosen)], use.names = FALSE)
  tryCatch(fit_all(closed_idx, open_idx), error = function(e) NULL)
}
cat("bootstrap done in", round(difftime(Sys.time(), t0, units = "mins"), 1), "min;",
    sum(sapply(boot_results, is.null)), "failed reps out of", N_BOOT, "\n")
boot_results <- boot_results[!sapply(boot_results, is.null)]

percentile_ci <- function(name, grid) {
  mat <- do.call(rbind, lapply(boot_results, function(r) r[[name]]))
  lcl <- apply(mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  ucl <- apply(mat, 2, quantile, probs = 0.975, na.rm = TRUE)
  grid %>% mutate(F1 = point_est[[name]], F1_LCL = lcl, F1_UCL = ucl, n_reps = nrow(mat))
}

out_closed_family <- percentile_ci("closed_family", g_closed_family_r %>% select(model_family))
out_closed_duration <- percentile_ci("closed_duration", g_closed_duration_r %>% select(model_family, duration_since_release))
out_closed_notelength <- percentile_ci("closed_notelength", g_closed_notelength_r %>% select(model_family, note_length))
out_open_family <- percentile_ci("open_family", g_open_family_r %>% select(model_family))
out_open_duration <- percentile_ci("open_duration", g_open_duration_r %>% select(model_family, duration_since_release))
out_open_size <- percentile_ci("open_size", g_open_size_r %>% select(model_family, log10_model_size))
out_open_notelength <- percentile_ci("open_notelength", g_open_notelength_r %>% select(model_family, note_length))

write_csv(out_closed_family, file.path(results_dir, "bootstrap_Table_5_F1_by_family_closed.csv"))
write_csv(out_closed_duration, file.path(results_dir, "bootstrap_Table_6_F1_by_duration_closed.csv"))
write_csv(out_closed_notelength, file.path(results_dir, "bootstrap_Table_16_F1_by_notelength_closed.csv"))
write_csv(out_open_family, file.path(results_dir, "bootstrap_Table_13_F1_by_family_open.csv"))
write_csv(out_open_duration, file.path(results_dir, "bootstrap_Table_15_F1_by_duration_open.csv"))
write_csv(out_open_size, file.path(results_dir, "bootstrap_Table_14_F1_by_size_open.csv"))
write_csv(out_open_notelength, file.path(results_dir, "bootstrap_Table_17_F1_by_notelength_open.csv"))

cat("\nWrote all 7 bootstrap CI tables to", results_dir, "\n")
cat("\n=== Closed family (Table 5) ===\n"); print(out_closed_family)
cat("\n=== Open family (Table 13) ===\n"); print(out_open_family)
