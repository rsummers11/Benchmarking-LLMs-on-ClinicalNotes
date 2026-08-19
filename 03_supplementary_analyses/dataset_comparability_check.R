# Checks structural comparability of the NIH and MIMIC note subsets: descriptive stats
# (note_length, missingness, gt prevalence), plus a mixed-effects model adding `dataset`
# (NIH vs MIMIC) as a fixed effect on top of the published recall/precision covariates.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(lme4))
suppressMessages(library(broom))
suppressMessages(library(broom.mixed))

results_dir <- "your_path_here/results"
final_data_dir <- "your_path_here/final_data"

## ---- 1. Descriptive structural comparability (full DAT, not just the GLMM's judge-parseable subset) ----
load(file.path(final_data_dir, "DAT.RData"))
DAT <- DAT %>% mutate(dataset = if_else(startsWith(as.character(recordId), "NIH"), "NIH", "MIMIC"))

cat("=========== Descriptive structural comparability, NIH vs MIMIC ===========\n")
cat("\n-- Notes and note length (characters) --\n")
print(DAT %>% distinct(note_id, dataset, note_length) %>% group_by(dataset) %>%
  summarise(n_notes = n(), mean_len = mean(note_length), median_len = median(note_length),
            sd_len = sd(note_length), min_len = min(note_length), max_len = max(note_length)))

cat("\n-- Missing/unparseable judge-response rate --\n")
print(DAT %>% group_by(dataset) %>% summarise(n_rows = n(), missing_rate = mean(GPT.o3.mini_binary == "")))

cat("\n-- Ground-truth positive prevalence --\n")
print(DAT %>% group_by(dataset) %>% summarise(prevalence = mean(gt == "True")))

cat("\n-- Diagnoses represented (should be 29/29 in both) --\n")
print(DAT %>% group_by(dataset) %>% summarise(n_diagnoses = n_distinct(diagnosis)))

## ---- 2. Mixed-effects check: does `dataset` matter once model/note/diagnosis structure is modeled? ----
data <- read_csv(file.path(results_dir, "glmm_input.csv"), show_col_types = FALSE) %>%
  filter(!is.na(`GPT-o3-mini_binary`)) %>%
  mutate(
    note_id_chr = as.character(note_id),
    dataset = factor(if_else(grepl("^[0-9]+$", note_id_chr), "NIH", "MIMIC")),
    note_id = factor(note_id),
    diagnosis = factor(diagnosis),
    model_family = factor(model_family),
    pred_true = `GPT-o3-mini_binary`,
    log10_model_size = log10(model_size)
  )
cat("\nrows after dropping missing judge responses:", nrow(data), "\n")
print(data %>% distinct(note_id, dataset) %>% count(dataset))

fit_pair <- function(label, subset_data, formula_glmm_pub, formula_glmm_dataset) {
  cat("\n=========", label, "=========\n")
  cat("n =", nrow(subset_data), "\n")

  t0 <- Sys.time()
  m_pub <- glmer(formula_glmm_pub, data = subset_data, family = binomial,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  cat("published GLMM fit in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

  t0 <- Sys.time()
  m_ds <- glmer(formula_glmm_dataset, data = subset_data, family = binomial,
                 control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  cat("+ dataset fixed effect fit in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")
  cat("convergence:", ifelse(length(m_ds@optinfo$conv$lme4$messages) == 0, "OK", paste(m_ds@optinfo$conv$lme4$messages, collapse = "; ")), "\n")

  strip_scale <- function(x) sub("^scale\\((.*)\\)$", "\\1", x)
  cmp <- full_join(
    tidy(m_pub, effects = "fixed") %>% mutate(term = strip_scale(term)) %>%
      select(term, estimate_published = estimate, p_published = p.value),
    tidy(m_ds, effects = "fixed") %>% mutate(term = strip_scale(term)) %>%
      select(term, estimate_with_dataset = estimate, p_with_dataset = p.value),
    by = "term"
  ) %>% mutate(label = label)
  cat("Fixed-effect comparison (published GLMM vs. + dataset):\n")
  print(cmp, n = Inf)

  cat("\nLikelihood ratio test (does adding `dataset` significantly improve fit?):\n")
  print(anova(m_pub, m_ds))

  cmp
}

closed <- data %>% filter(open_source == "no")
closed_recall <- fit_pair(
  "Closed models -- Recall",
  closed %>% filter(gt),
  pred_true ~ model_family + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis),
  pred_true ~ model_family + scale(note_length) + scale(duration_since_release) + dataset + (1 | note_id) + (1 | diagnosis)
)
closed_precision <- fit_pair(
  "Closed models -- Precision",
  closed %>% filter(pred_true),
  gt ~ model_family + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis),
  gt ~ model_family + scale(note_length) + scale(duration_since_release) + dataset + (1 | note_id) + (1 | diagnosis)
)

open_models <- data %>% filter(open_source == "yes")
open_recall <- fit_pair(
  "Open models -- Recall",
  open_models %>% filter(gt),
  pred_true ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis),
  pred_true ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + dataset + (1 | note_id) + (1 | diagnosis)
)
open_precision <- fit_pair(
  "Open models -- Precision",
  open_models %>% filter(pred_true),
  gt ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis),
  gt ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + dataset + (1 | note_id) + (1 | diagnosis)
)

all_cmp <- bind_rows(closed_recall, closed_precision, open_recall, open_precision)
write.csv(all_cmp, file.path(results_dir, "dataset_covariate_glmm_comparison.csv"), row.names = FALSE)
cat("\nWrote dataset_covariate_glmm_comparison.csv to", results_dir, "\n")
