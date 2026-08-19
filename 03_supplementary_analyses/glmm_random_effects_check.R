# Fits GLMMs with (1|note_id) + (1|diagnosis) random intercepts for all 4 of the manuscript's GLM
# specifications (closed/open x recall/precision), and compares the fixed-effect estimates and
# random-effect variance components against the published fixed-effects-only GLM.
#
# Input: results/glmm_input.csv (built from all_data.parquet).

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(lme4))
suppressMessages(library(broom))
suppressMessages(library(broom.mixed))

results_dir <- "your_path_here/results"

data <- read_csv(file.path(results_dir, "glmm_input.csv"), show_col_types = FALSE)
cat("rows read:", nrow(data), "\n")
cat("gt class:", class(data$gt), " GPT-o3-mini_binary class:", class(data[["GPT-o3-mini_binary"]]), "\n")

# readr parses both gt and GPT-o3-mini_binary as genuine logical (TRUE/FALSE/NA) columns, not
# strings -- match analysis_report_v2.Rmd line 97 (drop unparseable/missing judge responses) via
# is.na(), and use the logical column directly rather than comparing to the string "True" (which
# would silently coerce the logical to "TRUE"/"FALSE" and never match "True"/"False").
data <- data %>% filter(!is.na(`GPT-o3-mini_binary`))
cat("rows after dropping missing:", nrow(data), "\n")

data <- data %>% mutate(
  note_id = factor(note_id),
  diagnosis = factor(diagnosis),
  model_family = factor(model_family),
  pred_true = `GPT-o3-mini_binary`,
  log10_model_size = log10(model_size)
)

fit_pair <- function(label, subset_data, formula_fixed, formula_mixed) {
  cat("\n=========", label, "=========\n")
  cat("n =", nrow(subset_data), "\n")

  t0 <- Sys.time()
  m_fixed <- glm(formula_fixed, data = subset_data, family = binomial)
  cat("glm (fixed-effects only) fit in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")

  t0 <- Sys.time()
  m_mixed <- glmer(formula_mixed, data = subset_data, family = binomial,
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
  cat("glmer (+ note_id, diagnosis random intercepts) fit in", round(difftime(Sys.time(), t0, units = "secs"), 1), "sec\n")
  cat("convergence:", ifelse(length(m_mixed@optinfo$conv$lme4$messages) == 0, "OK", paste(m_mixed@optinfo$conv$lme4$messages, collapse = "; ")), "\n")

  vc <- as.data.frame(VarCorr(m_mixed))
  cat("Random-effect variance components:\n")
  print(vc[, c("grp", "vcov", "sdcor")])

  # both formulas use scale(x) on continuous covariates now (same units in glm and glmer), so
  # strip the wrapper just to get a clean display name -- estimates are now directly comparable.
  strip_scale <- function(x) sub("^scale\\((.*)\\)$", "\\1", x)
  fixed_cmp <- full_join(
    tidy(m_fixed) %>% mutate(term = strip_scale(term)) %>%
      select(term, estimate_glm = estimate, se_glm = std.error, p_glm = p.value),
    tidy(m_mixed, effects = "fixed") %>% mutate(term = strip_scale(term)) %>%
      select(term, estimate_glmm = estimate, se_glmm = std.error, p_glmm = p.value),
    by = "term"
  ) %>% mutate(label = label)
  cat("Fixed-effect comparison (glm vs glmer):\n")
  print(fixed_cmp, n = Inf)

  list(varcomp = vc %>% mutate(label = label), fixed_cmp = fixed_cmp)
}

# --- Closed-source models ---
closed <- data %>% filter(open_source == "no")
closed_recall <- fit_pair(
  "Closed models -- Recall",
  closed %>% filter(gt),
  as.factor(pred_true) ~ model_family + scale(note_length) + scale(duration_since_release),
  pred_true ~ model_family + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis)
)
closed_precision <- fit_pair(
  "Closed models -- Precision",
  closed %>% filter(pred_true),
  as.factor(gt) ~ model_family + scale(note_length) + scale(duration_since_release),
  gt ~ model_family + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis)
)

# --- Open-weights models ---
open <- data %>% filter(open_source == "yes")
open_recall <- fit_pair(
  "Open models -- Recall",
  open %>% filter(gt),
  as.factor(pred_true) ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release),
  pred_true ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis)
)
open_precision <- fit_pair(
  "Open models -- Precision",
  open %>% filter(pred_true),
  as.factor(gt) ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release),
  gt ~ model_family + scale(log10_model_size) + scale(note_length) + scale(duration_since_release) + (1 | note_id) + (1 | diagnosis)
)

all_varcomp <- bind_rows(closed_recall$varcomp, closed_precision$varcomp, open_recall$varcomp, open_precision$varcomp)
all_fixed_cmp <- bind_rows(closed_recall$fixed_cmp, closed_precision$fixed_cmp, open_recall$fixed_cmp, open_precision$fixed_cmp)

write.csv(all_varcomp, file.path(results_dir, "glmm_random_effect_variances.csv"), row.names = FALSE)
write.csv(all_fixed_cmp, file.path(results_dir, "glmm_vs_glm_fixed_effects.csv"), row.names = FALSE)
cat("\nWrote glmm_random_effect_variances.csv and glmm_vs_glm_fixed_effects.csv to", results_dir, "\n")
