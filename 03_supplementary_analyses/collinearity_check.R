# Checks collinearity between log10(model_size) and duration_since_release in the open-weights
# GLMs: fits the published GLM specifications and reports VIF plus the pairwise correlation.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(car))

results_dir <- "your_path_here/results"

data <- read_csv(file.path(results_dir, "glmm_input.csv"), show_col_types = FALSE)
data <- data %>% filter(!is.na(`GPT-o3-mini_binary`)) %>%
  mutate(model_family = factor(model_family), pred_true = `GPT-o3-mini_binary`, log10_model_size = log10(model_size))

open <- data %>% filter(open_source == "yes")

cat("=== Pairwise correlation: log10(model_size) vs duration_since_release, at the OBSERVATION level ===\n")
print(cor.test(open$log10_model_size, open$duration_since_release))

cat("\n=== Open models -- Recall GLM (published spec) ===\n")
recall_data <- open %>% filter(gt)
m_recall <- glm(pred_true ~ model_family + log10_model_size + note_length + duration_since_release,
                 data = recall_data, family = binomial)
print(summary(m_recall)$coefficients)
cat("\nVIF:\n")
print(vif(m_recall))

cat("\n=== Open models -- Precision GLM (published spec) ===\n")
precision_data <- open %>% filter(pred_true)
m_precision <- glm(gt ~ model_family + log10_model_size + note_length + duration_since_release,
                    data = precision_data, family = binomial)
print(summary(m_precision)$coefficients)
cat("\nVIF:\n")
print(vif(m_precision))

cat("\n=== For context: what if model_generation WERE included? (not in the published model) ===\n")
gen_lookup <- pq_generation <- read_csv(file.path(results_dir, "..", "..", "final_data", "models_run.csv"), show_col_types = FALSE) %>%
  select(model, model_generation) %>% distinct()
recall_data2 <- recall_data %>% left_join(gen_lookup, by = "model") %>%
  mutate(model_generation = factor(paste(model_family, model_generation)))
m_recall_gen <- glm(pred_true ~ model_family + log10_model_size + model_generation + note_length + duration_since_release,
                     data = recall_data2, family = binomial)
cat("VIF with model_generation added (GVIF^(1/2Df) for the factor):\n")
print(vif(m_recall_gen))
