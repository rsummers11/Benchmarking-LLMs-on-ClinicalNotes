# Refits the published open-model recall/precision GLMs with `multimodal` added as a covariate,
# to check whether it's a significant predictor and whether the other coefficients shift once
# it's included. Closed models are all multimodal, so this only applies to the open-weights GLMs.

.libPaths(c("your_path_here/.Rlib",
            .libPaths()))
suppressMessages(library(tidyverse))
suppressMessages(library(car))

final_data_dir <- "your_path_here/final_data"
load(file.path(final_data_dir, "DAT.RData"))
DAT <- DAT %>% filter(GPT.o3.mini_binary != "")  # matches analysis_report_v2.Rmd line 97

open_models <- DAT %>% filter(open_source == "yes") %>%
  mutate(model_family = factor(model_family),
         multimodal = factor(multimodal, levels = c("no", "yes")),
         log10_model_size = log10(as.numeric(as.character(model_size))))

cat("=== VL vs text-only model/row counts (open-weights) ===\n")
print(open_models %>% distinct(model, model_family, multimodal) %>% count(model_family, multimodal))
print(open_models %>% count(multimodal))

recall_data <- open_models %>% filter(gt == "True") %>% mutate(GPT.o3.mini_binary = as.factor(GPT.o3.mini_binary))
precision_data <- open_models %>% filter(GPT.o3.mini_binary == "True") %>% mutate(gt = as.factor(gt))

cat("\n================ RECALL ================\n")
cat("--- published spec (no multimodal term) ---\n")
m_recall_pub <- glm(GPT.o3.mini_binary ~ model_family + log10_model_size + note_length + duration_since_release,
                     data = recall_data, family = binomial)
print(summary(m_recall_pub)$coefficients)

cat("\n--- with multimodal covariate added ---\n")
m_recall_mm <- glm(GPT.o3.mini_binary ~ model_family + log10_model_size + multimodal + note_length + duration_since_release,
                    data = recall_data, family = binomial)
print(summary(m_recall_mm)$coefficients)
cat("\nVIF (with multimodal added):\n")
print(vif(m_recall_mm))

cat("\n--- coefficient shift: published vs. multimodal-adjusted (common terms) ---\n")
common <- intersect(names(coef(m_recall_pub)), names(coef(m_recall_mm)))
print(data.frame(term = common,
                  published = coef(m_recall_pub)[common],
                  with_multimodal = coef(m_recall_mm)[common],
                  abs_diff = abs(coef(m_recall_pub)[common] - coef(m_recall_mm)[common])))

cat("\nLikelihood ratio test (does adding multimodal significantly improve fit?):\n")
print(anova(m_recall_pub, m_recall_mm, test = "Chisq"))

cat("\n\n================ PRECISION ================\n")
cat("--- published spec (no multimodal term) ---\n")
m_prec_pub <- glm(gt ~ model_family + log10_model_size + note_length + duration_since_release,
                   data = precision_data, family = binomial)
print(summary(m_prec_pub)$coefficients)

cat("\n--- with multimodal covariate added ---\n")
m_prec_mm <- glm(gt ~ model_family + log10_model_size + multimodal + note_length + duration_since_release,
                  data = precision_data, family = binomial)
print(summary(m_prec_mm)$coefficients)
cat("\nVIF (with multimodal added):\n")
print(vif(m_prec_mm))

cat("\n--- coefficient shift: published vs. multimodal-adjusted (common terms) ---\n")
common <- intersect(names(coef(m_prec_pub)), names(coef(m_prec_mm)))
print(data.frame(term = common,
                  published = coef(m_prec_pub)[common],
                  with_multimodal = coef(m_prec_mm)[common],
                  abs_diff = abs(coef(m_prec_pub)[common] - coef(m_prec_mm)[common])))

cat("\nLikelihood ratio test (does adding multimodal significantly improve fit?):\n")
print(anova(m_prec_pub, m_prec_mm, test = "Chisq"))

cat("\n\n================ Closed models: multimodal composition ================\n")
closed_models <- DAT %>% filter(open_source == "no")
print(closed_models %>% distinct(model, multimodal))
