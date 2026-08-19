library(lme4)
library(emmeans)
library(ggplot2)
library(knitr)
library(dplyr)
library(broom)
library(doParallel)
library(foreach)
# `projroot` is set as a global before this file is source()'d (see analysis_report_v2.Rmd)
source(file.path(projroot, "bootstrap_f1_helper.R"))

##############################################
##############Function 1 #####################
##############################################
recall_precision_closed=function(rdata,rttl,pdata,pttl,f1ttl,full_data)
{
  data=rdata;
  
  #RECALL
  
  recall_model_fixed <- glm(
    as.factor(GPT.o3.mini_binary) ~  model_family +
      note_length + duration_since_release,
    data = data,
    family = binomial
  )
  print(summary(recall_model_fixed))
  coef_table <- tidy(recall_model_fixed)
  write.csv(coef_table, "model_coefficients_closed_models_recall.csv")
  
  # emmeans for model_family and duration_since_release
  recall_emm_fixed <- emmeans(recall_model_fixed, 
                              ~ model_family + duration_since_release, 
                              type = "response")
  recall_emm_duration_df=data.frame(recall_emm_fixed)
    print(
      knitr::kable(recall_emm_duration_df, caption = "Table 1: Recall by Model Family (Closed Models)")
    )
 write.csv(recall_emm_duration_df,
           file.path(projroot,"Table_1_Recall_by_Model_Family(Closed Models).csv"),
           row.names = FALSE)
# Plot
  #I. Plot Recall by family
  #print(ggplot(recall_emm_duration_df, aes(x = model_family, y = response, color = model_family)) +
  print(ggplot(recall_emm_duration_df, aes(x = model_family, y = response)) +
          geom_point(position = position_dodge(width = 0.5), size = 3, color = "darkred") +
          geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.5), width = 0.2, color = "darkred") +
          labs(title = paste("Recall by Model Family\n",rttl),
               x = "Model Family", y = "Recall (Probability)") +
          theme_minimal())
  
  
  #II. Plot recall by duration_since_release on the x-axis
  # Predict probabilities using the model for a sequence of duration_since_release values
  duration_values <- seq(min(data$duration_since_release), max(data$duration_since_release), 
                         length.out = 100)
  pred_data_gpt <- data.frame(duration_since_release = duration_values,
                          model_family = "GPT",  # Replace with an appropriate family if needed
                          note_length = mean(data$note_length))  # Use a representative value for other covariates
  # Use a representative value for other covariates
  pred_data_claude <- data.frame(duration_since_release = duration_values,
                              model_family = "Claude",  # Replace with an appropriate family if needed
                              note_length = mean(data$note_length)) 
  pred_data <- rbind(pred_data_gpt, pred_data_claude)
  
  # Get predicted probabilities and standard errors for the specified durations
  pred_probs <- predict(recall_model_fixed, newdata = pred_data, type = "response", 
                        se.fit = TRUE)
  
  # Combine predictions with their corresponding durations
  pred_data$prob <- pred_probs$fit
  pred_data$se <- pred_probs$se.fit
  
  # Calculate the asymptotic confidence intervals
  pred_data$asymp.LCL <- pred_data$prob - 1.96 * pred_data$se
  pred_data$asymp.UCL <- pred_data$prob + 1.96 * pred_data$se
  recall_pred_data=pred_data
  

print(
  kable(pred_data, 
          caption = paste("Table 2: Recall by Release Duration (Closed Models)\n","Mean=",
                          mean(pred_data$duration_since_release)) #%>%
    #  kable_styling("striped", full_width = FALSE) %>%
    #  row_spec(row.idx, background = "yellow")
  ))
write.csv(pred_data,
          file.path(projroot,"Table_2_Recall_by_Release_Duration(Closed Models).csv"),
          row.names = FALSE)
  # Plot recall probabilities vs. duration_since_release with confidence intervals
  print(ggplot(pred_data, aes(x = duration_since_release, y = prob)) +
          geom_line(size = 1, color='forestgreen') +  # Line plot for recall probabilities
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2,fill='forestgreen') +  # Confidence interval band
          labs(title = paste("Recall vs. Duration Since Release\n",rttl),
               x = "Duration Since Release (Days)", y = "Recall (Probability)") +
          theme_minimal())
  
  #PRECISION
  
  data=pdata
  
  # Fit precision model: how often ground truth is positive when model says positive
  precision_model_fixed <- glm(
    as.factor(gt) ~ model_family + note_length + duration_since_release,
    data = data,
    family = binomial
  )
  print(summary(precision_model_fixed))
  coef_table <- tidy(precision_model_fixed)
  write.csv(coef_table, "model_coefficients_closed_models_precision.csv")
  
  # emmeans for model_family
  precision_emm_fixed <- emmeans(precision_model_fixed, 
                                 ~ model_family + duration_since_release, 
                                 type = "response")
  precision_emm_duration_df <- data.frame(precision_emm_fixed)
  print(
    knitr::kable(precision_emm_duration_df, caption = "Table 3: Precision by Model Family (Closed Models)")
  )
  
  write.csv(precision_emm_duration_df,
            file.path(projroot,"Table_3_Precision_by_Model_Family(Closed Models).csv"),
            row.names = FALSE)
  #I. Plot precision by model family
  print(ggplot(precision_emm_duration_df, aes(x = model_family, y = response)) +
          geom_point(position = position_dodge(width = 0.5), size = 3,, color = "darkred") +
          geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), position = position_dodge(width = 0.5), width = 0.2, color = "darkred") +
          labs(title = paste("Precision by Model Family\n", pttl),
               x = "Model Family", y = "Precision (Probability)") +
          theme_minimal())
  
  #II. Plot precision over time (duration_since_release)
  duration_values <- seq(min(data$duration_since_release), max(data$duration_since_release), 
                         length.out = 100)
  pred_data_gpt <- data.frame(duration_since_release = duration_values,
                          model_family = "GPT",  # Adjust if needed
                          note_length = mean(data$note_length))
  pred_data_claude <- data.frame(duration_since_release = duration_values,
                              model_family = "Claude",  # Adjust if needed
                              note_length = mean(data$note_length))
  pred_data <- rbind(pred_data_gpt, pred_data_claude)
  # Predict precision probabilities and SE
  pred_probs <- predict(precision_model_fixed, newdata = pred_data, type = "response", se.fit = TRUE)
  
  # Combine into prediction dataframe
  pred_data$prob <- pred_probs$fit
  pred_data$se <- pred_probs$se.fit
  pred_data$asymp.LCL <- pred_data$prob - 1.96 * pred_data$se
  pred_data$asymp.UCL <- pred_data$prob + 1.96 * pred_data$se
  
  row.idx=which(pred_data$duration_since_release == mean(pred_data$duration_since_release))
  
  print(
    kable(pred_data, 
          caption = paste("Table 4: Precision by Release Duration (Closed Models)\n","Mean=",
                          mean(pred_data$duration_since_release)) #%>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(pred_data,
            file.path(projroot,"Table_4_Precision_by_Release_Duration(Closed Models).csv"),
            row.names = FALSE)
  precision_pred_data=pred_data
  
  
  # Line plot for precision vs duration
  print(ggplot(pred_data, aes(x = duration_since_release, y = prob)) +
          geom_line(size = 1, color = "forestgreen") +
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2, fill = "forestgreen") +
          labs(title = paste("Precision vs. Duration Since Release\n", pttl),
               x = "Duration Since Release (Days)", y = "Precision (Probability)") +
          theme_minimal())
  
  ######### F1 SCORE ##############
  # F1 point estimate: combine the recall/precision emmeans (unchanged). F1 confidence interval:
  # nonparametric bootstrap (resample notes, refit both GLMs, re-predict at this same reference
  # grid) instead of the delta method -- see bootstrap_f1_helper.R for why.

  f1_emm_family <- inner_join(
    recall_emm_duration_df %>% select(model_family, response_recall = response, asymp.LCL_recall = asymp.LCL, asymp.UCL_recall = asymp.UCL),
    precision_emm_duration_df %>% select(model_family, response_precision = response, asymp.LCL_precision = asymp.LCL, asymp.UCL_precision = asymp.UCL),
    by = "model_family"
  )

  f1_emm_family <- f1_emm_family %>%
    mutate(
      precision = response_precision,
      recall = response_recall,
      F1 = 2 * (precision * recall) / (precision + recall)
    )

  # emmeans' reference grid for `~ model_family + duration_since_release` holds duration_since_release
  # and note_length at the mean of each model's OWN fitting data (rdata for recall, pdata for
  # precision) -- reproduce that explicitly so predict() below matches the emmeans point estimate.
  recall_grid_family <- data.frame(
    model_family = f1_emm_family$model_family,
    note_length = mean(rdata$note_length),
    duration_since_release = mean(rdata$duration_since_release)
  )
  precision_grid_family <- data.frame(
    model_family = f1_emm_family$model_family,
    note_length = mean(pdata$note_length),
    duration_since_release = mean(pdata$duration_since_release)
  )
  f1_family_ci <- bootstrap_f1_ci(
    full_data, recall_model_fixed, precision_model_fixed,
    recall_filter = function(d) d %>% filter(gt == "True"),
    precision_filter = function(d) d %>% filter(GPT.o3.mini_binary == "True"),
    recall_grid = recall_grid_family, precision_grid = precision_grid_family
  )
  f1_emm_family$F1_LCL <- f1_family_ci$F1_LCL
  f1_emm_family$F1_UCL <- f1_family_ci$F1_UCL
  f1_emm_family$se_F1 <- f1_family_ci$F1_SE

  print(
    knitr::kable(f1_emm_family, caption = "Table 5: F1 by Model Family (Closed Models)")
  )
  write.csv(f1_emm_family,
            file.path(projroot,"Table_5_F1_Score_by_Model_Family(Closed Models).csv"),
            row.names = FALSE)
  
  #I. Plot F1 by Model Family
  print(ggplot(f1_emm_family, aes(x = model_family, y = F1, color = model_family)) +
          geom_point(position = position_dodge(width = 0.5), size = 3, color = "darkred") +
          geom_errorbar(aes(ymin = F1_LCL, ymax = F1_UCL), position = position_dodge(width = 0.5), width = 0.2, color = "darkred") +
          labs(title = paste("F1 Score by Model Family\n", f1ttl),
               x = "Model Family", y = "F1 Score") +
          theme_minimal())
  
  #II. Plot F1 by duration
  
  # --- Rename and prepare inputs ---
  recall_df <- recall_pred_data %>% 
    rename(recall = prob, se_recall = se) %>%
    select(duration_since_release, model_family, recall, se_recall)
  
  precision_df <- precision_pred_data %>% 
    rename(precision = prob, se_precision = se) %>%
    select(duration_since_release, model_family, precision, se_precision)
  
  # --- Merge recall and precision ---
  f1_df <- left_join(recall_df, precision_df, by = c("duration_since_release", "model_family"))
  
  # --- F1 score (unchanged); F1 confidence interval via bootstrap instead of the delta method ---
  f1_df <- f1_df %>% mutate(F1 = 2 * (precision * recall) / (precision + recall))

  # recall_pred_data / precision_pred_data are the SAME grids already used for the point estimate
  # (note_length held at rdata's / pdata's own mean respectively) -- reuse them directly.
  f1_duration_ci <- bootstrap_f1_ci(
    full_data, recall_model_fixed, precision_model_fixed,
    recall_filter = function(d) d %>% filter(gt == "True"),
    precision_filter = function(d) d %>% filter(GPT.o3.mini_binary == "True"),
    recall_grid = recall_pred_data %>% select(model_family, note_length, duration_since_release),
    precision_grid = precision_pred_data %>% select(model_family, note_length, duration_since_release)
  )
  f1_df$F1_LCL <- f1_duration_ci$F1_LCL
  f1_df$F1_UCL <- f1_duration_ci$F1_UCL
  f1_df$se_F1 <- f1_duration_ci$F1_SE

  row.idx=which(f1_df$duration_since_release == mean(f1_df$duration_since_release))
  
  print(
    kable(f1_df, 
          caption = paste("Table 6: F1 by Release Duration (Closed Models)\n","Mean=",
                          mean(f1_df$duration_since_release)) #%>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(f1_df,
            file.path(projroot,"Table_6_F1_Score_by_Release_Duration(Closed Models).csv"),
            row.names = FALSE)
  
  # --- Plot F1 Score over Duration ---
  print(ggplot(f1_df, aes(x = duration_since_release, y = F1)) +
          geom_line(color = "forestgreen", size = 1) +
          geom_ribbon(aes(ymin = F1_LCL, ymax = F1_UCL), fill = "forestgreen", alpha = 0.2) +
          labs(
            title = paste("F1 Score vs. Duration Since Release\n", f1ttl),
            x = "Duration Since Release (Days)",
            y = "F1 Score"
          ) +
          theme_minimal())
  
  # Combine all into one dataframe
  comparison_df <- f1_df %>%
    select(duration_since_release, F1, F1_LCL, F1_UCL) %>%
    rename(value = F1, LCL = F1_LCL, UCL = F1_UCL) %>%
    mutate(metric = "F1 Score") %>%
    bind_rows(
      recall_df %>%
        mutate(value = recall,
               LCL = recall - 1.96 * se_recall,
               UCL = recall + 1.96 * se_recall,
               metric = "Recall") %>%
        select(duration_since_release, value, LCL, UCL, metric),
      precision_df %>%
        mutate(value = precision,
               LCL = precision - 1.96 * se_precision,
               UCL = precision + 1.96 * se_precision,
               metric = "Precision") %>%
        select(duration_since_release, value, LCL, UCL, metric)
    )
  
  # Plot
  ggplot(comparison_df, aes(x = duration_since_release, y = value, color = metric, fill = metric)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.15, color = NA) +
    labs(title = "Precision, Recall, and F1 Score vs. Duration Since Release",
         x = "Duration Since Release (Days)", y = "Score (Probability)") +
    theme_minimal() +
    scale_color_manual(values = c("Precision" = "darkred", "Recall" = "mediumblue", "F1 Score" = "forestgreen")) +
    scale_fill_manual(values = c("Precision" = "darkred", "Recall" = "mediumblue", "F1 Score" = "forestgreen"))
  
} #end function

##############################################
##############Function 2 #####################
##############################################
recall_precision_open=function(rdata,rttl,pdata,pttl,f1ttl,full_data)
{
  data=rdata;
  data$GPT.o3.mini_binary=as.factor(data$GPT.o3.mini_binary)
  data$model_family=as.factor(as.character(data$model_family))
  #data$model_size=as.factor(data$model_size)
  data$log10_model_size <- log10(as.numeric(as.character(data$model_size)))
  data$generation=as.factor(data$model_generation)
  
  #RECALL
  
  recall_model_fixed <- glm(
    GPT.o3.mini_binary ~  model_family + log10_model_size + #model_generation +
      note_length + duration_since_release,
    data = data,
    family = binomial
  )
  print(summary(recall_model_fixed))
  coef_table <- tidy(recall_model_fixed)
  write.csv(coef_table, "model_coefficients_open_models_recall.csv")
  
  # I. Plot 1: Recall vs. Model_Family ---
  # Create prediction grid by model_family
  pred_data_family <- expand.grid(
    model_family = levels(data$model_family),
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE)
  )
  
  pred_family <- predict(recall_model_fixed, newdata = pred_data_family, type = "link", se.fit = TRUE)
  pred_data_family$prob <- plogis(pred_family$fit)
  pred_data_family$asymp.LCL <- plogis(pred_family$fit - 1.96 * pred_family$se.fit)
  pred_data_family$asymp.UCL <- plogis(pred_family$fit + 1.96 * pred_family$se.fit)
  
  print(
    knitr::kable(pred_data_family, caption = "Table 7: Recall by Model Family (Open Models)")
  )
  write.csv(pred_data_family,
            file.path(projroot,"Table_7_Recall_by_Model_Family(Open Models).csv"),
            row.names = FALSE)

  ggplot(pred_data_family, aes(x = model_family, y = prob)) +
    geom_point(size = 3, color = "darkred") +
    geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "darkred") +
    labs(title = "Recall vs. Model Family",
         x = "Model Family", y = "Recall (Probability)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  # II. Plot 2: Recall vs. Duration Since Release ---
  
  # Create prediction grid for duration
  pred_data_duration_qwen <- data.frame(
    model_family = "Qwen",  # Or any one family to condition on
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = seq(min(data$duration_since_release), max(data$duration_since_release), length.out = 100)
  )
  pred_data_duration_llama <- data.frame(
    model_family = "Llama",  # Or any one family to condition on
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = seq(min(data$duration_since_release), max(data$duration_since_release), length.out = 100)
  )
  pred_data_duration <- rbind(pred_data_duration_qwen, pred_data_duration_llama)
  # Predict with CI
  pred_duration <- predict(recall_model_fixed, newdata = pred_data_duration, type = "link", se.fit = TRUE)
  pred_data_duration$prob <- plogis(pred_duration$fit)
  pred_data_duration$asymp.LCL <- plogis(pred_duration$fit - 1.96 * pred_duration$se.fit)
  pred_data_duration$asymp.UCL <- plogis(pred_duration$fit + 1.96 * pred_duration$se.fit)
  
  row.idx=which(pred_data_duration$duration_since_release == mean(pred_data_duration$duration_since_release))
  
  print(
    kable(pred_data_duration, 
          caption = paste("Table 8: Recall by Release Duration (Open Models)\n","Mean=",
                          mean(pred_data_duration$duration_since_release)) # %>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(pred_data_duration,
            file.path(projroot,"Table_8_Recall_by_Release_Duration(Open Models).csv"),
            row.names = FALSE)
  
  # Plot
  print(ggplot(pred_data_duration, aes(x = duration_since_release, y = prob)) +
          geom_line(size = 1, color = "forestgreen") +
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = "forestgreen", alpha = 0.2) +
          labs(title = paste("Recall vs. Duration Since Release\n", rttl),
               x = "Duration Since Release (Days)", y = "Recall (Probability)") +
          theme_minimal())
  
  # III. Plot 3: Recall vs. log10(Model Size) ---
  
  # Create prediction grid for log10_model_size
  pred_data_size_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = seq(min(data$log10_model_size), 
                           max(data$log10_model_size), length.out = 100),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE))
  pred_data_size_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = seq(min(data$log10_model_size), 
                           max(data$log10_model_size), length.out = 100),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE))

  pred_data_size <- rbind(pred_data_size_llama, pred_data_size_qwen)
  # Predict with CI
  pred_size <- predict(recall_model_fixed, newdata = pred_data_size, type = "link", se.fit = TRUE)
  pred_data_size$prob <- plogis(pred_size$fit)
  pred_data_size$asymp.LCL <- plogis(pred_size$fit - 1.96 * pred_size$se.fit)
  pred_data_size$asymp.UCL <- plogis(pred_size$fit + 1.96 * pred_size$se.fit)
  
  row.idx=which(pred_data_size$log10_model_size == mean(pred_data_size$log10_model_size))
  
  print(
    kable(pred_data_size, 
          caption = paste("Table 9: Recall by log10(Model Size) (Open Models)\n","Mean=",
                          mean(pred_data_size$log10_model_size)) # %>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))    
  write.csv(pred_data_size,
            file.path(projroot,"Table_9_Recall_by_log10_Model_Size(Open Models).csv"),
            row.names = FALSE)
  
  note_lengths <- seq(min(data$note_length), max(data$note_length), length.out = 100)
  pred_data_notelength_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = note_lengths,
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE))
  pred_data_notelength_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = note_lengths,
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE))
  
  pred_data_notelength <- rbind(pred_data_notelength_llama, pred_data_notelength_qwen)
  
  pred_notelength <- predict(recall_model_fixed, newdata = pred_data_notelength, type = "link", se.fit = TRUE)
  pred_data_notelength$prob <- plogis(pred_notelength$fit)
  pred_data_notelength$asymp.LCL <- plogis(pred_notelength$fit - 1.96 * pred_notelength$se.fit)
  pred_data_notelength$asymp.UCL <- plogis(pred_notelength$fit + 1.96 * pred_notelength$se.fit)
  

  print(
    kable(pred_data_notelength, 
          caption = paste("Table 18: Recall by note_length (Open Models)\n","Mean=",
                          mean(pred_data_notelength$note_length)) # %>%
          #      kable_styling("striped", full_width = FALSE) %>%
          #      row_spec(row.idx, background = "yellow")
    ))    
  write.csv(pred_data_notelength,
            file.path(projroot,"Table_18_Recall_by_notelength(Open Models).csv"),
            row.names = FALSE)
  
  # Plot
  print(ggplot(pred_data_size, aes(x = log10_model_size, y = prob)) +
          geom_line(size = 1, color = "steelblue") +
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = "steelblue", alpha = 0.2) +
          labs(title = paste("Recall vs. Log10(Model Size)\n", rttl),
               x = "Log10(Model Size in Billions)", y = "Recall (Probability)") +
          theme_minimal())
  
  
  # Plot recall probabilities vs. duration_since_release with confidence intervals
  print(ggplot(pred_data_duration, aes(x = duration_since_release, y = prob)) +
          geom_line(size = 1, color='forestgreen') +  # Line plot for recall probabilities
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2,fill='forestgreen') +  # Confidence interval band
          labs(title = paste("Recall vs. Duration Since Release\n",rttl),
               x = "Duration Since Release (Days)", y = "Recall (Probability)") +
          theme_minimal())
  
  
  
  #PRECISION
  
  data=pdata
  
  data$gt=as.factor(data$gt)
  data$model_family=as.factor(as.character(data$model_family))
  #data$model_size=as.factor(data$model_size)
  data$log10_model_size <- log10(as.numeric(as.character(data$model_size)))
  data$generation=as.factor(data$model_generation)
  
  
  # Fit precision model: how often ground truth is positive when model says positive
  precision_model_fixed <- glm(
    gt ~ model_family + log10_model_size + note_length + duration_since_release,
    data = data,
    family = binomial
  )
  print(summary(precision_model_fixed))
  coef_table <- tidy(precision_model_fixed)
  write.csv(coef_table, "model_coefficients_open_models_precision.csv")
  #I. Plot precision by model_family
  
  pred_data_family <- expand.grid(
    model_family = levels(data$model_family),
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE)
  )
  
  pred_family <- predict(precision_model_fixed, newdata = pred_data_family, type = "link", se.fit = TRUE)
  pred_data_family$prob <- plogis(pred_family$fit)
  pred_data_family$asymp.LCL <- plogis(pred_family$fit - 1.96 * pred_family$se.fit)
  pred_data_family$asymp.UCL <- plogis(pred_family$fit + 1.96 * pred_family$se.fit)
  
  print(
    knitr::kable(pred_data_family, caption = "Table 10: Precision by Model Family (Open Models)")
  )
  write.csv(pred_data_family,
            file.path(projroot,"Table_10_Precision_by_Model_Family(Open Models).csv"),
            row.names = FALSE)
  
  print(ggplot(pred_data_family, aes(x = model_family, y = prob)) +
          geom_point(size = 3, color = "darkred") +
          geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2, color = "darkred") +
          labs(title = paste("Precision vs. Model Family\n",pttl),
               x = "Model Family", y = "Precision (Probability)") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  
  #II. Plot precision by days since release=duration
  
  # Create prediction grid for duration
  pred_data_duration_qwen <- data.frame(
    model_family = "Qwen",  # Pick one family
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = seq(min(data$duration_since_release), max(data$duration_since_release), length.out = 100)
  )
  pred_data_duration_llama <- data.frame(
    model_family = "Llama",  # Pick one family
    log10_model_size = mean(data$log10_model_size, na.rm = TRUE),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = seq(min(data$duration_since_release), max(data$duration_since_release), length.out = 100)
  )
  pred_data_duration <- rbind(pred_data_duration_llama, pred_data_duration_qwen)
  
  # Predict with CI on link scale
  pred_duration <- predict(precision_model_fixed, newdata = pred_data_duration, type = "link", se.fit = TRUE)
  pred_data_duration$prob <- plogis(pred_duration$fit)
  pred_data_duration$asymp.LCL <- plogis(pred_duration$fit - 1.96 * pred_duration$se.fit)
  pred_data_duration$asymp.UCL <- plogis(pred_duration$fit + 1.96 * pred_duration$se.fit)
  
  row.idx=which(pred_data_duration$duration_since_release == mean(pred_data_duration$duration_since_release))
  
  print(
    kable(pred_data_duration, 
          caption = paste("Table 11: Precision by Release Duration (Open Models)\n","Mean=",
          mean(pred_data_duration$duration_since_release)) #%>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(pred_data_duration,
            file.path(projroot,"Table_11_Precision_by_Release_Duration(Open Models).csv"),
            row.names = FALSE)
  
  # Plot
  print(ggplot(pred_data_duration, aes(x = duration_since_release, y = prob)) +
          geom_line(size = 1, color = "forestgreen") +
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = "forestgreen", alpha = 0.2) +
          labs(title = paste("Precision vs. Duration Since Release\n", pttl),
               x = "Duration Since Release (Days)", y = "Precision (Probability)") +
          theme_minimal())
  
  
  #III. Plot precision over log10 model size (in billions)
  # Create prediction grid for log10_model_size
  pred_data_size_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = seq(min(data$log10_model_size), max(data$log10_model_size), length.out = 100),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE)
  )
  pred_data_size_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = seq(min(data$log10_model_size), max(data$log10_model_size), length.out = 100),
    note_length = mean(data$note_length, na.rm = TRUE),
    duration_since_release = mean(data$duration_since_release, na.rm = TRUE)
  )
  pred_data_size <- rbind(pred_data_size_llama, pred_data_size_qwen)
  
  # Predict with CI on link scale
  pred_size <- predict(precision_model_fixed, newdata = pred_data_size, type = "link", se.fit = TRUE)
  pred_data_size$prob <- plogis(pred_size$fit)
  pred_data_size$asymp.LCL <- plogis(pred_size$fit - 1.96 * pred_size$se.fit)
  pred_data_size$asymp.UCL <- plogis(pred_size$fit + 1.96 * pred_size$se.fit)
  
  row.idx=which(pred_data_size$duration_since_release == mean(pred_data_size$duration_since_release))
  
  print(
    kable(pred_data_size, 
          caption = paste("Table 12: Precision by log10(Model Size) (Open Models)\n","Mean=",
                          mean(pred_data_size$log10_model_size)) #%>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(pred_data_size,
            file.path(projroot,"Table_12_Precision_by_log10_Model_Size(Open Models).csv"),
            row.names = FALSE)
  
  
  # Plot
  print(ggplot(pred_data_size, aes(x = log10_model_size, y = prob)) +
          geom_line(size = 1, color = "steelblue") +
          geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = "steelblue", alpha = 0.2) +
          labs(title = paste("Precision vs. Log10(Model Size)\n", pttl),
               x = "Log10(Model Size in Billions)", y = "Precision (Probability)") +
          theme_minimal())
  
  
  ######### F1 SCORE ##############
  # ----- Initial Setup -----
  rdata$log10_model_size <- log10(as.numeric(as.character(rdata$model_size)))
  pdata$log10_model_size <- log10(as.numeric(as.character(pdata$model_size)))
  
  
  # ----- From Fitted Recall Model -----
  
  # Recall predictions by model_family
  recall_pred_data_family <- expand.grid(
    model_family = unique(rdata$model_family),
    log10_model_size = mean(rdata$log10_model_size),
    note_length = mean(rdata$note_length),
    duration_since_release = mean(rdata$duration_since_release)
  )
  recall_pred <- predict(recall_model_fixed, newdata = recall_pred_data_family, type = "link", se.fit = TRUE)
  recall_pred_data_family$recall <- plogis(recall_pred$fit)
  recall_pred_data_family$se_recall <- recall_pred$se.fit
  
  
  # Recall predictions by duration
  recall_pred_data_duration_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = mean(rdata$log10_model_size),
    note_length = mean(rdata$note_length),
    duration_since_release = seq(min(rdata$duration_since_release), max(rdata$duration_since_release), length.out = 100)
  )
  recall_pred_data_duration_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = mean(rdata$log10_model_size),
    note_length = mean(rdata$note_length),
    duration_since_release = seq(min(rdata$duration_since_release), max(rdata$duration_since_release), length.out = 100)
  )
  recall_pred_data_duration <- rbind(recall_pred_data_duration_llama, recall_pred_data_duration_qwen)
  
  recall_pred <- predict(recall_model_fixed, newdata = recall_pred_data_duration, type = "link", se.fit = TRUE)
  recall_pred_data_duration$recall <- plogis(recall_pred$fit)
  recall_pred_data_duration$se_recall <- recall_pred$se.fit
  
  # Recall predictions by log10_model_size
  recall_pred_data_size_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = seq(min(rdata$log10_model_size), max(rdata$log10_model_size), length.out = 100),
    note_length = mean(rdata$note_length),
    duration_since_release = mean(rdata$duration_since_release)
  )
  recall_pred_data_size_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = seq(min(rdata$log10_model_size), max(rdata$log10_model_size), length.out = 100),
    note_length = mean(rdata$note_length),
    duration_since_release = mean(rdata$duration_since_release)
  )
  recall_pred_data_size <- rbind(recall_pred_data_size_llama, recall_pred_data_size_qwen)
  recall_pred <- predict(recall_model_fixed, newdata = recall_pred_data_size, type = "link", se.fit = TRUE)
  recall_pred_data_size$recall <- plogis(recall_pred$fit)
  recall_pred_data_size$se_recall <- recall_pred$se.fit
  
  # ----- From Fitted Precision Model -----
  
  # Precision predictions by model_family
  precision_pred_data_family <- expand.grid(
    model_family = unique(pdata$model_family),
    log10_model_size = mean(pdata$log10_model_size),
    note_length = mean(pdata$note_length),
    duration_since_release = mean(pdata$duration_since_release)
  )
  precision_pred <- predict(precision_model_fixed, newdata = precision_pred_data_family, type = "link", se.fit = TRUE)
  precision_pred_data_family$precision <- plogis(precision_pred$fit)
  precision_pred_data_family$se_precision <- precision_pred$se.fit
  
  # Precision predictions by duration
  precision_pred_data_duration_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = mean(pdata$log10_model_size),
    note_length = mean(pdata$note_length),
    duration_since_release = seq(min(pdata$duration_since_release), max(pdata$duration_since_release), length.out = 100)
  )
  precision_pred_data_duration_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = mean(pdata$log10_model_size),
    note_length = mean(pdata$note_length),
    duration_since_release = seq(min(pdata$duration_since_release), max(pdata$duration_since_release), length.out = 100)
  )
  precision_pred_data_duration <- rbind(precision_pred_data_duration_llama, precision_pred_data_duration_qwen)
  precision_pred <- predict(precision_model_fixed, newdata = precision_pred_data_duration, type = "link", se.fit = TRUE)
  precision_pred_data_duration$precision <- plogis(precision_pred$fit)
  precision_pred_data_duration$se_precision <- precision_pred$se.fit
  
  # Precision predictions by log10_model_size
  precision_pred_data_size_qwen <- data.frame(
    model_family = "Qwen",
    log10_model_size = seq(min(pdata$log10_model_size), max(pdata$log10_model_size), length.out = 100),
    note_length = mean(pdata$note_length),
    duration_since_release = mean(pdata$duration_since_release)
  )
  precision_pred_data_size_llama <- data.frame(
    model_family = "Llama",
    log10_model_size = seq(min(pdata$log10_model_size), max(pdata$log10_model_size), length.out = 100),
    note_length = mean(pdata$note_length),
    duration_since_release = mean(pdata$duration_since_release)
  )
  precision_pred_data_size <- rbind(precision_pred_data_size_llama, precision_pred_data_size_qwen)
  precision_pred <- predict(precision_model_fixed, newdata = precision_pred_data_size, type = "link", se.fit = TRUE)
  precision_pred_data_size$precision <- plogis(precision_pred$fit)
  precision_pred_data_size$se_precision <- precision_pred$se.fit
  
  # ----- Merge Recall + Precision -> F1 point estimate (unchanged); F1 CI via bootstrap -----
  # recall_pred_data_* / precision_pred_data_* are the SAME grids already used for the point
  # estimates (each holding the "other" covariates at ITS OWN model's fitting-subset mean --
  # recall model's grids at rdata's mean, precision model's at pdata's mean); reuse them directly
  # so the bootstrap can't drift from whatever reference convention was used above.

  # NOTE: merge() sorts its output by the join keys, so recall_grid/precision_grid must be built
  # from the already-merged f1_df_* (not the pre-merge recall/precision_pred_data_*), or the
  # bootstrap draws get assigned to the wrong rows.

  # Duration
  f1_df_duration <- merge(recall_pred_data_duration, precision_pred_data_duration, by = c("duration_since_release", "model_family"))
  f1_df_duration <- f1_df_duration %>% mutate(F1 = 2 * (precision * recall) / (precision + recall))
  ci_duration <- bootstrap_f1_ci(
    full_data, recall_model_fixed, precision_model_fixed,
    # open-model formulas use the bare column (GPT.o3.mini_binary / gt) as response, relying on
    # the as.factor() conversion the point-estimate fit applies beforehand -- replicate it here or
    # every bootstrap refit errors on a character-typed binomial response.
    recall_filter = function(d) d %>% filter(gt == "True") %>% mutate(GPT.o3.mini_binary = as.factor(GPT.o3.mini_binary)),
    precision_filter = function(d) d %>% filter(GPT.o3.mini_binary == "True") %>% mutate(gt = as.factor(gt)),
    recall_grid = f1_df_duration %>% select(model_family, duration_since_release, log10_model_size = log10_model_size.x, note_length = note_length.x),
    precision_grid = f1_df_duration %>% select(model_family, duration_since_release, log10_model_size = log10_model_size.y, note_length = note_length.y)
  )
  f1_df_duration$F1_LCL <- ci_duration$F1_LCL
  f1_df_duration$F1_UCL <- ci_duration$F1_UCL
  f1_df_duration$se_F1 <- ci_duration$F1_SE

  # Model family
  f1_df_family <- merge(recall_pred_data_family, precision_pred_data_family, by = "model_family")
  f1_df_family <- f1_df_family %>% mutate(F1 = 2 * (precision * recall) / (precision + recall))
  ci_family <- bootstrap_f1_ci(
    full_data, recall_model_fixed, precision_model_fixed,
    recall_filter = function(d) d %>% filter(gt == "True") %>% mutate(GPT.o3.mini_binary = as.factor(GPT.o3.mini_binary)),
    precision_filter = function(d) d %>% filter(GPT.o3.mini_binary == "True") %>% mutate(gt = as.factor(gt)),
    recall_grid = f1_df_family %>% select(model_family, log10_model_size = log10_model_size.x, note_length = note_length.x, duration_since_release = duration_since_release.x),
    precision_grid = f1_df_family %>% select(model_family, log10_model_size = log10_model_size.y, note_length = note_length.y, duration_since_release = duration_since_release.y)
  )
  f1_df_family$F1_LCL <- ci_family$F1_LCL
  f1_df_family$F1_UCL <- ci_family$F1_UCL
  f1_df_family$se_F1 <- ci_family$F1_SE
  print(
    knitr::kable(f1_df_family, caption = "Table 13: F1 by Model Family (Open Models)")
  )
  write.csv(f1_df_family,
            file.path(projroot,"Table_13_F1_Score_by_Model_Family(Open Models).csv"),
            row.names = FALSE)

  # Model size
  f1_df_size <- merge(recall_pred_data_size, precision_pred_data_size, by = c("log10_model_size", "model_family"))
  f1_df_size <- f1_df_size %>% mutate(F1 = 2 * (precision * recall) / (precision + recall))
  ci_size <- bootstrap_f1_ci(
    full_data, recall_model_fixed, precision_model_fixed,
    recall_filter = function(d) d %>% filter(gt == "True") %>% mutate(GPT.o3.mini_binary = as.factor(GPT.o3.mini_binary)),
    precision_filter = function(d) d %>% filter(GPT.o3.mini_binary == "True") %>% mutate(gt = as.factor(gt)),
    recall_grid = f1_df_size %>% select(model_family, log10_model_size, note_length = note_length.x, duration_since_release = duration_since_release.x),
    precision_grid = f1_df_size %>% select(model_family, log10_model_size, note_length = note_length.y, duration_since_release = duration_since_release.y)
  )
  f1_df_size$F1_LCL <- ci_size$F1_LCL
  f1_df_size$F1_UCL <- ci_size$F1_UCL
  f1_df_size$se_F1 <- ci_size$F1_SE

  row.idx=which(f1_df_size$log10_model_size == mean(f1_df_size$log10_model_size))
  
  print(
    kable(f1_df_size, 
          caption = paste("Table 14: F1 by log10(Model Size) (Open Models)\n","Mean=",
                          mean(f1_df_size$log10_model_size)) # %>%
#      kable_styling("striped", full_width = FALSE) %>%
#      row_spec(row.idx, background = "yellow")
  ))
  write.csv(f1_df_size,
            file.path(projroot,"Table_14_F1_Score_by_log10_Model_Size(Open Models).csv"),
            row.names = FALSE)
  
  row.idx=which(f1_df_duration$duration_since_release == mean(f1_df_duration$duration_since_release))
  
  print(
    kable(f1_df_duration, 
          caption = paste("Table 15: F1 by Release Duration (Open Models)\n","Mean=",
                          mean(f1_df_duration$duration_since_release)) 
          #%>%
      #kable_styling("striped", full_width = FALSE) %>%
      #row_spec(row.idx, background = "yellow")
  ))
  write.csv(f1_df_duration,
            file.path(projroot,"Table_15_F1_Score_by_Release_Duration(Open Models).csv"),
            row.names = FALSE)
  
  # ----- Plots -----
  
  # Plot 1: F1 vs. Duration
  print(ggplot(f1_df_duration, aes(x = duration_since_release, y = F1)) +
          geom_line(color = "forestgreen", size = 1) +
          geom_ribbon(aes(ymin = F1_LCL, ymax = F1_UCL), fill = "forestgreen", alpha = 0.2) +
          labs(title = paste("F1 Score vs. Duration Since Release\n",f1ttl), x = "Duration (days)", y = "F1 Score") +
          theme_minimal())
  
  # Plot 2: F1 vs. Model Family
  print(ggplot(f1_df_family, aes(x = model_family, y = F1, color = model_family)) +
          geom_point(size = 3, color = "darkred") +
          geom_errorbar(aes(ymin = F1_LCL, ymax = F1_UCL), width = 0.2, color = "darkred") +
          labs(title = paste("F1 Score by Model Family\n",f1ttl), x = "Model Family", y = "F1 Score") +
          theme_minimal())
  
  # Plot 3: F1 vs. log10(Model Size)
  print(ggplot(f1_df_size, aes(x = log10_model_size, y = F1)) +
          geom_line(color = "steelblue", size = 1) +
          geom_ribbon(aes(ymin = F1_LCL, ymax = F1_UCL), fill = "steelblue", alpha = 0.2) +
          labs(title = paste("F1 Score vs. Log10(Model Size)\n",f1ttl), 
               x = "Log10(Model Size in Billions)", 
               y = "F1 Score") +   theme_minimal())
  
  # Combine for duration -- F1's LCL/UCL come from f1_df_duration's bootstrap CI (already
  # computed above), not a fresh delta-method se; recall/precision keep their own Wald SEs since
  # the delta-method instability is specific to the compound F1 metric.
  plot_data_duration <- data.frame(
    duration_since_release = recall_pred_data_duration$duration_since_release,
    model_family = recall_pred_data_duration$model_family,
    recall = recall_pred_data_duration$recall,
    se_recall = recall_pred_data_duration$se_recall,
    precision = precision_pred_data_duration$precision,
    se_precision = precision_pred_data_duration$se_precision
  )
  plot_data_duration <- plot_data_duration %>%
    inner_join(f1_df_duration %>% select(duration_since_release, model_family, F1, F1_LCL, F1_UCL),
               by = c("duration_since_release", "model_family"))

  long_duration <- rbind(
    data.frame(x = plot_data_duration$duration_since_release, y = plot_data_duration$recall, LCL = plot_data_duration$recall - 1.96 * plot_data_duration$se_recall, UCL = plot_data_duration$recall + 1.96 * plot_data_duration$se_recall, metric = "Recall"),
    data.frame(x = plot_data_duration$duration_since_release, y = plot_data_duration$precision, LCL = plot_data_duration$precision - 1.96 * plot_data_duration$se_precision, UCL = plot_data_duration$precision + 1.96 * plot_data_duration$se_precision, metric = "Precision"),
    data.frame(x = plot_data_duration$duration_since_release, y = plot_data_duration$F1, LCL = plot_data_duration$F1_LCL, UCL = plot_data_duration$F1_UCL, metric = "F1 Score")
  )

  # Combine for model size -- same treatment as duration above.
  plot_data_size <- data.frame(
    log10_model_size = recall_pred_data_size$log10_model_size,
    model_family = recall_pred_data_size$model_family,
    recall = recall_pred_data_size$recall,
    se_recall = recall_pred_data_size$se_recall,
    precision = precision_pred_data_size$precision,
    se_precision = precision_pred_data_size$se_precision
  )
  plot_data_size <- plot_data_size %>%
    inner_join(f1_df_size %>% select(log10_model_size, model_family, F1, F1_LCL, F1_UCL),
               by = c("log10_model_size", "model_family"))

  long_size <- rbind(
    data.frame(x = plot_data_size$log10_model_size, y = plot_data_size$recall, LCL = plot_data_size$recall - 1.96 * plot_data_size$se_recall, UCL = plot_data_size$recall + 1.96 * plot_data_size$se_recall, metric = "Recall"),
    data.frame(x = plot_data_size$log10_model_size, y = plot_data_size$precision, LCL = plot_data_size$precision - 1.96 * plot_data_size$se_precision, UCL = plot_data_size$precision + 1.96 * plot_data_size$se_precision, metric = "Precision"),
    data.frame(x = plot_data_size$log10_model_size, y = plot_data_size$F1, LCL = plot_data_size$F1_LCL, UCL = plot_data_size$F1_UCL, metric = "F1 Score")
  )
  
  # Plot all metrics vs duration
  print(ggplot(long_duration, aes(x = x, y = y, color = metric, fill = metric)) +
          geom_line(size = 1) +
          geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.2, color = NA) +
          labs(title = paste("Recall, Precision & F1 vs. Duration Since Release\n", f1ttl),
               x = "Duration Since Release (Days)", y = "Metric Value") +
          theme_minimal() +
          scale_color_manual(values = c("Recall" = "mediumblue", "Precision" = "darkred", "F1 Score" = "forestgreen")) +
          scale_fill_manual(values = c("Recall" = "mediumblue", "Precision" = "darkred", "F1 Score" = "forestgreen")))

  # Plot all metrics vs log10(model size)
  print(ggplot(long_size, aes(x = x, y = y, color = metric, fill = metric)) +
          geom_line(size = 1) +
          geom_ribbon(aes(ymin = LCL, ymax = UCL), alpha = 0.2, color = NA) +
          labs(title = paste("Recall, Precision & F1 vs. Log10(Model Size)\n", f1ttl),
               x = "Log10(Model Size in Billions)", y = "Metric Value") +
          theme_minimal() +
          scale_color_manual(values = c("Recall" = "mediumblue", "Precision" = "darkred", "F1 Score" = "forestgreen")) +
          scale_fill_manual(values = c("Recall" = "mediumblue", "Precision" = "darkred", "F1 Score" = "forestgreen")))
  
} #end function
