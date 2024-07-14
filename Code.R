#Install_Package
install.packages("ggplot2")
install.packages("devtools")
install.packages("dplyr")
install.packages("lme4)")
if (!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
pacman::p_load(knitr, tidyverse, psych, irr, readxl, BayesFactor, haven, latex2exp)

#library
library(ggplot2)
library(dplyr)
library(validateHOT)
library(pacman)
library(data.table)
library(psych)
library(readr)
library(lme4)
library(tidyverse)

#Read_File
CBC_HOT1 <-read.csv2("Desktop/Analysis_Thesis/Run/CBC_Holdout_Task1.csv")
CBC_HOT2 <- read.csv2("Desktop/Analysis_Thesis/Run/CBC_Holdout_Task2.csv")
CBC_HOT3 <-read.csv2("Desktop/Analysis_Thesis/Run/CBC_Holdout_Task3.csv")
CBCT_HOT1<-read.csv2("Desktop/Analysis_Thesis/Run/Tournament_Holdout_Task1.csv")
CBCT_HOT2 <-read.csv2("Desktop/Analysis_Thesis/Run/Tournament_Holdout_Task2.csv")
CBCT_HOT3 <- read.csv2("Desktop/Analysis_Thesis/Run/Tournament_Holdout_Task3 (edited).csv")
HOT4_Rank <- read_csv2("Desktop/Analysis_Thesis/Run/HOT4_CBC_CBT.csv")
Mixed_effect <- read_csv2("Desktop/Analysis_Thesis/Run/Mixed_Effect_data.csv")


#Rename
CBC_HOT1 <- CBC_HOT1 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

CBC_HOT2 <- CBC_HOT2 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

CBC_HOT3 <- CBC_HOT3 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

CBCT_HOT1 <- CBCT_HOT1 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

CBCT_HOT2 <- CBCT_HOT2 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

CBCT_HOT3 <- CBCT_HOT3 %>% 
  select(-Predictive_Choice) %>% 
  rename("Choice" = ncol(.)) %>% 
  rename_all(., function(x) c("ID", paste0("Option_", c(1:(ncol(.) - 2))), "Choice"))

Validate_a_single_HOT <- function(dataset) {
  if (!"pacman" %in% rownames(installed.packages())) install.packages("pacman")
  
  library(pacman)
  
  pacman::p_load(tidyverse, tidyr, dplyr, tidyselect, psych, irr, readxl, BayesFactor)
  
  
  d <- as.data.frame(dataset)
  
  d$Predicted_Choice <- 0
  
  for (i in 1:nrow(d)) {
    for (k in 1:(ncol(d) - 3)) {
      if (d[i, (k + 1)] == max(d[i, 2:(ncol(d) - 2)])) {
        d$Predicted_Choice[i] <- k
      }
    }
  }
  
  
  d_stacked <- d %>%
    pivot_longer(cols = names(d)[2]:names(d)[(ncol(d) - 2)], names_to = "Option", values_to = "Predicted_Share")
  
  d_stacked$temp <- rep(1:(ncol(d) - 3), nrow(d))
  d_stacked$Choice <- ifelse(d_stacked$Choice != d_stacked$temp, 0, 1)
  d_stacked$Predicted_Choice <- ifelse(d_stacked$Predicted_Choice != d_stacked$temp, 0, 1)
  d_stacked$temp <- NULL
  
  ## hintrate
  
  hitrate <- nrow(d_stacked[d_stacked$Choice == 1 & d_stacked$Predicted_Choice == 1, ]) / nrow(d)
  
  ## binomial test for hitrate
  
  undirected_binomial_test_chance_level <- binom.test(nrow(d_stacked[d_stacked$Choice == 1 & d_stacked$Predicted_Choice == 1, ]),
                                                      nrow(d),
                                                      alternative = "greater",
                                                      p = (1 / (ncol(d) - 3)),
                                                      conf.level = .95
  )
  
  ## Bayes Factor for hitrate against chance level
  
  BF <- proportionBF(
    nrow(d_stacked[d_stacked$Choice == 1 & d_stacked$Predicted_Choice == 1, ]),
    nrow(d),
    (1 / (ncol(d) - 3))
  )
  
  BF <- extractBF(BF)
  
  
  ## MAE, MedAE, and RMSE
  
  
  observed_share <- numeric(ncol(d) - 3)
  predicted_share <- numeric(ncol(d) - 3)
  
  
  for (i in 1:(ncol(d) - 3)) {
    observed_share[i] <- nrow(d[d$Choice == i, ]) / (nrow(d))
    predicted_share[i] <- mean(d[, (i + 1)]) / 100
  }
  
  
  
  
  Error_Matrix <- as.data.frame(cbind(observed_share, predicted_share))
  names(Error_Matrix) <- c("observed_share", "predicted_share")
  Error_Matrix$Absolute_Error <- abs(Error_Matrix$observed_share - Error_Matrix$predicted_share)
  Error_Matrix$Squared_Error <- (Error_Matrix$observed_share - Error_Matrix$predicted_share) * (Error_Matrix$observed_share - Error_Matrix$predicted_share)
  
  MAE <- mean(Error_Matrix$Absolute_Error)
  
  MedAE <- median(Error_Matrix$Absolute_Error)
  
  RMSE <- sqrt(mean(Error_Matrix$Squared_Error))
  
  ## Visualization
  
  
  Error_Matrix_long <- Error_Matrix[,1:2] %>%
    mutate(choice_option = names(d)[2:(ncol(d)-2)]) %>%
    pivot_longer(
      cols = 1:2,
      names_to = "Predicted_vs_Observed",
      values_to = "Shares"
    ) 
  
  
  Share_plot <- ggplot(Error_Matrix_long, aes(fill = Predicted_vs_Observed, y = Shares, x = choice_option)) +
    geom_bar(width = 0.7, position = position_dodge(width = 0.8), stat = "identity") +
    geom_hline(yintercept = 0) +
    theme_classic() +
    ylab("Observed and prediected choice shares of options in HOT") +
    theme(
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.line = element_blank(),
      axis.title.y = element_text(size = 11),
      axis.title.x = element_blank(),
      legend.position = "bottom",
    ) +
    geom_text(
      aes(label = format(round(Shares, 3), nsmall = 3)),
      position = position_dodge(width = 0.8),
      size = 4.5,
      vjust = -0.48
    ) +
    scale_y_continuous(limits = c(0, 1))
  
  
  ## Cohen's kappa
  
  kappa <- cohen.kappa(x = select(d, Choice, Predicted_Choice), alpha = .05)
  
  kappa2 <- kappa2(ratings = select(d, Choice, Predicted_Choice))
  
  ## Mean hit probability
  
  MHP <- mean(d_stacked[d_stacked$Choice == 1, ]$Predicted_Share)
  
  Single_HOT_Validation_Results <- list(
    Alternatives_In_HOT = length(predicted_share),
    Chance_Level_correct_Prediction_Percent = 1 / length(predicted_share) * 100,
    Hitrate = hitrate * 100,
    binomial_test_Hitrate_greater_chance_p_Value = undirected_binomial_test_chance_level$p.value,
    Bayes_Factor_hitrate_unequal_chance = BF$bf,
    MAE = MAE * 100,
    MedAE = MedAE * 100,
    RMSE = RMSE * 100,
    Plot = Share_plot,
    Cohens_Kappa_with_CIs = kappa$confid[1, ],
    z_test_Cohens_Kappa_test_statistic = kappa2$statistic,
    z_test_Cohens_Kappa_p_value = kappa2$p.value,
    MHP = MHP
  )
  
  return(Single_HOT_Validation_Results)
}
Validate_a_single_HOT(CBC_HOT1)
Validate_a_single_HOT(CBC_HOT2)
Validate_a_single_HOT(CBC_HOT3)
Validate_a_single_HOT(CBCT_HOT1)
Validate_a_single_HOT(CBCT_HOT2)
Validate_a_single_HOT(CBCT_HOT3)


#spearman_correlation

spearman_correlation_CBC <- cor(HOT4_Rank$Predictive_Choice_Rank_CBC, HOT4_Rank$Actual_Choice_Rank, method = "spearman")
spearman_correlation_CBT <- cor(HOT4_Rank$Predictive_Choice_Rank_CBT, HOT4_Rank$Actual_Choice_Rank, method = "spearman")
# Print the reslt
print(spearman_correlation_CBC)
print(spearman_correlation_CBT)

#Mixed_Effect
model <- glmer(HIT ~ Condition + (1 | ID) + (1 | HOT), family = binomial, data = Mixed_effect)

summary(model)



