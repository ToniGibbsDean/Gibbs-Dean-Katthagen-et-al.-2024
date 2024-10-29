################################################################################################
# 1. Packages and path to project directory ######
################################################################################################
    set.seed(0.1)
  
    require(tidyverse)
    library(lme4)
    library(lmerTest)
    library(nlme)
    library(emmeans)
    #take scientific notation off
    options(scipen=999)
     library(sjPlot)

source("Code/03_SpTaskMetricsAnalysis/SPQData/01_LMEBasicMets.r")
source("Code/03_SpTaskMetricsAnalysis/SPQData/02_LMEModelling_LRs.r")
source("Code/04_ComputationalModelling/HGF_SPQData/04_LME_be1.r")
source("Code/04_ComputationalModelling/HGF_SPQData/04_LME_kaa.r")
source("Code/04_ComputationalModelling/HGF_SPQData/04_LME_kax.r")




p_values_Null_score <- summary(Null_score)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_score <- summary(Reduced_score)$coefficients[, "Pr(>|t|)"]
p_values_Full_score <- summary(Full_score)$coefficients[, "Pr(>|t|)"]

p_values_null_perfe <- summary(Null_perfe)$coefficients[, "Pr(>|t|)"]
p_values_full_perfe <- summary(Full_perfe)$coefficients[, "Pr(>|t|)"]
p_values_reduced_perfe <- summary(Reduced_perfe)$coefficients[, "Pr(>|t|)"]

p_values_Null_lr <- summary(Null_lr)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_lr <- summary(Reduced_lr)$coefficients[, "Pr(>|t|)"]
p_values_Full_lr <- summary(Full_lr)$coefficients[, "Pr(>|t|)"]

p_values_Null_conf <- summary(Null_conf)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_conf <- summary(Reduced_conf)$coefficients[, "Pr(>|t|)"]
p_values_Full_conf<- summary(Full_conf)$coefficients[, "Pr(>|t|)"]

p_values_Null_be1 <- summary(Null_be1)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_be1 <- summary(Reduced_be1)$coefficients[, "Pr(>|t|)"]
p_values_Full_be1<- summary(Full_be1)$coefficients[, "Pr(>|t|)"]

p_values_Null_kax <- summary(Null_kax)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_kax <- summary(Reduced_kax)$coefficients[, "Pr(>|t|)"]
p_values_Full_kax<- summary(Full_kax)$coefficients[, "Pr(>|t|)"]

p_values_Null_kaa <- summary(Null_kaa)$coefficients[, "Pr(>|t|)"]
p_values_Reduced_kaa <- summary(Reduced_kaa)$coefficients[, "Pr(>|t|)"]
p_values_Full_kaa<- summary(Full_kaa)$coefficients[, "Pr(>|t|)"]

p_values_list <- list(
  Null_score = p_values_Null_score,
  Reduced_score = p_values_Reduced_score,
  Full_score = p_values_Full_score,
  Null_perfe = p_values_null_perfe,
  Full_perfe = p_values_full_perfe,
  Reduced_perfe = p_values_reduced_perfe,
  Null_lr = p_values_Null_lr,
  Reduced_lr = p_values_Reduced_lr,
  Full_lr = p_values_Full_lr,
  Null_conf = p_values_Null_conf,
  Reduced_conf = p_values_Reduced_conf,
  Full_conf = p_values_Full_conf,
  Null_be1 = p_values_Null_be1,
  Reduced_be1 = p_values_Reduced_be1,
  Full_be1 = p_values_Full_be1,
  Null_kax = p_values_Null_kax,
  Reduced_kax = p_values_Reduced_kax,
  Full_kax = p_values_Full_kax,
  Null_kaa = p_values_Null_kaa,
  Reduced_kaa = p_values_Reduced_kaa,
  Full_kaa = p_values_Full_kaa
)

#other method
p_adjusted_list <- lapply(p_values_list, function(p) p.adjust(p, method = "BH"))

all_p_values <- unlist(p_values_list)

# Apply global FDR correction
p_adjusted_global <- p.adjust(all_p_values, method = "BH")

# View the globally adjusted p-values
p_values_table <- data.frame(
  Predictor = names(all_p_values),
  Original_P_Value = sprintf("%.3f", all_p_values),  # Format to 3 decimal places
  Adjusted_P_Value = sprintf("%.3f", p_adjusted_global)  # Format adjusted p-values
)
# Load dplyr for pretty printing (optional)
library(dplyr)

# Arrange by adjusted p-value and print
p_values_table <- p_values_table %>%
  arrange(Adjusted_P_Value)


#reduced_significant_models <- p_values_table %>%
 # filter(
  #  grepl("Reduced", Predictor) & 
   # as.numeric(Original_P_Value) < 0.05 & 
    #as.numeric(Adjusted_P_Value) > 0.06  # Ensure Adjusted_P_Value is greater than alpha
  #) %>%
  #arrange(Predictor)

reduced_models <- p_values_table %>%
  filter(grepl("Reduced", Predictor)) %>%  # Filter for 'Reduced'
  arrange(Predictor) 

as.tibble(reduced_models) %>% print(n=1000) 