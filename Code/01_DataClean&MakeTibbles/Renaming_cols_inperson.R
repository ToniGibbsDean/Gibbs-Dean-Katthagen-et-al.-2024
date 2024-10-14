### ONCAMPUS DATA CLEANING####

# 1. Packages and path to project directory ######
set.seed(0.1)

library(tidyverse)
library(lme4)
library(lmerTest)
library(nlme)
library(multcomp)
library(emmeans)
library(corrr)
#take scientific notation off
options(scipen=999)
library(sjPlot)
library(data.table)

setwd("D:/R/LDM/data/")

# 2. load data and make any additional data cleaning exclusions ####
#Definite exclusions and mutations 
#assign to an object; add in your own file structure from your PC
OnCampustibble<-readRDS(".//IntermediateOutputs/OnCampusTibble.RDS") %>%
  filter(level!="1") %>%
  mutate(BW=110-participantConfidence)  %>% 
  mutate(bw_pe=(BW/PE)) %>%
  mutate_if(is_character, as_factor) %>%
  mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
  mutate(level=as.factor(level)) %>%
  mutate(gamers=gamingtime.quant>2) %>%
  mutate(PEscaled=abs(scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition)) %>%
  mutate(PEsignedScaled=scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition) %>%
  mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
  mutate(SDhigh = SD %in% 0.12) %>%
  mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                           level == 6 & trialScore>0  ~ trialScore/2,
                                           TRUE ~ as.numeric(trialScore))) 




#Recoding of demographics where needed####

#recoding demograhpic variables so the groups arent too small etc. :
#Groups combined in some istances listed below:
#gender: other specified they were female in the text option
#Education A level with AS and other (as text says higher national diploma in both); PhD and postgrad
#Ethnicity – issue with prefer not to say as 1 – mixed category shouild now be mixed or non specified 
#Employment – other and prefer not to say combined 

#recoding code - switch in the demos of interest 
tibbledemo <- OnCampustibble %>%
  mutate(gender=recode(gender, 
                       "Other (please specify)" ="Female")) %>%  
  mutate(education=recode(education, 
                          "Other (please specify)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                          "AS Levels"              = "A Levels or equivalent diplomas (e.g. BTEC)",
                          #"A Levels or equivalent diplomas (e.g. BTEC)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                          "PhD or equivalent professional qualification" = "Postgraduate degree (MSc/MA/MRes)"))  %>% 
  mutate(employment=recode(employment,
                           "Other (please specify)" = "Prefer not to say")) # pull(employment) %>% table  

#####output
saveRDS(tibbledemo, file = ".//OnCampusTibble.RDS")