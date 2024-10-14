################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################

# set seed and load pacakges
set.seed(0.1)

library(tidyverse)
library(ggplot2)
# library(ggplotify)
library(nlme)
library(lme4)
library(lmerTest)
library(sjPlot)
library(patchwork)
library(ggpubr)
        library(ggpubr)

options(scipen = 999)

path <- "Figures"

trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat_GENPOP2.csv") %>% as_tibble
personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat_GENPOP2.csv") %>% 
  as_tibble %>% 
  mutate(level=as.factor(level)) %>%
  filter(!level==2)

################################################################################################################################################
# set contrasts for Levels 
################################################################################################################################################

        personwise_ParamXspaceTdat$level<-droplevels(personwise_ParamXspaceTdat$level)
                        cMat <- cbind(c(-1,1,0,0), # 3 vs 4
                                      c(0,-1,1,0), # 4 vs 5
                                      c(0,0,-1,1)) #5vs 6
                        contrasts(personwise_ParamXspaceTdat$level) <-cMat
                        colnames(attr(personwise_ParamXspaceTdat$level, "contrasts")) <- c("3v4", "4v5", "5v6")

################################################################################################################################################
#model selection: reduction
################################################################################################################################################

    modFull<-
            personwise_ParamXspaceTdat %>%
                #select( Participant.Private.ID, Level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ level*masqCombined_anxiety +
                           level*spq_total +
                           level*pdi_total +
                           level*green_total + 
                           level*masqCombined_depression +
                           age + 
                           gender + 
                           device_type + 
                           education +
                           ethnicity +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modFull)

    modRedu<-
            personwise_ParamXspaceTdat %>%
                #select( Participant.Private.ID, Level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ level*spq_total +
                           level*pdi_total +
                           education +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
                                    summary(modRedu)

    modNull<-
            personwise_ParamXspaceTdat %>%
                #select( Participant.Private.ID, Level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(modNull,
                        modRedu,
                        modFull)    
MuMIn::r.squaredGLMM(modNull)
MuMIn::r.squaredGLMM(modRedu)
MuMIn::r.squaredGLMM(modFull)


