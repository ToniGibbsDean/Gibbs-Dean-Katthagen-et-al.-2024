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

options(scipen = 999)

path <- "Figures"

trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat.csv") %>% as_tibble
personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat.csv") %>% as_tibble %>% mutate(level=as.factor(level))

################################################################################################################################################
# set contrasts for levels 
################################################################################################################################################
              
 personwise_ParamXspaceTdat_noL3 <- personwise_ParamXspaceTdat %>% filter(!level==3)
        personwise_ParamXspaceTdat_noL3$level<-droplevels(personwise_ParamXspaceTdat_noL3$level)
                        cMat <- cbind(c(-1,1,0), # 3 vs 4
                                      c(0,-1,1)) #5vs 6
                        contrasts(personwise_ParamXspaceTdat_noL3$level) <-cMat
                        colnames(attr(personwise_ParamXspaceTdat_noL3$level, "contrasts")) <- c("4v5", "5v6")

################################################################################################################################################
# modelling kax
################################################################################################################################################

modFull_kax<-
            personwise_ParamXspaceTdat_noL3 %>%
            #filter(!kax>25) %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kax ~ level*spqH + age + gender + device_type + education +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modFull_kax)

    modRedu_kax<- #winning
            personwise_ParamXspaceTdat_noL3 %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kax ~ level +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modRedu_kax)

    modNull_kax<-
            personwise_ParamXspaceTdat_noL3 %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kax ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(modFull_kax,
                        modRedu_kax,
                        modNull_kax)    

MuMIn::r.squaredGLMM(modFull_kax)
MuMIn::r.squaredGLMM(modRedu_kax)
MuMIn::r.squaredGLMM(modNull_kax)
   