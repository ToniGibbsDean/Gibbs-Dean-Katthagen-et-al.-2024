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

 personwise_ParamXspaceTdat_noL4 <- personwise_ParamXspaceTdat %>% filter(!level==4)
        personwise_ParamXspaceTdat_noL4$level<-droplevels(personwise_ParamXspaceTdat_noL4$level)
                        cMat <- cbind(c(-1,1,0), # 3 vs 4
                                      c(0,-1,1)) #5vs 6
                        contrasts(personwise_ParamXspaceTdat_noL4$level) <-cMat
                        colnames(attr(personwise_ParamXspaceTdat_noL4$level, "contrasts")) <- c("3v5", "5v6")

################################################################################################################################################
# modelling for kaa
################################################################################################################################################

modFull_kaa<-
            personwise_ParamXspaceTdat_noL4 %>%
            #filter(!kaa>25) %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kaa ~ level*spqH + age + gender + device_type + education +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modFull_kaa)

    modRedu_kaa<- #winning
            personwise_ParamXspaceTdat_noL4 %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kaa ~ level + gender +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modRedu_kaa)

    modNull_kaa<-
            personwise_ParamXspaceTdat_noL4 %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(kaa ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(modFull_kaa,
                        modRedu_kaa,
                        modNull_kaa)    

MuMIn::r.squaredGLMM(modFull_kaa)
MuMIn::r.squaredGLMM(modRedu_kaa)
MuMIn::r.squaredGLMM(modNull_kaa)