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

options(scipen = 999)

path <- "Figures"

trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat.csv") %>% as_tibble
personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat.csv") %>% as_tibble %>% mutate(level=as.factor(level))

Noiseblock = "#718ad0"
Volatilityblock = "#bf8c7f"
Combineduncertainty = "#66803c"
bivalent = "#97493b"

################################################################################################################################################
# set contrasts for levels 
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
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ level*spqH + age + gender + device_type + education +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modFull)

    modRedu<-
            personwise_ParamXspaceTdat %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ level*spqH +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

                                    summary(modRedu)

    modNull<-
            personwise_ParamXspaceTdat %>%
                #select( Participant.Private.ID, level,value, age,gender,device_type,education, spqH, param) %>%
                drop_na %>%
                lmer(be1 ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))

output<-MuMIn::model.sel(modNull,
                        modRedu,
                        modFull)    
x<-as.data.frame(output)


MuMIn::r.squaredGLMM(modNull)
MuMIn::r.squaredGLMM(modRedu)
MuMIn::r.squaredGLMM(modFull)


SPQ_be1XlevelXgroup_plot<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=level, fill=spqH))+ 
                        geom_boxplot() +
                        theme_classic()+
                        scale_fill_manual(values = c(Noiseblock, Volatilityblock, Combineduncertainty, bivalent))

SPQ_be1_plot<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=spqH, group=spqH))+ 
                        geom_boxplot(aes(fill=spqH)) +
                        theme_classic()+
                        scale_fill_manual(values = c(Noiseblock, Volatilityblock, Combineduncertainty, bivalent))

SPQ_be1Xlevel_plot<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=level))+ 
                        geom_boxplot(aes(fill=level)) +
                        theme_classic() +
                        scale_fill_manual(values = c(Noiseblock, Volatilityblock, Combineduncertainty, bivalent))


ggsave(SPQ_be1XlevelXgroup_plot, file="Figures/SPQ_be1XlevelXgroup_plot.pdf")
ggsave(SPQ_be1_plot, file="Figures/SPQ_be1_plot.pdf")
ggsave(SPQ_be1Xlevel_plot, file="Figures/SPQ_be1Xlevel_plot.pdf")
