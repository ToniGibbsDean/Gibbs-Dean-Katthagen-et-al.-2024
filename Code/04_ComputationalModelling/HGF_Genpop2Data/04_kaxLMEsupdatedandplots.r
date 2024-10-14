################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################
        
    #set seed and load pacakges 
        set.seed(0.1)
    
        library(tidyverse)
        #library(ggplot2)
        #library(ggplotify)
        #library(nlme)
        library(lme4)
        library(lmerTest)
        library(sjPlot)
        library(patchwork)
        library(ggpubr)
        library(MuMIn)


        options(scipen=999)

        figures<-"Figures" 

        trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat_GENPOP2.csv") %>% as_tibble
        personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat_GENPOP2.csv") %>% 
        as_tibble %>% 
        mutate(level=as.factor(level)) %>%
        filter(!level %in% c(2,3))


################################################################################################################################################
# create wide df for modelling parameters 
################################################################################################################################################

    ##########################
    # create summary df for key task metrics 
    ##########################
        
        personwise_ParamXspaceTdat$level<-droplevels(personwise_ParamXspaceTdat$level)
                        cMat <- cbind(c(-1,1,0), # 3 vs 4
                                      c(0,-1,1), # 4 vs 5
                                      c(0,0,-1)) #5vs 6
                        contrasts(personwise_ParamXspaceTdat$level) <-cMat
                        colnames(attr(personwise_ParamXspaceTdat$level, "contrasts")) <- c("4v5", "5v6")


################################################################################################################################################
#
################################################################################################################################################

personwise_ParamXspaceTdat %>% ggplot(aes(x=log(kax))) +
geom_density()

        fullModkax<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%
                lmer(log(kax) ~ level*masqCombined_anxiety +
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
        summary(fullModkax)
        
        reduModkax<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%            
            lmer(log(kax) ~ level*spq_total +
                           level*pdi_total +
                           level*green_total + 
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
        
        summary(reduModkax)

        reduModkax2<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%            
            lmer(log(kax) ~ level*pdi_total +
                           green_total + 
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
        
        summary(reduModkax2)

        NullModkax2<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%  
            lmer(log(kax) ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
        
MuMIn::model.sel(fullModkax, reduModkax, NullModkax2)
MuMIn::r.squaredGLMM(NullModkax2)
MuMIn::r.squaredGLMM(reduModkax)
MuMIn::r.squaredGLMM(fullModkax)

summary(reduModkax)

        kaxPDIinteractionBlock5and6_noStandErr<-personwise_ParamXspaceTdat %>% 
          ggplot(aes(x=pdi_total, y=log(kax), group=level)) +
          geom_point(aes(color=level), alpha=0.2) +
          geom_smooth(aes(color=level), method=lm) +
          theme_classic()

        kaxPDIinteractionBlock5and6<-personwise_ParamXspaceTdat %>% 
          ggplot(aes(x=pdi_total, y=log(kax), group=level)) +
          geom_point(aes(color=level), alpha=0.2) +
          geom_smooth(aes(color=level), method=lm, se=F) +
          theme_classic()

        kaxGreenOverall_noStandErr<-personwise_ParamXspaceTdat %>%
          ggplot(aes(x=green_total, y=log(kax))) +
          geom_point(alpha=0.2) +
          geom_smooth(method=lm) +
          theme_classic()

        kaxGreenOverall<-personwise_ParamXspaceTdat %>%
          ggplot(aes(x=green_total, y=log(kax))) +
          geom_point(alpha=0.2) +
          geom_smooth(method=lm, se=F) +
          theme_classic()

ggsave(kaxPDIinteractionBlock5and6_noStandErr, file="Figures/kaxPDIinteractionBlock5and6_noStandErr.pdf")
ggsave(kaxPDIinteractionBlock5and6, file="Figures/kaxPDIinteractionBlock5and6.pdf")
ggsave(kaxGreenOverall_noStandErr, file="Figures/kaxGreenOverall_noStandErr.pdf")
ggsave(kaxGreenOverall, file="Figures/kaxGreenOverall.pdf")