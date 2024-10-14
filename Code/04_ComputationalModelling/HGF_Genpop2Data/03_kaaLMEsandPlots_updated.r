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
        filter(!level %in% c(2,4))%>%
        filter(!kaa>25)


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
                        colnames(attr(personwise_ParamXspaceTdat$level, "contrasts")) <- c("3v5", "5v6")


################################################################################################################################################
#
################################################################################################################################################

personwise_ParamXspaceTdat %>% ggplot(aes(x=log(kaa))) +
geom_density()

        fullModkaa<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%
                lmer(log(kaa) ~ level*masqCombined_anxiety +
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
        summary(fullModkaa)
        
        reduModkaa<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%lmer(log(kaa) ~ level + 
                           education + pdi_total +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
        summary(reduModkaa)

        NullModkaa2<-personwise_ParamXspaceTdat %>%
            group_by(Participant.Private.ID, level) %>%
            #select(kaa, ethnicity, employment, psychdx, gamers, education,
             #      level, Participant.Private.ID, spq_total, pdi_total, green_total,
              #     masqCombined_anxiety, masqCombined_depression) %>%
            drop_na() %>%lmer(log(kaa) ~ 1 +
                          (1|Participant.Private.ID) + 
                          (1|level), data=., REML=F,
                                    control=lme4::lmerControl(optimizer="bobyqa", 
                                    optCtrl=list(maxfun=2e7)))
        
MuMIn::model.sel(fullModkaa, reduModkaa, NullModkaa2)
MuMIn::r.squaredGLMM(NullModkaa2)
MuMIn::r.squaredGLMM(reduModkaa)
MuMIn::r.squaredGLMM(fullModkaa)

summary(reduModkaa)


##############################################################################################################################
#plots
##############################################################################################################################
kaaByLevel_violinPlot <- personwise_ParamXspaceTdat %>% 
  ggplot(aes(y=log(kaa), x=level, group=level)) +
  geom_violin(aes(fill=level)) +
  theme_classic()

kaaByLevel_boxPlot <- personwise_ParamXspaceTdat %>% 
  ggplot(aes(y=log(kaa), x=level, group=level)) +
  geom_violin(aes(fill=level)) +
  theme_classic()

ggsave(kaaByLevel_violinPlot, file="Figures/kaaByLevel_violinPlot.pdf")
ggsave(kaaByLevel_boxPlot, file="Figures/kaaByLevel_boxPlot.pdf")

##############################################################################################################################
#correaltion kaa x metrics
##############################################################################################################################