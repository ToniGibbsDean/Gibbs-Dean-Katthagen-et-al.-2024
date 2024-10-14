################################################################################################
# 1. Packages and path to project directory ######
################################################################################################
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
    #library(sjPlot)

################################################################################################
# 2. load data
################################################################################################

    spqtibble<-readRDS("Outputs/SPQTibble.RDS") %>%
                    filter(SPQsum != "n/a") %>% #removes 3
                    filter(level!="1") %>%
                    filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                    mutate(spqH=SPQsum>=30)%>%
                    mutate(SDHigh=(SD==0.12)) %>%
                    mutate(level=as.factor(level)) %>%
                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                    mutate(highVolatility=level %in% c(4:6)) %>%
                     mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) 

    
# Recode variables where needed
      spq<- spqtibble %>%
                        mutate(Ethnicity=as.character(Ethnicity)) %>%
                        mutate(Education=as.character(Education)) %>%
                        mutate(Gender=as.character(Gender)) %>%
                        mutate(Ethnicity=recode(Ethnicity, "6" = "2")) %>% 
                        mutate(Education=recode(Education, "1" = "2", "3" = "4", "5" = "6", "7" = "6"))
################################################################################################       
#create modelling dfs
################################################################################################
        df_mod<-spq %>%
            #mutate(gamers=gamingtime.quant>2) %>% 
            group_by(Participant.Private.ID, level) %>%
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            summarise(meanScore=mean(trialScore), 
                    corectedScore=mean(correctedL6trialScore),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPerfE=mean(PerfE),
                    #bonus=sum(reward),
                    age=dplyr::first(Age),
                    ethnicity=as.factor(dplyr::first(Ethnicity)),
                    education=as.factor(dplyr::first(Education)),
                    employment=as.factor(dplyr::first(Employment)),
                    #country=dplyr::first(Country),
                    #language=dplyr::first(Language),
                    HeadInjury=dplyr::first(HeadInjury),
                    gender=as.factor(dplyr::first(Gender)),
                    device_type=dplyr::first(Participant.Device.Type),
                    trialName=dplyr::first(trialName),
                    spqH=as.factor(dplyr::first(spqH)),
                    wideStartSD=as.factor(dplyr::first(startSD)),
                    highVol=as.factor(dplyr::first(highVolatility)),
                    highnoise=as.factor(dplyr::first(SDHigh))) %>%
                    filter(level %in% c(3, 4, 5, 6))

        
        df_mod<- df_mod %>%
                drop_na
 df_mod_confidence<-spq %>%
                                #mutate(gamers=gamingtime.quant>2) %>% 
                                group_by(Participant.Private.ID, level, SDHigh) %>%
                                mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
                                summarise(meanScore=mean(trialScore), 
                                        meanconf=mean(participantConfidence),
                                        sumsuccess=sum(success/length(level)),
                                        meanPE=mean(PE),
                                        #bonus=sum(reward),
                                        age=dplyr::first(Age),
                                        ethnicity=as.factor(dplyr::first(Ethnicity)),
                                        education=as.factor(dplyr::first(Education)),
                                        employment=as.factor(dplyr::first(Employment)),
                                        #country=dplyr::first(Country),
                                        #language=dplyr::first(Language),
                                        HeadInjury=dplyr::first(HeadInjury),
                                        gender=as.factor(dplyr::first(Gender)),
                                        device_type=dplyr::first(Participant.Device.Type),
                                        trialName=dplyr::first(trialName),
                                        spqH=as.factor(dplyr::first(spqH)),
                                        wideStartSD=as.factor(dplyr::first(startSD)),
                                        highnoise=as.factor(dplyr::first(SDHigh))) %>%
                                        filter(level %in% c(3, 4, 5, 6))

        
        df_mod_confidence<- df_mod_confidence %>%
                                                drop_na

            df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                        c(0,-1,1,0), # 4 vs 5
                        c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")


#######################
#score 
#######################
#lowest AIC = 
#NB when comparing with HGF stuff - remember that I have also taken out the tablet users here, but I think they
#must still be included in the  HGF data for some reason. 

null_score<-df_mod %>% 
lmer(corectedScore ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )
summary(mostsimple)

full_score <- df_mod %>% 
lmer(corectedScore ~ level*spqH + gender + age + education + ethnicity + 
    device_type + employment + 
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

reduced_score <- df_mod %>% 
lmer(corectedScore ~ 
      level + 
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                        control=lme4::lmerControl(optimizer="bobyqa", 
                        optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(null_score, full_score, reduced_score)
MuMIn::r.squaredGLMM(null_score)
MuMIn::r.squaredGLMM(full_score)
MuMIn::r.squaredGLMM(reduced_score)

summary(reduced_score)

################################################################################################
#modelling perfE
################################################################################################

Null_perfe <- df_mod %>%
mutate(logmeanPerfE = log(meanPerfE)) %>%
lmer(logmeanPerfE ~ 1 + (1|Participant.Private.ID) + 
(1|trialName/level), 
data=., 
REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Full_perfe <- df_mod %>% 
lmer(corectedScore ~ level*spqH + 
    gender + 
    age + 
    education + 
    ethnicity + 
    device_type + 
    employment + 
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

Reduced_perfe <- df_mod %>%  
mutate(logmeanPerfE=log(meanPerfE)) %>%
lmer(logmeanPerfE ~ education + device_type + level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)) )

MuMIn::model.sel(Null_perfe, Reduced_perfe, Full_perfe)
MuMIn::r.squaredGLMM(Null_perfe)
MuMIn::r.squaredGLMM(Reduced_perfe)
MuMIn::r.squaredGLMM(Full_perfe)


################################################################################################
#modelling BEAM
################################################################################################

Null_conf <- df_mod_confidence %>% 
lmer(meanconf ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Full_conf<- df_mod_confidence %>% 
lmer(meanconf ~ highnoise*spqH + 
gender + 
age + 
education + 
ethnicity + 
device_type + 
employment + 
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

Reduced_conf <-  df_mod_confidence %>%  
lmer(meanconf ~ #ethnicity +
     highnoise +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(Null_conf, Reduced_conf, Full_conf)
MuMIn::r.squaredGLMM(Null_conf)
MuMIn::r.squaredGLMM(Reduced_conf)
MuMIn::r.squaredGLMM(Full_conf)