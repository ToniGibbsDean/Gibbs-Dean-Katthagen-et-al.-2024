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

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
  inPersondat<-readRDS("Outputs/OnCampusTibble.RDS") %>%
                filter(level!="1") %>%
                mutate(BW=110-participantConfidence)  %>% 
                mutate(bw_pe=(BW/PE)) %>%
                mutate(wPE = PE/BW) %>%
                mutate_if(is_character, as_factor) %>%
                mutate(SDHigh=(SD==0.12)) %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level)) %>%
                #mutate(gamers=gamingtime.quant>2) %>%
                mutate(PEscaled=abs(scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition)) %>%
                mutate(PEsignedScaled=scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition) %>%
                mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                mutate(SDhigh = SD %in% 0.12) %>%
                mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) %>%
                mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                mutate( signedPositionChange=lead(participantPosition)- participantPosition) %>%
                mutate( positionChange=abs(lead(participantPosition)- participantPosition)) %>%
                mutate( LR = signedPositionChange/signedPE) 

################################################################################################
#Recoding of demographics where needed
################################################################################################

 #recoding demograhpic variables so the groups arent too small etc. :
        #Groups combined in some istances listed below:
                #gender: other specified they were female in the text option
                #Education A level with AS and other (as text says higher national diploma in both); PhD and postgrad
                #Ethnicity – issue with prefer not to say as 1 – mixed category shouild now be mixed or non specified 
                #Employment – other and prefer not to say combined 
        
        #recoding code - switch in the demos of interest 
        dat <- inPersondat %>%
                        mutate(gender=recode(gender, 
                                "Other (please specify)" ="Female")) %>%  
                        mutate(education=recode(education, 
                                "Other (please specify)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "AS Levels"              = "A Levels or equivalent diplomas (e.g. BTEC)",
                                #"A Levels or equivalent diplomas (e.g. BTEC)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "PhD or equivalent professional qualification" = "Postgraduate degree (MSc/MA/MRes)"))  %>% 
                        mutate(employment=recode(employment,
                                "Other (please specify)" = "Prefer not to say")) # pull(employment) %>% table           
                


################################################################################################       
#create modelling df
################################################################################################

GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
        
df_mod <- dat %>%
            group_by(Participant.Private.ID, level) %>%
            mutate(gamers=gamingtime.quant>2) %>% 
            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>% 
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            summarise(score=mean(trialScore), 
                    beta=dplyr::first(beta),
                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPEscaled=mean(PEscaled),
                    meanPEsignedScaled=mean(PEsignedScaled),
                    meanPerfE=mean(PerfE),
                    meanwPE=mean(wPE),
                    PerfE=dplyr::first(PerfE),
                    PE=dplyr::first(PE),
                    PEscaled=dplyr::first(PEscaled),
                    PEsignedScaled=dplyr::first(PEsignedScaled),
                    gamingtime=dplyr::first(gamingtime),
                    pdi_total=dplyr::first(pdi_total),
                    green_total=dplyr::first(green_total),
                    gamers=dplyr::first(gamers),
                    education=dplyr::first(education),
                    employment=dplyr::first(employment),
                    spq_total=dplyr::first(spq_total),
                    #FourClass=dplyr::first(c4class),
                    #wideStartSD=dplyr::first(wideStartSD),
                    SDhigh=dplyr::first(SDhigh),
                    trialName=dplyr::first(trialName),
                    gender=dplyr::first(gender),
                    age=dplyr::first(age) )%>%
                    filter(level %in% c(3, 4, 5, 6)) 

        df_mod<- df_mod %>%
                drop_na


        df_mod_confidence<-dat %>%
                mutate(gamers=gamingtime.quant>2) %>% 
                mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
                group_by(Participant.Private.ID, level, SDhigh, trialName, gender, age) %>%
                summarise(score=mean(trialScore), 
                    #beta=dplyr::first(beta),
                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                    meanconf=mean(participantConfidence),
                    meanBW=mean(BW),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPEscaled=mean(PEscaled),
                    meanPEsignedScaled=mean(PEsignedScaled),
                    meanPerfE=mean(PerfE),
                    meanwPE=mean(wPE),
                    PerfE=dplyr::first(PerfE),
                    PE=dplyr::first(PE),
                    PEscaled=dplyr::first(PEscaled),
                    PEsignedScaled=dplyr::first(PEsignedScaled),
                    gamingtime=dplyr::first(gamingtime),
                    pdi_total=dplyr::first(pdi_total),
                    green_total=dplyr::first(green_total),
                    gamers=dplyr::first(gamers),
                    education=dplyr::first(education),
                    employment=dplyr::first(employment),
                    spq_total=dplyr::first(spq_total),
                    #FourClass=dplyr::first(c4class),
                    #wideStartSD=dplyr::first(wideStartSD),
                    SDhigh=dplyr::first(SDhigh),
                    trialName=dplyr::first(trialName),
                    gender=dplyr::first(gender),
                    age=dplyr::first(age) )%>%
                    filter(level %in% c(3, 4, 5, 6)) 
        df_mod_confidence<- df_mod_confidence %>%
                drop_na

    #contrasts
                df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                               c(0,-1,1,0), # 4 vs 5
                               c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")

#######################
#score
#######################

df_mod %>%
  ggplot(aes(x=meancorrectedL6trialScore)) +
  geom_density() +
  facet_wrap(~level)

all<-df_mod %>% 
lmer(meancorrectedL6trialScore ~ age +
        gender +
        spq_total*level +
        employment +
        gamers +
        education +
        green_total*level +
        pdi_total*level +
        #level +
        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
        control=lme4::lmerControl(optimizer="bobyqa", 
        optCtrl=list(maxfun=2e7)) )
#summary(all)

         reduced<-df_mod %>% 
                        lmer(meancorrectedL6trialScore ~ #age +
                                                         #gender +
                                                         spq_total*level +
                                                         #employment +
                                                        #gamers +
                                                         education +
                                                         green_total +
                                                         pdi_total*level +
                                                         #level +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(reduced)

         reduced2<-df_mod %>% 
                        lmer(meancorrectedL6trialScore ~ #age +
                                                         #gender +
                                                         spq_total +
                                                         #employment +
                                                         #gamers +
                                                         education +
                                                         green_total +
                                                         pdi_total*level +
                                                         #level +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(reduced2)
         null<-df_mod %>% 
                        lmer(meancorrectedL6trialScore ~ 1+
                                                         #level +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        summary(reduced3)



        MuMIn::model.sel(all, reduced, reduced2, null) #--> reduced 4
        car::qqPlot(resid(reduced4))
        scatter.smooth(residuals(reduced4) ~ fitted(reduced4))

MuMIn::r.squaredGLMM(all)
MuMIn::r.squaredGLMM(reduced2)
MuMIn::r.squaredGLMM(null)

#######################
#perfE
#######################

df_mod %>%
  ggplot(aes(x=meanPerfE)) +
  geom_density() +
  facet_wrap(~level)

  df_mod %>%
        ggplot(aes(y=meanPerfE, group=level)) +
        geom_boxplot()

           all_perfe<-df_mod %>% 
                        lmer(log(meanPerfE) ~ age +
                                          gender +
                                          spq_total*level +
                                          employment +
                                          gamers +
                                          education +
                                          green_total*level +
                                          pdi_total*level +
                                          #level +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        #summary(all_perfe)

         reduced_perfe<-df_mod %>% 
                        lmer(log(meanPerfE) ~ #age +
                                                         #gender +
                                                        # spq_total*level +
                                                         #employment +
                                                         #gamers +
                                                         #education +
                                                        # green_total*level +
                                                         pdi_total +
                                                         level +
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        #summary(reduced_perfe)

         null<-df_mod %>% 
                        lmer(log(meanPerfE) ~ 1+
                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                        optCtrl=list(maxfun=2e7)) )
                                                        #summary(reduced_perfe)
        MuMIn::model.sel(null, reduced_perfe, all_perfe) #--> reduced 4

MuMIn::r.squaredGLMM(all_perfe)
MuMIn::r.squaredGLMM(reduced_perfe)
MuMIn::r.squaredGLMM(null)

#######################
#Beam
#######################


df_mod_confidence %>%
  ggplot(aes(x=meanconf)) +
  geom_density() +
  facet_wrap(~level)

x<-df_mod_confidence %>%
      ggplot(aes(y=meanconf, group=level)) +
      geom_boxplot()

all_beam<-df_mod_confidence %>% 
lmer(meanconf ~ age +
gender +
spq_total*SDhigh +
employment +
gamers +
education +
green_total*SDhigh +
pdi_total*SDhigh +
#level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )
#summary(all_beam)

redu_beam<-df_mod_confidence %>% 
                        lmer(meanconf ~ #age +
                                        #gender +
                                        spq_total +
                                        employment +
                                        #gamers +
                                        education +
                                        pdi_total +
                                        SDhigh +
                                        #level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)) )
                                #summary(redu_beam)

null_beam<-df_mod_confidence %>% 
                        lmer(meanconf ~1+
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)) )
                                #summary(redu_beam)

        MuMIn::model.sel(all_beam, redu_beam, null_beam) #--> reduced 4

MuMIn::r.squaredGLMM(all_beam)
MuMIn::r.squaredGLMM(redu_beam)
MuMIn::r.squaredGLMM(null_beam)

#######################
#LR
#######################

df_mod %>%
  ggplot(aes(x=beta)) +
  geom_density() +
  facet_wrap(~level)

    all_lr<-df_mod %>% 
                lmer(beta ~ age +
                            gender +
                            spq_total*level +
                            employment +
                            gamers +
                            education +
                            green_total*level +
                            pdi_total*level +
                            #level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                          control=lme4::lmerControl(optimizer="bobyqa", 
                          optCtrl=list(maxfun=2e7)) )
                         # summary(all_lr)

  reduced_lr<-df_mod %>% 
                lmer(beta ~ #age +
                            #gender +
                            spq_total +
                            employment +
                            #gamers +
                            #education +
                            green_total +
                            #pdi_total +
                            level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                          control=lme4::lmerControl(optimizer="bobyqa", 
                          optCtrl=list(maxfun=2e7)) )
                         # summary(reduced_lr)


  null<-df_mod %>% 
                lmer(beta ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                          control=lme4::lmerControl(optimizer="bobyqa", 
                          optCtrl=list(maxfun=2e7)) )
                         # summary(null)

        MuMIn::model.sel(all_lr, reduced_lr, null) #--> reduced 4
MuMIn::r.squaredGLMM(all_lr)
MuMIn::r.squaredGLMM(reduced_lr)
MuMIn::r.squaredGLMM(null)