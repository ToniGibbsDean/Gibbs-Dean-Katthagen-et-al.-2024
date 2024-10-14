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
        genpop2tibble<-readRDS("Outputs/GenPop2Tibble_new.RDS") %>%
                filter(level!="1") %>%
                filter(Participant.Device.Type!="tablet") %>% #only one tablet - so remove
                #filter(devdx.text %in% c("ADHD", "Chronic migraine")) %>% #exclude all expect ADHD and chronic migraine (n10 altogether)
                #filter(devdx!="Yes (if you feel comfortable to do so please specify)") %>%
                mutate(BW=110-participantConfidence)  %>% 
                mutate(bw_pe=(BW/PE)) %>%
                mutate(wPE = PE/BW) %>%
                mutate_if(is_character, as_factor) %>%
                mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                mutate(level=as.factor(level)) %>%
                #mutate(gamers=gamingtime.quant>2) %>%
                mutate(PEscaled=abs(scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition)) %>%
                mutate(PEsignedScaled=scale(Result, center = c(-0.5), scale = c(0.01)) - participantPosition) %>%
                mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                mutate(SDhigh = SD %in% 0.12) %>%
                mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) 




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
        genpop2<- genpop2tibble %>%
                        mutate(gender=recode(gender, 
                                "Other (please specify)" ="Female")) %>%  
                        mutate(education=recode(education, 
                                "Other (please specify)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "AS Levels"              = "A Levels or equivalent diplomas (e.g. BTEC)",
                                #"A Levels or equivalent diplomas (e.g. BTEC)" = "A Levels or equivalent diplomas (e.g. BTEC)",
                                "PhD or equivalent professional qualification" = "Postgraduate degree (MSc/MA/MRes)"))  %>% 
                        mutate(ethnicity=recode(ethnicity,
                                "Prefer not to say" = "Mixed or multiple ethnic groups, or if your specific ethnicity is not listed, please specify (box will appear when ticked): ")) %>% 
                        mutate(employment=recode(employment,
                                "Other (please specify)" = "Prefer not to say")) # pull(employment) %>% table           
                

################################################################################################       
#create modelling df
################################################################################################

GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
        
df_mod <- genpop2 %>%
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
                    #meanchangeTF=dplyr::first(meanchange),
                    #SDhighcondit=dplyr::first(SDhigh),
                    masq_total_score=dplyr::first(masq_total_score), 
                    gdd_score=dplyr::first(gdd_score),
                    aa_score=dplyr::first(aa_score),
                    gda_score=dplyr::first(gda_score),
                    ad_score=dplyr::first(ad_score),
                    masqCombined_anxiety=sum(aa_score, gda_score),
                    masqCombined_depression=sum(ad_score, gdd_score),
                    gamingtime=dplyr::first(gamingtime),
                    pdi_total=dplyr::first(pdi_total),
                    dsm_total=dplyr::first(dsm_total),
                    green_total=dplyr::first(green_total),
                    gamers=dplyr::first(gamers),
                    education=dplyr::first(education),
                    ethnicity=dplyr::first(ethnicity),
                    employment=dplyr::first(employment),
                    psychep=dplyr::first(psychep), 
                    psychdx=dplyr::first(psychdx),
                    neurologicalImp=dplyr::first(devdx),
                    spq_total=dplyr::first(spq_total),
                    #FourClass=dplyr::first(c4class),
                    #wideStartSD=dplyr::first(wideStartSD),
                    SDhigh=dplyr::first(SDhigh),
                    trialName=dplyr::first(trialName),
                    gender=dplyr::first(gender),
                    age=dplyr::first(age),
                    device_type=dplyr::first(device_type)) %>%
                    filter(level %in% c(3, 4, 5, 6)) 

        df_mod<- df_mod %>%
                drop_na


        df_mod_confidence<-genpop2 %>%
                mutate(gamers=gamingtime.quant>2) %>% 
                mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
                group_by(Participant.Private.ID, level, SDhigh, trialName, gender, age, device_type) %>%
                summarise(score=mean(trialScore), 
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
                    #meanchangeTF=dplyr::first(meanchange),
                    #SDhighcondit=dplyr::first(SDhigh),
                    masq_total_score=dplyr::first(masq_total_score), 
                    gdd_score=dplyr::first(gdd_score),
                    aa_score=dplyr::first(aa_score),
                    gda_score=dplyr::first(gda_score),
                    ad_score=dplyr::first(ad_score),
                    masqCombined_anxiety=sum(aa_score, gda_score),
                    masqCombined_depression=sum(ad_score, gdd_score),
                    gamingtime=dplyr::first(gamingtime),
                    pdi_total=dplyr::first(pdi_total),
                    dsm_total=dplyr::first(dsm_total),
                    green_total=dplyr::first(green_total),
                    gamers=dplyr::first(gamers),
                    education=dplyr::first(education),
                    ethnicity=dplyr::first(ethnicity),
                    employment=dplyr::first(employment),
                    psychep=dplyr::first(psychep), 
                    psychdx=dplyr::first(psychdx),
                    neurologicalImp=dplyr::first(devdx),
                    spq_total=dplyr::first(spq_total),
                    wideStartSD=dplyr::first(startSD)) %>%
                    filter(level %in% c(3, 4, 5, 6)) 
        df_mod_confidence<- df_mod_confidence %>%
                drop_na

    #contrasts
            #exlucding L6 - sensitivity analysis
            # df_mod$level<-droplevels(df_mod$level)
            #  cMat <- cbind(c(1,-1,0), # 3 vs 4
            #             c(0,1,-1)) # 4 vs 5
                #        contrasts(df_mod$level) <-cMat
                #        colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5")
            #inc l6
                df_mod$level<-droplevels(df_mod$level)
                cMat <- cbind( c(-1,1,0,0), # 3 vs 4
                               c(0,-1,1,0), # 4 vs 5
                               c(0,0,-1,1)) #5vs 6
                contrasts(df_mod$level) <-cMat
                colnames(attr(df_mod$level, "contrasts")) <- c("3v4", "4v5", "5v6")

################################################################################################
#modelling score
################################################################################################

Null_score <- df_mod %>% 
lmer(meancorrectedL6trialScore ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Full_score <- df_mod %>% 
lmer(meancorrectedL6trialScore ~ device_type + 
        gamers*level + 
        gender + 
        masqCombined_depression*level +
        green_total*level +  
        pdi_total*level + 
        spq_total*level + 
        masqCombined_anxiety*level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Reduced_score <- df_mod %>% 
lmer(meancorrectedL6trialScore ~ device_type + 
        gamers*level + 
        gender + 
        masqCombined_depression +
        green_total +  
        #pdi_total*level + 
        spq_total*level + 
        masqCombined_anxiety +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(Null_score, Reduced_score, Full_score)
MuMIn::r.squaredGLMM(Null_score)
MuMIn::r.squaredGLMM(Reduced_score)
MuMIn::r.squaredGLMM(Full_score)

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
mutate(logmeanPerfE = log(meanPerfE)) %>%
lmer(logmeanPerfE ~ device_type + 
        gamers*level + 
        gender + 
        masqCombined_depression*level +
        green_total*level +  
        pdi_total*level + 
        spq_total*level + 
        masqCombined_anxiety*level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

Reduced_perfe <- df_mod %>%  
mutate(logmeanPerfE = log(meanPerfE)) %>% 
lmer(logmeanPerfE ~ device_type + gender + masqCombined_depression + level +
     green_total +  pdi_total:level + masqCombined_anxiety +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                        control=lme4::lmerControl(optimizer="bobyqa", 
                        optCtrl=list(maxfun=2e7)))

MuMIn::model.sel(Null_perfe, Reduced_perfe, Full_perfe)
MuMIn::r.squaredGLMM(Null_perfe)
MuMIn::r.squaredGLMM(Reduced_perfe)
MuMIn::r.squaredGLMM(Full_perfe)


################################################################################################
#modelling LR
################################################################################################

Null_lr <- df_mod %>%
lmer(beta ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Full_lr <- df_mod %>% 
lmer(beta ~ device_type + 
        gamers*level + 
        gender + 
        masqCombined_depression*level +
        green_total*level +  
        pdi_total*level + 
        spq_total*level + 
        masqCombined_anxiety*level +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

Reduced_lr <- df_mod %>%  
  lmer(beta ~ device_type + 
       gamers*level +    
       spq_total*level + 
       masqCombined_depression +
  (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)) )

MuMIn::model.sel(Null_lr, Reduced_lr, Full_lr)
MuMIn::r.squaredGLMM(Null_lr)
MuMIn::r.squaredGLMM(Reduced_lr)
MuMIn::r.squaredGLMM(Full_lr)

################################################################################################
#modelling BEAM
################################################################################################

Null_conf <- df_mod_confidence %>%
lmer(meanconf ~ 1 +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)) )

Full_conf <- df_mod_confidence %>% 
lmer(meanconf ~ device_type + 
gamers*SDhigh + 
gender + 
masqCombined_depression*SDhigh +
green_total*SDhigh +  
pdi_total*SDhigh + 
spq_total*SDhigh + 
masqCombined_anxiety*SDhigh +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
control=lme4::lmerControl(optimizer="bobyqa", 
optCtrl=list(maxfun=2e7)))

Reduced_conf <-  df_mod_confidence %>%  
lmer(meanconf ~ gamers*SDhigh + 
gender +
masqCombined_depression*SDhigh + 
masqCombined_anxiety*SDhigh + 
green_total*SDhigh  +
(1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                control=lme4::lmerControl(optimizer="bobyqa", 
                                optCtrl=list(maxfun=2e7)) )

MuMIn::model.sel(Null_conf, Reduced_conf, Full_conf)
MuMIn::r.squaredGLMM(Null_conf)
MuMIn::r.squaredGLMM(Reduced_conf)
MuMIn::r.squaredGLMM(Full_conf)