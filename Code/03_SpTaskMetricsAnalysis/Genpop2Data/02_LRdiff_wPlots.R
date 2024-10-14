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
    library(scales)
    
    #take scientific notation off
    #options(scipen=999)
     library(sjPlot)
     library(ggpubr)
     library(patchwork)

   path<-"Figures"


    MASQdepColour<- "#8d8449"
    MASQanxColour<-"#817fc7"

    greenColour<-"#bf5243"   
    PDIColour<-"#95b6c3"
    SPQColour<-"#513854" 
    

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
        genpop2tibble<-readRDS("Outputs/GenPop2Tibble_new.RDS") %>%
                filter(level!="1") %>%
                filter(device_type!="tablet") %>% #only one tablet - so remove
                #filter(devdx.text %in% c("ADHD", "Chronic migraine")) %>% #exclude all expect ADHD and chronic migraine (n10 altogether)
                filter(devdx!="Yes (if you feel comfortable to do so please specify)") %>%
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
                mutate(condition= case_when((SD == 0.03 | SD == 0.06) & (as.numeric(level == 5) | as.numeric(level == 6)) ~ "combined_highV_lowN",
                                                (SD == 0.03 | SD == 0.06) & (as.numeric(level == 4)) ~ "isolated_highV_lowN",
                                                (SD == 0.12) & (as.numeric(level == 4) | as.numeric(level == 5) | as.numeric(level == 6)) ~ "combined_highV_highN", 
                                                (SD == 0.03 | SD == 0.06) & (as.numeric(level == 2) | as.numeric(level == 3))  ~ "stable_lowV_lowN",
                                                (SD == 0.12) & (as.numeric(level == 2) | as.numeric(level == 3)) ~ "isolated_lowV_highN"
                                                )) %>%
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

###############################################################################################       
#create modelling df
################################################################################################
    GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
                            
       
    df_mod<-genpop2 %>%
            group_by(Participant.Private.ID, level, SDhigh) %>%
            mutate(gamers=gamingtime.quant>2) %>% 
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
            summarise(score=mean(trialScore), 
                    meancorrectedL6trialScore=mean(correctedL6trialScore),
                    betaLR=dplyr::first(beta),
                    meanconf=mean(participantConfidence),
                    sumsuccess=sum(success/length(level)),
                    meanPE=mean(PE),
                    meanPEscaled=mean(PEscaled),
                    meanPEsignedScaled=mean(PEsignedScaled),
                    meanPerfE=mean(PerfE),
                    #meanchangeTF=dplyr::first(meanchange),
                    #SDhighcondit=dplyr::first(SDhigh),
                    masq_total_score=dplyr::first(masq_total_score), 
                    gdd_score=dplyr::first(gdd_score),
                    aa_score=dplyr::first(aa_score),
                    gda_score=dplyr::first(gda_score),
                    ad_score=dplyr::first(ad_score),
                    masqCombined_anxiety=sum(aa_score, gda_score),
                    masqCombined_depression=sum(ad_score, gdd_score),
                    pdi_total=dplyr::first(pdi_total),
                    green_total=dplyr::first(green_total),
                    neurologicalImp=dplyr::first(devdx),
                    spq_total=dplyr::first(spq_total),
                    trialName=dplyr::first(trialName),
                    SDhigh=dplyr::first(SDhigh),
                    wideStartSD=dplyr::first(startSD)) #%>%
                    #filter(level %in% c(3, 4, 5, 6)) 

########################################################################################
#make LR difference metrics 
########################################################################################
    
        stable_vs_volatility_df <- df_mod %>%
                        dplyr::select(Participant.Private.ID, level, SDhigh,
                                        betaLR,
                                        masqCombined_anxiety,
                                        green_total,
                                        spq_total,
                                        pdi_total,
                                         masqCombined_depression) %>%
                        group_by(Participant.Private.ID, level, SDhigh) %>%
                        filter(level==4 | level==3 & SDhigh == FALSE) %>%
                        pivot_wider(names_from = level:SDhigh, values_from=betaLR, names_glue = "{.value}_{level}") %>%
                        mutate(LRDIFF_L3stablevsL4 = betaLR_4 - betaLR_3) 

noise_vs_volatility_df <- df_mod %>%
                dplyr::select(Participant.Private.ID, level, SDhigh,
                                betaLR,
                                masqCombined_anxiety,
                                spq_total, pdi_total, 
                                masqCombined_depression,
                                green_total) %>%
                group_by(Participant.Private.ID, level, SDhigh) %>%
                filter(level==4 | level==3 & SDhigh == TRUE) %>%
                pivot_wider(names_from = level:SDhigh, values_from=betaLR, names_glue = "{.value}_{level}") %>%
                mutate(LRDIFF_L3noisevsL4 = betaLR_4 - betaLR_3)



########################################################################################
# Plots - Browning LR diff line plots
########################################################################################

VOl_STA_df <-   df_mod %>%
        dplyr::select(Participant.Private.ID, level, SDhigh,
                        betaLR,
                        masqCombined_anxiety,
                        green_total,
                        spq_total,
                        pdi_total,
                        masqCombined_depression) %>%
        group_by(Participant.Private.ID, level, SDhigh) %>%
        filter(level==4 | level==3 & SDhigh == FALSE) %>%
        mutate(level=recode(level, 
                                "3" ="Stable",
                                "4" = "Volatile")) %>%
        ggplot(aes(x=level, y=betaLR)) +
        geom_point(alpha=0.4) +
        geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
        #facet_wrap(~slope)
        theme_classic() +    
        theme(  axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank()) +
        labs(y="LR")

VOl_STA_df_plot <- VOl_STA_df + stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)


VOL_NOIS_DF <-  df_mod %>%
        dplyr::select(Participant.Private.ID, level, SDhigh,
                        betaLR,
                        masqCombined_anxiety,
                        green_total,
                        spq_total,
                        pdi_total,
                        masqCombined_depression) %>%
        group_by(Participant.Private.ID, level, SDhigh) %>%
        filter(level==4 | level==3 & SDhigh == TRUE) %>%
        mutate(level=recode(level, 
                                "3" ="Noisy",
                                "4" = "Volatile")) %>%
        ggplot(aes(x=level, y=betaLR, group=level)) +
        geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
        geom_point(alpha=0.4) +
        #facet_wrap(~slope)
        theme_classic() +    
        theme(  axis.ticks.x = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.x = element_blank()) +
        labs(y="LR")

VOL_NOIS_DF_plot <- VOL_NOIS_DF + stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)


########################################################################################
# Plots LR diff between levels + correlations with symptom scales + boxplots for clusters
########################################################################################

#########################################
# stable vs volatility - psychosis plots
#########################################

stable_vs_volatility_df$pdi_total<- rescale(stable_vs_volatility_df$pdi_total, to=c(0,1))
stable_vs_volatility_df$spq_total<- rescale(stable_vs_volatility_df$spq_total, to=c(0,1))
stable_vs_volatility_df$green_total<- rescale(stable_vs_volatility_df$green_total, to=c(0,1))

stableVs4_psychosisPlot  <-  stable_vs_volatility_df %>%
                                #mutate(slope = betaLR_4 > betaLR_3) %>%
                                ggplot(aes(y=LRDIFF_L3stablevsL4)) +
                                geom_point(aes(x=spq_total), alpha=0.2, colour=SPQColour) +
                                geom_smooth(aes(x=spq_total), method="lm", colour=SPQColour, se=F) +
                                geom_point(aes(x=pdi_total), alpha=0.2, colour=PDIColour) +
                                geom_smooth(aes(x=pdi_total), method="lm", colour=PDIColour, se=F) +                            
                                geom_point(aes(x=green_total),alpha=0.2, colour=greenColour) +
                                geom_smooth(aes(x=green_total),method="lm", colour=greenColour, se=F) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                        #label.x.npc = 0.5, label.y.npc = 0.9) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                theme_classic() +    
                                theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) + 
                                        #axis.title.x = element_blank())   +  
                                labs(#fill="No. of times\na mean has\nbeen presented",
                                        #color=legend_colors,
                                        y = "LR Difference\n(LR under Volatility - LR under stability)",
                                        x = "Symptom Score")

shapiro.test(stable_vs_volatility_df$pdi_total)
normplot<-ggpubr::ggqqplot(stable_vs_volatility_df$pdi_total)
x<-plot(density(stable_vs_volatility_df$pdi_total))

stable_vs_volatility_df %>% ggplot(aes(x=pdi_total)) + geom_density()

stable_vs_volatility_df %>% ggplot(aes(x=LRDIFF_L3stablevsL4)) + geom_density()

pdicorr <-cor.test(stable_vs_volatility_df$pdi_total, stable_vs_volatility_df$LRDIFF_L3stablevsL4,  method = "spearman")
pdicorr

spqcorr <-cor.test(stable_vs_volatility_df$spq_total, stable_vs_volatility_df$LRDIFF_L3stablevsL4,  method = "spearman")
spqcorr

greencorr <-cor.test(stable_vs_volatility_df$green_total, stable_vs_volatility_df$LRDIFF_L3stablevsL4,  method = "spearman")
greencorr


#########################################
# stable vs volatility - anx/dep plots
#########################################




stable_vs_volatility_df$masqCombined_depression<- rescale(stable_vs_volatility_df$masqCombined_depression, to=c(0,1))
stable_vs_volatility_df$masqCombined_anxiety<- rescale(stable_vs_volatility_df$masqCombined_anxiety, to=c(0,1))

stableVs4_internalisingPlot  <-  stable_vs_volatility_df %>%
                                #mutate(slope = betaLR_4 > betaLR_3) %>%
                                ggplot(aes(y=LRDIFF_L3stablevsL4)) +
                                geom_point(aes(x=masqCombined_anxiety), alpha=0.2, colour=MASQanxColour) +
                                geom_smooth(aes(x=masqCombined_anxiety), method="lm", colour=MASQanxColour, se=F) +
                                geom_point(aes(x=masqCombined_depression), alpha=0.2, colour=MASQdepColour) +
                                geom_smooth(aes(x=masqCombined_depression), method="lm", colour=MASQdepColour, se=F) +                            
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                        #label.x.npc = 0.5, label.y.npc = 0.9) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                theme_classic() +    
                                theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) + 
                                        #axis.title.x = element_blank())   +  
                                labs(#fill="No. of times\na mean has\nbeen presented",
                                        #color=legend_colors,
                                        y = "LR Difference\n(LR under Volatility - LR under stability)",
                                        x = "Symptom Score")

depcorr <-cor.test(stable_vs_volatility_df$masqCombined_depression, stable_vs_volatility_df$LRDIFF_L3stablevsL4,  method = "spearman")
depcorr

anxcorr <-cor.test(stable_vs_volatility_df$masqCombined_anxiety, stable_vs_volatility_df$LRDIFF_L3stablevsL4,  method = "spearman")
anxcorr

########################################
# noise vs volatility - psychosis plots
########################################

noise_vs_volatility_df$pdi_total<- rescale(noise_vs_volatility_df$pdi_total, to=c(0,1))
noise_vs_volatility_df$spq_total<- rescale(noise_vs_volatility_df$spq_total, to=c(0,1))
noise_vs_volatility_df$green_total<- rescale(noise_vs_volatility_df$green_total, to=c(0,1))

noiseVs4_psychosisPlot  <-  noise_vs_volatility_df %>%
                                #mutate(slope = betaLR_4 > betaLR_3) %>%
                                ggplot(aes(y=LRDIFF_L3noisevsL4)) +
                                geom_point(aes(x=spq_total), alpha=0.2, colour=SPQColour) +
                                geom_smooth(aes(x=spq_total), method="lm", colour=SPQColour, se=F) +
                                geom_point(aes(x=pdi_total), alpha=0.2, colour=PDIColour) +
                                geom_smooth(aes(x=pdi_total), method="lm", colour=PDIColour, se=F) +                            
                                geom_point(aes(x=green_total),alpha=0.2, colour=greenColour) +
                                geom_smooth(aes(x=green_total),method="lm", colour=greenColour, se=F) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                        #label.x.npc = 0.5, label.y.npc = 0.9) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                theme_classic() +    
                                theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) + 
                                        #axis.title.x = element_blank())   +  
                                labs(#fill="No. of times\na mean has\nbeen presented",
                                        #color=legend_colors,
                                        y = "LR Difference\n(LR under Volatility - LR under noise)",
                                        x = "Symptom Score")

shapiro.test(noise_vs_volatility_df$pdi_total)
normplot<-ggpubr::ggqqplot(noise_vs_volatility_df$pdi_total)
x<-plot(density(noise_vs_volatility_df$pdi_total))

noise_vs_volatility_df %>% ggplot(aes(x=pdi_total)) + geom_density()

noise_vs_volatility_df %>% ggplot(aes(x=LRDIFF_L3stablevsL4)) + geom_density()

pdicorr <-cor.test(noise_vs_volatility_df$pdi_total, noise_vs_volatility_df$LRDIFF_L3noisevsL4,  method = "spearman")
pdicorr

spqcorr <-cor.test(noise_vs_volatility_df$spq_total, noise_vs_volatility_df$LRDIFF_L3noisevsL4,  method = "spearman")
spqcorr

greencorr <-cor.test(noise_vs_volatility_df$green_total, noise_vs_volatility_df$LRDIFF_L3noisevsL4,  method = "spearman")
greencorr


########################################
# noise vs volatility - anx/dep plots
########################################

noise_vs_volatility_df$masqCombined_anxiety<- rescale(noise_vs_volatility_df$masqCombined_anxiety, to=c(0,1))
noise_vs_volatility_df$masqCombined_depression<- rescale(noise_vs_volatility_df$masqCombined_depression, to=c(0,1))

noiseVs4_anxDepPlot  <-  noise_vs_volatility_df %>%
                                #mutate(slope = betaLR_4 > betaLR_3) %>%
                                ggplot(aes(y=LRDIFF_L3noisevsL4)) +
                                geom_point(aes(x=masqCombined_anxiety), alpha=0.2, colour=MASQanxColour) +
                                geom_smooth(aes(x=masqCombined_anxiety), method="lm", colour=MASQanxColour, se=F) +
                                geom_point(aes(x=masqCombined_depression), alpha=0.2, colour=MASQdepColour) +
                                geom_smooth(aes(x=masqCombined_depression), method="lm", colour=MASQdepColour, se=F) +                            
                                        #label.x.npc = 0.5, label.y.npc = 0.9) +
                                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01)
                                theme_classic() +    
                                theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank()) + 
                                        #axis.title.x = element_blank())   +  
                                labs(#fill="No. of times\na mean has\nbeen presented",
                                        #color=legend_colors,
                                        y = "LR Difference\n(LR under Volatility - LR under noise)",
                                       x = "Symptom Score")


depcorr_NV <-cor.test(noise_vs_volatility_df$masqCombined_depression, noise_vs_volatility_df$LRDIFF_L3noisevsL4,  method = "spearman")
depcorr_NV

anxcorr_NV <-cor.test(noise_vs_volatility_df$masqCombined_anxiety, noise_vs_volatility_df$LRDIFF_L3noisevsL4,  method = "spearman")
anxcorr_NV

########################################################################################
# Final Plots 
########################################################################################

    ggsave(VOL_NOIS_DF_plot, file=file.path(path, "VOL_NOIS_plot.pdf"))
    ggsave(VOl_STA_df_plot, file=file.path(path, "VOl_STA_plot.pdf"))

ggsave(noiseVs4_anxDepPlot, file=file.path(path, "noiseVs4_anxDepPlot.pdf"))
ggsave(noiseVs4_psychosisPlot, file=file.path(path, "noiseVs4_psychosisPlot.pdf"))
ggsave(stableVs4_psychosisPlot, file=file.path(path, "stableVs4_psychosisPlot.pdf"))
ggsave(stableVs4_internalisingPlot, file=file.path(path, "stableVs4_ianxDepPlot.pdf"))
