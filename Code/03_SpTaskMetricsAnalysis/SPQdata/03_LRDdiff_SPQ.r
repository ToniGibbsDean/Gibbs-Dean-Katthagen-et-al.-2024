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
    #options(scipen=999)
     library(sjPlot)
     library(ggpubr)
     library(patchwork)

################################################################################################
# 2. load data and make any additional data cleaning exclusions 
################################################################################################
  #Definite exclusions and mutations 
        #assign to an object; add in your own file structure from your PC
    SPQtibble<-readRDS("Outputs/SPQTibble.RDS") %>%
                    filter(SPQsum != "n/a") %>% #removes 3
                    filter(level!="1") %>%
                    filter(Participant.Device.Type %in% c("computer", "mobile")) %>% #removes 3
                    mutate(spqH=SPQsum>=30)%>%
                    mutate(SDHigh=(SD==0.12)) %>%
                    mutate(level=as.factor(level)) %>%
                    mutate(meanchange=Mean != dplyr::lag(Mean)) %>%
                    mutate(PREmeanchange= lead(Mean != dplyr::lag(Mean))) %>%
                    mutate(POSTmeanchange= lag(Mean != dplyr::lag(Mean))) %>%
                    mutate(Participant.Private.ID=as.factor(Participant.Private.ID)) %>%
                    mutate(highVolatility=level %in% c(4:6)) %>%
                     mutate(correctedL6trialScore = case_when(   level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                            level == 6 & trialScore>0  ~ trialScore/2,
                                            TRUE ~ as.numeric(trialScore))) %>%
                    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
                           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) 
        
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
      spq<- SPQtibble %>%
                        mutate(Ethnicity=as.character(Ethnicity)) %>%
                        mutate(Education=as.character(Education)) %>%
                        mutate(Gender=as.character(Gender)) %>%
                        mutate(Ethnicity=recode(Ethnicity, "6" = "2")) %>% 
                        mutate(Education=recode(Education, "1" = "2", "3" = "4", "5" = "6", "7" = "6"))


###############################################################################################       
#create modelling df
################################################################################################
    GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }
                            
       
     df_mod<-spq %>%
            #mutate(gamers=gamingtime.quant>2) %>% 
            group_by(Participant.Private.ID, level, SDHigh) %>%
            mutate(startSD = trialName %in% c("spacetask012reversed", "spacetask012")) %>%
            mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
            summarise(meanScore=mean(trialScore), 
                    betaLR=dplyr::first(beta),
                    corectedScore=mean(correctedL6trialScore),
                    #meanchange=dplyr::first(meanchange),
                    #PREmeanchange=dplyr::first(PREmeanchange),
                    #POSTmeanchange=dplyr::first(POSTmeanchange),
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
                    highVol=as.factor(dplyr::first(highVolatility)))%>%
                    #highnoise=as.factor(dplyr::first(SDHigh))) %>%
                    filter(level %in% c(2, 3, 4, 5, 6))

########################################################################################
#make LR difference metrics 
########################################################################################
    noise_volatility_df <- df_mod %>%
                dplyr::select(Participant.Private.ID, level, SDHigh, spqH,
                                betaLR, gender, education, ethnicity, age) %>%
                filter(level %in% c(3,4)) %>%
                filter(!level == 3 | !SDHigh == FALSE) 
   
    stable_volatility_df <- df_mod %>%
                dplyr::select(Participant.Private.ID, level, SDHigh, spqH,
                              betaLR, gender, education, ethnicity, age) %>%
                filter(level %in% c(3,4)) %>%
                filter(!level == 3 | !SDHigh == TRUE) 

    block2_block4vol <- df_mod %>%
                dplyr::select(Participant.Private.ID, level, SDHigh, spqH,
                              betaLR, gender, education, ethnicity, age) %>%
                filter(level %in% c(2,4))

########################################################################################
# Plots - Browning LR diff line plots
########################################################################################

browning_L3NoisyvsL4_plot <- noise_volatility_df %>%     
        mutate(test=case_when(level=="3" ~ "3 high noise",
                              level=="4" ~"Volatile block")) %>%
        ggplot(aes(x=test, y=betaLR)) +
        geom_point(alpha=0.4) +
        geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
        #facet_wrap(~slope)
        theme_classic() +    
        theme(  axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())+
        labs(y="Beta LR",
        x="Block",
        colour= "Increase in\ LR")

VOl_NOISE_df_plot_SPQ <- browning_L3NoisyvsL4_plot + stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)

                                                
     browning_L3stablevsL4<-  stable_volatility_df %>%    
                                                mutate(test=case_when(level=="3" ~ "Stable",
                                                                       level=="4" ~"Volatile block")) %>%
                                                     #                 level=="betaLR_3" ~ "3")) %>%
                                                ggplot(aes(x=test, y=betaLR)) +
                                                geom_point(alpha=0.4) +
                                                geom_line(aes(group=Participant.Private.ID), alpha=0.2) +
                                                #facet_wrap(~slope)
                                                theme_classic() +    
                                                theme(  axis.ticks.x = element_blank(),
                                                        axis.ticks.y = element_blank())+
                                                labs(y="Beta LR",
                                                     x="Block",
                                                     colour= "Increase in\ LR")

VOl_STA_df_plot_SPQ <-browning_L3stablevsL4 + stat_summary(fun.data = "mean_cl_boot", colour = "red", linewidth = 2, size = 1)

        mod_blockLRstabletonoise<-lm(betaLR~level,stable_volatility_df)
        summary(mod_blockLRstabletonoise)

        
########################################################################################
# Plots LR diff between levels + boxplots for spq groups
########################################################################################
    ####Level 3 noisy vs level 4
           lrdiff_L4vsL3highnoise  <-  noise_volatility_df %>%
                                    group_by(Participant.Private.ID, spqH, level) %>%
                                    dplyr::select(level, betaLR) %>%
                                    pivot_wider(names_from=level, values_from=betaLR, names_glue = "{.value}_{level}") %>% 
                                    mutate(LRdiff_L3noisyToL4 = betaLR_4 - betaLR_3) 

            lrdiff_L4vsL3highnoise_plot<-lrdiff_L4vsL3highnoise  %>% 
                                        ggplot(aes(x=spqH, y=LRdiff_L3noisyToL4)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_boxplot(aes(fill=spqH)) +
                                        theme_classic()
                                        
            l4vsl3mod <- lm(LRdiff_L3noisyToL4  ~ spqH, lrdiff_L4vsL3highnoise)
            summary(l4vsl3mod)

    ####Level 3 stable vs level 4
            lrdiff_L4vsL3stable  <-  stable_volatility_df %>%
                        group_by(Participant.Private.ID, spqH, level) %>%
                        dplyr::select(level, betaLR) %>%
                        pivot_wider(names_from=level, values_from=betaLR, names_glue = "{.value}_{level}") %>% 
                        mutate(LRdiff_L3stabletoL4 = betaLR_4 - betaLR_3) 

            lrdiff_L4vsL3stable_plot <- lrdiff_L4vsL3stable  %>% 
                                        ggplot(aes(x=spqH, y=LRdiff_L3stabletoL4)) +
                                        #geom_point(aes(color=slope)) +
                                        geom_boxplot(aes(fill=spqH)) +
                                        theme_classic()
                                        
            l4vsl3stablemod <- lm(LRdiff_L3stabletoL4  ~ spqH, lrdiff_L4vsL3stable)
            summary(l4vsl3stablemod)

########################################################################################
# Final Plots 
########################################################################################
    #####plot arrangements
    ggsave(browning_L3NoisyvsL4_plot, filename="Figures/browning_L3NoisyvsL4_plot.pdf")
    ggsave(browning_L3stablevsL4, filename="Figures/browning_L3stablevsL4.pdf")
    ggsave(lrdiff_L4vsL3highnoise_plot, filename="Figures/lrdiff_L4vsL3highnoise_plot.pdf")
    ggsave(lrdiff_L4vsL3stable_plot, filename="Figures/lrdiff_L4vsL3stable_plot.pdf")

    ggsave(VOl_NOISE_df_plot_SPQ, filename="Figures/VOl_NOISE_df_plot_SPQ.pdf")
    ggsave(VOl_STA_df_plot_SPQ, filename="Figures/VOl_STA_df_plot_SPQ.pdf")
    


joinboxplots<-lrdiff_L4vsL3highnoise_plot | lrdiff_L4vsL3stable_plot
 ggsave(joinboxplots, filename="Figures/joinboxplots.pdf")

joinlineplots<-browning_L3NoisyvsL4_plot | browning_L3stablevsL4
 ggsave(joinlineplots, filename="Figures/joinlineplots.pdf")
    #clusters 3
