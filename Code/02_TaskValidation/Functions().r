getBackgroundShading_noise<-function(direction){
                                    if(direction=="forward") {
                                                            backgroundshade<-data.frame( ymin = -Inf, 
                                                            ymax = Inf,
                                                            xmin = c(4,14,34,105, 115,  147, 178,199),
                                                            xmax = c(13,33,104,114, 146, 177, 198, Inf),
                                                            SD = c("Low SD","High SD", "Low SD", "High SD","Low SD", "High SD","Low SD", "High SD"),
                                                            fill=rep(c(Comp2, Comp1),4))

                                    } else if(direction=="reverse") {
                                                            break
                                                            backgroundshade<-data.frame( ymin = -Inf, 
                                                            ymax = Inf,
                                                            xmin = c(4,14,34,105, 115,  147, 178,199),
                                                            xmax = c(13,33,104,114, 146, 177, 198, Inf),
                                                            SD = c("Low SD","High SD", "Low SD", "High SD","Low SD", "High SD","Low SD", "High SD"),
                                                            rep(c(Comp2, Comp1),4))
                                    } else {break}

                                    return(backgroundshade)
                                    }
                                    
Cumulative_MeanTrack<-function(dataset){

            cumulativemean_contingencyplot<-dataset %>%
                                                filter(forward==TRUE) %>%
                                                #filter(!level=="2") %>%
                                                group_by(TrialNumber) %>% 
                                                summarise(meanPP=mean(participantPosition), 
                                                          originalResult=mean(originalResult), 
                                                          Mean=dplyr::first(Mean)) %>%
                                                mutate(originalResult=(originalResult+0.5)*100) %>%
                                                mutate(Mean=(Mean+0.5)*100) %>%
                                                mutate(cumMean10 = zoo::rollapplyr(originalResult, width = 10, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean15 = zoo::rollapplyr(originalResult, width = 15, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean5 = zoo::rollapplyr(originalResult, width = 5, FUN = mean, partial = TRUE)) %>%
                                                mutate(cumMean3 = zoo::rollapplyr(originalResult, width = 3, FUN = mean, partial = TRUE)) %>%

                                    #longdf<-pivot_longer(cumulativemean_contingencyplot) %>% 
                                                ggplot(aes(x=TrialNumber)) +
                                                    #geom_line(aes( y= cumMean5), color="orange") + 
                                                    geom_line(aes( y= cumMean3), color=L2Col, alpha=0.4) +                                         
                                                    geom_line(aes(y=meanPP), color="grey") +  
                                                    geom_line(aes(y=Mean), linetype="dotted", color="black")+
                                                    #geom_line(data=plotdata_PPs, aes(x=TrialNumber, y=meanPP)) +  
                                                    #geom_smooth() + 
                                                    #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                    #geom_vline(xintercept=c(54, 94, 178), colour="grey") +
                                                    #geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="grey") #+
                                                    #geom_vline(xintercept = meanchange==TRUE, colour="grey") #+
                                                         theme_classic() +    
                                                    theme(axis.ticks.x = element_blank(),
                                                          axis.ticks.y = element_blank(),
                                                          axis.title.x = element_blank())+
                                                          #legend.box.margin = margin(6, 6, 6, 6),
                                                          #legend.position='bottom')+
                                                    labs(#fill="No. of times\na mean has\nbeen presented",
                                                                    #color=legend_colors,
                                                                    y = "Rover Position")
                                                                   # x = "Trial Number")# +
                                                    #scale_color_manual(values=legend_colors) #+
                                                     #guides(color=guide_legend(nrow=2, byrow=TRUE)) 
                                               
                                                                                        
                   # ggplot() + 
                   # geom_line(data = mtcars_list[[1]], aes(x = wt, y = mpg, color = "4 Cyl")) +
                   # geom_line(data = mtcars_list[[2]], aes(x = wt, y = mpg, color = "6 Cyl")) + 
                  #  labs(color = "the legend") + 
                   # scale_color_manual(values = legend_colors) + 
                   # theme_bw()
                                                                                #LearningSDPlot<-LearningSDPlot + scale_fill_manual(values=c(Comp1, Comp6))
                        

                                return(cumulativemean_contingencyplot)
                    }


SDTrack<-function(dataset, direction){  

                if (direction == "forward") {
                    forward<-TRUE
                    }else if (direction == "reverse") {
                        forward<-FALSE
                        }else {break}

                            conf_noiseXwholegameXlevel<-dataset %>%
                                                    mutate(pid=as.factor(Participant.Private.ID)) %>%
                                                    filter(forward==forward ) %>% 
                                                    #filter(!level=="2") %>%
                                                    group_by(TrialNumber) %>%
                                                    summarise(meanBW=mean(110-participantConfidence),
                                                              originalResult=mean(originalResult),
                                                              SD=dplyr::first(SD),
                                                              SDHigh=dplyr::first(SDHigh)) %>%
                                                    mutate(cumnoise15 = zoo::rollapplyr(originalResult, width = 15, FUN = sd, partial = TRUE)) %>%
                                                    mutate(cumnoise5 = zoo::rollapplyr(originalResult, width = 10, FUN = sd, partial = TRUE)) %>%
                                                    #mutate(cumnoise20 = zoo::rollapplyr(originalResult, width = 20, FUN = sd, partial = TRUE)) %>%
                                                    ggplot(aes(x=TrialNumber, y=meanBW)) +
                                                        geom_line(aes( y= cumnoise15*100), color=L6Col, alpha=0.8) +
                                                        geom_line(aes( y= cumnoise5*100), color=L4Col, alpha=0.8) +
                                                        geom_line(aes(y=meanBW), colour="black", alpha=0.9) + 
                                                        geom_rect(data = getBackgroundShading_noise(direction), 
                                                                  aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), 
                                                                  inherit.aes = FALSE, 
                                                                  alpha = 0.2)+
                                                        scale_fill_manual(values = unique(getBackgroundShading_noise(direction)$fill), 
                                                            labels = unique(getBackgroundShading_noise(direction)$SD))+
                                                        geom_vline(xintercept=c(54, 94, 178), colour="black", linetype="dashed") +
                                                    theme_classic() +    
                                                    theme(  axis.ticks.x = element_blank(),
                                                            axis.ticks.y = element_blank(),
                                                            legend.position = "none") +
                                                            #axis.title.x=element_blank(),
                                                            #axis.text.x = element_blank(),
                                                            #axis.text.y = element_blank(),
                                                            #axis.title.y=element_blank()) +
                                                    labs(y="Beam Width",
                                                         x="Trial Number")
                                                         #fill= "Noise\nCondition" )


        return(conf_noiseXwholegameXlevel) 
        
        }


residplot_SDconditions_beamwidths<-function(dataset){
        

        modelshowingBW_SDcondi<-
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        
                                        lmer(meanBW ~  SDHigh + wideSDstart + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        BW_SDcondit_modresult<-summary(modelshowingBW_SDcondi)
        
        beammod_noSDcondit<- 
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        lmer(meanBW ~  wideSDstart + 
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        dfforplot<-dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanBW=mean(110-participantConfidence),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) 
        
        dfforplot$residual_beamxSDcondit<-resid(beammod_noSDcondit)

          LearningSDPlot<-   dfforplot %>%
                                   # group_by(SDHigh) %>%
                                    filter(!level %in% c(2,4)) %>%
                                    mutate(SD=case_when(SDHigh==FALSE ~ "Low Noise", SDHigh==TRUE ~ "High Noise")) %>%                
                                    ggplot(aes(y=residual_beamxSDcondit, x=SD,fill=as.factor(SD))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.position="none") +
                                                  #legend.box.background = element_rect(),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    #fill = "Noise Condition",
                                                    x= "Blocks",
                                                    y= "Space Task Beam Width\n(residual unexplained by covariates)")
                                    LearningSDPlot<-LearningSDPlot + scale_fill_manual(values=c(Comp1, Comp6))

            #return(list("modoutput"=BW_SDcondit_modresult,"plot"=LearningSDPlot))
            return(LearningSDPlot)
    }

meanCountPlot<-function(dataset) { #level,
 # make mean counting columns
        meantimeseen<-c()
        meancount<-c()
        perps<-unique(dataset$Participant.Private.ID)
        for (i in 1:length(perps)) {
                perpid<-perps[i]
                df_perp<-dataset[dataset$Participant.Private.ID==perpid,]
                lengthsofruns<-rle(as.character(df_perp$Mean))$lengths
                perp_meancount<-sequence(lengthsofruns)
                                
                perpmeantimeseen<-c()
                counter<-1
                for (i in 1:length(lengthsofruns)) {
                    perpmeantimeseen_new<-rep(counter, lengthsofruns[i])
                    perpmeantimeseen<-c(perpmeantimeseen, perpmeantimeseen_new)
                    if((i %% 2) == 0) { counter<-counter+1}
                }
                meancount<-c(meancount, perp_meancount)
                perpmeantimeseen[1]<-1 # accoutn for the very first mean havign alreay been seen in the set up for the level
                meantimeseen<-c(meantimeseen, perpmeantimeseen)

        }

        dataset$meancount<-meancount
        dataset[["meanTimesSeen"]]<-as.factor(meantimeseen)


                #make plots

                PEplot<- dataset %>% # select(meancount, meanTimesSeen)
                        ggplot(aes(x=as.factor(meancount), y=PE)) +
                                    geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                                    geom_smooth() +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        axis.text.x = element_blank(),
                                        axis.title.x=element_blank(),
                                        legend.justification = c("right", "top"),
                                        legend.position = c(.99, .99),
                                        legend.box.background = element_rect(),
                                        legend.key = element_rect(fill = "white", colour = "black"),
                                        legend.box.margin = margin(6, 6, 6, 6),
                                        legend.title = element_text(size = 10, colour = "black"),
                                        legend.text = element_text(size = 8, colour = "black")) +
                                    labs(fill="No. Times\nMean Seen",
                                        # x = "No. times mean seen",
                                        y = "Average Prediction Error") 
                        PEFinalPLot<-PEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2))
                            

                PerfEplot<- dataset %>% # select(meancount, meanTimesSeen)
                                    ggplot(aes(x=as.factor(meancount), y=PerfE)) +
                                    geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                                    geom_smooth() +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        legend.justification = c("right", "top"),
                                        legend.position = "none") +
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="No. Times Mean was Seen",
                                        x = "No. Trials From Mean Change",
                                        y = "Average Performance Error") 
                        PEFinalPLot<-PEplot+ scale_fill_manual(values=c(Comp3, Comp4, Comp2))


                #meancountplots_PE_perfE<-egg::ggarrange(PEplot,PerfEplot)
                #meancountplots_LR_LRvio<-egg::ggarrange(LRcoefsplot,correctedLR_violin)

            #return(meancountplots_PE_perfE, meancountplots_LR_LRvio)
            #return(meancountplots_PE_perfE)
            return(PerfEplot)

            }

meancount_LR_violins<-function(dataset, LRtype) {

                    if (LRtype == "Nassar") {
                                        #do nothing
                    }else if (LRtype == "Corrected") {
                                        dataset$LR = dataset$correctedLR 
                    }else if (LRtype == "Excluded") {
                                        dataset <- dataset %>% filter(LR<1 & LR>0)  #%>%dplyr::select(LR) %>% summary
                    }else {break}

        
        meantimeseen<-c()
        meancount<-c()
        perps<-unique(dataset$Participant.Private.ID)
        for (i in 1:length(perps)) {
                perpid<-perps[i]
                df_perp<-dataset[dataset$Participant.Private.ID==perpid,]
                lengthsofruns<-rle(as.character(df_perp$Mean))$lengths
                perp_meancount<-sequence(lengthsofruns)
                                
                perpmeantimeseen<-c()
                counter<-1
                for (i in 1:length(lengthsofruns)) {
                    perpmeantimeseen_new<-rep(counter, lengthsofruns[i])
                    perpmeantimeseen<-c(perpmeantimeseen, perpmeantimeseen_new)
                    if((i %% 2) == 0) { counter<-counter+1}
                }
                meancount<-c(meancount, perp_meancount)
                perpmeantimeseen[1]<-1 # accoutn for the very first mean havign alreay been seen in the set up for the level
                meantimeseen<-c(meantimeseen, perpmeantimeseen)

        }

        dataset$meancount<-meancount
        dataset[["meanTimesSeen"]]<-as.factor(meantimeseen)

        LRcoefsplot<- dataset %>% # select(meancount, meanTimesSeen)
                        filter(level==4) %>%
                        filter(meanTimesSeen==1) %>%
                        ggplot(aes(x=as.factor(meancount), y=LR)) +
                        geom_boxplot(outlier.shape=NA, aes(fill=as.factor(meanTimesSeen)))  +
                        geom_smooth() +
                        theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        #axis.text.x = element_blank(),
                                        axis.title.x = element_blank(),
                                        #legend.justification = c("right", "top"),
                                        legend.position = "none")+
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="Mean label",
                                        # x = "No. times mean seen",
                                        y = paste0(LRtype, " LR")) 
                        LRcoefsplot<-LRcoefsplot+ scale_fill_manual(values=c(Comp2))


                correctedLR_violin<-dataset %>% # select(meancount, meanTimesSeen)
                                    filter(level==4) %>%
                                    filter(meanTimesSeen==1) %>%
                                    mutate(as.factor(meancount)) %>%
                                    ggplot(aes(factor(meancount), LR)) +
                                    geom_violin(aes(fill=as.factor(meanTimesSeen)))  +
                                    theme_classic() +
                                    theme(axis.ticks.x = element_blank(),
                                          axis.ticks.y = element_blank(),
                                          #axis.text.x = element_blank(),
                                        #legend.justification = c("right", "top"),
                                        legend.position = "none") +
                                        #legend.box.background = element_rect(),
                                        #legend.key = element_rect(fill = "white", colour = "black"),
                                        #legend.box.margin = margin(6, 6, 6, 6),
                                        #legend.title = element_text(size = 10, colour = "black"),
                                        #legend.text = element_text(size = 8, colour = "black")) +
                                    labs(#fill="Mean label",
                                        x = "No. trials after a mean change",
                                        y = paste0(LRtype, " LR")) 
                        correctedLR_violin<-correctedLR_violin+ scale_fill_manual(values=c(Comp4))

    return(list("meancountplot"=LRcoefsplot, "violinplot"=correctedLR_violin))
}

LR_meanchange<-function(dataset,LRtype) { #direction can be "forward" or "reverse"

                    if (LRtype == "Nassar") {
                                        #do nothing
                    }else if (LRtype == "Corrected") {
                                        dataset$LR = dataset$correctedLR 
                    }else if (LRtype == "Excluded") {
                                        dataset <- dataset %>% filter(LR<1 & LR>0)  #%>%dplyr::select(LR) %>% summary
                    }else {break}


        meanchange_LR_vol<-dataset %>%
                                            group_by(TrialNumber) %>%
                                            filter(forward==TRUE ) %>% 
                                            summarise(LRmeanTrialwise=mean(LR)) %>%
                                            #filter(c(LR<1 & LR>0)) %>%
                                            #filter(!level %in% c(2,3)) %>%
                                            #filter(!level=="2") %>%
                                            ggplot(aes(x=TrialNumber, y=(LRmeanTrialwise))) +    
                                            geom_line(color="orange") +  
                                            theme_classic() +                
                                            theme(axis.ticks.x = element_blank(),
                                                axis.ticks.y = element_blank(),
                                                axis.title.x=element_blank())+
                                                #axis.text.x = element_blank(),
                                                #axis.text.y = element_blank(),
                                                #axis.title.y=element_blank()) +
                                                #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                geom_vline(xintercept=c(94, 178), colour="black", linetype="dashed") +
                                                geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="dark grey", linetype='dotted') +
                                                geom_rect(data = getBackgroundShading_noise(direction), aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), inherit.aes = FALSE, alpha = 0.15) +
                                                scale_fill_manual(values = unique(getBackgroundShading_noise(direction)$fill), 
                                                                  labels = unique(getBackgroundShading_noise(direction)$SD)) +
                                                labs( y = paste0(LRtype, " LR"),
                                                    fill="Noise Condition")


        return(meanchange_LR_vol)
}
                  
BernikerHyp1_2<-function(dataset, taskversion) {

                    if (taskversion == "1") {
                                        dataset$Hint = scale(dataset$Hint) 
                    }else if (taskversion == "2") {
                                        dataset$Hint = lag(dataset$Result)
                    }else {break}


    BernikerHyp1<- dataset %>%
                #mutate(Hint=taskversion) %>%
                group_by(SDHigh) %>%
                filter(!participantPosition<0) %>%
                filter(level==3) %>%
                ggplot(aes(x=scale(participantPosition), y=Hint, colour=SDHigh)) +
                geom_point(alpha=0.1)+
                geom_smooth(method="lm")+
                geom_line(aes(y=Mean), colour="blue", linetype="dotted", alpha=0.5) +
                #stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                #                                label.x.npc = 0.5, label.y.npc = 0.9) +
                #geom_smooth(aes(y=scale(meanHintlagResult)), colour="blue", method="lm", linetype="dotted") +
                #geom_smooth(aes(y=scale(lagResult)), colour="purple", method="lm") +
                #geom_smooth(aes(y=scale(Mean), colour="pink", method="lm"))+
                #facet_wrap(~level) +
                theme_classic() +
                theme(axis.ticks.x = element_blank(),
                      axis.ticks.y = element_blank()) +
                labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    #fill = "Noise Condition",
                    x= "Participant Position",
                    y= "Input A (cue)")
    BernikerHyp1<-BernikerHyp1 + scale_color_manual(values=c(Comp6, Comp1))

        corr_lownoise_Dat<-   dataset %>%   group_by(SDHigh)%>%
                            filter(SDHigh==FALSE) %>%
                            filter(!participantPosition<0) %>%
                            filter(level==3) 
          corr_lownoise<-                  cor.test(corr_lownoise_Dat$participantPosition, corr_lownoise_Dat$Hint)

         corr_highnoise_Dat<-   dataset %>%   group_by(SDHigh)%>%
                            filter(SDHigh==TRUE) %>%
                            filter(level==3) 
         corr_highnoise<-                   cor.test(corr_highnoise_Dat$participantPosition, corr_highnoise_Dat$Hint)




  


    BHyp2mod<-
                                dataset %>%  
                                        group_by(Participant.Private.ID, level, SDHigh) %>%
                                        summarise(meanPerfE=mean(PerfE),
                                                  wideSDstart=dplyr::first(startSDHigh),
                                                  trialName=dplyr::first(trialName)) %>%
                                        
                                        lmer(meanPerfE ~  SDHigh +
                                        (1|Participant.Private.ID) + (1|trialName/level), data=., REML=F,
                                                                        control=lme4::lmerControl(optimizer="bobyqa", 
                                                                        optCtrl=list(maxfun=2e7)) )
        Hyp2modelshowingPerfE_SDcondi<-summary(BHyp2mod)
        
        BernikerHyp2_SDxperfE<-   dataset %>%
                                   # group_by(SDHigh) %>%
                                    #filter(!level %in% c(2,4)) %>%
                                       group_by(Participant.Private.ID, SDHigh) %>%
                                       mutate(SD=case_when(SDHigh==FALSE ~ "Low Noise", SDHigh==TRUE ~ "High Noise")) %>%   
                                       summarise(meanPerfE=mean(PerfE), SD=dplyr::first(SD)) %>%     
                                    ggplot(aes(y=meanPerfE, x=SD,fill=as.factor(SD))) +
                                            geom_boxplot(outlier.size=1, outlier.color="darkgrey") +
                                            theme_classic() +
                                            theme(axis.ticks.x = element_blank(),
                                                  axis.ticks.y = element_blank(),
                                                  legend.position = "none") +
                                                  #legend.box.background = element_rect(),
                                                  #legend.box.margin = margin(6, 6, 6, 6),
                                                  #legend.key = element_rect(fill = "white", colour = "black"),
                                                  #legend.title = element_text(size = 10, colour = "black"),
                                                  #legend.text = element_text(size = 8, colour = "black"),
                                                  #legend.justification = c("right", "center"))+
                                                  #legend.position = c(.95, .95)) +
                                            labs(   #title="Use of Beam by SD condition (wide or narrow), across blocks", 
                                                    #caption = "Figure made using residuals from mixed model",
                                                    fill = "Noise Condition",
                                                    x= "Noise Condition",
                                                    y= "Average Performance Error")
        BernikerHyp2_SDxperfE<-BernikerHyp2_SDxperfE + scale_fill_manual(values=c(Comp1, Comp6))


        return(list("plot1"=BernikerHyp1, "modoutput"=Hyp2modelshowingPerfE_SDcondi,"plot2"=BernikerHyp2_SDxperfE, "corr_lownoise"=corr_lownoise, "corr_highnoise"=corr_highnoise))    
          
  }

PE_PerfE_MeanChange<-function(dataset) {

                                conf_volXwholegame<-dataset %>%
                                                            group_by(TrialNumber) %>%
                                                            filter(forward==TRUE ) %>% 
                                                            summarise(meanPerfE=mean(PerfE), meanPE=mean(PE)) %>%
                                                            #filter(!level=="2") %>%
                                                            ggplot(aes(x=TrialNumber, y=PE)) +    
                                                                geom_line(aes(x=TrialNumber, y=meanPerfE), color=Comp4) +  
                                                                geom_line(aes(x=TrialNumber, y=meanPE), color=Comp1) +  
                                                                ylim(0,80) +
                                                                #geom_smooth() + 
                                                                #geom_bar(aes(fill=highVolatility), position="fill", stat="identity", width=1, alpha=0.1) +
                                                                geom_vline(xintercept=c(14, 54, 94, 178), colour="black", linetype="dashed") +
                                                                geom_vline(xintercept=c(54, 64, 74, 82, 93, 94, 105, 115, 125, 135, 146, 167, 178, 188, 209), colour="dark grey", linetype='dotted') +
                                                                theme_classic() +
                                                                theme(axis.ticks.x = element_blank(),
                                                                              axis.ticks.y = element_blank())+
                                                                              #axis.title.x=element_blank(),
                                                                              #axis.text.x = element_blank(),
                                                                              #axis.text.y = element_blank(),
                                                                              #axis.title.y=element_blank()) +
                                                                labs(x="Trial Number",
                                                                     y="Prediction Error")

            return(conf_volXwholegame)                                                
            
            }
