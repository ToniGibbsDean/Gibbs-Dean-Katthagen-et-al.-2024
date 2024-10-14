
################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################
    set.seed(0.1)
    
    library(tidyverse)
    library(ggplot2)
   # library(ggplotify)
    library(nlme)
    library(lme4)


  spqForward<-readRDS("Outputs/SPQTibble.RDS" ) %>%
            filter(trialName %in% c('spacetask003', 'spacetask012')) 

  spqReverse<-readRDS("Outputs/SPQTibble.RDS" ) %>%
            filter(trialName %in% c('spacetask003reversed', 'spacetask012reversed')) 


  inputForward003<-read.csv("Data/SPQ/spacetask003.csv")
  inputForward012<-read.csv("Data/SPQ/spacetask012.csv")
  inputReverse003<-read.csv("Data/SPQ/spacetask003reversed.csv")
  inputReverse012<-read.csv("Data/SPQ/spacetask012reversed.csv")



###################
#INPUTS
###################

#1) regenerate the inputs to sense check    
    forwardinputs_sensecheck<- spqForward %>%
                group_by(Participant.Private.ID) %>%
                filter(level %in% c(3:6)) %>%
                mutate(scaledResult=scale(originalResult, center = c(-0.5), scale = c(0.01))) %>%
                select(scaledResult) #%>% print(n=219) 
                #y$trialnumc<-1:219
                #write.table(forwardinputs_sensecheck,"/Users/tonigibbs-dean/Documents/SpaceTaskProject/SPQ/Data/Parameters/forwardinputs_sensecheck.txt",sep="\t",row.names=FALSE, col.names=FALSE)

    p012<-parameters012 %>%
                    group_by(Participant.Private.ID) %>%
                    mutate(scaledResult=scale(result, center = c(-0.5), scale = c(0.01))) %>%
                    select(scaledResult)
                    p012$trialnum<-1:219
                
    p003<-parameters003 %>%
                   group_by(Participant.Private.ID) %>%
                   mutate(scaledResult=scale(result, center = c(-0.5), scale = c(0.01))) %>%
                   select(scaledResult)

    
  forwardInput<-inputForward003 %>%
                    filter(level %in% c(3:6)) %>%
                    mutate(scaledResult=scale(result, center = c(-0.5), scale = c(0.01))) %>%
                    #select(scaledResult) #%>% #print(n=219) 

                    #filter(level==4) %>%
                    select(scaledResult, level, trialnum, mean, sd)
                   
                    write.table(forwardInput,"Outputs/forwardInput.txt",sep="\t",row.names=FALSE, col.names=FALSE)



######################################
#GENERATE RESPONSES: trialScore
######################################


 trialscoreL3_6<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level %in% c(3:6)) %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(4) 
                            #as.matrix(trialscoreL3_6_reverse)

 trialscoreL3_6 <- matrix(data = trialscoreL3_6, ncol = 77)

 write.table(trialscoreL3_6,"Outputs/trialscoreL3_6.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                    
########################################
#RESPONSES: Participant Position x level
########################################

forwardresponse_level3<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="3") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            forwardresponse_level3 <- matrix(data = forwardresponse_level3, ncol = 77)
                            write.table(forwardresponse_level3,"Outputs/forwardresponse_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponse_level4<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="4") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            forwardresponse_level4 <- matrix(data = forwardresponse_level4, ncol = 77)
                            write.table(forwardresponse_level4,"Outputs/forwardresponse_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponse_level5<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="5") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            forwardresponse_level5 <- matrix(data = forwardresponse_level5, ncol = 77)
                            write.table(forwardresponse_level5,"Outputs/forwardresponse_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponse_level6<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            forwardresponse_level6 <- matrix(data = forwardresponse_level6, ncol = 77)
                            write.table(forwardresponse_level6,"Outputs/forwardresponse_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)

############
#whole game
############


forwardresponse_wholegame<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            #filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            forwardresponse_wholegame <- matrix(data = forwardresponse_wholegame, ncol = 77)
                            write.table(forwardresponse_wholegame,"Outputs/forwardresponse_wholegame.txt",sep="\t",row.names=FALSE, col.names=FALSE)



######################################
#RESPONSES: BW x level
######################################

forwardresponseBEAM_level3<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="3") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            forwardresponseBEAM_level3 <- matrix(data = forwardresponseBEAM_level3, ncol = 77)
                            write.table(forwardresponseBEAM_level3,"Outputs/forwardresponseBEAM_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponseBEAM_level4<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="4") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            forwardresponseBEAM_level4 <- matrix(data = forwardresponseBEAM_level4, ncol = 77)
                            write.table(forwardresponseBEAM_level4,"Outputs/forwardresponseBEAM_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponseBEAM_level5<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="5") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            forwardresponseBEAM_level5 <- matrix(data = forwardresponseBEAM_level5, ncol = 77)
                            write.table(forwardresponseBEAM_level5,"Outputs/forwardresponseBEAM_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
forwardresponseBEAM_level6<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            forwardresponseBEAM_level6 <- matrix(data = forwardresponseBEAM_level6, ncol = 77)
                            write.table(forwardresponseBEAM_level6,"Outputs/forwardresponseBEAM_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)

############
#whole game
############

  forwardresponseBEAM_wholegame<- spqForward %>%
                            group_by(Participant.Private.ID) %>%
                            filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            forwardresponseBEAM_wholegame <- matrix(data = forwardresponseBEAM_wholegame, ncol = 77)
                            write.table(forwardresponseBEAM_wholegame,"Outputs/forwardresponseBEAM_wholegame.txt",sep="\t",row.names=FALSE, col.names=FALSE)
