
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

 reverseinputs_sensecheck<- spqReverse %>%
                #group_by(Participant.Private.ID, level) %>%
                filter(level %in% c(3:6)) %>%
                mutate(scaledResult=scale(originalResult, center = c(-0.5), scale = c(0.01))) %>%
                select(scaledResult) %>% #print(n=219) 
                pull(2)
                #y$trialnumc<-1:219

  inputForward003 %>%
                    filter(level %in% c(3:6)) %>%
                    mutate(scaledResult=scale(result, center = c(-0.5), scale = c(0.01))) %>%
                    select(scaledResult) %>% #print(n=219) 

  reverseinput<-inputReverse003 %>%
                    filter(level %in% c(3:6)) %>%
                    mutate(scaledResult=scale(result, center = c(-0.5), scale = c(0.01))) %>%
                    #select(scaledResult) #%>% #print(n=219) 
                    #filter(level==4) %>%
                    select(scaledResult, level, trialnum, mean, sd)
                   
                    write.table(reverseinput,"Outputs/reverseinput.txt",sep="\t",row.names=FALSE, col.names=FALSE)



 
 
 ######################################
#RESPONSES: trialScore
######################################
 
  trialscoreL3_6_reverse<- spqReverse %>%
                              group_by(Participant.Private.ID) %>%
                              filter(level %in% c(3:6)) %>%
                              select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                              pull(4) 
                              #as.matrix(trialscoreL3_6_reverse)

  trialscoreL3_6_reverse <- matrix(data = trialscoreL3_6_reverse, ncol = 76)

  write.table(trialscoreL3_6_reverse,"Outputs/trialscoreL3_6_reverse.txt",sep="\t",row.names=FALSE, col.names=FALSE)
                      


########################################
#RESPONSES: Participant Position x level
########################################

    reverseResponse_level3<- spqReverse %>%
                                #group_by(Participant.Private.ID, level) %>%
                                filter(level=="3") %>%
                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                pull(2)
                                reverseResponse_level3 <- matrix(data = reverseResponse_level3, ncol = 76)
                                write.table(reverseResponse_level3,"Outputs/reverseResponse_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
    reverseResponse_level4<- spqReverse %>%
                                #group_by(Participant.Private.ID, level) %>%
                                filter(level=="4") %>%
                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                pull(2)
                                reverseResponse_level4 <- matrix(data = reverseResponse_level4, ncol = 76)
                                write.table(reverseResponse_level4,"Outputs/reverseResponse_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
    reverseResponse_level5<- spqReverse %>%
                                #group_by(Participant.Private.ID, level) %>%
                                filter(level=="5") %>%
                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                pull(2)
                                reverseResponse_level5 <- matrix(data = reverseResponse_level5, ncol = 76)
                                write.table(reverseResponse_level5,"Outputs/reverseResponse_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
    reverseResponse_level6<- spqReverse %>%
                                #group_by(Participant.Private.ID, level) %>%
                                filter(level=="6") %>%
                                select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                                pull(2)
                                reverseResponse_level6 <- matrix(data = reverseResponse_level6, ncol = 76)
                                write.table(reverseResponse_level6,"Outputs/reverseResponse_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)



#####pp for overall game
    reverseResponse_wholegame<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            #filter(level=="3") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(2)
                            reverseResponse_wholegame <- matrix(data = reverseResponse_wholegame, ncol = 76)
                            write.table(reverseResponse_wholegame,"Outputs/reverseResponse_wholegame.txt",sep="\t",row.names=FALSE, col.names=FALSE)
    
######################################

reverseResponseBEAM_level3<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            filter(level=="3") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            reverseResponseBEAM_level3 <- matrix(data = reverseResponseBEAM_level3, ncol = 76)
                            write.table(reverseResponseBEAM_level3,"Outputs/reverseResponseBEAM_level3.txt",sep="\t",row.names=FALSE, col.names=FALSE)
reverseResponseBEAM_level4<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            filter(level=="4") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            reverseResponseBEAM_level4 <- matrix(data = reverseResponseBEAM_level4, ncol = 76)
                            write.table(reverseResponseBEAM_level4,"Outputs/reverseResponseBEAM_level4.txt",sep="\t",row.names=FALSE, col.names=FALSE)
reverseResponseBEAM_level5<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            filter(level=="5") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            reverseResponseBEAM_level5 <- matrix(data = reverseResponseBEAM_level5, ncol = 76)
                            write.table(reverseResponseBEAM_level5,"Outputs/reverseResponseBEAM_level5.txt",sep="\t",row.names=FALSE, col.names=FALSE)
reverseResponseBEAM_level6<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            reverseResponseBEAM_level6 <- matrix(data = reverseResponseBEAM_level6, ncol = 76)
                            write.table(reverseResponseBEAM_level6,"Outputs/reverseResponseBEAM_level6.txt",sep="\t",row.names=FALSE, col.names=FALSE)


reverseResponseBEAM_wholegame<- spqReverse %>%
                            #group_by(Participant.Private.ID, level) %>%
                            #filter(level=="6") %>%
                            select(participantPosition, participantConfidence, trialScore, level, TrialNumber) %>%
                            pull(3)
                            reverseResponseBEAM_wholegame <- matrix(data = reverseResponseBEAM_wholegame, ncol = 76)
                            write.table(reverseResponseBEAM_wholegame,"Outputs/reverseResponseBEAM_wholegame.txt",sep="\t",row.names=FALSE, col.names=FALSE)
