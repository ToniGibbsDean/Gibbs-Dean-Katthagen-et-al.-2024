
#SPACE TASK
## loading libraries you may want to use in your work
library(plyr)
library(tidyverse)
library(dplyr)

## loading your data
path<-"Data"

#parameters
CorrectGameLength<-216 # no level 1

# load data
pfiles<-list.files(file.path(path, "Inperson/Parameters"))
tfiles<-list.files(file.path(path, "Inperson/TaskData"))

p<-lapply(file.path(path, "Inperson/Parameters", pfiles), read.csv, header=TRUE)
names(p)<-substr(pfiles, start=1, stop=nchar(pfiles)-4)

t<-lapply(file.path(path, "Inperson/TaskData", tfiles), read.csv)
names(t)<-tfiles

#merge task datas
t<-rbind(t[[1]])

#for all poeple with correct game length append columns with their parameter data
IDs<-unique(t$Participant.Private.ID)
df<-c()
for (ID in IDs) {
  PersonsTaskData<-t[t$Participant.Private.ID==ID,] %>%
    filter(!is.na(level))
  
  gameLength<-dim(PersonsTaskData)[1]
  if (gameLength==CorrectGameLength) {
    if(length(unique(PersonsTaskData$trialName))==1) {
      PersonParameters<-p[names(p)==unique(PersonsTaskData$trialName)] [[1]]%>% filter(level!=1)
      PersonFullData<-cbind(PersonsTaskData, PersonParameters)
      names(PersonFullData)<-c(names(PersonsTaskData), c("Mean", "SD", "Hint", "Result", "Level", "TrialNumber")) #rename columns to avoid renaming by spreadsheet
    } else {print("ERROR - multiple spreadsheets used by one person")}
    df<-rbind(df,PersonFullData)
  }
}

###demo data
demo_quest <- read.csv("Data/Inperson/demo_questionnaires.csv",",", header = T, stringsAsFactors = FALSE) 
df_demo <- left_join(df, demo_quest, by = c("Participant.Private.ID"))


tibble<-as_tibble(df_demo)

# tibble processing - data cleaning and column tidying

#exclusions
#record exclusions based on incomplete or repeated games
excludeRepeatOrIncomplete <- length(IDs) -length(unique(tibble$Participant.Private.ID))

#exclude based on trial performance and symptom scale attention
#get table of all reasons for exclusion for each participant
ExclusionBasis<-tibble %>%
  group_by(Participant.Private.ID) %>%
  reframe(  nomoveX=sum(distanceMovedX==0), 
              nomoveY=sum(distanceMovedY==0), 
              nomoveXY=sum(distanceMovedX==0 & distanceMovedY==0), 
              overtime=sum(timeToComplete>5000), 
              undertime=sum(timeToComplete<1000), 
              zerotime=sum(timeToComplete==0)) %>%
  reframe(  Participant.Private.ID=Participant.Private.ID, 
              excludeX= nomoveX>108, 
              excludeY=nomoveY>108, 
              excludeXY= nomoveXY >21, 
              excludeOvertime= overtime >21, 
              excludeUndertime= undertime >21) #, excludeZerotime= zerotime >0)
#extract indices of participants to exclude
IndicesToExclude<-ExclusionBasis %>%
  select(starts_with("exclude")) %>%
  rowSums()>0
IdsToExclude<-ExclusionBasis$Participant.Private.ID[IndicesToExclude]

#create exclusions tracking table      
track<-c(excludeRepeatOrIncomplete=excludeRepeatOrIncomplete,
         colSums(ExclusionBasis[,2:6]), 
         TotalExclusions=sum(IndicesToExclude) +  excludeRepeatOrIncomplete )

#apply exclusions
tibble<-tibble[!tibble$Participant.Private.ID %in% IdsToExclude,]

#column tidying
# column removal and naming
tibble<-tibble%>%
  select(Event.Index, Participant.Private.ID, originalHint:disorganised_score)
  
# column na management
tibble$success[is.na(tibble$success)]<-0
tibble$tutorialRepeated[is.na(tibble$tutorialRepeated)]<-0

#column additons
tibble$PE<-abs(scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition)
tibble$signedPE<-scale(tibble$originalResult, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition

tibble$PerfE<-abs(scale(tibble$Mean, center = c(-0.5), scale = c(0.01))- tibble$participantPosition)
tibble$signedPerfE<-scale(tibble$Mean, center = c(-0.5), scale = c(0.01)) - tibble$participantPosition


#save output
write.csv(file=file.path("Outputs", "TrackingOnCampus.csv"), track)
saveRDS(file=file.path("Outputs", "OnCampusTibble.RDS"), tibble)
