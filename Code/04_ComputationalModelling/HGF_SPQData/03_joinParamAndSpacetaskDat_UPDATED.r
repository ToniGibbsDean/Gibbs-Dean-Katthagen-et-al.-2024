################################################################################################################################################
# 1. Packages and path to project directory ######
################################################################################################################################################

# set seed and load pacakges
set.seed(0.1)

library(tidyverse)
library(ggplot2)
# library(ggplotify)
library(nlme)
library(lme4)
library(lmerTest)
library(sjPlot)
library(patchwork)

options(scipen = 999)

path <- "Figures"

# functions

removeDirectionalColumnNaming<-function(spqDirectional_df){
    oldnames <- names(spqDirectional_df)
    newnames <- oldnames %>%
    gsub("_f_", "_", .) %>%
    gsub("_r_", "_", .) 
    spqDirectional_df %>% rename_at(vars(all_of(oldnames)), ~ newnames)
}
# load colours


# read in parameters - L5 MAY CHANGE and l6 as s=now doing the error message iterations thing
# these parameters were created using the new SPQ_HGF-creatingInputs_Responses file so I could ensure the ordering

L3_params_F <- read.csv("Data/SPQ/parameters_spq_forward_level3.csv")#3 L3_params_F %>% dplyr::select(L3_f_warning) %>% filter(L3_f_warning==1) %>%  summarise(n())
L4_params_F <- read.csv("Data/SPQ/parameters_spq_forward_level4.csv") #3 L4_params_F %>% dplyr::select(L4_f_warning) %>% filter(L4_f_warning==1) %>%  summarise(n())
L5_params_F <- read.csv("Data/SPQ/parameters_spq_forward_level5.csv")#1 L5_params_F %>% dplyr::select(L5_f_warning) %>% filter(L5_f_warning==1) %>%  summarise(n())
L6_params_F <- read.csv("Data/SPQ/parameters_spq_forward_level6.csv") #0 L6_params_F %>% dplyr::select(L6_f_warning) %>% filter(L6_f_warning==1) %>%  summarise(n())

L3_params_R <- read.csv("Data/SPQ/parameters_spq_reverse_level3.csv") #4 L3_params_R %>% dplyr::select(L3_r_warning) %>% filter(L3_r_warning==1) %>%  summarise(n())
L4_params_R <- read.csv("Data/SPQ/parameters_spq_reverse_level4.csv") #3 L4_params_F %>% dplyr::select(L4_f_warning) %>% filter(L4_f_warning==1) %>%  summarise(n())
L5_params_R <- read.csv("Data/SPQ/parameters_spq_reverse_level5.csv") #3 L5_params_R %>% dplyr::select(L5_r_warning) %>% filter(L5_r_warning==1) %>%  summarise(n())
L6_params_R <- read.csv("Data/SPQ/parameters_spq_reverse_level6.csv") #1 L6_params_R %>% dplyr::select(L6_r_warning) %>% filter(L6_r_warning==1) %>%  summarise(n())


# read in main data frame

spqall <- readRDS("Outputs/SPQTibble.RDS") 

##########################
# get params into simngle joinable dataframe
#########################

Ids<-spqall%>%
    select(trialName) %>%
    group_by(Participant.Private.ID) %>%
    summarise_all(first)

spqForward <- Ids %>% #70
    filter(trialName %in% c("spacetask003", "spacetask012")) %>%
    cbind(L3_params_F, L4_params_F, L5_params_F, L6_params_F) %>%
    filter_at(vars(matches("warning")), all_vars(!.==1)) %>%
    removeDirectionalColumnNaming %>% 
    select(-trialName) %>%
    pivot_longer(cols = !Participant.Private.ID, names_to = c("Level", "param"), names_pattern = c("(.)_(.+)"))  %>% 
    filter(!param %in% c("sub_id", "warning"))  %>%
    pivot_wider(names_from=param, values_from=value) %>%
    mutate(Level=as.numeric(Level))

spqReversed <- Ids %>% #69
    filter(trialName %in% c("spacetask003reversed", "spacetask012reversed"))%>%
    cbind(L3_params_R, L4_params_R, L5_params_R, L6_params_R) %>%
    filter_at(vars(matches("warning")), all_vars(!.==1)) %>%
    removeDirectionalColumnNaming %>%
    select(-trialName) %>%
    pivot_longer(cols = !Participant.Private.ID, names_to = c("Level", "param"), names_pattern = c("(.)_(.+)"))  %>% 
    filter(!param %in% c("sub_id", "warning"))  %>%
    pivot_wider(names_from=param, values_from=value) %>%
    mutate(Level=as.numeric(Level))


param_df<-rbind(spqForward, spqReversed) %>% as_tibble

# join parameters to main dataframe

trialwise_ParamXspaceTdat <- left_join(spqall, param_df, by=c("Participant.Private.ID" , "Level"))


#############################################
#make personiwse df
#############################################
df_mod <- trialwise_ParamXspaceTdat %>%
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
    mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                             level == 6 & trialScore>0  ~ trialScore/2,
                                             TRUE ~ as.numeric(trialScore))) %>%
    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) %>%
    filter(level %in% c(3:6)) %>%
    mutate(level=as.factor(level)) %>%
    group_by(Participant.Private.ID, level) %>% 
    summarise(meanscore=mean(trialScore),
             score=mean(trialScore), 
             meancorrectedL6trialScore=mean(correctedL6trialScore),
             meanconf=mean(participantConfidence),
             meanPE=mean(PE),
             meanPerfE=mean(PerfE),
              trialName = dplyr::first(trialName),
        gender = dplyr::first(Gender),
        age = dplyr::first(Age),
        device_type = dplyr::first(Participant.Device.Type),
        education = dplyr::first(Education),
        spqH = dplyr::first(spqH),
        mux = dplyr::first(mux),
        mua = dplyr::first(mua),
        kax = dplyr::first(kax),
        kaa = dplyr::first(kaa),
        be1 = dplyr::first(be1),
        zem = dplyr::first(zem),
        zes = dplyr::first(zes)
        )     




#save
write.csv(trialwise_ParamXspaceTdat, file = "Outputs/trialwise_ParamXspaceTdat.csv")
write.csv(df_mod, file = "Outputs/personwise_ParamXspaceTdat.csv")
