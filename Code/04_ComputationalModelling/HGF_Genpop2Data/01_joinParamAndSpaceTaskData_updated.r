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

L3_params_F <- read.csv("Data/Genpop2/parameters_genpop_forward_level3.csv") #3 L3_params_F %>% dplyr::select(L3_f_warning) %>% filter(L3_f_warning==1) %>%  summarise(n())
L4_params_F <- read.csv("Data/Genpop2/parameters_genpop_forward_level4.csv") #15  L4_params_F %>% dplyr::select(L4_f_warning) %>% filter(L4_f_warning==1) %>%  summarise(n())
L5_params_F <- read.csv("Data/Genpop2/parameters_genpop_forward_level5.csv") #10  L5_params_F %>% dplyr::select(L5_f_warning) %>% filter(L5_f_warning==1) %>%  summarise(n())
L6_params_F <- read.csv("Data/Genpop2/parameters_genpop_forward_level6.csv") #5    L6_params_F %>% dplyr::select(L6_f_warning) %>% filter(L6_f_warning==1) %>%  summarise(n())

L3_params_R <- read.csv("Data/Genpop2/parameters_genpop_reverse_level3.csv") #3 L3_params_R %>% dplyr::select(L3_r_warning) %>% filter(L3_r_warning==1) %>%  summarise(n())
L4_params_R <- read.csv("Data/Genpop2/parameters_genpop_reverse_level4.csv") #3  L4_params_R %>% dplyr::select(L4_r_warning) %>% filter(L4_r_warning==1) %>%  summarise(n())
L5_params_R <- read.csv("Data/Genpop2/parameters_genpop_reverse_level5.csv") #1  L5_params_R %>% dplyr::select(L5_r_warning) %>% filter(L5_r_warning==1) %>%  summarise(n())
L6_params_R <- read.csv("Data/Genpop2/parameters_genpop_reverse_level6.csv") #8  L6_params_R %>% dplyr::select(L6_r_warning) %>% filter(L6_r_warning==1) %>%  summarise(n())


# read in main data frame

genpop <- readRDS("Outputs/GenPop2Tibble_new.rds") %>%
                filter(level!="1") %>%
                filter(device_type!="tablet") %>% #only one tablet - so remove
                #filter(devdx.text %in% c("ADHD", "Chronic migraine")) %>% #exclude all expect ADHD and chronic migraine (n10 altogether)
                filter(devdx!="Yes (if you feel comfortable to do so please specify)") %>%
                filter(!Participant.Private.ID=="6639617") #NB - attention check participant removal 

##########################
# get params into simngle joinable dataframe
#########################

Ids <- genpop %>%
    group_by(Participant.Private.ID) %>%
    select(trialName) %>%
    summarise_all(first)

Forward <- Ids %>%
    filter(trialName %in% c("spacetask003", "spacetask012")) %>%
    cbind(L3_params_F, L4_params_F, L5_params_F, L6_params_F) %>%
    filter_at(vars(matches("warning")), all_vars(!.==1)) %>%
    removeDirectionalColumnNaming %>% 
    select(-trialName) %>%
    pivot_longer(cols = !Participant.Private.ID, names_to = c("Level", "param"), names_pattern = c("(.)_(.+)"))  %>% 
    filter(!param %in% c("sub_id", "warning"))  %>%
    pivot_wider(names_from=param, values_from=value) %>%
    mutate(Level=as.numeric(Level))

Reversed <- Ids %>%
    filter(trialName %in% c("spacetask003reversed", "spacetask012reversed"))%>%
    cbind(L3_params_R, L4_params_R, L5_params_R, L6_params_R) %>%
    filter_at(vars(matches("warning")), all_vars(!.==1)) %>%
    removeDirectionalColumnNaming %>%
    select(-trialName) %>%
    pivot_longer(cols = !Participant.Private.ID, names_to = c("Level", "param"), names_pattern = c("(.)_(.+)"))  %>% 
    filter(!param %in% c("sub_id", "warning"))  %>%
    pivot_wider(names_from=param, values_from=value) %>%
    mutate(Level=as.numeric(Level))


param_df<-rbind(Forward, Reversed) %>% as_tibble

# join parameters to main dataframe

trialwise_ParamXspaceTdat <- left_join(genpop, param_df, by=c("Participant.Private.ID" , "Level"))


#############################################
#make personiwse df
#############################################
df_mod <- trialwise_ParamXspaceTdat %>%
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
    mutate(correctedL6trialScore = case_when(level == 6  & trialScore < 0 ~ 0, #correct level 6 scores
                                level == 6 & trialScore>0  ~ trialScore/2,
                                TRUE ~ as.numeric(trialScore))) %>%#
    mutate(level_3split = case_when( level == 3 & SDhigh == TRUE ~ "3highnoise",
                                    level == 3 & SDhigh == FALSE ~ "3lownoise",
                                    level == 4 ~ "4",
                                    level == 5 ~ "5",
                                    level == 6 ~ "6")) %>%
    mutate(forward=trialName %in% c("spacetask003","spacetask012"), 
           reverse=trialName %in% c("spacetask003reversed","spacetask012reversed")) %>%
    filter(level %in% c(3:6)) %>%
    mutate(level=as.factor(level)) %>%
    mutate(gamers=gamingtime.quant>2) %>% 
    group_by(Participant.Private.ID, level) %>% 
    summarise(meanscore=mean(trialScore),
             meancorrectedL6trialScore=mean(correctedL6trialScore),
             meanconf=mean(participantConfidence),
             meanPE=mean(PE),
             meanPerfE=mean(PerfE),
            trialName = dplyr::first(trialName),
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
            gender = dplyr::first(gender),
        age = dplyr::first(age),
        device_type = dplyr::first(Participant.Device.Type),
        education = dplyr::first(education),
        mux = dplyr::first(mux),
        mua = dplyr::first(mua),
        kax = dplyr::first(kax),
        kaa = dplyr::first(kaa),
        be1 = dplyr::first(be1),
        zem = dplyr::first(zem),
        zes = dplyr::first(zes)
        )     




#save
write.csv(trialwise_ParamXspaceTdat, file = "Outputs/trialwise_ParamXspaceTdat_GENPOP2.csv")
write.csv(df_mod, file = "Outputs/personwise_ParamXspaceTdat_GENPOP2.csv")
