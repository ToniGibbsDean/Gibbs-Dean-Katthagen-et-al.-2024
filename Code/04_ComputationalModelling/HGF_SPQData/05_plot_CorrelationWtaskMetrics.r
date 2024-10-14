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
library(ggpubr)

options(scipen = 999)

path <- "Figures"

trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat.csv") %>% as_tibble
personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat.csv") %>% as_tibble %>% mutate(level=as.factor(level))


################################################################################################################################################
#be1 corr plots with task metrics 
################################################################################################################################################

makeCorrPlots<-function(metric, p) {

plot<-personwise_ParamXspaceTdat %>%
    ggplot(aes(x=metric, y=p)) +
    geom_point()+
    geom_smooth(method=lm) +
    #facet_wrap(~level) +
    stat_cor(method = "spearman",p.accuracy = 0.001, r.accuracy = 0.01, 
                label.x.npc = 0.3, label.y.npc = 0.1, size=2) 

    return(plot)
}

be1Score<-makeCorrPlots(personwise_ParamXspaceTdat$meancorrectedL6trialScore, personwise_ParamXspaceTdat$be1)
be1perfE<-makeCorrPlots(personwise_ParamXspaceTdat$meanPerfE, personwise_ParamXspaceTdat$be1)

kaaScore<-makeCorrPlots(personwise_ParamXspaceTdat$meancorrectedL6trialScore, personwise_ParamXspaceTdat$kaa)
kaaperfE<-makeCorrPlots(personwise_ParamXspaceTdat$meanPerfE, personwise_ParamXspaceTdat$kaa)

kaxScore<-makeCorrPlots(personwise_ParamXspaceTdat$meancorrectedL6trialScore, personwise_ParamXspaceTdat$kax)
kaxperfE<-makeCorrPlots(personwise_ParamXspaceTdat$meanPerfE, personwise_ParamXspaceTdat$kax)

ggsave(be1Score, file="Figures/SPQ_be1Xscore_Corr.pdf")
ggsave(kaaScore, file="Figures/SPQ_kaaXscore_Corr.pdf")
ggsave(kaxScore, file="Figures/SPQ_kaxXscore_Corr.pdf")