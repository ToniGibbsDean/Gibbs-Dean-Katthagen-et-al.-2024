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
library(Hmisc)


options(scipen = 999)

path <- "Figures"

personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat_GENPOP2.csv") %>% as_tibble %>% mutate(level=as.factor(level))
trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat_GENPOP2.csv") %>% as_tibble


########################################################################################################################################################################
#kaa correaltions and corr plots
########################################################################################################################
kaaDf<-  
    personwise_ParamXspaceTdat %>%
    #filter(!level==4) %>%
    filter(!kaa>25) %>%
    dplyr::select(kaa, 
                  meanPE, 
                  meanPerfE, 
                  meanconf, 
                  meancorrectedL6trialScore,
                  level,
                  pdi_total,
                  spq_total,
                  green_total)

kaaDf_trialwise<-  
    trialwiseJoinedDat %>%
    #filter(!level==4) %>%
    filter(!kax>25) %>%
    dplyr::select(kax, 
                  PE, 
                  PerfE, 
                  participantConfidence, 
                  trialScore,
                  level,Participant.Private.ID, LR)
    
kaaMatDf_levelwise<- kaaDf %>% 
  dplyr::select(kaa, meanPE, meanPerfE, meanconf, meancorrectedL6trialScore, level) %>% 
  pivot_wider(names_from = level, values_from = kaa)

kaaMatDfTRIALWISE_levelwise<- kaaDf_trialwise %>% group_by(Participant.Private.ID) %>%
  dplyr::select(kax, 
                  PE, 
                  PerfE, 
                  participantConfidence, 
                  trialScore,
                  level, 
                  LR) %>% 
  pivot_wider(names_from = level, values_from = kax)

res2_levelwise_kaa <- rcorr(as.matrix(kaaMatDf_levelwise))
# Extract the correlation coefficients
res2_levelwise_kaa$r
# Extract p-values
res2_levelwise_kaa$P

res2_levelwise_trialwise_kaa <- rcorr(as.matrix(kaaMatDfTRIALWISE_levelwise))
# Extract the correlation coefficients
res2_levelwise_trialwise_kaa$r
# Extract p-values
res2_levelwise_trialwise_kaa$P


res2_overall_kaa <- rcorr(as.matrix(kaaDf))
# Extract the correlation coefficients
res2_overall_kaa$r
# Extract p-values
res2_overall_kaa$P

res2_overall_kaa_TRIALWISE <- rcorr(as.matrix(kaaDf_trialwise))
# Extract the correlation coefficients
res2_overall_kaa_TRIALWISE$r
# Extract p-values
res2_overall_kaa_TRIALWISE$P


plotKaaLevelwise_SCORE_l3sig<-ggscatter(kaaDf, x = "meancorrectedL6trialScore", y = "kaa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kaa" ,
          facet.by = "level")

plotKaaOverall_meanPE<-ggscatter(kaaDf, x = "meanPE", y = "kaa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kaa")

plotKaaOverall_pdiSigFinding<-ggscatter(kaaDf, x = "pdi_total", y = "kaa", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "pdi_total", ylab = "Kaa")
#
#######################################################################################################################################################################
#kax correaltions and corr plots
########################################################################################################################

kaxDf<-  
    personwise_ParamXspaceTdat %>%
    filter(!level==3) %>% 
    filter(!kax>25) %>% 
    dplyr::select(kax, 
                  meanPE, 
                  meanPerfE, 
                  meanconf, 
                  meancorrectedL6trialScore,
                  level,
                  pdi_total,
                  spq_total,
                  green_total)

kaxDf<-  
    trialwiseJoinedDat %>%
    group_by(Participant.Private.ID, level) %>%
    filter(!level==3) %>% 
    filter(!kax>25) %>% 
    dplyr::select(kax, 
                  PE, 
                  PerfE, 
                  participantConfidence, 
                  trialScore,
                  level)
    
kaxMatDf_levelwise<- kaxDf %>% 
                    dplyr::select(kax, meanPE, meanPerfE, meanconf, meancorrectedL6trialScore,level) %>% 
                    pivot_wider(names_from = level, values_from = kax)

kaxMatDf_trialwise_levelwise<- kaxDf %>% 
                    dplyr::select(kax, 
                  PE, 
                  PerfE, 
                  participantConfidence, 
                  trialScore,
                  level) %>% 
                    pivot_wider(names_from = level, values_from = kax)

res2_levelwise <- rcorr(as.matrix(kaxMatDf_trialwise_levelwise))
# Extract the correlation coefficients
res2_levelwise$r
# Extract p-values
res2_levelwise$P


res2_overall <- rcorr(as.matrix(kaxDf))
# Extract the correlation coefficients
res2_overall$r
# Extract p-values
res2_overall$P

plotKaxLevelwise_PE<-ggscatter(kaxDf, x = "meanPE", y = "kax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kax" ,
          facet.by = "level")

plotKaxLevelwise_SCORE<-ggscatter(kaxDf, x = "meancorrectedL6trialScore", y = "kax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kax" ,
          facet.by = "level")

plotKaxOverall<-ggscatter(kaxDf, x = "meanPE", y = "kax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kax")

plotKaxOverall<-ggscatter(kaxDf, x = "meancorrectedL6trialScore", y = "kax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "Kax")

plotKaaOverall_spqSigFinding<-ggscatter(kaxDf, x = "spq_total", y = "kax", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "spq_total", ylab = "Kaa")

#######################################################################################################################################################################
#be1 correaltions and corr plots
########################################################################################################################

be1Df<-  
    personwise_ParamXspaceTdat %>%
    dplyr::select(be1, 
                  meanPE, 
                  meanPerfE, 
                  meanconf, 
                  meancorrectedL6trialScore,
                  level,
                  pdi_total,
                  spq_total,
                  green_total)
    
be1MatDf_levelwise<- be1Df %>% 
                    dplyr::select(be1, meanPE, meanPerfE, meanconf, meancorrectedL6trialScore,level) %>% 
                    pivot_wider(names_from = level, values_from = be1)

res2_levelwise <- rcorr(as.matrix(be1MatDf_levelwise))
# Extract the correlation coefficients
res2_levelwise$r
# Extract p-values
res2_levelwise$P


res2_overall <- rcorr(as.matrix(be1Df))
# Extract the correlation coefficients
res2_overall$r
# Extract p-values
res2_overall$P

plotbe1_meanConf<-ggscatter(be1Df, x = "meanconf", y = "be1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean conf", ylab = "be1")

plotbe1_SCORE<-ggscatter(be1Df, x = "meancorrectedL6trialScore", y = "be1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean score", ylab = "be1")

plotbe1_PE<-ggscatter(be1Df, x = "meanPE", y = "be1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean PE", ylab = "be1")

plotbe1_PErfe<-ggscatter(be1Df, x = "meanPerfE", y = "be1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Mean perfE", ylab = "be1")



GetBeta<-function(PE, PC) { 
             x<-lm(scale(PE) ~ scale(PC))
            return(x$coef[2])
            }


LRcorrKaa<-trialwiseJoinedDat %>%
  group_by(Participant.Private.ID, level) %>%
  mutate(beta=GetBeta(PE= signedPE, PC=signedPositionChange)) %>%
  #filter(!level %in% c(2,3)) %>%
  #filter(!kax>25) %>% 
  select(beta, be1) %>%
  summarise(lr=dplyr::first(beta), be1=dplyr::first(be1)) %>%
  drop_na()

ggscatter(LRcorrKaa, x = "lr", y = "be1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "LR", ylab = "be1", facet.by = "level")


kaaLRlevel<- LRcorrKaa %>% 
                    dplyr::select(be1, meanPE, meanPerfE, meanconf, meancorrectedL6trialScore,level) %>% 
                    pivot_wider(names_from = level, values_from = be1)