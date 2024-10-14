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
        library(ggpubr)

options(scipen = 999)

path <- "Figures"

trialwiseJoinedDat<-read.csv("Outputs/trialwise_ParamXspaceTdat_GENPOP2.csv") %>% as_tibble
personwise_ParamXspaceTdat <- read.csv("Outputs/personwise_ParamXspaceTdat_GENPOP2.csv") %>% 
  as_tibble %>% 
  mutate(level=as.factor(level)) %>%
  filter(!level==2)

Noiseblock = "#718ad0"
Volatilityblock = "#bf8c7f"
Combineduncertainty = "#66803c"
bivalent = "#97493b"
################################################################################################################################################
#be1 plots 
################################################################################################################################################


be1PDI_witOutSE<- personwise_ParamXspaceTdat %>%
            filter(level %in% c(3,4)) %>%
            ggplot(aes(x=pdi_total, y=be1)) +
            geom_point(aes(color=level), alpha=0.3) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() 

be1SPQ_withSE_56<- personwise_ParamXspaceTdat %>%
            filter(level %in% c(5,6)) %>%
            ggplot(aes(x=spq_total, y=be1)) +
            geom_point(aes(color=level), alpha=0.2) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() +
            scale_color_manual(values=c(Combineduncertainty, bivalent))+
            coord_cartesian(ylim = c(0.8,1.1))



be1SPQ_withSE_34<- personwise_ParamXspaceTdat %>%
            filter(level %in% c(3,4)) %>%
            ggplot(aes(x=spq_total, y=be1)) +
            geom_point(aes(color=level), alpha=0.2) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() +
            scale_color_manual(values=c(Noiseblock, Volatilityblock)) +
            coord_cartesian(ylim = c(0.8,1.1))


 genpop2_spqXbe1_l3vs4AND5vs6 <- be1SPQ_withSE_34 | be1SPQ_withSE_56 
    
be1CorScore_overall_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=meancorrectedL6trialScore))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        theme_classic()


ggsave(be1PDI_witOutSE, filename="Figures/genpop2_be1xPDI_level3and4interaction.pdf")
ggsave(be1SPQ_withSE_56, filename="Figures/genpop2_spqXbe1_level5and6Interaction.pdf")
ggsave(be1SPQ_withSE_34, file="Figures/genpop2_spqXbe1_level3and4Interaction.pdf")
ggsave(genpop2_spqXbe1_l3vs4AND5vs6, file="Figures/genpop2_spqXbe1_l3vs4AND5vs6.pdf")

































personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=level, color=level))+ geom_boxplot()

be1CorScore_levelwise_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=meancorrectedL6trialScore))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        facet_grid(~level) +
                        theme_classic()

be1CorScore_overall_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=meancorrectedL6trialScore))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        theme_classic()


be1CorPerfE_levelwise_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=log(meanPerfE)))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        facet_grid(~level) +
                        theme_classic()

be1CorPerfE_overall_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=log(meanPerfE)))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        theme_classic()

be1CorPerfE_levelwise_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=log(meanPerfE)))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        facet_grid(~level) +
                        theme_classic()

be1CorPerfE_overall_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=log(meanPerfE)))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        theme_classic()

be1CorConf_levelwise_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=meanconf))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        facet_grid(~level) +
                        theme_classic()

be1CorConf_overall_genpop2<-personwise_ParamXspaceTdat %>%
                        ggplot(aes(y=be1, x=meanconf))+ 
                        geom_point() + 
                        geom_smooth(method=lm) +                                       
                           stat_cor(method = "pearson",p.accuracy = 0.001, r.accuracy = 0.01, 
                                    label.x.npc = 0.3, label.y.npc = 0.1, size=2) +
                        theme_classic()

be1PDI_withSE<- personwise_ParamXspaceTdat %>%
            ggplot(aes(x=pdi_total, y=be1)) +
            geom_point(aes(color=level)) +
            geom_smooth(aes(group=level, color=level), method=lm) +
            theme_classic() 
    
be1PDI_witOutSE<- personwise_ParamXspaceTdat %>%
            filter(level %in% c(3,4)) %>%
            ggplot(aes(x=pdi_total, y=be1)) +
            geom_point(aes(color=level), alpha=0.3) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() 

be1SPQ_withSE<- personwise_ParamXspaceTdat %>%
            filter(level %in% c(3,4)) %>%
            ggplot(aes(x=spq_total, y=be1)) +
            geom_point(aes(color=level), alpha=0.2) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() 
    
be1SPQ_witOutSE<- personwise_ParamXspaceTdat %>%
            ggplot(aes(x=spq_total, y=be1)) +
            geom_point(aes(color=level)) +
            geom_smooth(aes(group=level, color=level), se=F, method=lm) +
            theme_classic() 


ggsave(be1SPQ_witOutSE, file="Figures/be1SPQ_witOutSE.pdf")
ggsave(be1SPQ_withSE, file="Figures/be1SPQ_withSE.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1PDI_witOutSE.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1PDI_withSE.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1CorConf_overall_genpop2.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1CorConf_levelwise_genpop2.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1CorPerfE_overall_genpop2.pdf")
ggsave(be1SPQ_witOutSE, file="Figures/be1CorPerfE_levelwise_genpop2.pdf")
ggsave(be1CorScore_levelwise_genpop2, file="Figures/be1CorScore_levelwise_genpop2.pdf")
ggsave(be1CorScore_overall_genpop2, file="Figures/be1CorScore_overall_genpop2.pdf")



