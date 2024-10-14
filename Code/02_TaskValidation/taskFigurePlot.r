dat %>% 
   mutate(PerfE_new=abs(Mean - participantPosition)) %>%
  filter(trialName=="spacetask003") %>%
  ggplot(aes(x=positionChange, y=PerfE_new)) +
  geom_point()

L2<-dat %>% 
  filter(Participant.Private.ID==8919550) %>%
  filter(level==2) %>%
  select(TrialNumber, level,trialName, Mean, SD, originalResult) %>%
  filter(trialName=="spacetask003") %>%
  mutate(SDupper = Mean + SD/2) %>%
  mutate(SDlower = Mean - SD/2) %>%
  ggplot(aes(x=TrialNumber, y=originalResult)) +
  geom_point() +
  geom_line(aes(y=Mean)) +
  geom_line(aes(y=SDupper), linetype="dashed") +
  geom_line(aes(y=SDlower), linetype="dashed") +
  ylim(-0.5,0.5)+
  theme_classic()

L3<-dat %>% 
  filter(Participant.Private.ID==8919550) %>%
  filter(level==3) %>%
  select(TrialNumber, level,trialName, Mean, SD, originalResult) %>%
  filter(trialName=="spacetask003") %>%
  mutate(SDupper = Mean + SD/2) %>%
  mutate(SDlower = Mean - SD/2) %>%
  ggplot(aes(x=TrialNumber, y=originalResult)) +
  geom_point() +
  geom_line(aes(y=Mean)) +
  geom_line(aes(y=SDupper), linetype="dashed") +
  geom_line(aes(y=SDlower), linetype="dashed") +
  ylim(-0.5,0.5)+
  theme_classic()

L4<-dat %>% 
  filter(Participant.Private.ID==8919550) %>%
  filter(level==4) %>%
  select(TrialNumber, level,trialName, Mean, SD, originalResult) %>%
  filter(trialName=="spacetask003") %>%
  mutate(SDupper = Mean + SD/2) %>%
  mutate(SDlower = Mean - SD/2) %>%
  ggplot(aes(x=TrialNumber, y=originalResult)) +
  geom_point() +
  geom_line(aes(y=Mean)) +
  geom_line(aes(y=SDupper), linetype="dashed") +
  geom_line(aes(y=SDlower), linetype="dashed") +
  ylim(-0.5,0.5)+
  theme_classic()

L5<-dat %>% 
  filter(Participant.Private.ID==8919550) %>%
  filter(level==5) %>%
  select(TrialNumber, level,trialName, Mean, SD, originalResult) %>%
  filter(trialName=="spacetask003") %>%
  mutate(SDupper = Mean + SD/2) %>%
  mutate(SDlower = Mean - SD/2) %>%
  ggplot(aes(x=TrialNumber, y=originalResult)) +
  geom_point() +
  geom_line(aes(y=Mean)) +
  geom_line(aes(y=SDupper), linetype="dashed") +
  geom_line(aes(y=SDlower), linetype="dashed") +
  ylim(-0.5,0.5) +
  theme_classic()

L6<-dat %>% 
  filter(Participant.Private.ID==8919550) %>%
  filter(level==6) %>%
  select(TrialNumber, level,trialName, Mean, SD, originalResult) %>%
  filter(trialName=="spacetask003") %>%
  mutate(SDupper = Mean + SD/2) %>%
  mutate(SDlower = Mean - SD/2) %>%
  ggplot(aes(x=TrialNumber, y=originalResult)) +
  geom_point() +
  geom_line(aes(y=Mean)) +
  geom_line(aes(y=SDupper), linetype="dashed") +
  geom_line(aes(y=SDlower), linetype="dashed") +
  ylim(-0.5,0.5) +
  theme_classic()

plot<-L2 / L3 / L4 / L5 / L6

ggsave(plot, filename="plot.pdf" )