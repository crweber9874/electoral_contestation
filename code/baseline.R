### Baseline effects
library(pscl)
h1 <-  as.formula(as.factor(violent) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + latino + black + college + ideology + christian + 
                         sdo + as.factor(state) + VIOLENT) 

h2 <-  as.formula(as.factor(burn) ~  authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state)) 

s1 <-  as.formula(as.factor(criticize) ~  authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state) + SM) 

s2 <-  as.formula(as.factor(court) ~  authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state)) 

s3 <-  as.formula(as.factor(recount) ~  authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state)) 

m1<- polr(h1, data = subset(dat, treat ==1)) %>% summary
m2<- polr(h2, data = subset(dat, treat ==1)) %>% summary
m3<- polr(s1, data = subset(dat, treat ==1)) %>% summary
m4<- polr(s2, data = subset(dat, treat ==1)) %>% summary
m5<- polr(s3, data = subset(dat, treat ==1)) %>% summary

plot_dat = rbind(winner_loser_baseline(dat, "violent"),
                 winner_loser_baseline(dat, "burn"),
                 winner_loser_baseline(dat, "criticize"),
                 winner_loser_baseline(dat, "recount"),
                 winner_loser_baseline(dat, "court")) %>% data.frame() %>% 
                 mutate(item = rep(c("Protest", "Flag Burn", "Criticize", "Recount", "Court"), each =16)) 



  
plot_dat$variable <- factor(plot_dat$names, levels = rev(c("Authoritarianism", "RR", "Vote Trump", "Right-Wing Media",
                           "Age", "Female" , "Latino" , "Black" , "College" , "Ideology" , "Christian" , 
                           "SDO" , "Colorado*", "New Mexico*", "Nevada*", "Utah*")) )

ggplot(plot_dat, aes(y=mid, ymin=min, ymax=max, x=variable)) + 
  facet_wrap (~item)+
  geom_point(size =0.5) +
  geom_errorbar( width = 0.03, alpha = 0.9) +
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation, Pre-election Effects") +
  labs(caption="2020 Western States Study. ")+
  scale_y_continuous("Average Marginal Effect", limits = c(-0.5, 0.5)) + 
  geom_hline(yintercept = 0, linetype = "dotdash") +
  coord_flip()

dev.copy(png,'baseline_effects.jpg',
         width = 750, height = 500)
dev.off()


dat$soft <- rowMeans(cbind(dat$recount, dat$criticize, dat$court), na.rm = T)
dat$hard <- rowMeans(cbind(dat$burn, dat$violent), na.rm = T)

hard_baseline <-  as.formula(hard~  authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state)) 

soft_baseline <-  as.formula(soft~  authoritarianism + rr + trump_vote + rwm +
                               age + female + latino + black + college + ideology + christian + 
                               sdo + as.factor(state)) 


lm(hard_baseline, data = subset(dat, treat ==1)) %>% summary

plot_dat = rbind(winner_loser_baseline_l(dat, "hard"),
                 winner_loser_baseline_l(dat, "soft")) %>% data.frame() %>% 
  mutate(item = rep(c("Hard Contestation", "Soft Contestation"), each =17)) 
  
plot_dat$Variable <- factor(plot_dat$names, levels = rev(c("Intercept", "Authoritarianism", "RR", "SDO",  "Ideology", "Vote Trump", "Right-Wing Media",
                                                           "Age", "Female" , "Latino" , "Black" , "College"  , "Christian" , 
                                                            "Colorado*", "New Mexico*", "Nevada*", "Utah*")) )
plot_dat = subset(plot_dat, Variable !="Intercept")


ggplot(plot_dat, aes(y=mid, ymin=min, ymax=max, x=Variable)) + 
  facet_wrap (~item)+
  geom_point(size =0.5) +
  geom_errorbar( width = 0.03, alpha = 0.9) +
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation, Pre-election Effects") +
  labs(caption="2020 Western States Study. ")+
  scale_y_continuous("Average Marginal Effect") + 
  scale_y_continuous("Marginal Effect") + 
  geom_hline(yintercept = 0, linetype = "dotdash") +
  coord_flip()


dev.copy(png,'baseline_effects2.jpg',
         width = 750, height = 500)
dev.off()
