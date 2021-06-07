### Write G-estimation Function for factor model ####
library(boot)
library(dplyr)

### Here is where you set parameter input ##
### Specify a model for the weight equations (this is used for the standardized inverse propensity weights)
### Specify the lavaan model. 

###dat$trust_federal = with(dat, rowMeans(cbind(trust_sc, trust_president, trust_congress), na.rm= T)) %>% zero.one()
dat$trust_federal = with(dat, rowMeans(cbind(trust_sc, trust_president, trust_congress), na.rm= T)) %>% zero.one()
dat$trust_state =   with(dat, rowMeans(cbind(trust_governor, trust_stateleg), na.rm= T)) %>% zero.one()

weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + as.factor(state))
weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + as.factor(state))
#trust1        =~ 1*trust_congress + 1*trust_sc
#trust2        =~ trust_governor + trust_stateleg 



### Declare which data are to be treated as ordinal #


ordinal_data = c(             "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", "participation_protest")
### Lavaan sim does all the processing/estimation. 
### Here is how to run the boostrapped simulations, this saves a boot object -- which I process for figures just below
out_weights   = boot(dat, lavaan_sim, R = 100, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)
#subset(type == "Soft") %>% 

out_weights %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_1.jpg',
         width = 750, height = 500,)
dev.off()

out_weights %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_2.jpg',
         width = 750, height = 500)
dev.off()
###################################################################################################
############ State Heterogeneity ##################################################################
###################################################################################################
### This throws some strange error messages
ordinal_data = c(             "violent", "burn", "court", "recount", "criticize")
### Heterogeneity across states ####
weight_1 <- as.formula(I(treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + latino + black + college + ideology + christian)

weight_2 <- as.formula(I(treat==3) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + latino + black + college + ideology + christian)

out_weights_az   = boot(subset(dat, state ==1), lavaan_sim_2, R = 100, 
                        ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model_state,  
                        weight_1 = weight_1,
                        weight_2 = weight_2)
out_weights_co   = boot(subset(dat, state ==2), lavaan_sim_2, R = 100, 
                        ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model_state,  
                        weight_1 = weight_1,
                        weight_2 = weight_2)
out_weights_rest   = boot(subset(dat, state > 3), lavaan_sim_2, R = 100, 
                        ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model_state2,  
                        weight_1 = weight_1,
                        weight_2 = weight_2)

##
##### Generate Figures #####

out_weights_az %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Arizona)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-2, 2)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_4.jpg',
         width = 750, height = 500,)
dev.off()

out_weights_co %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Colorado)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-2, 2)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_5.jpg',
         width = 750, height = 500,)
dev.off()

out_weights_rest %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Rest)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect") + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_6.jpg',
         width = 750, height = 500,)
dev.off()


weight_1 <- as.formula(I(treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))

weight_2 <- as.formula(I(treat==3) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female +  college + ideology + christian + as.factor(state))

out_weights_white   = boot(subset(dat, white ==1), lavaan_sim_2, R = 100, 
                        ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model_state,  
                        weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_latino   = boot(subset(dat, latino ==1), lavaan_sim_2, R = 100, 
                           ordinal_data = ordinal_data,
                           lavaan_model = lavaan_model_state,  
                           weight_1 = weight_1,
                           weight_2 = weight_2)

out_weights_white %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (White Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect") + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_7.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_latino %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Latino Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect") + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_8.jpg',
         width = 750, height = 500,)
dev.off()




out_weights_latino %>% plot_data_generator_states() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Latino)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_8.jpg',
         width = 750, height = 500,)
dev.off()

#### Colorado ###
out_weights_co   = boot(subset(dat, state ==2), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_co %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Colorado)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_9.jpg',
         width = 750, height = 500,)
dev.off()

out_weights_co %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Colorado)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_10.jpg',
         width = 750, height = 500,)
dev.off()


#### Colorado ###
out_weights_nv   = boot(subset(dat, state ==3), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_nv %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Nevada)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_11.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_nv %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Nevada)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_12.jpg',
         width = 750, height = 500,)
dev.off()


#### New Mexico ###
out_weights_nm   = boot(subset(dat, state ==4), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_nm %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (New Mexico)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_13.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_nm %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (New Mexico)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_14.jpg',
         width = 750, height = 500,)
dev.off()

#### Utah ###
out_weights_ut   = boot(subset(dat, state ==5), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_ut %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Utah)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_15.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_ut %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Utah)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)


dev.copy(png,'section3_16.jpg',
         width = 750, height = 500,)
dev.off()

###################################################################################################
############ Robustness Checks   ##################################################################
###################################################################################################
### Examine racial groups separately #####
weight_1 <- as.formula(I(treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))
weight_2 <- as.formula(I(treat==3) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))
### Here is how to run the boostrapped simulations, this saves a boot object -- which I process for figures just below
out_weights_white   = boot(subset(dat, white ==1), lavaan_sim, 3, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)

=======
                              "trust_congress" , "trust_president", "trust_sc",
                              "trust_governor", "trust_stateleg",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", "participation_protest")
                              
#                               , 
#                               "participation_contact", "participation_donate",
#                               "confidence_ballot", "efficacy_dontcare", "efficacy_complicated"
# ) 
# ### Primary bootstrapping model ###
# 


### Lavaan sim does all the processing/estimation. 
### Here is how to run the boostrapped simulations, this saves a boot object -- which I process for figures just below
out_weights   = boot(dat, lavaan_sim, R = 100, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)
### This generates the treatment effect plot data set
plot_data_generator = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975),
    type = rep(c("Hard", 
                 "Soft", 
                 "Participation", 
                 "Concern", 
                 "Trust(Federal)", 
                 "Trust(State)", 
                 "Ballot Confidence", 
                 "Efficacy (Internal)", 
                 "Efficacy(External)")),
    period = rep(c("Uncertainty", "Post_Call"), each = ncol(output$t)/2 ),
    voter = rep(c("Biden", "Trump"), each = 9 ))
  return(plot_dat)
}

#subset(type == "Soft") %>% 

out_weights %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_1.jpg',
         width = 750, height = 500,)
dev.off()

out_weights %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_2.jpg',
         width = 750, height = 500)
dev.off()
###################################################################################################
############ State Heterogeneity ##################################################################
###################################################################################################

### Heterogeneity across states ####

weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + latino + black + college + ideology + christian)
weight_2 <- as.formula((treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + latino + black + college + ideology + christian)

out_weights_az   = boot(subset(dat, state ==1), lavaan_sim, 3, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_az %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Arizona)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_7.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_latino %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Arizona)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_8.jpg',
         width = 750, height = 500,)
dev.off()

#### Colorado ###
out_weights_co   = boot(subset(dat, state ==2), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_co %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Colorado)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_9.jpg',
         width = 750, height = 500,)
dev.off()

out_weights_co %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Colorado)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_10.jpg',
         width = 750, height = 500,)
dev.off()


#### Colorado ###
out_weights_nv   = boot(subset(dat, state ==3), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_nv %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Nevada)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_11.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_nv %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Nevada)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_12.jpg',
         width = 750, height = 500,)
dev.off()


#### New Mexico ###
out_weights_nm   = boot(subset(dat, state ==4), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_nm %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (New Mexico)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_13.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_nm %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (New Mexico)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_14.jpg',
         width = 750, height = 500,)
dev.off()

#### Utah ###
out_weights_ut   = boot(subset(dat, state ==5), lavaan_sim, 100, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model, weight_1 = weight_1,
                        weight_2 = weight_2)

out_weights_ut %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Utah)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'section3_15.jpg',
         width = 750, height = 500,)
dev.off()


out_weights_ut %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Utah)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1, 1)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)


dev.copy(png,'section3_16.jpg',
         width = 750, height = 500,)
dev.off()

###################################################################################################
############ Robustness Checks   ##################################################################
###################################################################################################
### Examine racial groups separately #####
weight_1 <- as.formula(I(treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))
weight_2 <- as.formula(I(treat==3) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))
### Here is how to run the boostrapped simulations, this saves a boot object -- which I process for figures just below
out_weights_white   = boot(subset(dat, white ==1), lavaan_sim, 3, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)

>>>>>>> bccaddf5aa5165b32d5dd5f4c7650eba9339d18a
out_weights_white %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (White Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.5, 1.5)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_3.jpg',
         width = 750, height = 500)
dev.off()

out_weights_white %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (White Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.5, 1.5)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_4.jpg',
         width = 750, height = 500)
dev.off()

### Latinos ####

### Examine racial groups separately #####
weight_1 <- as.formula((treat==2) ~    authoritarianism + rr + independent + republican + trump_vote + rwm +
                         age + female + college + ideology + christian + as.factor(state))
weight_2 <- as.formula((treat==3) ~   authoritarianism + rr + independent + republican + trump_vote + rwm + 
                         age + female + college + ideology + christian + as.factor(state))
### Here is how to run the boostrapped simulations, this saves a boot object -- which I process for figures just below
out_weights_latino   = boot(subset(dat, latino ==1), lavaan_sim, 3, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)

out_weights_latino %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(type == "Soft" | type == "Hard") %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects (Latino Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.5, 1.5)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'section3_5.jpg',
         width = 750, height = 500,)
dev.off()

out_weights_latino %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type)+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Placebo Tests (Latino Respondents)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.5, 1.5)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0)

dev.copy(png,'section3_6.jpg',
         width = 750, height = 500,)
dev.off()



# Robustness checks, 
# estimated without weights, just the full model
lavaan_sim_noweights = function(dat, indices){
  .df <- dat[indices, ]
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
  temp_dat = rbind(
    data.frame(
      trump_vote = coef(model1)[grep("trump_vote", names(coef(model1)))],
      post1 = coef(model1)[grep("post1", names(coef(model1)))],
      post2 = coef(model1)[grep("post2", names(coef(model1)))],
      TT1 = coef(model1)[grep("TT1", names(coef(model1)))],
      TT2 = coef(model1)[grep("TT2", names(coef(model1)))]
    ),
    data.frame(
      trump_vote = coef(model2)[grep("trump_vote", names(coef(model2)))],
      post1 = coef(model2)[grep("post1", names(coef(model2)))],
      post2 = coef(model2)[grep("post2", names(coef(model2)))],
      TT1 = coef(model2)[grep("TT1", names(coef(model2)))],
      TT2 = coef(model2)[grep("TT2", names(coef(model2)))]
    )
  )
  temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
  # Form Predictions
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
  }
out_noweights   = boot(dat, lavaan_sim_noweights, 2)
# Now estimate the model with white respondents. 
lavaan_sim_whites = function(dat, indices){
  .df <- dat[indices, ]
  .df <- subset(.df, white == 1)
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
# DELETE AND FIX EVERYTHWERE 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
  temp_dat = rbind(
    data.frame(
      trump_vote = coef(model1)[grep("trump_vote", names(coef(model1)))],
      post1 = coef(model1)[grep("post1", names(coef(model1)))],
      post2 = coef(model1)[grep("post2", names(coef(model1)))],
      TT1 = coef(model1)[grep("TT1", names(coef(model1)))],
      TT2 = coef(model1)[grep("TT2", names(coef(model1)))]
    ),
    data.frame(
      trump_vote = coef(model2)[grep("trump_vote", names(coef(model2)))],
      post1 = coef(model2)[grep("post1", names(coef(model2)))],
      post2 = coef(model2)[grep("post2", names(coef(model2)))],
      TT1 = coef(model2)[grep("TT1", names(coef(model2)))],
      TT2 = coef(model2)[grep("TT2", names(coef(model2)))]
    )
  )
  temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
  # Form Predictions
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
}
out_whites   = boot(dat, lavaan_sim_whites, 100)
# And Latinos
lavaan_sim_latinos = function(dat, indices){
  .df <- dat[indices, ]
  .df <- subset(.df, latino == 1)
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
  temp_dat = rbind(
    data.frame(
      trump_vote = coef(model1)[grep("trump_vote", names(coef(model1)))],
      post1 = coef(model1)[grep("post1", names(coef(model1)))],
      post2 = coef(model1)[grep("post2", names(coef(model1)))],
      TT1 = coef(model1)[grep("TT1", names(coef(model1)))],
      TT2 = coef(model1)[grep("TT2", names(coef(model1)))]
    ),
    data.frame(
      trump_vote = coef(model2)[grep("trump_vote", names(coef(model2)))],
      post1 = coef(model2)[grep("post1", names(coef(model2)))],
      post2 = coef(model2)[grep("post2", names(coef(model2)))],
      TT1 = coef(model2)[grep("TT1", names(coef(model2)))],
      TT2 = coef(model2)[grep("TT2", names(coef(model2)))]
    )
  )
  temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
  # Form Predictions
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
  }
out_latinos   = boot(dat, lavaan_sim_latinos, 100)
# I don't see evidence that an oversample or something that YouGov has done 
# produced discrepancies on racial/ethnic lines.
### Concern about experiments ###
lavaan_sim_exp = function(dat, indices){
  .df <- dat[indices, ]
  den1 <- glm(I(treat==2) ~ + authoritarianism + rr + independent + republican + trump_vote + rwm +
                age + female + latino + black + college + ideology + christian + as.factor(state), 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(I(treat==3) ~ + authoritarianism + rr + independent + republican + trump_vote + rwm +
                age + female + latino + black + college + ideology + christian + as.factor(state), 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  weights1 <- glm(I(treat==2) ~ 1, 
                  data = .df, family=binomial("logit")) %>% 
    augment(type.predict = "response") %>%
    mutate(wts = ifelse(.df$treat !=2, ((1-.fitted)/(1-den1)), ((.fitted)/(den1))))
  weights2 <- glm(I(treat==3) ~ 1, 
                  data = .df, family=binomial("logit")) %>% 
    augment(type.predict = "response") %>%
    mutate(wts = ifelse(.df$treat !=3, ((1-.fitted)/(1-den2)), ((.fitted)/(den2))))
  ### Append data with new weights ###
  .df$weights1 = weights1$wts
  .df$weights2 = weights2$wts
  
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2
              violent ~ VIOLENT
              criticize ~ SM

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df, sampling.weights = "weights1") 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df, sampling.weights = "weights2") 
  
  ## Regex out all the relevant stuff ###
  
  temp_dat = rbind(
    data.frame(
      trump_vote = coef(model1)[grep("trump_vote", names(coef(model1)))],
      post1 = coef(model1)[grep("post1", names(coef(model1)))],
      post2 = coef(model1)[grep("post2", names(coef(model1)))],
      TT1 = coef(model1)[grep("TT1", names(coef(model1)))],
      TT2 = coef(model1)[grep("TT2", names(coef(model1)))]
    ),
    data.frame(
      trump_vote = coef(model2)[grep("trump_vote", names(coef(model2)))],
      post1 = coef(model2)[grep("post1", names(coef(model2)))],
      post2 = coef(model2)[grep("post2", names(coef(model2)))],
      TT1 = coef(model2)[grep("TT1", names(coef(model2)))],
      TT2 = coef(model2)[grep("TT2", names(coef(model2)))]
    )
  )
  temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
  # Form Predictions
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
}
out_sim_exp   = boot(dat, lavaan_sim_exp, 100)



out_weights

out_sim_exp %>% plot_data_generator() %>% subset(type == "Soft") %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Party", values=c("blue", "red"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Conditional Treatment Effects of Post-Call") +
  labs(caption="2020 Western States Study. Estimates are doubly robust (G-estimation/IPW)")+
  scale_y_continuous("ATE", limits = c(-1, 1)) + 
  scale_x_discrete("Form of Contestation") +
  geom_hline(yintercept = 0) 




# model1a <-  " hard =~ violent + burn
#               soft =~ recount + criticize + court
#               participation =~ participation_persuade + participation_socialmedia + 
#               participation_yard + participation_volunteer + 
#               participation_protest + participation_contact  +  participation_donate
#               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 



names(dat) # SM; VIOL
dat$pre =   ifelse(dat$treat==1, 1, 0)
dat$post1 = ifelse(dat$treat==2, 1, 0)
dat$post2 = ifelse(dat$treat==3, 1, 0)
dat$TT1 = dat$trump_vote  * dat$post1
dat$TT2 = dat$trump_vote  * dat$post2

model_items <-  " hard =~ violent + burn 
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2
              violent ~ VIOLENT
              criticize ~ SM
"
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate"
) 
model1 = cfa(model_items, ordered=full_data_key, 
             data=dat) 

# This works fine -- now put in a function and simulate the effects of everything.Bootstrap, 
# Pull out effects


# Then just estimate predictions from the lavaan model #
# It should 

## If all this works, the final model is one where we regress: violent ~ VIOLENT + post1 + post2 + post1:VIOLENT....

CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "christian",
               "republican",
               "democrat",
               "conservatism",
               "anger",
               "anxiety",
               "authoritarianism")

controls = c("")

b$t
              
## Plot function ##
b.c


         ## Calculate fitted values from this ##



weight2 <- glm(I(treat==3) ~ + authoritarianism + rr + independent + republican + trump_vote  + rwm +
                 age + female + latino + black + college + ideology + christian + as.factor(state), 
               data = dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = 1 / (ifelse(dat$post_call != 2, 1 - .fitted, .fitted)) ) ### These are the propensity score weights




# estimation of denominator of ip weights
denom.fit <- glm(qsmk ~ as.factor(sex) + as.factor(race) + age + I(age^2) + 
                   as.factor(education) + smokeintensity + 
                   I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
                   as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                 family = binomial(), data = nhefs.nmv)
summary(denom.fit)


model<-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"

model2 =  cfa(mod, ordered=names(dat)[1:4], 
              data=dat, group = "group")


a0_corr1 = lavInspect(model1, what = "std.lv")$a0$beta[3]
a1_corr1 = lavInspect(model1, what = "std.lv")$a1$beta[3]