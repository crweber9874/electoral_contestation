### Appendix ####
#### October 27, 2021. 
library(dplyr)
### Declare some variables (again)
dat$trust_federal = with(dat, rowMeans(cbind(trust_sc, trust_president, trust_congress), na.rm= T)) %>% zero.one()
dat$trust_state =   with(dat, rowMeans(cbind(trust_governor, trust_stateleg), na.rm= T)) %>% zero.one()
#### These are the propensity weights
dat$pre =    ifelse(dat$treat==1, 1, 0)
dat$post1 =  ifelse(dat$treat==2, 1, 0)
dat$post2 =  ifelse(dat$treat==3, 1, 0)
dat$TT1 = dat$trump_vote  * dat$post1 ### Interactions for winner and loser effects.
dat$TT2 = dat$trump_vote  * dat$post2
### Robustness, Experimetnal Effects 




out_weights   = boot(dat, lavaan_sim_predict, R = 500, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal_treat, parallel = "multicore", ncpus = 6)
save(out_weights, file = "outweights_treat_exp.rda")

### For main winner and loser effects
plot_data_generator_all = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975))
  return(plot_dat)
}

### This generates the plotting data frame######
output = out_weights %>% plot_data_generator_all() 
output$voter[grepl("trump", names(out_weights$t0))] = "Trump Voter"
output$voter[grepl("biden", names(out_weights$t0))] = "Biden Voter"
output$treat[grepl("_dif", names(out_weights$t0))] =  "Treatment Effect"
output$treat = if_else(is.na(output$treat), "Point Estimate", output$treat)
output$type = rep(c("High Cost", 
                    "Low Cost", 
                    "Concern", 
                    "Trust(Federal)", 
                    "Trust(State)", 
                    "Ballot Confidence"), each = 6)
output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Uncertainty_Treat", "Uncertainty_Treat"), times = 6),
                  rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Post_Treat", "Post_Treat"), times = 6)) 


library(patchwork)
part1 =   
  subset(output, type == "Low Cost" | type == "High Cost")  %>% 
  subset(treat == "Point Estimate") %>%
  mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~type, nrow = 2)+
  geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.25, position = position_dodge(width = 0.2))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.2))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle=0),
        plot.title = element_text(color="black", size=12)) 

output$treatment[grepl("Uncertainty_", output$period)] =    "Uncertainty"
output$treatment[grepl("Post_", output$period)] =    "Post-Call"

part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
  subset(treat == "Treatment Effect") %>%
  mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=treatment, colour=voter)) + 
  facet_wrap(~type, nrow = 2) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  scale_fill_grey() +
  coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) +
  ggtitle("Winner/Loser Effects") +
  scale_x_discrete("", position = "top")+
  scale_y_continuous("Marginal Effect")+
  geom_hline(yintercept = 0) 

png(filename = "Figures/figure_latent_robust_exp.png",
      res = 100, width = 600, height = 600)
part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects (Robustness, Experimental Effects), ',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)

dev.off()




### This is the primary function without weights. 
### Robustness: No weights in SEM model
lavaan_sim_predict_no = function(dat, indices, ordinal_data = ordinal_data,
                              lavaan_model = lavaan_model){
  .df <- dat[indices, ]
  
  model1 = cfa(lavaan_model, ordered=ordinal_data, 
               data=.df)  
  
  model2 = cfa(lavaan_model, ordered=ordinal_data, 
               data=.df) 
  
  
  temp_dat = rbind(
    data.frame(
      trump_vote = coef(model1) [grep("trump_vote", names(coef(model1)))],
      post1 = coef(model1) [grep("post1", names(coef(model1)))],
      post2 = coef(model1) [grep("post2", names(coef(model1)))],
      TT1 = coef(model1) [grep("TT1", names(coef(model1)))],
      TT2 = coef(model1) [grep("TT2", names(coef(model1)))]
    ),
    data.frame(
      trump_vote = coef(model2) [grep("trump_vote", names(coef(model2)))],
      post1 = coef(model2) [grep("post1", names(coef(model2)))],
      post2 = coef(model2) [grep("post2", names(coef(model2)))],
      TT1 = coef(model2) [grep("TT1", names(coef(model2)))],
      TT2 = coef(model2) [grep("TT2", names(coef(model2)))]
    )
  )
  temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
  
  temp_predictions = c()
  # Form Predictions
  for (i in 1:6){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    temp_predictions = c(temp_predictions, c(trumpa= ta, trumpb=tb, 
                        bidena=ba, bidenb = bb, trump_dif = tb - ta, 
                        biden_dif= bb - ba))
  }
  for (i in 7:12){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    temp_predictions = c(temp_predictions, 
                         c(trumpa= ta, trumpb=tc, bidena=ba, bidenb = bc, trump_dif = tc - ta, biden_dif= bc - ba)) 
  }
  return(temp_predictions)
}



out_weights_no   = boot(dat, lavaan_sim_predict_no, R = 500, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, parallel = "multicore", ncpus = 6)
#save(out_weights, file = "outweights.rda")


### This generates the plotting data frame######
output = out_weights_no %>% plot_data_generator_all() 


output$voter[grepl("trump", names(out_weights$t0))] = "Trump Voter"
output$voter[grepl("biden", names(out_weights$t0))] = "Biden Voter"
output$treat[grepl("_dif", names(out_weights$t0))] =  "Treatment Effect"
output$treat = if_else(is.na(output$treat), "Point Estimate", output$treat)
output$type = rep(c("High Cost", 
                    "Low Cost", 
                    "Concern", 
                    "Trust(Federal)", 
                    "Trust(State)", 
                    "Ballot Confidence"), each = 6)
output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Uncertainty_Treat", "Uncertainty_Treat"), times = 6),
                  rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Post_Treat", "Post_Treat"), times = 6)) 


library(patchwork)
part1 =   
    subset(output, type == "Low Cost" | type == "High Cost")  %>% 
    subset(treat == "Point Estimate") %>%
    mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~type, nrow = 2)+
  geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.25, position = position_dodge(width = 0.2))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.2))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
#  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle=0),
        plot.title = element_text(color="black", size=12)) 

output$treatment[grepl("Uncertainty_", output$period)] =    "Uncertainty"
output$treatment[grepl("Post_", output$period)]        =    "Post-Call"

part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
  subset(treat == "Treatment Effect") %>%
  mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=treatment, colour=voter)) + 
    facet_wrap(~type, nrow = 2) + 
    geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
    geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
    scale_fill_grey() +
    coord_flip()+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          #axis.title = element_blank(),
         # axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle=0),
          axis.text=element_text(size=8),
           plot.title = element_text(color="black", size=12)) +
    ggtitle("Winner/Loser Effects") +
    scale_x_discrete("", position = "top")+
    scale_y_continuous("Marginal Effect")+
    geom_hline(yintercept = 0) 


png(filename = "Figures/figure_latent_noweights.png",
      res = 100, width = 600, height = 600)
part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects (No Weights)',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)
dev.off()


### Reestimate marginal models with linear regression and IPW weights.
dat$high = rowMeans(cbind(dat$violent, dat$burn)) %>% zero.one()
dat$low = rowMeans(cbind(dat$court, dat$criticize, dat$recount, dat$criticize)) %>% zero.one()

variables = c("high", "low")
robustness_checks_weight = list()
robustness_checks_noweight = list()
### Create a massive list....weights and non 
for(var in variables){
  temp1a = boot(dat, treat.effects.linear, R = 500, 
              dependent.variable = var , 
              type = "point", "weighting" = TRUE)
  temp1b = boot(dat, treat.effects.linear, R = 500, 
               dependent.variable = var , 
               type = "marginal", "weighting" = TRUE)  
  robustness_checks_weight[[paste0(var, "_point")]]    = temp1a
  robustness_checks_weight[[paste0(var, "_marginal")]] = temp1b  
}

point_estimate    = robustness_checks_weight[grepl("_point", names(robustness_checks_weight))] 
marginal_estimate = robustness_checks_weight[grepl("_marginal", names(robustness_checks_weight))] 
output1 = data.frame()
output2 = data.frame()
for(i in names(point_estimate)){
  output1 = rbind(output1, point_estimate[[i]] %>%  plot_data_generator_all())
}
for(i in names(marginal_estimate)){
  output2 = rbind(output2, marginal_estimate[[i]] %>% plot_data_generator_all())
}
output1$voter     =     rep(c("Trump Voter", "Biden Voter"))
output1$period    =     rep(c("Pre-Election", "Uncertainty", "Post-Election"), each = 2)
output1$item       =    rep(c("High Cost", "Low Cost"),  each = 6)

output2$voter     =     rep(c("Trump Voter", "Biden Voter"))
output2$period    =     rep(c("Uncertainty", "Post-Election"), each = 2)
output2$item       =    rep(c("High Cost", "Low Cost"),  each = 4)

part1 =   output1 %>%
    mutate(period = factor(period, c("Post-Election", "Uncertainty", "Pre-Election"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~item, nrow = 2)+
  geom_linerange(aes(x=period, ymax=middle, ymin=0), size = 1.25, position = position_dodge(width = 0.4))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.4))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
#  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle=0),
        plot.title = element_text(color="black", size=12)) 



part2 =   output2 %>% 
  mutate(item = factor(item, c("Low Cost", "High Cost"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap(~item, nrow = 2) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.9), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.9), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  scale_fill_grey() +
  coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) +
  ggtitle("Winner/Loser Effects") +
  scale_x_discrete("", position = "top")+
  scale_y_continuous("Marginal Effect Estimate")+
  geom_hline(yintercept = 0) 

png(filename = "Figures/robustness_linear.png",
      res = 100, width = 600, height = 600)

(part1 + part2) +
  plot_layout(guides='collect')+ plot_annotation(
    title = "Robustness, Linear Regression",
    #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = "2020 Western States Study",
    theme = theme(plot.title = element_text(size = 14),
                  legend.position = "bottom")
  )

dev.off()
####################################
# Estimate effects by race#####
# white ==1
# latino == 1
# by state, AZ, COL, NV, NM, UT

latino_dat = subset(dat, latino == 1)

weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + + college + ideology + christian + sdo + as.factor(state)) ## Probability of treatment 1
weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + college + ideology + christian + sdo + as.factor(state))  ### Probability of treatment 2

### Declare which data are to be treated as ordinal #
ordinal_data = c(             "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", 
                              "participation_protest")
#### Model estimates ###
### Here's how I generate the weights
# Predict the probability of each treatment, conditional on some covariates -- models above...
## This is pretty good: https://towardsdatascience.com/doubly-robust-estimators-for-causal-inference-in-statistical-estimation-3c00847e9db
den1 <- glm(weight_1, 
            data = latino_dat, family=binomial("logit")) %>% predict(type = "response")
den2 <- glm(weight_2, 
            data = latino_dat, family=binomial("logit")) %>% predict(type = "response")


weights1 <- glm(I(treat==2) ~ 1, 
                data = latino_dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(latino_dat$treat !=2, ((1-.fitted)/(1-den1)), ((.fitted)/(den1))))

weights2 <- glm(I(treat==3) ~ 1, 
                data = latino_dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(latino_dat$treat !=3, ((1-.fitted)/(1-den2)), ((.fitted)/(den2))))

## Basically just weight the data according to the propensity of receiving the treatment. Then weight in the mdoel 1/pr(treatment).
## We're just weighting "up" those observations that (due to non-random assignment) are less likely to be observed in the data. 
## the weight is standardized by the proportion of cases in each treatment condition.


### Append data with new weights ###
latino_dat$weights1 = weights1$wts
latino_dat$weights2 = weights2$wts
latino_dat$pre =    ifelse(latino_dat$treat==1, 1, 0)
latino_dat$post1 =  ifelse(latino_dat$treat==2, 1, 0)
latino_dat$post2 =  ifelse(latino_dat$treat==3, 1, 0)
latino_dat$TT1 = latino_dat$trump_vote  * latino_dat$post1 ### Interactions for winner and loser effects.
latino_dat$TT2 = latino_dat$trump_vote  * latino_dat$post2

out_weights   = boot(lavaan_sim_predict, R = 500, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, parallel = "multicore", ncpus = 6,
                     data = latino_dat, )
save(out_weights, file = "outweights_latino.rda")


### For main winner and loser effects
plot_data_generator_all = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975))
  return(plot_dat)
}

### This generates the plotting data frame######
output = out_weights %>% plot_data_generator_all() 
output$voter[grepl("trump", names(out_weights$t0))] = "Trump Voter"
output$voter[grepl("biden", names(out_weights$t0))] = "Biden Voter"
output$treat[grepl("_dif", names(out_weights$t0))] =  "Treatment Effect"
output$treat = if_else(is.na(output$treat), "Point Estimate", output$treat)
output$type = rep(c("High Cost", 
                    "Low Cost", 
                    "Concern", 
                    "Trust(Federal)", 
                    "Trust(State)", 
                    "Ballot Confidence"), each = 6)
output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Uncertainty_Treat", "Uncertainty_Treat"), times = 6),
                  rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Post_Treat", "Post_Treat"), times = 6)) 


library(patchwork)
part1 =   
  subset(output, type == "Low Cost" | type == "High Cost")  %>% 
  subset(treat == "Point Estimate") %>%
  mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~type, nrow = 2)+
  geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.25, position = position_dodge(width = 0.2))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.2))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle=0),
        plot.title = element_text(color="black", size=12)) 

output$treatment[grepl("Uncertainty_", output$period)] =    "Uncertainty"
output$treatment[grepl("Post_", output$period)] =    "Post-Call"

part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
  subset(treat == "Treatment Effect") %>%
  mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=treatment, colour=voter)) + 
  facet_wrap(~type, nrow = 2) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  scale_fill_grey() +
  coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) +
  ggtitle("Winner/Loser Effects") +
  scale_x_discrete("", position = "top")+
  scale_y_continuous("Marginal Effect")+
  geom_hline(yintercept = 0) 

png(filename = "Figures/figure_latino.png",
      res = 100, width = 600, height = 600)
part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects (Latino Respondents)',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)
dev.off()
########################################################################################
white_dat = subset(dat, white == 1)

weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + + college + ideology + christian + sdo + as.factor(state)) ## Probability of treatment 1
weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + college + ideology + christian + sdo + as.factor(state))  ### Probability of treatment 2

### Declare which data are to be treated as ordinal #
ordinal_data = c(             "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", 
                              "participation_protest")
#### Model estimates ###
### Here's how I generate the weights
# Predict the probability of each treatment, conditional on some covariates -- models above...
## This is pretty good: https://towardsdatascience.com/doubly-robust-estimators-for-causal-inference-in-statistical-estimation-3c00847e9db
den1 <- glm(weight_1, 
            data = white_dat, family=binomial("logit")) %>% predict(type = "response")
den2 <- glm(weight_2, 
            data = white_dat, family=binomial("logit")) %>% predict(type = "response")


weights1 <- glm(I(treat==2) ~ 1, 
                data = white_dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(white_dat$treat !=2, ((1-.fitted)/(1-den1)), ((.fitted)/(den1))))

weights2 <- glm(I(treat==3) ~ 1, 
                data = white_dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(white_dat$treat !=3, ((1-.fitted)/(1-den2)), ((.fitted)/(den2))))

## Basically just weight the data according to the propensity of receiving the treatment. Then weight in the mdoel 1/pr(treatment).
## We're just weighting "up" those observations that (due to non-random assignment) are less likely to be observed in the data. 
## the weight is standardized by the proportion of cases in each treatment condition.


### Append data with new weights ###
white_dat$weights1 =  weights1$wts
white_dat$weights2 =  weights2$wts
white_dat$pre      =  ifelse(white_dat$treat==1, 1, 0)
white_dat$post1    =  ifelse(white_dat$treat==2, 1, 0)
white_dat$post2    =  ifelse(white_dat$treat==3, 1, 0)
white_dat$TT1      =  white_dat$trump_vote  * white_dat$post1 ### Interactions for winner and loser effects.
white_dat$TT2      =  white_dat$trump_vote  * white_dat$post2

out_weights   = boot(lavaan_sim_predict, R = 500, 
                     ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, 
                     parallel = "multicore", 
                     ncpus = 6,
                     data = white_dat)
save(out_weights, file = "outweights_white.rda")


### For main winner and loser effects
plot_data_generator_all = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975))
  return(plot_dat)
}

### This generates the plotting data frame######
output = out_weights %>% plot_data_generator_all() 
output$voter[grepl("trump", names(out_weights$t0))] = "Trump Voter"
output$voter[grepl("biden", names(out_weights$t0))] = "Biden Voter"
output$treat[grepl("_dif", names(out_weights$t0))] =  "Treatment Effect"
output$treat = if_else(is.na(output$treat), "Point Estimate", output$treat)
output$type = rep(c("High Cost", 
                    "Low Cost", 
                    "Concern", 
                    "Trust(Federal)", 
                    "Trust(State)", 
                    "Ballot Confidence"), each = 6)
output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Uncertainty_Treat", "Uncertainty_Treat"), times = 6),
                  rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Post_Treat", "Post_Treat"), times = 6)) 


library(patchwork)
part1 =   
  subset(output, type == "Low Cost" | type == "High Cost")  %>% 
  subset(treat == "Point Estimate") %>%
  mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~type, nrow = 2)+
  geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.25, position = position_dodge(width = 0.2))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.2))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text=element_text(size=8),
        axis.text.x = element_text(angle=0),
        plot.title = element_text(color="black", size=12)) 

output$treatment[grepl("Uncertainty_", output$period)] =    "Uncertainty"
output$treatment[grepl("Post_", output$period)] =    "Post-Call"

part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
  subset(treat == "Treatment Effect") %>%
  mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=treatment, colour=voter)) + 
  facet_wrap(~type, nrow = 2) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  scale_fill_grey() +
  coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) +
  ggtitle("Winner/Loser Effects. Non-Hispanic White Respondents") +
  scale_x_discrete("", position = "top")+
  scale_y_continuous("Marginal Effect")+
  geom_hline(yintercept = 0) 

png(filename = "Figures/figure_white.png",
      res = 100, width = 600, height = 600)
part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects\nNon-Hispanic White Respondents',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)
dev.off()

### By State  ###
### Wrapper above functinos

contested_dat = subset(dat, AZ == 1 | NV ==1)
uncontested_dat = subset(dat, !c(AZ == 1 | NV ==1))

state_effects = function(data = white_dat){
  weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female +  college + ideology + christian + sdo + latino + black) ## Probability of treatment 
  weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                         age + female + college + ideology + christian + sdo + latino + black)  ### Probability of treatment 2
  ### Declare which data are to be treated as ordinal #
  ordinal_data = c(             "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", 
                              "participation_protest")
  #### Model estimates ###
  ### Here's how I generate the weights
  # Predict the probability of each treatment, conditional on some covariates -- models above...
  ## This is pretty good: https://towardsdatascience.com/doubly-robust-estimators-for-causal-inference-in-statistical-estimation-3c00847e9db
  den1 <- glm(weight_1, 
            data = data, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(weight_2, 
            data = data, family=binomial("logit")) %>% predict(type = "response")


weights1 <- glm(I(treat==2) ~ 1, 
                data = data, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(data$treat !=2, ((1-.fitted)/(1-den1)), ((.fitted)/(den1))))

weights2 <- glm(I(treat==3) ~ 1, 
                data = data, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(data$treat !=3, ((1-.fitted)/(1-den2)), ((.fitted)/(den2))))

## Basically just weight the data according to the propensity of receiving the treatment. Then weight in the mdoel 1/pr(treatment).
## We're just weighting "up" those observations that (due to non-random assignment) are less likely to be observed in the data. 
## the weight is standardized by the proportion of cases in each treatment condition.


### Append data with new weights ###
data$weights1 =  weights1$wts
data$weights2 =  weights2$wts
data$pre      =  ifelse(data$treat==1, 1, 0)
data$post1    =  ifelse(data$treat==2, 1, 0)
data$post2    =  ifelse(data$treat==3, 1, 0)
data$TT1      =  data$trump_vote  * data$post1 ### Interactions for winner and loser effects.
data$TT2      =  data$trump_vote  * data$post2

out_weights   = boot(lavaan_sim_predict, R = 500, 
                     ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, 
                     parallel = "multicore", 
                     ncpus = 6,
                     data = data)
 return(out_weights)
}
con = state_effects(data = contested_dat)
un = state_effects(data = uncontested_dat)
      

state_figure = function(output = az, label = "Arizona, Nevada"){
  output = output %>% plot_data_generator_all() 
  output$voter[grepl("trump", names(out_weights$t0))] = "Trump Voter"
  output$voter[grepl("biden", names(out_weights$t0))] = "Biden Voter"
  output$treat[grepl("_dif", names(out_weights$t0))] =  "Treatment Effect"
  output$treat = if_else(is.na(output$treat), "Point Estimate", output$treat)
  output$type = rep(c("High Cost", 
                      "Low Cost", 
                      "Concern", 
                      "Trust(Federal)", 
                      "Trust(State)", 
                      "Ballot Confidence"), each = 6)
  output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Uncertainty_Treat", "Uncertainty_Treat"), times = 6),
                    rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Post_Treat", "Post_Treat"), times = 6)) 
  
  
  library(patchwork)
  part1 =   
    subset(output, type == "Low Cost" | type == "High Cost")  %>% 
    subset(treat == "Point Estimate") %>%
    mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
    ggplot(aes(y=middle, ymin=lo, ymax=hi, 
               x=period, colour=voter)) + 
    facet_wrap (~type, nrow = 2)+
    geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.25, position = position_dodge(width = 0.2))+
    scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
    #scale_fill_grey() +
    geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.2))+
    #theme_bw() + 
    coord_flip() + 
    ggtitle("Latent Variable Support") +
    #  labs(caption="2020 Western States Study")+
    scale_y_continuous("Point Estimate")  +
    scale_x_discrete("")  +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          #axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text=element_text(size=8),
          axis.text.x = element_text(angle=0),
          plot.title = element_text(color="black", size=12)) 
  
  output$treatment[grepl("Uncertainty_", output$period)] =    "Uncertainty"
  output$treatment[grepl("Post_", output$period)] =    "Post-Call"
  
  part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
    subset(treat == "Treatment Effect") %>%
    mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
    ggplot(aes(y=middle, ymin=lo, ymax=hi, 
               x=treatment, colour=voter)) + 
    facet_wrap(~type, nrow = 2) + 
    geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
    geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
    scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
    scale_fill_grey() +
    coord_flip()+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          #axis.title = element_blank(),
          # axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_text(angle=0),
          axis.text=element_text(size=8),
          plot.title = element_text(color="black", size=12)) +
    ggtitle("Winner/Loser Effects. ") +
    scale_x_discrete("", position = "top")+
    scale_y_continuous("Marginal Effect")+
    geom_hline(yintercept = 0) 
  
return(part1 + part2 + plot_annotation(
    title = paste0('Electoral Contestation, Voting, and Winner-Loser Effects', '\n', label),
    caption = "2020 Western States Study",
    theme = theme(plot.title = element_text(size = 14))
  )
)
}

png(filename = "Figures/Az.png",
      res = 100, width = 600, height = 600)
state_figure(con, label = "Arizona, Nevada")
dev.off()


png(filename = "Figures/figure_un.png",
      res = 100, width = 600, height = 600)
state_figure(output = un, label = "Utah, New Mexico, Colorado")
dev.off()

# png(filename = "Figures/figure_co.png",
#       res = 100, width = 600, height = 600)
# state_figure(output = co, label = "Colorado")
# dev.off()

