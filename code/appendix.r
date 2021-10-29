### Appendix ####
#### October 27, 2021. 
library(boot)
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

### This is the primary function without weights. 
### TD: function option
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

### For main winner and loser effects
plot_data_generator_all = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975))
  return(plot_dat)
}


out_weights_no   = boot(dat, lavaan_sim_predict_no, R = 500, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, parallel = "multicore", ncpus = 6)
#save(out_weights, file = "outweights.rda")


### This generates the plotting data frame######
output = out_nweights %>% plot_data_generator_all() 


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
                  rep(c("Pre", "Post", "Pre", "Post", "Post_Treat", "Post_Treat"), times = 6)) 


library(patchwork)
part1 =   
    subset(output, type == "Low Cost" | type == "High Cost")  %>% 
    subset(treat == "Point Estimate") %>%
    mutate(period = factor(period, c("Post", "Uncertainty", "Pre"))) %>%
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
output$treatment[grepl("Post_", output$period)]        =    "Post-Election"

part2 =   subset(output, type == "High Cost" | type == "Low Cost")  %>% 
  subset(treat == "Treatment Effect") %>%
  mutate(treatment = factor(treatment, c("Post-Election", "Uncertainty"))) %>%
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

part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects (No Weights)',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)
  
dev.copy(png,'figure_latent_noweights.png',
         width = 750, height = 500,)
dev.off()

### Reestimate marginal models with linear regression and IPW weights.
variables = c("hard", "soft")
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

(part1 + part2) +
  plot_layout(guides='collect')+ plot_annotation(
    title = "Robustness, Linear Regression",
    #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = "2020 Western States Study",
    theme = theme(plot.title = element_text(size = 14),
                  legend.position = "bottom")
  )

dev.copy(png,'robustness_linear.png',
         width = 750, height = 500)
dev.off()


####################################
