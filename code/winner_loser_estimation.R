### Estimate a marginal structural model in lavaan ####


###dat$trust_federal = with(dat, rowMeans(cbind(trust_sc, trust_president, trust_congress), na.rm= T)) %>% zero.one()
dat$trust_federal = with(dat, rowMeans(cbind(trust_sc, trust_president, trust_congress), na.rm= T)) %>% zero.one()
dat$trust_state =   with(dat, rowMeans(cbind(trust_governor, trust_stateleg), na.rm= T)) %>% zero.one()
#### These are the propensity weights
weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + sdo + as.factor(state)) ## Probability of treatment 1
weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + sdo + as.factor(state))  ### Probability of treatment 2

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
            data = dat, family=binomial("logit")) %>% predict(type = "response")
den2 <- glm(weight_2, 
            data = dat, family=binomial("logit")) %>% predict(type = "response")


weights1 <- glm(I(treat==2) ~ 1, 
                data = dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = ifelse(dat$treat !=2, ((1-.fitted)/(1-den1)), ((.fitted)/(den1))))

weights2 <- glm(I(treat==3) ~ 1, 
                data = dat, family=binomial("logit")) %>% 
                augment(type.predict = "response") %>%
                mutate(wts = ifelse(dat$treat !=3, ((1-.fitted)/(1-den2)), ((.fitted)/(den2))))

## Basically just weight the data according to the propensity of receiving the treatment. Then weight in the mdoel 1/pr(treatment).
## We're just weighting "up" those observations that (due to non-random assignment) are less likely to be observed in the data. 
## the weight is standardized by the proportion of cases in each treatment condition.


### Append data with new weights ###
dat$weights1 = weights1$wts
dat$weights2 = weights2$wts
dat$pre =    ifelse(dat$treat==1, 1, 0)
dat$post1 =  ifelse(dat$treat==2, 1, 0)
dat$post2 =  ifelse(dat$treat==3, 1, 0)
dat$TT1 = dat$trump_vote  * dat$post1 ### Interactions for winner and loser effects.
dat$TT2 = dat$trump_vote  * dat$post2



cfa(lavaan_model_causal_treat, ordered=ordinal_data, 
    data=latino_dat)  %>% summary(fit.measures = TRUE)  ### This prints the output from the primary model used in the paper...

### Lavaan sim does all the processing/estimation. 
### Here is how to run the boostrapped simulations, this saves a boot object -- 
####  which I process for figures just below
### We can just boostrap these effects. So sample from the observed ata. 
### I wrote a function to do this in helper functions. Basically, I'm just estimating
### These models on R number of datasets (I've been doing 100, but that can be changed)
### the function is called lavaan_sim

out_weights   = boot(dat, lavaan_sim_predict, R = 500, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal, parallel = "multicore", ncpus = 6)
save(out_weights, file = "outweights.rda")

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

part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)

dev.copy(png,'figure_latent.png',
         width = 750, height = 500,)
dev.off()


#load("outweights.rda")

# Input = data frame, lavaan_sim -- which takes input:
# which variables are ordinal
# the lavaan model formula
# the propensity score weights.

# It does take a bit:

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

part1 + part2 + plot_annotation(
  title = 'Electoral Contestation, Voting, and Winner-Loser Effects',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)
  
dev.copy(png,'figure_latent.png',
         width = 750, height = 500,)
dev.off()


### Generate the Placebo Estimates ####

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
output$period = c(rep(c("Pre", "Uncertainty", "Pre", "Uncertainty", "Trump_t1", "Biden_t1"), times = 6), 
                  rep(c("Pre", "Post-Call", "Pre", "Post-Call", "Trump_t2", "Biden_t2"), times = 6))


part1 =   
  subset(output, !(type == "High Cost" | type == "Low Cost") ) %>% 
  subset(treat == "Point Estimate") %>%
  mutate(period = factor(period, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=period, colour=voter)) + 
  facet_wrap (~type, nrow = 4)+
  geom_linerange(aes(x=period, ymax=middle, ymin=-2), size = 1.1, 
                 position = position_dodge(width = 0.45))+
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  #scale_fill_grey() +
  geom_point(aes(y=middle,  x=period, colour=voter), alpha=0.7, size = 2.1, position = position_dodge(width = 0.45))+
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Latent Variable Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        #axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) 

output$treatment[grepl("_t1", output$period)] =    "Uncertainty"
output$treatment[grepl("_t2", output$period)] =    "Post-Call"


part2 =   subset(output, !(type == "Low Cost" | type == "High Cost") ) %>% 
  subset(treat == "Treatment Effect", nrows =4) %>%
  mutate(treatment = factor(treatment, c("Post-Call", "Uncertainty"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=treatment, colour=voter)) + 
  facet_wrap(~type, nrow = 4) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.2), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  position=position_dodge(width = 0.2), size =1.2) +
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  scale_fill_grey() +
  coord_flip()+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        #axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        #axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) +
  ggtitle("Winner/Loser Effects") +
  scale_x_discrete("", position = "bottom")+
  scale_y_continuous("Marginal Effect")+
  geom_hline(yintercept = 0) 

part1 + part2 + plot_annotation(
  title = 'Electoral Confidence, Concern, and Trust',
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14))
)


dev.copy(png,'figure_latent_placebo.png',
         width = 750, height = 500,)
dev.off()

#### Generate point estimates 

#### By variable ###


individual.variables = list()
vars = c("violent", "burn", "recount", "criticize", "court")

#vars = c("recount")

for(var in vars){
  individual.variables[[var]]   = boot(dat, treat.effects.ordinal, R = 500, 
                       dependent.variable = var, type = "point", 
                       weighting = "TRUE", parallel = "multicore")
}
individual.variables_me = list()
for(var in vars){
  individual.variables_me[[var]]   = boot(dat, treat.effects.ordinal, R = 500, 
                                       dependent.variable = var, type = "marginal", 
                                       weighting = "TRUE", parallel = "multicore")
}

### Save Data ###
save(individual.variables   , file = "individual_variables.rda")
save(individual.variables_me, file = "individual_variables_me.rda")
load("individual_variables.rda")
load("individual_variables_me.rda")

output = individual.variables[[1]] %>% plot_data_generator_all() 
for(i in 2:5){
  output = rbind(output, individual.variables[[i]] %>% plot_data_generator_all())
}



output$election =    rep(c("Pre", "Uncertainty", "Post-Call"), each = 2)
output$voter    =    rep(c("Trump Voter", "Biden Voter"))
output$item    =    rep(c("Protest", "Flag Burn", "Recount", "Criticize", "Court"),  each = nrow(output)/5)


part1a =  #subset(output, item == "Court") %>% o
  #mutate(response = factor(response, c("Support", "Neutral", "Oppose"))) %>%
  output %>%
  mutate(election = factor(election, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=election, colour=voter)) + 
  facet_wrap (~item, nrow = 5)+
  geom_linerange(aes(x=election, ymax=middle, ymin = 0), size = 1.25, 
                 position = position_dodge(width = 0.9)) + 
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  geom_point(aes(x = election, y = middle), position = position_dodge(width = 0.9), size = 3, alpha =0.4) + 
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Probability of Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12)) 




output = individual.variables_me[[1]] %>% plot_data_generator_all() 
for(i in 2:5){
  output = rbind(output, individual.variables_me[[i]] %>% plot_data_generator_all())
}

output$election =    rep(c( "Uncertainty", "Post-Call"))
output$voter    =    rep(c("Trump Voter", "Biden Voter"), each = 2)
output$item    =    rep(c("Protest", "Flag Burn", "Recount", "Criticize", "Court"),  each = nrow(output)/5)


part2a =   output %>%
#subset(output, item == "Court")  %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=election, colour=voter)) + 
  facet_wrap(~item, nrow = 5) + 
  geom_point(size = 2.3, position=position_dodge(width = 0.9), alpha = 0.3) +
  geom_errorbar( width = 0.01, alpha = 0.9,  
                position=position_dodge(width = 0.9), size =1.2) +
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
  


(part1a + part2a) +
  plot_layout(guides='collect')+ plot_annotation(
  title = "Winner/Loser Effects, by Item",
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14),
                legend.position = "bottom")
)

dev.copy(png,'individual_items.png',
         width = 750, height = 500)
dev.off()




vars = c("recount")
for(var in vars){
  individual.variables[[var]]   = boot(dat, treat.effects.ordinal, R = 500, 
                       dependent.variable = var, type = "point", 
                       weighting = "TRUE", parallel = "multicore")
}
individual.variables_me = list()
for(var in vars){
  individual.variables_me[[var]]   = boot(dat, treat.effects.ordinal, R = 500, 
                                       dependent.variable = var, type = "marginal", 
                                       weighting = "TRUE", parallel = "multicore")
}

output = individual.variables[[1]] %>% plot_data_generator_all() 
output$election =    rep(c("Pre", "Uncertainty", "Post-Call"), each = 2)
output$voter    =    rep(c("Trump Voter", "Biden Voter"))


part1a =  #subset(output, item == "Court") %>% o
  #mutate(response = factor(response, c("Support", "Neutral", "Oppose"))) %>%
  output %>%
  mutate(election = factor(election, c("Post-Call", "Uncertainty", "Pre"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=election, colour=voter)) + 
  geom_linerange(aes(x=election, ymax=middle, ymin = 0), size = 1.25, 
                 position = position_dodge(width = 0.6)) + 
  scale_colour_manual(name="Voter", values=c("#0000ffe0", "#c20505"))+
  geom_point(aes(x = election, y = middle), position = position_dodge(width = 0.6), size = 3, alpha =0.4) + 
  #theme_bw() + 
  coord_flip() + 
  ggtitle("Probability of Support") +
  #  labs(caption="2020 Western States Study")+
  scale_y_continuous("Point Estimate")  +
  scale_x_discrete("")  +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        #axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle=0),
        axis.text=element_text(size=8),
        plot.title = element_text(color="black", size=12))   


output = individual.variables_me[[1]] %>% plot_data_generator_all() 

output$election =    rep(c( "Uncertainty", "Post-Call"))
output$voter    =    rep(c("Trump Voter", "Biden Voter"), each = 2)


part2a =   output %>%
#subset(output, item == "Court")  %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, 
             x=election, colour=voter)) + 
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
  


(part1a + part2a) +
  plot_layout(guides='collect')+ plot_annotation(
  title = "Support Ballot Recounts",
  #subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
  caption = "2020 Western States Study",
  theme = theme(plot.title = element_text(size = 14),
                legend.position = "bottom")
)

dev.copy(png,'ballot.png',
         width = 500, height = 200)
dev.off()


#### Robustness Checks #######
## Balance
install.packages("WeightIt")
library("WeightIt")

(w1 = weightit(weight_1, data = dat, estimand = "ATE", ps = "weights1" ))
(w2 = weightit(weight_2, data = dat, estimand = "ATE", ps = "weights2" ))

bal.tab(weight_1, data = dat, weights = "att.weights")

bal.tab(dat[,c("authoritarianism", "rr", "trump_vote", "rwm", 
               "age", "female", "latino", "black", "college", 
               "ideology", "christian", "sdo", "state")] , treat = (dat$treat == 2), weights = dat$weights1)
covs = dat[,c("authoritarianism", "rr", "trump_vote", "rwm", 
               "age", "female", "latino", "black", "college", 
               "ideology", "christian", "sdo", "state")]

dat$treat1 = (dat$treat == 2)
dat$treat2 = (dat$treat == 3)

m.out1 <- matchit(f.build("treat1", covs), data = dat, 
                 method = "nearest", replace = TRUE)
m.out2 <- matchit(f.build("treat2", covs), data = dat, 
                 method = "nearest", replace = TRUE)

love.plot(m.out1, binary = "std",  line = TRUE, thresholds = c(m = .1),
title = "Balance Statistics, Weighted and Unweighted")

dev.copy(png,'balance1.png',
         width = 750, height = 500)
dev.off()

love.plot(m.out2, binary = "std",  line = TRUE, thresholds = c(m = .1),
title = "Balance Statistics, Weighted and Unweighted")


dev.copy(png,'balance2.png',
         width = 750, height = 500)
dev.off()

















#### Placebo tests
out_weights %>% plot_data_generator() %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  subset(!(type == "Soft" | type == "Hard")) %>% 
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type, scale = "free")+
  geom_point(size =0.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Vote", values=c("black", "grey"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Winner and Loser Effects \n(Confidence, Election Integrity, Government Trust)") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Average Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0, linetype = "dotdash") 


dev.copy(png,'section3_2_FINAL.jpg',
         width = 750, height = 500)
dev.off()



#### By variable ###
treat.effects = function(data = dat, indices, dependent.variable = "violent", 
                         point_estimate = TRUE){
  .df <- data[indices, ]
  model = polr(paste0("as.factor(", dependent.variable, ")~", "trump_vote + post1 + post2 + TT1 + TT2" ), data = .df) 
  tempdat1 = model.matrix(model)[,-1] %>% data.frame()
  permutes = paste0(c("y0", "y1", "y2"), "_", rep(c("trump", "biden"), 3) )
  assign(permutes[1], tempdat1 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 0) %>% 
           mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix()) 
  
  assign(permutes[2],  tempdat1 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 0) %>% 
           mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() )
  
  assign(permutes[3], tempdat1 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 1) %>% 
           mutate(post2 = 0)  %>% mutate(TT1 = 1)  %>% mutate(TT2 = 0) %>% as.matrix() )
  
  assign(permutes[4], tempdat1 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 1) %>% 
           mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() )
  
  assign(permutes[5], tempdat1 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 0) %>% 
           mutate(post2 = 1)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 1) %>% as.matrix() )
  
  assign(permutes[6], tempdat1 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 0) %>% 
           mutate(post2 = 1)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() )
  ## Estimate agreement
  output = c(
    trump_pre =  1 - plogis(model$zeta[3] - y0_trump[1,] %*% model$coefficients),
    biden_pre =  1 - plogis(model$zeta[3] - y0_biden[1,] %*% model$coefficients),
    trump_unc =  1 - plogis(model$zeta[3] - y1_trump[1,] %*% model$coefficients),
    biden_unc  = 1 - plogis(model$zeta[3] - y1_biden[1,] %*% model$coefficients),
    trump_post = 1 - plogis(model$zeta[3] - y2_trump[1,] %*% model$coefficients),
    biden_post = 1 - plogis(model$zeta[3] - y2_biden[1,] %*% model$coefficients)
  )
  me = c(output[3] - output[1],
         output[4] - output[2],
         output[5] - output[1],
         output[6] - output[2])
  if(point_estimate == TRUE){
    return(output)
  }
  if(point_estimate == FALSE){
    return(me)
  }
}




individual.variables = list()
vars = c("violent", "burn", "recount", "criticize", "court")
for(i in 1:5){
  individual.variables[[i]] =   boot(dat, treat.effects, R = 100, dependent.variable  = vars[i])
  print(vars[i])
}

tmp1 = tmp2 = tmp3 = tmp4 = c()
for(i in 1:5){
  tmp1 =  rbind(tmp1, apply(individual.variables[[i]]$t, 2, quantile, 0.50  ))
  tmp2 =  rbind(tmp2, apply(individual.variables[[i]]$t, 2, quantile, 0.975 ))
  tmp3 =  rbind(tmp3, apply(individual.variables[[i]]$t, 2, quantile, 0.025 ))
}

plot_dat = data.frame(min = c(tmp3), max = c(tmp2), mean = c(tmp1))

plot_dat$variable = rep(c("Protest Election", "Burn Flag", "Support Recount", "Openly Criticize", "Challenge in Court"))
plot_dat$voter    = rep(c("Trump", "Biden"), each = 5)
plot_dat$treatment    = rep(c("Uncertainty", "Post"), each = nrow(plot_dat)/2)
plot_dat$treatment   =  factor(plot_dat$treatment, c("Uncertainty", "Post"))

ggplot(plot_dat, aes(y=mean, ymin=min, ymax=max, x=treatment, colour = voter, group = voter)) + 
  facet_wrap (~variable)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Vote", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation and winner/loser effects") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-0.40, 0.40)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 


dev.copy(png,'treatment_byvar.jpg',
         width = 750, height = 500,)
dev.off()



###################################################################################################
############ State Heterogeneity ##################################################################
###################################################################################################
### I can't get this to work:

### This throws some strange error messages
### Estimate a three group model, constrain loadings, but allow coefficients to vary.

# lavaan_model_state ="
#             ### Measurement Modeld
#               hard_contestation          =~ violent + burn
#               
#               soft_contestation        =~  recount + criticize + court
# 
#               ### Structural Model  
#               
#               hard_contestation          ~ trump_vote + post1 + post2 + TT1 + TT2
#               soft_contestation          ~ trump_vote + post1 + post2 + TT1 + TT2
# "
# 
# ordinal_data = c(             "violent", "burn", "court", "recount", "criticize")
# 
# lavaan_state = function(dat, indices, ordinal_data = ordinal_data,
#                       lavaan_model = lavaan_model_state){
#   .df <- dat[indices, ]
#   
#   model1 = cfa(lavaan_model_state, ordered=ordinal_data, 
#                data=.df, group = "state",  sampling.weights = "weights1",
#                group.equal = c("loadings", "thresholds", "means"
#                                ))
#   
#   model2 = cfa(lavaan_model_state, ordered=ordinal_data, 
#                data=.df, group = "state",  sampling.weights = "weights2",
#                group.equal = c("loadings", "thresholds", "means"
#                ))
#   mod1 = coef(model1)[grep("hard_contestation", names(coef(model1)))]
#   mod2 = coef(model2)[grep("hard_contestation", names(coef(model2)))]
#   hard1 = c(
#     mod1[grep("trump_vote", names(mod1))],
#     mod1[grep("post1", names(mod1))],
#     mod1[grep("post2", names(mod1))],
#     mod1[grep("TT1", names(mod1))],
#     mod1[grep("TT2", names(mod1))]
#   )
#   hard2 = c(
#     mod2[grep("trump_vote", names(mod2))],
#     mod2[grep("post1", names(mod2))],
#     mod2[grep("post2", names(mod2))],
#     mod2[grep("TT1", names(mod2))],
#     mod2[grep("TT2", names(mod2))]
#   )
#   mod1 = coef(model1)[grep("soft_contestation", names(coef(model1)))]
#   mod2 = coef(model2)[grep("soft_contestation", names(coef(model2)))]
#   
#   soft1 = c(
#     mod1[grep("trump_vote", names(mod1))],
#     mod1[grep("post1", names(mod1))],
#     mod1[grep("post2", names(mod1))],
#     mod1[grep("TT1", names(mod1))],
#     mod1[grep("TT2", names(mod1))]
#   )
#   soft2 = c(
#     mod2[grep("trump_vote", names(mod2))],
#     mod2[grep("post1", names(mod2))],
#     mod2[grep("post2", names(mod2))],
#     mod2[grep("TT1", names(mod2))],
#     mod2[grep("TT2", names(mod2))])
#   
#   temp_dat = data.frame(hard1 = hard1, soft1 = soft1, hard2 = hard2, soft2 = soft2)
#   temp_dat$group <- rep(c(1,2,3,4,5))
#   
#   # Form Predictions by state
#   for (i in 1:5){
#     ba = c(0, 0, 0, 0, 0) %*% as.matrix(subset(temp_dat, group ==i))
#     ta = c(1, 0, 0, 0, 0) %*% as.matrix(subset(temp_dat, group ==i))
#     bb = c(0, 1, 0, 0, 0) %*% as.matrix(subset(temp_dat, group ==i))
#     tb = c(1, 1, 0, 1, 0) %*% as.matrix(subset(temp_dat, group ==i))
#     bc = c(0, 0, 1, 0, 0) %*% as.matrix(subset(temp_dat, group ==i))
#     tc = c(1, 0, 1, 0, 1) %*% as.matrix(subset(temp_dat, group ==i))
#     
#     assign(paste0("trump_", i), tb - ta)
#     assign(paste0("biden_", i), bb - ba)
#    
#     assign(paste0("trump2_", i), tc - ta)
#     assign(paste0("biden2_", i), bc - ba)
#     
#   }
#   c(trump_1[1:4], trump_2[1:4], trump_3[1:4], trump_4[1:4], trump_5[1:4],
#     biden_1[1:4], biden_2[1:4], biden_3[1:4], biden_4[1:4], biden_5[1:4],
#     trump2_1[1:4], trump2_2[1:4], trump2_3[1:4], trump2_4[1:4], trump2_5[1:4],
#     biden2_1[1:4], biden2_2[1:4], biden2_3[1:4], biden2_4[1:4], biden2_5[1:4]
#     
#     
#   )}
# 
# 
# out_weights_state   = boot(dat, lavaan_state, R = 3, 
#                            ordinal_data = ordinal_data,
#                      lavaan_model = lavaan_model_state)
# 
# plot_data_generator_states = function(output){
#   plot_dat = data.frame(
#     middle = apply(output$t, 2, quantile, 0.5),
#     hi = apply(output$t, 2, quantile, 0.025),
#     lo = apply(output$t, 2, quantile, 0.975),
#     voter = rep(c("Trump", "Biden"), each = 20 ),
#     type = rep(c("Hard", "Soft")),
#     period = rep(c("Uncertainty", "Post_Call"), each = 40),
#     state = rep(c("Arizona", "Colorado", "New Mexico", "Nevada", "Utah"), each = 4))
#   return(plot_dat)
# }
# 
# out_weights_state %>% plot_data_generator_states() %>% 
#   mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
#   ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
#   facet_wrap (~type + state, scale = "free")+
#   geom_point(size =0.7) +
#   geom_errorbar( width = 0.01, alpha = 0.9) +
#   scale_colour_manual(name="Vote", values=c("blue", "red"))+
#   scale_fill_grey() +
#   theme_bw() + 
#   theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
#   ggtitle("Effects Disaggregated by State") +
#   labs(caption="2020 Western States Study. Estimates are from covariance structure model")+
#   scale_y_continuous("Marginal Effect") + 
#   scale_x_discrete("") +
#   geom_hline(yintercept = 0, linetype = "dotdash") 
# 
# 
# 
# dat$AZ  = ifelse(dat$state == 1, 1, 0)
# dat$COL = ifelse(dat$state == 2, 1, 0)
# dat$NV  = ifelse(dat$state == 3, 1, 0)
# dat$NM  = ifelse(dat$state == 4, 1, 0)
# dat$UT  = ifelse(dat$state == 5, 1, 0)

robustness_linear1   = boot(subset(dat, state ==1), winner_loser, R = 100, outcome = "hard" )
robustness_linear2   = boot(subset(dat, state ==2), winner_loser, R = 100, outcome = "hard" )
robustness_linear3   = boot(subset(dat, state ==3), winner_loser, R = 100, outcome = "hard" )
robustness_linear4   = boot(subset(dat, state ==4), winner_loser, R = 100, outcome = "hard" )
robustness_linear5   = boot(subset(dat, state ==5), winner_loser, R = 100, outcome = "hard" )

plot_dat1 = rbind(
        robustness_linear1 %>% plot_data_generator_state(),
        robustness_linear2 %>% plot_data_generator_state(),
        robustness_linear3 %>% plot_data_generator_state(),
        robustness_linear4 %>% plot_data_generator_state(),
        robustness_linear5 %>% plot_data_generator_state()) %>%  
  data.frame() %>% mutate(state = rep(c("Arizona", "Colorado", "New Mexico", "Nevada", "Utah"), each =4))
plot_dat1$type = "Hard"

plot_dat2 = rbind(
  robustness_linear1 %>% plot_data_generator_state(),
  robustness_linear2 %>% plot_data_generator_state(),
  robustness_linear3 %>% plot_data_generator_state(),
  robustness_linear4 %>% plot_data_generator_state(),
  robustness_linear5 %>% plot_data_generator_state()) %>%  
  data.frame() %>% mutate(state = rep(c("Arizona", "Colorado", "New Mexico", "Nevada", "Utah"), each =4))
plot_dat2$type = "Soft"

rbind(plot_dat1, plot_dat2) %>%
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  mutate(state = factor(state, c("Arizona", "Colorado", "New Mexico", "Nevada", "Utah"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~state + type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation and winner/loser effects") +
  labs(caption="2020 Western States Study. ")+
  scale_y_continuous("Marginal Effect", limits = c(-1.25, 1.25)) + 
  geom_hline(yintercept = 0, linetype = "dotdash") 


dev.copy(png,'sectionstate_robustness.jpg',
         width = 750, height = 500)
dev.off()

###### Truncated Effects ######
robustness_linear1   = boot(subset(dat, white == 1), winner_loser, R = 100, outcome = "hard" )
robustness_linear2   = boot(subset(dat, latino ==1), winner_loser, R = 100, outcome = "hard" )

plot_dat1 = rbind(
  robustness_linear1 %>% plot_data_generator_state(),
  robustness_linear2 %>% plot_data_generator_state()) %>%
  data.frame() %>% mutate(race = rep(c("White", "Latino"), each =4))
plot_dat1$type = "Hard"

plot_dat2 = rbind(
  robustness_linear1 %>% plot_data_generator_state(),
  robustness_linear2 %>% plot_data_generator_state()) %>%
  data.frame() %>% mutate(race = rep(c("White", "Latino"), each =4))
plot_dat2$type = "Soft"

rbind(plot_dat1, plot_dat2) %>%
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  mutate(race = factor(race, c("White", "Latino"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~race + type)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation and winner/loser effects") +
  labs(caption="2020 Western States Study. ")+
  scale_y_continuous("Marginal Effect", limits = c(-1.25, 1.25)) + 
  geom_hline(yintercept = 0, linetype = "dotdash") 

dev.copy(png,'sectionrace_robustness.jpg',
         width = 750, height = 500)
dev.off()


