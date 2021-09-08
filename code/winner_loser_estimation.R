### Estimate a marginal structural model in lavaan ####
#### July 21, 2021. 
library(boot)
library(dplyr)

### Here is where you set parameter input ##
### Specify a model for the weight equations (this is used for the standardized inverse propensity weights)
### Specify the lavaan model. 

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


cfa(lavaan_model, ordered=ordinal_data, 
    data=dat)  %>% summary(fit.measures = TRUE)  ### This prints the output from the primary model used in the paper...

### Lavaan sim does all the processing/estimation. 
### Here is how to run the boostrapped simulations, this saves a boot object -- 
####  which I process for figures just below
### We can just boostrap these effects. So sample from the observed ata. 
### I wrote a function to do this in helper functions. Basically, I'm just estimating
### These models on R number of datasets (I've been doing 100, but that can be changed)
### the function is called lavaan_sim

out_weights   = boot(dat, lavaan_sim, R = 1000, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model_causal)

# Input = data frame, lavaan_sim -- which takes input:
# which variables are ordinal
# the lavaan model formula
# the propensity score weights.

# It does take a bit:

## This produces a lot of ouput, which I clean in plot_data_generator (also in helper functions)
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
  ggtitle("Electoral contestation and winner/loser effects") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'treatment_1_final.jpg',
         width = 750, height = 500,)
dev.off()

### Just some linear regressions:
dat$soft <- rowMeans(cbind(dat$recount, dat$criticize, dat$court), na.rm = T)
dat$hard <- rowMeans(cbind(dat$burn, dat$violent), na.rm = T)

### Combine with sem_out
robustness_linear1   = boot(dat, winner_loser, R = 1000, outcome = "hard" )
robustness_linear2   = boot(dat, winner_loser, R = 1000, outcome = "soft" ) ### You can change the outcome, make sure it's 0-1

sem_out = out_weights %>% plot_data_generator() %>% subset(type == "Soft" | type == "Hard")
sem_out$estimator = "MSM-SEM,IPW"
msm1_out = robustness_linear1 %>% plot_data_linear() 
msm1_out$estimator = "MSM-LM,IPW"
msm1_out$type = "Hard"
msm2_out = robustness_linear2 %>% plot_data_linear() 
msm2_out$estimator = "MSM-LM,IPW"
msm2_out$type = "Soft"
combined  = rbind(sem_out, msm1_out, msm2_out)

combined %>%
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  facet_wrap (~type + estimator)+
  geom_point(size =1.7) +
  geom_errorbar( width = 0.01, alpha = 0.9) +
  scale_colour_manual(name="Party", values=c("gray", "black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle=0)) + 
  ggtitle("Electoral contestation and winner/loser effects") +
  labs(caption="2020 Western States Study")+
  scale_y_continuous("Average Marginal Effect", limits = c(-1.0, 1.0)) + 
  scale_x_discrete("") +
  geom_hline(yintercept = 0) 

dev.copy(png,'treatment_combined_final.jpg',
         width = 750, height = 500,)
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
treat.effects = function(data = dat, indices, dependent.variable = "violent"){
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
  
  return(me)
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


