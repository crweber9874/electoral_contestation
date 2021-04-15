####### Note: I created several files for the three way interaction ######
####### It's not the most efficient, but these models generate a lot, so I did this to keep things straight.

##### Females, Males ######


#### Feel free to change anything, but:

# Be sure all variables are scaled from 0 to 1. You can use the zero.one() function

# The ATE output can be understood as follows;

### These are the combinations to understand the output.
### Ate1 First set of outcomes in list: M1 = 0, M2 = 0 (e.g., No Surveillance, Biden)
###Ate2 Second set of outcomes in list: M1 = 0, M2 = 1 (e.g., No Surveillance, Trump)
###Ate3 Third set of outcomes in list: M1 = 1, M2 = 0 (e.g.,  Surveillance, Biden)
###Ate4 Fourth set of outcomes in list: M1 = 1, M2 = 1 (e.g.,  Surveillance, Trump)

### The way it's organized now, just change the first variable, since hte second is necessary -- winner v. loser

load("raw_data.rda")
summary(dat)
#### Pull in data ####
ITER = 2000
CHAINS = 3
#### Zero one code####
library(dplyr)

dat$strength = abs(dat$pid7 - 4) %>% zero.one()

#### Data called for estimation #####
tempdat <- dat
tempdat = tempdat %>% na.omit()
tempdat$conservatism = tempdat$ideology %>% zero.one()


model3 <- "
data {
  int<lower=0> N;
  int<lower=0> P;
  int<lower=0> Q;
  matrix[N, P] X;
  matrix[N,Q] A; 
  int K; 
  int<lower=0,upper=6> Y[N];
}
transformed data {
  vector[N] boot_probs = rep_vector(1.0/N, N);  
}
parameters {
  vector[P] alphaZ; 
  vector[Q] alphaA; 
  ordered[K - 1] c;
}
model{
  alphaA ~ normal(0, 2.5);
  alphaZ ~ normal(0, 2.5);
  Y ~ ordered_logistic(X * alphaZ + A * alphaA, c);
}

generated quantities {  
  int row_i;
  matrix[N,4] ATE_T0 = rep_matrix(0, N, 4);  //This just puts the counterfactuals in a matrix
  matrix[N,4] ATE_T1 = rep_matrix(0, N, 4);  
  real ATE_1 = 0; //Aggregate effect
  real ATE_2 = 0;
  real ATE_3 = 0;
  real ATE_4 = 0;
for (i in 1:N) {
  row_i = categorical_rng(boot_probs);
  //Generate predictions for T = 0 (e.g., surveillance)
  ATE_T0[i, 1] = X[row_i] * alphaZ; //no surveillance, biden 
  ATE_T0[i, 2] = X[row_i] * alphaZ + alphaA[3] ; //no surveillance, trump 
  ATE_T0[i, 3] = X[row_i] * alphaZ + alphaA[2]; // surveillance, biden 
  ATE_T0[i, 4] = X[row_i] * alphaZ + alphaA[3] + alphaA[2] ; // surveillance, trump 
  //Generate predictions for T = 0 (e.g., surveillance)
  ATE_T1[i, 1] = X[row_i] * alphaZ +  alphaA[1]; //no surveillance, biden, treat 
  ATE_T1[i, 2] = X[row_i] * alphaZ  + alphaA[1] + alphaA[3]; //no surveillance, trump, treat
  ATE_T1[i, 3] = X[row_i] * alphaZ +  alphaA[2] + alphaA[1]; // surveillance, biden, treat
  ATE_T1[i, 4] = X[row_i] * alphaZ +  alphaA[1] + alphaA[2] + alphaA[3]; // surveillance, trump, treat 
  
  ATE_T0[i, 1] = 1 - bernoulli_logit_rng(c[3] - ATE_T0[i, 1]);  
  ATE_T0[i, 2] = 1 - bernoulli_logit_rng(c[3] - ATE_T0[i, 2]);  
  ATE_T0[i, 3] = 1 - bernoulli_logit_rng(c[3] - ATE_T0[i, 3]);  
  ATE_T0[i, 4] = 1 - bernoulli_logit_rng(c[3] - ATE_T0[i, 4]);  
  ATE_T1[i, 1] = 1 - bernoulli_logit_rng(c[3] - ATE_T1[i, 1]);  
  ATE_T1[i, 2] = 1 - bernoulli_logit_rng(c[3] - ATE_T1[i, 2]);  
  ATE_T1[i, 3] = 1 - bernoulli_logit_rng(c[3] - ATE_T1[i, 3]);  
  ATE_T1[i, 4] = 1 - bernoulli_logit_rng(c[3] - ATE_T1[i, 4]);  
  
  ATE_1 =  ATE_1 + (ATE_T1[i, 1]-   ATE_T0[i, 1])/N;
  ATE_2 =  ATE_2 + (ATE_T1[i, 2]-   ATE_T0[i, 2])/N;
  ATE_3 =  ATE_3 + (ATE_T1[i, 3]-   ATE_T0[i, 3])/N;
  ATE_4 =  ATE_4 + (ATE_T1[i, 4]-   ATE_T0[i, 4])/N;
}
}

"
############################################################################################################
####################################  Cut and paste this block to run different models #######################
############################################################################################################
CHAINS = 3
ITER = 2000
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "surveillance",
               "latino", 
               "black",
               "college", 
               "christian",
               "conservatism",
               "disposition",
               "VIOLENT",
               "female")
IV <- c("uncertainty") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "violent"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit1 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)


IV <- c("post_call") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "violent"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit2 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

# Fit 1 = uncertainty, Fit 2 = post_call
output1<- list(
  summary(fit1, pars = "ATE_1")[["summary"]],
  summary(fit1, pars = "ATE_2")[["summary"]],
  summary(fit1, pars = "ATE_3")[["summary"]],
  summary(fit1, pars = "ATE_4")[["summary"]],
  summary(fit2, pars = "ATE_1")[["summary"]],
  summary(fit2, pars = "ATE_2")[["summary"]],
  summary(fit2, pars = "ATE_3")[["summary"]],
  summary(fit2, pars = "ATE_4")[["summary"]]
  
)
# this generates a lot of output the 2x2 treatment effect for two treatments for 5 items ####
################################################################################################################################################
################################################################################################################################################



###########################################################################################################
####################################  Cut and paste this block to run different models #######################
############################################################################################################
CHAINS = 3
ITER = 2000
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "surveillance",
               "latino", 
               "black",
               "college", 
               "christian",
               "conservatism",
               "disposition",
               "SM",
               "female")
IV <- c("uncertainty") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "criticize"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit1 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)


IV <- c("post_call") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit2 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

# Fit 1 = uncertainty, Fit 2 = post_call
output2<- list(
  summary(fit1, pars = "ATE_1")[["summary"]],
  summary(fit1, pars = "ATE_2")[["summary"]],
  summary(fit1, pars = "ATE_3")[["summary"]],
  summary(fit1, pars = "ATE_4")[["summary"]],
  summary(fit2, pars = "ATE_1")[["summary"]],
  summary(fit2, pars = "ATE_2")[["summary"]],
  summary(fit2, pars = "ATE_3")[["summary"]],
  summary(fit2, pars = "ATE_4")[["summary"]]
  
)
# this generates a lot of output the 2x2 treatment effect for two treatments for 5 items ####
################################################################################################################################################
################################################################################################################################################




############################################################################################################
####################################  Cut and paste this block to run different models #######################
############################################################################################################
CHAINS = 3
ITER = 2000
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "surveillance",
               "latino", 
               "black",
               "college", 
               "christian",
               "conservatism",
               "disposition",
               "female")
IV <- c("uncertainty") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "recount"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit1 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)


IV <- c("post_call") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit2 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

# Fit 1 = uncertainty, Fit 2 = post_call
output3<- list(
  summary(fit1, pars = "ATE_1")[["summary"]],
  summary(fit1, pars = "ATE_2")[["summary"]],
  summary(fit1, pars = "ATE_3")[["summary"]],
  summary(fit1, pars = "ATE_4")[["summary"]],
  summary(fit2, pars = "ATE_1")[["summary"]],
  summary(fit2, pars = "ATE_2")[["summary"]],
  summary(fit2, pars = "ATE_3")[["summary"]],
  summary(fit2, pars = "ATE_4")[["summary"]]
  
)
# this generates a lot of output the 2x2 treatment effect for two treatments for 5 items ####
################################################################################################################################################
################################################################################################################################################




############################################################################################################
####################################  Cut and paste this block to run different models #######################
############################################################################################################
CHAINS = 3
ITER = 2000
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "surveillance",
               "latino", 
               "black",
               "college", 
               "christian",
               "conservatism",
               "disposition",
               "female")
IV <- c("uncertainty") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "court"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit1 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)


IV <- c("post_call") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit2 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

# Fit 1 = uncertainty, Fit 2 = post_call
output4<- list(
  summary(fit1, pars = "ATE_1")[["summary"]],
  summary(fit1, pars = "ATE_2")[["summary"]],
  summary(fit1, pars = "ATE_3")[["summary"]],
  summary(fit1, pars = "ATE_4")[["summary"]],
  summary(fit2, pars = "ATE_1")[["summary"]],
  summary(fit2, pars = "ATE_2")[["summary"]],
  summary(fit2, pars = "ATE_3")[["summary"]],
  summary(fit2, pars = "ATE_4")[["summary"]]
  
)
# this generates a lot of output the 2x2 treatment effect for two treatments for 5 items ####
################################################################################################################################################
################################################################################################################################################


############################################################################################################
####################################  Cut and paste this block to run different models #######################
############################################################################################################
CHAINS = 3
ITER = 2000
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "surveillance",
               "latino", 
               "black",
               "college", 
               "christian",
               "conservatism",
               "disposition",
               "female")
IV <- c("uncertainty") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
DV <- "burn"
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit1 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)


IV <- c("post_call") ### Need to update counterfcatuals if change
MODERATORS <- c("strength" ,"trump_vote") ### Need to update counterfcatuals if change
stan_data = stan_e2(independent.variable =    IV, 
                    dependent.variable = DV, 
                    controls = CONTROLS,
                    moderators = MODERATORS,
                    data = tempdat)
fit2 = stan(model_code = model3, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

# Fit 1 = uncertainty, Fit 2 = post_call
output5<- list(
  summary(fit1, pars = "ATE_1")[["summary"]],
  summary(fit1, pars = "ATE_2")[["summary"]],
  summary(fit1, pars = "ATE_3")[["summary"]],
  summary(fit1, pars = "ATE_4")[["summary"]],
  summary(fit2, pars = "ATE_1")[["summary"]],
  summary(fit2, pars = "ATE_2")[["summary"]],
  summary(fit2, pars = "ATE_3")[["summary"]],
  summary(fit2, pars = "ATE_4")[["summary"]]
  
)
# this generates a lot of output the 2x2 treatment effect for two treatments for 5 items ####
################################################################################################################################################
################################################################################################################################################

### First set of outcomes in list: M1 = 0, M2 = 0 (e.g., No Surveillance, Biden)
### Second set of outcomes in list: M1 = 0, M2 = 1 (e.g., No Surveillance, Trump)
### Third set of outcomes in list: M1 = 1, M2 = 0 (e.g.,  Surveillance, Biden)
### Fourth set of outcomes in list: M1 = 1, M2 = 1 (e.g.,  Surveillance, Trump)


plot_dat1 = data.frame(value = c(unlist(output1), unlist(output2), unlist(output3), 
                                 unlist(output4), unlist(output5)),
                       statistic = rep(c("Mean", "SE", "SD", "Min", "Min2", "Median", "Max2", "Max", 
                                         "Neff", "rhat")),
                       marginal_effect = rep(c("Uncertainty", "Post-Call"), each =20*2),
                       strength = rep(c("weak", "strong"), each = 10*2),
                       voter = rep(c("Biden", "Trump"), each = 10),
                       question = rep(c("Protest", "Criticize", "Recount", "Court", "Burn"), each =length(unlist(output1)))) %>%
  filter(statistic == "Mean"|statistic == "Max"| statistic == "Min")







plot_dat1 =   reshape2::dcast(plot_dat1, marginal_effect + strength + voter + question ~ statistic, value.var = "value", sum ) 
plot_dat1$marginal_effect <- factor(plot_dat1$marginal_effect , levels = c("Uncertainty", "Post-Call"))

png("winner_three.png")
ggplot(plot_dat1, aes(y = as.numeric(Mean), ymin = as.numeric(Min), 
                      ymax = as.numeric(Max), x = as.factor(marginal_effect), 
                      group = as.factor(voter), colour  = as.factor(voter))) +
  facet_wrap(~question*strength, scales = "free_y") +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Trump Voter", values=c("Grey", "Black"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Conditional Treatment Effects of Election Timing") +
  labs(caption="2020 Western States Study. Estimates are from parametric g-estimation, ordered logit")+
  scale_y_continuous("ATE", limits = c(-0.2, .2)) + 
  scale_x_discrete("Form of Contestation") +
  geom_hline(yintercept = 0) 
dev.off()
