load("raw_data.rda")
detach("package:rstan")
library(dplyr)
summary(dat)



CHAINS  = 2
ITER = 1000
data <- dat
data$conservatism = data$ideology %>% zero.one()

model1 <- "
data {
  int<lower=0> N;
  int<lower=0> P;
  matrix[N, P] X;
  vector[N] A; 
  int K; 
  int<lower=0,upper=6> Y[N];
}
transformed data {
  vector[N] boot_probs = rep_vector(1.0/N, N);  
}
parameters {
  vector[P + 1] alpha;
  ordered[K - 1] c;
}
transformed parameters{
  vector[P] alphaZ = head(alpha, P);
  real alphaA = alpha[P + 1];
}
model{
  alpha ~ normal(0, 2.5);
  Y ~ ordered_logistic(X * alphaZ + A * alphaA, c);
}
generated quantities {
    int row_i;
    real ATE1 = 0;
    real ATE2 = 0;
    real ATE3 = 0;
    vector[N] Yl_a1;
    vector[N] Yl_a0;
    vector[N] Ym_a1;
    vector[N] Ym_a0;
    vector[N] Yh_a1;
    vector[N] Yh_a0;
    vector[N] eta1;
    vector[N] eta2;
  for (n in 1:N) {
    row_i = categorical_rng(boot_probs);
    eta1[n] = X[row_i] * alphaZ;
    eta2[n]=  X[row_i] * alphaZ + alphaA;
    Yl_a1[n] = bernoulli_logit_rng(c[2] - eta2[n]);
    Yl_a0[n] = bernoulli_logit_rng(c[2] - eta1[n]);
    Ym_a1[n] = bernoulli_logit_rng(c[3] - eta2[n]) - bernoulli_logit_rng(c[2] - eta2[n]); 
    Ym_a0[n] = bernoulli_logit_rng(c[3] - eta1[n]) - bernoulli_logit_rng(c[2] - eta1[n]); 
    /// Treatment effect indecisive
    Yh_a1[n] = 1 - bernoulli_logit_rng(c[3] - eta2[n]); 
    Yh_a0[n] = 1 - bernoulli_logit_rng(c[3] - eta1[n]);   
    ATE1 = ATE1 + (Yl_a1[n] - Yl_a0[n])/N;
    ATE2 = ATE2 + (Ym_a1[n] - Ym_a0[n])/N;
    ATE3 = ATE3 + (Yh_a1[n] - Yh_a0[n])/N;
    }
}
"
################################################################################################################
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "strength",
               "latino", 
               "black",
               "college", 
               "christian",
               "surveillance",
               "republican",
               "democrat",
               "conservatism",
               "VIOLENT",
               "trump_vote",
               "authoritarianism")
IV <- "female"
DV <- "violent"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, IV]
tempdat= data[, c(IV, CONTROLS, DV)]
###########################################################################
stan_e1 = function(independent.variable =    independent.variable, 
                   dependent.variable = dependent.variable, 
                   controls = controls, data = data){
  y = data[,DV]
  x = data[,CONTROLS]
  z = data[,IV]
  ### Construct Stan Data Generator #####
  dat = list(
    N =length(y),
    P =ncol(x),
    X = x,
    Y = y,
    A = z,
    K = max(y)
  )
  
  ### Generate the appropriate stan model ####
  return(dat)
}

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
              data = stan_data, iter = ITER, chains = CHAINS)

### Generate Predictions from this -- store -- then refit model varying DV, but need to regenerate tempdat

plot <- data.frame(
  rbind(
  summary(fit, pars = "ATE1")[["summary"]],
  summary(fit, pars = "ATE2")[["summary"]],
  summary(fit, pars = "ATE3")[["summary"]]
))
  

################################################################################################################
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "strength",
               "latino", 
               "black",
               "college", 
               "christian",
               "surveillance",
               "republican",
               "democrat",
               "conservatism",
               "SM",
               "trump_vote",
               "authoritarianism")
IV <- "female"
DV <- "criticize"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, IV]
tempdat= data[, c(IV, CONTROLS, DV)]
###########################################################################

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

### Generate Predictions from this -- store -- then refit model varying DV, but need to regenerate tempdat

p1 <- data.frame(
  rbind(
    summary(fit, pars = "ATE1")[["summary"]],
    summary(fit, pars = "ATE2")[["summary"]],
    summary(fit, pars = "ATE3")[["summary"]]
  ))

plot = rbind(plot, p1)






################################################################################################################
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "strength",
               "latino", 
               "black",
               "college", 
               "christian",
               "surveillance",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "female"
DV <- "recount"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, IV]
tempdat= data[, c(IV, CONTROLS, DV)]
###########################################################################

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

### Generate Predictions from this -- store -- then refit model varying DV, but need to regenerate tempdat

p1 <- data.frame(
  rbind(
    summary(fit, pars = "ATE1")[["summary"]],
    summary(fit, pars = "ATE2")[["summary"]],
    summary(fit, pars = "ATE3")[["summary"]]
  ))

plot = rbind(plot, p1)




################################################################################################################
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "strength",
               "latino", 
               "black",
               "college", 
               "christian",
               "surveillance",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "female"
DV <- "court"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, IV]
tempdat= data[, c(IV, CONTROLS, DV)]
###########################################################################

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

### Generate Predictions from this -- store -- then refit model varying DV, but need to regenerate tempdat

p1 <- data.frame(
  rbind(
    summary(fit, pars = "ATE1")[["summary"]],
    summary(fit, pars = "ATE2")[["summary"]],
    summary(fit, pars = "ATE3")[["summary"]]
  ))

plot = rbind(plot, p1)

################################################################################################################
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "strength",
               "latino", 
               "black",
               "college", 
               "christian",
               "surveillance",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "female"
DV <- "burn"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, IV]
tempdat= data[, c(IV, CONTROLS, DV)]
###########################################################################

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

### Generate Predictions from this -- store -- then refit model varying DV, but need to regenerate tempdat



p1 <-rbind(
    summary(fit, pars = "ATE1")[["summary"]],
    summary(fit, pars = "ATE2")[["summary"]],
    summary(fit, pars = "ATE3")[["summary"]]
  )


colnames(p1) <- colnames(plot)    # Change column names

plot = rbind(plot, p1)
plot$item <- rep(c("Protest", "Criticize", "Recount", "Court", "Burn"), each =3)
plot$response <- rep(c("Oppose", "Neutral", "Support"))
names(plot) <- c("mean", "se", "sd", "min", "min2", "mid", "max2", "max", "something", "something2", "item", "response")


plot$response <- factor(plot$response , levels = c("Oppose", "Neutral", "Support"))

png("main_strength.png")
ggplot(plot, aes(y = mean, ymin = min, ymax =  max, x = as.factor(response)))+                                                                                   
  facet_wrap(~item) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Response", values=c("red", "blue", "purple"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Marginal Effect (Sex)") +
  labs(caption="2020 Western States Study. Estimates are from parametric g-estimation, ordered logit")+
  scale_y_continuous("ATE", limits = c(-0.4, 0.4)) + 
  scale_x_discrete("Response") +
  geom_hline(yintercept = 0) 
dev.off()




png("emotions.png")
ggplot(plot, aes(y = mean, ymin = min, ymax =  max, x = response))+                                                                                   
  facet_wrap(~item) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Response", values=c("red", "blue", "purple"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Marginal Effect of Emotional State (Surveillance)") +
  labs(caption="2020 Western States Study. Estimates are from parametric g-estimation, ordered logit")+
  scale_y_continuous("ATE", limits = c(-0.4, 0.4)) + 
  scale_x_discrete("Response") +
  geom_hline(yintercept = 0) 
dev.off()







stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)



####################   ####################   ####################   
DV <- "violent"
data = data %>% filter(treat ==1) %>% na.omit()
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, c(independent.variable)]
tempdat= data[, c(independent.variable, controls, dependent.variable)]
####################   ####################   ####################   

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit <- stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)















#### This runs the analysis, saves in a list 
output = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(independent.variable = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = dependent.variable, data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= ITER)
  output[[i]] <- mod
  cat("Still Going, Now on Treatment", names(treatment)[i])
}

################## ################## ################## ################## ##################
################## ################## ################## ################## ##################
################## ################## ################## ################## ##################


#####summarize this and save it ### Then rerun and do the same thing. ####






###### This helper function pulls the lists, combines and generates plot #######
val = NA
for(i in 1:length(output)){
  val = c(val, output[[i]])
}
val = val[-1]
figure_dat <- data.frame(val=val) %>%
  mutate(statistic = rep(c("Mean", "SE", "SD", "lower", "twentyfive", 
                           "median", "seventyfive", "upper" ,"n_eff",
                           "rhat" ), times = length(val)/10)) 
figure_dat$ATE <- rep(c("oppose", "middle", "support"), each = 10, times =10)
figure_dat$item <- rep(names(rhs_var), each = 10*3)
table_data = figure_dat %>% filter((statistic == "Mean"| 
                                      statistic == "upper"| 
                                      statistic == "lower"))
table_data = subset(table_data, ATE!="oppose")

temp1 = table_data %>% filter(statistic == "Mean")
temp2 = table_data %>% filter(statistic == "upper")
temp3 = table_data %>% filter(statistic == "lower")
temp_plot = cbind(temp1, temp2[,1], temp3[,1])
names(temp_plot) = c("mean", "statistic" ,"category", "item", "max", "min")

### Include these in main effects table ####

plot = ggplot(temp_plot, 
              aes(y=mean, ymin=min, ymax=max, x= item)) + 
  facet_wrap(~category) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Contestation", values=c("Grey", "Black"))+
  scale_fill_grey() +
  coord_flip()
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Conditional Treatment Effects of Election Timing") +
  labs(caption="2020 Western States Study. Estimates are double robust (G-estimation/IPW)")+
  scale_y_continuous("ATE", limits = c(-0.4, 0.4)) + 
  scale_x_discrete("Form of Contestation") +
  geom_hline(yintercept = 0) 
##### 
png("trump_effect.png")
plot(plot)
dev.off()
### 



#### Generate the predictions ####







###########################    Remaining Variables ##########################
ITER = 2000
CHAINS = 3
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "ideology",
               "christian",
               "republican",
               "democrat",
               "ideology",
               "surveillance")
IV <- "authoritarianism"
DV <- "recount"

data = dat %>% filter(treat ==1) %>% na.omit()

### This block creates some variables useful to loop through data frame
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
rhs_var= data[, c(independent.variable, controls)]
tempdat= data[, c(independent.variable, controls, dependent.variable)]
treatment = list()
for(item in names(rhs_var)){
  treatments = item 
  controls =  rhs_var[, !names(rhs_var) %in% treatment] %>% names()
  treatment[[item]] <- c(treatments, controls)
}

#### This runs the model -- just change to different outputs ####
output2 = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = dependent.variable, data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= ITER)
  output[[i]] <- mod
  print("Still Going")
}

############################################################################################


dependent.variable <- DV



output = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = dependent.variable, data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= 2000)
  output[[i]] <- mod
  print("Still Going")
}




#### Generate a function to create figures  ####
val = NA
for(i in 1:length(output)){
  val = c(val, output[[i]])
}
val = val[-1]
figure_dat <- data.frame(val=val) %>%
  mutate(statistic = rep(c("Mean", "SE", "SD", "lower", "twentyfive", 
                           "median", "seventyfive", "upper" ,"n_eff",
                           "rhat" ), times = length(val)/10)) 
figure_dat$ATE <- rep(c("oppose", "middle", "support"), each = 10, times =9)
figure_dat$item <- rep(names(rhs_var), each = 9*3)
table_data = figure_dat %>% filter((statistic == "Mean"| statistic == "upper"| statistic == "lower"))




### Save Estimates ###

##### Generate Effects for remaining models ####
treatment = list()
for(item in names(rhs_var)){
  treatments = item 
  controls =  rhs_var[, !names(rhs_var) %in% treatment] %>% names()
  treatment[[item]] <- c(treatments, controls)
}





output1 = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = "violent", data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= 2000)
  output[[i]] <- mod
  print("Still Going")
}

output1 = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = "violent", data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= 2000)
  output[[i]] <- mod
  print("Still Going")
}

output1 = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = "violent", data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= 2000)
  output[[i]] <- mod
  print("Still Going")
}

output1 = list()
for(i in 1:length(treatment)){
  stan_data = stan_e1(marginal.effect = treatment[[i]][1],
                      controls = treatment[[i]][3:length(treatment[[i]])],
                      dependent.variable = "violent", data = tempdat)
  mod = stan_run(data = stan_data, model_code = model1, iter= 2000)
  output[[i]] <- mod
  print("Still Going")
}

#### Fold these into single dataset


## This output will create a list of effects, each for a specific variable -- 
## Extract those elements into a rectangular structure
val = NA
for(i in 1:length(output)){
  val = c(val, output[[i]])
}
val = val[-1]

## Takes the following input
# names(rhs_var)
# This is a bit messay
figure_dat = data.frame(val = val) %>%
              mutate(statistic = rep(c("Mean", "SE", "SD", "lower", "twentyfive", 
                         "median", "seventyfive", "upper" ,"n_eff",
                          "rhat" ), times = length(val)/10)) %>%
              mutate(item  = rep(names(rhs_var), each = 10*3 )) %>%
              mutate(ATE  =  rep(c("oppose", "middle", "support"), each = 10, times =7 ))
                           
### Select out the appropriate statistics for table ?####
### I need to just restructure this data file:



middle1 = figure_dat %>% filter(statistic == "Mean" & ATE == "support")  
high1 = figure_dat %>% filter(statistic == "upper" & ATE == "support")  
low1 =  figure_dat %>% filter(statistic == "lower" & ATE == "support")  

middle2 = figure_dat %>% filter(statistic == "Mean" & ATE == "middle")  
high2 = figure_dat %>% filter(statistic == "upper" & ATE == "middle")  
low2 =  figure_dat %>% filter(statistic == "lower" & ATE == "middle")  

plot_figure = rbind(
  cbind(middle1, upper = high1[,1], lower = low1[,1]),
  cbind(middle2, upper = high2[,1], lower = low2[,1]))
plot_figure$type = rep(c("Support", "No Opinion"), each = nrow(middl1))

### This approach is problematic because I've generated predictions for every variable ####
### Here, we'll select on the #problm variables: i.e., those that prove most divisive #####
### in the placebo condition. Here, don't worry about the looping through variables thing. 
### It should be sufficient to just write the models from the known functions.

### Extend rstan code to full model ###
### i.e., the model with two way interactions ###
### The difference here will be the predictions -- need to generate treatment effects 
### for the moderating variable -- winner v. loser x moderator
### List: trump_vote 
### Also double check 0-1 coding at the top....



###To do: Write function to vary data file ####

## Generate posterior prediction for support, oppose, neutral: fold into a treatment effect
stan_run <- function(data = dat, model_code = model1, iter = 2000){
      fit <- stan(model_code = model_code, 
             data = data, iter = iter, chains = 3)
### Extract marginal effects frtom post-estimation object ####
### Check 0-1 coding -- not sure about this.
return(list = c(summary(fit, pars = "ATE1")[["summary"]],
                summary(fit, pars = "ATE2")[["summary"]],
                summary(fit, pars = "ATE3")[["summary"]]
))
}

### Extract fit parameter


output = stan_run(data = dat, model_code = model1, iter = 1000)

## THis is probably easiest accomplished with a loop of some sort


lm(mod, dat) %>% summary