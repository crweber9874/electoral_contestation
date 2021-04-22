#### Generate the marginal effects for baseline covariates ###
#### This uses the stan model I label, "ologit_baseline.stan"
#### This seems to be suitable for our purposes:
ITER = 2000
CHAINS = 3
summary(dat)
library(dplyr)
setwd("/home/rstudio/electoral_contestation/electoral_contestation/code/")
source(file = "helper_functions.R")
# Updates: Added two negative emotions: anxiety and anger.
# ologit_baseline is a stan function found in the helper_functions. This should be a separate stan file.
# But for some reason stan cannot read the file.
### This is for data deletion #####
### To do: Impute responses? #####

### Declare the items analyzed throughout.
items = c("violent", "burn", "court", "recount", "criticize",
          "rwm",  "rr", "age", "female", "latino", "black",
          "college","christian","republican", "democrat",
          "ideology", "anxiety", "anger", "authoritarianism", 
          "strength", "SM", "VIOLENT", "violent")
baseline_data = df %>%  filter(treat ==1) %>% subset(select = items) %>% na.omit() %>% 
  mutate(anxiety = recode(anxiety, `1`=0, `2`=0, `3`=0, `4`=1)) %>%
  mutate(anger =   recode(anger, `1`=0, `2`=0, `3`=0, `4`=1))
baseline_data$conservatism <- baseline_data$ideology %>% zero.one()
### Start with the treatment effects for the violent and media items ###
### There shouldn't be controls in these, so it's just an ordered logit regression of the item on the treatment
### indicator.
# e.g., 
MASS::polr(as.factor(violent) ~ VIOLENT, data = baseline_data) %>% summary ## Violent manipulation
MASS::polr(as.factor(criticize) ~ SM, data = baseline_data) %>% summary ## Social Media manipulation
## For consistency, I'll just do this using the bayesian setup
# Specify the two exogenous treatment effects: 


### I estimate effects for each of the different items.
### The file reads in the stan code -- which is fine to ignore -- to generate predicted values
### If you run locally, you'll need to install the dependencies. Otherwise, just do it here.
controls <- CONTROLS <- c( "rwm", 
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
                           "authoritarianism",
                           "strength")
independent.variable <- "SM"
dependent.variable <- "criticize"
item_count = length(controls) + length(independent.variable)
tempdat= baseline_data[, c(independent.variable, controls, dependent.variable)]


stan_data = stan_e1(independent.variable = independent.variable,
                    controls = controls,
                    dependent.variable = dependent.variable, data = tempdat)
ologit_baseline_stan <-
  'data {
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
    Yh_a1[n] = 1 - bernoulli_logit_rng(c[3] - eta2[n]); 
    Yh_a0[n] = 1 - bernoulli_logit_rng(c[3] - eta1[n]);   
    ATE1 = ATE1 + (Yl_a1[n] - Yl_a0[n])/N;
    ATE2 = ATE2 + (Ym_a1[n] - Ym_a0[n])/N;
    ATE3 = ATE3 + (Yh_a1[n] - Yh_a0[n])/N;
    }
}
'

fit1 = stan(model_code = ologit_baseline_stan, verbose =FALSE,
           data = stan_data, iter = ITER, chains = CHAINS)
#### violent, burn, recount, court
#############
ITER = 1000
CHAINS = 3
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
               "surveillance",
               "authoritarianism",
               "strength")
IV <- "VIOLENT"
DV <- "violent"
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
tempdat= data[, c(independent.variable, controls, dependent.variable)]
#### Violent #####

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit2 = stan(model_code = "ologit_baseline.stan", verbose =FALSE,
           data = stan_data, iter = ITER, chains = CHAINS)

#### Burn #####

ITER = 1000
CHAINS = 3
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "conservatism",
               "christian",
               "republican",
               "democrat",
               "surveillance",
               "strength")

IV <- "authoritarianism"
DV <- "burn"
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
tempdat= data[, c(independent.variable, controls, dependent.variable)]
stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)


fit3 = stan(model_code = model1, verbose =FALSE,
           data = stan_data, iter = ITER, chains = CHAINS)

#### Recount #####
ITER = 1000
CHAINS = 3
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "conservatism",
               "christian",
               "republican",
               "democrat",
               "surveillance" ,
               "strength")
IV <- "authoritarianism"
DV <- "recount"
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
tempdat= data[, c(independent.variable, controls, dependent.variable)]

stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit4 = stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

#### Court #####
ITER = 1000
CHAINS = 3
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "conservatism",
               "christian",
               "republican",
               "democrat",
               "surveillance",
               "strength")



IV <- "authoritarianism"
DV <- "court"
dependent.variable <- DV
independent.variable <- IV
controls <- CONTROLS
item_count = length(controls) + length(independent.variable)
tempdat= data[, c(independent.variable, controls, dependent.variable)]


stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit5 = stan(model_code = model1, verbose =FALSE,
            data = stan_data, iter = ITER, chains = CHAINS)

### Process each one before saving to one-way stan

#save(fit1, fit2, fit3, fit5, fit5, file = "one_way_stan.rda")

## Parameter estimates
summary(fit, pars = "alpha")[["summary"]][,1:2]
## Names
CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "conservatism",
               "christian",
               "republican",
               "democrat",
               "surveillance",
               "authoritarianism",
               "strength")

IV <- "SM"
DV <- "criticize"

## Ordered Logit Parameters
library(xtable)
p1 <- data.frame(
           c(CONTROLS, IV),summary(fit1, pars = "alpha")[["summary"]][,c(1,3)],
           summary(fit2, pars = "alpha")[["summary"]][,c(1,3)]) 
names(p1) <- c("Item", "Mean", "SD", "Mean", "SD")
print(xtable(p1), include.rownames=FALSE)

CONTROLS <- c( "rwm", 
               "rr", 
               "age", 
               "female",
               "latino", 
               "black",
               "college", 
               "conservatism",
               "christian",
               "republican",
               "democrat",
               "surveillance",
               "strength")

IV <- "authoritarianism"
DV <- "burn"

p2 <- data.frame(
  c(CONTROLS, IV),
  summary(fit3, pars = "alpha")[["summary"]][,c(1,3)],
  summary(fit4, pars = "alpha")[["summary"]][,c(1,3)],
  summary(fit5, pars = "alpha")[["summary"]][,c(1,3)]
  ) 
names(p2) <- c("Item", rep(c("Mean", "SD"), times =3 ))
print(xtable(p2), include.rownames=FALSE)



                                                                     
                                                                    

