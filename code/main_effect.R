#### Generate the marginal effects for baseline covariates ###
#### This uses the stan model I label, "ologit_baseline.stan"
#### This seems to be suitable for our purposes:
ITER = 2000
CHAINS = 3
summary(dat)
library(dplyr)
## Anxiety, anger instead of surveillance

### This returns the right data, according to model specification#####
items = c("violent", "burn", "court", "recount", "criticize",
          "rwm",  "rr", "age", "female", "latino", "black",
          "college","christian","republican", "democrat",
          "ideology", "anxiety", "anger", "authoritarianism", 
          "strength", "SM", "VIOLENT", "violent")
baseline_data = df %>%  filter(treat ==1) %>% subset(select = items) %>% na.omit() %>% 
  mutate(anxiety = recode(anxiety, `1`=0, `2`=0, `3`=0, `4`=1))
  mutate(anger = recode(anger, `1`=0, `2`=0, `3`=0, `4`=1))
baseline_data$conservatism <- baseline_data$ideology %>% zero.one()
### Start with the treatment effects for the violent and media items ###
### There shouldn't be controls in these, so it's just an ordered logit regression of the item on the treatment
### indicator.
# e.g., 
MASS::polr(as.factor(violent) ~ VIOLENT, data = baseline_data) %>% summary ## Violent manipulation
MASS::polr(as.factor(criticize) ~ SM, data = baseline_data) %>% summary ## Social Media manipulation
## For consistency, I'll just do this using the bayesian setup
# Specify 


### I estimate effects for each of the different items.
### The file reads in the stan code -- which is fine to ignore -- to generate predicted values
### If you run locally, you'll need to install the dependencies. Otherwise, just do it here.
CONTROLS <- ''
IV <- "SM"
DV <- "criticize"
item_count = length(controls) + length(independent.variable)
tempdat= data[, c(independent.variable, controls, dependent.variable)]


stan_data = stan_e1(independent.variable = IV,
                    controls = CONTROLS,
                    dependent.variable = DV, data = tempdat)
fit1 = stan(model_code = model1, verbose =FALSE,
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
fit2 = stan(model_code = model1, verbose =FALSE,
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



                                                                     
                                                                    

