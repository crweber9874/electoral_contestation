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
               "female",
               "republican",
               "democrat",
               "conservatism",
               "VIOLENT",
               "trump_vote",
               "authoritarianism")
IV <- "surveillance"
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
               "female",
               "republican",
               "democrat",
               "conservatism",
               "SM",
               "trump_vote",
               "authoritarianism")
IV <- "surveillance"
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
               "female",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "surveillance"
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
               "female",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "surveillance"
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
               "female",
               "republican",
               "democrat",
               "conservatism",
               "trump_vote",
               "authoritarianism")
IV <- "surveillance"
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

png("main_emotion.png")
ggplot(plot, aes(y = mean, ymin = min, ymax =  max, x = as.factor(response)))+                                                                                   
  facet_wrap(~item) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Response", values=c("red", "blue", "purple"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Marginal Effect (Surveillance)") +
  labs(caption="2020 Western States Study. Estimates are from parametric g-estimation, ordered logit")+
  scale_y_continuous("ATE", limits = c(-0.4, 0.4)) + 
  scale_x_discrete("Response") +
  geom_hline(yintercept = 0) 
dev.off()
