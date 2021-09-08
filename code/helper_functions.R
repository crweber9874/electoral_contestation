##  Useful functions (July 14, 2021)
### These are some functions I used throughout the analysis. They simplify
### things by not requiring repetitive cutting and pasting of code
### There are a lot of robustness checks, for instance, in our analysis
### that would generate long files.


zero.one <-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}
# stan_e1 = function(independent.variable =    independent.variable, 
#                    dependent.variable = dependent.variable, 
#                    controls = controls, data = data){
#   y = data[,dependent.variable]
#   x = data[,controls]
#   z = data[,independent.variable]
#   ### Construct Stan Data Generator #####
#   dat = list(
#     N =length(y),
#     P =ncol(x),
#     X = x,
#     Y = y,
#     A = z,
#     K = max(y)
#   )
#   
#   ### Generate the appropriate stan model ####
#   return(dat)
# }
# 
# ologit_baseline_stan <-
#   'data {
#   int<lower=0> N;
#   int<lower=0> P;
#   matrix[N, P] X;
#   vector[N] A; 
#   int K; 
#   int<lower=0,upper=6> Y[N];
# }
# transformed data {
#   vector[N] boot_probs = rep_vector(1.0/N, N);  
# }
# parameters {
#   vector[P + 1] alpha;
#   ordered[K - 1] c;
# }
# transformed parameters{
#   vector[P] alphaZ = head(alpha, P);
#   real alphaA = alpha[P + 1];
# }
# model{
#   alpha ~ normal(0, 2.5);
#   Y ~ ordered_logistic(X * alphaZ + A * alphaA, c);
# }
# generated quantities {
#     int row_i;
#     real ATE1 = 0;
#     real ATE2 = 0;
#     real ATE3 = 0;
#     vector[N] Yl_a1;
#     vector[N] Yl_a0;
#     vector[N] Ym_a1;
#     vector[N] Ym_a0;
#     vector[N] Yh_a1;
#     vector[N] Yh_a0;
#     vector[N] eta1;
#     vector[N] eta2;
#   for (n in 1:N) {
#     row_i = categorical_rng(boot_probs);
#     eta1[n] = X[row_i] * alphaZ;
#     eta2[n]=  X[row_i] * alphaZ + alphaA;
#     Yl_a1[n] = bernoulli_logit_rng(c[2] - eta2[n]);
#     Yl_a0[n] = bernoulli_logit_rng(c[2] - eta1[n]);
#     Ym_a1[n] = bernoulli_logit_rng(c[3] - eta2[n]) - bernoulli_logit_rng(c[2] - eta2[n]); 
#     Ym_a0[n] = bernoulli_logit_rng(c[3] - eta1[n]) - bernoulli_logit_rng(c[2] - eta1[n]); 
#     Yh_a1[n] = 1 - bernoulli_logit_rng(c[3] - eta2[n]); 
#     Yh_a0[n] = 1 - bernoulli_logit_rng(c[3] - eta1[n]);   
#     ATE1 = ATE1 + (Yl_a1[n] - Yl_a0[n])/N;
#     ATE2 = ATE2 + (Ym_a1[n] - Ym_a0[n])/N;
#     ATE3 = ATE3 + (Yh_a1[n] - Yh_a0[n])/N;
#     }
# }
# '

### This is the primary simulation/bootsrapping function
lavaan_sim = function(dat, indices, ordinal_data = ordinal_data,
                      lavaan_model = lavaan_model){
  .df <- dat[indices, ]
  den1 <- glm(weight_1, 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(weight_2, 
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
  
  
  model1 = cfa(lavaan_model, ordered=ordinal_data, 
               data=.df, sampling.weights = "weights1")  

  model2 = cfa(lavaan_model, ordered=ordinal_data, 
               data=.df, sampling.weights = "weights2") 
  
  ## Regex out all the relevant stuff ###
  
  parameterEstimates(model2, standardized = FALSE)
  
  
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
  # Form Predictions
  for (i in 1:6){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 7:12){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_a", i), tc - ta)
    assign(paste0("biden2_a", i), bc - ba)
  }
  
  
  
  return(c(biden_a1, biden_a2, biden_a3, biden_a4, biden_a5, biden_a6, 
           trump_a1, trump_a2, trump_a3, trump_a4, trump_a5, trump_a6, 
           biden2_a7, biden2_a8, biden2_a9,  biden2_a10, biden2_a11, biden2_a12, 
           trump2_a7, trump2_a8, trump2_a9,  trump2_a10, trump2_a11, trump2_a12 ))
}

lavaan_sim_2 = function(dat, indices, ordinal_data = ordinal_data,
                        lavaan_model = lavaan_model_state, weight_1 = weight_1,
                        weight_2 = weight_2){
  .df <- dat[indices, ]
  den1 <- glm(weight_1, 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(weight_2, 
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
  
  ### Append data with new weights ###
  
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1 = cfa(lavaan_model_state, ordered=ordinal_data, 
               data=.df, sampling.weights = "weights1") 
  model2 = cfa(lavaan_model_state, ordered=ordinal_data, 
               data=.df, sampling.weights = "weights2") 
  
  
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
  for (i in 1:2){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 3:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_a", i), tc - ta)
    assign(paste0("biden2_a", i), bc - ba)
  }
  
  
  return(c(biden_a1, biden_a2,  
           trump_a1, trump_a2, 
           biden2_a3,  biden2_a4, 
           trump2_a3, trump2_a4))
}

# lavaan_sim = function(dat, indices, ordinal_data = ordinal_data,
#                       lavaan_model = lavaan_model){
#   .df <- dat[indices, ]
#   
#   model1 = cfa(lavaan_model, ordered=ordinal_data, 
#                data=.df, sampling.weights = "weights1") 
#   model2 = cfa(lavaan_model, ordered=ordinal_data, 
#                data=.df, sampling.weights = "weights2") 
#   
#   ## Regex out all the relevant stuff ###
#   
#   temp_dat = rbind(
#     data.frame(
#       trump_vote = coef(model1)[grep("trump_vote", names(coef(model1)))],
#       post1 = coef(model1)[grep("post1", names(coef(model1)))],
#       post2 = coef(model1)[grep("post2", names(coef(model1)))],
#       TT1 = coef(model1)[grep("TT1", names(coef(model1)))],
#       TT2 = coef(model1)[grep("TT2", names(coef(model1)))]
#     ),
#     data.frame(
#       trump_vote = coef(model2)[grep("trump_vote", names(coef(model2)))],
#       post1 = coef(model2)[grep("post1", names(coef(model2)))],
#       post2 = coef(model2)[grep("post2", names(coef(model2)))],
#       TT1 = coef(model2)[grep("TT1", names(coef(model2)))],
#       TT2 = coef(model2)[grep("TT2", names(coef(model2)))]
#     )
#   )
#   temp_dat$treatment = rep(c("Uncertainty", "Post_Call"), each = nrow(temp_dat)/2)
#   # Form Predictions
#   for (i in 1:9){
#     ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
#     ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
#     bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
#     tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
#     ### Form Treatment Effects
#     assign(paste0("trump_a", i), tb - ta)
#     assign(paste0("biden_a", i), bb - ba)
#   }
#   
#   for (i in 10:18){
#     ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
#     ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
#     bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
#     tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
#     ### Form Treatment Effects
#     assign(paste0("trump2_a", i), tc - ta)
#     assign(paste0("biden2_a", i), bc - ba)
#   }
#   
#   
#   
#   return(c(biden_a1, biden_a2, biden_a3, biden_a4, biden_a5, biden_a6, biden_a7, biden_a8, biden_a9, 
#            trump_a1, trump_a2, trump_a3, trump_a4, trump_a5, trump_a6, trump_a7, trump_a8, trump_a9,
#            biden2_a10,  biden2_a11, biden2_a12, biden2_a13, biden2_a14, biden2_a15, biden2_a16, biden2_a17, biden2_a18,
#            trump2_a10, trump2_a11, trump2_a12, trump2_a13, trump2_a14, trump2_a15, trump2_a16, trump2_a17, trump2_a18 ))
# }
# 

winner_loser = function(dat, indices, outcome = "hard"){
  .df <- dat[indices, ]
  eqn = paste0(outcome, "~trump_vote + post1 + post2 + TT1 + TT2 ")
  mod1 = lm(as.formula(eqn) , data = .df, weights = weights1)
  mod2 = lm(as.formula(eqn) , data = .df, weights = weights2)
  tempdat1 = model.matrix(mod1) %>% data.frame()
  tempdat2 = model.matrix(mod2) %>% data.frame()
  
  y0_trump_1 = tempdat1 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() %*% coef(mod1) %>% mean()
  y0_biden_1 = tempdat1 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() %*% coef(mod1) %>% mean()
  
  y0_trump_2 = tempdat2 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() %*% coef(mod2) %>% mean()
  y0_biden_2 = tempdat2 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() %*% coef(mod2) %>% mean()
  
  y1_trump = tempdat1 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 1) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 1)  %>% mutate(TT2 = 0)  %>% as.matrix() %*% coef(mod1) %>% mean()
  y1_biden = tempdat1 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 1) %>% 
    mutate(post2 = 0)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0)  %>% as.matrix() %*% coef(mod1) %>% mean()
  
  y2_trump = tempdat2 %>% mutate(trump_vote = 1)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 1)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 1)  %>% as.matrix() %*% coef(mod2) %>% mean()
  y2_biden = tempdat2 %>% mutate(trump_vote = 0)  %>% mutate(post1 = 0) %>% 
    mutate(post2 = 1)  %>% mutate(TT1 = 0)  %>% mutate(TT2 = 0) %>% as.matrix() %*%  coef(mod2) %>% mean()
  
  out1 = y1_trump - y0_trump_1
  out2 = y2_trump - y0_trump_2
  out3 = y1_biden - y0_biden_1
  out4 = y2_biden - y0_biden_2
  return(c(out1, out2, out3, out4))
}



### For main winner and loser effects
plot_data_generator = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975),
    type = rep(c("Hard", 
                 "Soft", 
                 "Concern", 
                 "Trust(Federal)", 
                 "Trust(State)", 
                 "Ballot Confidence")),
    period = rep(c("Uncertainty", "Post_Call"), each = ncol(output$t)/2 ),
    voter = rep(c("Biden", "Trump"), each = 6 ))
  return(plot_dat)
}
### For robustness checks - -linear regression
plot_data_linear = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975),
    period = rep(c("Uncertainty", "Post_Call")),
    voter = rep(c("Trump", "Biden"), each = 2 ))
  return(plot_dat)
}
### For robustness checks -- state analysis, linear regression
plot_data_generator_state = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975),
    period = rep(c("Uncertainty", "Post_Call") ),
    voter = rep(c("Trump", "Biden"), each = 2 ))
  return(plot_dat)
}


### Marginal Effects

winner_loser_baseline = function(dat, outcome = "violent"){
  eqn = paste0(paste0("as.factor(", outcome, ")"), "~ authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state) ")
  mod1 = polr(as.formula(eqn) , data = dat)
### Predict marginal effect of each variable with respect to categories 4/5
  mod_data = model.matrix(mod1)[,-1] %>% data.frame()

  beta.sim<-mvrnorm(1000, c(mod1$coefficients, mod1$zeta), vcov(mod1)) %>% as.data.frame()
  marginal_dat = data.frame(mid=NA, max=NA, min=NA)
for(i in 1:ncol(mod_data)){
    temp_d1 = temp_d2 = mod_data
    temp_d1[,i] <- min(temp_d1[,i])
    temp_d2[,i] <- max(temp_d2[,i])
    sim.high =  1 - plogis(beta.sim[, grep("[/|]", names(beta.sim))][,3] -  as.matrix(temp_d2) %*% 
                             t(beta.sim[, grep("[a-zA-Z]", names(beta.sim))]))
    sim.low =   1 - plogis(beta.sim[, grep("[/|]", names(beta.sim))][,3] -  as.matrix(temp_d1) %*% 
                             t(beta.sim[, grep("[a-zA-Z]", names(beta.sim))]))
    tempdat = data.frame(
    mid = apply(sim.high - sim.low, 2, mean) %>% quantile(0.50) ,
    max = apply(sim.high - sim.low, 2, mean) %>% quantile(0.975)  ,
    min = apply(sim.high - sim.low, 2, mean) %>% quantile(0.025) 
    )
    marginal_dat = rbind(marginal_dat, tempdat) 
  }

    marginal_dat <- marginal_dat[-1,]
    marginal_dat$names <- c("Authoritarianism", "RR", "Vote Trump", "Right-Wing Media",
                               "Age", "Female" , "Latino" , "Black" , "College" , "Ideology" , "Christian" , 
                               "SDO" , "Colorado*", "New Mexico*", "Nevada*", "Utah*")
  return(marginal_dat)
}
  
winner_loser_baseline_l = function(dat, outcome = "hard"){
  eqn = paste0(paste0(outcome, "~ authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state)"))
  mod1 = lm(as.formula(eqn) , data = dat)
  ### Predict marginal effect of each variable with respect to categories 4/5
  mod_data = model.matrix(mod1) %>% data.frame()
  
  beta.sim<-mvrnorm(1000, c(mod1$coefficients, mod1$zeta), vcov(mod1)) %>% as.data.frame()
  marginal_dat = data.frame(mid=NA, max=NA, min=NA)
  
  
  for(i in 1:ncol(mod_data)){
    temp_d1 = temp_d2 = mod_data
    temp_d1[,i] <- min(temp_d1[,i])
    temp_d2[,i] <- max(temp_d2[,i])
    sim.high =  as.matrix(temp_d2) %*% 
                             t(beta.sim)
    sim.low =    as.matrix(temp_d1) %*% t(beta.sim)
    tempdat = data.frame(
      mid = apply(sim.high - sim.low, 2, mean) %>% quantile(0.50) ,
      max = apply(sim.high - sim.low, 2, mean) %>% quantile(0.975),
      min = apply(sim.high - sim.low, 2, mean) %>% quantile(0.025) 
    )
    marginal_dat = rbind(marginal_dat, tempdat) 
  }
  
      
  marginal_dat <- marginal_dat[-1,]
  marginal_dat$names <- c("Intercept", "Authoritarianism", "RR", "Vote Trump", "Right-Wing Media",
                          "Age", "Female" , "Latino" , "Black" , "College" , "Ideology" , "Christian" , 
                          "SDO" , "Colorado*", "New Mexico*", "Nevada*", "Utah*")
  return(marginal_dat)
}


