##  Useful functions (October 29, 2021)
### These are some functions I used throughout the analysis. They simplify
### things by not requiring repetitive cutting and pasting of code
### There are a lot of robustness checks, for instance, in our analysis
### that would generate long files.
zero.one <-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

##### This is the primary function function ###
lavaan_sim_predict = function(dat, indices, ordinal_data = ordinal_data,
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

#### Linear model: This is the linear regression, robstuness one.

treat.effects.linear = function(data = dat, indices, dependent.variable = "soft",
                                 type = "point",
                                 weighting = TRUE,...
){
  weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                           age + female + latino + black + college + ideology + 
                           christian + sdo + as.factor(state)) ## Probability of treatment 1
  weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                           age + female + latino + black + college + ideology + 
                           christian + sdo + as.factor(state)) ## Probability of treatment 1
  .df <- data[indices, ]
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
  .df$pre_election  =    ifelse(.df$treat==1, 1, 0)
  .df$uncertainty   =    ifelse(.df$treat==2, 1, 0)
  .df$post_election  =   ifelse(.df$treat==3, 1, 0)
  .df$TT1 = .df$trump_vote  * .df$uncertainty
  .df$TT2 = .df$trump_vote  * .df$post_election
  if(weighting == TRUE){
    modela = lm(paste0(dependent.variable, "~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights1) 
    modelb = lm(paste0(dependent.variable, "~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights2) 
 }
  if(weighting == FALSE){
    modela = lm(paste0(dependent.variable, "~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights1) 
    modelb = lm(paste0(dependent.variable, "~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights2) 
  }
  
  
  tempdat1 = model.matrix(modela) %>% data.frame()
  ### Permutations -- just pre v post....
  permutes = paste0(c("y0", "y1", "y2"), "_", rep(c("trump", "biden"), each = 3) )
  
  assign(permutes[1], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  
  assign(permutes[2], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 1) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 1)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  
  assign(permutes[3], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 1) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 1)  %>% as.matrix()) 
  
  assign(permutes[4], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  assign(permutes[5], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 1) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  assign(permutes[6], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 1) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  
  ## Estimate agreement
  output = c(
    trump_pre =      y0_trump[1,] %*% modela$coefficients,
    biden_pre =      y0_biden[1,] %*% modela$coefficients,
    trump_unc =      y1_trump[1,] %*% modela$coefficients,
    biden_unc =      y1_biden[1,] %*% modela$coefficients,
    trump_post =     y2_trump[1,] %*% modelb$coefficients,
    biden_post =     y2_biden[1,] %*% modelb$coefficients

  )
  me = c(trump1   =  output["trump_unc"]  - output["trump_pre"],
         biden1 =    output["biden_unc"]  - output["biden_pre"],
         trump2   =  output["trump_post"] - output["trump_pre"],
         biden2 =    output["biden_post"] - output["biden_pre"]
 
  )
  if(type == "point") return(output)
  if(type == "marginal") return(me)
}

### Ordinal Model: This is the ordinal regression model
treat.effects.ordinal = function(data = dat, indices, 
                                 dependent.variable = "violent",
                                 type = "point",
                                 weighting = TRUE,...
){
    weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + trump_vote + rwm +
                           age + female + latino + black + college + ideology + 
                           christian + sdo + as.factor(state)) ## Probability of treatment 1
   weight_2 <- as.formula((treat==3) ~  authoritarianism + rr + trump_vote + rwm +
                           age + female + latino + black + college + ideology + 
                           christian + sdo + as.factor(state)) ## Probability of treatment 1
  .df <- data[indices, ]
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
  .df$pre_election  =    ifelse(.df$treat==1, 1, 0)
  .df$uncertainty   =    ifelse(.df$treat==2, 1, 0)
  .df$post_election  =   ifelse(.df$treat==3, 1, 0)
  .df$TT1 = .df$trump_vote  * .df$uncertainty
  .df$TT2 = .df$trump_vote  * .df$post_election
  if(weighting == TRUE){
     modela = polr(paste0("as.factor(", dependent.variable, ")~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights1) 
     modelb = polr(paste0("as.factor(", dependent.variable, ")~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df, weights = weights2) 
  }
  if(weighting == FALSE){
     modela = polr(paste0("as.factor(", dependent.variable, ")~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df) 
     modelb = polr(paste0("as.factor(", dependent.variable, ")~", "trump_vote + uncertainty + post_election + TT1 + TT2" ), 
                 data = .df) 
  }
  
  tempdat1 = model.matrix(modela)[,-1] %>% data.frame()
  ### Permutations -- just pre v post....
  permutes = paste0(c("y0", "y1", "y2"), "_", rep(c("trump", "biden"), each = 3) )
  
  assign(permutes[1], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  
  assign(permutes[2], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 1) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 1)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  
  assign(permutes[3], tempdat1 %>% mutate(trump_vote = 1)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 1) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 1)  %>% as.matrix()) 
  
  assign(permutes[4], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  assign(permutes[5], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 1) %>% mutate(post_election = 0) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  assign(permutes[6], tempdat1 %>% mutate(trump_vote = 0)  %>%
                         mutate(uncertainty = 0) %>% mutate(post_election = 1) %>% 
                         mutate(TT1 = 0)  %>%  mutate(TT2 = 0)  %>% as.matrix()) 
  ## Estimate agreement
  output = c(
    trump_pre  =  1 - plogis(modela$zeta[3] - y0_trump[1,] %*% modela$coefficients),
    biden_pre  =  1 - plogis(modela$zeta[3] - y0_biden[1,] %*% modela$coefficients),
    trump_unc  =  1 - plogis(modela$zeta[3] - y1_trump[1,] %*% modela$coefficients),
    biden_unc  =  1 - plogis(modela$zeta[3] - y1_biden[1,] %*% modela$coefficients),
    trump_post =  1 - plogis(modelb$zeta[3] - y2_trump[1,] %*% modelb$coefficients),
    biden_post =  1 - plogis(modelb$zeta[3] - y2_biden[1,] %*% modelb$coefficients))

  me = c(trump1 = output["trump_unc"]  - output["trump_pre"],
         trump2 = output["trump_post"] - output["trump_pre"],
         biden1 = output["biden_unc"]  - output["biden_pre"],
         biden2 = output["biden_post"] - output["biden_pre"]
  )
  if(type == "point") return(output)
  if(type == "marginal") return(me)
}

### For main winner and loser effects
plot_data_generator_all = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975))
  return(plot_dat)
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

### Marginal Effects, baseline model

winner_loser_baseline = function(dat, outcome = "violent"){
  eqn = paste0(paste0("as.factor(", outcome, ")"), "~ authoritarianism + rr + trump_vote + rwm +
                    age + female + latino + black + college + ideology + christian + 
                    sdo + as.factor(state) ")
  mod1 = polr(as.formula(eqn) , data = dat)
### Predict marginal effect of each variable with respect to categories 4/5
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
