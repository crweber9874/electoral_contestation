### Write G-estimation Function for factor model ####
### Estimate the propensity scores from data ####
# make sure everything is zero.one
## Declare model parameters ###
weight_1 <- as.formula((treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + as.factor(state))
weight_2 <- as.formula((treat==2) ~  authoritarianism + rr + independent + republican + trump_vote + rwm +
                      age + female + latino + black + college + ideology + christian + as.factor(state))
lavaan_model =" hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
ordinal_data = c(            "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate"
) 
### Primary bootstrapping model ###

lavaan_sim = function(dat, indices, ordinal_data = ordinal_data,
                      lavaan_model = lavaan_model, weight_1 = weight_1,
                      weight_2 = weight_2){
  .df <- dat[indices, ]
  den1 <- glm(model, 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(model, 
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
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_a", i), tc - ta)
    assign(paste0("biden2_a", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_a5,  biden2_a6, biden2_a7, biden2_a8, 
           trump2_a5, trump2_a6, trump2_a7, trump2_a8
  ))
}
out_weights   = boot(dat, lavaan_sim, 25, ordinal_data = ordinal_data,
                     lavaan_model = lavaan_model, weight_1 = weight_1,
                     weight_2 = weight_2)
plot_data_generator = function(output){
  plot_dat = data.frame(
    middle = apply(output$t, 2, quantile, 0.5),
    hi = apply(output$t, 2, quantile, 0.025),
    lo = apply(output$t, 2, quantile, 0.975),
    type = rep(c("Hard", "Soft", "Participation", "Concern")),
    period = rep(c("Uncertainty", "Post_Call"), each = ncol(output$t)/2 ),
    voter = rep(c("Biden", "Trump"), each = 4 ))
  return(plot_dat)
}

### 




# Robustness checks, 
# estimated without weights, just the full model
lavaan_sim_noweights = function(dat, indices){
  .df <- dat[indices, ]
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
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
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
  }
out_noweights   = boot(dat, lavaan_sim_noweights, 2)
# Now estimate the model with white respondents. 
lavaan_sim_whites = function(dat, indices){
  .df <- dat[indices, ]
  .df <- subset(.df, white == 1)
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
# DELETE AND FIX EVERYTHWERE 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
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
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
}
out_whites   = boot(dat, lavaan_sim_whites, 100)
# And Latinos
lavaan_sim_latinos = function(dat, indices){
  .df <- dat[indices, ]
  .df <- subset(.df, latino == 1)
  .df$pre =   ifelse(.df$treat==1, 1, 0)
  .df$post1 = ifelse(.df$treat==2, 1, 0)
  .df$post2 = ifelse(.df$treat==3, 1, 0)
  
  .df$RT = .df$republican  * .df$post1
  .df$IT = .df$independent * .df$post1
  
  .df$TT1 = .df$trump_vote  * .df$post1
  .df$TT2 = .df$trump_vote  * .df$post2
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df) 
  
  ## Regex out all the relevant stuff ###
  
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
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
  }
out_latinos   = boot(dat, lavaan_sim_latinos, 100)
# I don't see evidence that an oversample or something that YouGov has done 
# produced discrepancies on racial/ethnic lines.
### Concern about experiments ###
lavaan_sim_exp = function(dat, indices){
  .df <- dat[indices, ]
  den1 <- glm(I(treat==2) ~ + authoritarianism + rr + independent + republican + trump_vote + rwm +
                age + female + latino + black + college + ideology + christian + as.factor(state), 
              data = .df, family=binomial("logit")) %>% predict(type = "response")
  den2 <- glm(I(treat==3) ~ + authoritarianism + rr + independent + republican + trump_vote + rwm +
                age + female + latino + black + college + ideology + christian + as.factor(state), 
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
  
  model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2
              violent ~ VIOLENT
              criticize ~ SM

"
  
  # model1a <-  " hard =~ violent + burn
  #               soft =~ recount + criticize + court
  #               participation =~ participation_persuade + participation_socialmedia + 
  #               participation_yard + participation_volunteer + 
  #               participation_protest + participation_contact  +  participation_donate
  #               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
  #               concern_delay + concern_illegal
  # "
  full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                                
                                "concern_covid", "concern_lines", "concern_intimidate",
                                "concern_accept", "concern_delay", "concern_illegal",
                                "participation_persuade", "participation_socialmedia",
                                "participation_yard", "participation_volunteer", "participation_protest", 
                                "participation_contact", "participation_donate"
  ) 
  
  
  model1 = cfa(model1a, ordered=full_data_key, 
               data=.df, sampling.weights = "weights1") 
  model2 = cfa(model1a, ordered=full_data_key, 
               data=.df, sampling.weights = "weights2") 
  
  ## Regex out all the relevant stuff ###
  
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
  for (i in 1:4){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 5:8){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_b", i), tc - ta)
    assign(paste0("biden2_b", i), bc - ba)
  }
  return(c(biden_a1, biden_a2, biden_a3, biden_a4,
           trump_a1, trump_a2, trump_a3, trump_a4, 
           biden2_b5,  biden2_b6, biden2_b7, biden2_b8, 
           trump2_b5, trump2_b6, trump2_b7, trump2_b8
  ))
}
out_sim_exp   = boot(dat, lavaan_sim_exp, 100)



out_weights

out_sim_exp %>% plot_data_generator() %>% subset(type == "Soft") %>% 
  mutate(period = factor(period, c("Uncertainty", "Post_Call"))) %>%
  ggplot(aes(y=middle, ymin=lo, ymax=hi, x=period, colour=voter)) + 
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.01) +
  scale_colour_manual(name="Party", values=c("blue", "red"))+
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="right", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Conditional Treatment Effects of Post-Call") +
  labs(caption="2020 Western States Study. Estimates are doubly robust (G-estimation/IPW)")+
  scale_y_continuous("ATE", limits = c(-1, 1)) + 
  scale_x_discrete("Form of Contestation") +
  geom_hline(yintercept = 0) 




# model1a <-  " hard =~ violent + burn
#               soft =~ recount + criticize + court
#               participation =~ participation_persuade + participation_socialmedia + 
#               participation_yard + participation_volunteer + 
#               participation_protest + participation_contact  +  participation_donate
#               concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 



names(dat) # SM; VIOL
dat$pre =   ifelse(dat$treat==1, 1, 0)
dat$post1 = ifelse(dat$treat==2, 1, 0)
dat$post2 = ifelse(dat$treat==3, 1, 0)
dat$TT1 = dat$trump_vote  * dat$post1
dat$TT2 = dat$trump_vote  * dat$post2

model_items <-  " hard =~ violent + burn 
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal
              hard ~ trump_vote + post1 + post2 + TT1 + TT2
              soft ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern ~ trump_vote + post1 + post2 + TT1 + TT2
              violent ~ VIOLENT
              criticize ~ SM
"
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate"
) 
model1 = cfa(model_items, ordered=full_data_key, 
             data=dat) 

# This works fine -- now put in a function and simulate the effects of everything.Bootstrap, 
# Pull out effects


# Then just estimate predictions from the lavaan model #
# It should 

## If all this works, the final model is one where we regress: violent ~ VIOLENT + post1 + post2 + post1:VIOLENT....

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
               "anger",
               "anxiety",
               "authoritarianism")

controls = c("")

b$t
              
## Plot function ##
b.c


         ## Calculate fitted values from this ##



weight2 <- glm(I(treat==3) ~ + authoritarianism + rr + independent + republican + trump_vote  + rwm +
                 age + female + latino + black + college + ideology + christian + as.factor(state), 
               data = dat, family=binomial("logit")) %>% 
  augment(type.predict = "response") %>%
  mutate(wts = 1 / (ifelse(dat$post_call != 2, 1 - .fitted, .fitted)) ) ### These are the propensity score weights




# estimation of denominator of ip weights
denom.fit <- glm(qsmk ~ as.factor(sex) + as.factor(race) + age + I(age^2) + 
                   as.factor(education) + smokeintensity + 
                   I(smokeintensity^2) + smokeyrs + I(smokeyrs^2) + 
                   as.factor(exercise) + as.factor(active) + wt71 + I(wt71^2), 
                 family = binomial(), data = nhefs.nmv)
summary(denom.fit)


model<-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"

model2 =  cfa(mod, ordered=names(dat)[1:4], 
              data=dat, group = "group")


a0_corr1 = lavInspect(model1, what = "std.lv")$a0$beta[3]
a1_corr1 = lavInspect(model1, what = "std.lv")$a1$beta[3]