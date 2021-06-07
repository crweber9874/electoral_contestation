lavaan_model ="
            ### Measurement Modeld
              hard          =~ violent + burn
              
              soft          =~ recount + criticize + court
              
              participation =~ participation_yard + participation_volunteer + 
                               participation_protest + participation_contact  +  participation_donate
              
              concern        =~ concern_covid + concern_lines + concern_intimidate + 
                                concern_accept + concern_delay + concern_illegal
            
              ### Structural Model  
              
              hard          ~ trump_vote + post1 + post2 + TT1 + TT2
              soft          ~ trump_vote + post1 + post2 + TT1 + TT2
              participation ~ trump_vote + post1 + post2 + TT1 + TT2
              concern       ~ trump_vote + post1 + post2 + TT1 + TT2
              trust_federal        ~ trump_vote + post1 + post2 + TT1 + TT2
              trust_state       ~ trump_vote + post1 + post2 + TT1 + TT2
              confidence_ballot ~ trump_vote + post1 + post2 + TT1 + TT2
              efficacy_complicated ~ trump_vote + post1 + post2 + TT1 + TT2
              efficacy_dontcare ~ trump_vote + post1 + post2 + TT1 + TT2

"


lavaan_sim = function(dat, indices, ordinal_data = ordinal_data,
                      lavaan_model = lavaan_model, weight_1 = weight_1,
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
  for (i in 1:9){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 10:18){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_a", i), tc - ta)
    assign(paste0("biden2_a", i), bc - ba)
  }
  
  
  
  return(c(biden_a1, biden_a2, biden_a3, biden_a4, biden_a5, biden_a6, biden_a7, biden_a8, biden_a9, 
           trump_a1, trump_a2, trump_a3, trump_a4, trump_a5, trump_a6, trump_a7, trump_a8, trump_a9,
           biden2_a10,  biden2_a11, biden2_a12, biden2_a13, biden2_a14, biden2_a15, biden2_a16, biden2_a17, biden2_a18,
           trump2_a10, trump2_a11, trump2_a12, trump2_a13, trump2_a14, trump2_a15, trump2_a16, trump2_a17, trump2_a18 ))
}

lavaan_sim_noweights = function(dat, indices, ordinal_data = ordinal_data,
                      lavaan_model = lavaan_model, weight_1 = weight_1,
                      weight_2 = weight_2){
    .df <- dat[indices, ]
  
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
               data=.df) 
  model2 = cfa(lavaan_model, ordered=ordinal_data, 
               data=.df) 
  

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
  for (i in 1:9){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bb = as.numeric(temp_dat[i,1:5]) %*% c(0, 1, 0, 0, 0) ## Biden, Mid
    tb = as.numeric(temp_dat[i,1:5]) %*% c(1, 1, 0, 1, 0) ## Trump, Mid
    ### Form Treatment Effects
    assign(paste0("trump_a", i), tb - ta)
    assign(paste0("biden_a", i), bb - ba)
  }
  
  for (i in 10:18){
    ba = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 0, 0, 0) ## Biden, Pre
    ta = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 0, 0, 0) ## Trump, Pre
    bc = as.numeric(temp_dat[i,1:5]) %*% c(0, 0, 1, 0, 0) ## Biden, Post
    tc = as.numeric(temp_dat[i,1:5]) %*% c(1, 0, 1, 0, 1) ## Trump, Post
    ### Form Treatment Effects
    assign(paste0("trump2_a", i), tc - ta)
    assign(paste0("biden2_a", i), bc - ba)
  }
  
  
  return(c(biden_a1, biden_a2, biden_a3, biden_a4, biden_a5, biden_a6, biden_a7, biden_a8, biden_a9, 
           trump_a1, trump_a2, trump_a3, trump_a4, trump_a5, trump_a6, trump_a7, trump_a8, trump_a9,
           biden2_a10,  biden2_a11, biden2_a12, biden2_a13, biden2_a14, biden2_a15, biden2_a16, biden2_a17, biden2_a18,
           trump2_a10, trump2_a11, trump2_a12, trump2_a13, trump2_a14, trump2_a15, trump2_a16, trump2_a17, trump2_a18 ))
}
