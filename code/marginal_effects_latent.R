### Electoral Contestation
### Estimate the factor model with covariates ####
library("ggplot2")
library("semPlot")
library("psych")
items = c("violent", "burn", "court", "recount", "criticize","post_call", "uncertainty")
dat1 = df[,items] %>% na.omit()
items = c("violent", "burn", "court", "recount", "criticize")
dat2 = df[,items] %>% na.omit()
#### Winner and Loser effects #####
# (1) The base input model, start by testing DIF
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "VIOLENT", "SM")
dat = df[,full_data_key]
## construct the dif group code 

model1a <-  "      hard =~ violent + burn
                   soft =~ recount + criticize + court"

cfa_model = cfa(model1a, ordered=full_data_key, data=df[,full_data_key],
                group = "treat") 

summary(cfa_model)







model_code <- "hard =~ recount + criticize 
                soft =~ court + violent + burn
                hard ~ post_call + uncertainty
                soft ~ post_call + uncertainty"
model = sem(m1, ordered=items, data=dat1)


### I embed the lavaan code in a bootsrapping routine ###
boot_marginals <- function(data, indices) {
  .df <- data[indices, ]
  model_code <- "hard =~ recount + criticize 
                soft =~ court + violent + burn
                hard ~ post_call + uncertainty
                soft ~ post_call + uncertainty"
  model = sem(m1, ordered=items, data=dat1) ### Now use this to create the marginal effects
  

  # This is the matrix call,
  # add the data
  as.matrix(dat1[,c("post_call", "uncertainty")]) %*% as.matrix(coef(model)[c("hard~post_call", "hard~uncertainty")]) 
  
    m1<-as.formula(hard ~ post_call + independent + republican +  RT + IT +  trump_vote + rwm  +   
                   rr +  age + female + latino + black + college + 
                   ideology + christian + as.factor(state))
  m2<-as.formula(soft ~ post_call + independent + republican +  RT + IT +  trump_vote + rwm  +   
                   rr +  age + female + latino + black + college + 
                   ideology + christian + as.factor(state))
  m1 = glm(m1, data = .df, weights = weights2) 
  m2 = glm(m2, data = .df, weights = weights2) 
  y0 <- m1 %>% 
    augment(newdata = counterfactuals$ind0) %>%
    subset(select = ".fitted")
  y1 <- m1 %>% 
    augment(newdata = counterfactuals$ind1) %>%
    subset(select = ".fitted")
  
  #  summarize the mean difference and pull from data frame
  bind_cols(y0, y1) %>% 
    summarise(
      mean_y0 = mean(y0$.fitted),
      mean_y1= mean(y1$.fitted),
      difference1 = mean_y1 - mean_y0,
    ) %>% 
    pull(difference1) ## Toggle between these -- can't get function to run as "statistic"
}

model_code <- "              
              hard =~ recount + criticize 
              soft =~ court + violent + burn
              hard ~ post_call + uncertainty
              soft ~ post_call + uncertainty
            "
            
counterfactual_data = list


#  predict on the synthetic responses, for each party
y0_dat_r <- data %>% 
  mutate(post_call  = 0) %>% 
  mutate(republican = 1) %>%  
  mutate(independent = 0) %>% 
  mutate(RT = 0)   %>%
  mutate(IT = 0)   
y1_dat_r <- data %>% 
  mutate(post_call  = 1) %>% 
  mutate(republican = 1) %>%  
  mutate(independent = 0) %>% 
  mutate(RT = 1)   %>%
  mutate(IT = 0)


model = cfa(m1, ordered=items, data=dat1) ### Now use this to create the marginal effects

### Manipulate a data set
dat1 = dat1 %>% mutate(post_call = 1)
lavPredict(model, type = "yhat", newdata= dat1)

treat = dat1
treat$post_call <- 1



