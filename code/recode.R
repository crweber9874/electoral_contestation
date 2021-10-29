# Recode data file.... (July 19, 2021)
# Electoral contestation
library(readstata13)
library(MASS)
library(ggplot2)
detach("packages:rlang")
detach("packages:car")
library(dplyr)
library(lavaan)
library(broom)
library(xtable)

df = read.dta13("data.dta",  
                missing.type=TRUE, generate.factors=TRUE)

#df = read.csv("~/Dropbox/electoral_contestation/zipcodes.csv")

### Recodes
state_list = c("Washington", "Oregon", "California", 
               "Idaho", "Utah", "Nevada", "Arizona",
               "New Mexico", "Colorado", "Wyoming", 
               "Montana", "Texas", "Oklahoma", "Kansas",
               "Sonora, Mexico", "Chihuaha, Mexico", 
               "Coahila, Mexico", "British Columbia, Canada",
               "Alberta, Canada")
region_list = c("West", "Southwest", "Northwest", 
                "West Coast", "Intermountain West",
                "Rocky Mountains", "Other")
state = ifelse(df$WSS01_1 == "selected", "Washington", NA) 
region = ifelse(df$WSS01_b_1 == "selected", "West", NA) 
opt = paste0("WSS01_", c(2:19))
for (i in 1:length(opt)){
  state = ifelse(df[, opt[i]] == "selected", state_list[i+1] , state) 
}

opt = paste0("WSS01_b_", c(2:7))
for (i in 1:length(opt)){
  region = ifelse(df[, opt[i]] == "selected", region_list[i+1] , region) 
}
df$vote1 <- car::recode(as.numeric(df$WSS36_b), "1=1 ; 2=0; else=NA")
df$vote2 <- car::recode(as.numeric(df$WSS36_c), "1=1 ; 2=0; else=NA")
df$voted <- ifelse(is.na(df$WSS36_b), df$vote2, df$vote1)
df$state = state
df$region = region
### Agree and Disagree Key ###
agree_key <- list(`1`= 5, `2`= 4,`3`= 3, `4`= 2, `5`= 1)
disagree_key <- list(`1`= 1, `2`= 2,`3`= 3, `4`= 4, `5`= 5)

#detach("package:car")
#library(dplyr)
df =  df %>%
  mutate(id = seq(1:nrow(df))) %>%
  mutate(state  =  state) %>%
  mutate(region =  region) %>%
  mutate(state_residence =  inputstate) %>%
  mutate(state_insult    = recode(as.numeric(WSS02_1), !!!agree_key)) %>%
  mutate(state_common    = recode(as.numeric(WSS02_2), !!!agree_key)) %>%
  mutate(state_interest  = recode(as.numeric(WSS02_3), !!!agree_key)) %>% 
  mutate(state_we        = recode(as.numeric(WSS02_4), !!!agree_key)) %>%
  mutate(state_partme    = recode(as.numeric(WSS02_5), !!!agree_key)) %>%
  mutate(state_proud     = recode(as.numeric(WSS02_6), !!!agree_key)) %>%
  mutate(state_identity_identity_strength = state) %>%
  mutate(moral_ind1      = recode(as.numeric(WSS09_b_1), !!!agree_key)) %>%
  mutate(moral_ind2      = recode(as.numeric(WSS09_b_2), !!!agree_key)) %>%
  mutate(moral_ind3      = recode(as.numeric(WSS09_b_3), !!!agree_key)) %>%
  mutate(moral_ind4      = recode(as.numeric(WSS09_b_4), !!!agree_key)) %>%
  mutate(sdo1            = recode(as.numeric(WSS10_1), !!!agree_key)) %>%
  mutate(sdo2r           = recode(as.numeric(WSS10_2), !!!disagree_key))  %>%
  mutate(sdo3            = recode(as.numeric(WSS10_3), !!!agree_key))  %>%
  mutate(sdo4r           = recode(as.numeric(WSS10_4), !!!disagree_key))  %>%
  mutate(rr1             = recode(as.numeric(WSS11_1), !!!agree_key)) %>%
  mutate(rr2r            = recode(as.numeric(WSS11_2), !!!disagree_key))  %>%
  mutate(rr3r            = recode(as.numeric(WSS11_3), !!!disagree_key))  %>%
  mutate(rr4             = recode(as.numeric(WSS11_4), !!!agree_key)) %>%
  mutate(emp1            = recode(as.numeric(WSS12_1), !!!agree_key)) %>%
  mutate(emp2            = recode(as.numeric(WSS12_2), !!!agree_key)) %>%
  mutate(emp3            = recode(as.numeric(WSS12_3), !!!agree_key)) %>%
  mutate(emp4            = recode(as.numeric(WSS12_4), !!!agree_key)) %>%
  mutate(ind1            = recode(as.numeric(WSS13_1), !!!agree_key)) %>%
  mutate(ind2r           = recode(as.numeric(WSS13_2), !!!disagree_key)) %>%
  mutate(ind3            = recode(as.numeric(WSS13_3), !!!agree_key)) %>%
  mutate(ind4r           = recode(as.numeric(WSS13_4), !!!disagree_key)) %>%
  # The fifth one doesn't have text.
  mutate(ind5r           = recode(as.numeric(WSS13_6), !!!disagree_key)) %>%
  mutate(anxiety         = recode(as.numeric(WSS14_1), !!!disagree_key)) %>%
  mutate(anger           = recode(as.numeric(WSS14_2), !!!disagree_key)) %>%
  mutate(hope            = recode(as.numeric(WSS14_3), !!!disagree_key)) %>%
  mutate(pride           = recode(as.numeric(WSS14_4), !!!disagree_key)) %>%
  mutate(disgust         = recode(as.numeric(WSS14_5), !!!disagree_key)) %>%
  mutate(enthusiasm      = recode(as.numeric(WSS14_6), !!!disagree_key)) %>%
  mutate(urban_r1        = recode(as.numeric(WSS31_1), !!!agree_key)) %>%
  mutate(urban_r3        = recode(as.numeric(WSS31_2), !!!agree_key)) %>%
  mutate(urban_r3        = recode(as.numeric(WSS31_3), !!!agree_key)) %>%
  mutate(urban_r4        = recode(as.numeric(WSS31_4), !!!agree_key)) %>%
  mutate(trump_vote      = voted) %>%
  mutate(age             = 2020 - birthyr) %>%
  mutate(gen             = car::recode(age, "18:39 = 'Millenial'; 40:59 = 'Gen X'; 60:80 = 'Boomer'; 80:92 = 'Greatest'" )) %>%
  mutate(female          = recode(as.numeric(gender), `1`=0, `2`=1)) %>%
  mutate(white           = car::recode(as.numeric(race),  "1=1 ;  2:8=0"))  %>%
  mutate(latino          = car::recode(as.numeric(race), "3=1;  1:2=0; 4:8=0"))  %>%
  mutate(black           = car::recode(as.numeric(race), " 2=1;  1=0; 3:8=0"))  %>%
  mutate(other           = car::recode(as.numeric(race), " 1:3=0; 4:8=1"))   %>%
  mutate(party3          = car::recode(as.numeric(pid3), "1='Democrat'; 2= 'Independent'; 3 = 'Republican'; else=NA" )) %>%
  mutate(independent     = car::recode(as.numeric(pid3), "1=0; 2=1; 3=0; else=NA" )) %>%
  mutate(republican      = car::recode(as.numeric(pid3), "1=0; 2=0; 3 = 1; else=NA" )) %>%
  mutate(democrat        = car::recode(as.numeric(pid3),"1=1; 2=0; 3 = 0; else=NA" )) %>%
  mutate(pid7            = car::recode(as.numeric(pid7), "8:9 = NA"))  %>%
  mutate(vote2016        = car::recode(as.numeric(presvote16post), "1=0; 2=1; else=NA")) %>%
  mutate(married         = car::recode(as.numeric(marstat), "1=1; 2:6=0")) %>%
  mutate(college         = car::recode(as.numeric(educ), "1:3=0; 4:8=1"))   %>%
  mutate(income          = car::recode(as.numeric(faminc_new), "1:7=0; 8:17=1"))  %>% 
  mutate(ideology        = recode(as.numeric(ideo5), `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)) %>%
  mutate(christian       = recode(as.numeric(religpew), `1`=1, `2`=1, `3`=1, `4`=1, `5`=0, `6`=0, `7`=0, `8`=0, `9`=0, `10`=0, `11`=0, `12`=0)) %>%
  mutate(interest        = recode(as.numeric(newsint), `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(auth1           = recode(as.numeric(WSS07_1a), `1`= 0, `2`=1)) %>% 
  mutate(auth2           = recode(as.numeric(WSS07_1b), `1`= 1, `2`=0)) %>% 
  mutate(auth3           = recode(as.numeric(WSS07_1c), `1`= 0, `2`=1)) %>% 
  mutate(auth4           = recode(as.numeric(WSS07_1d), `1`= 0, `2`=1)) %>%
  mutate(az_t1           = recode(as.numeric(WSS28_split), `1`= 0, `2`=1)) %>%
  mutate(az_rep_state    = as.numeric(WSS28_1)) %>%
  mutate(az_rep_nat      = as.numeric(WSS28_2)) %>%
  mutate(az_dem_state    = as.numeric(WSS28_3)) %>%
  mutate(az_dem_nat      = as.numeric(WSS28_4)) %>%
  mutate(VIOLENT         = recode(as.numeric(WSS40_1_split), `1`= 1, `2`=0)) %>%
  mutate(violent         = recode(as.numeric(WSS40_1),  !!!agree_key)) %>%
  mutate(burn            = recode(as.numeric(WSS40_2),  !!!agree_key)) %>%
  mutate(court           = recode(as.numeric(WSS40_3),  !!!agree_key)) %>%
  mutate(recount         = recode(as.numeric(WSS40_4),  !!!agree_key)) %>%
  mutate(criticize       = recode(as.numeric(WSS40_5),  !!!agree_key)) %>%
  mutate(SM              = recode(as.numeric(WSS40_5_split),  `1`= 1, `2`=0)) %>%
  mutate(trust_congress  = recode(as.numeric(WSS54_1),  `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_president = recode(as.numeric(WSS54_2),  `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_sc        = recode(as.numeric(WSS54_3),  `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_governor  = recode(as.numeric(WSS54_4),  `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_stateleg  = recode(as.numeric(WSS54_5),  `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_police    = recode(as.numeric(WSS54_6),   `1`=4, `2`=3, `3`=2, `4`=1)) %>%
  mutate(trust_science   = recode(as.numeric(WSS54_7),   `1`=4, `2`=3, `3`=2, `4`=1))  %>%
  mutate(efficacy_state  = recode(as.numeric(WSS33_split),  `1`=0, `2`=1 ))  %>%
  mutate(efficacy_complicated  = recode(as.numeric(WSS33_1), !!!disagree_key ))  %>%
  mutate(efficacy_dontcare  = recode(as.numeric(WSS33_2), !!!disagree_key ))  %>%
  mutate(efficacy_buycott  = recode(as.numeric(WSS34_a),  `1`=1, `2`=0 ))  %>%
  mutate(efficacy_purchase  = recode(as.numeric(WSS34_b),  `1`=1, `2`=0 ))  %>%
  mutate(confidence_ballot  = recode(as.numeric(WSS37),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
### Election concerns
  mutate(concern_usps       = recode(as.numeric(WSS41_1),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_covid      = recode(as.numeric(WSS41_2),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_lines      = recode(as.numeric(WSS41_3),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_intimidate = recode(as.numeric(WSS41_4),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_accept     = recode(as.numeric(WSS41_5),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_delay      = recode(as.numeric(WSS41_6),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(concern_illegal    = recode(as.numeric(WSS41_7),  `1`=4, `2`=3, `3`=2, `4`=1 ))  %>%
  mutate(court1  = recode(as.numeric(WSS49_1),  !!!agree_key))  %>%
  mutate(court2  = recode(as.numeric(WSS49_2),  !!!disagree_key))  %>%
  mutate(court3  = recode(as.numeric(WSS49_3),  !!!disagree_key))  %>%
  mutate(court3  = recode(as.numeric(WSS49_3),  !!!disagree_key))  %>%
  mutate(participation_meeting  = recode(as.numeric(WSS32_1), `1`=1, `2`=0 ))  %>%
  mutate(participation_yard  = recode(as.numeric(WSS32_2), `1`=1, `2`=0 ))  %>%
  mutate(participation_volunteer  = recode(as.numeric(WSS32_3), `1`=1, `2`=0 ))  %>%
  mutate(participation_protest  = recode(as.numeric(WSS32_4), `1`=1, `2`=0 ))  %>%
  mutate(participation_contact  = recode(as.numeric(WSS32_5), `1`=1, `2`=0 ))  %>%
  mutate(participation_donate = recode(as.numeric(WSS32_6), `1`=1, `2`=0 ))  %>%
  mutate(participation_socialmedia  = recode(as.numeric(WSS32_7), `1`=1, `2`=0 ))  %>%
  mutate(participation_persuade  = recode(as.numeric(WSS32_8), `1`=1, `2`=0 ))  %>%
  mutate(participation_none  = recode(as.numeric(WSS32_9), `1`=1, `2`=0 )) 
# I don't konw why????


# 1 AZ
# 2 COL
# 3 NV
# 4 NM
# 5 UT

df$state =  car::recode(as.numeric(df$state_residence), "3=1; 6=2; 29=3; 32=4; 45=5")

df$rwm = 0 
df$rwm[df$WSS58_open == "A non blue supportive source" ] <- 1
df$rwm[df$WSS58_open == "Alex Jones News Max" ] <- 1
df$rwm[df$WSS58_open == "All media lies for the democrats" ] <- 1
df$rwm[df$WSS58_open == "American Thinker, The Last Refuge" ] <- 1
df$rwm[df$WSS58_open == "Any non-Democrat supporting cable news, like OANN" ] <- 1
df$rwm[df$WSS58_open == "American Thinker, The Last Refuge" ] <- 1
df$rwm[df$WSS58_open == "Bill O'Reilly" ] <- 1
df$rwm[df$WSS58_open == "Bill O'Reilly.com " ] <- 1
df$rwm[df$WSS58_open == "Bill O'reilly" ] <- 1
df$rwm[df$WSS58_open == "Blaze, Epoch Times" ] <- 1
df$rwm[df$WSS58_open == "Bllaze, oann, Breibart, the daily cal.." ] <- 1
df$rwm[df$WSS58_open == "Bongino" ] <- 1
df$rwm[df$WSS58_open == "Breightbart and other news sites" ] <- 1
df$rwm[df$WSS58_open == "Breitbart" ] <- 1
df$rwm[df$WSS58_open == "Breitbart, Daily Mail, NewsMax" ] <- 1
df$rwm[df$WSS58_open == "Breitbart, Epoch Times" ] <- 1
df$rwm[df$WSS58_open == "Breitbart.com" ] <- 1
df$rwm[df$WSS58_open == "Breitbart.com, One America News" ] <- 1
df$rwm[df$WSS58_open == "Breitbart.com, The Blaze, OANN.com" ] <- 1
df$rwm[df$WSS58_open == "Brietbart" ] <- 1
df$rwm[df$WSS58_open == "Conservative Websites" ] <- 1
df$rwm[df$WSS58_open == "Conservative radio. Rush Limbaugh #1"] <- 1 
df$rwm[df$WSS58_open == "Conservative talk radio" ] <- 1
df$rwm[df$WSS58_open == "Daily Wire"] <- 1
df$rwm[df$WSS58_open == "Daily wire" ] <- 1
df$rwm[df$WSS58_open == "Daily wire, Breitbart"] <- 1
df$rwm[df$WSS58_open == "Dan Bongino" ] <- 1
df$rwm[df$WSS58_open == "Dr. Steve Turley"] <- 1
df$rwm[df$WSS58_open == "Ephich Times" ] <- 1
df$rwm[df$WSS58_open == "Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Epoch Times Newspaper" ] <- 1
df$rwm[df$WSS58_open == "Epoch Times, BBC America"] <- 1
df$rwm[df$WSS58_open == "Epoch Times, Federalist, BBC" ] <- 1
df$rwm[df$WSS58_open == "Epoch Times, The Federalist"] <- 1
df$rwm[df$WSS58_open == "Epoch times"] <- 1
df$rwm[df$WSS58_open == "Epoch, CBN"] <- 1
df$rwm[df$WSS58_open == "Epoch, Denver Gazette"] <- 1
df$rwm[df$WSS58_open == "Fox Business Channel, Newsmax"] <- 1
df$rwm[df$WSS58_open == "Gateway pundit, breitbart"] <- 1
df$rwm[df$WSS58_open == "LUCIANNE, REVOLVER"] <- 1
df$rwm[df$WSS58_open == "Local Conservative radio stations"] <- 1
df$rwm[df$WSS58_open == "New York Post, GodLikeProductions, NaturalNews"] <- 1
df$rwm[df$WSS58_open == "News Max"] <- 1
df$rwm[df$WSS58_open == "NewsMax"] <- 1
df$rwm[df$WSS58_open == "NewsMax & The Blaze Network"] <- 1
df$rwm[df$WSS58_open == "Newsmax"] <- 1
df$rwm[df$WSS58_open == "Newsmax TV"] <- 1
df$rwm[df$WSS58_open == "Newsmax and OANN"] <- 1
df$rwm[df$WSS58_open == "Newsmax, Epoch Times"] <- 1
df$rwm[df$WSS58_open == "Newsmax, OANN, Internet news sites"] <- 1
df$rwm[df$WSS58_open == "OAN"] <- 1
df$rwm[df$WSS58_open == "OAN N"] <- 1
df$rwm[df$WSS58_open == "OAN Newsmax"] <- 1
df$rwm[df$WSS58_open == "OAN, C-SPAN"] <- 1
df$rwm[df$WSS58_open == "OAN, NEWSMAX"] <- 1
df$rwm[df$WSS58_open == "OAN, Newsman"] <- 1
df$rwm[df$WSS58_open == "OAN, Newsmax"] <- 1
df$rwm[df$WSS58_open == "OANN"] <- 1
df$rwm[df$WSS58_open == "OANN, Censored.news, etc"] <- 1
df$rwm[df$WSS58_open == "OANN, Newsmax, blogs"] <- 1
df$rwm[df$WSS58_open == "Oan news/ variety of blogs"] <- 1
df$rwm[df$WSS58_open == "Oann"] <- 1
df$rwm[df$WSS58_open == "One America News"] <- 1
df$rwm[df$WSS58_open == "One America News , newsmax"] <- 1
df$rwm[df$WSS58_open == "One America News Network"] <- 1
df$rwm[df$WSS58_open == "One America News, NewsMax"] <- 1
df$rwm[df$WSS58_open == "OneAmericaNews"] <- 1
df$rwm[df$WSS58_open == "POANN"] <- 1
df$rwm[df$WSS58_open == "Private Conservative news sources"] <- 1
df$rwm[df$WSS58_open == "Q"] <- 1
df$rwm[df$WSS58_open == "Quanon"] <- 1
df$rwm[df$WSS58_open == "Redstate, Daily Caller, Townhall, Breitbart,etc."] <- 1
df$rwm[df$WSS58_open == "Rush Limbaugh"] <- 1
df$rwm[df$WSS58_open == "Steven Crowder"] <- 1
df$rwm[df$WSS58_open == "The AP, Epoch Times, BBC"] <- 1
df$rwm[df$WSS58_open == "The Blaze"] <- 1
df$rwm[df$WSS58_open == "The Daily Wire"] <- 1
df$rwm[df$WSS58_open == "The Epoch Times"] <- 1
df$rwm[df$WSS58_open == "The blaze tv"] <- 1
df$rwm[df$WSS58_open == "Townhall.com"] <- 1
df$rwm[df$WSS58_open == "Washington Examiner"] <- 1
df$rwm[df$WSS58_open == "Wnd, EpochThimes, other trusted aggregators"] <- 1
df$rwm[df$WSS58_open == "breitbart"] <- 1
df$rwm[df$WSS58_open == "conservative sources"] <- 1
df$rwm[df$WSS58_open == "drudge report, yahoo news"] <- 1
df$rwm[df$WSS58_open == "epoch times"] <- 1
df$rwm[df$WSS58_open == "news max and world net daily"] <- 1
df$rwm[df$WSS58_open == "newsmax"] <- 1
df$rwm[df$WSS58_open == "newsmax oan"] <- 1

df$rwm[df$WSS58_open == "newsmax oan"] <- 1

### Item Characteristics ####
psych::alpha(cbind(df$state_insult, df$state_common, 
                   df$state_interest, df$state_we, 
                   df$state_partme, df$proud))  ##
psych::alpha(cbind(df$moral_ind1, df$moral_ind2, 
                   df$moral_ind3, df$moral_ind4))  ## 
psych::alpha(cbind(df$sdo1, df$sdo2r, 
                   df$sdo3, df$sdo4r), check.keys=TRUE)  ## 
psych::alpha(cbind(df$rr1, df$rr2r, 
                   df$rr3r, df$rr4), check.keys=TRUE)  ## 
psych::alpha(cbind(df$emp1, df$emp2, 
                   df$emp3, df$emp4), check.keys=TRUE)  ## 
psych::alpha(cbind(df$ind1, df$ind2r, 
                   df$ind3, df$ind4r, df$ind5r ), check.keys=TRUE)  ## 
psych::alpha(cbind(df$auth1, df$auth2, 
                   df$auth3, df$auth4), check.keys=TRUE)  ## 
psych::alpha(cbind(df$urban_r1, df$urban_r2, 
                   df$urban_r3, df$urban_r4), check.keys=TRUE)  ## 
psych::alpha(cbind(df$sdo1, df$sdo2r, 
                   df$sdo3, df$sdo4r), check.keys=TRUE)  ##
#### These are some psychological scales:                                
cor(df$anger, df$anxiety)


zero.one<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}

df$surveillance = rowMeans(cbind(df$anger, df$anxiety, df$disgust), na.rm=T) %>% zero.one()
df$disposition = rowMeans(cbind(df$hope, df$pride, df$enthusiasm), na.rm=T) %>% zero.one()

df$state_pride = rowMeans(cbind(df$state_insult, df$state_common, 
                                df$state_interest, df$state_we, 
                                df$state_partme, df$proud), na.rm=T) %>% zero.one()
df$moral_individualism = rowMeans(cbind(df$moral_ind1, df$moral_ind2, 
                                        df$moral_ind3, df$moral_ind4), na.rm=T)%>% zero.one()
df$rr = rowMeans(cbind(df$rr1, df$rr2r, 
                       df$rr3r, df$rr4), na.rm=T)%>% zero.one()

df$group_empathy = rowMeans(cbind(df$emp1, df$emp2, 
                                  df$emp3, df$emp4), na.rm=T)%>% zero.one()
df$individualism = rowMeans(cbind(df$ind1, df$ind2r, 
                                  df$ind3, df$ind4r, df$ind5r ), na.rm=T)%>% zero.one()
df$authoritarianism = rowMeans(cbind(df$auth1, df$auth2, 
                                     df$auth3, df$auth4), na.rm=T)
df$urban_resentment = rowMeans(cbind(df$urban_r1, df$urban_r2, 
                                     df$urban_r3, df$urban_r4), na.rm=T)%>% zero.one()
df$sdo    = rowMeans(cbind(df$sdo1, df$sdo2r, 
                           df$sdo3, df$sdo4r), na.rm=T)%>% zero.one()


## dat = df %>% subset(select = c("pid", "authoritarianism", "violent", "burn", "court", "recount", "criticize", "VIOLENT", "SM"))
## write.csv(dat, file="~/Desktop/western.csv")
df$DATE =   as.Date(df$endtime, "%YY-%mm-%dd", tz = "MST")
df$post_election = ifelse(df$DATE > "2020-11-03", 1, 0)

df$post_call = ifelse(df$DATE >= "2020-11-07", 1, 0)
df$uncertainty = ifelse(df$DATE > "2020-11-03", 1, 0)
df$treat = NA
df$treat = ifelse(df$post_call ==0 & df$uncertainty ==0, 1, df$treat)
df$treat = ifelse(df$post_call ==0 & df$uncertainty ==1, 2, df$treat)
df$treat = ifelse(df$post_call ==1 & df$uncertainty ==1, 3, df$treat)

df$strength = abs(df$pid7-4) %>% zero.one()

#### Use this to export useable data somewhere #####
dat = 
  df %>% 
  subset(select = c("treat", "authoritarianism" ,"rr", "age", "party3", "rwm", "pid7",
                    "female", "latino", "black", "college", "ideology", "white",
                    "christian", "VIOLENT", "SM", "violent", "recount", "criticize", 
                    "burn", "court", "state", "democrat", "republican", "independent", 
                    "trump_vote", "surveillance", "anxiety", "anger", "hope", "pride", 
                    "disgust", "enthusiasm", "disposition", "strength",
                    "trust_congress" , "trust_president", "trust_sc",
                    "trust_governor", "trust_stateleg", "trust_police",
                    "trust_science", "efficacy_state", "efficacy_complicated",
                    "efficacy_dontcare",  "efficacy_buycott", "efficacy_purchase",
                    "participation_persuade", "participation_socialmedia",
                    "participation_yard", "participation_volunteer", "participation_protest", 
                    "participation_contact", "participation_donate", "confidence_ballot",
                    "violent", "burn", "court", "recount", "criticize",
                    "trust_congress" , "trust_president", "trust_sc",
                    "concern_covid", "concern_lines", "concern_intimidate",
                    "concern_accept", "concern_delay", "concern_illegal", "sdo"
                    
                    )) 


  

### Dummy out each variable  ####
df$age    = df$age %>% zero.one


dat$uncertainty = ifelse(df$treat == 2, 1, 0)
dat$post_call   = ifelse(df$treat == 3, 1, 0)
dat = na.omit(dat)


dat$RT = dat$republican * dat$post_call
dat$IT = dat$independent * dat$post_call
dat$TT = dat$trump_vote * dat$post_call

#### Generate the state interactions ####

dat$AZ  = ifelse(dat$state == 1, 1, 0)
dat$COL = ifelse(dat$state == 2, 1, 0)
dat$NV  = ifelse(dat$state == 3, 1, 0)
dat$NM  = ifelse(dat$state == 4, 1, 0)
dat$UT  = ifelse(dat$state == 5, 1, 0)


save(dat, file = "raw_data.rda")


