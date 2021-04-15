### Factor Models ####

dat = df %>% subset(select = c(c("violent", "burn", "court", "recount", "criticize",
                                              "trust_congress" , "trust_president", "trust_sc",
                                              "efficacy_complicated",
                                              "efficacy_buycott", "efficacy_purchase",
                                              "participation_persuade", "participation_socialmedia",
                                              "participation_yard", "participation_volunteer", "participation_protest", 
                                              "participation_contact", "participation_donate")))
psych::fa.parallel(dat, fm = 'minres', fa = 'fa')
psych::fa(efa_data, nfactors=4) %>% loadings

#dat  <- cbind(lavPredict(fit1), dat)
psych::alpha(cbind(dat$violent, dat$burn, dat$court, dat$recount, dat$criticize), check.keys=TRUE)  ## 0.69
psych::alpha(cbind(dat$violent, dat$burn), check.keys=TRUE) 
cor(cbind(dat$violent, dat$burn)) ## These items go together well...
