### Electoral Contestation
### Factor models
### Excutes: One image (scree plot), one table (CFI, TLI), semPlot, and factor correlations.
# Updated with confidence, concern questions: delete buycotting, etc.
# just correlate, in footnote, with other relevant items.

library("ggplot2")
library("semPlot")
library("psych")
### Analyzed items (just to clean up the data)
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate") 

dat = df %>% subset(select = full_data_key) %>% na.omit()
p.analysis<-psych::fa.parallel(dat, fm = 'minres', fa = 'fa', n.iter=100)
factor_model<-fa(dat, fm = 'minres', nfactors = 5)
### The factor solution is pretty good. There are five factors:
### Hard contestation, soft contestation, trust, participation, and electoral concern
str(factor_model$loadings, cutoff = 0.3) 
class(factor_model$loadings)
x = unclass(factor_model$loadings)
x = ifelse(abs(x)>0.3, x, NA) 
rownames(x) = c("violent", "burn", "court", "recount", "criticize",
                                              "trust_congress" , "trust_president", "trust_sc",
                                              "concern_covid", "concern_lines", "concern_intimidate",
                                              "concern_accept", "concern_delay", "concern_illegal",
                                               "Persuade","Social Media", "Yard Sign", "Volunteer",
                                               "Protest", "Contact Official", "Donate")
colnames(x) = paste("Factor", c(1:5), sep=" ")
xtable(x) ## This is about as clean as it gets

#### Figure 1 #####
plot.dat<-with(p.analysis, data.frame(sim=fa.sim, eigenvalues=fa.values,
                                      rep=fa.simr, factors=c(1:length(fa.values))))
gplot<-data.frame(eigenvalues=c(plot.dat$sim,  plot.dat$eigenvalues, plot.dat$rep),
                  condition=rep(c("Simulated", "True", "Resampled"), each=length(plot.dat$eigenvalues)),
                  factor=rep(c(1:length(plot.dat$eigenvalues)), times=3)
)
plot2<- ggplot(gplot, aes(x=factor, y=eigenvalues,
                          colour=as.factor(condition))) +
  geom_point(alpha=0.3)+
  geom_line(alpha=0.3)+
  scale_colour_manual(name="Condition", values=c("blue", "red", "purple"))+
  ggtitle("Parallel Factor Analysis") +
  theme(panel.background=element_rect(fill="white")) +
  theme(plot.background=element_rect(fill="white")) +
  # Format the grid
  theme(panel.grid.major=element_line(colour="#D0D0D0",size=.25)) +
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.text.y=element_text(size=11,colour="#535353",face="bold")) +
  theme(axis.title.y=element_text(size=11,colour="#535353",face="bold",vjust=1.5)) +
  theme(axis.title.x=element_text(size=11,colour="#535353",face="bold",vjust=-.5))+
  scale_x_continuous("Factors", limits=c(0,20))+
  scale_y_continuous("Eigenvalues", limits=c(-1,4))
plot2
### Useful descriptives
describe(dat)
cor(with(dat, cbind(nat1, nat2, nat3, nat4)), use="complete")
psych::alpha(with(dat, cbind(auth1, auth2, auth3, auth4)))
#dat  <- cbind(lavPredict(fit1), dat)
psych::alpha(cbind(dat$violent, dat$burn, dat$court, dat$recount, dat$criticize), check.keys=TRUE)  ## 0.69
psych::alpha(cbind(dat$violent, dat$burn), check.keys=TRUE) 
cor(cbind(dat$violent, dat$burn)) ## These items go together well...
### Sem analysis 
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate") 


model1a <-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"
model1b <-  " soft =~ recount + criticize + court + violent + burn
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"

model1c <-  " participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate + 
              recount + criticize + court + violent + burn
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"
model1d <-  " participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate 
              trust =~ trust_congress + trust_president + trust_sc +  recount + criticize + court + violent + burn
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"

model1e <-  " participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate 
              trust =~ trust_congress + trust_president + trust_sc  +  recount + criticize + court + violent + burn
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"
fit <- rbind(
  cfa(model1a, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1b, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1c, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1d, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1e, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli"))
  ) %>% data.frame() 
rownames(fit) <- c("Model 1", "Model 2", "Model 3", "Model4", "Model 5")
names(fit) <- c("Chi-Squared", "RMSEA", "CFI", "TLI")
xtable(fit, caption = "\textbf{Model 1} is a five factor model, where `hard' and `soft' contestation items
       load on separate factors, and trust, concern, and participation also load on independent factors.
       \textbf{Model 2} is a four factor model, where all contestation items load on one factor, and trust, concern, and participation 
       load on indepenedent factors. \textbf{Model 3} is a three factor model, where the contestation items to load on participation, and
       separate factors are estimated for trust and concern.  \textbf{Model 4} is a three factor model, where the contestation items to load on concern, and
       separate factors are estimated for participation and trust.  \textbf{Model 5} is a three factor model, where the contestation items 
       load on trust, and separate factors are estimated for concern and participation.")
### Divergent and Convergent Validity ####
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_persuade", "participation_socialmedia",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate", "efficacy_complicated", 
                              "efficacy_dontcare", "efficacy_buycott", "efficacy_purchase") 

##### Estimate the DIF Model ####
# Declare a three group model
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "treat") 
model1_noDIF <-  " hard =~ violent + burn
                   soft =~ recount + criticize + court"

cfa_model = cfa(model1a, ordered=full_data_key, data=df[,full_data_key],
                group = "treat") 


corrs = lavInspect(cfa_model,"cor.lv") %>% as.matrix %>% lower
rownames(corrs) <- colnames(corrs) <-  c("Hard", "Soft", "Participation", "Trust", "Efficacy")
xtable(corrs)
cfa_model  =  cfa(model1a, ordered=items, data=dat)
model  =  cfa(model1a, ordered=items, data=dat)




semPlot::semPaths(model, what="paths", "est", style="lisrel", rotation=1,
                  thresholds=FALSE, residuals=FALSE, intercepts=FALSE, 
                  sizeMan2=4, sizeMan=4, sizeLat2=4, sizeLat=4, 
                  label.prop=0.7, label.cex=1, title=TRUE, node.width=1,
                  edge.label.cex = 0.5, 
                  nCharNodes=0, label.font=1, label.scale=TRUE,
                  nodeLabels =  c("Protest", "Burn",
                                  "Court", "Criticize",
                                  "Recount", "Sign", "Volunteer",
                                  "Protest", "Persuade", "Media", "Sign", "Donate",
                                  "Congress", "President", "Scotus",
                                  "Complicated", "Buycott", "Purchase",
                                  "Hard", "Soft", "Participation", "Trust", "Efficacy"
                                  ))

corrs = lavInspect(cfa_model,"cor.lv") %>% as.matrix 
rownames(corrs) <- colnames(corrs) <- c("Hard", "Soft", "Participation", "Trust", "Efficacy")
xtable(corrs)
### Factor models for remaining analysis###
