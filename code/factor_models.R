### Electoral Contestation
### Factor models
### Excutes: One image (scree plot), one table (CFI, TLI), semPlot, and factor correlations.
# Updated with confidence, concern questions: delete buycotting, etc.
# just correlate, in footnote, with other relevant items.
library("ggplot2")
library("semPlot")
library("psych")

### First, examine measurement structure of trust ###

full_data_key = c(             "trust_congress" , "trust_president", "trust_sc",
                               "trust_stateleg", "trust_governor"
                               ) 
## Trust##
dat = df %>% subset(select = full_data_key) %>% na.omit()
p.analysis<-psych::fa.parallel(dat, fm = 'minres', fa = 'fa', n.iter=100)
with(df, psych::alpha(cbind( trust_congress , trust_president, trust_sc))) # 0.62
with(df, corr(cbind( trust_stateleg , trust_governor))) # 0.76
df$trust_federal = with(df, rowMeans(cbind( trust_congress , trust_president, trust_sc))) %>% zero.one()
df$trust_state = with(df, rowMeans(cbind( trust_stateleg , trust_governor))) %>% zero.one()
## Efficacy##
efficacy_complicated
efficacy_dontcare
## Electoral Concern ##
full_data_key = c(            "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal"
) 

dat = df %>% subset(select = full_data_key) %>% na.omit()
p.analysis<-psych::fa.parallel(dat, fm = 'minres', fa = 'fa', n.iter=100)
fa(dat, fm = 'minres', nfactors = 2)
## Concern is two dimensional, corresponding to concern over covid, line
## concern_covid, concern_lines, concern_intimidate
## These scales stink....
with(df, psych::alpha(cbind( concern_covid , concern_lines, concern_intimidate))) 
with(df, psych::alpha(cbind( concern_intimidate, concern_accept , concern_delay, concern_illegal))) # 0.62
df$locus_internal = with(df, rowMeans(cbind( concern_intimidate, concern_accept , concern_delay, concern_illegal))) %>% zero.one()
df$locus_external = with(df, rowMeans(cbind( concern_covid , concern_lines, concern_intimidate))) %>% zero.one()
#### Ballot confidence ###
table(df$confidence_ballot)
#df$confidence_ballot
#### Participation ###

### For the factor model ####
#### This should include everything in raw form
### Analyzed items (just to clean up the data)
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "trust_governor", "trust_stateleg",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate"
) 
### The factor analysis forms the first part of the scale development section
#### It starts with an EFA on a pretty exhaustive set of items
dat = df %>% subset(select = full_data_key) %>% na.omit()
p.analysis<-psych::fa.parallel(dat, fm = 'minres', fa = 'fa', n.iter=100, cor = "mixed")
factor_model<-fa(dat, fm = 'minres', nfactors = 6, cor = "mixed")
### The factor solution is pretty good. There are five factors:
### Hard contestation, soft contestation, trust, participation, and electoral concern
str(factor_model$loadings, cutoff = 0.3) 
class(factor_model$loadings)
x = unclass(factor_model$loadings)
x = ifelse(abs(x)>0.3, x, NA)   ## This look okay -- not many cross loadings ####
rownames(x) = c("Protest_Election", "Burn", "Court", "Recount", "Criticize",
                                              "Trust(Congress)" , "Trust(President)", "Trust(SCOTUS)",
                                              "Trust(Governor)", "Trust(Legislature)", 
                "Concern(Covid)", "Concern(Lines)", "Concern(Intimidate)",
                "Concern(Illegal)", "Concern(Delay)", "Concern(Illegal)",
                "Yard Sign", "Volunteer",
                "Protest", "Contact Official", "Donate")
colnames(x) = paste("Factor", c(1:5), sep=" ")  ## Table 1
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
##### Now estimate a bunch of factor models to explore uniqueness -- ignore observed for now.#####
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "trust_governor", "trust_stateleg",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate"
) 
model1a <-  " hard =~ violent + burn
              
              soft =~ recount + criticize + court
              
              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1b <-  "   
              soft =~ recount + criticize + court + violent + burn
              
              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1c <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate + 
              recount + criticize + court + violent + burn
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1d <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc + 
              recount + criticize + court + violent + burn
              trust2 =~ trust_governor + trust_stateleg  
              
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"


model1e <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg  + 
              recount + criticize + court + violent + burn
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              

"

model1f <-  "   

              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate  

              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg  
              
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal + 
              recount + criticize + court + violent + burn
              

"




### Useful descriptives
describe(dat)
cor(with(dat, cbind(nat1, nat2, nat3, nat4)), use="complete")
psych::alpha(with(dat, cbind(auth1, auth2, auth3, auth4)))
#dat  <- cbind(lavPredict(fit1), dat)
psych::alpha(cbind(dat$violent, dat$burn, dat$court, dat$recount, dat$criticize), check.keys=TRUE)  ## 0.69
psych::alpha(cbind(dat$violent, dat$burn), check.keys=TRUE) 
cor(cbind(dat$violent, dat$burn)) ## These items go together well...
cfa(model1a, ordered=full_data_key, data=df[,full_data_key])  %>% summary(fit.measures = TRUE)


fit <- rbind(
  cfa(model1a, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1b, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1c, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1d, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1e, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli")),
  cfa(model1f, ordered=full_data_key, data=df[,full_data_key]) %>% fitMeasures(c("chisq", "rmsea", "cfi", "tli"))
  
  ) %>% data.frame() 
rownames(fit) <- c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6")
names(fit) <- c("Chi-Squared", "RMSEA", "CFI", "TLI")

## Fix caption #
xtable(fit, caption = "\textbf{Model 1} is a six factor model, where `hard' and `soft' contestation items
       load on separate factors, and trust in federal institutions, trust in state institutions,
       concern, and participation also load on independent factors.
       \textbf{Model 2} is a five factor model, where all contestation items load on one factor, trust in federal institutions, trust in state institutions, concern, and participation 
       load on independent factors. 
       \textbf{Model 3} is a four factor model, where the contestation items load on the 
       participation factor, and trust in federal institutions, trust in state institutions, and concern items 
       load on independent factors.
       \textbf{Model 4} is a four factor model, where the contestation items load on the 
       load on  trust in federal institutions, and participation, trust in state institutions, and concern items 
       load on independent factors.
            \textbf{Model 5} is a four factor model, where the contestation items load on the 
       load on  trust in state institutions, and participation, trust in federal institutions, and concern items 
       load on independent factors.
        load on independent factors.
            \textbf{Model 6} is a four factor model, where the contestation items load on the 
       load on the ``concern'' factor, and participation, trust in federal institutions, and trust in state institutions items 
       load on independent factors.")
### Divergent and Convergent Validity ####
full_data_key = c(            "violent", "burn", "court", "recount", "criticize",
                              "trust_congress" , "trust_president", "trust_sc",
                              "trust_governor", "trust_stateleg",
                              "concern_covid", "concern_lines", "concern_intimidate",
                              "concern_accept", "concern_delay", "concern_illegal",
                              "participation_yard", "participation_volunteer", "participation_protest", 
                              "participation_contact", "participation_donate",
                              "confidence_ballot", "efficacy_dontcare", "efficacy_complicated"
) 
model1a <-  " hard =~ violent + burn
              
              soft =~ recount + criticize + court
              
              participation =~ participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              
              
              trust1 =~ trust_congress + trust_president + trust_sc
              trust2 =~ trust_governor + trust_stateleg 
              
              concern =~ concern_covid + concern_lines + concern_intimidate + 
              concern_accept + concern_delay + concern_illegal
              
              hard ~~ confidence_ballot
              soft ~~ confidence_ballot
              
              hard ~~ efficacy_complicated
              soft ~~ efficacy_complicated
              
              hard ~~ efficacy_dontcare
              soft ~~ efficacy_dontcare
              
              
              

"
corr.output = cfa(model1a, ordered=full_data_key, data=df[,full_data_key])

rbind(lavInspect(corr.output, "cor.lv")  [c("hard", "soft"),] %>% t(),
lavInspect(corr.output, "cor.all") [c("efficacy_dontcare", "efficacy_complicated", "confidence_ballot"), c("hard", "soft")]) %>% 
  xtable(caption = "Correlations between hard and soft contestation with participation, federal institution trust,
         state institution trust, concern about election integrity, confidence in the election, and internal and external efficacy.")


##### End Factor Models ###
##### Next Section: Predicting outcomes, ordinal scale #####







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

model<-  " hard =~ violent + burn
              soft =~ recount + criticize + court
              participation =~ participation_persuade + participation_socialmedia + 
              participation_yard + participation_volunteer + 
              participation_protest + participation_contact  +  participation_donate
              trust =~ trust_congress + trust_president + trust_sc
              concern =~ concern_covid + concern_lines + concern_intimidate + concern_accept + 
              concern_delay + concern_illegal"



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
