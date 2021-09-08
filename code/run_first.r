## Electoral Contestation (July 14, 2021)

## Run this file first.....
## You just need to set the dropbox file name you use, and then change the directory.
rm(list = ls())
setwd("~/electoral_contestation/")  ### Set your directory.
source("~/electoral_contestation/code/helper_functions.r")
source("~/electoral_contestation/code/simulation_parameters.r")

# install.packages("rstan")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("pscl")
# install.packages("tidyverse")
# install.packages("car")
# install.packages("foreign")
# install.packages("lavaan")
# install.packages("readstata13")
# install.packages("semPlot")
# install.packages("psych")
# install.packages("lavaan")
library("ggplot2")
library("semPlot")
library("psych")
library("rstan")
library("ggplot2")
library("MASS")
library("pscl")
library("tidyverse")
library("car")
library("foreign")
library("lavaan")
library("readstata13")

labs <- c("1", "2", "3", "4", "5")

a <- ggplot(dat, aes(x=violent)) +
  geom_bar(mapping = aes(y=..prop..), alpha = 0.5) + 
  geom_density(adjust = 2, color = "darkgrey") +  
  scale_x_discrete("", limits = factor(c(1:5)), labels = labs) + 
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Attend Protests") +
  ylab("Proportion") + 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold")) +
  scale_y_continuous("Proportion", limits = c(0, 0.5)) 


b <- ggplot(dat, aes(x=burn)) +
  geom_bar(mapping = aes(y=..prop..), alpha = 0.5) + 
  geom_density(adjust = 2, color = "darkgrey") +  
  scale_x_discrete("", limits = factor(c(1:5)), labels = labs) + 
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_text(angle = 90)) + 
  ggtitle("Burn the American Flag") +
  labs(caption="2020 Western States Survey")+ 
  ylab("Proportion") + 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=45)) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold"))   +
  scale_y_continuous("Proportion", limits = c(0, 0.6)) 



c <- ggplot(dat, aes(x=court)) +
  geom_bar(mapping = aes(y=..prop..), alpha = 0.5) + 
  geom_density(adjust = 2, color = "darkgrey") +  
  scale_x_discrete("", limits = factor(c(1:5)), labels = labs) + 
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_text(angle = 45)) + 
  ggtitle("Contest election in court") +
  ylab("Proportion")+ 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=90)) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold"))   +
  scale_y_continuous("Proportion", limits = c(0, 0.6)) 



d <- ggplot(dat, aes(x=recount)) +
  geom_bar(mapping = aes(y=..prop..), alpha = 0.5) + 
  geom_density(adjust = 2, color = "darkgrey") +  
  scale_x_discrete("", limits = factor(c(1:5)), labels = labs) + 
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_text(angle = 45)) + 
  ggtitle("Ballot recounts") +
  ylab("Proportion") + 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=90)) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold"))   +
  scale_y_continuous("Proportion", limits = c(0, 0.6)) 




e <- ggplot(dat, aes(x=criticize)) +
  geom_bar(mapping = aes(y=..prop..), alpha = 0.5) + 
  geom_density(adjust = 2, color = "darkgrey") +  
  scale_x_discrete("", limits = factor(c(1:5)), labels = labs) + 
  scale_fill_grey() +
  theme_bw() + 
  theme(legend.position="none", axis.ticks = element_blank(), axis.text.x = element_text(angle = 45)) + 
  ggtitle("Criticize Integrity") +
  labs(caption="2020 Western States Survey")+ 
  theme(plot.title=element_text(face="bold",hjust=-.08,vjust=2,colour="#3C3C3C",size=12)) +
  theme(axis.text.x=element_text(size=10,colour="#535353",face="bold", angle=90)) +
  theme(axis.text.y=element_text(size=10, colour="#535353",face="bold"))   +
  scale_y_continuous("Proportion", limits = c(0, 0.7)) 

png("remaining_FINAL.png")
ggpubr::ggarrange(a, b, c, d, e, nrow = 3, ncol = 2)
dev.off()
## dat = df %>% subset(select = c("pid", "authoritarianism", "violent", "burn", "court", "recount", "criticize", "VIOLENT", "SM"))
## write.csv(dat, file="~/Desktop/western.csv")
df$DATE =   as.Date(df$endtime, "%YY-%mm-%dd", tz = "MST")
df$post_election = ifelse(df$DATE > "2020-11-03", 1, 0)
