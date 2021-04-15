## Run this file first.....
## You just need to set the dropbox file name you use, and then change the directory.
## I'm trying to clean this up, so please do ask if you have questions...
rm(list = ls())
setwd("~/electoral_contestation/")  ### Set your directory.
#source("~/Dropbox/electoral_contestation/helper_functions.r")

### Uncomment and run #####
# install.packages("rstan")
# install.packages("ggplot2")
# install.packages("MASS")
# install.packages("pscl")
# install.packages("tidyverse")
# install.packages("car")
# install.packages("foreign")
# install.packages("lavaan")
# install.packages("readstata13")
library("rstan")
library("ggplot2")
library("MASS")
library("pscl")
library("tidyverse")
library("car")
library("foreign")
library("lavaan")
library("readstata13")

### Now progress through files, according to README
