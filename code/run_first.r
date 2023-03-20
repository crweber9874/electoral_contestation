## Electoral Contestation (March 17, 2022)

## Run this file first.....
## You just need to set the dropbox file name you use, and then change the directory.
rm(list = ls())
setwd("/Users/Chris/Dropbox/github_repos/electoral_contestation/electoral_contestation")  ### Set your directory.
source("code/helper_functions.R")
source("code/simulation_parameters.r")
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
library("boot")
library("patchwork")
library("broom")
library("cobalt")
library("WeightIt")
library("MatchIt")