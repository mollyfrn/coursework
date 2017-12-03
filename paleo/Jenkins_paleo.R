####Paleoclimatology Final Analysis####
##Molly Jenkins 
##12/03/2017 
####Local vs Regional Comparison of Tree Ring and Lake level data in SE PA#### 
##and the surrounding region

#set wd and load libraries 
# setwd("C:/git/coursework/paleo")
library(tidyverse) #data cleaning and visualization 
library(stats)
library(MASS)
library(rstanarm)
library(rstan)
library(gamlss) 
library(coda)     #mcmc's and Bayesian stats for nonlinear analysis