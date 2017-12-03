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

#load in txt files 

####Chesapeake data####
# Chesapeake Bay 2400 Year Isotope-Mg/Ca Data and Temperature Estimates 
#----------------------------------------------------------------------- 
#                World Data Service for Paleoclimatology, Boulder 
#                                  and 
#                     NOAA Paleoclimatology Program 
#             National Centers for Environmental Information (NCEI)
#----------------------------------------------------------------------- 
#----------------------------------------------------------------------- 
# Variables 
#
# Data variables follow are preceded by "##" in columns one and two.
# Data line variables format:  Variables list, one per line, shortname-tab-longname-tab-longname components (9 components: what, material, error, units, seasonality, archive, detail, method, C or N for Character or Numeric data) 
#
## core	core name, , , , , , , ,C
## depth_cm	depth, , , cm, , , , ,N
## age_AD	age, , , AD, , , , ,N
## Mg/Ca	Magnesium/Calcium ratio, Elphidium selseyense, , mmol/mol, , paleoceanography, , ,N 
## temp	temperature, , , degrees C, , climate reconstructions, Estimated Bay Temp , ,N
## temp-sm	temperature, , , degrees C, , climate reconstructions, Estimated Bay Temp 5PtSmooth, ,N
#
#----------------
# Data:
# Data lines follow (have no #)
# Data line format - tab-delimited text, variable short name as header
chesap = read.delim("cronin2010b-composite.txt", skip = 96, fill = TRUE, header = TRUE)
chesap2 = read.delim("cronin2010b-md03-2661.txt", skip = 79, nrows = 25, fill = TRUE, header = TRUE)
chesap2b = read.delim("cronin2010b-md03-2661.txt", skip = 133, fill = TRUE, header = TRUE)
chesap3 = read.delim("cronin2010b-ptmc-3.txt", skip = 94, fill = TRUE, header = TRUE)
chesap4 = read.delim("cronin2010b-ptxt-2.txt", skip = 94, fill = TRUE, header = TRUE)
chesap5 = read.delim("cronin2010b-rd-2209.txt", skip = 95, fill = TRUE, header = TRUE)


####Tree ring data####



####Lake level data####
