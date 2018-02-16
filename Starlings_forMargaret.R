### Author: Molly Jenkins 
### Date: 02/16/2018 
### This script takes the entire North American Breeding Bird Survey and subsets it to just Starling presence/absence over time. 
### This script also plots Starling data over time and space for the duration of the entire BBS. 

#setwd("C:/git/core-transient") 
library(tidyverse) 
library(ecodataretriever) 
BBS = "bioark.ad.unc.edu/Hurlbertlab/Jenkins/BBS/"

####Download BBS data at single route resolution, subset to just starlings (EUST; STUVUL)#### 

bbs = read.csv("bbs.csv", header = TRUE)

stars = bbs %>% 
  filter(AOU == "EUST" | AOU == "STUVUL")


####Plot North American continent with locations with Starling presence in at least 1 year####
NorthAm = 
routes = read.csv("bbs_routes.csv", header = TRUE)


####Extra: Over span of years included, color code routes by frequency of Starlings -> Green = every year, yellow = infrequently####


#Send to Margaret along with BBS data citation and R citation! 
