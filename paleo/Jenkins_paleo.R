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
sealev = read.csv("miller_sealev.csv", header = TRUE)
sealev = sealev[, 1:2]
colnames(sealev) <- c("Age(ma)", "sealevel(m)")
#derived sea level data at global 

#derived temp data at global 
temps = read.csv("LGM_temps.csv", skip = 35, header = FALSE) 
colnames(temps) <- c("Longitude", "Latitude", 
                     "WOA (1998) 10-m temperature (annual, deg C)",
                     "WOA (1998) 10-m temperature (JAS, deg C)",
                     "WOA (1998) 10-m temperature (JFM, deg C)",
                     "Reconstructed LGM SST (annual, deg C)",
                     "Reconstructed LGM SST (JAS, deg C)",
                     "Reconstructed LGM SST (JFM, deg C)",
                     "LGM anomaly (annual, deg C)",
                     "LGM anomaly (JAS, deg C)",
                     "LGM anomaly (JFM, deg C)",
                     "Normalized average reliability level (0-1)",
                     "Number of records", 
                     "Number of different proxies",
                     "LGM anomaly, propagated error (annual, deg C)",
                     "LGM anomaly, propagated error (JAS, deg C)",
                     "LGM anomaly, propagated error (JFM, deg C)",
                     "LGM anomaly, standard deviation (annual, deg C)",
                     "LGM anomaly, standard deviation (JAS, deg C)",
                     "LGM anomaly, standard deviation (JFM, deg C)",
                     "LGM anomaly, total error (annual, deg C)",
                     "LGM anomaly, total error (JAS, deg C)",
                     "LGM anomaly, total error (JFM, deg C)",
                     "LGM anomaly, minimum (annual, deg C)",
                     "LGM anomaly, minimum (JAS, deg C)",
                     "LGM anomaly, minimum (JFM, deg C)",
                     "LGM anomaly, maximum (annual, deg C)", 
                     "LGM anomaly, maximum (JAS, deg C)",
                     "LGM anomaly, maximum (JFM, deg C)")

temps2 = temps %>% 
  dplyr::select(-4, -5, -7, -8, -10, -11, -16, -17, 
                -19, -20, -22, -23, -25, -26, -28, -29)

temps2[temps2 == -99.99] <- NA

#write.csv(temps2, "tidytemps.csv", row.names = FALSE)

temps = read.csv("tidytemps.csv", header = TRUE)

#remove NA rows 
temp2 = na.omit(temps)
#pinpoint global avg at lgm 

lgmmean = mean(temp2[,3])

##############
chesap = read.delim("cronin2010b-composite.txt", skip = 96, fill = TRUE, header = TRUE)
#core derived temp data using isotope ratio of Mg to Ca 
monthly = read.csv("monthly_csv.csv", header = TRUE)
nhemi = read.table("nhemi.txt", skip = 1, nrows = 143, header = TRUE, fill = TRUE )
nhemi2 = nhemi %>% dplyr::select(Year, J.D) %>% 
  filter(Year != "Year" & J.D != "J-D")
nhemi2$J.D = as.integer(as.character(nhemi2$J.D))

chesap <- data.frame(chesap)  # convert to dataframe
p1 = ggplot(chesap) + geom_line(aes(x=age_AD, y=temp, color="temp")) +
  geom_line(aes(x=age_AD, y=Mg.Ca, col="Isotopic ratio")) + scale_color_discrete(name="Legend") +
  labs(title="Cronin Chesapeake Isotopic Temperature Patterns") # plot multiple time series using 'geom_line's
p1

#add ref point for temp @ lgm using temp2 data
p1+geom_abline(slope = 0, intercept = lgmmean)

nhemi2 <- data.frame(nhemi2)
p2 = ggplot(nhemi2)+geom_point(aes(x = Year, y = J.D))+
  labs(title = "Northern hemisphere reconstructed averages")
p2

p1 
#add in global data for comp 

