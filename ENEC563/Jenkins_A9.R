#Molly Jenkins
#ENEC563
#Assignment 9 
#04/24/2017

# These data are Example 3.3 of Manly (2001), pp. 96-100, who obtained the data from Baird (1996). 
#Manly describes the data as follows.
# 
# The accidental capture of marine mammals and birds in commercial fishing operations 
#is of considerable concern in many fisheries around the world. 
#This example concerns one such situation, which is of catches of the common dolphin (Delphinus delphis) 
#and the bottlenose dolphin (Tursiops truncatus) in the Taranaki Bight trawl fishery for jack mackerel 
#(Trachurus declivus, T. novaezalandiae, and T. murphyi) off the west coast of New Zealand. 
#The New Zealand Ministry of Fisheries puts official observers on about 10% of fishing vessels 
#to monitor dolphin bycatch. Data were collected by these observers for the six fishing seasons 
#1989/90 to 1994/95
#The table shows the number of observed trawls (Tows) 
#and the number of dolphins accidentally killed (Bycatch) 
#categorised by eight conditions for each fishing year: 
#the fishing area (the northern or southern Taranaki Bight), 
#by the gear type (bottom or midwater), 
#by the time (day or night). 
#Excluding five cases where there were no observed trawls, this gives 43 observations 
#on the bycatch under different conditions, in different years.

####Pre-assignment code####
setwd("C:/git/coursework/ENEC563")
library(ggplot2)
library(dplyr)
library(MASS)
library(gamlss)
library(maps)

byca = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/bycatch.csv", header = TRUE)

# Questions:
#1) Manly (2001) fits a Poisson regression model to these data in which he includes season, 
# area, gear type, and time of day as factors in a main effects model. 
# He fits the model in such a way that the response variable, Bycatch, can be interpreted 
# as a bycatch rate per tow, the number of dolphins killed per tow. Fit this model.  
# Recall that rates can be fit by adding an offset with the log of the tow variable.



# 2) Obtain the estimates of the area, gear type, and time effects. 
# Interpret what these estimates represent in practical terms. 
# For instance, how do the bycatch rates compare for night versus day, 
# south versus north, mid-water versus bottom?



# 3) Manly (2001), p. 100, states the following: 
# "All effects are highly significant in terms of the reduction in the deviance 
# that is obtained by adding them into the model, 
# and the final model gives a good fit to the data (chi-squared = 42.08 with 34 degrees of freedom, p = 0.161)." 
# Is Manly right? Why or why not?



# 4) Examine the summary table. 
# Do you see anything unusual in what's reported there 
# to suggest that maybe there is a problem with this model? What?



# 5) In light of the problem you saw in question 4, refit the model 
# but this time treat Season as a random effect rather than a fixed effect.



# 6) Carry out a graphical goodness of fit test (as in the last assignment) 
# for the random effects model. What observations appear to be poorly described by the model?