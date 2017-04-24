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
library(lme4)

byca = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/bycatch.csv", header = TRUE)

# Questions:
#1) Manly (2001) fits a Poisson regression model to these data in which he includes season, 
# area, gear type, and time of day as factors in a main effects model. 
# He fits the model in such a way that the response variable, Bycatch, can be interpreted 
# as a bycatch rate per tow, the number of dolphins killed per tow. Fit this model.  
# Recall that rates can be fit by adding an offset with the log of the tow variable.
mod1 = glm(Bycatch ~ Season + Area + Gear.Type+Time+offset(log(Tows)), data = byca, family = poisson)
summary(mod1)
#confint(mod1) #throwing up errors! 

# 2) Obtain the estimates of the area, gear type, and time effects. 
# Interpret what these estimates represent in practical terms. 
# For instance, how do the bycatch rates compare for night versus day, 
# south versus north, mid-water versus bottom?


ests = coef(mod1)[7:9]
#Bycatch generally consistent across all three variables; similar means and SE's, slightly higher for nighttime 
#but not much higher to exert a strong effect. 


# 3) Manly (2001), p. 100, states the following: 
# "All effects are highly significant in terms of the reduction in the deviance 
# that is obtained by adding them into the model, 
# and the final model gives a good fit to the data (chi-squared = 42.08 with 34 degrees of freedom, p = 0.161)." 
# Is Manly right? Why or why not?

###He is not right - the seasonal estimates introduce so much variation (esp. 90-91) on bycatch 
###in the relationship of bycatch to towing. 
###This obscures understanding any effects area, gear, and time may or may not have. 


# 4) Examine the summary table. 
# Do you see anything unusual in what's reported there 
# to suggest that maybe there is a problem with this model? What?

###The estimate and error reported for Season 1991-1992 wildly deviates from the other estimates.
###A standard error of almost 2500 does not make sense for this data, especially if for all other variables 
### it's between 0-1. 


# 5) In light of the problem you saw in question 4, refit the model 
# but this time treat Season as a random effect rather than a fixed effect.
mods2 = glmer(Bycatch~Area+Gear.Type+Time+(1|Season), data=byca, family=poisson)
summary(mods2)
ests2 = coef(mods2)

# 6) Carry out a graphical goodness of fit test (as in the last assignment) 
# for the random effects model. What observations appear to be poorly described by the model?
byca$lam <- fitted(mods2)
byca$rat = byca$Bycatch/byca$Tows 

upper.p = 1-ppois(byca$Bycatch-1, lambda =byca$lam)
upper.p
sum(upper.p<.025) # = 3
byca$Tows[upper.p<.025] 

lower.p <- ppois(byca$Bycatch, lambda =byca$lam)
lower.p
sum(lower.p<.025) #= 0
min(lower.p)

pval.dat <- data.frame(pvalue=c(lower.p, upper.p), Tows=rep(byca$Season,2), 
                       label=rep(c('lower', 'upper'), each=nrow(byca)))
ggplot(pval.dat,aes(x=pvalue,y=Tows))+geom_point()+facet_wrap(~label)+
  geom_vline(xintercept=.025,color="red",linetype=2)+
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())

byca$pplot <- ifelse(ppois(byca$Bycatch, lambda =byca$lam) < .5,
                     ppois(byca$Bycatch, lambda =byca$lam),
                     1-ppois(byca$Bycatch-1, lambda =byca$lam))
byca$lower <- ifelse(ppois(byca$Bycatch, lambda =byca$lam) < .5,
                     "lower","upper")
plot1 = ggplot(byca,aes(x=pplot,y=Tows,color=lower))+geom_point()+
  geom_vline(xintercept=.025,linetype=2)+
  labs(x = "", color="Tail of distribution")+theme(legend.position = "none")

plot2 = ggplot(byca,aes(x=pplot,y=Season,color=lower))+geom_point()+
  geom_vline(xintercept=.025,linetype=2)+theme(legend.position = "none")+
  labs(x="Tail probability of observation from model",color="Tail of distribution")

library(gridExtra)
library(grid)
library(ggmap)
#create a shared legend 
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="right"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lw <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  grid.arrange(arrangeGrob(grobs = gl), legend,
               ncol = 2, widths = unit.c(unit(1, "npc") - lw, lw))
}



grid_arrange_shared_legend(plot1, plot2)

ggplot(byca, aes(x=rat, y = Season))+geom_point(aes(color = "raw data"), size = 1)+
  geom_point(aes(x = lam, color = "Conditional means"), size = 4, shape = "|")+
  theme(legend.title = element_blank())+xlab("Bycatch/Tow Ratio") 
#some of our predications are still way high, way off but the rest seem to pair with the raw data really closely
#Worst predicted points have little consistently in common save for that they all had Bycatch vals of 5 or 6. 
