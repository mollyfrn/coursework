#Molly Jenkins
#ENEC563
#Assignment 8 
#04/17/2017

# These data are taken from Carroll (1998) who took the species data from Pearson and Cassola (1992). 
#Pearson and Cassola (1992) subdivided the Indian subcontinent into geographic units 
#using a grid consisting of 61 squares, each 275 by 275 km on a side. 
#When this was not possible because the grid cells fell along the coast or along international borders, 
#squares were established that had areas approximately equal to the other squares. 
#Within each grid cell the numbers of tiger beetle species (beetles) 
#and non-aquatic and non-marine breeding bird species (birds) were determined based on published accounts. 
#The altitudinal relief (relief) of each square was determined from topographic maps. 
#The variables latitude and longitude in the data frame correspond to the geographic centers of the grid cells.
# 
# Previous research on the use of bioindicators on the Indian subcontinent suggest that 
#tiger beetle diversity is a useful bioindicator of the number of bird species. 
#Altitudinal relief is also known to correlate with bird diversity. 
#The goal of this exercise is to obtain the best model that relates bird species richness 
#to both tiger beetle richness and altitudinal relief.
# 
####Pre-assignment code####
setwd("C:/git/coursework/ENEC563")
library(ggplot2)
library(dplyr)
library(MASS)
library(gamlss)

bbdf = read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/birdbeetles.txt", header = T)
hist(bbdf$birds)


####Questions####
# 1) To model bird species richness as a function of tiger beetle richness and altitudinal relief, 
# fit all of the standard regression models that are commonly used for count data.  
# Your models should include the following error structures:
#       i) Ordinary normal model
onmod = glm(Species~logarea, data=gala, family = gaussian) #with or without a log link?

#       ii) Poisson model
poimod = glm(Species~logarea, data=gala, family=poisson)


#       iii) negative binomial model (NB-2)
nbmod2 = glm.nb(Species~logarea, data=gala)
  

#       iv) negative binomial model (NB-1)
nbmod1 = gamlss(Species~log(Area), data=gala, family=NBII)
 
# 2) Compare the models and choose the "best". 
# Explain why you chose the model you did. 
# Summarize your work with a table listing: 
    # the models, 
    # their log-likelihoods, 
    # number of estimated parameters, 
    # and their AIC. 
my.AIC <- AIC(out.norm1, out.norm2, out.norm3, out.pois, out.NB2, out.gamlss2)
my.LL <- sapply(list(out.norm1, out.norm2, out.norm3, out.pois, out.NB2, out.gamlss2),
                logLik)
results1 <- data.frame(LL=my.LL, my.AIC)
results <- rbind(results1, lognorm.LL(gala$Species, out.lognorm),
                 sqrtnorm.LL(gala$Species, out.sqrtnorm))
rownames(results)[7:8] <- c('out.lognormal', 'out.sqrtnormal')
results[order(results$LL),]


# 3) Write down the equation for predicted bird richness from your final model. 
#   Interpret what the coefficients of the two predictors represent. 
#   For example, if beetle richness increases by one unit how does that affect the predicted bird richness? 
#   What summary measure of bird is richness is being predicted by your final model?


# 4) Check the goodness of fit of your final model: 
#     Use the method from lecture 20 that produces p-values for each observation 
#     and graph the p-values.
upper.p <- 1-pnbinom(gala$Species-1, mu=gala$mu, size=out.NB2$theta)
upper.p
sum(upper.p<.025)
gala$Island[upper.p<.025]

lower.p <- pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta)
lower.p
sum(lower.p<.025)
min(lower.p)

pval.dat <- data.frame(pvalue=c(lower.p, upper.p), island=rep(gala$Island,2), 
                       label=rep(c('lower', 'upper'), each=nrow(gala)))
ggplot(pval.dat,aes(x=pvalue,y=island))+geom_point()+facet_wrap(~label)+
  geom_vline(xintercept=.025,color="red",linetype=2)+
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())

# 5) Plot the geographic coordinates of the grid cells, 
#     using the latitude and longitude as the x and y aesthetics. 
#     Using your final model, color code the plotted points 
#     so that observations with positive model residuals are colored red 
#     and observations with negative model residuals are colored blue. 
#     Does the plot suggest any potential problems with your final model? What?
#use alt-occ example code from Fall 2016


