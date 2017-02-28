#Molly Jenkins
#Assignment 5
#ENEC563 
#02/27/2017
# 
# 1) Explain the experimental design that was used here. 
# Clearly identify the different kinds of experimental units and treatments using the language that is appropriate for this design.
# 
# 2) Analyze the manner in which species, depth, and season affected the sodium content of fish eggs using a linear mixed model.
# 
# 3) Prepare a graph that summarizes the results of your analysis.
# 
# 4) Assume that the three values of the season variable are equally spaced in time. 
# Refit your final model from question 2 but this time treat season as a continuous variable with equally spaced values. 
# Superimpose your final continuous season model on the graph of Question 3.
# 
# 5) Interpret your final model of Question 4. 
# In terms of this model how does the sodium content of the eggs of the two species differ? 
# Give both a qualitative and a quantitative answer.


#setwd(C:/git/coursework/ENEC563/)

fisheggs = read.table('https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/fisheggs.txt', header = TRUE)

####Problem 1####
#This is a split-plot design 
table(fisheggs$species, fisheggs$depth, fisheggs$season)
#nicely balanced

####Problem 2####
mod1 = aov(sodium~species*depth*season + Error(pond/species/depth/season), data = fisheggs) #structured error term
summary(mod1)

####Problem 3####
library(ggplot2)
intplot <- ggplot(fisheggs,aes(y=sodium))
intplot+stat_summary(aes(x=species,linetype=depth,group=depth),geom="line"
                     ,fun.y="mean")+labs(title=expression("species"%*%"depth interaction"))
intplot+stat_summary(aes(x=species,linetype=season,group=season),geom="line"
                     ,fun.y="mean")+labs(title=expression("species"%*%"season interaction"))
intplot+stat_summary(aes(x=depth,linetype=season,group=season),geom="line"
                     ,fun.y="mean")+labs(title=expression("depth"%*%"season interaction"))

####Problem 4####


####Problem 5####
