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


#setwd("C:/git/coursework/ENEC563/")

library(ggplot2)
library(dplyr)
library(lme4)

fisheggs = read.table('https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/fisheggs.txt', header = TRUE)

####Problem 1####
#This is a split-plot design 
table(fisheggs$species, fisheggs$depth, fisheggs$season, fisheggs$pond)
#nicely balanced
fisheggs$species = as.factor(fisheggs$species)
fisheggs$pond = as.factor(fisheggs$pond)
fisheggs$season = factor(fisheggs$season,levels=c("early","middle","late"))
fisheggs$season_num = as.numeric(as.character((factor(fisheggs$season,labels=c(1,2,3)))))
#add labels to species factor to look nice for plotting 
fisheggs$species <- factor(fisheggs$species,
                    labels = c("Species 1", "Species 2"))



####Problem 2####
sink("A5_table1.txt")
mod1 = aov(sodium~species*depth*season + Error(pond/depth/season), data = fisheggs) #structured error term
summary(mod1)
sink()

sink("A5_table1a.txt")
mod1_a = lme(sodium~species*depth*season, random = ~1|pond/depth/season, data = fisheggs)
summary(mod1_a)
sink()
#season seems to have a non-trivial effect 

####Problem 3####
altplot = ggplot(fisheggs,aes(x=season,y=sodium))
altplot+stat_summary(aes(color=pond, group=pond),geom="line"
                     ,fun.y="mean")+facet_grid(species~depth)+theme_bw() 



####Problem 4####
# Refit your final model from question 2 but this time treat season as a continuous variable with equally spaced values. 
# Superimpose your final continuous season model on the graph of Question 3.
fisheggs$season = as.integer(fisheggs$season)
mod2 = aov(sodium~species*depth*season + Error(pond/depth/season), data = fisheggs)
summary(mod2)


mod3 = lme(sodium~species*depth*season, random = ~1|pond/depth/season, data = fisheggs)
fisheggs$pred = as.integer(predict(mod3, level = 0))


altplot = ggplot(fisheggs,aes(x=season,y=sodium))
altplot+stat_summary(aes(color=pond, group=pond),geom="line"
                     ,fun.y="mean")+facet_grid(species~depth)+
  geom_line(aes(y=pred),color="black")+theme_bw() 

####Problem 5####
#Eggs in species 1 generally had a higher sodium concentration than the eggs of Species 2. 
#This does not seem to vary significantly with depth nor pond. 
#However, early season eggs had an overall higher concentration of sodium than eggs laid late in the season, regardless of location within or across ponds.  
