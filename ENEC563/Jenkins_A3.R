#Molly Jenkins
#Assignment 3
#ENEC563 
#02/14/2017

# The data are contained in a comma separated file. If you open the file in text reader, 
#you will notice that the first four rows contain descriptive information.
# 1_1) Read the data into R with one line of code.  
  #To do this you will need to learn how to skip rows at the beginning of a file by examing the help for the read.csv file.
  # 1_2) Rename the columns something more easy to type.
  # 1_3) Generate a table that shows how many replicates there are for each treatment combination for each block 
        #and indicate if the experiment is balanced.
  # 1_4) You will be analyzing all predictor variables (Treatment, Size and Replicate) as categorical variables.  
        #Make sure you convert them all to categorical ones.

# 2) Analyze this experiment as a fixed effect model
  # 2_1) Find the best model which uses the two experimental treatments and the blocking variable as fixed effects.  
        #You will be analyzing all predictor variables (Treatment, Size and Replicate) as categorical variables. 
  # 2_2) Explain why you cannot examine the three way interaction among all three variable.
  # 3) Find the best mixed model which uses the two experimental treatments as fixed effects and the blocking variable as a random effect.
    # Compare the amount of variation of the observations that is due to among block variation to the within block variation.
# 4) Give a one or two sentence description of what your final model from question 3 says.
# 5) Create a single graphic (the graphic may contain more than one panel) that summarizes the results of your analysis in question 3.
    # It should show the average species richness of plots as predicted by your model for those treatments that had a significant effect on richness.
    # It should clearly demonstrate how the individual treatment factors (not block) operate separately and in concert.
    # It should show the actual data points

#setwd("C:/git/coursework/ENEC563")
library(dplyr)


####Problem 1####
#1_1)
mossart = read.csv("Moss_ArthropodSR.csv", skip = 4)

#1_2)
mossart = mossart %>%
  rename("rep"= Replicates, "tr" = Treatment, "A" = Area..cm2., "n" = Species.Richness)

#1_3 
table(mossart$rep, mossart$tr, mossart$A)
#almost perfectly balanced save for the first block in the 20 Area patch


#1_4
mossart$tr = as.factor(mossart$tr)
mossart$A = as.factor(mossart$A)
mossart$rep = as.factor(mossart$rep)


####Problem 2:Fixed effect mod####
mod1 = lm(n~tr*A*rep, data = mossart)
summary(mod1) #just can't happen! 
mod2 = lm(n~tr*A+rep, data = mossart)
summary(mod2)
mod3 = lm(n~tr+A+rep, data = mossart)
summary(mod3)

#####Problem 3: Mixed effect mod####
mod4 = lme(n~tr*A,random=~1|rep,data=mossart,control=list(opt="optim"))
summary(mod4)
mod5 = lme(n~tr+A,random=~1|rep,data=mossart,control=list(opt="optim"))
summary(mod5)

#compare among block to within block


####Problem 4####
#

####Problem 5####
ggplot(mossart,aes(x=rep,y=n,color=tr,group=tr))+geom_point()+
  geom_path()+facet_grid(.~A)