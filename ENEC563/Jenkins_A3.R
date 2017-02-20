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
library(nlme)

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
summary(mod1) #just can't happen! 0 vals for the residuals! 
anova(mod1)
mod2 = lm(n~tr*A+rep, data = mossart)
summary(mod2) #explains ~83% of the variation, 73% adjusted
anova(mod2)
mod3 = lm(n~tr+A+rep, data = mossart)
summary(mod3) #explains 83% var again, 75% adjusted
anova(mod3)

#####Problem 3: Mixed effect mod####
mod4 = lme(n~tr*A,random=~1|rep,data=mossart,control=list(opt="optim"))
summary(mod4) #Tau = 0.04608588, sigma = 3.007672
anova(mod4)
mod5 = lme(n~tr+A,random=~1|rep,data=mossart,control=list(opt="optim"))
summary(mod5) #Tau = 0.04766504, sigma = 2.942209
anova(mod5)

VarCorr(mod4)
vars = as.numeric(VarCorr(mod4)[,1])
corr = vars[1]/sum(vars)
Tau = vars[1]
Sig = vars[2]


mean.dat <- data.frame(fix.ests=predict(mod2), mix.ests=predict(mod4)) 
moss2 <- cbind(mossart, mean.dat) 
moss2$mix.ests = as.numeric(moss2$mix.ests)

moss3 = moss2 %>%
  group_by(tr, A) %>% 
  mutate(pop_mean = mean(n)) %>%
  select(everything())



#keeps saying pop means the same for both treatments + areas
  
#in summary we can see our est for our intercept, the effect, the SE estimates, and p values 
#we can also see StdDev and Intercept = Tau; Residual = sigma 
#if squared would get sigma and tau squared
#if intercept higher than residual then indicative original model would be a source of huge error

#random = U_0i -> random effect of pot -> is a normal with mean 0 and variance Tau^2 
#within each pot there's a random mean, and we don't know what that is 

#there's var in each plant in each pot, and each pot is also going to have random var 

#can partition that variation and error into fixed (mean of 0, var of sigma) and random (mean of 0 and var of Tau)


####Problem 4####
##compare among block to within block by examining dfs & ratio of Tau to sig  
#is there an interaction between treatment and area? looks like the interactive models are marginally better than the additive mods in both cases, but not by much at all 
#compare mod2 and mod4? fixed vs random/mixed 
#rep very clearly explains a substantial proportion of the variation in the data

####Problem 5####
library(ggplot2)
theme_set(theme_bw())

ggplot(moss3,aes(x=tr,y=n))+geom_point(aes(color="Raw data"),size=1)+
    labs(x="Treatment",y="richness",color="")+facet_wrap(~A)+
  geom_point(aes(y=mix.ests,color="Conditional means"),shape="|", size = 5)+
  geom_point(aes(y=fix.ests,color="Fixed estimates"), size = 1) + 
  geom_point(data=moss3,aes(y=pop_mean, color = "Population mean"))+
  scale_color_manual( values = c("Raw data" = "grey","Conditional means" = "firebrick",
                                 "Fixed estimates" = "turquoise", "Population mean" = "olivedrab"))


# It should show the average species richness of plots as predicted by your model for those treatments that had a significant effect on richness.
# It should clearly demonstrate how the individual treatment factors (not block) operate separately and in concert.
# It should show the actual data points

