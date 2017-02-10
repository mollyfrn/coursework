####Jenkins A2####
#ENEC563 
#Assignment 2 
# 02/10/2017 

#Generate an "effects" graph for your final egg count model from Assignment 1 paralleling the example done in class. 
#Plot all the regression parameter estimates (except the intercept) along with 95% confidence intervals.

#Using your final egg count model from Assignment 1 obtain estimates of the "treatment" means, their standard errors, 
#and 95% confidence intervals for the means.

#Use the information you calculated in Question 2 to produce an interaction graph of your model like the one done in class. 
#Place "Density" on the x-axis and show two mean profiles, one for each "Season". 
#Add 95% confidence intervals for the means and include a legend that clearly identifies which profile corresponds to which season.


#setwd("C:/git/core-transient/")
#ref answer key from A1; needed to overlay pred from models on obs
library(wesanderson)
library(dplyr)
library(car)
library(ggplot2)

####Pre-assignment analysis prep (from Assignment 1, corrected)####
quinn1 = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/quinn1.csv", header = TRUE)

quinn1$total_eggs = quinn1$Eggs*quinn1$Density 

spr_mean = mean(quinn1$Eggs[quinn1$Season == "spring"])
spr_median = median(quinn1$Eggs[quinn1$Season == "spring"])
sumr_mean = mean(quinn1$Eggs[quinn1$Season == "summer"])
sumr_median = median(quinn1$Eggs[quinn1$Season == "summer"])

quinn1$fdensity = as.factor(quinn1$Density)

mod1 = lm(Eggs~Season*fdensity, data = quinn1) #straight vs curved -> curved better bc was simpler model 
mod2 = lm(Eggs~Season+fdensity, data = quinn1) #adding nonlinearity to model doesn't increae overfitting by much 
mod3 = lm(Eggs~Season, data = quinn1)
mod4 = lm(Eggs~fdensity, data= quinn1)
summary(mod1)
anova(mod1)
summary(mod2) 
anova(mod2)
summary(mod3) 
anova(mod3)
summary(mod4) 
anova(mod4)

quinn1$modelpreds = predict(mod1,newdata=quinn1)

theme_set(theme_bw())
ggplot(quinn1,aes(x=fdensity,y=modelpreds,color=Season))+geom_point()+
  geom_line(aes(group=Season))+labs(y="Eggs laid",x="Limpet density")

coefs = coef(mod1) 
#overall # of eggs does go down with density but is also much greater in the summer compared to spring 

####Assignment 2 unique work####

#Problem 1

parests = coef(mod1)
vcov(mod1)

par.se <- sqrt(diag(vcov(mod1)))
upper95 <- parests+par.se*qt(.975,df.residual(mod1))
lower95 <- parests-par.se*qt(.975,df.residual(mod1))
parplotframe <- data.frame(varlabels=factor(names(parests), levels=names(parests)),
                           parests,lower95, upper95)
parplotframe2 = parplotframe[2:length(parplotframe$varlabels),] #removing intercept

theme_set(theme_bw())
ggplot(parplotframe2,aes(y=varlabels,xmin=lower95,x=parests,xmax=upper95))+labs(title = "Problem 1")+
  geom_errorbarh(height=.2)+
  geom_point()+geom_vline(xintercept=0,linetype=2)+
  labs(y="",x="Estimated Effect")


#Problem 2
predparms <- with(quinn1,expand.grid(levels(Season),levels(fdensity)))
predparms
names(predparms) <- c("Season","fdensity")
plotframe3 <- data.frame(predparms,predict(mod1,newdata=predparms,
                                         interval="confidence", level = .95))

#Problem 3
theme_set(theme_bw())
ggplot(plotframe3,aes(x=fdensity,y=fit,ymin=lwr,ymax=upr,color=Season))+labs(title = "Problem 3")+
  geom_errorbar(width=.2)+
  geom_point()
  +geom_line() 

round(summary(mod1)$coefficients,3)
