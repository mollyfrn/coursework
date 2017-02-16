### R code from vignette source '/home/james/work/talks/UNCInterview/uncinstruction.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: "code for processing do not run"
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=4,fig.width=6)

#James teaching interview 

#fixed and random effects diff 
#biggest step from going from a generalized linear model is the addition of random effect 

#f(mu) = linear combo of parms and explantory vars 
#f(mu) = B0+B1x(continuous var)+B2z1(categorical vars)+B3z2(levels; translates to multiple dummy variables for a three level cat variable)

#final part is incorporating obs -> yi = mu(mean) + Ei error term 

#so we have fixed effects, but we need to talk about random vars as well 

#which brings us to blocking: originally ag exp setup 
#nice that pulls in historical paradigm that gave rise to technique 

#each treatment paired w/in a block; accounts for nuisance noise and similar envs 

#blocks = cat variable could mod as just another cat var 
#sample of pop of soil conditions across env; want to be able to extend inference to whole pop 
#beyond sample 

#which is what turns it from a fixed to random effects 
#spatial blocks 
#can also have temporal random effects 
#taxonomic grouping 

#sometimes also called grouping or clustering vars -> same as random effect in model 

#variation in soil is kind of a nuisance variable but don't need a nuisance where random effects 
#just needed for extending inference beyond the measured effect

#how to incorporate random effects
#3 of each type (global and nominal) in each pot, 16 pots 
#grew to first true leaf; l:w ratio of each leaf 

#i = pot (group level) #, j = plant # (individual level)

Yij = B0+B1(Xij) #Xij = 1 nominal; = 0 globe 

#Zi = 1 if ith pot, = 0 if other pot 
#Bi is change in mean bet/1st-ith pot 

#lot of blocking vars in model -> lots of dimensionality gets bulky with overfitting 
#random can minimize # parms with blocking and create greater power/parsimony 

#combine all fixed effect parms into 1 parm that varies for each pot 

#Yij = B0+B1Xij+Uoi+Eij (U = rando effect; can have random slopes that rep intrcxns bet random and fixed)
#E is noise 

#random effect has Tau^2 normal dist (in lieu of sigma)
#probabilistic model 
#what is probability of getting our data given the parameters? 
#what is set of parms that makes our data the most probable? 
#max likelihood -> this can get computationally difficult w/mixed mods 

#fcn for mixed model gets an added integral 
#probability of getting both our observed data and the unobserved random effects 
#integrate across all possible vals of those random effect 
#broken down using Bayesian stats

#Bayesians perspective makes estimating U's easier 

#REML - restricted max likelihood is default to get best ests on variance terms 
#nlme and lme4 






###################################################
### code chunk number 2: read data in a plot data
###################################################
plants <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/jimsonweed.txt", header=T)
plants[1:12,]
plants$fpot <- factor(plants$pot)

library(ggplot2)
ggplot(plants,aes(y=lw.rat,x=fpot,color=type))+geom_point()+
    labs(y="Length to width ratio",x="Pot number")+
    scale_color_discrete("Plant type",labels=c("Globe","Nominal"))


###################################################
### code chunk number 3: fit a fixed effect model with lm
###################################################
fixedmod <- lm(lw.rat~fpot+type,data=plants) #both pot and type as explanatory 
anova(fixedmod)
summary(fixedmod)


###################################################
### code chunk number 4: fit a random effect model with lme
###################################################
# treat blocks as random: using nlme package
library(nlme)
mod1.lme <- lme(fixed=lw.rat~type, random=~1|fpot, data=plants)
#specify fixed and random as sep args; lme4 incorporates into single formula 

###################################################
### code chunk number 5: examine the output of the model
###################################################
anova(mod1.lme)

#in this case, 3 individuals at every pot at every level, so well-balanced design 
###################################################
### code chunk number 6: examine the summary of the mixed model
###################################################
summary(mod1.lme)
#Tau on intercept and sigma on residual -> square both for variance or else use varcorr

###################################################
### code chunk number 7: isolate the random effects
###################################################
VarCorr(mod1.lme)


###################################################
### code chunk number 8: isolate the fixed effects
###################################################
fixef(mod1.lme)


###################################################
### code chunk number 9: coef produces a different output!
###################################################
coef(mod1.lme)
#best linear unbiased predictors (blups) 
#can use gen least squares for each of your pots post-hoc 
###################################################
### code chunk number 10: estimated random effects
###################################################
ranef(mod1.lme)
#the Uoi's

###################################################
### code chunk number 11: conditional predictions of means (pot level)
###################################################
predict(mod1.lme)[1:10]
#visualize how expected vals change for diff predictor vars
#also called conditional means; conditional on block/random effect (pot) 
###################################################
### code chunk number 12: population level predictions (without random effects)
###################################################
predict(mod1.lme, level=0)[1:10]


###################################################
### code chunk number 13: plot data and predictions from fixed effects and mixed effect models
###################################################

theme_set(theme_bw())

plants$mix.ests <- predict(mod1.lme,level=1)
plants$pop.mean <- predict(mod1.lme,level=0)
plants$fix.ests <- predict(fixedmod)

ggplot(plants,aes(y=lw.rat,x=fpot))+geom_point(aes(color="Raw data"),size=1)+
    geom_point(aes(y=mix.ests,color="Conditional means"),shape="-",size=10)+
    geom_point(aes(y=fix.ests,color="Fixed estimates"),shape="-",size=10)+
    geom_line(aes(x=as.numeric(fpot),y=pop.mean,color="Population Mean"))+
    facet_wrap(~type,scales="free_y")+
    labs(y="Length:Width")+scale_x_discrete("Pot",breaks=NULL)+
    scale_color_manual("",values=c("red","blue","black","grey"))+
    guides(color=guide_legend(override.aes = list(linetype = 0)))


