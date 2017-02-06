#q t = fatter, longer weird version of normal dist, more probability is in tails, qt has df option, t dist is area under curve in tail   
#q normal, normal bell curve, less probability in tails, no df parameter, z dist is area under curve in tail 

### R code from vignette source '/home/james/work/teach/563/lectures/lecture6/lecture6.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: "code for processing do not run"
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=4,fig.width=6)


###################################################
### code chunk number 2: lecture6.Rnw:38-42
###################################################
plants <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/jimsonweed.txt", header=T)
plants[1:12,]
plants$fpot <- factor(plants$pot)

#globe vs nominal 
#making pot a factor, sep effect for each pot 
###################################################
### code chunk number 3: lecture6.Rnw:46-51
###################################################

# display layout of experiment
library(ggplot2)
theme_set(theme_bw())
ggplot(plants,aes(x=lw.rat,y=fpot,color=type))+geom_point()                 


###################################################
### code chunk number 4: lecture6.Rnw:59-62
###################################################
# fit randomized block design
fullmod <- lm(lw.rat~fpot*type, data=plants)
anova(fullmod)

#pot and interaction term, type I anova because data is in fact balanced 
###################################################
### code chunk number 5: lecture6.Rnw:67-69
###################################################
addmod <- lm(lw.rat~type+fpot,data=plants)
anova(addmod)

#how much residual variation is there? compare to mean squared area 
#compare to random design 
#reduce amt of mean sq error by adding pot in as variable so
###################################################
### code chunk number 6: lecture6.Rnw:72-75
###################################################
# Model ignoring blocks: compare MSE to blocks model
mod0 <- lm(lw.rat~type, data=plants)
summary(mod0)

#length to width ratio with effect of plant type 
#such a simple model, don't need to do anova 
#assuming identically, independently distributed 
#iie 
#competition within a plot; expect correlations within a pot, so can't say "independent" perfectly 
#so we can model the effect of the pot, basically calibration tube analogous 

###################################################
### code chunk number 7: lecture6.Rnw:82-83
###################################################
summary(addmod)
anova(mod0)
str(anova(mod0))
(anova(mod0)$"Mean Sq"[2]-anova(addmod)$"Mean Sq"[3])/anova(mod0)$"Mean Sq"[2]
#about 25% 


#the simplest sufficient explanation is the best -> we prefer parsimony 
#block effect -> represents covariation amongst replicates (ie pot, blocking by pot variable) 
#due to unavoidable experimental variation 
#came out of ag literature from testing in ag fields due to variation in soil types within an ag field 
#porosity can vary; a lot of conditions can vary spatially, diff combos of those conditions can vary 
#result may depend on how things are put together 
#blocking vs random sample site selection 
#blocking -> putting things close together so that experience similar env conditions within a block 
#can be used for any type of variation nuisance that needs to be controlled for; not just spatial 
#can do genetic blocking! orgs related to each other may responds similarly to exp treatments, so 
#family block is important 
#temporal blocking to cover weekly/seasonal variation 
#don't really care about their specific numbers so can set as factor/categories; 
#if we kept raw numbers would add too many variables to our models and decrease parsimony 


###################################################
### code chunk number 8: lecture6.Rnw:89-90
###################################################
predict(addmod, newdata=data.frame(type=c('G','N')))
#doesn't work unless all predictor variables present in model need to be present in new data frame created

###################################################
### code chunk number 9: lecture6.Rnw:95-101
###################################################
# generate all unique combinations of pot and type
# type comes first to match the order of the data in data set
newdata <- expand.grid(type=levels(plants$type), fpot=levels(plants$fpot)) #32 combos, can predict means for levels
out.p <- predict(addmod, newdata=newdata)
modelpred <- cbind(newdata, out.p)
modelpred


###################################################
### code chunk number 10: lecture6.Rnw:106-109
###################################################
predict(mod0, newdata=data.frame(type=c('G','N'))) #with no pot, can predict with G and N 
tapply(modelpred$out.p,modelpred$type,mean) #if I wanted to do for updated model now, 
#could say "for each combo, avg across all pots what pred was for globe and nominal 
#t is short for table apply to out.p (pred values) and the table you want to apply over 
#(will apply for each level of a factor variable if specify column for single parameter), 
#then apply mean function over all

#mean of preds over G's and mean of preds over N's 
#end up with same pred avgs as before


###################################################
### code chunk number 11: lecture6.Rnw:114-120
###################################################
modelpred$groupmeans <- tapply(modelpred$out.p,modelpred$type,mean)  #if you assign a short vector to a long vector, will repeat it over and over (instead of giving you df row lengths error)
ggplot(plants,aes(x=lw.rat,y=fpot,color=type))+geom_point()+
  geom_point(data=modelpred,aes(x=out.p,y=as.factor(fpot)),shape="|",size=5)+
  geom_vline(data=modelpred,aes(xintercept=groupmeans,color=type),linetype=2)+
  labs(x="Length to width ratio",y="Pot number")+
  scale_color_discrete("Plant type",labels=c("Globe","Nominal"))
#dashed lines are the means of the preds calc'd above 
#dash/pipe shaped preds

#this is a fixed effect mod; not a random effect 
#effect for each pot, effective type is same for each pot 
#predicted value for each pot by treatment 
#dists parallel because additive mod; intractive mod would vary more 
#some pots have lower or greater than avg ratios of l:w 


#for each pot will have a beta and a zi; each pot will be cycled thru to see if it matches a subsequent pot 
#Zi reps whether plant i is in pot i 

#in mixed models we combine: 
#fixed effects (where for each variable we have a measured and defined effect we can account for) 
#with random effects -> more accurately parallels variation in the mean where the exact level isn't explicitly modeled 
#introduces natural stochasticity into models to help them more closely parallel reality 

#going to change fixed effect for pot into a random effect 

#random = U_0i -> random effect of pot -> is a normal with mean 0 and variance Tau^2 
#within each pot there's a random mean, and we don't know what that is 

#there's var in each plant in each pot, and each pot is also going to have random var 

#can partition that variation and error into fixed (mean of 0, var of sigma) and random (mean of 0 and var of Tau)

#two diff packages that do mixed models: 
#nlme - old, trusted, but less capable 
#lme4 - newer, rapidly changing, sometimes gives funny results, but more flexible 

library(nlme)
#workhorse for linear mixed effect models is just lme function 
?lme 

#have to sep model for fixed effects from random effects 
#fixed is formula, then data, then random = random effects 
#or can throw into right side of formula 

###################################################
### code chunk number 12: lecture6.Rnw:162-165
###################################################
# treat blocks as random: using nlme package
library(nlme)
mod2.lme <- lme(fixed=lw.rat~type, random=~1|fpot, data=plants)
#fixed effect for plant type 
#random equals tilda (just like with update mods) 
#1 is the intercept 
#adding this random variable to the intercept to shift intercept bc this is how it shifts var associated 
#intercept varies across pots; diff mean for each pot, random error according to pot 
#becomes very difficult to add more than one random effect in nlme that are non-nested;
#which is where lme4 comes in handy 


###################################################
### code chunk number 13: lecture6.Rnw:170-171
###################################################
anova(mod2.lme)


###################################################
### code chunk number 14: lecture6.Rnw:175-176
###################################################
summary(mod2.lme)
#in summary we can see our est for our intercept, the effect, the SE estimates, and p values 
#we can also see StdDev and Intercept = Tau; Residual = sigma 
#if squared would get sigma and tau squared
#if intercept higher than residual then indicative original model would be a source of huge error

###################################################
### code chunk number 15: lecture6.Rnw:184-185
###################################################
fixef(mod2.lme)

#if we want to look at coefs have to use fixef instead of coefs


###################################################
### code chunk number 16: lecture6.Rnw:190-191
###################################################
coef(mod2.lme)
#bc diff vals for each pot for each intercept 
#these mods don't est what the random effect is, just what Tau^2 is 


#there's a way to est B L U P 's (lol)
#Best Linear Unbiased Predictors 

#ggiven tau^2 can est what the "effective" random effect of the model are, but have to be done post hoc 

###################################################
### code chunk number 17: lecture6.Rnw:197-198
###################################################
ranef(mod2.lme) #what are the random effects? 

#gives us the BLUPs for how each pot varies from the mean under this model
###################################################
### code chunk number 18: lecture6.Rnw:203-204
###################################################
predict(mod2.lme)[1:10]

#can also use predict on these; within each type have the same predictor values but vary across pots
#so block level predictions, vary to pot 


###################################################
### code chunk number 19: lecture6.Rnw:209-210
###################################################
predict(mod2.lme, level=0)[1:10]

#no variation between types and pots at level = 0
#default is level 1; equiv to per pot preds; pop level preds (level 2) are long dashed lines across all pots in plot
###################################################
### code chunk number 20: lecture6.Rnw:218-220
###################################################
# treat blocks as random: using lme4 package
library(lme4)


###################################################
### code chunk number 21: lecture6.Rnw:225-227
###################################################
# to avoid function conflicts unload nlme from memory
detach(package:nlme)


###################################################
### code chunk number 22: lecture6.Rnw:232-233
###################################################
mod2.lmer <- lmer(lw.rat~type+(1|fpot), data=plants)

#same as above; just diff function 
#entire formula as argument; random effects added by putting in parenthetical notation 
#can have as many effects as you want, nested or not 
#can be numerically costly; can also sacrifice parsimony 

###################################################
### code chunk number 23: lecture6.Rnw:237-240
###################################################
# lmer does not return p-values
anova(mod2.lmer) #gives us sum of sqrs tho for anova 
summary(mod2.lmer)

#amt of residual error depends on random effects; need to use Tau^2 to estimate F stat 
#so it isn't an observed in this case, it is a rough estimate 


#std dev at random effects again corresponds to sigma and Tau's of data 
#but also includes our fixed effects 


#if we want to we can use lmer test to get p values 

#can also construct CI's based on these std errors, and use those to test effects of variables like plant type 
#can see if leads to overlapping or non-overlapping CI with 0 

#later will cover likelihood ratio test 
#can use Bayesian techniques 
#and finally can do a parametic bootstrap (since no p value given)


#we look at the real data, get some sort of test value like an F value 
#then based on null hypothesis, we simulate data, generate another test value (like an F value), 
#and then repeat 
#so use F values to gen data to gen F values 

#then we compare real test (F) value to simulated test values 
#to see how likely or unlikely the real value was and get an equivalent of how significant it was 
#(basically like a p value proxy)

###################################################
### code chunk number 24: lecture6.Rnw:259-263
###################################################
a <- c(3,2.5,3.4)
b <- c(3.1,3.3,3.4,3.05)
actual <- mean(a)-mean(b) #test stat is just the difference between the two 
actual

#is this difference unusually high? 
###################################################
### code chunk number 25: lecture6.Rnw:268-278
###################################################
meanrep <- function(popvector,n1,n2){ #takes a vector and two sample sizes
  mean(sample(popvector,n1))-mean(sample(popvector,n2))
}
#samples with replacement from first vector n times (based on sample sizes)
#rerunning to parallel diff between means from above eq cited in "actual"
set.seed(15)
sampledist <- c(actual,replicate(9999,meanrep(c(a,b),3,4))) # number times I want to replicate, and the function I want to rep
sampledist[1:10] #real is 1st val, and then all simulated values with dist between two 

ggplot(data.frame(sampledist),aes(x=sampledist))+geom_density()+
  annotate("point",y=0,x=c(actual,-actual),color="red",size=3)
#actual diff plotted as red points
#a fair bit of area under curve at both tails

###################################################
### code chunk number 26: lecture6.Rnw:285-286
###################################################
sum(sampledist<=actual|sampledist >= -actual)/10000

#how many in sample dist are less than actual value (bc a neg value) and the # that are greater than the - actual 



#next time: parametric isn't random assortment; assume null (no variation) model is correct, and calc f stats
#bootstrap f test
###################################################
### code chunk number 27: lecture6.Rnw:296-299
###################################################
# The F-statistic is in the ANOVA table
Factual<-anova(mod2.lmer)[,4]
Factual


###################################################
### code chunk number 28: lecture6.Rnw:303-306
###################################################
# use a parametric bootstrap to obtain a p-value
# fit a model to the data without type as a predictor
mod1.lmer <- lmer(lw.rat~(1|pot), data=plants)


###################################################
### code chunk number 29: lecture6.Rnw:310-321
###################################################
parbootf <- function(){
  # simulate data from model in which type has no effect
  rmath <- unlist(simulate(mod1.lmer))
  # estimate type model to these data
  rmod <- lmer(rmath~(1|pot)+type, data=plants)
  # extract statistic
  fstat <- anova(rmod)[1,4]
  fstat
}

Fstatdist <- replicate(9999,parbootf())


###################################################
### code chunk number 30: lecture6.Rnw:325-330
###################################################
max(Fstatdist)
Fstatdist <- c(Factual,c(Fstatdist))
# null distribution of F-statistic
ggplot(data.frame(Fstatdist),aes(x=Fstatdist))+geom_density()+
  annotate("point",y=0,x=Factual,color="red",size=3)


###################################################
### code chunk number 31: lecture6.Rnw:334-336
###################################################
# p-value of actual F-statistic
sum(Factual<=Fstatdist)/1000

