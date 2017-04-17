#using a random INTERCEPT 
#hierarchicial models -> what Bayesians call mixed effect mods 

#fixed effects - intercept as a predictor 
#random effect reps dev of that patch from the population mean 
  #distributed normally (assumption)

#fold the random intercept into the intercept itself 
#Beta0i -> random intercept for each patch i 
#don't need to add a random effect bc have a random intercept 
#mean B0 with variance Tau^2 
#Sij, still Poisson variance Mu ij 

#alt hyp is to talk about this in the context of a hierarchical model 
#cleanly seps out random effects nicely 
#patch level, island w/in patch level, and then island within every year 

#easier to understand where the variance is coming from 

### R code from vignette source '/home/james/work/teach/563/lectures/lecture24/lecture24.Rnw'

###################################################
### code chunk number 1: lecture24.Rnw:15-17
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=3,fig.width=4)


###################################################
### code chunk number 2: lecture24.Rnw:26-35
###################################################
library(rstanarm)
options(mc.cores = parallel::detectCores())

birds <- read.table('https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/birds.csv', header=T, sep=',')
birds <- birds[!is.na(birds$S),]
birds <- droplevels(birds)
model1.glm <- glm(S~factor(year), data=birds, family=poisson)
mixingarm <- stan_glm(S~factor(year), data=birds, family=poisson)
sepinterarm <- stan_glm(S~factor(year)+factor(patch), data=birds, family=poisson)


###################################################
### code chunk number 3: lecture24.Rnw:39-44
###################################################
#separate intercepts model
model2.glm <- glm(S~factor(patch)+factor(year), data=birds, family=poisson)
#random intercepts model
library(lme4)
model3.glmer <- glmer(S~factor(year) + (1|patch), data=birds, family=poisson) #random intercept varies by level of patch

###################################################
### code chunk number 4: lecture24.Rnw:60-61
###################################################
ranef(model3.glmer)$patch[1:10,1] #posthoc ests of the Uoi's 
#pretty good dist with mean of 0, can check out var of intercept, 
#our Tau^2 is rep of variation we see of the log of the base spp richness according to patch  
#random effects -> added to mean intercept = random intercepts 
###################################################
### code chunk number 5: lecture24.Rnw:65-66
###################################################
coef(model3.glmer)$patch[1:10,1]
#our random intercept for patch related coefs varying about mean of 3.19 
#random intercepts

###################################################
### code chunk number 6: lecture24.Rnw:71-72
###################################################
fixef(model3.glmer)[1] + ranef(model3.glmer)[[1]][1:10,1]
fixef(model3.glmer)[1] #mean intercept 

###################################################
### code chunk number 7: lecture24.Rnw:130-138
###################################################
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

dataList=with(birds,list(patch=as.numeric(patch),patchnum=length(unique(patch)),
                         N=length(S),S=S,year2006=as.numeric(year==2006),
                         year2007=as.numeric(year==2007))) #conditional mean just intercepts plus ran effects
birdsrandinter<- stan(file="randinterml.stan", data=dataList,iter=2000, chains=4)
#check to see if chains converged 
#what parameters equal -> all 4 chains overlap but vary randomly, not getting stuck 


###################################################
### code chunk number 8: lecture24.Rnw:142-143
###################################################
stan_trace(birdsrandinter)+ theme(text=element_text(family="Times"))
#2006 2007, and then the chains for each of our random effects 
#estimates ci for everything u throw at it 

#rhat vals for correlation/autocorrelation between diff chains 
#if we look at summary 

###################################################
### code chunk number 9: lecture24.Rnw:148-150
###################################################
max(summary(birdsrandinter)$summary[,"Rhat"] ) #values less than 1.1 are usually good, but easiest way to go is look at the max to see if converged
#well within range of having converged, really a func of both n_eff and actual # of iterations 

min(summary(birdsrandinter)$summary[,"n_eff"] ) #how many times have you effectively sampled? want to be greater than .25 of the actual samples
#much greater than 1/4 so we can be happy 


###################################################
### code chunk number 10: lecture24.Rnw:154-156
###################################################
library(rstanarm)
birdsrandinterarm <- stan_glmer(S~factor(year)+(1|patch),family=poisson,data=birds)
#compare with rstan traditional methods

###################################################
### code chunk number 11: lecture24.Rnw:160-163
###################################################
summary(birdsrandinter,pars=c("b0","beta","tau"))$summary[,"mean"] #using mean from Bayes method to compare with Likelihood Bayes method
#intercept, betas, std dev and var 
c(fixef(birdsrandinterarm),attr(VarCorr(birdsrandinterarm)$patch, "stddev"))
c(fixef(model3.glmer), attr(VarCorr(model3.glmer)$patch, "stddev")) 
#VarCorr -> extracting variance components of ran effect mods
#SUPER similar between Bayes and Likelihood Bayes, wow
#attr to get attribute out, specify what kind in "" ie "stddev" 
#as interchangeable as possible: rstan and rstanarm 
#pretty darn near concordinance amongst results
#variance components at end biased to be low with low sample size 

###################################################
### code chunk number 12: lecture24.Rnw:169-172
###################################################
summary(birdsrandinter,pars=c("beta0j"))$summary[1:10,"mean"]
coef(birdsrandinterarm)$patch[1:10,1]
coef(model3.glmer)$patch[1:10,1]
#estimates of random effects ^^^
#held in B0j vector; just the mean summary 
#same thing for the rstanarm and the glmer objects
#p similar, p close together
#var estimated for glmer a little low-biased generally to stay closer to the mean 
#using uninformative priors, Bayesian ests, using Bayesian methods are pretty similar 
#always produce an answer 

#UP next: level 2 predictors and Bayesian info criteria 

#level 2 preds remain constant within an observational hierarchy of your data 
#"whole plot" level predictors analagous in ag irrigation as whole plot with subplot splits 
#helpful to think back to that lesson 

#use rstan to det level 2 pred for each patch 
#add level 2 preds right into the mod 

###################################################
### code chunk number 13: lecture24.Rnw:182-183
###################################################
model4.glmer <- glmer(S~factor(year) + log(area) + landscape + (1|patch), data=birds, family=poisson)

#but now we want to create a var that has a box site (bauxite?) var for every patch 
#depending on whether each patch was a box site or not 

###################################################
### code chunk number 14: lecture24.Rnw:190-192
###################################################
patchlogarea <- tapply(log(birds$area), birds$patch, function(x) x[1])
class(patchlogarea)
#tapply is to apply a function to every level of a factor variable on a response variable 
#for every level of patch, apply a function to the log of the area, return just first number of each vector 
#log of area for 1 obs within a patch 
#array that looks like a vector, sometimes functions expect a vector so we just add that just in case

###################################################
### code chunk number 15: lecture24.Rnw:199-201
###################################################
patchland <- as.vector(tapply(birds$landscape, birds$patch, function(x) x[1]))
patchland


###################################################
### code chunk number 16: lecture24.Rnw:206-207
###################################################
levels(birds$landscape)
#automatically made it into a numeric value which is great bc stan only takes numeric vecs 
#we just need to remember what the values represent 

###################################################
### code chunk number 17: lecture24.Rnw:270-276
###################################################
dataList=with(birds,list(patch=as.numeric(patch),patchnum=length(unique(patch)),
                         N=length(S),S=S,year2006=as.numeric(year==2006),
                         year2007=as.numeric(year==2007),landscapebaux=as.numeric(patchland==2),
                         landscapefor=as.numeric(patchland==3),landscapeurb=as.numeric(patchland==4),
                         logarea=patchlogarea))
birdsrandintermod4 <- stan(file="randintermod4ml.stan", data=dataList,iter=2000, chains=4)


###################################################
### code chunk number 18: lecture24.Rnw:280-283
###################################################
stan_trace(birdsrandintermod4)+ theme(text=element_text(family="Times"))
max(summary(birdsrandintermod4)$summary[,"Rhat"] )
min(summary(birdsrandintermod4)$summary[,"n_eff"] )


###################################################
### code chunk number 19: lecture24.Rnw:287-288
###################################################
birdsrandintermod4arm <- stan_glmer(S~factor(year) + log(area) + landscape + (1|patch), data=birds, family=poisson)


###################################################
### code chunk number 20: lecture24.Rnw:292-295
###################################################
summary(birdsrandintermod4,pars=c("b0","beta","tau"))$summary[,"mean"]
c(fixef(birdsrandintermod4arm),attr(VarCorr(birdsrandintermod4arm)$patch, "stddev"))
c(fixef(model4.glmer),attr(VarCorr(model4.glmer)$patch,'stddev'))


###################################################
### code chunk number 21: lecture24.Rnw:378-385
###################################################
dataList=with(birds,list(patch=as.numeric(patch),patchnum=length(unique(patch)),N=length(S),
                         S=S,year2006=as.numeric(year==2006),year2007=as.numeric(year==2007),
                         landscapebaux=as.numeric(patchland==2),
                         landscapefor=as.numeric(patchland==3),
                         landscapeurb=as.numeric(patchland==4),logarea=patchlogarea))
birdsrandintermod4mean <- stan(file="randintermod4mlmean.stan", data=dataList,iter=2000, chains=4)



###################################################
### code chunk number 22: lecture24.Rnw:390-391
###################################################
summary(birdsrandintermod4mean,pars=c("muagr","mubaux","mufor","muurb"))$summary
#some overlap, bauxite and urban close in credible intervals and forest and ag overlapping in intervals
#we call these HBD intervals 
#will take 2.5% off each end -> regular 
#HBD will just continue down reducing until it hits 95% 
#can use :: to access function from a library without loading library itself

###################################################
### code chunk number 23: lecture24.Rnw:396-398
###################################################
coda::HPDinterval(coda::as.mcmc(as.data.frame(
  birdsrandintermod4mean)[,c("muagr","mubaux","mufor","muurb")]))
#largely quite close -> if large diffs between them, check out the posterior more closely
#calc functions AFTER the fact for rstanarm version
###################################################
### code chunk number 24: lecture24.Rnw:407-408
###################################################
names(extract(birdsrandintermod4arm$stanfit)) #doesn't work, need to fix code 
#extract to extract chains out of a model, want to use stanfit object fit 

# "I can embarrass my son trying to beatbox, but not you guys"

###################################################
### code chunk number 25: lecture24.Rnw:414-418
###################################################
exbeta0 <- as.vector(extract(birdsrandintermod4arm$stanfit,pars="alpha",permuted=FALSE))
exbetabaux <- as.vector(extract(birdsrandintermod4arm$stanfit,pars="beta",permuted=FALSE)[,,3])
exbetafor <- as.vector(extract(birdsrandintermod4arm$stanfit,pars="beta",permuted=FALSE)[,,4])
exbetaurb <- as.vector(extract(birdsrandintermod4arm$stanfit,pars="beta",permuted=FALSE)[,,5])


###################################################
### code chunk number 26: lecture24.Rnw:423-425
###################################################
mupost <- data.frame(muagr=exp(exbeta0),mubaux=exp(exbeta0+exbetabaux),
                     mufor=exp(exbeta0+exbetafor),muurb=exp(exbeta0+exbetaurb))


###################################################
### code chunk number 27: lecture24.Rnw:430-431
###################################################
apply(mupost,2,function(x) c(mean(x),quantile(x, c(.025, .975)))) #mean and 2 quantiles; 95% credible interval for 2 vars


###################################################
### code chunk number 28: lecture24.Rnw:435-436
###################################################
coda::HPDinterval(coda::as.mcmc(mupost))


###################################################
### code chunk number 29: lecture24.Rnw:513-514
###################################################
birdsrandintermod4loo <- stan(file="randintermod4mlloo.stan", data=dataList,iter=2000, chains=4)
#fits model with one missing piece of data and preds data and does that for every piece of data 

#k-fold cross validation 
#probability of Yi given parms theta given probability of theta from your missing data 
#favored over DIC (Freq approach to Bayesian model selection)
#while loo integrates over all of the posterior values 
#can be super computationally expensive but there are some easy ways to numerically estimate this 
#leave out 1 methodology 
#k fold cross validation 
#k randomly split up groups 
#instead of one piece of data, drops out 1/10 of the data, goes thru and does 10 diff preds and calcs integral 
#still intensive, but less so than original 

#need to have explicit calc of loglik from our model for each obs 
###################################################
### code chunk number 30: lecture24.Rnw:519-521
###################################################
library(loo)
loo(extract_log_lik(birdsrandintermod4loo))
#taking log probability as a mass function 
# Leave Out One (loo)

###################################################
### code chunk number 31: lecture24.Rnw:528-532
###################################################
loo1 <- loo(mixingarm)
loo2 <- loo(sepinterarm)
loo3 <- loo(birdsrandinterarm)
loo4 <- loo(birdsrandintermod4arm) #still not found 


###################################################
### code chunk number 32: lecture24.Rnw:537-539
###################################################
loo2 <-  kfold(sepinterarm) #fixing, 14 obs were in bad zone so switching to kfold 
loo3 <- loo(birdsrandinterarm,k_threshold=.7)


###################################################
### code chunk number 33: lecture24.Rnw:542-544
###################################################
compare(loo1,loo2,loo3,loo4) #don't cross compare loo IC's to AIC's, gotta be on own - but generally come to same conclusion!
AIC(model1.glm,model2.glm,model3.glmer,model4.glmer)

