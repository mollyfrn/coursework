### R code from vignette source '/home/james/work/teach/563/lectures/lecture20/lecture20.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: lecture20.Rnw:14-16
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=3,fig.width=4)


###################################################
### code chunk number 2: lecture20.Rnw:20-36
###################################################
# read in galapagos flora data
gala <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/galapagos.txt", header=T)
gala$logarea <- log(gala$Area)

out.norm1 <- lm(Species~Area, data=gala)
out.norm2 <- lm(Species~logarea, data=gala)
out.norm3 <- glm(Species~logarea,family=gaussian(link=log), data=gala)
out.pois <- glm(Species~logarea, data=gala, family=poisson)
library(gamlss)
out.NB1 <- gamlss(Species~log(Area), data=gala, family=NBII)
out.sqrtnorm <- lm(sqrt(Species)~logarea, data=gala)
out.lognorm <- lm(log(Species)~logarea, data=gala)
out.lognorm2 <- gamlss(Species~logarea,family=LOGNO,data=gala)
out.sqrtnorm <- lm(sqrt(Species)~logarea, data=gala)
library(MASS)
out.NB2 <- glm.nb(Species~logarea, data=gala)


###################################################
### code chunk number 3: lecture20.Rnw:41-49
###################################################
jensonplot <- ggplot(data.frame(x=seq(.8,3.5,.1)),aes(x=x))
jensonplot+stat_function(fun=exp)+
  annotate("segment",x=1,xend=3,y=exp(1),yend=exp(3))+
  annotate("point",2,exp(2),color=3)+
  annotate("text",2,exp(2),label="exp of mean",hjust=0,vjust=1,color=3)+
  annotate("point",2,(exp(1)+exp(3))/2,color=2)+
  annotate("point",2,(exp(1)+exp(3))/2,color=2)+
  annotate("text",2,(exp(1)+exp(3))/2,label="mean of exp",hjust=1.1,vjust=0,color=2)


###################################################
### code chunk number 4: lecture20.Rnw:60-77
###################################################
NB1func <- function(x) {exp(coef(out.gamlss2)[1]+coef(out.gamlss2)[2]*log(x))}
NB2func <- function(x) {exp(coef(out.NB2)[1]+coef(out.NB2)[2]*log(x))}
sigma2.lognorm <- sum(residuals(out.lognorm)^2)/length(residuals(out.lognorm))/2
lognormmedian <- function(x){exp(coef(out.lognorm)[1]+coef(out.lognorm)[2]*log(x))}
lognormmean <- function(x){exp(coef(out.lognorm)[1]+coef(out.lognorm)[2]*log(x)+
                                 sigma2.lognorm)}
sqrtmedian <- function(x){(coef(out.sqrtnorm)[1]+coef(out.sqrtnorm)[2]*log(x))^2}

fitplot <- ggplot(data.frame(area=seq(min(gala$Area),max(gala$Area),length.out=200)),
                  aes=area)
fitplot <- fitplot+geom_point(data=gala,aes(x=Area,y=Species))+scale_x_log10()
fitplot+stat_function(fun=NB1func,aes(color="NB1 Mean"))+
  stat_function(fun=NB2func,aes(color="NB2 Mean"))+
  stat_function(fun=lognormmedian,aes(color="lognormal median"))+
  stat_function(fun=lognormmean,aes(color="lognormal mean"))+
  stat_function(fun=sqrtmedian,aes(color="sqrt-normal median"))+
  labs(color="Fit",y="Species Richness")


###################################################
### code chunk number 5: lecture20.Rnw:82-83
###################################################
range(gala$Species)


###################################################
### code chunk number 6: lecture20.Rnw:87-89
###################################################
fitted(out.NB2)
gala$mu <- fitted(out.NB2)


###################################################
### code chunk number 7: lecture20.Rnw:94-97
###################################################
gala$z <- dnbinom(gala$Species, mu=fitted(out.NB2), size=out.NB2$theta)
min(gala$z)
max(gala$z)


###################################################
### code chunk number 8: lecture20.Rnw:101-104
###################################################
max(dnbinom(1000, mu=fitted(out.NB2), size=out.NB2$theta))
max(dnbinom(1500, mu=fitted(out.NB2), size=out.NB2$theta))
min(dnbinom(0, mu=fitted(out.NB2), size=out.NB2$theta))


###################################################
### code chunk number 9: lecture20.Rnw:111-113
###################################################
out.p<- sapply(1:29, function(x) dnbinom(0:1500, mu=fitted(out.NB2)[x], size=out.NB2$theta))
wideframe <- data.frame(out.p)


###################################################
### code chunk number 10: lecture20.Rnw:119-124
###################################################
names(wideframe) <- gala$Island
library(tidyr)
longframe <- gather(wideframe,key=Island,value=Probability)
head(longframe)
longframe$rich <- rep(0:1500,29)


###################################################
### code chunk number 11: lecture20.Rnw:128-129
###################################################
longframe <- longframe[longframe$Probability>1e-5,]


###################################################
### code chunk number 12: lecture20.Rnw:134-144
###################################################
library(ggplot2)
theme_set(theme_bw())
islandplot <- ggplot(longframe,aes(x=rich,y=Probability))
vline.data1 <- data.frame(rich=gala$mu,Island=as.character(gala$Island))
vline.data2 <- data.frame(rich=gala$Species,Island=as.character(gala$Island))
islandplot+geom_bar(stat="identity")+facet_wrap(~Island,scales="free")+
  geom_vline(aes(xintercept=rich,linetype="Predicted Richness"),
             data=vline.data1,color="red")+
  geom_vline(aes(xintercept=rich,linetype="Observed Richness"),
             data=vline.data2,color="blue")+labs(linetype="")


###################################################
### code chunk number 13: lecture20.Rnw:152-156
###################################################
upper.p <- 1-pnbinom(gala$Species-1, mu=gala$mu, size=out.NB2$theta)
upper.p
sum(upper.p<.025)
gala$Island[upper.p<.025]


###################################################
### code chunk number 14: lecture20.Rnw:163-167
###################################################
lower.p <- pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta)
lower.p
sum(lower.p<.025)
min(lower.p)


###################################################
### code chunk number 15: lecture20.Rnw:174-178
###################################################
pval.dat <- data.frame(pvalue=c(lower.p, upper.p), island=rep(gala$Island,2), label=rep(c('lower', 'upper'), each=nrow(gala)))
ggplot(pval.dat,aes(x=pvalue,y=island))+geom_point()+facet_wrap(~label)+
  geom_vline(xintercept=.025,color="red",linetype=2)+ 
  theme(panel.grid.minor.x=element_blank(),panel.grid.major.x=element_blank())


###################################################
### code chunk number 16: lecture20.Rnw:182-190
###################################################
gala$pplot <- ifelse(pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta) < .5, #is this p val less than a half? going to give one p val per obs
                     pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta), 
                     1-pnbinom(gala$Species-1, mu=gala$mu, size=out.NB2$theta))
gala$lower <- ifelse(pnbinom(gala$Species, mu=gala$mu, size=out.NB2$theta) < .5,
                     "lower","upper")
ggplot(gala,aes(x=pplot,y=Island,color=lower))+geom_point()+
  geom_vline(xintercept=.025,linetype=2)+
  labs(x="Tail probability of observation from model",color="Tail of distribution")
#which is the most extreme val for each one? instead of doubling plot, fits fairly well given dist of observations
#best we can do for negative binomial in goodness of fit, no single p val extracting function estimate is ideal 
#with poisson reg there are a couple of ways we can assess our goodness of fit 
#usually useful with count data to start with poisson regression 
#tho with most eco data get a p bad fit because of amt variance allowed to vary about the mean 

####Goodness of fit of Poisson with continuous predictors####
#can't test to see if one observation looks like a poisson or not if only one obs 

#residual deviance 
#can get for both poisson + binom models 
#going to have a SATURATED model. 
#one lambda fitted for each observation - Lamda~ 
#we KNOW this is overfitting - using data to explain the model 
#how good is model fit with fewer parms compared to way too many parms? 
#Max likelihood of model vs likelikhood of Theta~ (likelihood of our saturated model)
#define likelihood ratio of saturated model as (2*log likelihood of saturdated model)/(likelihood of Theta model)
#saturated is best we can do 
#this should be chi sqrd with n-p degrees of freedom, with p number parameters
#this is called residual deviance 

###################################################
### code chunk number 17: lecture20.Rnw:208-215
###################################################
# log-likelihood of current model
LL1 <- sum(log(dpois(gala$Species, lambda = fitted(out.pois)))) #sum of the log of poisson, with lamda of fitted
LL1 
# log-likelihood of saturated model
LL2 <- sum(log(dpois(gala$Species, lambda = gala$Species))) #lambda will be our observed richness, where mean = observation
LL2 #a lot higher
2*(LL2-LL1) #this is our residual deviance 
#is it big or small? relative to chi squared


###################################################
### code chunk number 18: lecture20.Rnw:219-221
###################################################
nrow(gala)-length(coef(out.pois)) #n is number of rows, p is length of coef vector, so 27 df 
1-pchisq(2*(LL2-LL1), 27) #machine 0
#saturated model is a better model than the model that we fit, which is bad because our sat model reps a way overfit model
#one way to do a goodness of fit test - look at res deviance and see what its dist is relative to chi sq

#Variance of poisson should be lambda, not greater 
#if greater than lambda, it's called overdispersion 
#calcing this - Phi is our Pearson deviance X^2/ our residual df 

#Phi = X^2/n-p > 1; overdispersion -> how much extra variance do we have?
#Quasipoisson/Quasilikelihood version -> fit our model using glm 
#find overdisp parm, and then we use that to scale our p-vals properly to make them more conservative 
#more var in our model than our data says 

#the other thing you can do 

###################################################
### code chunk number 19: lecture20.Rnw:255-256
###################################################
sum(residuals(out.pois, type='deviance')^2)


###################################################
### code chunk number 20: lecture20.Rnw:272-273
###################################################
2*sum(gala$Species * log(gala$Species/fitted(out.pois)))


###################################################
### code chunk number 21: lecture20.Rnw:278-282
###################################################
# to use the residual deviance as a goodness of fit statistic
# the expected counts need to be sufficiently large
fitted(out.pois)
sum(fitted(out.pois)<5)


###################################################
### code chunk number 22: lecture20.Rnw:291-292
###################################################
sum((gala$Species-fitted(out.pois))^2/fitted(out.pois))


###################################################
### code chunk number 23: lecture20.Rnw:303-304
###################################################
sum(residuals(out.pois, type='pearson')^2)


###################################################
### code chunk number 24: lecture20.Rnw:313-314
###################################################
sum(residuals(out.pois, type='pearson')^2)/out.pois$df.residual
#it's 23 times more dispersed!!! Dang. 

#also use AIC to choose amongst diff noise vals -> one breaker val that can dissolve extra variation

###################################################
### code chunk number 25: lecture20.Rnw:319-326
###################################################
# although the residual deviance is reported for an NB model
# it is not a goodness of fit statistic DO NOT USE
library(MASS)
out.NB2 <- glm.nb(Species~log(Area), data=gala)
summary(out.NB2)
# invalid test
1-pchisq(deviance(out.NB2),summary(out.NB2)$df.residual) #oh it's .25 and it passes! <- NO. better to compare AIC's or do ggplotting as on right for neg binom

