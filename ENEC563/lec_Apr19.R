### R code from vignette source '/home/james/work/teach/563/lectures/lecture25/lecture25.Rnw'

###################################################
### code chunk number 1: lecture25.Rnw:15-17
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=3,fig.width=4)


###################################################
### code chunk number 2: lecture25.Rnw:22-29
###################################################
binframe <- data.frame(success=rep(0:10,3),p=rep(c(.1,.5,.9),each=11))
binframe$probability <- dbinom(binframe$success,10,binframe$p)
library(ggplot2)
theme_set(theme_bw())
binplot <- ggplot(binframe,aes(x=success,y=probability))
binplot+geom_bar(stat="identity")+facet_wrap(~p,labeller="label_both")+xlab("Number of successes")



###################################################
### code chunk number 3: lecture25.Rnw:34-38
###################################################
binframe <- data.frame(success=c(30:70,60:100,60:100),p=rep(c(.5,.8,.9),each=41))
binframe$probability <- dbinom(binframe$success,100,binframe$p)
binplot <- ggplot(binframe,aes(x=success,y=probability))
binplot+geom_bar(stat="identity")+facet_wrap(~p,labeller="label_both",scale="free_x")+xlab("# of successes")


###################################################
### code chunk number 4: lecture25.Rnw:53-55
###################################################
wells <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/wells.txt",header=T)
wells


###################################################
### code chunk number 5: lecture25.Rnw:62-64
###################################################
wellplot <- ggplot(wells,aes(x=landuse, y=y/n,color=sewer))
wellplot+geom_point()                 


###################################################
### code chunk number 6: lecture25.Rnw:72-75
###################################################
# logistic regression model
wellsglm <- glm(cbind(y, n-y)~ landuse + sewer, data=wells, family=binomial)
summary(wellsglm)
#inverse logit transform intercept 
#could analyze as binary version of data 
#coefs neither additive nor interactive; represent a change in the odds ratio -> multiplicative change in the odds


###################################################
### code chunk number 7: lecture25.Rnw:83-88
###################################################
# create binary version of the file for comparison
# function to create binary response
yn.func <- function(x) rep(c(1,0),c(x[1],x[2]-x[1]))
# sample calculation
apply(wells[1:4,1:2], 1, yn.func)
#change set of binomial data into binary data #4 successes, 16 fails 

###################################################
### code chunk number 8: lecture25.Rnw:92-96
###################################################
# assemble data
wells.raw <- data.frame(yn=unlist(apply(wells[,1:2], 1, yn.func)), landuse=rep(wells$landuse,wells$n), sewer=rep(wells$sewer,wells$n))
dim(wells.raw)
wells.raw[1:8,]


###################################################
### code chunk number 9: lecture25.Rnw:100-103
###################################################
# fit same model this time with a binary response
wellsglmbern <- glm(yn~ landuse + sewer, data=wells.raw, family=binomial)
summary(wellsglmbern)


###################################################
### code chunk number 10: lecture25.Rnw:107-108
###################################################
rbind(coef(wellsglm),coef(wellsglmbern))


###################################################
### code chunk number 11: lecture25.Rnw:113-114
###################################################
rbind(c(AIC(wellsglm),logLik(wellsglm),deviance(wellsglm)),c(AIC(wellsglmbern),logLik(wellsglmbern),deviance(wellsglmbern)))


###################################################
### code chunk number 12: lecture25.Rnw:119-122
###################################################
# refit model to show individual logit means for land use
wellsglma <- glm(cbind(y, n-y)~ landuse + sewer-1, data=wells, family=binomial)
summary(wellsglma)


###################################################
### code chunk number 13: lecture25.Rnw:128-130
###################################################
# models are the same, just different parameterizations
AIC(wellsglm,wellsglma)


###################################################
### code chunk number 14: lecture25.Rnw:135-137
###################################################
wellsglm2 <- glm(cbind(y, n-y)~ landuse*sewer, data=wells, family=binomial)
anova(wellsglm,wellsglm2,test="Chisq")


###################################################
### code chunk number 15: lecture25.Rnw:145-150
###################################################
# predicted logits of individual observations
predict(wellsglm)
# predicted probabilities of individual observations
predict(wellsglm, type="response")
fitted(wellsglm)


###################################################
### code chunk number 16: lecture25.Rnw:154-157
###################################################
newdata <- expand.grid(landuse=levels(wells$landuse),sewer=levels(wells$sewer))
newdata$p <- predict(wellsglm,newdata=newdata,type="response")
newdata


###################################################
### code chunk number 17: lecture25.Rnw:161-170
###################################################
# display confidence intervals
linkpred <- predict(wellsglm,newdata=newdata,se.fit=T)
linkpred
newdata$logit <- linkpred$fit
newdata$linkupper <- linkpred$fit+1.96*linkpred$se.fit
newdata$linklower <- linkpred$fit-1.96*linkpred$se.fit
landplotlink <- ggplot(newdata,aes(x=landuse,y=logit,ymin=linklower,ymax=linkupper,color=sewer))
landplotlink <- landplotlink+geom_errorbar(position=position_dodge(.25),width=.2)+geom_point(pos=position_dodge(.25))
landplotlink


###################################################
### code chunk number 18: lecture25.Rnw:178-180
###################################################
wellsglm$family$linkinv
invlogit <- wellsglm$family$linkinv


###################################################
### code chunk number 19: lecture25.Rnw:185-190
###################################################
newdata$lower <- invlogit(newdata$linklower)
newdata$upper <- invlogit(newdata$linkupper)
landplot <- ggplot(newdata,aes(x=landuse,y=p,ymin=lower,ymax=upper,color=sewer))
landplot <- landplot+geom_errorbar(position=position_dodge(.25),width=.2)+geom_point(pos=position_dodge(.25))
landplot


###################################################
### code chunk number 20: lecture25.Rnw:197-199
###################################################
library(gridExtra)
grid.arrange(landplotlink,landplot)


###################################################
### code chunk number 21: lecture25.Rnw:203-204
###################################################
grid.arrange(landplotlink,landplot,nrow=1)


###################################################
### code chunk number 22: lecture25.Rnw:210-213
###################################################
# obtain mean of each binomial distribution
fitted(wellsglm)*wells$n
wells$y


###################################################
### code chunk number 23: lecture25.Rnw:218-219
###################################################
Ei <- cbind(fitted(wellsglm)*wells$n, wells$n-fitted(wellsglm)*wells$n)


###################################################
### code chunk number 24: lecture25.Rnw:224-226
###################################################
# too many predicted counts are small for chi-squared distribution to hold
sum(Ei<=5)/length(Ei)


###################################################
### code chunk number 25: lecture25.Rnw:234-236
###################################################
Oi <- cbind(wells$y, wells$n-wells$y)
sum((Oi-Ei)^2/Ei)


###################################################
### code chunk number 26: lecture25.Rnw:241-243
###################################################
# p-value for test
1-pchisq(sum((Oi-Ei)^2/Ei),nrow(wells)-length(coef(wellsglm)))


###################################################
### code chunk number 27: lecture25.Rnw:250-257
###################################################
# residual deviance can be used to check fit too
# (except probably inappropriate here)
summary(wellsglm)
# p-value for residual deviance
df.residual(wellsglm)
deviance(wellsglm)
1-pchisq(deviance(wellsglm),df.residual(wellsglm))


###################################################
### code chunk number 28: lecture25.Rnw:275-279
###################################################
res.dev <- sum(ifelse(Oi==0, 0, 2*Oi*log(Oi/Ei)))
res.dev
deviance(wellsglm)
1-pchisq(res.dev,wellsglm$df.residual)


###################################################
### code chunk number 29: lecture25.Rnw:287-305
###################################################
sim.func <- function(type="deviance") {
  obs <- sapply(1:nrow(wells), function(x) rbinom(1, prob=fitted(wellsglm)[x], size=wells$n[x]))
  my.glm <- glm(cbind(obs,n-obs)~landuse + sewer, data=wells, family=binomial)
  if(type=="pearson"){
    ei <- fitted(my.glm)*wells$n
    Ei <- cbind(ei, wells$n-ei)
    Oi <- cbind(obs, wells$n-obs)
    # pearson test
    pearson <- sum((Oi-Ei)^2/Ei)
    pearson
  }
  else{
    # residual deviance (same as G-test)
    dev <- deviance(my.glm)
    # return residual deviance
    dev
  }
}


###################################################
### code chunk number 30: lecture25.Rnw:309-315
###################################################
# obtain 999 residual deviances from model
sims <- replicate(999, sim.func())
# append actual residual deviance to make 1000
sims <- c(sims, deviance(wellsglm))
# proportion of simulated values that exceed actual value
sum(sims>=deviance(wellsglm))/length(sims) #what proportion of devs are greater than the deviances I do have 
#divided by # of devs
#reasonably modeled as binomial data 
#no diff bet dist as binomial compared to binary (?)

###################################################
### code chunk number 31: lecture25.Rnw:319-326
###################################################
# obtain 999 residual deviances from model
sims <- replicate(999, sim.func(type="pearson"))
# append actual residual deviance to make 1000
sims <- c(sims, sum((Oi-Ei)^2/Ei))
# proportion of simulated values that exceed actual value
sum(sims>=sum((Oi-Ei)^2/Ei))/length(sims)


