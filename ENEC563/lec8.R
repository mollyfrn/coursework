#Lecture 8 

### R code from vignette source '/home/james/work/teach/563/lectures/lecture8/lecture8.Rnw'

###################################################
### code chunk number 1: lecture8.Rnw:19-21
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=4,fig.width=6)


###################################################
### code chunk number 2: lecture8.Rnw:28-34
###################################################
nitro <- read.csv("nitro.csv")
nitrosub <- nitro[nitro$tag!=444,]
library(nlme)
nitromixed.lme <- lme(pN^2 ~ factor(phy) + factor(lh)*factor(n) + factor(func) + factor(p), random=~1|block, data=nitrosub)
anova(nitromixed.lme)
summary(nitromixed.lme)


###################################################
### code chunk number 3: lecture8.Rnw:39-43
###################################################
detach(package:nlme)
library(lme4)
nitro.lmer <- lmer(pN^2 ~ factor(lh)*factor(n) + factor(func) + factor(p) + (1|block) + (1|phy), data=nitrosub)
anova(nitro.lmer)


###################################################
### code chunk number 4: lecture8.Rnw:48-49
###################################################
summary(nitro.lmer)


###################################################
### code chunk number 5: lecture8.Rnw:59-60
###################################################
confint(nitro.lmer)


###################################################
### code chunk number 6: lecture8.Rnw:65-67
###################################################
confint(nitro.lmer,method="Wald")
confint(nitro.lmer,method="boot")


###################################################
### code chunk number 7: lecture8.Rnw:76-80
###################################################
sumdat <- expand.grid(lh=levels(nitrosub$lh), n=0:1,func=levels(nitrosub$func),p=0:1)
cmat2 <- function(lh,n,func,p) c(1, lh=='NP', n==1, func=='mock', p==1, (lh=='NP')*(n==1))
contmat <- apply(sumdat,1,function(x) cmat2(x[1],x[2],x[3],x[4]))
sumdat$ests <- t(contmat)%*%fixef(nitro.lmer)


###################################################
### code chunk number 8: lecture8.Rnw:84-88
###################################################
bootnum <- 1000

bb <- bootMer(nitro.lmer,function(x) t(contmat)%*%fixef(x),nsim=bootnum,seed=222)
bb2 <- bootMer(nitro.lmer,function(x) predict(x,newdata=sumdat,re.form=NA),nsim=bootnum,seed=222)


###################################################
### code chunk number 9: lecture8.Rnw:93-94
###################################################
str(bb)


###################################################
### code chunk number 10: lecture8.Rnw:100-102
###################################################
head(bb$t)
head(bb$t)-head(bb2$t)


###################################################
### code chunk number 11: lecture8.Rnw:107-114
###################################################
apply(bb$t,2,function(x) round(sort(x)[as.integer(c(bootnum*.025,
                                                    bootnum*(1-.025)+1))],3))

sumdat <- cbind(sumdat,t(apply(bb$t,2,function(x) round(
  sort(x)[as.integer(c(bootnum*.025,bootnum*(1-.025)+1))],3))))




###################################################
### code chunk number 12: lecture8.Rnw:121-137
###################################################
ci.func2 <- function(rowvals, glm.vmat) {
  nor.func1a <- function(alpha, sig) 1-pnorm(-qnorm(1-alpha/2) * sum(sqrt(diag(sig))) / sqrt(c(1,-1) %*% sig %*%c (1,-1))) - pnorm(qnorm(1-alpha/2) * sum(sqrt(diag(sig))) / sqrt(c(1,-1) %*% sig %*% c(1,-1)), lower.tail=F)
  nor.func2 <- function(a,sigma) nor.func1a(a,sigma)-.95
  n <- length(rowvals)
  xvec1b <- numeric(n*(n-1)/2)
  vmat <- glm.vmat[rowvals,rowvals]
  ind <- 1
  for(i in 1:(n-1)) {
    for(j in (i+1):n){
      sig <- vmat[c(i,j), c(i,j)]
      #solve for alpha
      xvec1b[ind] <- uniroot(function(x) nor.func2(x, sig), c(.001,.999))$root
      ind <- ind+1
    }}
  1-xvec1b
}


###################################################
### code chunk number 13: lecture8.Rnw:142-147
###################################################
vmat <- t(contmat)%*%vcov(nitro.lmer)%*%contmat

(vmat-cov(bb$t))/vmat
median((vmat-cov(bb$t))/vmat)
max((vmat-cov(bb$t))/vmat)


###################################################
### code chunk number 14: lecture8.Rnw:152-154
###################################################
sigmat <- matrix(0,nrow=16,ncol=16)
sigmat


###################################################
### code chunk number 15: lecture8.Rnw:161-167
###################################################
sigmat[upper.tri(sigmat)]<-ci.func2(1:16,as.matrix(vmat))
sigmat

sigmate <- matrix(0,nrow=16,ncol=16)
sigmate[upper.tri(sigmate)]<-ci.func2(1:16,as.matrix(cov(bb$t)))



###################################################
### code chunk number 16: lecture8.Rnw:171-173
###################################################
c(sigmat[4,],sigmat[,4])
max(c(sigmat[4,],sigmat[,4]))


###################################################
### code chunk number 17: lecture8.Rnw:177-178
###################################################
(1-max(c(sigmat[4,],sigmat[,4])))/2


###################################################
### code chunk number 18: lecture8.Rnw:182-186
###################################################
sigvec <- sapply(1:16,function(x) (1-max(sigmat[x,],sigmat[,x]))/2)
signum <- as.integer(sigvec*bootnum)
sigvece <- sapply(1:16,function(x) (1-max(sigmate[x,],sigmate[,x]))/2)
signume <- as.integer(sigvece*bootnum)


###################################################
### code chunk number 19: lecture8.Rnw:191-195
###################################################
sumdat <- cbind(sumdat,t(sapply(1:16,function(x) round(sort(bb$t[,x])[c(signum[x],bootnum-signum[x]+1)],3))))
sumdat <- cbind(sumdat,t(sapply(1:16,function(x) round(sort(bb$t[,x])[c(signume[x],bootnum-signume[x]+1)],3))))

names(sumdat)[6:11] <- c("lwr95","upr95","lwrdi","uprdi","lwrdie","uprdie")


###################################################
### code chunk number 20: lecture8.Rnw:199-204
###################################################
library(ggplot2)
sumdat$p2 <- factor(sumdat$p,labels=c("P Control","P Added"))
sumdat$func2 <- factor(sumdat$func,labels=c("Inoculated","Mock Inoculated"))
ggplot(sumdat,aes(x=lh,y=ests,group=factor(n)))+
  geom_point(aes(color=factor(n)),position=position_dodge(width=.2))


###################################################
### code chunk number 21: lecture8.Rnw:208-211
###################################################
intplot <- ggplot(sumdat,aes(x=lh,y=ests,group=factor(n),color=factor(n)))+
  geom_point(,position=position_dodge(width=.2))+
  facet_grid(func2~p2)+labs(x="Life history",y="Leaf nitrogen")


###################################################
### code chunk number 22: lecture8.Rnw:216-217
###################################################
intplot


###################################################
### code chunk number 23: lecture8.Rnw:224-229
###################################################
intplot <- intplot +
  geom_errorbar(aes(ymin=lwr95,ymax=upr95),width=.1,position=position_dodge(width=.2))+
  geom_errorbar(aes(ymin=lwrdie,ymax=uprdie),width=.1,size=2,position=position_dodge(width=.2))+
  geom_point(shape="-",size=8,color="black",position=position_dodge(width=.2))
intplot


###################################################
### code chunk number 24: lecture8.Rnw:234-236
###################################################
intplot+scale_x_discrete(labels=c("Exotic Annual","Native Perennial"))+
  scale_color_discrete("Nitrogen Treatment",labels=c("Control","Added"))

