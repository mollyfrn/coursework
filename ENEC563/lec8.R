#Lecture 8 

### R code from vignette source '/home/james/work/teach/563/lectures/lecture8/lecture8.Rnw'
setwd("C:/git/coursework/ENEC563")
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
#look at variance in groups vs between groups; residual is sigma sq'd 
#Tau is std dev of the random effect; sigma is std dev of epsilon(fixed effect); both sq'd are variances 

#Tau sq'd is the covariance WITHIN groups bc they covary together 
#sigma sq'd is the variance WITH groups
#total var is Tau^2 + Sigma^2 
#correlation coef for covariance is Tau^2 / (sigma^2 + Tau^2)

#varrcorr will do for us
VarCorr(nitromixed.lme)
vars = as.numeric(VarCorr(nitromixed.lme)[,1])
corr = vars[1]/sum(vars) #what is correlation based on observed pattern of covariation within a block? way of defining
###################################################
### code chunk number 3: lecture8.Rnw:39-43
###################################################
detach(package:nlme)
library(lme4)
nitro.lmer <- lmer(pN^2 ~ factor(lh)*factor(n) + factor(func) + factor(p) + (1|block) + (1|phy), data=nitrosub)
#assuming sample randomly from random phylogenetic groups 
#want to model crossed effects in lme4 
#randome effects added to main formula with random block and phylo group 
anova(nitro.lmer)
#problem? f vals but no p vals 
#one way to get around p vals would be to do likelihood ratio test in lieu of p vals 
#could bootstrap but time consuming so not today

###################################################
### code chunk number 4: lecture8.Rnw:48-49
###################################################
summary(nitro.lmer)
#16 treatment means 
#use info from parametric boostraps to gen 95% confidence intervals and diff adjusted CI's 
#compare diffs between two diff means 

#how to gen CI's on effects 
#effects are just the coefs on our models 
#confint function pretty effective 
#lmer -> default confint output is to do profile conf intervals 
#recall least sqares: surfaces drawn were parabolas, variance estimate was width of parabolas 

#profile conf uses likelihood instead of least squares 
#moves intercept to left or right, and resets model until the fit is right 
#computationally intense but awesomely calibrated 
#default for confint lmer output

###################################################
### code chunk number 5: lecture8.Rnw:59-60
###################################################
confint(nitro.lmer) #profile more conservatives


###################################################
### code chunk number 6: lecture8.Rnw:65-67
###################################################
confint(nitro.lmer,method="Wald") #assumed quadratic shape, super fast NA's ests for sig for block; less conservative, but always symmetrical and also smaller 
#not as great for mixed models
confint(nitro.lmer,method="boot") #param bootstrap refits model data gen'd from model; most conservative, best
#Confidence intervals for mixed models with crossed effects best practice^^^^


#but now want to create CI's for sample means 
#means rep linear combos of diff param values 
#the variance of these values does not equal the sum of the variance (not all variance accounted for)
#var(B0+b1+b2) != var(b0)+var(b1)+var(b2)

###################################################
### code chunk number 7: lecture8.Rnw:76-80
###################################################
sumdat <- expand.grid(lh=levels(nitrosub$lh), n=0:1,func=levels(nitrosub$func),p=0:1) #not making preds for random effects yet, just the pop means of the fixed effects
cmat2 <- function(lh,n,func,p) c(1, lh=='NP', n==1, func=='mock', p==1, (lh=='NP')*(n==1)) #func is inoculation, p is phosphorous
#gen contrast matrix 
contmat <- apply(sumdat,1,function(x) cmat2(x[1],x[2],x[3],x[4])) #1 for rows
sumdat$ests <- t(contmat)%*%fixef(nitro.lmer) #transpose for matrix multiplication

cbind(sumdat$ests, predict(nitro.lmer, newdata = sumdat, re.form=NA)) #ignore random effects in prediction = re.form, otherwise will need predictor vars for the random effect 
#same preds both methods
###################################################
### code chunk number 8: lecture8.Rnw:84-88
###################################################
bootnum <- 1000 #10,000 min generally for this

bb <- bootMer(nitro.lmer,function(x) t(contmat)%*%fixef(x),nsim=bootnum,seed=222) #first arg is mod you want to parm bootstrap off of; will gen whole new dataset for that mod
#2nd arg is what you want out of that bootstrap, then the number of nsim so # sims, and seed in function
#function(x) t(contmat)%*%fixef(x) multiplying contrast matrix by the fixed effects is 2nd arg
bb2 <- bootMer(nitro.lmer,function(x) predict(x,newdata=sumdat,re.form=NA),nsim=bootnum,seed=222)
#rather than contrast matrix and fixed effects we can use predict; tell it not to use random effects

###################################################
### code chunk number 9: lecture8.Rnw:93-94
###################################################
str(bb)


###################################################
### code chunk number 10: lecture8.Rnw:100-102
###################################################
head(bb$t)
sumdat[1,] #compare to results of first col of bb$t 
head(bb$t)-head(bb2$t) #same preds using diff functions but same seeds so we know they both work 

#sort from smallest to largest to figure out conf ints for each parm mean based on 1000 bootstraps vals 
#can use indices of sorted #'s to get conf ints 

###################################################
### code chunk number 11: lecture8.Rnw:107-114
###################################################
apply(bb$t,2,function(x) round(sort(x)[as.integer(c(bootnum*.025,
                                                    bootnum*(1-.025)+1))],3))
#row 1 = 2.5%, row 2 = 07.5%; 95% of our vals are between 10.2 and 15.7
sumdat <- cbind(sumdat,t(apply(bb$t,2,function(x) round(
  sort(x)[as.integer(c(bootnum*.025,bootnum*(1-.025)+1))],3))))
#add as cols, compare to ests 



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
#v-cov matrix of sample treatment means
(vmat-cov(bb$t))/vmat #find var for empirical var-cov cov of bootstrap means
median((vmat-cov(bb$t))/vmat) 
max((vmat-cov(bb$t))/vmat)
#not even diff by 1; min diff between diff matrices
#can use p bootstrap output to get ests for model and much better off for it
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
max(c(sigmat[4,],sigmat[,4])) #will use as our CI 
#comparisons of the 4th 
#picking the index that we will use as our bound - how do we pick? 
#have to gen alpha first 
#alpha is 34% 

#34% of our bootstrap outcomes ideally in tails 
#17% on either side 
#obs 170 out of 1000 and on other side will be 831th obs 

###################################################
### code chunk number 17: lecture8.Rnw:177-178
###################################################
(1-max(c(sigmat[4,],sigmat[,4])))/2 #how much of the tail % do we have


###################################################
### code chunk number 18: lecture8.Rnw:182-186
###################################################
sigvec <- sapply(1:16,function(x) (1-max(sigmat[x,],sigmat[,x]))/2)
signum <- as.integer(sigvec*bootnum)
sigvece <- sapply(1:16,function(x) (1-max(sigmate[x,],sigmate[,x]))/2)
signume <- as.integer(sigvece*bootnum)
#16 for number of cols 
# number of tail probability for every treatment mean (probability that it's in the tails)
#then multiply by the boot number to get it for left side and then subtract from 1000 to get it from the top 
#ddi twice for the two diff var-covar matrices we made (but typically only need once)

#diff bet bootstrap, diff-CI's
#boostrap CI's are wider, so more conservative bc overlap more likely (ie signume)
cbind(signum, signume)


###################################################
### code chunk number 19: lecture8.Rnw:191-195
###################################################
sumdat <- cbind(sumdat,t(sapply(1:16,function(x) round(sort(bb$t[,x])[c(signum[x],bootnum-signum[x]+1)],3))))
sumdat <- cbind(sumdat,t(sapply(1:16,function(x) round(sort(bb$t[,x])[c(signume[x],bootnum-signume[x]+1)],3))))
#taking integers and adding actual values + or - 1000
names(sumdat)[6:11] <- c("lwr95","upr95","lwrdi","uprdi","lwrdie","uprdie")


###################################################
### code chunk number 20: lecture8.Rnw:199-204
###################################################
library(ggplot2)
sumdat$p2 <- factor(sumdat$p,labels=c("P Control","P Added"))
sumdat$func2 <- factor(sumdat$func,labels=c("Inoculated","Mock Inoculated"))
ggplot(sumdat,aes(x=lh,y=ests,group=factor(n)))+
  geom_point(aes(color=factor(n)),position=position_dodge(width=.2))

#two diff ways to get nice labels on facets would be by changing factor labels 
#creating new factor variable called p2 
#based on p, but with new labels control and added
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

