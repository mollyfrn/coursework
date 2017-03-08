### R code from vignette source '/home/james/work/teach/563/lectures/lecture15/lecture15.Rnw'

###################################################
### code chunk number 1: lecture15.Rnw:18-20
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=3,fig.width=4)


###################################################
### code chunk number 2: lecture15.Rnw:28-29
###################################################
?dnbinom

#poisson -> variance getting closer to being same as the mean 
###################################################
### code chunk number 3: lecture15.Rnw:33-40
###################################################
nbinframe <- data.frame(x=rep(0:20,4),y=c(dnbinom(0:20,mu=4,size=0.1),
                                          dnbinom(0:20,mu=4,size=1),dnbinom(0:20,mu=4,size=10),
                                          dnbinom(0:20,mu=4,size=10)),size=factor(rep(c(0.1,1,10,100),each=21)))
library(ggplot2)
theme_set(theme_bw())
nbinplot <- ggplot(nbinframe,aes(x=x,y=y))
nbinplot+geom_bar(stat="identity")+facet_wrap(~size)


###################################################
### code chunk number 4: lecture15.Rnw:45-48
###################################################
nbinplot+geom_line(aes(color=size)) +geom_point(aes(color=size)
)+scale_color_discrete(labels=c(expression(theta == .1),
                                expression(theta == 1),expression(theta == 10),expression(theta == 100)))
#can draw greek letters!!!

###################################################
### code chunk number 5: lecture15.Rnw:52-58
###################################################
nbinpoisframe <- data.frame(x=rep(0:20,4),y=c(dnbinom(0:20,mu=4,size=1),
                                              dnbinom(0:20,mu=4,size=10),dnbinom(0:20,mu=4,size=100),
                                              dpois(0:20,lambda=4)),size=factor(rep(c("1","10","1000",
                                                                                      "Poisson"),each=21)))
nbinplot <- ggplot(nbinpoisframe,aes(x=x,y=y))
nbinplot+geom_bar(stat="identity")+facet_wrap(~size)


###################################################
### code chunk number 6: lecture15.Rnw:63-65
###################################################
nbinplot+geom_line(aes(color=size))+scale_color_discrete(labels=c(expression(
  theta == 1),expression(theta == 10),expression(theta == 100),"Poisson"))


###################################################
### code chunk number 7: lecture15.Rnw:69-70
###################################################
nbinplot+geom_bar(stat="identity",aes(fill=size),position="dodge")


###################################################
### code chunk number 8: lecture15.Rnw:75-81
###################################################
nbinframe <- data.frame(x=rep(0:40,4),y=c(dnbinom(0:40,mu=1,size=0.1),
                                          dnbinom(0:40,mu=10,size=.1),dnbinom(0:40,mu=100,size=.1),
                                          dnbinom(0:40,mu=1000,size=.1)),mu=factor(rep(c(1,10,100,1000),
                                                                                       each=41)))
nbinplot <- ggplot(nbinframe,aes(x=x,y=y))
nbinplot+geom_bar(stat="identity")+facet_wrap(~mu)


###################################################
### code chunk number 9: lecture15.Rnw:87-102
###################################################
num.stems <- c(6,8,9,6,6,2,5,3,1,4)
# data frame of tabulated data
aphids <- data.frame(aphids=0:9, counts=num.stems)
aphid.data <- data.frame(aphids=rep(0:9,num.stems))
poisson.LL <- function(lambda) sum(log(dpois(aphid.data$aphids, lambda)))
poisson.negloglik <- function(lambda) -poisson.LL(lambda)
out.pois <- nlm(poisson.negloglik,3)
exp.pois <- data.frame(x=0:9,y=c((dpois(0:8, out.pois$estimate)),
                                 1-ppois(8,out.pois$estimate))*50)
aphidplot <- ggplot(aphid.data,aes(x=aphids))
aphidplot <- aphidplot+geom_bar()+geom_line(data=exp.pois,
                                            aes(x=x,y=y,color="Poisson"))+geom_point(data=exp.pois,
                                                                                     aes(x=x,y=y,color="Poisson"))+labs(color="")
aphidplot



###################################################
### code chunk number 10: lecture15.Rnw:109-111
###################################################
NB.LL <- function(mu,theta) sum(log(dnbinom(aphid.data$aphids,mu=mu, size=theta)))
NB.LL(2,3)


###################################################
### code chunk number 11: lecture15.Rnw:116-119
###################################################
# version with a vector argument
NB.LL2 <- function(p) sum(log(dnbinom(aphid.data$aphids,mu=p[1], size=p[2])))
NB.LL2(c(2,3))


###################################################
### code chunk number 12: lecture15.Rnw:124-126
###################################################
aphidNB <- optim(c(3,4),NB.LL2,control=list(fnscale=-1),hessian=T)
aphidNB #similar output to nlm 
#final par ests are max likelihood ests 

#optim is most widely used optimization routine in R, uses a variety of methods 
#more likely to reach max than newton method 
#value is value at maximum 
#0 convergence means it reached full completion
#hessians we use to estimate the variance 
###################################################
### code chunk number 13: lecture15.Rnw:135-139
###################################################
# fit negative binomial using glm.nb
library(MASS) 
out.glmnb <- glm.nb(aphids~1,data=aphid.data)
summary(out.glmnb)

#default is a log link
#in order to get est of mean we have to take exponent of the coef of the output

#interested in finding regression on the intercept 
#doing reg on glm is regression on mean, how the mean varies typically
###################################################
### code chunk number 14: lecture15.Rnw:143-144
###################################################
exp(coef(out.glmnb)) #same theta est which is good 


###################################################
### code chunk number 15: lecture15.Rnw:148-150
###################################################
names(out.glmnb)
out.glmnb$theta


###################################################
### code chunk number 16: lecture15.Rnw:158-164
###################################################
# predicted probabilities with tail category
#compare fit of poisson and negative binomial 
#take probs, mult by 50 bc 50 probabilities, and for largest one add on tail
#tail freq by using pnbinom -> probability mass function, lower.tail = F to get upper tail

exp.p <- c(dnbinom(0:8,mu=aphidNB$par[1], size=aphidNB$par[2]), 
           pnbinom(8,mu=aphidNB$par[1], size=aphidNB$par[2],lower.tail=F))
exp.p
#expected counts/probabilities
#make sure they sum up to 1 
sum(exp.p)
exp.nb <- data.frame(x=0:9,y=50*exp.p) #multiply by number of obs we have

-aphidNB$hessian #under 1; solve, sqrt, diag
###################################################
### code chunk number 17: lecture15.Rnw:168-170
###################################################
aphidplot+geom_line(data=exp.nb,aes(x=x,y=y,color="Negative binomial")
)+geom_point(data=exp.nb,aes(x=x,y=y,color="Negative binomial"))

#add on a line with points
#neg binomial is MUCH better fit 
#we THINK we've got a better fit here 
#is it?
#let's do a goodness fit test 
#param and non-param tests 

#for param = need fewer than 20% of our expected probabilities to be less than 5 (looking for more greater than 5) 
#we have 4 cats that are less than five so breaks assumption 
#best case is to do non-param bootstrap test so shouldn't bother with param test 
#but can bunch together categories 

###################################################
### code chunk number 18: lecture15.Rnw:177-184
###################################################
exp.nb
# collaps categories to pass the >5 rule
exp.nb.short <- c(exp.nb$y[1:5], sum(exp.nb$y[6:7]), sum(exp.nb$y[8:10]))
exp.nb.short
# group observed values the same way
obs.short <- c(num.stems[1:5], sum(num.stems[6:7]), sum(num.stems[8:10]))
obs.short


###################################################
### code chunk number 19: lecture15.Rnw:189-194
###################################################
# Pearson chi squared statistic by hand sum from i = 1 to m where m = # categories; observed - expected ^2 over the expected (bio lab!!)
pearson <- sum((obs.short-exp.nb.short)^2/exp.nb.short)
pearson 
# p-value
1-pchisq(pearson, length(obs.short)-1-2)
#very high p value -> support for null hypothesis 
#null is that model fit the data, there is NO difference between model and data 
#preferable to do param bootstrap 

###################################################
### code chunk number 20: lecture15.Rnw:203-205
###################################################
# parametric bootstrap
chisq.test(num.stems, p=exp.p, simulate.p.value=T, B=999)
#p val from bootstrap agrees with param chi sqrd test
#goodness of fit exagerrated by lumping of categories? 

###################################################
### code chunk number 21: lecture15.Rnw:210-216
###################################################
#non param; no reason not to 
new.numstems <- c(num.stems,0)
new.NB.p <- c(dnbinom(0:9, mu=aphidNB$par[1], size=aphidNB$par[2]),
              1-pnbinom(9, mu=aphidNB$par[1], size=aphidNB$par[2]))
new.NB.p
new.NB.p*50
chisq.test(new.numstems, p=new.NB.p, simulate.p.value=TRUE, B=9999)
#p val is pretty low but only 20% of the time; still better than the poisson 
#1st arg is obs counts, prob within those, p value to get bootstrap, B is number of bootstraps
#had we used this aggregating technique we got great vals 
#whatever we do we want to get most conservative p value to avoid confirmation of assumptions 
#neg binomial much more flexible dist because it has this dispersion param 


#obs var is larger than predicted var often when we don't have a sep variance parameter 
#this is called overdispersion and it just means our model hasn't captured all of the variance that we see 
#one solution is to add more predictors 
#neg binomial -> diff dist that fits data better
#or quasi likelihood approach to increase variance predicted  -> var equals lambda times phi; there was phi times more variation than expected 
#increases size of CI's 
###################################################
### code chunk number 22: lecture15.Rnw:231-234
###################################################
slugs <- read.delim(url('https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/slugsurvey.txt'))
slugs[1:8,]
dim(slugs)


###################################################
### code chunk number 23: lecture15.Rnw:238-239
###################################################
slugs$field


###################################################
### code chunk number 24: lecture15.Rnw:243-245
###################################################
slug.table <- table(slugs$slugs, slugs$field)
slug.table


###################################################
### code chunk number 25: lecture15.Rnw:250-252
###################################################
slugplot <- ggplot(slugs,aes(x=slugs))
slugplot+geom_bar(aes(fill=field))


###################################################
### code chunk number 26: lecture15.Rnw:256-258
###################################################
slugplot+geom_bar(aes(fill=field),position="dodge")+scale_x_continuous(
  breaks=0:10)


###################################################
### code chunk number 27: lecture15.Rnw:261-263
###################################################
slugplot+geom_histogram(binwidth=1,boundary=-.5,aes(fill=field),position="dodge")+
  scale_x_continuous(breaks=0:10)


###################################################
### code chunk number 28: lecture15.Rnw:267-268
###################################################
slugplot+geom_bar()+facet_wrap(~field)+scale_x_continuous(breaks=0:10)


###################################################
### code chunk number 29: lecture15.Rnw:275-282
###################################################
# Single Poisson mean model
poisLL<-function(p) {
  LL <- sum(log(dpois(slugs$slugs,lambda=p)))
  LL}
mean(slugs$slugs)
poisLL(2)
poisLL(1.775)


###################################################
### code chunk number 30: lecture15.Rnw:287-290
###################################################
out.pois1 <- optim(2,poisLL,method="Brent",lower=0,upper=10,
                   control=list(fnscale=-1),hessian=T)
out.pois1
#can use optim to fit single dim models but neet to set method to Brent 
#set range of vals (initial guess 2, and a lower and an upper bound)
#add control of scale to maximize function 
#can have infinitely bad fits, but only one good one 
#best fit a poisson with a single mean is just the avg! 

###################################################
### code chunk number 31: lecture15.Rnw:295-297
###################################################
slugs$field=="Rookery"
as.numeric(slugs$field=="Rookery")


###################################################
### code chunk number 32: lecture15.Rnw:302-307
###################################################
# separate mean Poisson model
negpoisLL2<-function(p) {
  LL <- sum(log(dpois(slugs$slugs, lambda=p[1] + p[2]*(slugs$field=="Rookery"))))
  LL}
negpoisLL2(c(1,2)) #have to give it a two value vector mean in nursery is 1 
#and mean in rookery is 2 greater


###################################################
### code chunk number 33: lecture15.Rnw:314-315
###################################################
tapply(slugs$slugs, slugs$field,mean)


###################################################
### code chunk number 34: lecture15.Rnw:318-320
###################################################
out.pois2 <- optim(c(1.3,1),negpoisLL2,control=list(fnscale=-1),hessian=T)
out.pois2

#diff in mean between rookery and nursery 
#is nursery slug abun different P1 = B0, P2 = B1 
#null is that there are no diffs between them 
#null is Beta1 = 0, so no diff -> single mean model; single parm, Beta0 
#alt is that Beta1 != 0 -> 2 means model; 2 parms with both Beta0 and Beta1 

#compare the likelihood ratio of those means 

#if you get a neg value for likelihood ratio test, it just means you've done it backwards 
#or that you could be at a local minimum of the data 

###################################################
### code chunk number 35: lecture15.Rnw:332-336
###################################################
# likelihood ratio test that means are different
2*(out.pois2$value-out.pois1$value) #is 11 
LR <- 2*(out.pois2$value-out.pois1$value)
1-pchisq(LR,1) #p value is super low -> it's super rare that we'd see this big a diff if the null were true 
#reject null 

#likelihood itself is $value of object 
###################################################
### code chunk number 36: lecture15.Rnw:344-346
###################################################
#wald test: B0 null is that B1 = 0, alt is that it doesn't 
#two ways to do this: 
#1) create a CI = B^1 + or - the z value of .975*std error(B^1)
#std error(B^1) is from the inverse of info matrix ( matrix)
#2nd diagonal, member 2,2 is var and you take the sqrt to get std error

#B^1 / std error of B^1

#first need to find std error so take inverse of info matrix



info = -out.pois2$hessian #info matrix inverse
se2 <- sqrt(diag(solve(-out.pois2$hessian))) #need to use solve 
#two unknowns in two equations -> 
#solving this is equiv to finding inverse of matrix 
se2

#first val is for B0; 2nd is std error for our B1 
###################################################
### code chunk number 37: lecture15.Rnw:350-351
###################################################
#developing CI's for both B0 and B1
rbind(out.pois2$par + qnorm(.025)*se2, out.pois2$par + qnorm(.975)*se2)
#1st column is B0, 2nd is for B1 
#they don't overlap at 0, so we are confident that the means are diff 
#can get p value by calc-ing w stat (p value for a wald test)

###################################################
### code chunk number 38: lecture15.Rnw:360-362
###################################################
W <- out.pois2$par[2]/se2[2]
W

#to get p we do 2 sided test 
###################################################
### code chunk number 39: lecture15.Rnw:368-369
###################################################
2*(1-pnorm(W)) #1 - cumulative probability of getting w 

#using log link so interpreted slightly diff to get basically same result 
###################################################
### code chunk number 40: lecture15.Rnw:376-379
###################################################
# fit same models using glm
slugglm0 <- glm(slugs~1, data=slugs ,family=poisson)
slugglm1 <- glm(slugs~field, data=slugs, family=poisson)


###################################################
### code chunk number 41: lecture15.Rnw:383-385
###################################################
# likelihood ratio test comparing models
anova(slugglm0, slugglm1, test='Chisq')


###################################################
### code chunk number 42: lecture15.Rnw:390-393
###################################################

# sequential likelihood ratio tests
anova(slugglm1, test='Chisq')

#want log link if you end up with neg vals 
###################################################
### code chunk number 43: lecture15.Rnw:397-399
###################################################
# Wald test
summary(slugglm1)


###################################################
### code chunk number 44: lecture15.Rnw:404-407
###################################################
slugtable <- data.frame(table(slugs$slugs, slugs$field))
slugtable

#plot 2 mods, gen expected counts for each
#make predictions to add mu on there 

###################################################
### code chunk number 45: lecture15.Rnw:411-413
###################################################
slugtable$mu <- predict(slugglm1, type='response', newdata=data.frame(field=slugtable$Var2))
slugtable
#typically a pred on the log scale or response/back transformed scale with log link 
#so specifying type = response where field table equals slug table 

###################################################
### code chunk number 46: lecture15.Rnw:418-422
###################################################
# calcualte probabilities of each category
slugtable$p <- dpois(as.numeric(as.character(slugtable$Var1)), lambda=slugtable$mu)
sum(slugtable$p)

#now need to calc number and mult by 40 to get freq/expected freq 

###################################################
### code chunk number 47: lecture15.Rnw:427-429
###################################################
# add tail probability to the last category
slugtable$p <- dpois(as.numeric(as.character(slugtable$Var1)), lambda=slugtable$mu)+(slugtable$Var1==10)*ppois(as.numeric(as.character(slugtable$Var1)), lambda=slugtable$mu, lower.tail=F)

#gets up to 2 once added on 
###################################################
### code chunk number 48: lecture15.Rnw:433-434
###################################################
sum(slugtable$p)


###################################################
### code chunk number 49: lecture15.Rnw:438-442
###################################################
# obtain expected counts
table(slugs$field)
slugtable$exp <- 40*slugtable$p
slugtable


###################################################
### code chunk number 50: lecture15.Rnw:446-448
###################################################
slugplot <- ggplot(slugtable,aes(x=Var1,y=Freq))
slugplot+geom_bar(stat="identity")+geom_line(aes(y=exp,group=Var2))+geom_point(aes(y=exp))+facet_wrap(~Var2)

