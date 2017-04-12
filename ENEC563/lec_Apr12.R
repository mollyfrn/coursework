#lec Apr 12 

#q 3 on assignment 

#log link mu = e^B0+b1X = e^B0 * e^B1X 
#when we use a log link and increase incrementally by 1 (mu_new) = mu * e^B1
#if we increase X by 1
#compare to additive/incremental 
#contrast - when you have a log link (log mu) you can never have a negative val, never going below 0 

#####
### R code from vignette source '/home/james/work/teach/563/lectures/lecture23/lecture23.Rnw'

#weakly informative priors -= vals with mans close to 0, very large std dev, normal otherwise 
setwd("C:/git/coursework/ENEC563")
###################################################
### code chunk number 1: lecture23.Rnw:14-16
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=3,fig.width=4)


###################################################
### code chunk number 2: lecture23.Rnw:21-23
###################################################
ipo <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/ipomopsis.txt", header=T)
out2 <- lm(Fruit ~ Root + Grazing, data=ipo)


###################################################
### code chunk number 3: lecture23.Rnw:65-68
###################################################
root <- ipo$Root
fruit <- ipo$Fruit
graze <- as.numeric(ipo$Grazing)-1


###################################################
### code chunk number 4: lecture23.Rnw:73-78
###################################################
library(rstan)
options(mc.cores = parallel::detectCores())

dataList <- list(N=length(root), fruit=fruit, root=root,graze=graze)
ipoanc <- stan(file="ipo.stan", data=dataList,iter=1000, chains=4)


###################################################
### code chunk number 5: lecture23.Rnw:85-87
###################################################
print(ipoanc)
summary(out2)


###################################################
### code chunk number 6: lecture23.Rnw:94-95
###################################################
stan_plot(ipoanc)+geom_vline(xintercept=0,linetype=2) #+ 
 # theme(text=element_text(family="Times")) #need potentially to add this theme to all stan plots
#44 warnings 


###################################################
### code chunk number 7: lecture23.Rnw:100-101
###################################################
stan_hist(ipoanc) #+ theme(text=element_text(family="Times"))


###################################################
### code chunk number 8: lecture23.Rnw:134-136
###################################################
ipoancstrongprior <- stan(file="ipostrongprior.stan", data=dataList,iter=1000, chains=4)
print(ipoancstrongprior)


###################################################
### code chunk number 9: lecture23.Rnw:141-142
###################################################
ipoancstrongprior <- stan(file="ipostrongprior.stan", data=dataList,iter=1500, chains=4)


###################################################
### code chunk number 10: lecture23.Rnw:144-147
###################################################

print(ipoancstrongprior)
coef(out2)
#get approx same results as would from freq convergence 

###################################################
### code chunk number 11: lecture23.Rnw:157-158
###################################################
#str(ipoanc)


###################################################
### code chunk number 12: lecture23.Rnw:163-164
###################################################
#str(ipoanc@sim)


###################################################
### code chunk number 13: lecture23.Rnw:169-170
###################################################
str(ipoanc@sim$samples[[1]])


###################################################
### code chunk number 14: lecture23.Rnw:176-177
###################################################
extract(ipoanc,permuted=FALSE)[1:10,,"beta[1]"]


###################################################
### code chunk number 15: lecture23.Rnw:186-188
###################################################
ggplot(data.frame(beta1=extract(ipoanc,permuted=FALSE)[,1,"beta[2]"],beta2=extract(ipoanc,permuted=FALSE)[,1,"beta[3]"]),aes(x=beta1,y=beta2))+geom_path(alpha=.1)
ggplot(data.frame(beta1=extract(ipoanc,permuted=FALSE)[,1,"beta[2]"],beta2=extract(ipoanc,permuted=FALSE)[,1,"beta[3]"]),aes(x=beta1,y=beta2))+geom_point()


###################################################
### code chunk number 16: lecture23.Rnw:193-194
###################################################
pairs(ipoanc) #varcov table 


###################################################
### code chunk number 17: lecture23.Rnw:199-200
###################################################
stan_trace(ipoanc) #+ theme(text=element_text(family="Times"))

#you WANT there to be mixing
###################################################
### code chunk number 18: lecture23.Rnw:209-211
###################################################
print(ipoanc)



###################################################
### code chunk number 19: lecture23.Rnw:215-216
###################################################
summary(ipoanc)


###################################################
### code chunk number 20: lecture23.Rnw:221-222
###################################################
summary(ipoanc)$summary


###################################################
### code chunk number 21: lecture23.Rnw:227-228
###################################################
summary(ipoanc,probs=c(.005,.5,.995))$summary


###################################################
### code chunk number 22: lecture23.Rnw:240-276
###################################################
# Figure 4: lecture 24
set.seed(10)
#generate data
stuff<-c(rnorm(1000), rnorm(300,mean=5.5))
# kernel density estimate
out.d <- density(stuff)
# Determine W that cuts off p = 0.95
out.diff <- sapply(seq(0.35, 0.5,.001), function(x) sum(out.d$y[out.d$y>=quantile(out.d$y, x)])/sum(out.d$y)-0.95)
which.one <- which.min(out.diff[out.diff>0])
cut.off <- seq(0.35,0.5,.001)[which.one]

# credible intervals
my.x<-out.d$x[out.d$y>=quantile(out.d$y, cut.off)]
my.y<-out.d$y[out.d$y>=quantile(out.d$y, cut.off)]
my.x2 <- out.d$x[out.d$x<=quantile(stuff,.975) & out.d$x>=quantile(stuff,.025)]

# identify regions under curve
grp1.x <- my.x[my.x >=range(my.x[my.x<3])[1] & my.x <=range(my.x[my.x<3])[2]]
grp1.y <- my.y[my.x >=range(my.x[my.x<3])[1] & my.x <=range(my.x[my.x<3])[2]]
grp2.x <- my.x[my.x >=range(my.x[my.x>3])[1] & my.x <=range(my.x[my.x>3])[2]]
grp2.y <- my.y[my.x >=range(my.x[my.x>3])[1] & my.x <=range(my.x[my.x>3])[2]]

plot(density(stuff), xlab=expression(theta), main='',cex.lab=1.2, col='grey70')
rug(my.x,side=1,col='dodgerblue1',line=-.5)
rug(my.x2,side=1,col=2)
#shade area under curve
polygon(c(grp1.x, grp1.x[length(grp1.x)],grp1.x[1]),c(grp1.y,0,0), col='lightblue', border=NA, density=50)
polygon(c(grp2.x, grp2.x[length(grp2.x)],grp2.x[1]),c(grp2.y,0,0), col='lightblue', border=NA, density=50)
segments(grp1.x[1],quantile(out.d$y, cut.off),grp2.x[length(grp2.x)],quantile(out.d$y, cut.off),lty=2)

arrows(3,.15,0.5,.1,code=2,angle=45,length=.1)
arrows(3.3,.15,5.5,.07,code=2,angle=45,length=.1)
text( 3.15,.15, 'Area = 0.95',pos=3,offset=0.1)
legend('topright',c('HPD','Percentile'),col=c('dodgerblue1',2), pch=15, pt.cex=1.2, cex=.9, bty='n', title=expression(bold('95% Credible Intervals')))
lines(out.d$x, out.d$y)
text(grp2.x[length(grp2.x)],quantile(out.d$y, cut.off),'W',pos=4)


###################################################
### code chunk number 23: lecture23.Rnw:281-283
###################################################
library(coda)
HPDinterval(as.mcmc(as.data.frame(ipoanc)[,1:4]))


###################################################
### code chunk number 24: lecture23.Rnw:291-297
###################################################
b1 <- data.frame(b1=extract(ipoanc)$beta[,2])
b1dens <- density(b1$b1)
b1mode <- b1dens$x[b1dens$y==max(b1dens$y)]
vlineframe <- data.frame(xline=c(b1mode,median(b1$b1),mean(b1$b1),coef(out2)[2]),label=c("Mode of posterior","Median of posterior","Mean of posterior","MLE"))
densplot <- ggplot(b1,aes(x=b1))
densplot+geom_density()+geom_vline(data=vlineframe,aes(xintercept=xline,color=label,linetype=label))+labs(color="",linetype="")


###################################################
### code chunk number 25: lecture23.Rnw:304-315
###################################################
birds <- read.table( 'birds.csv', header=T, sep=',')
birds <- birds[!is.na(birds$S),]
#complete pooling model
model1.glm <- glm(S~factor(year), data=birds, family=poisson)
#separate intercepts model
model2.glm <- glm(S~factor(patch)+factor(year), data=birds, family=poisson)
#random intercepts model
library(lme4)
model3.lmer <- glmer(S~factor(year) + (1|patch), data=birds, family=poisson)
#random intercepts model with predictors
model4.lmer <- glmer(S~factor(year)+log(area)+landscape +(1|patch), data=birds, family=poisson)


###################################################
### code chunk number 26: lecture23.Rnw:349-352
###################################################
dataList=list(N=length(birds$S),S=birds$S,year2006=as.numeric(birds$year==2006),year2007=as.numeric(birds$year==2007))
birdsmixing<- stan(file="completemix.stan", data=dataList,iter=1000, verbose=FALSE,chains=4)
print(birdsmixing,pars="beta")


###################################################
### code chunk number 27: lecture23.Rnw:357-359
###################################################
print(birdsmixing,pars="mu")
summary(birdsmixing,pars="mu")$summary[,1]


###################################################
### code chunk number 28: lecture23.Rnw:364-368
###################################################
library(rstanarm)
birdmix <- stan_glm(S~factor(year),family=poisson,data=birds)
summary(birdmix,digits=4)
predict(birdmix,type="response")

#computationally much faster
###################################################
### code chunk number 29: lecture23.Rnw:377-378
###################################################
birdpatchinter <- stan_glm(S~factor(patch)+factor(year),family=poisson,data=birds)


###################################################
### code chunk number 30: lecture23.Rnw:383-384
###################################################
birdpatchinter <- stan_glm(S~factor(patch)+factor(year),family=poisson,data=birds,iter=3000)


###################################################
### code chunk number 31: lecture23.Rnw:423-424
###################################################
J <- length(unique(birds$patch))


###################################################
### code chunk number 32: lecture23.Rnw:427-428
###################################################
patch <- as.numeric(factor(birds$patch))


###################################################
### code chunk number 33: lecture23.Rnw:432-434
###################################################
dataList=list(patch=patch,patchnum=J,N=length(birds$S),S=birds$S,year2006=as.numeric(birds$year==2006),year2007=as.numeric(birds$year==2007))
birdsepinter<- stan(file="sepinter.stan", data=dataList,iter=1000, chains=4)

