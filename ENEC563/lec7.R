### R code from vignette source '/home/james/work/teach/563/lectures/lecture7/lecture7.Rnw'
#picking up where left off on getting p values from parametric boostrapping
###################################################
### code chunk number 1: lecture7.Rnw:18-20
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=4,fig.width=6)


###################################################
### code chunk number 2: lecture7.Rnw:25-30
###################################################
plants <- read.table("https://sakai.unc.edu/access/content/group/f5feddf9-8dc1-40d7-be6a-477559004456/Data/jimsonweed.txt", header=T)
plants$fpot <- factor(plants$pot) #additive model with fixed effect for pot; pot avg and then change in lw ratio that's constant across all pots
fixedmod <- lm(lw.rat~fpot+type, data=plants)
anova(fixedmod) #both pot and type are sig but can't remove blocking variable from model; if ignored you are inflating your power
summary(fixedmod)


###################################################
### code chunk number 3: lecture7.Rnw:42-44
###################################################
library(lme4)
mod2.lmer <- lmer(lw.rat~type+(1|fpot), data=plants)
#forumaltion: random effects within (), what random effect is going to be on is the 1 
#so just changes the intercept (altho can be on slopes), changes the mean for each data point

anova(mod2.lmer)
#doesn't give us a p value! 
#sampled from data we had and created a test stat from it for f stat 
#p boots is a little diff 
#take the null model: since interested in testing effect of type here, null will be just intercept*random effect 
#then parametrically simulate data using simulate function 
#then fit the complicated model 
#then get the f value
#then repeat # times 
#will then have n (#) f values from simulated model and fitting complicated model on it 
#so extract actual f value 

#so taking 4th value from column vector of anova 






###################################################
### code chunk number 4: lecture7.Rnw:50-53
###################################################
# The F-statistic is in the ANOVA table
Factual<-anova(mod2.lmer)[,4]
Factual


###################################################
### code chunk number 5: lecture7.Rnw:57-60
###################################################
# use a parametric bootstrap to obtain a p-value
# fit a model to the data without type as a predictor
mod1.lmer <- lmer(lw.rat~(1|fpot), data=plants) #type not included bc null, type no effect 


###################################################
### code chunk number 6: lecture7.Rnw:64-75
###################################################
parbootf <- function(model){
  # simulate data from model in which type has no effect
  rmath <- unlist(simulate(model)) #just recreates dataframe 
  # estimate type model to these data
  rmod <- lmer(rmath~(1|pot)+type, data=plants) #fitting test/complicated model where type DOES have effect
  # extract statistic
  fstat <- anova(rmod)[1,4]
  fstat
}
set.seed(111) #can put any number you want in here, has no bearing 
Fstatdist <- replicate(999,parbootf(mod1.lmer)) #repeats a function a set number of times (alt to forloop)
#how many times should you bootstrap? as many times as possible to get a precise enough p value 
#min of 10,000 best; can also use forloop in lieu of or nesting function instead of "replicate" function
#but built in functions may be faster than forloops so whatevs 

###################################################
### code chunk number 7: lecture7.Rnw:79-85
###################################################
max(Fstatdist)
Fstatdist <- c(Factual,c(Fstatdist))
# null distribution of F-statistic
library(ggplot2)
ggplot(data.frame(Fstatdist),aes(x=Fstatdist))+geom_density()+ #set dist as dataframe 
  annotate("point",y=0,x=Factual,color="red",size=3)


###################################################
### code chunk number 8: lecture7.Rnw:89-91
###################################################
# p-value of actual F-statistic
sum(Factual<=Fstatdist)/1000


###################################################
### code chunk number 9: lecture7.Rnw:100-104
###################################################
# treat blocks as random: using nlme package
library(nlme)
mixed.lme1 <- lme(fixed=lw.rat~type, random=~1|pot, data=plants)
anova(mixed.lme1)


###################################################
### code chunk number 10: lecture7.Rnw:109-110
###################################################
intervals(mixed.lme1)


###################################################
### code chunk number 11: lecture7.Rnw:114-115
###################################################
intervals(mixed.lme1,which="fixed")


###################################################
### code chunk number 12: lecture7.Rnw:120-124
###################################################
# to avoid function conflicts unload nlme from memory
detach(package:nlme)
library(lme4)
mixed.lmer1 <- lmer(lw.rat~type+(1|pot), data=plants) 


###################################################
### code chunk number 13: lecture7.Rnw:131-133
###################################################
fitted(mixed.lmer1)
fitted(mixed.lme1)


###################################################
### code chunk number 14: lecture7.Rnw:142-144
###################################################
mean.dat <- data.frame(fix.ests=predict(fixedmod), mix.ests=fitted(mixed.lmer1))
new.dat3 <- cbind(plants, mean.dat)


###################################################
### code chunk number 15: lecture7.Rnw:155-157
###################################################
as.numeric(new.dat3$type)
new.dat3$pop.mean <- fixef(mixed.lmer1)[1]+fixef(mixed.lmer1)[2]*(as.numeric(new.dat3$type)-1)


###################################################
### code chunk number 16: lecture7.Rnw:161-170
###################################################
require(ggplot2)
theme_set(theme_bw())

ggplot(new.dat3,aes(x=lw.rat,y=fpot))+geom_point(aes(color="Raw data"),size=1)+
  geom_point(aes(x=mix.ests,color="Conditional means"),shape="|",size=4)+
  geom_point(aes(x=fix.ests,color="Fixed estimates"),shape="|",size=4)+
  geom_line(aes(y=as.numeric(fpot),x=pop.mean,color="Population Mean"),
            linetype=2)+facet_wrap(~type)+
  labs(x="Length:Width",y="Pot",color="")


###################################################
### code chunk number 17: lecture7.Rnw:175-182
###################################################
ggplot(new.dat3,aes(x=lw.rat,y=fpot))+geom_point(aes(color="Raw data"),size=1)+
  geom_point(aes(x=mix.ests,color="Conditional means"),shape="|",size=4)+
  geom_point(aes(x=fix.ests,color="Fixed estimates"),shape="|",size=4)+
  geom_line(aes(y=as.numeric(fpot),x=pop.mean,color="Population Mean"),
            linetype=2)+facet_wrap(~type,scales="free_x")+
  labs(x="Length:Width",y="Pot",color="")



###################################################
### code chunk number 18: lecture7.Rnw:189-193
###################################################
install.packages("faraway")
library(faraway)
data(oatvar)
oatvar[1:8,]


###################################################
### code chunk number 19: lecture7.Rnw:200-202
###################################################
table(oatvar$block, oatvar$variety)



###################################################
### code chunk number 20: lecture7.Rnw:208-210
###################################################
oatfixed1<- lm(yield~block*variety, data=oatvar)
anova(oatfixed1)


###################################################
### code chunk number 21: lecture7.Rnw:216-218
###################################################
oatfixed2<- lm(yield~block+variety, data=oatvar)
anova(oatfixed2)


###################################################
### code chunk number 22: lecture7.Rnw:223-225
###################################################
library(nlme)
oatmixed <- lme(yield~variety,random=~1|block,data=oatvar,control=list(opt="optim"))


###################################################
### code chunk number 23: lecture7.Rnw:241-242
###################################################
coef(oatfixed2)


###################################################
### code chunk number 24: lecture7.Rnw:247-249
###################################################
blockcoefs <- c(0,coef(oatfixed2)[2:5])
treatcoefs <- c(0,coef(oatfixed2)[6:12])


###################################################
### code chunk number 25: lecture7.Rnw:253-256
###################################################
oatvar$ab <- rep(blockcoefs,8) * rep(treatcoefs, each=5)
oatvarextend <- data.frame(oatvar, a=rep(blockcoefs, 8), b=rep(treatcoefs, each=5))
oatvarextend[1:12,]


###################################################
### code chunk number 26: lecture7.Rnw:260-262
###################################################
oatfixed3 <- update(oatfixed2, .~.+ab)
anova(oatfixed3)


###################################################
### code chunk number 27: lecture7.Rnw:271-272
###################################################
ggplot(oatvar,aes(x=yield,y=block,color=variety))+geom_point()


###################################################
### code chunk number 28: lecture7.Rnw:276-278
###################################################
ggplot(oatvar,aes(x=yield,y=block,color=variety,group=variety))+geom_point()+
  geom_path()


###################################################
### code chunk number 29: lecture7.Rnw:283-284
###################################################



###################################################
### code chunk number 30: lecture7.Rnw:291-294
###################################################
nitro <- read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/nitro.csv")
dim(nitro)
nitro[1:8,]


###################################################
### code chunk number 31: lecture7.Rnw:300-301
###################################################
table(nitro$lh, nitro$func, nitro$n, nitro$p)


###################################################
### code chunk number 32: lecture7.Rnw:306-308
###################################################
nitro$trt <- paste(nitro$lh, nitro$func, nitro$n, nitro$p, sep='.')
nitro[1:8,]


###################################################
### code chunk number 33: lecture7.Rnw:311-312
###################################################
table(nitro$trt, nitro$block)


###################################################
### code chunk number 34: lecture7.Rnw:316-317
###################################################
table(nitro$trt, nitro$phy)


###################################################
### code chunk number 35: lecture7.Rnw:326-330
###################################################
nitrosub <- nitro[nitro$tag!=444,]
nitrofix <- lm(pN^2 ~ factor(block)+factor(phy) + factor(lh)*factor(n)*factor(func)*factor(p), 
               data=nitrosub)
anova(nitrofix)


###################################################
### code chunk number 36: lecture7.Rnw:335-337
###################################################
library(car)
Anova(nitrofix)


###################################################
### code chunk number 37: lecture7.Rnw:344-348
###################################################
nitrofix1 <- lm(pN^2 ~ factor(block)+factor(phy) + (factor(lh) + factor(n) + factor(func) + 
                                                      factor(p))^2, data=nitrosub)
nitrofix2 <- update(nitrofix1, .~. + factor(lh):factor(func):factor(p))
anova(nitrofix2)


###################################################
### code chunk number 38: lecture7.Rnw:353-354
###################################################
Anova(nitrofix1)


###################################################
### code chunk number 39: lecture7.Rnw:359-362
###################################################
nitrofix3 <- lm(pN^2 ~ factor(block)+factor(phy) + factor(lh)*factor(n)+factor(func)+factor(p), 
                data=nitrosub)
anova(nitrofix3)


###################################################
### code chunk number 40: lecture7.Rnw:365-366
###################################################
Anova(nitrofix3)


###################################################
### code chunk number 41: lecture7.Rnw:375-380
###################################################
library(nlme)
nitromixed.lme <- lme(pN^2 ~ factor(phy) + factor(lh)*factor(n) + factor(func) + factor(p), 
                      random=~1|block, data=nitrosub)
anova(nitromixed.lme)
summary(nitromixed.lme)


###################################################
### code chunk number 42: lecture7.Rnw:405-410
###################################################
detach(package:nlme)
library(lme4)
mod3.lmer <- lmer(pN^2 ~ factor(lh)*factor(n) + factor(func) + factor(p) + (1|block) + (1|phy), 
                  data=nitrosub)
anova(mod3.lmer)


###################################################
### code chunk number 43: lecture7.Rnw:418-419
###################################################
summary(mod3.lmer)

