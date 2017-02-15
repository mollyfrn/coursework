#Lec 9 
#https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/ipomopsis.txt
#ANCOVA

### R code from vignette source '/home/james/work/teach/563/lectures/lecture9/lecture9.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: lecture9.Rnw:24-26
###################################################
opts_knit$set(width=40)
opts_chunk$set(size="small",fig.height=4,fig.width=6)


###################################################
### code chunk number 2: lecture9.Rnw:38-40
###################################################
ipo <- read.delim('https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/ipomopsis.txt') #equivalent to read.table('ipomopsis.txt',header=T,sep='\t')
ipo[1:10,]


###################################################
### code chunk number 3: lecture9.Rnw:48-52
###################################################
ipolm <- lm(Fruit~Grazing, data=ipo) #assumes homoskedasticity; variance bet groups is same
#mean of fruit grazed is 67.9 mg; effect of no grazing is to lower fruit weight by about 17 mg's
anova(ipolm)
# grazing has positive effect
summary(ipolm)


###################################################
### code chunk number 4: lecture9.Rnw:57-61
###################################################
# t-test with separate variances
t.test(Fruit ~ Grazing, data = ipo)
# t-test with pooled variances = ANOVA
t.test(Fruit ~ Grazing, data = ipo, var.equal=T) #assume var is equal 
#^^^^can conclude that var between two is the same bc p vals unaffected bet/two diff t.tests^

###################################################
### code chunk number 5: lecture9.Rnw:65-67
###################################################
library(ggplot2)
ggplot(ipo,aes(x=Grazing,y=Fruit))+geom_boxplot()
#plot realtionship for continuous var 

###################################################
### code chunk number 6: lecture9.Rnw:71-73
###################################################
ipoplot <- ggplot(ipo,aes(x=Root,y=Fruit,color=Grazing))
ipoplot+geom_point()
#classic case of confounded variables 
#larger roots in grazed plants which then also has larger fruits 
#chicken or the egg? roots or grazing cause larger fruit? 
#scale out root mass 

###################################################
### code chunk number 7: lecture9.Rnw:80-86
###################################################
# change response to ratio
iporatiolm <- lm(I(Fruit/Root)~Grazing, data=ipo) #I stands for inhibit; inhibit tendency for R to associate root as a subset of fruit 
# grazing effect is not significant
anova(iporatiolm)
# grazing has negative effect but not significant
summary(iporatiolm)
#grazing explains almost none of the variation, especially on the fruit to root ratio/relationship 


#ANCOVA appraoch: 
#look at three diff models for these data 
#look at model for our mean and our yhats 
#x = root biomass, z = grazing variable 0 grzed, 1 ungrazed , mu = mean fruit size 
#mu1 = B0+B1X -> linear relationship bet/root and fruit with no grazing effect; mod1 
#mu2 = B0+B1X+B2z -> will shift both intercepts but specficially fruit intercept; 1 will be for grazed and one for ungrazed, will put two lines 
  #when grazed, z = 0, so mu2 = B0; when not grazed, z= 1, so mu2 = B0+B2
#mu3 = B0+B1x+B2z+B3XZ 
#gonna have intrxn term bet/grazed and root size (B3 term), 
#will be 2 lines but not necessarily parallel 
#when z = 0 (grazed), mu = B0+B1x 
#when z = 1 (ungrazed), mu = B0+B1x+B2+B3 (subbed in 1 for z's)
  #so (B0+B2)+(B1+B3)x -> 1st is intercept; 2nd is slope 

#fit and plot below: 

###################################################
### code chunk number 8: lecture9.Rnw:134-136
###################################################
iporoot1 <- lm(Fruit~Root,data=ipo)
summary(iporoot1)
#suggests negative root mass?? can't have -41 mg 
#extrapolation beyond range of data is absurd!
#if you want meaningful intercepts you can center your data; subtract off mean to get vals both neg and pos 
#can be helpful with fitting mixed models 
#standardizing is dividing by std. dev 


###################################################
### code chunk number 9: lecture9.Rnw:142-146
###################################################
iporoot2 <- lm(Fruit~Root+Grazing, data=ipo) #additive model
anova(iporoot2)
# now grazing has a significant negative effect
summary(iporoot2)


###################################################
### code chunk number 10: lecture9.Rnw:153-159
###################################################

# examine interaction model
iporoot3 <- lm(Fruit~Root*Grazing, data=ipo) #intrxn effect in model 
# test whether slopes are the same
anova(iporoot3)
summary(iporoot3)


###################################################
### code chunk number 11: lecture9.Rnw:172-174
###################################################
ipoplot <-ggplot(ipo,aes(x=Root,y=Fruit))
ipoplot+geom_point()+geom_abline(intercept=coef(iporoot1)[1],slope=coef(iporoot1)[2])
#1st and 2nd coefs from simple model 
#brute force way to extract intercept and slope from model 

###################################################
### code chunk number 12: lecture9.Rnw:179-180
###################################################
ipoplot+geom_point()+geom_smooth(se=F)
#default loess is a locally weighted regression 
#or using GAMs are great for large datasets with nonlinear env variables (me, Sara, Lewis)



###################################################
### code chunk number 13: lecture9.Rnw:186-187
###################################################
fortify(iporoot2)[1:10,] #look at what model output would look like for ggplot 
#.hat is influence or effect 
#sigma is the width of the SE band 
#cook's dist is dist data is from predicted 
#fitted is val from model 
#have access to all of these from model!

###################################################
### code chunk number 14: lecture9.Rnw:192-193
###################################################
ggplot(iporoot2,aes(x=Root,y=Fruit,color=Grazing))+geom_point()+geom_line(aes(y=.fitted))
#line aesthetic for fitted vals from model 

###################################################
### code chunk number 15: lecture9.Rnw:200-201
###################################################
ggplot(iporoot3,aes(x=Root,y=Fruit,color=Grazing))+geom_point()+geom_smooth(method=lm,se=F)


###################################################
### code chunk number 16: lecture9.Rnw:243-246
###################################################
goby <- read.delim('goby.txt')
goby[1:10,]
goby$frefuge <- factor(goby$refuge,labels=c("low","medium","high")) 


###################################################
### code chunk number 17: lecture9.Rnw:250-252
###################################################
gobyplot <- ggplot(goby,aes(x=density,y=mortality, color=frefuge))
gobyplot+geom_point()                  


###################################################
### code chunk number 18: lecture9.Rnw:256-261
###################################################

# separate slopes model
gobymod <- lm(mortality~density*frefuge, data=goby)
coef(gobymod)
anova(gobymod)


###################################################
### code chunk number 19: lecture9.Rnw:291-292
###################################################
summary(gobymod)


###################################################
### code chunk number 20: lecture9.Rnw:296-302
###################################################

# refit model with 3 as the reference group
goby$frefugehigh <- factor(goby$frefuge,levels=c("high","medium","low"))
gobymoda <- lm(mortality~density*frefugehigh, data=goby)
# slope for refuge 2 is different from slope for refuge 1
summary(gobymoda)


###################################################
### code chunk number 21: lecture9.Rnw:309-311
###################################################
gobymodb <- lm(mortality~frefuge +frefuge:density -1, data=goby)
summary(gobymodb)


###################################################
### code chunk number 22: lecture9.Rnw:318-319
###################################################
gobyplot+geom_point()+geom_smooth(method=lm,se=F)


###################################################
### code chunk number 23: lecture9.Rnw:324-328
###################################################
goby$pred <- predict(gobymod)
gobyplot <- ggplot(goby,aes(x=density,y=mortality,color=frefuge))
gobyplot+geom_point()+geom_line(aes(y=pred))+scale_color_discrete("Refuge density",labels=c(
  "Low","Medium","High"))

