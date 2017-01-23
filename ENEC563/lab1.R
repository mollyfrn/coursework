### R code from vignette source '/home/james/work/teach/563/lectures/lecture2/lab1.Rnw'

# Interpretive code 
# R console is the interpreter 

###################################################
### code chunk number 1: lab1.Rnw:17-21
###################################################
2+2
4*7
(6+8)/7-1
2^2


###################################################
### code chunk number 2: lab1.Rnw:26-29
###################################################
log(4)
log10(4)
sin(2*pi) #irrational roundoff number errors can occur 


###################################################
### code chunk number 3: lab1.Rnw:34-35
###################################################
help()


###################################################
### code chunk number 4: lab1.Rnw:39-40
###################################################
help(sin)


###################################################
### code chunk number 5: lab1.Rnw:49-50
###################################################
help.search("Square root")


###################################################
### code chunk number 6: lab1.Rnw:55-57
###################################################
w <- 5+7
w/6


###################################################
### code chunk number 7: lab1.Rnw:64-65
###################################################
response <- c(4.493,3.796, 3.901, 3.992,4.458, 3.947, 3.916, 3.524, 4.245)


###################################################
### code chunk number 8: lab1.Rnw:70-71
###################################################
response[3]


###################################################
### code chunk number 9: lab1.Rnw:76-78
###################################################
d <- c(2,3,8)
response[d]


###################################################
### code chunk number 10: lab1.Rnw:83-84
###################################################
response[c(2,3,8)]


###################################################
### code chunk number 11: lab1.Rnw:89-91
###################################################
samples <- 1:9
samples


###################################################
### code chunk number 12: lab1.Rnw:95-97
###################################################
samples+response
samples*response


###################################################
### code chunk number 13: lab1.Rnw:102-104
###################################################
response*2
response+10 #element-wise effects automatically, which is nice and intentional for stats computing


###################################################
### code chunk number 14: lab1.Rnw:110-114
###################################################
log(response)
mean(response)
median(response)
sd(response)
summary(response)
log(mean(response))

####first problem time####
#1: mean of the first 3 elements of response vector
means = mean(response[c(1, 2, 3)])

#2: create vector starts at 8 and runs backwards to 1 
eight = seq(8, 1, by = -1)
eight_2 = 8:1 

###################################################
### code chunk number 15: lab1.Rnw:125-127
###################################################
x <- cbind(samples,response)
x


###################################################
### code chunk number 16: lab1.Rnw:131-133
###################################################
x <- rbind(samples,response
)


###################################################
### code chunk number 17: lab1.Rnw:140-143
###################################################
x
x[2]
x[3]


###################################################
### code chunk number 18: lab1.Rnw:148-149
###################################################
x[1,5]
x[2,5]

###################################################
### code chunk number 19: lab1.Rnw:154-156
###################################################
x[1,]
x[,5]


###################################################
### code chunk number 20: lab1.Rnw:165-166
###################################################
getwd()


###################################################
### code chunk number 21: lab1.Rnw:177-178
###################################################
setwd("C:/git/coursework/ENEC563")
#change to git

###################################################
### code chunk number 22: lab1.Rnw:183-184
###################################################
tadpoles <- read.table("tadpoles.csv",header=T,sep=",")
#when in folder

###################################################
### code chunk number 23: lab1.Rnw:189-190
###################################################
tadpoles <- read.table("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/tadpoles.csv",sep=",",header=T)
#from web

###################################################
### code chunk number 24: lab1.Rnw:197-207
###################################################
names(tadpoles)
head(tadpoles)
dim(tadpoles)
#examine a subset of rows
tadpoles[1:8,]
#or a subset of columns and rows
tadpoles[1:8,1:2]
#examine colums with name "fac1"
tadpoles[,"fac1"]
tadpoles$fac1


###################################################
### code chunk number 25: lab1.Rnw:213-217
###################################################
class(tadpoles)
class(tadpoles$response)
class(tadpoles$treatment)
class(tadpoles$fac1)


###################################################
### code chunk number 26: lab1.Rnw:224-225
###################################################
contrasts(tadpoles$fac2) #can do with factor variables

#can set a 0 for dummy variables; can tell us which dummy variables are used 


###################################################
### code chunk number 27: lab1.Rnw:232-233
###################################################
with(tadpoles,contrasts(fac1)) #default place to look for variables
contrasts(tadpoles$fac1) # same thing 

###################################################
### code chunk number 28: lab1.Rnw:238-240
###################################################
class(tadpoles$fac3)
tadpoles$family <- factor(tadpoles$fac3)


###################################################
### code chunk number 29: lab1.Rnw:245-249
###################################################
table(tadpoles$treatment) #unbalanced design bc tadpoles died, a little tricky
with(tadpoles,table(fac1,fac2)) #how many replicates of diff variable values are there? 
table(tadpoles[,3:5])
table(tadpoles[,c(3:4,6)])


###################################################
### code chunk number 30: lab1.Rnw:265-267
###################################################
simplemod <- lm(response~fac2,data=tadpoles)
summary(simplemod)

#intercept here is the mean response in detritus treatment 
#the ??1 is the difference between the shrimp and the detritus treatments 

###################################################
### code chunk number 31: lab1.Rnw:274-277
###################################################
tv <- qt(.975,237)
.10211+.05081*tv
.10211-.05081*tv
#value we use for confidence intervals is 1.96 - diff between this and our tv value is where sig is derived

###################################################
### code chunk number 32: lab1.Rnw:295-296
###################################################
contrasts(tadpoles$treatment)


###################################################
### code chunk number 33: lab1.Rnw:299-301
###################################################
oneway <- lm(response~treatment,data=tadpoles)
summary(oneway)
#pretty great!! explains a lot, a lot better than a random model 
#we don't know which particular variable is important or about intrxns

###################################################
### code chunk number 34: lab1.Rnw:319-321
###################################################
threeway <- lm(response~fac1+fac2+family+fac1:fac2+fac1:family+fac2:family+
                 fac1:fac2:family,data=tadpoles)

#allows treatments to be looked at independently of each other 

###################################################
### code chunk number 35: lab1.Rnw:326-328
###################################################
threeway <- lm(response~fac1*fac2*family,data=tadpoles)
summary(threeway)


###################################################
### code chunk number 36: lab1.Rnw:338-339
###################################################
anova(threeway)


###################################################
### code chunk number 37: lab1.Rnw:355-358
###################################################
install.packages("car")
library(car)
Anova(threeway)


###################################################
### code chunk number 38: lab1.Rnw:365-366
###################################################
simplerlm <- update(threeway, .~. - fac1:family - fac1:fac2:family)


###################################################
### code chunk number 39: lab1.Rnw:374-375
###################################################
boxplot(response,data=tadpoles)


###################################################
### code chunk number 40: lab1.Rnw:380-381
###################################################
boxplot(response~fac1*fac2*family,data=tadpoles)


###################################################
### code chunk number 41: lab1.Rnw:386-387
###################################################
plot(response~interaction(fac1,fac2,family),data=tadpoles)


###################################################
### code chunk number 42: lab1.Rnw:394-395
###################################################
plot(response~interaction(fac1,fac2,family),data=tadpoles,xlab="Treatment",ylab="Intestinal activity")


###################################################
### code chunk number 43: lab1.Rnw:406-409
###################################################
install.packages("ggplot2")
library(ggplot2)
ggplot(tadpoles)


###################################################
### code chunk number 44: lab1.Rnw:414-415
###################################################
ggplot(tadpoles)+geom_boxplot(mapping=aes(x=1,y=response))


###################################################
### code chunk number 45: lab1.Rnw:419-420
###################################################
ggplot(tadpoles)+geom_point(mapping=aes(x=1,y=response))


###################################################
### code chunk number 46: lab1.Rnw:425-426
###################################################
ggplot(tadpoles)+geom_boxplot(aes(x=interaction(fac1,fac2,family),y=response))


###################################################
### code chunk number 47: lab1.Rnw:431-433
###################################################
ggplot(tadpoles)+geom_boxplot(aes(x=interaction(fac1,fac2,family),y=response))+
  xlab("Treatment")+ylab("Intestinal function")


###################################################
### code chunk number 48: lab1.Rnw:440-441
###################################################
tadpoles$modelpreds <- predict(simplerlm)


###################################################
### code chunk number 49: lab1.Rnw:446-448
###################################################
tadpoles$modelpreds <- predict(simplerlm,newdata=tadpoles,na.action=na.exclude)
tadpoles$modelpreds[1:10]


###################################################
### code chunk number 50: lab1.Rnw:455-458
###################################################
ggplot(tadpoles)+geom_point(aes(x=fac1,y=modelpreds,color=fac2))+
  facet_wrap(~family)+geom_line(aes(x=fac1,y=modelpreds,color=fac2,group=fac2))+
  labs(x="Treatment",y="Response",color="Diet")


###################################################
### code chunk number 51: lab1.Rnw:463-464
###################################################
summary(simplerlm)


###################################################
### code chunk number 52: lab1.Rnw:469-470
###################################################
simplestlm <- update(simplerlm,.~. - fac1:fac2)


###################################################
### code chunk number 53: lab1.Rnw:475-479
###################################################
tadpoles$modelpreds <- predict(simplestlm,newdata=tadpoles,na.action=na.exclude)
ggplot(tadpoles)+geom_point(aes(x=fac2,y=modelpreds,shape=fac1,color=family))+
  geom_line(aes(x=fac2,y=modelpreds,color=family,group=interaction(fac1,family)))+
  labs(x="Diet",y="Intestinal function",color="Family",shape="Treatment")


###################################################
### code chunk number 54: lab1.Rnw:488-493
###################################################
tadpoles$modelpreds <- predict(threeway,newdata=tadpoles,na.action=na.exclude)
ggplot(tadpoles)+geom_point(aes(x=fac2,y=modelpreds,color=family))+
  geom_line(aes(x=fac2,y=modelpreds,color=family,group=family))+facet_wrap(~fac1)+
  labs(x="Diet",y="Intestinal function",color="Family")



###################################################
### code chunk number 55: lab1.Rnw:497-501
###################################################
ggplot(tadpoles)+geom_point(aes(x=fac1,y=modelpreds,color=family))+
  geom_line(aes(x=fac1,y=modelpreds,color=family,group=family))+facet_wrap(~fac2)+
  labs(x="Treatment",y="Intestinal function",color="Family")



###################################################
### code chunk number 56: lab1.Rnw:507-510
###################################################
ggplot(tadpoles)+geom_point(aes(x=family,y=modelpreds,color=fac1))+
  geom_line(aes(x=family,y=modelpreds,color=fac1,group=fac1))+facet_wrap(~fac2)+
  labs(x="Family",y="Intestinal function",color="Treatment")

