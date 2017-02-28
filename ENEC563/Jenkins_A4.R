#Molly Jenkins
#Assignment 4
#ENEC563 
#02/27/2017
# 
# 1) Read the data in and use table to see the number of counts of each family.  One family has only three observations.
#   i) Remove this family from the data.
#   ii) If you examine the Family, you will notice that there is a "ghost" level for the family that you remove.  
    #Run the droplevels function on the data frame and put the output in your data frame for analysis.
# 
# 2) If you run table on the Taxon variable, you will notice that some species have multiple representatives. 
  #There are frequently different relationships within taxa than there are among taxa for body size relationships
    # i) Discuss why this would be a problem for your analysis if you are primarily concerned with among taxon relationships
    # ii) To get around this problem, generate a new data frame that was one row for each Taxon and includes just the Family, 
        # and the mean pre- and post- diaphragmatic body length (see hint!)

# 3) Carry out a formal statistical test to determine whether the allometric exponents of the pre- and pos- diaphragmatic lengths 
    # are constant across the four families.  For convenience,  
    # use log (post- diaphragmatic length) as the response (x, dep) variable and log(pre-diaphragmatic length) as a predictor (y, ind).

# 4) If you determine that the exponents are different, determine the families for which the allometric exponents are significantly different 
    # and the orders for which they are not.

# 5) Refit the model so that the 8 coefficients are the intercepts and slopes for each of the families.  
    # Generate a confidence interval for each of the slopes and determine 
    # which families the relationship can and cannot be distinguished from an isometric one (i.e. the slope is 1).

# 6) Summarize the 3-5.
#   i) Describe in words the results of the models.  Focus on any differences betwee the aquatic (Otariidae and Phocidae) and terrestrial (Bovidae and Felidae) families.
#   ii) Display the results of the analysis by plotting the data and the predictions from the best fit model.


#setwd('C:/git/coursework/ENEC563/')
library(nlme)
library(dplyr)


####Problem 1####
body = read.csv('mammalianbody.csv', header = TRUE)
table(body$Family)

body2 = body[body[,1] != "Odobenidae", ] #all rows where family is not equal to Odobenidae
unique(body2$Family) #ghost level there when category isn't! 
body2 = droplevels(body2)
unique(body2$Family) #fixed

####Problem 2####
table(body2$Taxon)
#This could be problematic as it introduces variation within-groups; 
#no two representatives of the same group are necessarily the same in terms of allometric scaling. 
#This introduces extra variation in terms of deriving a generalizable body scale relationship.  

body3 = body2 %>% 
  group_by(Family,Taxon) %>%
  summarize(meanpre = mean(PreL), 
            meanpos= mean(PosL))

####Problem 3####
body3$logpre = log(body3$meanpre)
body3$logpos = log(body3$meanpos)

mod1 = lm(logpos~logpre*Family, data = body3) #Bovidae is intercept; separate slopes model
summary(mod1)
anova(mod1)
coef(mod1)
body3$preds = predict(mod1)
plot = ggplot(body3,aes(x=logpre,y=logpos,color=Family))+geom_point()
contrasts(body3$Family)
####Problem 4####
#Otariidae and Phocidae MAY be different, but Otariidae especially 

body3$fampho<-factor(body3$Family,c("Phocidae","Otariidae","Bovidae","Felidae")) #creates a new factor with Phocidae as the base level.
contrasts(body3$fampho)

####Problem 5####
mod2 = lm(logpos~logpre*fampho, data = body3)
summary(mod2)
anova(mod2)

#additive model with Family and Family:log(pre diaphragmatic length) and removing the intercept from the model

mod3 = lm(logpos~Family+Family:logpre -1, data=body3)
summary(mod3)
plot+geom_point()+geom_line(aes(y = predict(mod3)))
#slope for Otariidae definitely looks different but could also be because of single data point 
#check and compare output to lec9, rerun