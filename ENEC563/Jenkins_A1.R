####Jenkins A1####
#ENEC563 
#Assignment 1 
# 01/29/2017 
#setwd("C:/git/core-transient/")
library(wesanderson)
library(dplyr)
library(car)
library(ggplot2)

####Problem 1####

quinn1 = read.csv("https://sakai.unc.edu/access/content/group/7d7a0e1c-4adb-4ee2-ace8-490a89313a59/Data/quinn1.csv", header = TRUE)

#a
quinn1$total_eggs = quinn1$Eggs*quinn1$Density 


#b, fix
spr_mean = quinn1 %>% 
  group_by(Season, Density) %>% 
  filter(Season == "spring") %>%
  summarize(spr_mean = mean(Eggs))

spr_median = quinn1 %>% 
  group_by(Season, Density) %>% 
  filter(Season == "spring") %>%
  summarize(spr_median = median(Eggs))


sumr_mean = quinn1 %>% 
  group_by(Season, Density) %>% 
  filter(Season == "summer") %>%
  summarize(spr_mean = mean(Eggs))
  
sumr_median = quinn1 %>% 
  group_by(Season, Density) %>% 
  filter(Season == "spring") %>%
  summarize(sumr_median = median(Eggs))

#c 
quinn1 = quinn1 %>%
  group_by(Season, Density) %>% 
  mutate(avg_eggden = mean(Eggs)) %>%
  select(everything())
quinn1$avg_eggden = as.factor(quinn1$avg_eggden)

####Problem 2####
mod1 = lm(Eggs~Season+Density+Season:Density, data = quinn1)
mod2 = lm(Eggs~Season:Density, data = quinn1)
mod3 = lm(Eggs~Season+Density, data = quinn1)
mod4 = lm(Eggs~Season, data = quinn1)
mod5 = lm(Eggs~Density, data= quinn1)
summary(mod1)
summary(mod2)  
summary(mod3) #but together season and density in the additive model explain 85% of the variation, lower SE than mod4
summary(mod4) #season seems to have a much stronger effect than density, independent of density, R^2 value is still good 
summary(mod5)

quinn1$modelpreds = predict(mod3,newdata=quinn1)

####Problem 3#### 
theme_set(theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))
plot1 = ggplot(quinn1)+geom_boxplot(aes(fill = factor(Season), x=interaction(Season,Density),y=Eggs))+
  scale_fill_manual(values = wes_palette("Darjeeling"), name = "Season")+ 
  labs(x="Season x Density",y="Avg Eggs per Limpet")


