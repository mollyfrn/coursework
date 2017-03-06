#Molly Jenkins 
#Vellend ch. 6, Box 4 
# Description of the code.
# 
# This is the start of the first time loop, one loop for each year. 
# freq.1 at the start of the year is used to calculate fit.ratio and Pr.1, 
# which will remain constant for the whole year (a period of J deaths).

# for (i in 1:(num.years-1)) {
#   
#   freq.1 <- sum(COM==1)/J; freq.2 <- 1 - freq.1
#   fit.ratio <- exp(freq.dep*(freq.1-0.5) + log(fit.ratio.avg))
#   Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2)

#   This is the second time loop, one loop for each of J deaths.

#   for (k in 1:J) {
#     COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1))
#   }


#"Delayed" means that fitness is fixed for an entire "year", thus requiring two time loops instead of one. 
#Only one simulation is run here (i.e., there is no loop over num.sims)

## specify parameters, initial conditions, and output vector
num.years <- 50
freq.1.vec <- vector(length = num.years)

J <- 500
init.1 <- 0.1*J
COM <- vector(length = J)
COM[1:init.1] <- 1 #first 50 out of 500 vals = 1
COM[(init.1+1):J] <- 2 #remaining values out of 500 vals (in this case, 450) = 2
#instead of creating a loop with J*num.years deaths, two loops created

year <- 2

fit.ratio.avg <- 1
freq.dep <- -20 #strong negative freq dependence

## record data (frequency of species 1) for year 1
freq.1.vec[1] <- sum(COM==1)/J

## run simulation  
for (i in 1:(num.years-1)) { #for each year in set
  
  freq.1 <- sum(COM==1)/J;  #frequency of spp 1
  freq.2 <- 1 - freq.1   #freq of spp 2 is inversely proportional 
  fit.ratio <- exp(freq.dep*(freq.1-0.5) + log(fit.ratio.avg)) #how far away/past spp 1 is from equib 
  #determines whether it can continue on or bottlenecks, favoring spp 2
  
  Pr.1 <- fit.ratio*freq.1/(fit.ratio*freq.1 + freq.2) #which spp has the upper hand? 
  #if close to 1, spp 1, if close to 0, spp 2
  
  for (k in 1:J) {
    COM[ceiling(J*runif(1))] <- sample(c(1,2), 1, prob=c(Pr.1,1-Pr.1)) #accounting for random selection, 
    #probability that spp 1 will come out on top over spp 2 within a given year? 
    #puts off compounded probabilities/death effects to end of a given year 
  }
  
  ## record data
  freq.1.vec[year] <- sum(COM==1)/J
  year <- year + 1
}

## graph the results
plot1 = plot(1:num.years, freq.1.vec, type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))