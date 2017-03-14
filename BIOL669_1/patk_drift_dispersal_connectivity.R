## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
freq.1.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.2.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.3.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.4.mat <- matrix(nrow = num.years, ncol = num.patch)
freq.5.mat <- matrix(nrow = num.years, ncol = num.patch)


J <- 100 # number of individuals PER PATCH
init.1 <- 0.2*J 
init.2 <- 0.2*J
init.3 <- 0.2*J
init.4 <- 0.2*J
init.5 <- 0.2*J
COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.1,] <- 1; COM[(init.1+1):(0.4*J),] <- 2; COM[(0.4*J+1):(0.6*J),] <- 3; COM[(0.6*J+1):(0.8*J),] <- 4; COM[(0.8*J+1):J,] <- 5

year <- 2 

m <- 0.2
fit.ratio.avg <- vector(length=num.patch)
fit.ratio.avg[] <- 1
freq.dep <- vector(length=num.patch)
freq.dep[] <- 0

## record data (frequency of species X) for year 1
freq.1.mat[1,] <- init.1/J 
freq.2.mat[1,] <- init.2/J 
freq.3.mat[1,] <- init.3/J 
freq.4.mat[1,] <- init.4/J 
freq.5.mat[1,] <- init.5/J 

## run simulation
for (i in 1:(J*num.patch*(num.years-1))) {
  
  ## define connectivity for dispersal events 
  frompatch.id = c(1,2,3,4,5,6,7,8,9,10) # vector of possible emigration patches (each patch has a unique integer for its identity)
  topatch.id = c(1,2,3,4,5,6,7,8,9,10) # vector of possible immigration patches (same IDs as above)
  patch.prob <- (sample(frompatch.id,1,replace=TRUE)/sample(topatch.id,1,replace=TRUE)) # ratio of em-/immigration patch IDs = 
  # probability of successful dispersal bewtween the two randomly selected patches above (frompatch.id and topatch.id)
  
  ## choose a patch where a death even will occur
  patch <- sample(1:num.patch,1)
  
  ## calculate Pr.X if dispersal occurs
  if (runif(1) < (m*patch.prob)) {  # multiplying disperal rate (m) by patch.prob gives final probability of successful dispersal
    Pr.1 <- sum(COM==1)/(J*num.patch)
    Pr.2 <- sum(COM==2)/(J*num.patch)
    Pr.3 <- sum(COM==3)/(J*num.patch)
    Pr.4 <- sum(COM==4)/(J*num.patch)
    Pr.5 <- sum(COM==5)/(J*num.patch)
  } else { 
    
    ## calculate Pr.X if local reproduction (not dispersal)
    freq.1 <- sum(COM[,patch]==1)/J; freq.2 <- sum(COM[,patch]==2)/J; freq.3 <- sum(COM[,patch]==3)/J; freq.4 <- sum(COM[,patch]==4)/J; freq.5 <- sum(COM[,patch]==5)/J
    Pr.1 <- freq.1; Pr.2 <- freq.2; Pr.3 <- freq.3; Pr.4 <- freq.4; Pr.5 <- freq.5
  }
  
  COM[ceiling(J*runif(1)),patch] <- sample(c(1,2,3,4,5), 1, prob=c(Pr.1,Pr.2,Pr.3,Pr.4,Pr.5)) 
  
  ## record data  
  if (i %% (J*num.patch) == 0) {
    freq.1.mat[year,] <- colSums(COM==1)/J
    freq.2.mat[year,] <- colSums(COM==2)/J
    freq.3.mat[year,] <- colSums(COM==3)/J
    freq.4.mat[year,] <- colSums(COM==4)/J
    freq.5.mat[year,] <- colSums(COM==5)/J
    year <- year + 1 
  }
} 

## graph the results
par(mfrow=c(2,3))
plot(1:num.years, freq.1.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 1", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.1.mat[,2], type="l", col="red", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,3], type="l", col="blue", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,4], type="l", col="green", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,5], type="l", col="yellow", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,6], type="l", col="purple", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,7], type="l", col="orange", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,8], type="l", col="gray", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,9], type="l", col="brown", ylim=c(0,1))
  lines(1:num.years,freq.1.mat[,10], type="l", lty=2, ylim=c(0,1))
}
plot(1:num.years, freq.2.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 2", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.2.mat[,2], type="l", col="red", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,3], type="l", col="blue", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,4], type="l", col="green", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,5], type="l", col="yellow", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,6], type="l", col="purple", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,7], type="l", col="orange", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,8], type="l", col="gray", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,9], type="l", col="brown", ylim=c(0,1))
  lines(1:num.years,freq.2.mat[,10], type="l", lty=2, ylim=c(0,1))
}
plot(1:num.years, freq.3.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 3", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.3.mat[,2], type="l", col="red", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,3], type="l", col="blue", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,4], type="l", col="green", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,5], type="l", col="yellow", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,6], type="l", col="purple", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,7], type="l", col="orange", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,8], type="l", col="gray", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,9], type="l", col="brown", ylim=c(0,1))
  lines(1:num.years,freq.3.mat[,10], type="l", lty=2, ylim=c(0,1))
}
plot(1:num.years, freq.4.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 4", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.4.mat[,2], type="l", col="red", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,3], type="l", col="blue", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,4], type="l", col="green", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,5], type="l", col="yellow", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,6], type="l", col="purple", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,7], type="l", col="orange", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,8], type="l", col="gray", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,9], type="l", col="brown", ylim=c(0,1))
  lines(1:num.years,freq.4.mat[,10], type="l", lty=2, ylim=c(0,1))
}
plot(1:num.years, freq.5.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 5", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.5.mat[,2], type="l", col="red", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,3], type="l", col="blue", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,4], type="l", col="green", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,5], type="l", col="yellow", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,6], type="l", col="purple", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,7], type="l", col="orange", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,8], type="l", col="gray", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,9], type="l", col="brown", ylim=c(0,1))
  lines(1:num.years,freq.5.mat[,10], type="l", lty=2, ylim=c(0,1))
}