## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
freq.mat <- matrix(nrow = num.years, ncol = num.patch)
#collapse into dataframe/single 50x10 matrix since redundant


#collapse into single init val 
J <- 100 # number of individuals PER PATCH
init.1 <- 0.2*J 


COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.1,] <- 1; 
COM[(init.1+1):(0.4*J),] <- 2; 
COM[(0.4*J+1):(0.6*J),] <- 3; 
COM[(0.6*J+1):(0.8*J),] <- 4; 
COM[(0.8*J+1):J,] <- 5

year <- 2 

m <- 0.2
fit.ratio.avg <- c(1:(length =num.patch))
fit.ratio.avg[] <- 1
freq.dep <- c(1:(length=num.patch))
freq.dep[] <- 0


#collapse to be nested 
## record data (frequency of species X) for year 1
freq.mat[1,] <- init.1/J 
#deleted extraneous matrices bc all the same 1st column


#make vectors of to and from outside of loop 
frompatch.id = c(1:(length=num.patch)) # vector of possible emigration patches (each patch has a unique integer for its identity)
topatch.id = c(1:(length=num.patch)) # vector of possible immigration patches (same IDs as above)


## run simulation
for (i in 1:(J*num.patch*(num.years-1))) {
    ## define connectivity for dispersal events 
    patch.prob <- (sample(frompatch.id,1,replace=TRUE)/sample(topatch.id,1,replace=TRUE)) 
  # ratio of em-/immigration patch IDs = 
  # probability of successful dispersal bewtween the two randomly selected patches above 
  # (frompatch.id and topatch.id)
  
    
  ## choose a patch where a death even will occur
  patch <- sample(1:num.patch,1)
  
  for(n in nums){ #outside of if/else bc both dealing with same n's
  ## calculate Pr.n if dispersal occurs
    if (runif(1) < (m*patch.prob)) {  
    # multiplying disperal rate (m) by patch.prob gives final probability of successful dispersal
   
      Pr.n <- sum(COM==n)/(J*num.patch)
      output = data.frame(Pr.n, final_Pr)
    
    } else { 
    
      ## calculate Pr.n if local reproduction (not dispersal)
      Pr.n <- sum(COM[,patch]==n)/J; 
    }
    #need to label and configure output storage
    #can create single df of probabilities as output from loop since running thru them anyway
  }
  
  
  #could do another forloop for n 1:5 and Pr.n of 1:5 since aligned
  COM[ceiling(J*runif(1)),patch] <- sample(n, 1, prob=Pr.n[n])) 
  

  
  ## record data  
  if (i %% (J*num.patch) == 0) {
    freq.mat[year,] <- colSums(COM==n)/J
    year <- year + 1 
  }
} 





#come back to this later
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