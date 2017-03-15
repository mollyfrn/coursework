
speciesnum = c(1:5)
freq.mat = list()
  
for(n in speciesnum){ #outside of workings bc both dealing with same n's

## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
temp.mat.n <- matrix(nrow = num.years, ncol = num.patch) #each row is a year 
#collapse into dataframe/single 50x10 matrix since redundant


#collapse into single init val 
J <- 100 # number of individuals PER PATCH
init.n <- 0.2*J 


COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.n,] <- 1; 
COM[(init.n+1):(0.4*J),] <- 2; 
COM[(0.4*J+1):(0.6*J),] <- 3; 
COM[(0.6*J+1):(0.8*J),] <- 4; 
COM[(0.8*J+1):J,] <- 5

year <- 2 

m <- 0.2

#collapse to be nested 
## record data (frequency of species X) for year 1
temp.mat.n[1,] <- init.n/J 
#deleted extraneous matrices bc all the same 1st column



#I think this patch.prob needs to be max 1 otherwise there is a chance that m*patch.prob is over 1?


## run simulation
for (i in 1:(J*num.patch*(num.years-1))) {
    ## choose a patch where a death even will occur
    #make vectors of to and from outside of loop 
    from.patches=c(1:num.patch)
    patch <- sample(1:num.patch,1) #patch where death occurs
    from.patch.sample<-sample(from.patches, 1, replace=TRUE)
    patch.prob<-min(patch, from.patch.sample)/max(patch, from.patch.sample)
  
  
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

  
  
  #this needs to be outside the loop; loop can only go as far as generating vector of 5 probabilities 
  COM[ceiling(J*runif(1)),patch] <- sample(speciesnum, 1, prob=Pr.n) ###this is where the problems are happening 
  

  
  ## record data  -> FIX OUTPUT
  if (i %% (J*num.patch) == 0) {
    temp.mat.n[year,] <- colSums(COM==n)/J
    year <- year + 1 
  }
  output = list(temp.mat.n, freq.mat)
} 

}

#come back to this later
## graph the results
par(mfrow=c(2,3))
plot(1:num.years, freq.mat.n[,1], type="l", xlab="Time", 
     ylab=paste("Frequency of species", n, sep = ""), ylim=c(0,1))
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
