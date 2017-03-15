
## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
#collapse into single init val 
J <- 100 # number of individuals PER PATCH
init <- 0.2*J 

freq.1.mat <- matrix(init/J, nrow = num.years, ncol = num.patch) #find way to collapse 
freq.2.mat <- matrix(init/J, nrow = num.years, ncol = num.patch)
freq.3.mat <- matrix(init/J, nrow = num.years, ncol = num.patch)
freq.4.mat <- matrix(init/J, nrow = num.years, ncol = num.patch)
freq.5.mat <- matrix(init/J, nrow = num.years, ncol = num.patch)
#each row is a year 


COM <- matrix(nrow=J, ncol=num.patch)
COM[1:init.n,] <- 1; 
COM[(init.n+1):(0.4*J),] <- 2; 
COM[(0.4*J+1):(0.6*J),] <- 3; 
COM[(0.6*J+1):(0.8*J),] <- 4; 
COM[(0.8*J+1):J,] <- 5

year <- 2 
m <- 0.2

## run simulation
for (i in 1:(J*num.patch*(num.years-1))) { #only a single number within a simulation ahhhhh!!!
  Pr_vec = c()
  speciesnum = c(1:5)
  for(n in speciesnum){
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
      } else { 
          ## calculate Pr.n if local reproduction (not dispersal)
      Pr.n <- sum(COM[,patch]==n)/J; 
    }
    
    Pr_vec = c(Pr_vec, Pr.n) #now just need to vary probabilities bc not for some reason
  } #end of speciesnum forloop 
  
  
    #need to label and configure output storage
    #can create single df of probabilities as output from loop since running thru them anyway
    COM[ceiling(J*runif(1)),patch] <- sample(c(1:5), 1, prob=c(Pr_vec[1], 
                                                                   Pr_vec[2], 
                                                                   Pr_vec[3], 
                                                                   Pr_vec[4], 
                                                                   Pr_vec[5])) 
    
    ## record data as discrete matrices 
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
  lines(1:num.years,freq.1.mat[,i], type="l", col=rainbow(num.patch)[i] , ylim=c(0,1))
}

plot(1:num.years, freq.2.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 2", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.2.mat[,i], type="l", col=rainbow(num.patch)[i], ylim=c(0,1))
}
plot(1:num.years, freq.3.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 3", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.3.mat[,i], type="l", col=rainbow(num.patch)[i], ylim=c(0,1))
}
plot(1:num.years, freq.4.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 4", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.4.mat[,i], type="l", col=rainbow(num.patch)[i], ylim=c(0,1))
}
plot(1:num.years, freq.5.mat[,1], type="l", xlab="Time", 
     ylab="Frequency of species 5", ylim=c(0,1))
for (i in 2:(num.patch)) {
  lines(1:num.years,freq.5.mat[,i], type="l", col=rainbow(num.patch)[i], ylim=c(0,1))
}
