
## specify parameters, initial conditions, and output matrix
num.years <- 50 
num.patch <- 10
#collapse into single init val 
J <- 100 # number of individuals PER PATCH
init <- 0.2*J 

n = c(1:5)

#find way to pull into 5 discrete matrices instead of them being bound by rows
mat = replicate(5, matrix(init/J, nrow = num.years, ncol = num.patch))
freq.1.mat = mat[,,1]; freq.2.mat = mat[,,2]; freq.3.mat = mat[,,3]; 
freq.4.mat = mat[,,4]; freq.5.mat = mat[,,5]


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

matrices = list(freq.1.mat, freq.2.mat, freq.3.mat, freq.4.mat, freq.5.mat)

for(t in matrices){
## graph the results
  test = matrices[t]
par(mfrow=c(2,3))
plot(1:num.years, t[,1], type="l", xlab="Time", 
     ylab=paste("Frequency of species in", t, sep = ""), ylim=c(0,1))
  for (i in 2:(num.patch)) {
    lines(1:num.years,m[,i], type="l", col=rainbow(num.patch)[i] , ylim=c(0,1))
  }
}