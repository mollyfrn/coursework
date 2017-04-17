data {
int patchnum;         # number of patches
int N;         	      # number of obs.
int patch[N];	      # patch number for each obs.
int S[N];	      # observed richness 
vector[N] year2006;   # dummy variables for year
vector[N] year2007;
}


parameters {
vector[2] beta;        # coefficient for regression
vector[patchnum] u0j;  # random effects
real b0;	       # intercept
real<lower=0> tau;     # standard deviation for random effects
}

transformed parameters{
vector[N] mu;                # patch mean
vector[patchnum] beta0j;     # intercept + random effect for each patch

for(j in 1:patchnum){
      beta0j[j]=b0+u0j[j];	# calculate intercept for each patch
      }
for(i in 1:N){
mu[i] = exp(beta0j[patch[i]]+beta[1]*year2006[i]+beta[2]*year2007[i]); #calculate conditional mean
}
}

model {
u0j~normal(0,tau);
S~poisson(mu);
}
