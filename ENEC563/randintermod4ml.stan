data {
int patchnum;  # This is the number of patches (the number of levels of the random effect)
int N;         # This is the number of total observations
int patch[N];  # This is the vector with the patch number for each observation
int S[N];      # Observed species richness
vector[N] year2006; # Whether or not the observation was year 2006
vector[N] year2007; # Whether or not the observation was year 2007
vector[patchnum] landscapebaux; #The next three variables are dummy variables for landscape
vector[patchnum] landscapefor;  #note that agriculture is the base leve.
vector[patchnum] landscapeurb;
vector[patchnum] logarea;
}


parameters {
vector[6] beta;      # These are the parameters
vector[patchnum] u0j;  # These are the random effects
real b0;               # This is the overall intercept
real<lower=0> tau;     # This is the variance of the random effects

}

transformed parameters{
vector[N] mu;             # Vector of means
vector[patchnum] beta0j;  # These are the random intercepts

for(j in 1:patchnum){
      beta0j[j]=b0+u0j[j]+beta[3]*landscapebaux[j]+beta[4]*landscapefor[j]+beta[5]*landscapeurb[j]+beta[6]*logarea[j];          # These are the predicted random intercepts 
      }
for(i in 1:N){
mu[i] = exp(beta0j[patch[i]]+beta[1]*year2006[i]+beta[2]*year2007[i]);  #These are the predicted means
}
}

model {
u0j~normal(0,tau);  # This is the distribution of the random effects
S~poisson(mu);      # This is the distribution of the observed richness
}
