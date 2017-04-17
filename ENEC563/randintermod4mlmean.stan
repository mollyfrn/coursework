data {
int patchnum;                      #number of patches
int N;				   #number of observations
int patch[N];			   #patch number of each observation
int S[N];			   #observed richness
vector[N] year2006;		   #dummy variables for year
vector[N] year2007;
vector[patchnum] landscapebaux;	   #dummy variables for landscape
vector[patchnum] landscapefor;
vector[patchnum] landscapeurb;
vector[patchnum] logarea;	   #log of area
}


parameters {
vector[6] beta;			  #parameter estimates
vector[patchnum] u0j;		  #random effects
real b0;	 		  #intercept
real<lower=0> tau;		  #standard deviate of random effect

}

transformed parameters{
vector[N] mu;			  #conditional mean of prediction
vector[patchnum] beta0j;	  #random slope for each patch

for(j in 1:patchnum){
      beta0j[j]=b0+u0j[j]+beta[3]*landscapebaux[j]+beta[4]*landscapefor[j]+beta[5]*landscapeurb[j]+beta[6]*logarea[j];
      }
for(i in 1:N){
mu[i]=exp(beta0j[patch[i]]+beta[1]*year2006[i]+beta[2]*year2007[i]);
}
}

model {
u0j~normal(0,tau);
S~poisson(mu);
}

generated quantities {  # Here I generate a predicted mean for each of the landscape categories
real muagr;
real mubaux;
real mufor;
real muurb;

muagr=exp(b0);          #mean for agriculture
mubaux=exp(b0+beta[3]); #mean for bauxite
mufor=exp(b0+beta[4]);  #mean for forest
muurb=exp(b0+beta[5]);  #mean for urban
}