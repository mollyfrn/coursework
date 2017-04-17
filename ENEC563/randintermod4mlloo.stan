data {
int patchnum;
int N;
int patch[N];
int S[N];
vector[N] year2006;
vector[N] year2007;
vector[patchnum] landscapebaux;
vector[patchnum] landscapefor;
vector[patchnum] landscapeurb;
vector[patchnum] logarea;
}


parameters {
vector[6] beta;
vector[patchnum] u0j;
real b0;
real<lower=0> tau;

}

transformed parameters{
vector[N] mu;
vector[patchnum] beta0j;

for(j in 1:patchnum){
      beta0j[j]= b0+u0j[j]+beta[3]*landscapebaux[j]+beta[4]*landscapefor[j]+beta[5]*landscapeurb[j]+beta[6]*logarea[j];
      }
for(i in 1:N){
mu[i] = exp(beta0j[patch[i]]+beta[1]*year2006[i]+beta[2]*year2007[i]);
}
}

model {
u0j~normal(0,tau);
S~poisson(mu);
}

generated quantities {
vector[N] log_lik;
for(i in 1:N)	
      log_lik[i]=poisson_lpmf(S[i]|mu[i]);  # Here I generate the likelihood for this chain.

}
