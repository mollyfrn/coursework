data {
int patchnum;
int N;
int patch[N];
int S[N];
vector[N] year2006;
vector[N] year2007;
}


parameters {
vector[2] beta;
vector[patchnum] intercept;
}

transformed parameters{
vector[N] mu;
for(i in 1:N){
mu[i] = exp(intercept[patch[i]]+beta[1]*year2006[i]+beta[2]*year2007[i]);
}
}

model {

S~poisson(mu);
}