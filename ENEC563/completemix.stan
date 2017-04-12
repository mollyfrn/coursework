data {
int N;
int S[N];
vector[N] year2006;
vector[N] year2007;
}


parameters {
vector[3] beta;
}

transformed parameters{
vector[N] mu;
mu =exp(beta[1]+beta[2]*year2006+beta[3]*year2007);
}

model {
S~poisson(mu);
}