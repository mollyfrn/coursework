data {
int N;
vector[N] fruit;
vector[N] root;
vector[N] graze;
}

parameters {
vector[3] beta;
real<lower=0> sigma;
}
model {

fruit~normal(beta[1]+beta[2]*root+beta[3]*graze,sigma);
}