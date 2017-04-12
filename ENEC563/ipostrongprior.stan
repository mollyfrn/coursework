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
beta[1]~normal(0,20);
beta[2]~normal(0,20);
beta[3]~normal(0,20);

fruit~normal(beta[1]+beta[2]*root+beta[3]*graze,sigma);
}