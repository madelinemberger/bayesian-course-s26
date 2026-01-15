// Stan model for simple linear regression

data {
 int < lower = 1 > N; // sample size
 vector[N] y; // response
 vector[N] x; // predictor
}

parameters {
  real alpha; // intercept
  real < lower = 0 > beta; // slope
  real < lower = 0, upper = 50 > sigma; // error
}

model {
  vector[N] mu;
  mu = alpha + beta*x;
  y ~ normal(mu, sigma);
  alpha ~ normal(178,20);
  beta ~ lognormal(0,1);
  sigma ~ uniform(0,50);
}

generated quantities {
} 

