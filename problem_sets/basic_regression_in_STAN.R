
#   -----------------------------------------------------------------------
# A basic linear regression in STAN
#   -----------------------------------------------------------------------

# following: 
# STAN users guide: https://mc-stan.org/docs/2_19/stan-users-guide/linear-regression.html
# https://ourcodingclub.github.io/2018/04/17/stan-intro.html


# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(rethinking)

# Define model ------------------------------------------------------------

write("// Stan model for simple linear regression

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
",

"stan_lm.stan")

stanc("stan_lm.stan")


# data --------------------------------------------------------------------

data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,] # subset for adults
d2$weight2 <- d2$weight - mean(d2$weight)

stan_data <- list(N = nrow(d2), x = d2$weight2, y = d2$height)

# fit model ---------------------------------------------------------------

fit <- stan(file="stan_lm.stan", data=stan_data)
fit

