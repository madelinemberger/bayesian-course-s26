
#   -----------------------------------------------------------------------
# Chapter 4
#   -----------------------------------------------------------------------

# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(rethinking)
library(bayesplot)
library(dplyr)
library(ggplot2)
library(tidybayes)

# data --------------------------------------------------------------------

data("Howell1")
d <- Howell1
d2 <- d[d$age >= 18,] # subset for adults

# m4.1 ------------------------------------------------------------

write("// 

data {
 int < lower = 1 > N; // sample size
 vector[N] h; // response
}

parameters {
  real mu; // mean
  real < lower = 0 > sigma; // error
}

model {
  h ~ normal(mu, sigma);
  mu ~ normal(178,20);
  sigma ~ uniform(0,50);
}

generated quantities {
} 
",

"stan_m4.1.stan")

stanc("stan_m4.1.stan")


# fit model

stan_data <- list(N = nrow(d2), h = d2$height)

fit.m4.1 <- rstan::stan(file="stan_m4.1.stan", data=stan_data, chains=3, iter=1000)
fit.m4.1

# diagnostics

mcmc_trace(fit.m4.1, pars=c("mu","sigma"))


# m4.3 ------------------------------------------------------------

write("// 

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

"stan_m4.3.stan")

stanc("stan_m4.3.stan")


# fit model

d2$weight2 <- d2$weight - mean(d2$weight)

stan_data <- list(N = nrow(d2), x = d2$weight2, y = d2$height)

fit.m4.3 <- rstan::stan(file="stan_m4.3.stan", data=stan_data, chains=3, iter=1000)
fit.m4.3

# diagnostics

mcmc_trace(fit.m4.3, pars=c("alpha","beta","sigma"))

# plot results
fit.m4.3.sum <- fit.m4.3 %>% 
  spread_draws(alpha,beta) %>%  #extract posterior samples for intercept and slope into a tibble
  median_qi() # summarize
ypred <- fit.m4.3.sum$alpha + fit.m4.3.sum$beta*d2$weight2
plot(height ~ d2$weight2, data=d2 , col=rangi2, xlab="weight")
points(d2$weight2,ypred,type="l")
