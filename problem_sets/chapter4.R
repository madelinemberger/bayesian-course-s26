
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
library(tidyverse)

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
  vector[N] y_hat;
  for(i in 1:N){
  y_hat[i] = normal_rng(alpha + beta * x[i], sigma); // simulated data, this is where we account for sigma
  }
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
  median_qi() # summarize, get the upper lower and median of each parameter
ypred <- fit.m4.3.sum$alpha + fit.m4.3.sum$beta*d2$weight2
plot(height ~ d2$weight2, data=d2 , col=rangi2, xlab="weight")
points(d2$weight2,ypred,type="l")


# look at lines
ggplot()+
  geom_point(data=d2, aes(x=weight2, y=height))+
  geom_abline(
    data = post %>% sample_n(10),
    aes(intercept = alpha,
        slope = beta),
    alpha = 0.5, color = "skyblue"
  )

# predict mean at a certain place, what is the average height if x is a certain weight?

mu_at_50 <- post$alpha + post$beta*(50 - mean(d2$weight)) # 1500 estimates of height at X
plot(density(mu_at_50))

# Or, could predict every mu
# first create a sequence of x you are interested in, which will be on the horizontal axis

# for every value of x, create a sequence. now we have 1500 x 100 = 150000 rows 
# for each new value of x, what are the posterior lines you could draw with the 1500 alphas and betas?
x_grid <- d2 %>% 
  expand(weight2 = seq(min(weight2), max(weight2), length.out = 100))

draws_with_preds <- fit.m4.3 %>% 
  spread_draws(alpha, beta) %>% 
  cross_join(x_grid) %>%
  mutate(mu_hat = alpha + beta * weight2) # plugging in each x to each alpha and beta to get a y hat

# we made a mini posterior for each value x, 1500 mu hats
ggplot(draws_with_preds,
       aes(x = weight2, y = y_hat, group = .draw))+
  geom_point(alpha = 0.5, color = "coral")

# but where is sigma? its a generated quantity! stan does it for you as you sample the mcmc
# we need sigma to estimate y hat


  