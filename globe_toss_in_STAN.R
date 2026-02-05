
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

library(bayesplot)
library(tidybayes)

# Define model ------------------------------------------------------------

write("// Stan model for globe toss

data {
 int < lower = 0 > N; // number of tosses
 int < lower = 0, upper = N > W; // times we land on water
}

parameters {
  real < lower = 0, upper = 1 > p; // probability of water
}

model {
  p ~ uniform(0,1); // prior
  W ~ binomial(N, p); // likelihood
}

",

"stan_globe_toss.stan")

stanc("stan_globe_toss.stan") # compile Stan code in C++


# data --------------------------------------------------------------------

stan_data <- list(N = 10, W = 6)

# fit model ---------------------------------------------------------------

fit <- rstan::stan(file="stan_globe_toss.stan", data=stan_data, 
                   chains=4, cores=4, iter = 1000,
                   warmup = 500)
fit


# diagnostics -------------------------------------------------------------

mcmc_trace(fit, pars="p")

# posterior visuals and summaries -----------------------------------

fit %>% spread_draws(p) # extract posterior samples for p into a tibble

fit %>% spread_draws(p) %>% 
  ggplot(aes(x=p))+
  stat_halfeye() # show posterior draw graph

fit %>% spead_draws(p) %>% 
  median_qi() # quantiles
