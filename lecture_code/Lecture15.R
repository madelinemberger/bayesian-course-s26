
#   -----------------------------------------------------------------------
# Hominen brain size - Bayesian Workflow
#   -----------------------------------------------------------------------

# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(bayesplot)
library(dplyr)
library(ggplot2)
library(tidybayes)
library(loo)


# data --------------------------------------------------------------------

d <- tibble(sppnames = c("afarensis", "africanus", "habilis", "boisei", "rudolfensis", "ergaster", "sapiens"),
            brainvolcc = c( 438 , 452 , 612, 521, 752, 871, 1350 ),
            masskg = c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 )) %>%
  mutate(brain_std = brainvolcc / max(brainvolcc),
         mass_std1 = (masskg - mean(masskg)) / sd(masskg),
         mass_std2 = mass_std1^2,
         mass_std3 = mass_std1^3,
         mass_std4 = mass_std1^4,
         mass_std5 = mass_std1^5,
         mass_std6 = mass_std1^6)


# m7.1 --------------------------------------------------------------------

write("// Stan model for hominen brain size

data {
 int < lower = 1 > N; // sample size
 vector[N] y; // response
 vector[N] m; // predictor
}

parameters {
  real alpha; // intercept
  real beta_1; // slope
  real log_sigma; // error
}

transformed parameters {
  vector[N] mu;
  mu = alpha + beta_1*m;
}

model {
  y ~ normal(mu, exp(log_sigma));
  alpha ~ normal(0.5,1);
  beta_1 ~ normal(0,10);
  log_sigma ~ lognormal(0,1);
}

generated quantities {
  // prior predictive check;
  real alpha_sim;
  alpha_sim = normal_rng(0.5,1);

  real beta_sim;
  beta_sim = normal_rng(0,10);
  
  real sigma_sim;
  sigma_sim = lognormal_rng(0,1);
  
  real y_sim[N];
  for(i in 1:N){
    y_sim[i] = normal_rng(alpha_sim + beta_sim*m[i], exp(sigma_sim));  // Simulate from prior
  }

  // posterior predictive check;
  vector[N] log_lik;
  vector[N] y_hat;
  for (i in 1:N) {
    log_lik[i] = normal_lpdf(y[i] | mu[i], exp(log_sigma));
    y_hat[i] = normal_rng(mu[i],exp(log_sigma));
  }

} 

",

"stan_hominan1.stan")

stanc("stan_hominan1.stan")

stan_data <- list(N = nrow(d), y=d$brain_std, m=d$mass_std1)

m7.1 <- stan(file="stan_hominan1.stan", data=stan_data)
m7.1 # note divergent transitions, low n_eff for sigma and lp
pairs(m7.1, pars=c("alpha","beta_1","log_sigma")) # red dots will tell you divergent transitions. THESE ARE BAD

# convergence 
names(m7.1)
mcmc_trace(m7.1, pars=c("alpha","beta_1","log_sigma"))

# prior check
m7.1 %>% 
  spread_draws(y_sim[i]) %>% 
  ggplot() +
  geom_density(aes(x=y_sim)) 

m7.1 %>% 
  spread_draws(sigma_sim) %>% 
  ggplot() +
  geom_density(aes(x=sigma_sim)) 

# posterior checks
log_lik_1 <- extract_log_lik(m7.1, merge_chains = FALSE)
r_eff_1=relative_eff(log_lik_1) # A vector of relative effective sample sizes from the MCMC
loo_1 <- loo(log_lik_1, r_eff=r_eff_1, save_psis = T)
loo_1

# check for influential observations
plot(loo_1)

# save posterior predictions
y_hat <- extract(m7.1)["y_hat"]

# posterior check
bayesplot::ppc_loo_pit_overlay(d$brain_std, as.array(y_hat)$y_hat, lw = weights(loo_1$psis_object))
