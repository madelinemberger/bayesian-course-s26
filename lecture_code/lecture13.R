
#   -----------------------------------------------------------------------
# simulating data example
#   -----------------------------------------------------------------------

# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(bayesplot)
library(dplyr)
library(ggplot2)
library(tidybayes)

# simulate data ------------------------------------------------------------

# We counted fish inside and outside of an MPA and want to know if fish biomass is higher inside than outside
# lets simulate the experiment

n <- 100 # number of transects (per group)
mean_outside <- 1.3 # (log biomass)
mean_inside <- 2*mean_outside
sd_biomass <- 1 # assume equal variance

data <- data.frame(
  in_out = c(rep("1_in",n),rep("2_out",n)),
  log_biomass = c(
    rnorm(n,mean_inside,sd_biomass),
    rnorm(n,mean_outside,sd_biomass)
  )
)

# difference in mean by gender --------------------------------------------

write("// Stan model for regression with categories

data {
 int < lower = 1 > N; // sample size
 vector[N] y; // response
 int x[N]; // predictor
 int < lower = 1 > J; // number of categories
}

parameters {
  vector[J] a; // slope(s)
  real < lower = 0 > sigma; // error
}

model {
  vector[N] mu;
  mu = a[x];
  y ~ normal(mu, sigma);
  for(j in 1:J){
    a[j] ~ lognormal(2,1);
  }
  sigma ~ exponential(1);
}

generated quantities {
  // prior predictive check;
  vector[J] a_sim;
  for(j in 1:J){
    a_sim[j] = lognormal_rng(2,1);
  }
  real sigma_sim = exponential_rng(1);
  real y_sim[N];
  for(i in 1:N){
    y_sim[i] = normal_rng(a_sim[x[i]], sigma_sim);  // Simulate from prior
  }
  
  // posterior predictive check;
  vector[N] y_hat;
  for (i in 1:N) {
    y_hat[i] = normal_rng(a[x[i]], sigma); // Simulated data
  }

} 
",

"stan_lm_cat.stan")

# compile
stanc("stan_lm_cat.stan")

# format data
data$X <- as.numeric(as.factor(data$in_out))
stan_data <- list(N = nrow(data), J = length(unique(data$X)), x = data$X, y = data$log_biomass)

# fit
fit_inout <- stan(file="stan_lm_cat.stan", data=stan_data)
fit_inout

# convergence 
names(fit_inout)
mcmc_trace(fit_inout, pars=c("a[1]","a[2]","sigma"))

###### prior check
prior_sim <- fit_inout %>% 
  spread_draws(y_sim[i]) 
nrow(prior_sim) # 1500 post-warmup draws * 50 values of A (=N)

prior_sim %>% 
  left_join(data %>% select(in_out) %>% mutate(i=seq(1:nrow(data))), by="i") %>% 
  ggplot() +
  geom_density(aes(x=y_sim,color=as.factor(in_out))) + 
  facet_wrap(~in_out)

###### posterior predictive check
y_hat_sum <- fit_inout %>%
  spread_draws(y_hat[i]) %>%
  median_qi(.width=0.5)
data$y_hat_median <- y_hat_sum$y_hat

plot(data$log_biomass,data$y_hat_median)

###### summarize posterior
a.draws <- fit_inout %>% 
  spread_draws(a[i]) 

s <- a.draws %>%
  median_qi(.width=0.89) # summarize

ggplot() +
  geom_density(data=a.draws %>% mutate(group=as.factor(i)), aes(x=a,color=group)) +
  geom_vline(aes(xintercept=mean_inside)) + 
  geom_vline(aes(xintercept=mean_outside))
