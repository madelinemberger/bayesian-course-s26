
#   -----------------------------------------------------------------------
# Binomial regression
#   -----------------------------------------------------------------------

# Assignment: 
# This is code for generating data under a binomial model and the stan code for fitting a logistic regression 
# for the probability of getting accepted to graduate school as a function of GPA and GRE scores. 
# Add to the code the 
# 1) prior check(s), 2) convergence check(s), 3) posterior check(s), and 4) visualization(s) and interpretation(s) of 
# the results that you think are appropriate for the case. Upload your expanded R code. 

# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(bayesplot)
library(dplyr)
library(ggplot2)
library(tidybayes)
library(loo)


# generate binomial data --------------------------------------------------

N <- 100

# predictors
gre <- rnorm(N,0,1)
gpa <- rnorm(N,0,1)

# log-odds of getting accepted
logit_p <- -1 + 0.2*gre + 0.8*gpa

# generate probabilities under binomial model
p <- 1/(1 + exp(-logit_p))
summary(p)

# simulate response variable
y <- rbinom(N,1,p)

plot(gre,p)
plot(gpa,p)


# logistic regression -----------------------------------------------------

write("
data { 
  int < lower=1 > N;
  int y[N]; // response
  vector[N] gre; // predictor
  vector[N] gpa; // predictor

}

parameters {
  real alpha;
  real beta_gre;
  real beta_gpa;
}

model {
  y ~ bernoulli_logit(alpha + beta_gre*gre + beta_gpa*gpa);
  alpha ~ normal(0,1);
  beta_gre ~ normal(0, 2);
  beta_gpa ~ normal(0, 2);
}

generated quantities {
  real alpha_sim = normal_rng(0, 1);
  real beta_gre_sim = normal_rng(0, 2);
  real beta_gpa_sim = normal_rng(0, 2);
  real y_sim[N] = bernoulli_logit_rng(alpha_sim + beta_gre_sim*gre + beta_gpa_sim*gpa);  // Simulate from prior

  array[N] int y_rep;
  vector[N] logit_p = alpha + beta_gre*gre + beta_gpa*gpa;
  for (i in 1:N) {
    y_rep[i] = bernoulli_logit_rng(logit_p[i]);
  }
}

      ", "stan_logistic.stan")

stanc("stan_logistic.stan")

stan_data <- list(N=N, y=y, gre=gre, gpa=gpa)

fit <- stan(file="stan_logistic.stan", data=stan_data) # default is 4 chains

## prior check - what would the priors predict?

data_df <- data.frame(
  N = seq(1:100),
  gre = gre,
  gpa = gpa
)


index_draws_array <- as.array(fit)

prior_sim <- index_draws_array %>% 
  posterior::as_draws_df() %>% 
  spread_draws(y_sim[N],
               beta_gpa_sim,
               beta_gre_sim,
               alpha_sim) %>%
  left_join(data_df, by = "N") %>% # the length should reflect 100 data points x 4 chains x 1000 iterations per chain
  mutate(p = 1 / (1 + exp(-(alpha_sim + beta_gre_sim * gre + beta_gpa_sim * gpa)))) # calculate the probability of getting in using inverse logit

prior_sim %>% 
  left_join(data_df,by ="N") %>% 
  ggplot() +
  geom_point(aes(x=gpa,y=y_sim), color = "aquamarine3") +
  xlab("GPA") +
  ylab("Acceptance")+
  theme_bw() 

prior_sim %>% 
  ggplot() +
  geom_point(aes(x=gre,y=y_sim), color = "dodgerblue") +
  xlab("GRE Score") +
  ylab("Acceptance")+
  theme_bw() 

### graph of probability
prior_sim %>%
  filter(.draw %in% sample(unique(.draw), 50)) %>% 
  ggplot(aes(x = gpa, y = p, group = .draw)) +
  geom_line(alpha = 0.3, color = "firebrick") +
  ylim(0, 1) +
  labs(title = "Prior Predictive Check: Probabilities",
       subtitle = "Sample of 50 possible regression lines based on priors") +
  theme_bw()

prior_sim %>%
  filter(.draw %in% sample(unique(.draw), 50)) %>% 
  ggplot(aes(x = gre, y = p, group = .draw)) +
  geom_line(alpha = 0.3, color = "firebrick2") +
  ylim(0, 1) +
  labs(title = "Prior Predictive Check: Probabilities of acceptance based on GRE",
       subtitle = "Sample of 50 possible regression lines based on priors") +
  theme_bw()

# conclusion: these priors are not very informative and all over the place

## convergence

mcmc_trace(fit, pars = c("alpha","beta_gre", "beta_gpa"))

## posterior checks - compare observed data to replicate data

posterior_reps <- fit %>% 
  spread_draws(y_rep[N]) # get the posterior data

observed_prop <- mean(y) # what proportion were accepted in the raw data

# did the posterior predict the number of acceptanaces?
posterior_reps %>%
  group_by(.draw) %>%
  summarize(prop_ones = mean(y_rep)) %>%
  ggplot(aes(x = prop_ones)) +
  geom_histogram(fill = "aquamarine3", color = "white", bins = 30) +
  geom_vline(xintercept = observed_prop, color = "coral", size = 1.5) +
  labs(title = "Proportion of acceptances - observed and predicted",
       subtitle = "orange line = observed proportion, histogram = model predictions") +
  theme_minimal()

## visualize results - plot a fitted curve holding each of the predictors at their means
library(tidyr)
# seq of possible gpa and gres as grids
gpa_grid <- data.frame(gpa_seq = seq(min(gpa), max(gpa), length.out = 100))
gre_grid <- data.frame(gre_seq = seq(min(gre), max(gre), length.out = 100))

results_curve_1 <- fit %>% 
  spread_draws(alpha, beta_gre, beta_gpa) %>% 
  sample_n(100) %>% 
  crossing(gpa_grid) %>% 
  mutate(p = plogis(alpha + beta_gre*mean(gre) + beta_gpa * gpa_seq)) %>% 
  ggplot(aes(x = gpa_seq, y = p)) +
  geom_line(aes(group = .draw), alpha = 0.1, color = "aquamarine3") +
  geom_point(data = data_df, aes(x = gpa, y = y), 
             alpha = 0.5, position = position_jitter(height = 0.02)) +
  labs(title = "Posterior fit: prob of acceptance vs GPA",
       subtitle = "holding at GRE mean",
       y = "GPA") +
  theme_minimal()
  
results_curve_1

results_curve_2 <- fit %>% 
  spread_draws(alpha, beta_gre, beta_gpa) %>% 
  sample_n(100) %>% 
  crossing(gre_grid) %>% 
  mutate(p = plogis(alpha + beta_gre*gre_seq) + beta_gpa * mean(gpa)) %>% 
  ggplot(aes(x = gre_seq, y = p)) +
  geom_line(aes(group = .draw), alpha = 0.1, color = "dodgerblue3") +
  geom_point(data = data_df, aes(x = gre, y = y), 
             alpha = 0.5, position = position_jitter(height = 0.02)) +
  labs(title = "Posterior fit: prob of acceptance vs GRE",
       subtitle = "holding at GPA mean",
       y = "GRE Score") +
  theme_minimal()

results_curve_2
  
# results: GPA has a stronger relationship with acceptance. 
# not sure how to get the intervals for this one? but can kind of see with the lines
  