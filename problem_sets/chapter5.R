
#   -----------------------------------------------------------------------
# Chapter 5 - multiple regression
#   -----------------------------------------------------------------------

# initialization ----------------------------------------------------------

library(rstan)
options(mc.cores = parallel::detectCores()) # set up to estimate your model in parallel
rstan_options(auto_write = TRUE) # automatically save a bare version of a compiled Stan program to the hard disk so that it does not need to be recompiled

library(bayesplot)
library(dplyr)
library(ggplot2)
library(tidybayes)

# data --------------------------------------------------------------------
# rate of divorce for each state
# scale them all
d <- read.csv("problem_sets/WaffleDivorce.csv")
d$D <- scale(d$Divorce)[,1] 
d$M <- scale(d$Marriage)[,1]
d$A <- scale(d$MedianAgeMarriage)[,1]

# m5.1 ------------------------------------------------------------

write("// 

data {
 int < lower = 1 > N; // sample size
 vector[N] D; // response
 vector[N] A; // predictor
}

parameters {
  real a; // intercept
  real bA; // slope
  real sigma; // error
}

model {
  vector[N] mu;
  mu = a + bA*A;
  D ~ normal(mu, sigma);
  a ~ normal(0,0.2);
  bA ~ normal(0,0.5);
  sigma ~ exponential(1);
}

generated quantities {

// prior predictive check;
  real a_sim = normal_rng(0, 0.2);
  real b_sim = normal_rng(0, 0.5);
  real sigma_sim = exponential_rng(1);
  real y_sim[N] = normal_rng(a_sim + b_sim * A, sigma_sim);  // Simulate from prior

// posterior predictive check;
  vector[N] y_hat;
  for (i in 1:N) {
    y_hat[i] = normal_rng(a + bA * A[i], sigma); // Simulated data
  }
}


",

"stan_m5.1.stan")

stanc("stan_m5.1.stan")

# fit model

stan_data <- list(N = nrow(d), A = d$A, D = d$D)

fit.m5.1 <- rstan::stan(file="stan_m5.1.stan", data=stan_data, chains=3, iter=1000)
fit.m5.1

###### prior check, version 1, simulate from random draws of a and b
# this is just sampling individual params rather jointly estimating. still ok to do this, better than not
# doing it at all

a = rnorm(50, 0, 0.2)
b = rnorm(50, 0, 0.5)
p <- ggplot()
for (i in 1:50) {
  p <- p + geom_abline(slope = b[i], intercept = a[i])
}
p + xlim(-3, 3) + ylim(-3, 3) +
  labs(x = 'Median age marriage (std)',
       y = 'Divorce rate (std)')

###### prior check, version 2, simulate from joint distributions drawn in MCMC
prior_sim <- fit.m5.1 %>% 
  spread_draws(y_sim[i]) 
nrow(prior_sim) # 1500 post-warmup draws * 50 values of A (=N)

prior_sim %>% 
  left_join(d %>% select(A) %>% mutate(i = seq(1:50)),"i") %>% 
  ggplot() +
  geom_point(aes(x=A,y=y_sim)) +
  xlab("Median age marriage (scaled)") +
  ylab("Divorce rate (scaled)")

# diagnostics
mcmc_trace(fit.m5.1, pars=c("a","bA","sigma"))

# plot results
fit.m5.1.sum <- fit.m5.1 %>% 
  spread_draws(a,bA) %>%  #extract posterior samples for intercept and slope into a tibble
  median_qi() # summarize
ypred <- fit.m5.1.sum$a + fit.m5.1.sum$bA*d$A
plot(d$A,d$D, col="skyblue", xlab="Median age marriage (scaled)",ylab="Divorce rate (scaled)")
points(d$A,ypred,type="l")

x_grid <- d %>% tidyr::expand(A = seq(min(A), max(A), length.out = 10))

draws_with_preds <- fit.m5.1 %>%
  spread_draws(a, bA) %>% # sample from posterior, 1500
  cross_join(x_grid) %>%  # 10 samples from actual data
  mutate(mu_hat = a + bA * A)

# this creates a df with 1500 x 10 rows, multiple mu hats per A (observed data)

draws_with_preds %>%
  group_by(A) %>%
  median_qi(mu_hat, .width = c(.89, .95)) %>% 
  ggplot(aes(x = A, y = mu_hat)) +
  geom_point(data=d,aes(x=A,y=D),color="skyblue",pch=1) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper), color = "steelblue",alpha=0.5) + # try to plot without this
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Divorce Rate (std)", x = "Age at Marriage (std)") +
  theme_minimal()

# m5.3 ------------------------------------------------------------

write("// 

data {
 int < lower = 1 > N; // sample size
 vector[N] D; // response
 vector[N] A; // predictor
 vector[N] M; // predictor
}

parameters {
  real a; // intercept
  real bA; // slope
  real bM; // slope
  real sigma; // error
}

model {
  vector[N] mu;
  mu = a + bA*A + bM*M;
  D ~ normal(mu, sigma);
  a ~ normal(0,0.2);
  bA ~ normal(0,0.5);
  bM ~ normal(0,0.5);
  sigma ~ exponential(1);
}

generated quantities {

// prior predictive check;
  real a_sim = normal_rng(0, 0.2);
  real bA_sim = normal_rng(0, 0.5);
  real bM_sim = normal_rng(0, 0.5);
  real sigma_sim = exponential_rng(1);
  real y_sim[N] = normal_rng(a_sim + bA_sim * A + bM_sim * M, sigma_sim);  // Simulate from prior

// posterior predictive check;
  vector[N] y_hat;
  for (i in 1:N) {
    y_hat[i] = normal_rng(a + bA * A[i] + bM * M[i], sigma); // Simulated data
  }
}


",

"stan_m5.3.stan")

stanc("stan_m5.3.stan")

# fit model

stan_data <- list(N = nrow(d), A = d$A, D = d$D, M = d$M)

fit.m5.3 <- rstan::stan(file="stan_m5.3.stan", data=stan_data, chains=3, iter=1000)
fit.m5.3

# diagnostics
mcmc_trace(fit.m5.3, pars=c("a","bA","bM","sigma"))

# posterior predictive check
y_hat_sum <- fit.m5.3 %>%
  spread_draws(y_hat[i]) %>%
  median_qi(.width=0.5)
d$y_hat_median <- y_hat_sum$y_hat
  
fit.m5.3 %>%
  spread_draws(a, bA, bM) %>%
  cross_join(
    d %>% select(A,M,D,Divorce)) %>% 
  mutate(mu_hat = a + bA * A + bM * M) %>% 
  group_by(D) %>% 
  median_qi(mu_hat, .width = c(.89)) %>% 
  ggplot() +
  theme_minimal() +
  geom_abline(aes(intercept = 0, slope = 1), 
              linetype = 'dashed', color = 'gray70') +
  geom_segment(aes(x = D, xend = D, 
                   y = .lower, yend = .upper),
               color = 'dodgerblue') +
  geom_point(data = d,
             aes(D, y_hat_median), 
             shape = 1, color = 'dodgerblue', fill = 'white') +
  labs(x = "Observed Divorce Rate (scaled)", y = 'Estimated Average Divorce Rate')

# plot counterfactualss
draws_with_preds <- fit.m5.3 %>%
  spread_draws(a, bA, bM) %>%
  cross_join(
    d %>% tidyr::expand(A = seq(min(A), max(A), length.out = 10), 
                        M = 0)
    ) %>% 
  mutate(mu_hat = a + bA * A + bM * M)

draws_with_preds %>%
  group_by(A) %>%
  median_qi(mu_hat, .width = c(.89, .95)) %>% 
  ggplot(aes(x = A, y = mu_hat)) +
  geom_point(data=d,aes(x=A,y=D),color="skyblue",pch=1) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper), color = "steelblue",alpha=0.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Divorce Rate (std)", x = "Age at Marriage (std)") +
  theme_minimal()

draws_with_preds <- fit.m5.3 %>%
  spread_draws(a, bA, bM) %>%
  cross_join(
    d %>% tidyr::expand(A = 0, 
                        M = seq(min(M), max(M), length.out = 10))
  ) %>% 
  mutate(mu_hat = a + bA * A + bM * M)

draws_with_preds %>%
  group_by(M) %>%
  median_qi(mu_hat, .width = c(.89, .95)) %>% 
  ggplot(aes(x = M, y = mu_hat)) +
  geom_point(data=d,aes(x=M,y=D),color="skyblue",pch=1) +
  geom_lineribbon(aes(ymin = .lower, ymax = .upper), color = "steelblue",alpha=0.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Divorce Rate (std)", x = "Marriage Rate (scaled)") +
  theme_minimal()
