// Stan model for globe toss

data {
 int < lower = 0 > N; // number of tosses
 int < lower = 0, upper = N > W; // times we land on water
}

parameters {
  real < lower = 0, upper = 1 > p; // probability of water
}

model {
  p ~ uniform(0,1); // prior
  W ~ binomial(N,p); // likelihood
}


