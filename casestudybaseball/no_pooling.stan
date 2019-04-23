data {
  int<lower=0> N; // items
  int<lower=0> K[N]; // initial trials
  int<lower=0> y[N]; // initial sucesses

  int<lower=0> K_new[N]; // new trials
  int<lower=0> y_new[N]; // new successes

  }

parameters {
  vector<lower = 0, upper = 1>[N] theta; // chance of success (pooled)
}

model {
  y ~ binomial(K, theta);
}
