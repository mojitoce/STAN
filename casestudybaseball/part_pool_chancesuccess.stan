data {
  int<lower=0> N; // items
  int<lower=0> K[N]; // initial trials
  int<lower=0> y[N]; // initial sucesses

  int<lower=0> K_new[N]; // new trials
  int<lower=0> y_new[N]; // new successes

  }

parameters {
  real<lower = 0, upper = 1> phi; // population chance of success
  real<lower = 1> kappa; // population concentration
  vector<lower = 0, upper = 1>[N] theta; // chance of success (pooled)
}

model {
  kappa ~ pareto(1, 1.5); // hyperprior alongside uniform phi
  theta ~ beta(kappa * phi, kappa * (1 - phi)); // prior
  y ~ binomial(K, theta); // likelihood
}
