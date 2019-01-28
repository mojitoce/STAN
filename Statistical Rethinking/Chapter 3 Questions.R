library(rethinking)

## Easy
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid, prob = posterior, size = 1000, replace = TRUE)
n.samples <- length(samples)

# Posterior below 0.2
sum(samples < 0.2) / n.samples # 0
sum(posterior[p_grid < 0.2])

# 20% of prob lies below
quantile(samples, 0.2) # Phi = 0.52

# Narrowest interval containing 66% of prob
HPDI(samples, 0.66)


## Medium
p_grid.med <- seq(from = 0, to = 1, length.out = 10000)
prior.med <- rep(1, 10000)
likelihood.med <- dbinom(8, size = 15, prob = p_grid.med)
posterior.med <- likelihood.med * prior.med
posterior.med <- posterior.med / sum(posterior.med)

samples.med <- sample(p_grid.med, prob = posterior.med, size = 10000, replace = TRUE)
