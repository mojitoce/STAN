# Statistical Rethinking Chapter 3
library(rethinking)
library(ggplot2)

# Sampling from a grid-approximate posterior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)

likelihood <- dbinom(6, size = 9, prob = p_grid)

posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample(p_grid, prob = posterior, size = 1000, replace = TRUE)

plot(samples)
dens(samples)

### Sampling to Summarize
## 1) Intervals of defined boundaries

# add up posterior prob where p < 0.5
sum(posterior[p_grid < 0.5])
sum(samples < 0.5)  / length(samples)

sum(samples > 0.5 & samples < 0.75) / length(samples)

# Intervals of defined mass
quantile(samples, 0.5) # Param for which anything below contains 50% of prob

PI(samples, prob = 0.5)
HPDI(samples, prob = 0.5)


# Point estimates
p_grid[which.max(posterior)] #MAP
chainmode(samples, adj = 0.01)
