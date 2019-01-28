# Statistical Rethinking Chapter 2
library(rethinking)
library(ggplot2)

### Grid Approximation
# Define grid
p_grid <- seq(from = 0, to = 1, length.out = 20)

# Define prior 
# prior <- rep(1, 20)
# prior <- ifelse(p_grid < 0.5, 0, 1)
prior <- exp(-5*abs(p_grid - 0.5))

# Compute likelihood
likelihood <- dbinom(6, size = 9, prob = p_grid)

# Compute product of likelihood and prior
unstd.posterior <- prior * likelihood

# Compute posterior
posterior <- unstd.posterior / sum(unstd.posterior)
ggplot(data.frame(p_grid, posterior), aes(x = p_grid, y = posterior)) + 
  geom_point() + geom_line()


### Quadratic Approximation
globe.qa <- map(
  alist(
    w ~ dbinom(9, p), # Binomial Likelihood
    p ~ dunif(0, 1)
  ),
  data = list(w = 6)
)
# Provide a formula, a list of data, and a list of start values for the parameters. 
precis(globe.qa) # Assuming posterior is Gaussian, it is maximized at 0,67, and its standard dev is 0.16

# Analytical calculation
w <- 6
n <- 9
curve(dbeta(x, w+1, n-w+1), from = 0, to = 1)
curve(dnorm(x, 0.67, 0.16), lty = 2, add = TRUE) # Quadratic approximation

