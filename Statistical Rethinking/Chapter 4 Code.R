library(rethinking)
library(plyr)
library(dplyr)


# Normal by Addition
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))

# Normal by Multiplication
prod(1 + runif(12, 0, 0.1))
growth <- replicate(10000, prod( 10 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

# Normal by log-multiplication
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 1))))
dens(log.big)


# Gaussian model for Height
data(Howell1)
d <- Howell1
str(d)
d.adult <- d %>% filter(age >= 18)
dens(d.adult$height)

sample_mu <- rnorm(1e4, 178, 20) # Prior mu
sample_sigma <- runif(1e4, 0, 50) # Prior sigma
prior_h <- rnorm(1e4, sample_mu, sample_sigma) # Prior N(mu, sigma)
dens(prior_h)

# Brute force calculation of grid approximation
mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)
post <-  expand.grid(mu = mu.list, sigma = sigma.list)
post$LL <- sapply(1:nrow(post), function(i) # Posterior computed for all mean combinations 140 to 160 and sd from 4 to 9
  sum(dnorm(d.adult$height,
            mean = post$mu[i],
            sd = post$sigma[i],
            log = TRUE)))
post$prod <- post$LL + dnorm(post$mu, 178, 20, TRUE) +
  dunif(post$sigma, 0, 50, TRUE) 
post$prob <- exp(post$prod - max(post$prod))

contour_xyz(post$mu, post$sigma, post$prob)
image_xyz(post$mu, post$sigma, post$prob)


# Let's sample the posterior
sample.rows <- sample(1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

plot(sample.mu, sample.sigma, cex = 0.5, pch = 16, col = col.alpha(rangi2, 0.1))
dens(sample.mu)
dens(sample.sigma)

HPDI(sample.mu)
HPDI(sample.sigma)


## Quadratic Approximation
map.list <- alist(
  height ~ dnorm(mu, sigma),
  mu ~ dnorm(178, 20),
  sigma ~ dunif(0, 50)
)

m4.1 <- map(map.list, data = d.adult)
precis(m4.1)

m4.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu ~ dnorm(178, 0.5),
    sigma ~ dunif(0, 50)
  ),
  data = d.adult
)
precis(m4.2)
