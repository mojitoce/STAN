# Chapter 4 Questions

library(rethinking)
library(plyr)
library(dplyr)

# Medium
## M1
mu.list <- rnorm(1000, mean = 0, sd = 10)
sigma <- runif(1000, min = 0, max = 10)

y <- rnorm(1000, mean = mu.list, sd = sigma)

## M2
map.list.M1 <- alist(
  y ~ dnorm(mu, sigma),
  mu ~ dnorm(0, 10),
  sigma ~ dunif(0, 10)
)


# Hard
## H1
data(Howell1)
kung.data <- Howell1
summary(kung.data)

mh.1 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu  <- a + b*weight,
    a ~ dnorm(130, 40),
    b ~ dnorm(2, 10),
    sigma ~ dunif(0, 30)),
  data = kung.data)
precis(mh.1, corr = TRUE)

test.weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)
pred.height <- link(mh.1, data = data.frame(weight = test.weight))
# Predicts 1000 samples of mu for every test data
pred.height.mean <- apply(pred.height, 2, mean)
pred.height.hpdi <- apply(pred.height, 2, HPDI, prob = 0.89)


## H2
kung.minors <- kung.data %>% filter(age < 18)
summary(kung.minors)

### a
mh.2 <- map(
  alist(
    height ~ dnorm(mu, sigma),
    mu  <- a + b*weight,
    a ~ dnorm(110, 30),
    b ~ dnorm(3, 10),
    sigma ~ dunif(0, 20)),
  data = kung.data)
precis(mh.2, corr = TRUE)

### b
weight.seq <- seq(from = 0, to = 50, by = 1)
mu.pred <- link(mh.2, data = data.frame(weight = weight.seq))
mu.p.mean <- apply(mu.pred, 2, mean)
mu.p.hpdi <- apply(mu.pred, 2, HPDI, prob = 0.89)

plot(height ~ weight, data = kung.minors)
lines(weight.seq, mu.p.mean)
shade(mu.p.hpdi, weight.seq)

sim.height <- sim(mh.1, data = list(weight = weight.seq))
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.89)
shade(height.PI, weight.seq)


## H3
### a
kung.data.b <- kung.data %>% 
  mutate(log.weight = log(weight))

mh.3 <- map(
  alist(height ~ dnorm(mu, sigma),
        mu  <- a + b*log(weight),
        a ~ dnorm(178, 100),
        b ~ dnorm(0, 100),
        sigma ~ dunif(0, 50)),
  data = kung.data.b)
precis(mh.3)

### b
weight.seq2 <- seq(from = min(kung.data.b$weight), to = max(kung.data.b$weight), by = 1)
mu.pred <- link(mh.3, data = data.frame(weight = weight.seq2))
mu.p.mean <- apply(mu.pred, 2, mean)
mu.p.hpdi <- apply(mu.pred, 2, HPDI, prob = 0.97)

plot(height ~ weight, data = kung.data.b, col = col.alpha(rangi2, 0.4))
lines(weight.seq2, mu.p.mean)
shade(mu.p.hpdi, weight.seq2)

sim.height <- sim(mh.3, data = list(weight = weight.seq2))
str(sim.height)
height.PI <- apply(sim.height, 2, PI, prob = 0.97)
shade(height.PI, weight.seq2)
