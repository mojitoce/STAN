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

# M2. Calculate 90% HDPI
HPDI(samples.med, 0.9) # 0.33 to 0.71

# M3. Posterior Predictive Check
ppc15 <- rbinom(10000, size = 15, prob = samples.med)
prob815 <- mean(ppc15==8) # 0.114

# M4. Posterior with 8/15 and probability of observing 6/9
ppc9 <- rbinom(10000, size = 9, prob = samples.med)
prob.69 <- mean(ppc9 == 6)

# M5. Use different prior
prior.med2 <- c(rep(0, 5000), rep(1, 5000))
posterior.med2 <- likelihood.med * prior.med2
posterior.med2 <- posterior.med2 / sum(posterior.med2)

dens(posterior.med2[posterior.med2 > 0])
samples.med2 <- sample(p_grid.med, prob = posterior.med2, size = 10000, replace = TRUE)
HPDI(samples.med2, 0.9) # 0.5 to 0.7

ppc15.2 <- rbinom(10000, size = 15, prob = samples.med2)
ppc815.2 <- mean(ppc15.2 == 8)# 0.161

ppc9.2 <- rbinom(10000, size = 9, prob = samples.med2)
ppc.69.2 <- mean(ppc9.2 == 6) #0.23


## Hard
data(homeworkch3)

# H1. 
pboy.grid <- seq(from = 0, to = 1, length = 10000)
const.prior.birth <- rep(1, 10000)

boys <- sum(birth1) + sum(birth2)
tot <- 200
likelihood.b <- dbinom(boys, size = tot, prob = pboy.grid)

posterior.b <- const.prior.birth * likelihood.b
posterior.b <- posterior.b / sum(posterior.b)

# Param maximising posterior
pboy.grid[which.max(posterior.b)] # 0.555

samples.b <- sample(pboy.grid, prob = posterior.b, size = 10000, replace = TRUE)

HPDI(samples.b, 0.5)
HPDI(samples.b, 0.89)
HPDI(samples.b, 0.97)

sim.b <- rbinom(10000, size = 200, prob = samples.b)
dens(sim.b)
plot(density(sim.b))
abline(v = 111) # 111 (what we actually observed) seems to be in middle of plot.
# Even though not the most likely value

b1 <- sum(birth1)
sim.b1 <- rbinom(10000, size = 100, prob = samples.b)
plot(density(sim.b1))
abline(v = 51) # Model does not seem to fit observations for first births


g1 <- sum(1-birth1)
gbirth2 <- birth2[birth1 == 0]

sim.gfirst <- rbinom(10000, size = 51, prob = samples.b)
plot(density(sim.gfirst))
abline(v = 39)
# Number of girl first borns 51
# Number of boys born after girl first borns 39
# Posterior predicts that this number should be around 25-30
# Births do not look to be independendt as we assumed
