library(data.table)
library(rstan)
library(coda)
setwd("~/Learning/Github/STAN/casestudybaseball")

df <- fread(file = 'efronmorris.txt')

batting.stats <- df[, list(FirstName, LastName, Hits, `At-Bats`,
                           `RemainingAt-Bats`, RemainingHits = SeasonHits - Hits)]

N <- dim(batting.stats)[1]
K <- batting.stats$`At-B`
y <- batting.stats$Hits
K_new <- batting.stats$`RemainingAt-Bats`
y_new <- batting.stats$RemainingHits

stan.data <- list(N, K, y, K_new, y_new)


M <- 5000

# Complete Pooling
fit.pool <- stan(file = 'complete_pooling.stan', 
                 data = stan.data, iter = M, chains = 4)

ss.pool <- extract(fit.pool)
phi <- ss.pool$phi

print(fit.pool, c('phi'), probs = c(0.1, 0.5, 0.9))


# No Pooling
fit.no.pool <- stan(file = 'part_pool_chancesuccess.stan', data = stan.data,
                    iter = M, chains = 4)

ss.no.pool <- extract(fit.no.pool)
print(fit.no.pool, c('theta'), probs = c(0.1, 0.5, 0.9))


# Partial Pooling (Chance of Success)
fit.partial.cos.pool <- stan(file = 'part_pool_chancesuccess.stan', data = stan.data,
                    iter = M, chains = 4, 
                    control = list(stepsize = 0.01, adapt_delta = 0.99))

ss.partial.cos.pool <- extract(fit.partial.cos.pool)
print(fit.partial.cos.pool, c('kappa', 'phi', 'theta'), probs = c(0.1, 0.5, 0.9))


# Error
# Warning messages:
# 1: There were 7 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# 2: Examine the pairs() plot to diagnose sampling problems
# To remedy this we manually change stepsize and adapt_delta params



# Partial Pooling (Log Odds)
fit.partial.logodd.pool <- stan(file = 'part_pool_logodds.stan', data = stan.data,
                         iter = M, chains = 4, 
                         control = list(stepsize = 0.01, adapt_delta = 0.99))
