library(data.table)
library(rstan)
library(coda)
setwd("~/Learning/Maths and Stats/STAN/Case Study - Baseball")

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

fit.pool <- stan(file = 'complete_pooling.stan', 
                 data = stan.data, iter = M, chains = 4)

ss.pool <- extract(fit.pool)
phi <- ss.pool$phi

print(fit.pool, c('phi'), probs = c(0.1, 0.5, 0.9))


