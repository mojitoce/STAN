library(rethinking)
library(plyr)
library(dplyr)
data(milk)
d <- milk

m.pf <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bf ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
m.pl <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl * perc.lactose,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m.pf, digits = 3)
precis(m.pl, digits = 3) # Posteriors basically mirror images of each other but with different signs


m.multi <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bl * perc.lactose + bf * perc.fat,
    a ~ dnorm(0.6, 10),
    bl ~ dnorm(0, 1),
    bf ~ dnorm(0, 1),    
    sigma ~ dunif(0, 10)
  ),
  data = d
)
precis(m.multi, digits = 3)

pairs(~ kcal.per.g + perc.fat + perc.lactose, data = d, col = rangi2)
cor(d$perc.fat, d$perc.lactose)

