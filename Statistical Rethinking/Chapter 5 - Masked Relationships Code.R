library(rethinking)
data(milk)
d <- milk
str(d)

d.cc <- d %>% filter(complete.cases(d))

m.neoperc <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc,
    a ~ dnorm(0, 100),
    bn ~ dnorm(0, 1), 
    sigma ~ dunif(0, 1)
  ),
  data = d.cc
)  
precis(m.neoperc, digits = 3) # bn super super small

np.seq <- 0:100
pred.data <- data.frame(neocortex.perc = np.seq)
mu.np <- link(m.neoperc, data = pred.data, n = 1e4)
mu.np.mean <- apply(mu.np, 2, mean)
mu.np.PI <- apply(mu.np, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = d.cc, col = rangi2)
lines(np.seq, mu.np.mean)
lines(np.seq, mu.np.PI[1,], lty = 2)
lines(np.seq, mu.np.PI[2,], lty = 2)
# Not capturing well at all

d.cc$log.mass <- log(d.cc$mass)

m.mass <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bm * log.mass,
    a ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = d.cc
)
precis(m.mass, digits = 3) # Mass has negative effect

m.multi <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu <- a + bn * neocortex.perc + bm * log.mass,
    a ~ dnorm(0, 5),
    bn ~ dnorm(0, 1),
    bm ~ dnorm(0, 1),
    sigma ~ dunif(0, 1)
  ),
  data = d.cc
)
precis(m.multi, digits = 3)


# Counterfactuals
mean.log.mass <- mean(d.cc$log.mass)
pred.data2 <- data.frame(neocortex.perc = np.seq,
                         log.mass = mean.log.mass)

mu <- link(m.multi, pred.data2, n = 1e4)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)

plot(kcal.per.g ~ neocortex.perc, data = d.cc, type = 'n')
lines(np.seq, mu.mean)
lines(np.seq, mu.PI[1,], lty = 2)
lines(np.seq, mu.PI[2,], lty = 2)
