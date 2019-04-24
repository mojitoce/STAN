library(rethinking)
library(plyr)
library(dplyr)

data(WaffleDivorce)
div.d <- WaffleDivorce %>% 
  mutate(MedianAgeMarriage.s = (MedianAgeMarriage - mean(MedianAgeMarriage)) / sd(MedianAgeMarriage),
         Marriage.s = (Marriage - mean(Marriage)) / sd(Marriage))

m5.3 <- map(
  alist(
    Divorce ~ dnorm(mu, sigma),
    mu <- alpha + bR*Marriage.s + bA*MedianAgeMarriage.s,
    alpha ~ dnorm(10, 10),
    bR ~ dnorm(0, 1),
    bA ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = div.d
)
plot( precis(m5.3) )
# Posterior mean for marriage rate, bR, close to zero with a lot of probability on both sides
# Posterior mean for age at marriage, bA, below zero
# We can interpret as: Once we know median age at marriage for a state, there is little or no additional
# predictive power in also knowing rate of marriage in that state


## Predictor Residual Plots
m5.4 <- map(
  alist(
    Marriage.s ~ dnorm(mu, sigma),
    mu <- a + b*MedianAgeMarriage.s,
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = div.d
)

mu <- coef(m5.4)['a'] + coef(m5.4)['b'] * div.d$MedianAgeMarriage.s
m.resid <- div.d$Marriage.s - mu

plot(Marriage.s ~ MedianAgeMarriage.s, div.d, col = rangi2)
abline(m5.4)
for (i in 1:length(m.resid)) {
  x <- div.d$MedianAgeMarriage.s[i] # x location of line segment
  y <- div.d$Marriage.s[i] # observed endpoint of line segment
  # draw line segment
  lines(c(x,x), c(mu[i], y), lwd = 0.5, col = col.alpha('black', 0.7))
}



## Counterfactual Plots
A.avg <- mean(div.d$MedianAgeMarriage.s)
R.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data <- data.frame(Marriage.s = R.seq, 
                        MedianAgeMarriage.s = A.avg)

# Compute counterfactual mean divorce
mu <- link(m5.3, data = pred.data)
mu.mean <- apply(mu, 2, mean)
mu.PI <- apply(mu, 2, PI)
R.sim <- sim(m5.3, data = pred.data, n = 10000) # 10000 simulations from the dummy data
R.PI <- apply(R.sim, 2, PI)

plot(Divorce ~ Marriage.s, data = div.d)
mtext('MedianAgeMarriage.s = 0')
lines(R.seq, mu.mean)
shade(mu.PI, R.seq)
shade(R.PI, R.seq)


R.avg <- mean(div.d$Marriage.s)
A.seq <- seq(from = -3, to = 3, length.out = 30)
pred.data2 <- data.frame(Marriage.s = R.avg,
                         MedianAgeMarriage.s = A.seq)
mu2 <- link(m5.3, data = pred.data2)
mu2.mean <- apply(mu2, 2, mean)
mu2.PI <- apply(mu2, 2, PI)
R2.sim <- sim(m5.3, data = pred.data2, n = 10000)
R2.PI <- apply(R2.sim, 2, PI)

plot(Divorce ~ MedianAgeMarriage.s, data = div.d)
mtext('Marriage.s = 0')
lines(A.seq, mu2.mean)
shade(mu2.PI, A.seq)
shade(R2.PI, A.seq)


## Posterior Prediction Plots
mu3 <- link(m5.3)
mu3.mean <- apply(mu3, 2, mean)
mu3.PI <- apply(mu3, 2, PI)

divorce.sim <- sim(m5.3, n = 1e4)
divorce.PI <- apply(divorce.sim, 2, PI)

plot(mu3.mean ~ div.d$Divorce, col = rangi2, ylim = range(mu.PI),
     xlab = 'Observed divorce', ylab = 'Predicted Divorce')
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(div.d)) {
  lines(rep(div.d$Divorce[i], 2), c(mu3.PI[1, i], mu3.PI[2, i]), col = rangi2)
}
identify(x = div.d$Divorce, y = mu3.mean, labels = div.d$Loc, cex = 0.8)


divorce.resid <- div.d$Divorce - mu3.mean
o <- order(divorce.resid)
dotchart(divorce.resid[o], labels = div.d$Loc[o], xlim = c(-6, 5), cex = 0.5)
abline(v = 0, col = col.alpha('black', 0.2))
for (i in 1:nrow(div.d)) {
  j <- o[i]
  lines(div.d$Divorce[j] - c(mu3.PI[1, j], mu3.PI[2, j]), rep(i, 2))
  points(div.d$Divorce[j] - c(divorce.PI[1, j], divorce.PI[2, j]), rep(i, 2), pch = 3, cex = 0.6, col = 'gray')
}
