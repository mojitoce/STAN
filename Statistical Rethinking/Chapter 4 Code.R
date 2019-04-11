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



## Fitting a linear model
m4.3 <- map(alist(height ~ dnorm(mu, sigma),
                  mu <-  a + b * weight,
                  a ~ dnorm(178, 100),
                  b ~ dnorm(0, 10),
                  sigma ~ dunif(0, 50)), 
            data = d.adult)
precis(m4.3, corr = TRUE)


# To solve almost perfect correlation of a and b we can try centering our dependent variable
d.adult$weight <- d.adult$weight - mean(d.adult$weight)

m4.4 <- map(alist(height ~ dnorm(mu, sigma),
                  mu <-  a + b * weight,
                  a ~ dnorm(178, 100),
                  b ~ dnorm(0, 10),
                  sigma ~ dunif(0, 50)), 
            data = d.adult)

# Table of Estimates
precis(m4.4, corr = TRUE)


# Plotting posterior inference against the data
plot(height ~ weight, data = d.adult)
abline(a = coef(m4.3)['a'], b = coef(m4.4)['b'])


# Adding uncertainty around the mean
post <- extract.samples(m4.3)
post[1:5,]

# Display a few lines to see scatter with increasing amount of data
N <- 10
d.adult.N <- d.adult[1:N,]

mN <- map(alist(height ~ dnorm(mu, sigma),
                  mu <-  a + b * weight,
                  a ~ dnorm(178, 100),
                  b ~ dnorm(0, 10),
                  sigma ~ dunif(0, 50)), 
            data = d.adult.N)

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( d.adult.N$weight , d.adult.N$height ,
      xlim=range(d.adult$weight) , ylim=range(d.adult$height) ,
      col=rangi2 , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 ) {
  abline( a=post$a[i] , b=post$b[i] , col=col.alpha("black",0.3))
          }



### 4.5 Polynomial Regression ###
plot(height ~ weight, data = d) # Relation is curved

# Standardise data
d.s <- d %>% 
  mutate(weight.s = (weight - mean(d$weight)) / sd(d$weight),
         weight.s2 = weight.s * weight.s,
         weight.s3 = weight.s ^ 3)

m4.5 <- map(alist(height ~ dnorm(mu, sigma),
                  mu <-  a + b1 * weight.s + b2 * weight.s2,
                  a ~ dnorm(178, 100),
                  b1 ~ dnorm(0, 10),
                  b2 ~ dnorm(0, 10),
                  sigma ~ dunif(0, 50)), 
            data = d.s)
precis(m4.5)


weight.seq <- seq( from=-2.2 , to=2 , length.out=30 )
pred_dat <- list( weight.s=weight.seq , weight.s2=weight.seq^2 )
mu <- link( m4.5 , data=pred_dat )
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )
sim.height <- sim( m4.5 , data=pred_dat )
height.PI <- apply( sim.height , 2 , PI , prob=0.89 )


plot( height ~ weight.s , d.s , col=col.alpha(rangi2,0.5) )
lines( weight.seq , mu.mean )
shade( mu.PI , weight.seq )
shade( height.PI , weight.seq )
















