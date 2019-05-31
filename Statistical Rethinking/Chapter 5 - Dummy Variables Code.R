data(milk)
d <- milk

unique(d$clade)
d.dummy <- d %>% 
  mutate(clade.NWM = ifelse(clade == 'New World Monkey', 1, 0),
         clade.S = ifelse(clade == 'Strepsirrhine', 1, 0),
         clade.OWM = ifelse(clade == 'Old World Monkey', 1, 0))

m.dummy <- map(
  alist(
    kcal.per.g ~ dnorm(mu, sigma),
    mu ~ a + b.NWM * clade.NWM + b.S * clade.S + b.OWM * clade.OWM,
    a ~ dnorm(0.6, 4),
    b.NWM ~ dnorm(0, 1),
    b.S ~ dnorm(0, 1),
    b.OWM ~ dnorm(0, 1),
    sigma ~ dunif(0, 10)
  ),
  data = d.dummy
)

precis(m.dummy, digits = 4)

post <- extract.samples(m.dummy)
mu.ape <- post$a
mu.NWM <- post$a + post$b.NWM
mu.OWM <- post$a + post$b.OWM
mu.S <- post$a + post$b.S
precis(data.frame(mu.ape, mu.NWM, mu.OWM, mu.S))


diff.NWM.OWM <- mu.NWM - mu.OWM
quantile(diff.NWM.OWM, probs = c(0.025, 0.5, 0.975))


