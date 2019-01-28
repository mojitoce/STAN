library(data.table)
setwd("~/Learning/GitHub/STAN/casestudybaseball")

df <- fread(file = 'efronmorris.txt')

batting.stats <- df[, list(FirstName, LastName, Hits, `At-Bats`,
                           `RemainingAt-Bats`, RemainingHits = SeasonHits - Hits)]

N <- dim(batting.stats)[1]
K <- batting.stats$`At-B`
y <- batting.stats$Hits
K_new <- batting.stats$`RemainingAt-Bats`
y_new <- batting.stats$RemainingHits
d