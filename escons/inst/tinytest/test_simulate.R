res <- escons:::simulate_vote_share(.5, .1, 1000)
## Checks simulation is drawing suff correct results
expect_true(abs(mean(res) - .5) < .1)
## Checks simulation is drawing suff correct results
expect_true(abs(sd(res) - .1) < .1)

res <- escons:::simulate_vote_share(c(.5, .5), c(.1, .1), 1000)
## Checks simulation is drawing suff correct results using data.frame method
expect_true(min(abs(colMeans(res) - c(.5, .5))) < .1)
## Checks simulation is drawing suff correct results using data.frame method
expect_true(min(abs(apply(res, 2, sd) - c(.1, .1))) < .1)

## Checks warning if simulations thrown invalid proportions
expect_warning(escons:::simulate_vote_share(1, .1, 1000))

votes <- c(391, 311, 184, 73, 27, 12, 2)

## Checks that simulation without variance returns correct proportions
res <- escons:::simulate_vote_share(votes/sum(votes), .0, 1000)
expect_true(all(colMeans(res) == votes/sum(votes)))
## Checks simulation is drawing suff correct results using data.frame method
res <- escons:::simulate_vote_share(votes/sum(votes), .1, 1000)
expect_true(min(abs(colMeans(res) - votes/sum(votes))) < .1)

## ---------------------------------------- 
## Test of public function
Nsim <- 10
res2021 <- data.frame("Barcelona"=c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0),
                      "Girona"=c(15.2, 21.8, 32.7, 6.1, 4.0, 9.0, 3.3, 2.0),
                      "Lleida"=c(15.0, 26.6, 28.0, 5.5, 3.2, 7.4, 3.2, 3.5),
                      "Tarragona"=c(20.0, 24.5, 19.4, 9.4, 4.9, 6.8, 5.2, 4.3))/100
sig2021 <- moe(res2021, N=1000, level=.95)
parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
res <- simulate(res2021, sig2021, parties, N=Nsim)
res <- as.data.frame(res)

## Tests that all simulations add up to full parliament
expect_equal(unique(by(res$total, res$simulation, sum)), 135)
## Tests that all simulations return correct district seats
expect_equal(unique(by(res$Barcelona, res$simulation, sum)), 85)
expect_equal(unique(by(res$Girona, res$simulation, sum)), 17)
expect_equal(unique(by(res$Tarragona, res$simulation, sum)), 15)
expect_equal(unique(by(res$Lleida, res$simulation, sum)), 18)
## Tests that all simulations include all parties
expect_equal(unique(table(res$party)), Nsim)

## Check calculations without variance
sig2021[] <- 0
res <- simulate(res2021, sig2021, parties, N=Nsim*2)
res <- as.data.frame(res)

## ---------------------------------------- 
## Reference values provided by https://icon.cat/util/elecciones

## Check seat allocation by district (parties sorted alphabetically)
expect_equal(as.vector(by(res$Barcelona, res$party, mean)), c(5, 5, 7, 19, 16, 3, 23, 7))
expect_equal(as.vector(by(res$Girona, res$party, mean)), c(0, 2, 0, 4, 7, 0, 3, 1))
expect_equal(as.vector(by(res$Lleida, res$party, mean)), c(0, 1, 0, 6, 7, 0, 3, 1))
## There are ties in the data: Testing against min for each party
expect_equal(as.vector(by(res$Tarragona, res$party, min)), c(1, 1, 0, 4, 3, 0, 4, 1))
