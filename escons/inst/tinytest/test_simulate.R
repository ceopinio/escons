set.seed(9013)

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
expect_true(all.equal(colMeans(res), votes/sum(votes)))
## Checks simulation is drawing suff correct results using data.frame method
res <- escons:::simulate_vote_share(votes/sum(votes), .1, 1000)
expect_true(min(abs(colMeans(res) - votes/sum(votes))) < .1)

## ---------------------------------------- 
## Test of public function
Nsim <- 10
res2021 <- data.frame("Barcelona"=c(25.05, 20.42, 17.9, 7.83, 7.76, 6.3, 6.1, 4.05),
                      "Girona"=c(15.15, 21.8, 32.68, 6.16, 4.04, 9.04, 3.25, 1.99),
                      "Lleida"=c(15.0, 26.58, 28.04, 5.53, 3.23, 7.4, 3.21, 3.54),
                      "Tarragona"=c(20.04, 24.49, 19.37, 9.4, 4.91, 6.79, 5.23, 4.32))/100
sig2021 <- moe(res2021, N=1000, level=.95)
parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
res <- simulate(res2021, sig2021, parties, N=Nsim)
res <- as.data.frame(res)

## Tests that all simulations add up to full parliament
expect_equal(unique(by(res$total, res$simulation, sum)), 135)
## Tests that all simulations return correct district seats
expect_equal(unique(by(res$Barcelona, res$simulation, sum)), 85)
expect_equal(unique(by(res$Girona, res$simulation, sum)), 17)
expect_equal(unique(by(res$Tarragona, res$simulation, sum)), 18)
expect_equal(unique(by(res$Lleida, res$simulation, sum)), 15)
## Tests that all simulations include all parties
expect_equal(unique(table(res$party)), Nsim)

## Check calculations without variance
sig2021_zero <- sig2021
sig2021_zero[] <- 0
res_zero <- simulate(res2021, sig2021_zero, parties, N=Nsim)
res_zero <- as.data.frame(res_zero)

## ---------------------------------------- 
## Reference values provided by https://icon.cat/util/elecciones

## Check seat allocation by district (parties sorted alphabetically)
## True results come from https://resultados.elpais.com/elecciones/2021/autonomicas/09/index.html
barcelona <- as.list(by(res_zero$Barcelona, res_zero$party, mean))
barcelona_res <- list("Cs"=5, "CUP–G"=5, "ECP–PEC"=7, "ERC"=19, "JxCat"=16, "PP"=3, "PSC"=23, "Vox"=7)

expect_equal(barcelona[order(names(barcelona))], barcelona_res[order(names(barcelona_res))])

girona <- as.list(by(res_zero$Girona, res_zero$party, mean))
girona_res <- list("Cs"=0, "CUP–G"=2, "ECP–PEC"=0, "ERC"=4, "JxCat"=7, "PP"=0, "PSC"=3, "Vox"=1)

expect_equal(girona[order(names(girona))], girona_res[order(names(girona_res))])

tarragona <- as.list(by(res_zero$Tarragona, res_zero$party, mean))
tarragona_res <- list("Cs"=1, "CUP–G"=1, "ECP–PEC"=1, "ERC"=5, "JxCat"=4, "PP"=0, "PSC"=4, "Vox"=2)

expect_equal(tarragona[order(names(tarragona))], tarragona_res[order(names(tarragona_res))])

lleida <- as.list(by(res_zero$Lleida, res_zero$party, mean))
lleida_res <- list("Cs"=0, "CUP–G"=1, "ECP–PEC"=0, "ERC"=5, "JxCat"=5, "PP"=0, "PSC"=3, "Vox"=1)

expect_equal(lleida[order(names(lleida))], lleida_res[order(names(lleida_res))])

## ---------------------------------------- 
## Order of input data does not matter
res2021 <- res2021[, c(2, 1, 4, 3)]
res_permutation <- simulate(res2021, sig2021_zero, parties, N=Nsim)
res_permutation <- as.data.frame(res_permutation)

expect_identical(res_zero, res_permutation)
