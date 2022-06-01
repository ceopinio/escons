votes <- c(391, 311, 184, 73, 27, 12, 2)

## ---------------------------------------- 
## Reference values provided by https://icon.cat/util/elecciones
  
# Checks that it works in trivial allocations
expect_equal(escons:::dhondt(1, c(1, 0)), c(1, 0))
# Checks that it works in trivial allocations
expect_equal(escons:::dhondt(1, c(.1, 0)), c(1, 0))
# Checks that allocation returns correct results 
expect_equal(escons:::dhondt(21, votes), c(9, 7, 4, 1, 0, 0, 0))
# Checks that ties return warnings
expect_warning(escons:::dhondt(2, c(1, 1)))
# Checks that wrapper correctly sets electoral threshold (default = .3)
expect_equal(escons:::shares2seats(21, votes/sum(votes)), c(9, 7, 4, 1, 0, 0, 0))
# Checks that wrapper correctly sets electoral threshold (no threshold)
expect_equal(escons:::shares2seats(21, votes/sum(votes), 0), c(9, 7, 4, 1, 0, 0, 0))
# Checks that wrapper correctly sets electoral threshold (threshold = .08)
expect_equal(escons:::shares2seats(21, votes/sum(votes), .08), c(10, 7, 4, 0, 0, 0, 0))
