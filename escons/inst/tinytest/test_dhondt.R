votes <- c(391, 311, 184, 73, 27, 12, 2)

expect_equal(escons:::dhondt(1, c(1, 0)), c(1, 0))
expect_equal(escons:::dhondt(1, c(.1, 0)), c(1, 0))
expect_equal(escons:::dhondt(21, votes), c(9, 7, 4, 1, 0, 0, 0))
expect_warning(escons:::dhondt(2, c(1, 1)))

expect_equal(escons:::shares2seats(21, votes/sum(votes)), c(9, 7, 4, 1, 0, 0, 0))
expect_equal(escons:::shares2seats(21, votes/sum(votes), 0), c(9, 7, 4, 1, 0, 0, 0))
expect_equal(escons:::shares2seats(21, votes/sum(votes), .1), c(10, 7, 4, 0, 0, 0, 0))
