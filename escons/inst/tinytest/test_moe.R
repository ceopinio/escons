## Checks error with illegal proportions
expect_error(moe(1.1, 1000, .95))
## Checks error with illegal proportions
expect_error(moe(-0.1, 1000, .95))
## Checks error with illegal level
expect_error(moe(.5, 1000, 95))
## Checks error with illegal proportion using data.frame method
expect_error(moe(data.frame("a"=1.1, "b"=-0.5), 1000, .95))
## Checks MOE for value against table
expect_equal(round(moe(.5, 1000, .95), 3), 0.031)
## Checks MOE for value against table using data.frame method
expect_equal(round(moe(data.frame("a"=.5, "b"=.5), 1000, .95), 3),
             data.frame("a"=.031, "b"=.031))
