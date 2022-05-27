expect_error(moe(1.1, 1000, .95))
expect_error(moe(-0.1, 1000, .95))
expect_error(moe(.5, 1000, 95))
expect_error(moe(data.frame("a"=1.1, "b"=-0.5), 1000, .95))

expect_equal(round(moe(.5, 1000, .95), 3), 0.031)
expect_equal(round(moe(data.frame("a"=.5, "b"=.5), 1000, .95), 3),
             data.frame("a"=.031, "b"=.031))
