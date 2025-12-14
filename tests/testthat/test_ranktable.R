test_that("ranktable tests", {

    x <- rrank()
    expect_output(print(x))
    expect_output(print(summary(x)))

    expect_true(all(as.ranktable(as.ordertable(x)) == x))

    expect_silent(ordertable_to_ranktable(pentathlon_table))

    x <- cbind(c(a=3, b=1, c=2), 1:3)
    colnames(x) <- c("j1", "j2")
    x <-  as.ordertable(x)
    y <- as.ranktable(x)
    expect_true(all(unclass(y) == letters[c(2,1,3,2,1,3)]))

    expect_true(suppfun(pentathlon_table) == pentathlon)
    
} )
