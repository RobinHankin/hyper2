test_that("ranktable tests", {

    x <- rrank()
    expect_output(print(x))
    expect_output(print(summary(x)))

    
} )
