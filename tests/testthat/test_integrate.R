test_that("integrate() tests", {
    TOL <- 1e-7

    x <- c(a=1,b=2,c=3)
    expect_true(abs(B(dirichlet(alpha = x)) - prod(gamma(x))/gamma(sum(x))) < TOL)

    expect_true(all(abs(mean_hyper2(dirichlet(alpha=x)) - c(1/6, 1/3, 1/2)) < TOL))

    
} )
