test_that("formula1 points systems", {
    expect_true(all(formula1_points_systems()$top7==rep(1,7)))
} )
