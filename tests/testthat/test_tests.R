test_that("Miscellaneous hyper3 tests", {

    expect_output(print(equalp.test(icons)))
    expect_output(print(knownp.test(icons,setNames(zipf(6),pnames(icons)))))
    expect_output(print(samep.test(icons,c("NB","L"))))
    expect_output(print(specificp.test(icons,"NB",0.1)))
    expect_output(print(specificp.test(icons,"NB",0.1,alternative="greater")))
    expect_output(print(specificp.test(icons,"NB",0.1,alternative="less")))
    expect_output(print(specificp.test(icons,"NB",0.1,alternative="two.sided")))
} )
