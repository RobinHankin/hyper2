test_that("Miscellaneous hyper3 tests", {
    h1 <- hyper3_bw(B=list("a",c("a","b"),"b"),W=list(1.2,c(1.2,1),1),powers=c(3,4,-7))
    h2 <- hyper3_nv(list(c(b=3,c=1,a=9),c(x=6,y=8)),c(5,-5))

    M <- rbind(c(1.2,0,0,0,0),c(1.2,1,0,0,0),c(9,3,1,0,0),c(0,1,0,0,0),c(0,0,0,6,8))
    colnames(M) <- c("a","b","c","x","y")
    h3 <- hyper3_m(M,c(3,4,5,-7,-5))

    expect_true(h1+h2 == h3)


    
} )
