test_that("dirichlet() and dirichlet3() tests", {

    f <- function(n){
        jj <- setNames(sample(n,n),sample(letters[seq_len(n)]))
        expect_true(dirichlet(alpha=jj+1) == dirichlet(powers=jj))
    }

    sapply(4:9,f)

    
    expect_true(
        dirichlet(powers=c(t=4,r=6,p=2)) ==
        hyper2(list("r","t",c("t","p","r"),"p") ,c(6,4,-12,2))
    )


    expect_error(dirichlet3(1:5))  # no names attribute
    
    jj <- setNames(4:6,c("o","a","w"))
    expect_error(dirichlet3(jj,setNames(0.8 + 1:3,c("o","w","a")))) # names different order
    
    expect_true(
        dirichlet3(jj,7) ==
        hyper3(list(c(a=1),c(a=1,o=7,w=1),c(o=7),c(w=1)), powers=c(5,-15,4,6))
    )
    
    expect_true(
        dirichlet3(jj,lambda=1:3) ==
        hyper3(list(c(a=2),c(a=2,o=1,w=3),c(o=1),c(w=3)),powers=c(5,-15,4,6))
    )

} )
