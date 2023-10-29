## This file follows the structure of aaa.R in the freegroup package.

## Define some checker functions, and call them at the end.  They
## should all return TRUE if the package works, and stop with error if
## a test is failed.  Function checker1() has one argument, checker2()
## two, and checker3() has three.  

test_that("Test suite aaa.R",{

checker1 <- function(x){

    expect_error(!x)

    expect_error(x < x)
    expect_error(x > x)
    expect_error(x <=x)
    expect_error(x >=x)

    expect_error(x/x)
    expect_error(x^x)
  
    if(length(x)>1){expect_error(x+1)}
    if(length(x)>1){expect_error(x-1)}
    if(length(x)>1){expect_error(1+x)}
    if(length(x)>1){expect_error(1-x)}

    expect_true(x == x, info=x)

    expect_true(1*x == x, info=x)
    expect_true(2*x == x+x, info=x)
    expect_true(3*x == x+x+x, info=x)
    expect_true(4*x == x+x+x+x, info=x)
    expect_true(5*x == x+x+x+x+x, info=x)
    expect_true(6*x == x+x+x+x+x+x, info=x)

    expect_true(x+x-x == x, info=x)

  return(TRUE)
}  # checker1() closes


checker2 <- function(x,y){
  expect_true(x == y+x-y, info=list(x,y))
  expect_true(x+y == y+x, info=list(x,y))

  return(TRUE)
}

checker3 <- function(x,y,z){
  expect_true(x+(y+z) == (x+y)+z, info=list(x,y,z)) # additive associativity
  return(TRUE)
} # checker3() closes


for(i in 1:2){
    x <- rhyper2()
    y <- rhyper2()
    z <- rhyper2()

    checker1(x)
    checker2(x,y)
    checker3(x,y,z)

}

checker1(rhyper2()*0)
checker2(rhyper2(),rhyper2()*0)
checker2(rhyper2(),rhyper2()*0)
checker3(rhyper2(),rhyper2(),rhyper2()*0)
checker3(rhyper2(),rhyper2()*0,rhyper2()*0)

})
