library(hyper2)


hotstart <- indep(maxp(collusion))

f <- function(LO){

  eq <- indep(equalp(collusion))
  small <- 1e-3

  eq1 <- hotstart
  eq2 <- hotstart

  eq1[3] <- hotstart[4]*exp(-LO)
  eq2[4] <- hotstart[3]*exp(-LO)
  if(LO<0){
    eq1[3] <- eq1[3]*1.01
    eq2[4] <- eq2[4]*0.99
  } else {
    eq1[3] <- eq1[3]*1.01
    eq2[4] <- eq2[4]*0.99
  }
  
  c(
      maxp(collusion,startp=eq1, fcm=c(0,0, -1, +exp(-LO),rep(0,21)),fcv=0,give=TRUE,control=list(trace=100,maxit=10000))$value,
      maxp(collusion,startp=eq2, fcm=c(0,0, +1, -exp(-LO),rep(0,21)),fcv=0,give=TRUE)$value
  )
  
}

x <- seq(from= -0.4,to=0.4,by=0.1)
jj <- sapply(x,f)

plot  (x,jj[1,],col='black',pch=16)
points(x,jj[2,],col='red',pch=16)
