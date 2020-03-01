library("hyper2")

allbakers <- c("Alice","Amelia","Dan","David","Helena","Henry",
             "Jamie","Michael","Michelle","Phil","Priya","Rosie","Steph")
# NB alphabetical order

week01 <- c(Alice = 5, Amelia = 4, David = 10, Dan = 9, Helena = 12, Henry = 1, Jamie = 13, Michael = 11, Michelle = 6, Phil = 8, Priya = 7, Rosie = 2, Steph = 3)
week02 <- c(Alice = 1, Amelia = 9, David =  2,          Helena = 12, Henry = 6, Jamie = 11, Michael =  4, Michelle = 8, Phil = 3, Priya = 7, Rosie = 5, Steph =10)
week03 <- c(Alice = 7, Amelia =11, David =  2,          Helena =  8, Henry = 1,             Michael =  6, Michelle = 5, Phil =10, Priya = 4, Rosie = 9, Steph = 3)
week04 <- c(Alice = 8,             David =  2,          Helena =  9, Henry = 3,             Michael =  7, Michelle = 5, Phil = 6, Priya =10, Rosie = 4, Steph = 1)
week05 <- c(Alice = 6,             David =  9,          Helena =  1, Henry = 3,             Michael =  8, Michelle = 7,           Priya = 2, Rosie = 5, Steph = 4)
week06 <- c(Alice = 1,             David =  2,                       Henry = 4,             Michael =  6,                         Priya = 7, Rosie = 5, Steph = 3)
week07 <- c(Alice = 6,             David =  2,                       Henry = 3,             Michael =  5,                                    Rosie = 1, Steph = 4)
week08 <- c(Alice = 3,             David =  1,                       Henry = 5,                                                              Rosie = 2, Steph = 4)
week09 <- c(Alice = 4,             David =  2,                                                                                               Rosie = 1, Steph = 3)
week10 <- c(Alice = 2,             David =  1,                                                                                                          Steph = 3)


w01 <- c(David = 10, Alice = 5, Steph = 3, Rosie = 2, Henry = 1, Michael = 11, Priya = 7, Michelle = 6, Helena = 12, Phil = 8, Amelia = 4, Jamie = 13, Dan = 9)
w02 <- c(David =  2, Alice = 1, Steph =10, Rosie = 5, Henry = 6, Michael =  4, Priya = 7, Michelle = 8, Helena = 12, Phil = 3, Amelia = 9, Jamie = 11)
w03 <- c(David =  2, Alice = 7, Steph = 3, Rosie = 9, Henry = 1, Michael =  6, Priya = 4, Michelle = 5, Helena =  8, Phil =10, Amelia =11)
w04 <- c(David =  2, Alice = 8, Steph = 1, Rosie = 4, Henry = 3, Michael =  7, Priya =10, Michelle = 5, Helena =  9, Phil = 6)
w05 <- c(David =  9, Alice = 6, Steph = 4, Rosie = 5, Henry = 3, Michael =  8, Priya = 2, Michelle = 7, Helena =  1)
w06 <- c(David =  2, Alice = 1, Steph = 3, Rosie = 5, Henry = 4, Michael =  6, Priya = 7)
w07 <- c(David =  2, Alice = 6, Steph = 4, Rosie = 1, Henry = 3, Michael =  5)
w08 <- c(David =  1, Alice = 3, Steph = 4, Rosie = 2, Henry = 5)
w09 <- c(David =  2, Alice = 4, Steph = 3, Rosie = 1)
w10 <- c(David =  1, Alice = 2, Steph = 3)


# Calculate H, the likelihood function for the observed ordering:
f <- function(x){rank_likelihood(char2num(names(sort(x)),allbakers))}

H <- hyper2(pnames=allbakers)
H <- H + f(w01)
H <- H + f(w02)
H <- H + f(w03)
H <- H + f(w04)
H <- H + f(w05)
H <- H + f(w06)
H <- H + f(w07)
H <- H + f(w08)
H <- H + f(w09)
H <- H + f(w10)

## Now some observed statistics:
mp_obs  <- maxp(H)
X_obs <-  2*(loglik(indep(mp_obs),H) - loglik(indep(equalp(H)),H))
print(paste("asymptotic pvalue = ",pchisq(X_obs,df=12,lower.tail=FALSE)))
m_obs <- names(which.max(mp_obs))
name_maxlike_obs <- names(which.max(mp_obs))


# Now some permutation tests:
shuffle <- function(a){
  out <- sample(a)
  names(out) <- names(a)
  return(out)
}


n <- 1000
X <- rep(NA,n)
m <- rep(NA,n)
name_maxlike <- rep("",n)
for(i in seq_len(n)){
  print(i)
  Hstar <- hyper2(pnames=allbakers)
  Hstar <- Hstar + f(shuffle(w01))
  Hstar <- Hstar + f(shuffle(w02))
  Hstar <- Hstar + f(shuffle(w03))
  Hstar <- Hstar + f(shuffle(w04))
  Hstar <- Hstar + f(shuffle(w05))
  Hstar <- Hstar + f(shuffle(w06))
  Hstar <- Hstar + f(shuffle(w07))
  Hstar <- Hstar + f(shuffle(w08))
  Hstar <- Hstar + f(shuffle(w09))
  Hstar <- Hstar + f(shuffle(w10))
  
  
  mp <- maxp(Hstar)
  X[i] <- 2*(loglik(indep(mp),Hstar) - loglik(indep(equalp(Hstar)),Hstar))
  m[i] <- max(mp)
  name_maxlike[i] <- names(which.max(mp))
}


## In theory, X should be distributed as chi-square with 13-1=12
## degrees of freedom:
hist(X)
abline(v=X_obs,lwd=7)

# Check the asymptotic distribution:
dev.new()
par(pty='s')
qqplot(rchisq(1e5,df=17),X,asp=1,pty='s',xlim=c(0,50),ylim=c(0,50))
abline(0,1)



 
