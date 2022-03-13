library(hyper2)
n <- 10
p <- zipf(n)
names(p) <- letters[seq_len(n)]
M <- matrix(0,n,n)
rownames(M) <- names(p)
colnames(M) <- names(p)
index <- which(upper.tri(M),arr.ind=TRUE)
for(o in seq_len(nrow(index))){
  i <- index[o,1]
  j <- index[o,2]
  prob <- p[i]/(p[i] + p[j]) # Bradley-Terry
  nobs <- 80 
  i_wins <- rbinom(1,nobs,prob)
  j_wins <- nobs - i_wins
  M[i,j] <- i_wins
  M[j,i] <- j_wins
}