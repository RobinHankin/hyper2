`ordertable_to_preftable` <- function(M){
  choices <- rownames(M)
  judges <- colnames(M)
  n <- ncol(M) # number of judges
  m <- nrow(M) # number of choices
  out <- matrix(NA, nrow = n, ncol = m)
  rownames(out) <- NULL
  colnames(out) <- paste0("c",seq_len(m))
  
  for(i in seq_len(n)){
    out[i,] <- choices[order(M[,i])] # the meat
  }
  return(as.preftable(out))
}

`as.preftable` <- function(M){
  if(is.preftable(M)){return(M)}
  if(!is.matrix(M)){stop("argument M must be a matrix")}
  n <- nrow(M) # number of judges
  choices <- sort(M[1,])
  for(i in seq_len(n)[-1]){
    stopifnot(all(sort(M[i,]) == choices))
  }
  
  if(is.null(rownames(M))){
    rownames(M) <- paste0("judge", seq_len(n))
  }
  
  class(M) <- "preftable"
  return(M)
}

is.preftable <- function(M){inherits(M, "preftable")}
  
`print.preftable` <- function(x, ...){
  cat("A preftable object:\n")
  print(noquote(unclass(x)))
  return(invisible(x))
}

`preftable_to_ranktable` <- function(x) {
  stopifnot(is.preftable(x))
  n <- nrow(x) # number of judges
  m <- ncol(x) # number of choices
 
  choices <- sort(x[1,])
  out <- matrix(NA, nrow = n, ncol = m)
  rownames(out) <- rownames(x)
  colnames(out) <- choices
  
  for(i in seq_len(n)){
    out[i,] <- rank(x[i,]) # the meat
  }
  class(out) <- "ranktable"
  return(out)
}

`preftable_to_supp` <- function(x){
  H <- hyper2()
  for(i in seq_len(nrow(x))){
    H <- H + rankvec_likelihood(x[i,])
  }
  return(H)
}

if(FALSE){
a1 <- preftable_to_supp(as.preftable(sushi_table[1:5,]))
a2 <- ordertable2supp(ranktable_to_ordertable(preftable_to_ranktable(as.preftable(sushi_table[1:5,]))))
stopifnot(a1 == a2)

# following is wrong:
a3 <- rank_likelihood(preftable_to_ranktable(as.preftable(sushi_table[1:5,])))

jj <- as.preftable(sushi_table[1:5,])

a4 <- (
    rankvec_likelihood(jj[1,]) + 
    rankvec_likelihood(jj[2,]) + 
    rankvec_likelihood(jj[3,]) + 
    rankvec_likelihood(jj[4,]) + 
    rankvec_likelihood(jj[5,])
)
}
