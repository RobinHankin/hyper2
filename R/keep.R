         
`tidy` <- function(H){
  wanted <- sort(unique(c(brackets(H),recursive=TRUE)))  # numeric

  o <- order(wanted)  # == seq_along(wanted)

  bracketout <- list()
  powerout <- NULL
  for(i in seq_along(H)){
    b <- brackets(H)[[i]]  # numeric (not necessarily sorted)
    if(any(b %in% wanted)){
      bracketout <- c(bracketout, list(which(apply(outer(b,wanted,"=="),2,any)))) # easily the ugliest line in any of my code, anywhere
      powerout <- c(powerout, powers(H)[i])
    }
  }
  if(identical(pnames(H),NA)){
    pout <- NA
  } else {
    pout <- pnames(H)[wanted]
  }
  return(hyper2(bracketout,powerout,pout))
}

`keep_flawed` <- function(H, wanted, tidy=TRUE){
  p <- pnames(H)    # might be NA
  if(is.character(wanted)){
    stopifnot(all(wanted %in% p))
    wanted <- which(p %in% wanted) # 'wanted' now numeric
  } else {
    jj <- seq_len(size(H))
    stopifnot(all(wanted %in% jj))
    wanted <- which(jj %in% wanted) # 'wanted' now numeric
  }

  bracketout <- list()
  powerout <- NULL
  for(i in seq_along(H)){
    b <- brackets(H)[[i]]
    jj <- b[b %in% wanted]   # the meat
    if(length(jj)>0){
      bracketout <- c(bracketout, list(jj))
      powerout <- c(powerout, powers(H)[i])
    }
  }
  out <-hyper2(L=bracketout,d=powerout,pnames=p)
  if(tidy){out <- tidy(out)}
  return(out)
}

`discard_flawed` <- function(H, unwanted, tidy=TRUE){
  p <- pnames(H)
  if(is.character(unwanted)){
    stopifnot(all(unwanted %in% p))
    wanted <- which(!(p %in% unwanted))
  } else {
      jj <- seq_len(size(H))
      stopifnot(all(unwanted %in% jj))
      wanted <- which(!(jj %in% unwanted))
  }
  return(keep_flawed(H,wanted=wanted, tidy=tidy))  # the meat
}
