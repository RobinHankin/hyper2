         
`tidy` <- function(H){
    hyper2(brackets(H),powers(H),pnames=sort(unique(c(brackets(H),recursive=TRUE))))
}

`keep_flawed` <- function(H, wanted){
  p <- pnames(H)    # might be NA
  stopifnot(is.character(wanted))
  stopifnot(all(wanted %in% p))

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
  hyper2(L=bracketout,d=powerout,pnames=p)
}

`discard_flawed` <- function(H, unwanted){
    p <- pnames(H)
    stopifnot(is.character(unwanted))
    stopifnot(all(unwanted %in% p))
    return(keep_flawed(H,wanted=p[!(p %in% unwanted)]))
}
