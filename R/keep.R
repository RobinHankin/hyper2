         
`tidy` <- function(H){
  b <- elements(brackets(H))
  p <- elements(powers(H))
  hyper2(b,p,pnames=sort(unique(c(b,recursive=TRUE))))
}

`keep_flawed` <- function(H, wanted){
  p <- pnames(H)    # might be NA
  bH <- elements(brackets(H))
  pH <- elements(powers(H))
  stopifnot(is.character(wanted))
  stopifnot(all(wanted %in% p))

  bracketout <- list()
  powerout <- NULL
  for(i in seq_along(H)){
    b <- bH[[i]]
    jj <- b[b %in% wanted]   # the meat
    if(length(jj)>0){
      bracketout <- c(bracketout, list(jj))
      powerout <- c(powerout, pH[i])
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
