`hyper3` <-  function(L=list(), powers=0, pnames){ # hyper3(list(c(a=1,b=4),c(b=1,c=3,e=9)),1:2,letters[1:5])
    if(length(powers)==1){powers <- rep(powers,length(L))}
    stopifnot(all(unlist(lapply(L,is_ok_weightedplayers))))
    if(missing(pnames)){pnames <- sort(unique(names(c(L) ,recursive=TRUE)))}
    stopifnot(is_valid_hyper3(L,powers,pnames))
    out <- identityL3(lapply(L,names),lapply(L,as.vector),powers)
    out$pnames <- pnames
    class(out) <- 'hyper3'  # This is the only class assignment in the package
    return(out)
}

`is_ok_weightedplayers` <- function(x){ # x=c(a=33,b=4,c=65)
    if(is.null(names(x))){stop("must be a named vector")}
    if(any(is.null(names(x)))){stop("all members must be named")}
    if(any(x<0)){stop("negative members not allowed")}
    if(any(table(names(x))>1)){stop("repeated player [maybe this should not be an error]")}
    return(TRUE)
}
   

`is_valid_hyper3` <- function(L,d,pnames){
    if(length(L) != length(d)){stop("length of L must match length of d")}
    if(any(!(unique(c(names(L))) %in% pnames))){stop("pnames must include all named elements")}
    return(TRUE)
}

