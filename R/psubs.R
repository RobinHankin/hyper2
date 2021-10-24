`psubs_single` <- function(H,from,to){
    stopifnot(length(from)==1)
    stopifnot(length(to)==1)
    b <- elements(brackets(H))
    p <- elements(powers(H))
    stopifnot(!(to %in% c(b,recursive=TRUE)))
    hyper2(lapply(b,function(x){x[x==from] <- to ; return(x)}),p)
}

`psubs` <- function(H,from,to){
    if(missing(to)){
        to <- from
        from <-pnames(H)
    }
    stopifnot(length(from) == length(to))
    wanted <- from %in% pnames(H)
    from <- from[wanted]
    to <- to[wanted]
        
    l <- length(from)
    if(l==0){
        return(H)
    } else if(l==1){
        return(psubs_single(H,from,to))
    } else {
        return(Recall(psubs_single(H,from[1],to[1]),from[-1],to[-1]))
    }
}
