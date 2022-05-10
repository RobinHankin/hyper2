`lsl` <- function(suplists,powers){
    stopifnot(is.list(suplists))
    stopifnot(unlist(lapply(suplists,function(x){inherits(x,"suplist")})))
    stopifnot(is.numeric(powers))
    stopifnot(is.vector(powers))
    stopifnot(length(powers) == length(suplists))
    out <- list(suplists=suplists,powers=powers)
    class(out) <- "lsl"
    return(out)
}

`loglik_lsl` <- function(p, LSL, log = TRUE){
    out <- 0
    for(i in seq_along(LSL$powers)){
        out <- out + LSL$powers[i]*log(like_single_list(p,LSL$suplists[[i]]))
    }
    if(log){
        return(out)
    } else {
        return(exp(out))
    }
}
    
`Ops.lsl` <- function(e1,e2){
    f <- function(...){stop("odd---neither argument has class suplist?")}
    unary <- nargs() == 1
    lclass <- inherits(e1,"lsl")
    rclass <- !unary && inherits(e2,"lsl")
    
    if(unary){stop("Unary operators not implemented for lsl objects")}
    
    if(.Generic == "+"){
        stopifnot(lclass & rclass)
        return(lsl_add(e1,e2))
    } else {
        stop(gettextf("binary operator %s not implemented for lsl objects", dQuote(.Generic)))
    }
}

`lsl_add` <- function(e1,e2){  # terribly inefficient, do not use
    lsl(
        suplists = c(e1$suplists,e2$suplists),
        powers   = c(e1$powers, e2$powers)
    )
}
