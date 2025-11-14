#' @export
multinom <- function(x){
    stopifnot(all(x >= 0))
    xf <- as.frab(x)
    v <- disordR::elements(frab::values(xf)) # vector of integers
    n <- disordR::elements(names(xf))        # vector of names

    if(all(x > 0)){  # NB strict
        L <- c(n, list(n))
    } else {
        L <- c(n, list(unique(names(x))))
    }
    return(hyper2(L, c(v, -sum(v))))
}
