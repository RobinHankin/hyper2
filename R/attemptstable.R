 `as.attemptstable` <- function(x){
    x <- as.data.frame(x)
    class(x) <- c("attemptstable", "data.frame")  # the only place
    return(x)
}

`print.attemptstable` <- function(x, ...){
    cat("An attemptstable:\n")
    class(x) <- "data.frame"
    x[is.na(x)] <- "X"
    print(x)
}

`suppfun.attemptstable` <- function(x, ...){
    attemptstable2supp3(x, ...)
}

as.vector.attemptstable <- function(x, mode){
    attemptstable2supp3(x, decreasing=TRUE, give.supp=FALSE, dnf.last=TRUE)
}

`attemptstable2supp3` <- function(x, decreasing=TRUE, give.supp=TRUE, dnf.last=TRUE){
    o <- c(matrix(suppressWarnings(as.numeric(c(x, recursive=TRUE))), nrow(x), ncol(x)))
    names(o) <- c(matrix(rownames(x), nrow(x), ncol(x)))
    o <- sort(o, na.last=TRUE, decreasing=decreasing)
    if(give.supp){
        if(dnf.last){
            nf <- names(o)[is.na(o)]
        } else {
            nf <- NULL
        }
        return(ordervec2supp3(v=names(o)[!is.na(o)], nonfinishers=nf))
    } else {
        return(o)
    }
}


