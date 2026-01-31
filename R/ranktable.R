`print.ranktablesummary` <- function(x, ...){
    x <- unclass(x)
    x <- cbind(x[,1:3], "...", x[,ncol(x) + c(-1,0)])
    x <- noquote(x)
    print(x)
    class(x) <- NULL
    return(invisible(x))
}

`summary.ranktable` <- function(object, ...){
    class(object) <- "ranktablesummary"
    return(object)
}

`ranktable_to_ordertable` <- function(xrank){
    out <- apply(xrank, 1, order)
    rownames(out) <- sort(unique(as.vector(xrank)))
    colnames(out) <- rownames(xrank)
    return(as.ordertable(out))
}

`ordertable_to_ranktable` <- function(xorder){
  stopifnot(all(apply(xorder, 2, function(x){all(sort(x) == seq_along(x))})))
  o <- t(apply(xorder, 2, function(x){seq_len(nrow(xorder))[order(x)]}))
  colnames(o) <- rownames(xorder)
  o <- rbind(o)
  if(is.null(colnames(o))){
    cn <- seq_len(ncol(o))
  } else {
    cn <- colnames(o)
  }
  out <- matrix(cn[o], ncol = ncol(o))
  colnames(out) <- paste("c", seq_len(ncol(o)), sep = "")
  rownames(out) <- rownames(o)
  return(as.ranktable(out))
}

setGeneric("as.ranktable", function(x){standardGeneric("as.ranktable")})

`is.ranktable` <- function(x){inherits(x, "ranktable")}
`as.ranktable` <- function(x){UseMethod("as.ranktable")}
`as.ranktable.ordertable` <- function(x){ordertable_to_ranktable(x)}
`as.ranktable.ranktable` <- function(x){x}
`as.ranktable.matrix` <- function(x){
    stopifnot(is.matrix(x))
    stopifnot(is.character(x))
    stopifnot(all(apply(x, 1, table) == 1))
    colnames(x) <- paste("c", seq_len(ncol(x)),sep="")
    class(x) <- "ranktable"  # this is the only place yadda yada ya
    return(x)
}

`print.ranktable` <- function(x,...){
    cat("A ranktable:\n")
    print(noquote(unclass(x)))
}

`suppfun.ranktable` <- function(x, times, ...){
  x <- rbind(x)  # deals with vectors
  if(missing(times)){times <- 1}
  times <- cbind(times, rep(1, nrow(x)))[,1]
  out <- hyper2()
  for(i in seq_len(nrow(x))){
      out <- out + suppfun(x[i, , drop=TRUE]) * times[i]
  } 
  return(out)
}
