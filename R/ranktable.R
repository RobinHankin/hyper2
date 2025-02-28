`ranktable_to_printable_object` <- function(x){
  x <- rbind(x)
  if(is.null(colnames(x))){
    cn <- seq_len(ncol(x))
  } else {
    cn <- colnames(x)
  }
  thing <- matrix(cn[x],ncol=ncol(x))
  colnames(thing) <- paste("c",seq_len(ncol(x)),sep="")
  rownames(thing) <- rownames(x)
  class(thing) <- "noquote"
  return(thing)
}

`print.ranktable` <- function(x,...){
    cat("A ranktable:\n")
    print(ranktable_to_printable_object(x))
}

`print.ranktablesummary` <- function(x,...){
    x <- ranktable_to_printable_object(x)
    class(x) <- "matrix"
    x <- cbind(x[,1:3],"...",x[,ncol(x)+c(-1,0)])
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
  out <- apply(xrank,1,order)
  rownames(out) <- colnames(xrank)
  return(out)
}

`ordertable_to_ranktable` <- function(xorder){
  stopifnot(all(apply(xorder,2,function(x){all(sort(x) == seq_along(x))})))
  out <- t(apply(xorder,2,function(x){seq_len(nrow(xorder))[order(x)]}))
  colnames(out) <- rownames(xorder)
  class(out) <- "ranktable"
  return(out)
}

setGeneric("as.ranktable", function(x){standardGeneric("as.ranktable")})
`as.ranktable` <- function(x){UseMethod("as.ranktable")}
`as.ranktable.ordertable` <- function(x){ordertable_to_ranktable(x)}

