`as.suplist` <- function(L){
  if(inherits(L,"hyper2")){out <- list(L)}
  stopifnot(is.list(L))
  stopifnot(all(unlist(lapply(L,function(x){inherits(x,"hyper2")}))))
  class(L) <- c("list","suplist")
  return(L)
}

`Ops.suplist` <-
  function (e1, e2 = NULL) 
{
  f <- function(...){stop("odd---neither argument has class suplist?")}
  unary <- nargs() == 1
  lclass <- inherits(e1,"hyper2")
  rclass <- !unary && inherits(e2,"hyper2")
  
  if(unary){stop("Unary operators not implemented for suplist objects")}
  
    if (.Generic == "+"){
      return(suplist_add(e1,e2))
    } else {
      stop("Binary operator '", .Generic, "' is not implemented for hyper2 objects")
    }
}

`suplist_add` <- function(e1,e2){
  e1 <- as.suplist(e1)
  e2 <- as.suplist(e2)
  n <- length(e1)
  out <- list()
  for(i in seq_along(e1)){
    for(j in seq_along(e2)){
      out[[i + (j-1)*n]] <- e1[[i]] + e2[[j]]
    }
  }
  return(as.suplist(out))
}

`sum.suplist` <- function(x, ..., na.rm=FALSE){
  if(nargs()==1){
    return(x)
  } else if (nargs()==2){
    return(suplist_add(x, ...))
  } else {
    return(suplist_add(x, Recall(...)))
  }
}
