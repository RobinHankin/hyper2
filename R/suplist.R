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
  lclass <- inherits(e1,"suplist")
  rclass <- !unary && inherits(e2,"suplist")
  
  if(unary){stop("Unary operators not implemented for suplist objects")}
  
  if(.Generic == "+"){
      stopifnot(lclass & rclass)    
      return(suplist_add(e1,e2))
    } else if (.Generic == "*"){
      if(lclass & rclass){
          stop("<suplist> * <suplist> not defined")
      } else if(lclass & !rclass){  # W*6
          return(suplist_times_scalar(e1,e2))
      } else if (!lclass & rclass){ # 6*W
          return(suplist_times_scalar(e2,e1))
      } else {
	  f()
      }
    } else {
      stop("Binary operator '", .Generic, "' is not implemented for hyper2 objects")
    }
}

`suplist_times_scalar` <- function(e1,e2){
   stopifnot(e2==round(e2))
   stopifnot(length(e2)==1)
   stopifnot(e2>=0)
   if(e2==0){
     return(as.suplist*lapply(e1,function(x){x*0}))
   } else if(e2==1){
     return(e1)
   } else {
     out <- e1
     for(i in seq_len(e2-1)){out <- out + e1}
     return(out)
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
