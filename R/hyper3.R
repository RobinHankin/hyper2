`hyper3` <- function(B=list(), W=list(), powers=0,pnames){
    if(all(unlist(lapply(B,is_ok_weightedplayers)))){
        return(hyper3_nv(B,powers,pnames))
    } else {
        return(hyper3_bw(B,W,powers,pnames))
    }
}

`hyper3_bw` <- function(B=list(), W=list(), powers=0,pnames){
                                        # hyper3_bw(list(c("a","b"),c("b","c","e")),list(c(1,4),c(1,3,9)),1:2,letters[1:5]) 
    stopifnot(length(B) == length(W))
    if(length(powers)==1){powers <- rep(powers,length(B))}
    if(missing(pnames)){pnames <- sort(unique(c(B ,recursive=TRUE)))}
    out <- identityL3(B,W,powers)  # the meat
    out$pnames <- pnames
    class(out) <- c('hyper3','hyper2')  # this is the only place class hyper3  is set
    return(out)
}

`hyper3_nv` <-  function(L=list(), powers=0, pnames){ # "nv" = Named Vectors
    ## hyper3_nv(list(c(a=1,b=4),c(b=1,c=3,e=9)),1:2,letters[1:5])
    stopifnot(all(unlist(lapply(L,is_ok_weightedplayers))))
    hyper3_bw(lapply(L,names),lapply(L,as.vector),powers,pnames)
}


`is_ok_weightedplayers` <- function(x){ # x=c(a=33,b=4,c=65)
    if(is.null(names(x))){
        return(FALSE)
    }
    if(any(is.null(names(x)))){
        return(FALSE)
    }
    if(any(x<0)){
        return(FALSE)
    }
    if(any(table(names(x))>1)){
        return(FALSE)
    }
    return(TRUE)
}

`is_valid_hyper3` <- function(L,d,pnames){
    if(length(L) != length(d)){stop("length of L must match length of d")}
    if(any(!(unique(c(lapply(L,names),recursive=TRUE)) %in% pnames))){
        stop("pnames must include all named elements")
    }
    return(TRUE)
}

`pnv` <- function(x){ # "pnv" == "print method for named vectors"
    paste(paste(names(x),unname(x),sep="="),collapse=", ")
}

`as.namedvectorlist` <- function(H3){
    out <- list()
    b <- disordR::elements(brackets(H3))
    w <- disordR::elements(weights(H3))
    for(i in seq_along(H3)){
        jj <- w[[i]]
        names(jj) <- b[[i]]
        out[[i]] <- jj
    }
    return(out)
}

`print.hyper3` <- function(x,...){
    b <- as.namedvectorlist(x)
    p <- elements(powers(x))
    out <- "log( "
    for(i in seq_along(b)){
        out <- paste(out, "(", pnv(b[[i]]),")^",p[i],sep="")
        if(i<length(b)){out <- paste(out, " * ", sep = "")}
    }
    out <- paste(out, ")\n", sep = "")
    for (i in strwrap(out)) {
        cat(noquote(i))
        cat("\n")
    }
    return(invisible(x))
}

`is.hyper3` <- function(H){inherits(H,"hyper3")}

`powers<-.hyper3` <- function(H,value){
    stopifnot(consistent(powers(H),value))
    if(!is.disord(value) & length(value)>1){stop("replacement not defined")}
    hyper3_nv(as.namedvectorlist(H),powers=value,pnames=pnames(H))
}

`hyper3_equal` <- function(e1,e2){
  equality3(
      elements(brackets(e1)), elements(weights(e1)),elements(powers(e1)),
      elements(brackets(e2)), elements(weights(e2)),elements(powers(e2))
      )
}

`hyper3_add` <- function(e1,e2){
  b1 <- elements(brackets(e1))
  b2 <- elements(brackets(e2))
  w1 <- elements(weights(e1))
  w2 <- elements(weights(e2))
  p1 <- elements(powers(e1))
  p2 <- elements(powers(e2))
  n1 <- pnames(e1)
  n2 <- pnames(e2)
  out <- addL3(b1,w1,p1,b2,w2,p2)
  
  if(all(n2 %in% n1)){
      jj <- n1
  } else if(all(n1 %in% n2)){
      jj <- n2
  } else {
      jj <- sort(unique(c(b1,b2,recursive=TRUE)))
  }
  
  return(hyper3(out[[1]],out[[2]],out[[3]],pnames=jj))
}

`Ops.hyper3` <-  function (e1, e2 = NULL) 
{
  f <- function(...){stop("odd---neither argument has class hyper3?")}
  unary <- nargs() == 1
  lclass <- inherits(e1,"hyper3")
  rclass <- !unary && inherits(e2,"hyper3")
  
  if(unary){
      if(.Generic == "+"){
          return(e1)
      } else if (.Generic == "-"){
          return(hyper2_prod(e1,-1))
      } else {
          stop(gettextf("Unary operator %s not defined for hyper3 objects", dQuote(.Generic)))
      }
  }  # if(unary) closes

  if (!is.element(.Generic, c("+", "-", "==", "!=", "*", "^" ))){
      stop(gettextf("binary operator %s not defined for hyper3 objects", dQuote(.Generic)))
  }
  
  if(lclass && rclass){  
    if (.Generic == "+"){
      return(hyper3_add(e1,e2))
    } else if (.Generic == "-"){
      return(hyper3_add(e1,hyper2_prod(e2,-1)))
    } else if (.Generic == "==") {
      return(hyper3_equal(e1, e2))
    } else if (.Generic == "!=") {
      return(!hyper3_equal(e1, e2))
    } else {
      stop(gettextf("H3 %s H3 not defined ", dQuote(.Generic)))
    }
  } else {  # one of lclass,rclass
    if (.Generic == "*"){    # H * n
      if(lclass && !rclass){
        return(hyper2_prod(e1,e2))
      } else if (!lclass && rclass){
        return(hyper2_prod(e2,e1))
      } else {
        stop("method not defined for hyper3")
      }
    } else if (.Generic == "+"){  # H + x
      if(lclass && !rclass){
        return(hyper2_sum_numeric(e1,e2))
      } else if (!lclass && rclass){
        return(hyper2_sum_numeric(e2,e1))
      } else {
        stop("this cannot happen")
      }
    } else if (.Generic == "-"){
      if(lclass && !rclass){
        return(hyper3_sum_numeric(e1,-e2))
      } else if (!lclass && rclass){
        return(hyper3_sum_numeric(e2,-e1))
      } else {
        stop("this cannot happen")
      }
    }
  }
}

# accessor3 <- function(L, W, powers, Lwanted, Wwanted) {}


`[.hyper3` <- function(x, ...){ # H3[list(c(a=1),c(a=1,b=2))]
    dots <- list(...)
    if(nargs() == 2){ # need to distinguish x[c(a=1,b=1)] or x[list(c(a=1,b=2),c(a=1,b=3))]
        if(is.list(dots[[1]])){dots <- dots[[1]]}
    }
    dots <- list(dots[[1]])

    out <- accessor3(
        L       = elements(brackets(x)),
        W       = elements(weights(x)),
        powers  = elements(powers(x)),
        Lwanted = lapply(dots,names),
        Wwanted = lapply(dots,as.vector)
                     )
    return(hyper3_bw(out[[1]],out[[2]],out[[3]],pnames=pnames(x)))
}

`assign_lowlevel3`<- function(x,index,value){ #H[index] <- value
    stopifnot(class(x) == 'hyper2')
    
    if(is.list(index)){
        ignore <- 3  # 'index' is supposed to be a list
    } else if(is.matrix(index)){
        index <- as.list(as.data.frame(t(index)))
    } else if(is.vector(index)){
        index <- list(index)
    } else {
        stop("replacement index must be a list, a matrix, or a vector")
    }
    value <- elements(value)
    stopifnot(is.numeric(value)) # coercion to integer is done in C
    stopifnot(is.vector(value))
    if(length(value)==1){
        value <- rep(value, length(index))
    }
    return(assigner(brackets(x),powers(x),index,value))
}

`overwrite_lowlevel3` <- function(x,value){
  stopifnot(class(x)     == 'hyper2')
  stopifnot(class(value) == 'hyper2')

  overwrite(brackets(x), powers(x), brackets(value), powers(value))
}

`[<-.hyper3` <- function(x, index, ..., value){
    if(missing(index)){  # A[] <- B
        jj <- overwrite_lowlevel(x,value)
        if(all(pnames(value) %in% pnames(x))){
            out <- hyper2(jj[[1]],jj[[2]],pnames=pnames(x))
        } else {
            out <- hyper2(jj[[1]],jj[[2]])
        }
    } else { # index supplied
        jj <- assign_lowlevel(x,index,value)
        if(all(c(index,recursive=TRUE) %in% pnames(x))){
            out <- hyper2(jj[[1]],jj[[2]],pnames=pnames(x)) # do not change pnames
        } else { # index introduces a new pname
            out <- hyper2(jj[[1]],jj[[2]])
        }
    }
    return(out)
}
