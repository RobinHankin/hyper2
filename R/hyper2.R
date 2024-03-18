`hyper2` <-  function(L=list(), d=0, pnames){
  if(length(d)==1){d <- rep(d,length(L))}
  if(missing(pnames)){pnames <- sort(unique(c(L,recursive=TRUE)))}
  stopifnot(is_valid_hyper2(L,d,pnames))
  out <- identityL(L,d)
  out$pnames <- pnames
  class(out) <- 'hyper2'  # This is the only class assignment in the package
  return(out)
}

## Following three functions are the only accessor methods in the package
setGeneric("brackets",function(x){standardGeneric("brackets")})
setGeneric("powers"  ,function(x){standardGeneric("powers"  )})
setGeneric("powers<-",function(x,value){standardGeneric("powers<-")})
setGeneric("pnames"  ,function(x){standardGeneric("pnames"  )})
`brackets` <- function(H){UseMethod("brackets")}
`powers`   <- function(H){UseMethod("powers"  )}
`powers<-` <- function(H,value){UseMethod("powers<-")}
`pnames`   <- function(H){UseMethod("pnames"  )}
`brackets.hyper2` <- function(H){disord(H$brackets,h=hashcal(H))}

`powers.hyper2` <- function(H){
  if(is_constant(H)){
    out <- 0
  } else {
    out <- H$powers
  }
  return(disord(out,h=hashcal(H)))
}

`powers<-.hyper2` <- function(H,value){
    stopifnot(consistent(powers(H),value))
    hyper2(elements(brackets(H)),elements(value))
}

`pnames.hyper2` <- function(H){ H$pnames }
`pnames.suplist` <- function(H){pnames(H[[1]])}
## accessor methods end

## Following function is the only setter method in the package
setGeneric("pnames<-",function(x,value){standardGeneric("pnames<-")})
`pnames<-` <- function(x,value){UseMethod("pnames<-")}
`pnames<-.hyper2` <- function(x,value){hyper2(elements(brackets(x)),elements(powers(x)),pnames=value)}

## setter methods end

`is.hyper2` <- function(H){inherits(H,"hyper2") & !inherits(H,"hyper3")}
`is.hyper3` <- function(H){inherits(H,"hyper2") &  inherits(H,"hyper3")}

`length.hyper2` <- function(x){length(x$brackets)}

`is_constant` <- function(H){ length(H)==0 }

`is_valid_hyper2` <- function(L,d,pnames){
  stopifnot(is.list(L))
  stopifnot(is.vector(d))
  stopifnot(is.numeric(d))
  stopifnot(length(L) == length(d))
  stopifnot(all(unique(c(L,recursive=TRUE)) %in% pnames))

  ## catch hyper2(list(c("a","a")),1):
  stopifnot(all(unlist(lapply(L,function(l){all(table(l)==1)})))) 
  return(TRUE)
}

`size` <- function(H){
  if(is.null(H)){return(0)}
  if(inherits(H,"suplist")){
      return(max(sapply(seq_along(H),function(i){size(H[[i]])})))
  }
  if(identical(pnames(H),NA)){
    return(max(c(brackets(H),recursive=TRUE)))
  } else {
    return(length(pnames(H)))
  }
}

`as.hyper2` <- function(L,d,pnames){
    if(is.matrix(L)){
        L <- as.list(as.data.frame(t(L)))
    } else if(is.hyper3(L)){
        return(hyper3_to_hyper2(L))
    } else if(is.list(L)){
        ignore <- 0
    } else {
        stop("first argument must be a list or a matrix")
    }
    return(hyper2(L,d,pnames))
}

`.print.helper` <- function(cv){  # Character vector
  if(length(cv)==1){return(cv)}
  out <- paste("(",cv[1],sep="")
  for(i in seq_along(cv)[-1]){ out <- paste(out," + ",cv[i],sep="") }
  out <- paste(out,")",sep="")
  return(out)
}
  
`print.hyper2` <- function(x,...){
  if(!isFALSE(getOption("give_warning_on_nonzero_power_sum"))){
      if(sum(powers(x)) !=0){
          warning("powers have nonzero sum")
      }
  }
  b <- elements(brackets(x))
  powers <- elements(powers(x))
  if(length(b)==0){  # not is.null(b)
      out <- paste(.print.helper(pnames(x)),"^0",sep="")
  }
  out <- "log("
  for(i in seq_along(b)){
    pn <- unlist(b[i])
    pp <- powers[i]
    if(pp==1){
      out <- paste(out, .print.helper(pn))
    } else {
      out <- paste(out, .print.helper(pn), "^",pp,sep="")
    }
    if(i < length(b)){out <- paste(out," * ",sep="")}
  }
  
  out <- paste(out,")\n",sep="")
  
  for(i in strwrap(out)){
    cat(noquote(i))
    cat("\n")
  }
  return(invisible(x))
}

`hyper2_add` <- function(e1,e2){
  b1 <- elements(brackets(e1))
  b2 <- elements(brackets(e2))
  p1 <- elements(powers(e1))
  p2 <- elements(powers(e2))
  n1 <- pnames(e1)
  n2 <- pnames(e2)
  out <- addL(b1,p1,b2,p2)
  
  if(all(n2 %in% n1)){
      jj <- n1
  } else if(all(n1 %in% n2)){
      jj <- n2
  } else {
      jj <- sort(unique(c(b1,b2,recursive=TRUE)))
  }
  
  return(hyper2(out[[1]],out[[2]],pnames=jj))
}

`loglik` <- function(p,H,log=TRUE){
  if(is.matrix(p)){
    return(apply(p,1,function(o){loglik_single(p=o,H, log=log)}))
  } else {
    return(loglik_single(p,H,log=log))
  }
}

`loglik_single` <- function(p,H,log=TRUE){
  stopifnot(sum(powers(H))==0)
  stopifnot(all(p>=0))
  if(length(p) == size(H)-1){
    stopifnot(sum(p)<=1)
    probs <- fillup(p)
  } else if(length(p) == size(H)){
    if(is.null(names(p))){stop("p==size(H), p must be a named vector")}
    stopifnot(abs(sum(p)-1) < 1e-6)  # small numerical tolerance
    p <- ordertrans(p,H)  # no warning given if names not in correct order...
    stopifnot(identical(names(p),pnames(H))) #...but they must match up
    probs <- p
  } else {
    stop("length(p) must be either size(H) or size(H)-1")
  }
  if(is.hyper2(H)){
      out <- evaluate(brackets(H), powers(H), probs=probs, pnames=pnames(H))
  } else if(is.hyper3(H)){
      out <- evaluate3(brackets(H), weights(H),powers(H), probs=probs, pnames=pnames(H))
  } else {
      stop("H must be a hyper2 or hyper3 object")
  }

  if(log){
    return(out)
  } else {
    return(exp(out))
  }
}

`hyper2_equal` <- function(e1,e2){
  equality(
      brackets(e1),powers(e1),
      brackets(e2),powers(e2)
  )
}

`hyper2_sum_numeric` <- function(H,r){
  pH <- powers(H)
  if(length(pH) == 1){
    return(pH+r)
  } else {
    stop('H + r is only defined if length(powers(H))==1 because the order of the powers is undefined')
  }
}

`hyper2_prod` <- function(H,n){
    powers(H) <- powers(H)*n
    return(H)
}

`Ops.hyper2` <-
  function (e1, e2 = NULL) 
{
  f <- function(...){stop("odd---neither argument has class hyper2?")}
  unary <- nargs() == 1
  lclass <- inherits(e1,"hyper2")
  rclass <- !unary && inherits(e2,"hyper2")
  
  if(unary){
    stop("unary operator '", .Generic, "' is not implemented for hyper2 objects")
    }

  if (!is.element(.Generic, c("+", "-", "==", "!=", "*"))){
    stop("binary operator '", .Generic, "' is not implemented for hyper2 objects")
  }
  
  if(lclass && rclass){  
    if (.Generic == "+"){
      return(hyper2_add(e1,e2))
    } else if (.Generic == "-"){
      return(hyper2_add(e1,hyper2_prod(e2,-1)))
    } else if (.Generic == "==") {
      return(hyper2_equal(e1, e2))
    } else if (.Generic == "!=") {
      return(!hyper2_equal(e1, e2))
    } else {
      stop("H1 ", .Generic, " H2 not defined")
    }
  } else {  # one of lclass,rclass
    if (.Generic == "*"){    # H * n
      if(lclass && !rclass){
        return(hyper2_prod(e1,e2))
      } else if (!lclass && rclass){
        return(hyper2_prod(e2,e1))
      } else {
        stop("method not defined for hyper2")
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
        return(hyper2_sum_numeric(e1,-e2))
      } else if (!lclass && rclass){
        return(hyper2_sum_numeric(e2,-e1))
      } else {
        stop("this cannot happen")
      }
    }
  }
}
`character_to_number` <- function(char,pnames){ # char = c("a","b","D")
    stopifnot(all(char %in% pnames))
    unlist(apply(outer(char,pnames, "=="),1,which))
    }

`char2num` <- character_to_number

`[.hyper2` <- function(x, ...){
    dots <- list(...)
    first <- dots[[1]]

    if(nargs() > 2){  # something like H[3,4]
      wanted <- list(c(dots,recursive=TRUE))
    } else if(is.list(first)){
      wanted <- dots[[1]]
    } else if(is.matrix(first)){
        wanted <- as.list(as.data.frame(t(first)))
    } else if(is.disord(first)){
        return(hyper2(elements(brackets(x)[first]),elements(powers(x)[first])))
    } else if(is.vector(first)){
      wanted <- list(first)
    } else {
      wanted <- dots
    }

    out <- accessor(x[[1]],x[[2]],wanted)
    return(hyper2(out[[1]],out[[2]],pnames=pnames(x)))
}

`assign_lowlevel`<- function(x,index,value){ #H[index] <- value
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

`overwrite_lowlevel` <- function(x,value){
  stopifnot(class(x)     == 'hyper2')
  stopifnot(class(value) == 'hyper2')

  overwrite(brackets(x), powers(x), brackets(value), powers(value))
}

`[<-.hyper2` <- function(x, index, ..., value){
    if(inherits(value,"weight")){
        out <- setweight(as.hyper3(x),index,value)
    } else if(missing(index)){  # A[] <- B
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

`gradient` <- function(H,probs=indep(maxp(H))){
    if(length(probs) == size(H)){
        jj <- probs
    } else if(length(probs) == size(H)-1){
        jj <- fillup(probs)
    } else {
        stop("probs is wrong length")
    }
    if(is.hyper2(H)){
        return(differentiate(brackets(H), powers(H), jj, pnames(H), size(H))$grad_comp)
    } else {
        return(differentiate3(brackets(H), weights(H), powers(H), jj, pnames(H), size(H))$grad_comp)
    }
}

`gradientn` <- function(H,probs=maxp(H)){
  stopifnot(length(probs) == size(H)) # not necessarily summing to 1!
  out <- differentiate_n(brackets(H), powers(H), probs, pnames(H),size(H))$grad_comp
  names(out) <- pnames(H)
  return(out)
}

`hessian` <- function(H, probs=indep(maxp(H)),border=TRUE){
    n <- size(H)
    stopifnot(length(probs) == n-1)
    out <- hessian_lowlevel(brackets(H),powers(H),fillup(probs),pnames(H),n)$block_hessian_components
    out <- matrix(out[seq_len((n-1)^2)],n-1,n-1)
    if(isFALSE(border)){return(out)}
    jj <- gradient(H,probs=probs)
    out <- cbind(rbind(out,jj),c(jj,0))
    rownames(out) <- pnames(H)
    colnames(out) <- pnames(H)
    return(out)
}

`is_ok_hessian` <- function(M,give=TRUE){
    stopifnot(is.matrix(M))
    stopifnot(nrow(M) == ncol(M))
    n <- nrow(M)
    M <- M[n:1,n:1]
    s <- seq(from=3,to=n)
    alt <- (s%%2) == 1  # T,F,T,F,T...
    jj <- sapply(s,function(n){det(M[seq_len(n),seq_len(n)])})
    if(give){return(jj)} else {return(all((jj>0) == alt))}
}

`fillup` <- function(x,H=NULL,total=1){
  if(is.matrix(x)){
    out <- cbind(x,total-rowSums(x))
    if(!is.null(H)){colnames(out) <- pnames(H)}
  } else {  # assumed to be a vector
    out <- c(x,total-sum(x))
    if(!is.null(H)){names(out) <- pnames(H)}
  }
  return(out)
}

`indep` <- function(x){
   if(is.matrix(x)){
     return(x[,-ncol(x)])
  } else {  # assumed to be a vector
    return(x[-length(x)])
  }
}

`maxp` <- function(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, n=1, show=FALSE, justlikes=FALSE, ...){
    if(isTRUE(getOption("use_alabama"))){ms <- maxp_single2} else {ms <- maxp_single}
    best_so_far <- -Inf # best (i.e. highest) likelihood found to date
    likes <- rep(NA,n)
    if(is.null(startp)){ startp <- indep(equalp(H)) }
    if(length(startp) == size(H)){startp <- indep(startp)}
    for(i in seq_len(n)){
        if(i>1){startp <- startp+runif(size(H)-1,max=SMALL/size(H))}
        jj <- ms(H, startp=startp, give=TRUE, fcm=fcm, fcv=fcv, SMALL=SMALL, ...)
        likes[i] <- jj$value
        if(show){cat(paste(i,"; ", best_so_far, "  " , jj$value,"\n", sep=""))}
        if(jj$value > best_so_far){ # that is, if we have found something better
            out <- jj
            best_so_far <- jj$value
        }
    }  # i loop closes
    if(justlikes){ return(likes) }
    if(give){
        return(c(out,likes=list(likes),evaluate=list(fillup(out$par,H))))
    } else {
      return(fillup(out$par,H))
    }
}  # maxp() closes

`maxp_single` <- function(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, maxtry=100, ...){
    if(inherits(H,"suplist")){return(maxplist(Hlist=H,startp=startp,give=give,fcm=fcm,fcv=fcv,...))}
    if(inherits(H,"lsl"    )){return(maxp_lsl(Hlist=H,startp=startp,give=give,fcm=fcm,fcv=fcv,...))}
    
    if(length(startp) == size(H)){startp <- indep(startp)}

    n <- size(H)
    if(is.null(startp)){
        startp <- rep(1/n,n-1)
    }

    objective <- function(p){ -loglik(p,H) }
    gradfun   <- function(p){ -(gradient(H,p))} #NB minus signs
    
    UI <- rbind(
        diag(nrow=n-1),  # regular: p1 >=0, p2 >= 0, ..., p_{n-1} >= 0
        -1,              # fillup: p_n = 1-(p1+p2+...+p_{n-1}) >= 0
        fcm)             # further problem-specific constraints
    CI <- c(
        rep(SMALL,n-1),  # regular
        -1+SMALL,        # fillup
        fcv)             # further contraint vector

    ## repeat until stupid wmmin bug does not occur...
    teenytiny <- 1e-15
    startp_try <- startp
    for(i in seq_len(maxtry)){
        out <- try(constrOptim(
            theta = startp_try,
            f     = objective,
            grad  = gradfun,
            ui    = UI,
            ci    = CI,
            ...))
        if(!inherits(out, "try-error")){  # worked!
            break
        } else { # stupid wmmin bug occurs
            cat("known R bug (bugzilla ID 17703; wmmin not finite).  Kludge:  maxp_single() will try  a slightly different start point\n")
            startp_try <- startp + teenytiny*(runif(length(startp))-0.5)
            teenytiny <- teenytiny * 1.1
        }
    }
    if(i==maxtry){  # stupid wmmin bug occurs every time...
        stop("known R bug (bugzilla ID 17703).  Try increasing maxtry.")
    }
        

    out$value <- -out$value # correct for -ve sign in objective()
    
    if(give){
      return(out)
    } else {
      jj <- fillup(out$par)
      if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
      return(jj)
    }
}

## Function maxp_single2() is like maxp_single() but uses the alabama
## package for constrained optimization instead of constrOptim() with
## its stupid wmmin finite error.




## above takes UI and CI [intended for constrOptim()] and returns a function

`maxp_single2` <- function(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, maxtry=100, ...){
    if(inherits(H,"suplist")){return(maxplist(Hlist=H,startp=startp,give=give,fcm=fcm,fcv=fcv,...))}
    
    n <- size(H)
    if(is.null(startp)){
        startp <- rep(1/n,n-1)
    }

    if(length(startp) == size(H)){startp <- indep(startp)}

    objective <- function(p){ -loglik(p,H) }
    gradfun   <- function(p){ -(gradient(H,p))} #NB minus signs
    
    UI <- rbind(
        diag(nrow=n-1),  # regular: p1 >=0, p2 >= 0, ..., p_{n-1} >= 0
        -1,              # fillup: p_n = 1-(p1+p2+...+p_{n-1}) >= 0
        fcm)             # further problem-specific constraints
    CI <- c(
        rep(SMALL,n-1),  # regular
        -1+SMALL,        # fillup
        fcv)             # further contraint vector

        out <- constrOptim.nl(
            par = startp,
            fn = objective,
            gr  = gradfun,
            hin = function(x){drop(UI%*%x - CI)},
            control.outer = list(trace=FALSE),
            ...)
   
    out$value <- -out$value # correct for -ve sign in objective()
    
    if(give){
      return(out)
    } else {
      jj <- fillup(out$par)
      if(!identical(pnames(H),NA)){names(jj) <- pnames(H)}
      return(jj)
    }
}

`maxp_simplex` <- function(H, n=100, show=FALSE, give=FALSE, ...){
    if(isTRUE(getOption("use_alabama"))){ms <- maxp_single2} else {ms <- maxp_single}
    best_so_far <- -Inf # best (i.e. highest) likelihood found to date
    likes <- rep(NA,n)
    for(i in seq_len(n)){
        jj <- ms(H, startp=indep(rp_unif(1,H)), give=TRUE, ...)
        likes[i] <- jj$value
        if(show){cat(paste(i,"; ", best_so_far, "  " , jj$value,"\n", sep=""))}
        if(jj$value > best_so_far){ # that is, if we have found something better
            out <- jj
            best_so_far <- jj$value
        }
    }  # i loop closes, stop searching
    if(give){
        return(c(out,likes=list(likes)))
    } else {
        out <- fillup(out$par)
        if(!identical(pnames(H),NA)){names(out) <- pnames(H)}
        return(out)
    }
}

`maxplist` <- function (Hlist, startp = NULL, give = FALSE, fcm = NULL, fcv = NULL, SMALL=1e-6, ...){
    n <- size(Hlist[[1]])
    if (is.null(startp)) {
        startp <- rep(1/n, n - 1)
    }
    objective <- function(p) {
        -like_single_list(p, Hlist)
    }
    
    UI <- rbind(diag(nrow = n - 1), -1, fcm)
    CI <- c(rep(SMALL, n - 1), -1 + SMALL, fcv)

    if(isTRUE(getOption("use_alabama"))){
        out <- constrOptim.nl(par = startp, fn = objective, gr = NULL, 
                              hin = function(x){drop(UI%*%x - CI)},
                              control.outer = list(trace=FALSE),...)
    } else {
        out <- constrOptim(theta = startp, f = objective, grad = NULL, 
                           ui = UI, ci = CI, ...)
    }
        out$value <- -out$value
        if (give) {
        return(out)
    }
    else {
        jj <- fillup(out$par)
        return(jj)
    }
}

`maxp_lsl` <- function (HLSL, startp = NULL, give = FALSE, fcm = NULL, fcv = NULL, SMALL=1e-6, ...){
    allpnames <- pnames(HLSL[[1]][[1]])
    n <- length(allpnames)

    if (is.null(startp)) {
        startp <- rep(1/n, n)
	names(startp) <- allpnames
        startp <- indep(startp)
    }
    
    objective <- function(p) { -loglik_lsl(p, HLSL) }
    
    UI <- rbind(diag(nrow = n - 1), -1, fcm)
    CI <- c(rep(SMALL, n - 1), -1 + SMALL, fcv)
    
    if(isTRUE(getOption("use_alabama"))){
        out <- constrOptim.nl(par = startp, fn = objective, gr = NULL, 
                              hin = function(x){drop(UI%*%x - CI)},
                              control.outer = list(trace=FALSE),...)
    } else {
        out <- constrOptim(theta=startp,f=objective,grad = NULL,ui=UI,ci=CI,...)
    }
    out$value <- -out$value
    if (give) {
        return(out)
    }
    else {
        jj <- fillup(out$par)
        names(jj) <- allpnames
        return(jj)
    }
}

`sum.hyper2` <- function(x, ..., na.rm=FALSE){
  if(nargs()==1){
    return(x)
  } else if (nargs()==2){
    return(hyper2_add(x, ...))
  } else {
    return(hyper2_add(x, Recall(...)))
  }
}

`head.hyper2` <- function(x,...){ x[head(elements(brackets(x)),...)] }

`rank_likelihood` <- function(M,times=1){
  M <- rbind(M)  # deals with vectors
  times <- cbind(seq_len(nrow(M)),times)[,2]
  if(is.null(colnames(M))){
    cn <- NA
  } else {
    cn <- colnames(M)
  }
  out <- hyper2(pnames=cn)
  for(i in seq_len(nrow(M))){
      out <- out + ordervec2supp(rev(M[i,,drop=TRUE]))
  } 
  return(out)
}

`rankvec_likelihood` <- function(v){
  stopifnot(all(table(v)==1))
  out <- hyper2()
  v <- rev(v)   # first-placed competitor is the first element of v
  for(i in seq_along(v)){
    out[v[i]] <- out[v[i]] + 1  #  out[v[i]] %<>% inc
    out[v[seq_len(i)]] <- out[v[seq_len(i)]] - 1
  }
  return(out)
}

`race` <- rankvec_likelihood

#`addrank` <- function(H,ranks){
#     .Defunct(new = 'rank_likelihood',msg='use H <- H+rank_likelihood(...)')
#     ranks <- rbind(ranks)
# 
#     for(r in seq_len(nrow(ranks))){
#         rank <- rev(ranks[r,])
#         for(i in seq_along(rank)){
#             H[rank[i]] <- powers(H[rank[i]]) + 1
#             H[rank[seq_len(i)]] <- powers(H[rank[seq_len(i)]])-1
#         }
#     }
#     return(H)
# }

`rrank_single` <- function(p){
    ell <- length(p)
    r <- integer(ell)
    i <- 1   # 'i' means placing.
    while(any(p>0)){  
        m <- sample(ell,1,prob=p) #randomly chosen competitor
        r[i] <- m   # place 'i' is competitor 'm'  
        p[m] <- 0   # competitor 'm' can't place again.
        i <- i+1    # consider the next place
    }
    return(r)
}

rorder_single <- function(p){
    o <- rrank_single(p)
    o[o] <- seq_along(o)
    return(o)
}

`rrank` <- function(n=1, p, pnames=NULL, fill=FALSE, rnames=NULL){
    if(fill){ p <- fillup(p) }
    if(is.null(pnames)){pnames <- names(p)}
    out <- t(replicate(n, rrank_single(p)))
    colnames(out) <- pnames
    rownames(out) <- rnames
    class(out) <- "ranktable"
    return(drop(out))
}

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
  stopifnot(all(apply(xorder,2,function(x){all(sort(x)==seq_along(x))})))
  out <- t(apply(xorder,2,function(x){seq_len(nrow(xorder))[order(x)]}))
  colnames(out) <- rownames(xorder)
  class(out) <- "ranktable"
  return(out)
}

`.allorders` <- function(x){
  out <- perms(length(x))
  matrix(x[out],nrow=length(x))
}

`pair_grid` <- function(a,b){  ## sort of a  generalized expand.grid()
  rbind(
      kronecker(a,t(rep(1L,ncol(b)))),
      kronecker(t(rep(1L,ncol(a))),b))
}

`mult_grid` <- function(L){
  if(length(L) == 0){
    return(1)
  } else if(length(L)==1){
    return(L)
  } else {
    return(Recall(c(list(pair_grid(L[[1]],L[[2]])),L[-(1:2)])))
  }
}

`general_grouped_rank_likelihood` <- function(H, ...){
  ## ggrl(1:3,4,5:9) means group(1:3) came first, 4 in the middle, 5:9
  ## last
    dotargs <- list(...)
    if(any(unlist(lapply(dotargs, is.character)))){
        dotargs <- lapply(dotargs, character_to_number, pnames=pnames(H))
    }

    if(any(table(c(dotargs,recursive=TRUE))>1)){
        stop('repeated competitor')
    }

    out <- mult_grid(lapply(dotargs, .allorders))[[1]]
    out[] <- pnames(H)[out]
    out <- apply(out,2,rankvec_likelihood)
    for(i in seq_along(out)){
        pnames(out[[i]]) <- pnames(H)
    }
    return(as.suplist(out))
}

`ggrl` <- general_grouped_rank_likelihood

`choose_losers` <- function(H,all_players,losers){
  stopifnot(losers %in% all_players)
  winners <- all_players[!all_players %in% losers]
  ggrl(H,winners,losers)
}

`choose_winners` <- function(H,all_players, winners){
  stopifnot(all(winners %in% all_players))
  losers <- all_players[!all_players %in% winners]
  ggrl(H,winners,losers)
}


`goodbad` <- function(winners,losers){
    stopifnot(!any(winners %in% losers))
    ggrl(hyper2(pnames=c(winners,losers),winners,losers))
}

`elimination` <- function(all_players){
    if(is.numeric(all_players) & (length(all_players)==1)){
      all_players <- letters[seq_len(all_players)]
    }
    all_players <- rev(all_players)
    H <- ggrl(hyper2(pnames=sort(all_players)),all_players,all_players[length(all_players)])
    players <- all_players[-length(all_players)]
    while(length(players)>1){
        for(i in seq_along(H)){
            H[[i]] <- ggrl(H[[i]],players,players[length(players)])
        }
        H <- unlist(H,recursive=FALSE)
        players <- players[-length(players)]
    }
    return(as.suplist(H))
}

`like_single_list` <- function(p,Lsub){;  # eg like_single(p,Lc)
  sum(unlist(lapply(Lsub,loglik,p=p,log=FALSE)))
  ## sum, because it can be any one of the orders specified in the
  ## elements of Lsub
}

`like_series` <- function(p,L,log=TRUE){
    out <- prod(unlist(lapply(L, function(Lsub){like_single_list(p,Lsub)})))
    if(log){ out <- log(out) }
    return(out)
}

`dirichlet` <-  function(powers, alpha){
    if(!xor(missing(powers),missing(alpha))){
        stop("supply exactly one of powers, alpha")
    }
  
    if(missing(powers)){
        powers <- alpha-1
    }

    np <- names(powers)
    if(is.null(np)){stop("supply a named vector")}
    hyper2(as.list(np),d=powers) - hyper2(list(np),d=sum(powers))
}

`GD` <- function(alpha, beta, beta0=0){
  k <- length(alpha)
  stopifnot(length(beta) == k-1)
  
  H <- dirichlet(powers=alpha-1)
  for(i in 2:(k-1)){
    H[names(alpha)[i:k]] <- beta[i-1] -(alpha[i]+beta[i])
  }

  H[names(alpha)[k]] <- beta[k-1]-1
  H[names(alpha)] <- beta0-(alpha[1]+beta[1])
  return(H)   
}

`GD_wong` <- function(alpha, beta){
  k <- length(alpha)
  stopifnot(length(beta) == k)

  gamma <- beta[-k]-alpha[-1]-beta[-1]
  gamma <- c(gamma, beta[k]-1)
  
  H <- dirichlet(powers=alpha-1)
  jj <- gamma[length(gamma)]
  H[names(jj)] <- jj
  for(i in seq_len(k-1)){
    H[names(alpha)[(i+1):(k)]] <- gamma[i]
  }
  return(H)   
}

`equalp` <- function(H){
    n <- size(H)
    out <- rep(1/n,n)
    names(out) <- pnames(H)
    return(out)
}

`zipf` <- function(n){
  nn <- NULL
  if(is.hyper2(n)){
    nn <- pnames(n)
    n <- size(n)
  }
  jj <- 1/seq_len(n)
  names(jj) <- nn
  jj/sum(jj)
}

`saffy` <- function(M){
    out <- hyper2(pnames=colnames(M))
    for(i in seq_len(nrow(M))){
        onerow <- M[i,]
        choices <- names(onerow[!is.na(onerow)])
        out[choices] %<>%  `-`(sum(onerow,na.rm=TRUE))
    }
    for(j in seq_len(ncol(M))){
        out[colnames(M)[j]] %<>% `+`(sum(M[,j],na.rm=TRUE))
    }
    return(out)
}

`volley` <- function(M){
    out <- hyper2()
    pn <- colnames(M)
    if(is.null(pn)){stop("colnames of matrix are NULL")}

    for(i in seq_len(nrow(M))){
        onerow <- M[i,]
        winning_side <- onerow==1
        losing_side  <- onerow==0
        playing <- !is.na(onerow)
        if(any(playing) & any(winning_side) & any(losing_side)){
          out[pn[which(winning_side)]] %<>% inc
          out[pn[which(playing)]] %<>% dec
        }
    }
    return(out)
}

`inc` <- function(H,val=1){ H %<>% `+`(val)}   # increment
`dec` <- function(H,val=1){ H %<>% `-`(val)}   # decrement

`trial` <- function(winners,players,val=1){
    stopifnot(all(winners %in% players))
    H <- hyper2()
    H[winners] %<>% `+`(val)
    H[players] %<>% `-`(val)
    return(H)
}   

`rhyper2` <- function(n=8, s=5,  pairs=TRUE, teams=TRUE, race=TRUE, pnames=letters){
  n <- n - n%%2  # Force 'n' to be even
  H <- hyper2()
  if(pairs){
      M <- replicate(s,sample(n,2,replace=FALSE),simplify=TRUE)
      M[] <- pnames[M]
      H <- hyper2(split(M,rep(seq_len(ncol(M)),each=nrow(M))),-1)
      H <- H + hyper2(as.list(M[1,]),1)
  }

  if(teams){
      for(i in seq_len(s)){
          players <- pnames[seq_len(n)]
          H <- H + trial(sample(players,n/2,replace=FALSE),players)
      }
  }

  if(race){  # Plackett-Luce
      for(i in seq_len(s)){
          H <- H + rankvec_likelihood(pnames[sample(n)])
    }
  }

  return(H)
}

`ordervec2supp` <- function(d){
    if(is.null(names(d))){stop("vector must be named")}
    wanted <- d!=0
    if(any(sort(d[wanted]) != seq_len(sum(wanted)))){
        stop("nonzero elements of d should be 1,2,3,4,...,n")
    }
    
    nd <- names(d)
    out <- hyper2()
    while(any(d>0)){
        eligible <- which(d>=0)  #NB inclusive inequality; zero is DNC etc who

        ## Increment numerator power of the first choice among eligible players:
        out[nd[d==1]] %<>% inc
        
        ## Power of set of all eligible players decrements:
        out[nd[eligible]] %<>% dec
        
        ## once you've come first in the field, you are ineligible to be first again:
        d[d==1] <- -1  # NB strictly <0
        
        ## Now, everyone moves down the list, so who *was* in
        ## second place becomes first place, who *was* third place
        ## becomes second, and so on:
        
        d[d>0] %<>% dec
        
    } # while() loop closes
    return(out)
}


`complete` <- function(a,noscore=NULL,check=FALSE){
  if(is.null(noscore)){
    noscore <- c("Ret", "WD", "DNS", "DSQ", "DNP", "NC", "DNQ", "EX", "Sick")
  }
  
  out <- apply(a,2,
               function(v){
                 v[v %in% noscore] <- "0"
                 v <- as.numeric(v)
                 if(all(sort(v[v>0]) == seq_along(v))){
                   v[v>0] <- rank(v[v>0])
                   return(v)
                 } else {
                   stop("some ranks missing")
                 }
               } )
  rownames(out) <- rownames(a)
  return(out)
}

`ordertable2supp_new` <- function(x, noscore=NULL, check=FALSE){
    x <- complete(x,noscore=noscore,check=check)
    out <- hyper2()
    
    ## Now cycle through the rows; each row is a venue [voter]
    for(i in seq_len(ncol(x))){
        o <- x[,i,drop=TRUE]
        out %<>% `+`(ordervec2supp(o))
    } # i loop closes
    return(out)
}

`ordertable2supp` <- function(x, noscore, incomplete=TRUE){
    if(missing(noscore)){
        noscore <- c("Ret", "WD", "DNS", "DSQ", "DNP", "NC", "DNQ", "EX", "Sick")
    }
    venues <- colnames(x)

    ## Now create a numeric matrix, fmat.  Two steps: first, count
    ## any no-score as zero:
  
    jj <- apply(x,2,function(y){
        if(any(y %in% noscore)){y[y%in%noscore] <- 0}
        return(y)
    })
    
    ## Second, convert to numeric and strip out names; transpose of
    ## x (because we want each row to be a venue):
    fmat <- matrix(as.numeric(jj),byrow=TRUE,ncol=nrow(x)) 
    colnames(fmat) <- rownames(x)
    rownames(fmat) <- venues
    
    out <- hyper2(d=ncol(fmat))
    
    ## Now cycle through the rows; each row is a venue [voter]
    for(i in seq_len(nrow(fmat))){
        o <- fmat[i,,drop=TRUE]
        if(incomplete){ o[o>0] <- rank(o[o>0]) }
        out %<>% `+`(ordervec2supp(o))
    } # i loop closes
    return(out)
} 

`zapweak` <- function(H, minstrength=1e-5, maxit, ...){
  if(missing(maxit)){maxit <- size(H)-1}
  for(i in seq_len(maxit)){
    cat(paste("iteration ",i,", size(H) = ",size(H),"\n",sep=""))
    m <- maxp(H,n=1,...)
    too_weak <- m < minstrength
    if(any(too_weak)){
      H %<>% discard_flawed(names(m[which.min(m)]))
    } else {
     break
    }
  }
  return(H)
}

`pwa` <- function(H,pwa,chameleon='S'){  

    stopifnot(pwa %in% pnames(H))
    stopifnot(!(chameleon %in% pnames(H)))  # ... check that the chameleon isn't already a competitor, and
  
    B <- elements(brackets(H))
    ## overwrite B in place:
    for(i in seq_along(B)){
        if(any(pwa %in% B[[i]])){ B[[i]] <- c(B[[i]],chameleon) }
    }
    return(hyper2(B,elements(powers(H))))
}

`wikitable_to_ranktable`  <- function(wikitable, strict=FALSE){
  f <- function(x){  # deal with DNF etc and zero
    suppressWarnings(out <- as.numeric(as.vector(x)))
    DNF <- is.na(out) | (out==0)
    if(sum(DNF)>1 & strict){
      warning("more than one competitor did not place.  EM algorithm used")
    }
    out[DNF] <- max(out[!DNF]) + seq_len(sum(DNF))
    return(out)
  }

  xx <- apply(wikitable,2,f)
  rownames(xx) <- rownames(wikitable)
  ordertable_to_ranktable(xx)
}

`is.dirichlet` <- function(H){
    len <- unlist(elements(lapply(brackets(H), length)))
    pow <- elements(powers(H))
    bra <- elements(brackets(H))
    if (sum(len > 1) > 1) {
        print("brackets must all be of length 1 [the numerators] except one [the denominator]")
        return(FALSE)
    }
    denominator_bracket <- unlist(bra[len > 1])
    numerator_brackets <- unlist(bra[len == 1])
    if (all(numerator_brackets %in% denominator_bracket)) {
        return(TRUE)
    }
    else {
        print("Dirichlet distribution requires denominator to be all competitors")
        return(FALSE)
    }
}

`rdirichlet` <- function(n, H){
    if(is.hyper2(H)){
        if(is.dirichlet(H)){
            alpha <- powers(H)+1
            pn <- pnames(H)
            s <- size(H)
        } else { # hyper2, but not dirichlet
            stop("hyper2 object supplied but is not a Dirichlet distribution")
        }
    } else {   # H is vector of alpha
        pn <- names(H)
        s <- length(H)
        alpha <- H
    }
    out <- t(apply(matrix(rgamma(n*s,shape=alpha),s,n),2,function(x){x/sum(x)}))
    colnames(out) <- pn
    return(out)
}

`rp_unif` <-function(n,H){
    stopifnot(is.hyper2(H))
    alpha <- rep(1,size(H)) # '1' because rdirichlet() takes alpha, not powers
    names(alpha) <- pnames(H)
    return(rdirichlet(n=n,H=alpha))
}
    
`discard_flawed2` <- function(x, unwanted,...){
    if(is.character(unwanted)){
        wanted <- !which(rownames(x)==unwanted)
    }
    o <- wikitable_to_ranktable(x,...) # was   o <- ordertable_to_ranktable(x)
    class(o) <- "matrix"
    o <- t(apply(o[,unwanted],1,rank))
    colnames(o) <- rownames(x)[unwanted]
    class(o) <- "ranktable"
    return(ranktable_to_ordertable(o))
}

`as.ordertable` <- function(w){
    out <-
        apply(w,2,    
              function(x){ # 'x' is a column of w
                  out <- suppressWarnings(as.numeric(x))  # 'ret' etc -> NA
                  no <- is.na(out)
                  o <- out[!no]
                  o[o>0] <- rank(o[o>0])
                  out[!no] <- o
                  out[no] <- 0
                  return(out)
              } )
    rownames(out) <- rownames(w)
    return(out)
}

`ordertable2points` <- function(o,points,totals=TRUE){
    points <- c(points,rep(0,1+max(o)-length(points)))
    o[o==0] <- length(points)
    o[] <- points[o]
    if(totals){
        return(rowSums(o))
    } else {
        return(o)
    }
}

`ordertrans` <- function(x,players){

    if(missing(players)){
      return(x[order(names(x))])
    } else {
      if(is.hyper2(players) | is.hyper3(players)){
        players <- pnames(players)
      }
    }
      
    stopifnot(length(x) == length(players))
    stopifnot(all(sort(names(x)) == sort(players)))
    stopifnot(all(table(names(x))==1))
    x[apply(outer(players,names(x),`==`),1,which)]
}

`ordertransplot` <- function(ox,oy, plotlims, ...){
  stopifnot(all(sort(names(ox))==sort(names(oy))))
  ox <- ordertrans(ox)
  oy <- ordertrans(oy)
  par(pty='s') # square plot
  if(missing(plotlims)){plotlims <- c(0,max(c(ox,oy)))}
  plot(ox,oy,asp=1,pty='s',xlim=plotlims,ylim=plotlims,pch=16,...)
  abline(0,1)
  for(i in seq_along(ox)){text(ox[i],oy[i],names(ox)[i],pos=4,col='gray',cex=0.7)}
}

`consistency` <- function(H,plot=TRUE,...){
  m1 <- maxp(H)
  pnames(H) <- rev(pnames(H))
  m2 <- rev(maxp(H))
  out <- rbind(orig=m1,rev=m2,diff=m1-m2)
  if(plot){
    par(pty="s")
    plot(m1,m2,xlab='',ylab='',log="xy",type="n")
    abline(0,1,col='gray')
    points(m1,m2,pch=16, ...)
    for (i in seq_along(m1)) {
      text(m1[i], m2[i], names(m1)[i], pos = 4, col = "gray", cex = 0.7)
    }
    return(invisible(out))
  } else {
    return(out)
  }
}

`zermelo` <- function(M,maxit=100,start,tol=1e-10,give=FALSE){
    diag(M) <- 0  # usual convention is NA on the diagonal
    rM <- rowSums(M)  # only need to calculate this once
    M <- M+t(M)
    if(missing(start)){
        p <- rep(1/nrow(M),nrow(M))  # starting point for iteration
    } else {
        p <- start
    }
    if(give){
        pout <- matrix(0,maxit+1,ncol(M))
        colnames(pout) <- colnames(M)
        pout[1,] <- p
    }
    for(i in seq_len(maxit)){
        pnew <- rM/colSums(M/outer(p,p,`+`))  # the meat
        pnew[is.nan(pnew)] <- 0
        pnew <- pnew/sum(pnew)  # normalize
        if(give){pout[i+1,] <- pnew}
        if(all(abs(p-pnew)<tol)){break}else{p <- pnew}  # maybe finish early
    }
    if(give){
        return(pout[seq_len(i+1),])
    } else {
        return(pnew)
    }
}

`pairwise` <- function(M){
    diag(M) <- 0
    if(is.null(rownames(M))){
        rownames(M) <- paste("p",seq_len(nrow(M)),sep="_")
        colnames(M) <- rownames(M)
    }
    jj <- M + t(M)
    
    index <- which(upper.tri(M),arr.ind=TRUE)
    dirichlet(rowSums(M)) - hyper2(apply(index,1,function(x){rownames(M)[x]},simplify=FALSE),jj[index])
}

`home_away` <- function (home_games_won, away_games_won){
    if (is.complex(home_games_won)) {
        away_games_won <- Im(home_games_won)
        home_games_won <- Re(home_games_won)
    }
    
    teams <- rownames(home_games_won)
    stopifnot(identical(teams, colnames(home_games_won)))
    stopifnot(identical(teams, rownames(away_games_won)))
    stopifnot(identical(teams, colnames(away_games_won)))
    teams <- c(teams, "home")
    H <- hyper2(pnames = teams)
    for (i in seq_len(nrow(home_games_won))) {
        for (j in seq_len(ncol(home_games_won))) {
            if (i != j) {
                home_team <- teams[i]
                away_team <- teams[j]

		home_wins <- home_games_won[i,j]
		away_wins <- away_games_won[i,j] 
                no_of_matches <- home_wins + away_wins 
		
                H[c(home_team,           "home")] %<>% inc(home_wins)
 		H[c(          away_team        )] %<>% inc(away_wins) 
                H[c(home_team,away_team, "home")] %<>% dec(no_of_matches)
            }
        }
    }
    return(H)
}

`balance` <- function(H){
    H[pnames(H)] <- 0
    H[pnames(H)] <- -sum(powers(H))
    return(H)
}

`rrace` <- function(strengths){
    out <- strengths
    for (i in seq_along(out)) {
        nextfinisher <- sample(names(strengths),size=1,prob=strengths)
        out[i] <- nextfinisher
        strengths <- strengths[names(strengths) != nextfinisher]
    }
    names(out) <- NULL
    return(out)
}
