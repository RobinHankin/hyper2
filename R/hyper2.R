`hyper2` <-  function(L=list(), d=0, pnames=NA){
  if(length(d)==1){d <- rep(d,length(L))}  
  stopifnot(is_valid_hyper2(L,d,pnames))
  L <- lapply(L,as.integer)
  out <- identityL(L,d)
  out$pnames <- pnames
  class(out) <- 'hyper2'  # This is the only class assignment in the package
  return(out)
}

## Following three functions are the only accessor methods in the package
setGeneric("brackets",function(x){standardGeneric("brackets")})
setGeneric("powers"  ,function(x){standardGeneric("powers"  )})
setGeneric("pnames"  ,function(x){standardGeneric("pnames"  )})
`brackets` <- function(H){UseMethod("brackets")}
`powers`   <- function(H){UseMethod("powers"  )}
`pnames`   <- function(H){UseMethod("pnames"  )}
`brackets.hyper2` <- function(H){ H$brackets }

`powers.hyper2` <- function(H){
  if(is_constant(H)){
    return(0)
  } else {
    return(H$powers)
  }
}

`pnames.hyper2` <- function(H){ H$pnames }
`pnames.suplist` <- function(H){pnames(H[[1]])}
## accessor methods end

## Following function is the only setter method in the package
setGeneric("pnames<-",function(x,value){standardGeneric("pnames<-")})
`pnames<-` <- function(x,value){UseMethod("pnames<-")}

`pnames<-.hyper2` <- function(x,value){
  if(identical(pnames(x),NA)){
    return(hyper2(brackets(x),powers(x),pnames=value))
  } else {
    return(change_pnames(x,value))
  }
}
## setter methods end

`is.hyper2` <- function(H){inherits(H,"hyper2")}
`length.hyper2` <- function(x){length(x$brackets)}
`change_pnames` <- function(H,new_pnames){  # new_pnames is a character vector, eg c('a', 'b')
  if(identical(pnames(H),NA)){return(hyper2(brackets(H),powers(H),pnames=new_pnames))}
  if(identical(new_pnames,NA)){return(hyper2(brackets(H),powers(H)))}

  stopifnot(all(pnames(H) %in% new_pnames))
  b <- brackets(H)
  pn <- pnames(H)
  pow <- powers(H)
  out <- hyper2(pnames=new_pnames)
  for(i in seq_along(b)){
#    out[new_pnames[new_pnames %in% pn[b[[i]]]]]  <- pow[i]
    out[which(new_pnames %in% pn[b[[i]]])]  <- pow[i]
  }
  return(out)
}

`is_constant` <- function(H){ length(H)==0 }

`is_valid_hyper2` <- function(L,d,pnames){
  stopifnot(is.list(L))
  stopifnot(is.vector(d))
  stopifnot(is.numeric(d))
  stopifnot(length(L) == length(d))
  stopifnot(all(unlist(lapply(L,function(x){all(x==round(x))}))))
  if(!identical(pnames,NA)){
    if(length(L)==0){
      hsize <- 0
    } else {
      hsize <- max(c(L,recursive=TRUE))
    }
    stopifnot(length(pnames) >= hsize)
  }
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
  } else if(is.list(L)){
    ignore <- 0
  } else {
    stop("first argument must be a list or a matrix")
  }
  return(hyper2(L,d,pnames))
}

`.print.helper` <- function(pnames,vec){
  if(length(vec)==1){
    out <- pnames[vec]
  } else {
    out <- paste("(",pnames[vec[1]],sep="")
    
    for(i in vec[-1]){
      out <- paste(out," + ",pnames[i],sep="")
    }
    out <- paste(out,")",sep="")
  }
  return(out)
}
  
`print.hyper2` <- function(x,...){
  b <- brackets(x)
  powers <- powers(x)
  if(length(b)==0){  # not is.null(b)
    n <- 1
    if(identical(pnames(x),NA)){
      pn <- "1"
    } else {  # empty but with specified pnames
      pn <- pnames(x)
    }
    n <- length(pn)
    b <- list(seq_len(n))
  } else {  # non-empty
    pn <- pnames(x)
    n <- max(c(b,recursive=TRUE))   # b non-empty; n>0
    if(identical(pnames(x),NA)){
      pn <- paste("p",seq_len(n),sep="")
    } else {
      pn <- pnames(x)
    }
  }

  out <- "log("
  for(i in seq_along(b)){
    jj <- unlist(b[i])
    pp <- powers[i]
    if(pp==1){
      out <- paste(out, .print.helper(pn, jj))
    } else {
      out <- paste(out, .print.helper(pn, jj), "^",pp,sep="")
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
  out <- addL(brackets(e1),powers(e1),brackets(e2),powers(e2))

  if(identical(pnames(e1),NA) & identical(pnames(e2),NA)){
    jj <- NA
  } else if(identical(pnames(e1),NA) & !identical(pnames(e2),NA)){
    jj <- pnames(e2)
  } else if(identical(pnames(e2),NA) & !identical(pnames(e1),NA)){
    jj <- pnames(e1)
  } else if(!identical(pnames(e2),NA) & !identical(pnames(e1),NA)){
    stopifnot(identical(pnames(e1),pnames(e2)))
    jj <- pnames(e1)
  } else {
    stop("this cannot happen")
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
  stopifnot(length(p) == size(H)-1)
  stopifnot(all(p>=0))
  stopifnot(sum(p)<=1)

  out <- evaluate(brackets(H), powers(H), probs=fillup(p))
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
  stopifnot(is.numeric(n))
  stopifnot(length(n)==1)
  return(hyper2(brackets(H),powers(H)*n,pnames=pnames(H)))
}

`Ops.hyper2` <-
  function (e1, e2 = NULL) 
{
  f <- function(...){stop("odd---neither argument has class hyper2?")}
  unary <- nargs() == 1
  lclass <- inherits(e1,"hyper2")
  rclass <- !unary && inherits(e2,"hyper2")
  
  if(unary){
    stop("Unary operator '", .Generic, "' is not implemented for hyper2 objects")
    }

  if (!is.element(.Generic, c("+", "-", "==", "!=", "*", "^" ))){
    stop("Binary operator '", .Generic, "' is not implemented for hyper2 objects")
  }
  
  if(lclass && rclass){  
    if (.Generic == "+"){
      return(hyper2_add(e1,e2))
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
    } else if(is.vector(first)){
      wanted <- list(first)
    } else {
      wanted <- dots
    }

    if(any(unlist(lapply(wanted, is.character)))){
        wanted <- lapply(wanted, character_to_number, pnames=pnames(x))
    }
    
    out <- accessor(x[[1]],x[[2]],wanted)
    return(hyper2(out[[1]],out[[2]],pnames=pnames(x)))
}

`.assign_lowlevel`<- function(x,index,value){ #H[index] <- value

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

    if(any(unlist(lapply(index, is.character)))){
        index <- lapply(index, character_to_number, pnames=pnames(x))
    }

    stopifnot(is.numeric(value)) # coercion to integer is done in C
    stopifnot(is.vector(value))
    if(length(value)==1){
        value <- rep(value, length(index))
    }
    return(assigner(brackets(x),powers(x),index,value))
}

`.overwrite_lowlevel` <- function(x,value){
  stopifnot(class(x)     == 'hyper2')
  stopifnot(class(value) == 'hyper2')

  overwrite(brackets(x), powers(x), brackets(value), powers(value))
}

`[<-.hyper2` <- function(x, index, ..., value){
  if(missing(index)){  # A[] <- B
    out <- .overwrite_lowlevel(x,value)
  } else {
    out <- .assign_lowlevel(x,index,value)
  }
    return(hyper2(out[[1]],out[[2]],pnames=pnames(x)))
}

`gradient` <- function(H,probs=indep(maxp(H))){
  stopifnot(length(probs) == size(H)-1)
  differentiate(brackets(H), powers(H), fillup(probs), size(H))$grad_comp
}

`gradientn` <- function(H,probs=maxp(H)){
  stopifnot(length(probs) == size(H))
  out <- differentiate_n(brackets(H), powers(H), probs, size(H))$grad_comp
  if(!identical(pnames(H),NA)){names(out) <- pnames(H)}
  return(out)
}

`hessian` <- function(H, probs=indep(maxp(H)),border=TRUE){
    n <- size(H)
    stopifnot(length(probs) == n-1)
    out <- hessian_lowlevel(brackets(H),powers(H),fillup(probs),n)$block_hessian_components
    out <- matrix(out,n,n)
    if(isFALSE(border)){return(out)}
    out <- rbind(c(0,rep(1,n)),cbind(1,out))
    if(!identical(pnames(H),NA)){
        jj <- c("usc",as.character(pnames(H))) # "usc" = unit sum constraint
        rownames(out) <- jj
        colnames(out) <- jj
    }
    return(out)
}

`is_ok_hessian` <- function(H){
    hess <- hessian(H)
    s <- seq(from=3,to=nrow(hess))
    alt <- (s%%2) == 1  # T,F,T,F,T...
    jj <- sapply(s,function(n){det(hess[seq_len(n),seq_len(n)])})
    all((jj>0) == alt)
}

`fillup` <- function(x,total=1){
  if(is.matrix(x)){
    return(cbind(x,total-rowSums(x)))
  } else {  # assumed to be a vector
    return(c(x,total-sum(x)))
  }
}

`indep` <- function(x){
   if(is.matrix(x)){
     return(x[,-ncol(x)])
  } else {  # assumed to be a vector
    return(x[-length(x)])
  }
}

`maxp` <- function(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, n=10, show=FALSE, justlikes=FALSE, ...){

    best_so_far <- -Inf # best (i.e. highest) likelihood found to date
    likes <- rep(NA,n)
    if(is.null(startp)){ startp <- indep(equalp(H)) }

    for(i in seq_len(n)){
        if(i>1){startp <- startp+runif(size(H)-1,max=SMALL/size(H))}
        jj <- maxp_single(H, startp=startp, give=TRUE, fcm=fcm, fcv=fcv, SMALL=SMALL, ...)
        likes[i] <- jj$value
        if(show){cat(paste(i,"; ", best_so_far, "  " , jj$value,"\n", sep=""))}
        if(jj$value > best_so_far){ # that is, if we have found something better
            out <- jj
            best_so_far <- jj$value
        }
    }  # i loop closes
    if(justlikes){ return(likes) }
    if(give){
        return(c(out,likes=list(likes)))
    } else {
        out <- fillup(out$par)
        if(!identical(pnames(H),NA)){names(out) <- pnames(H)}
        return(out)
    }
}  # maxp() closes

`maxp_single` <- function(H, startp=NULL, give=FALSE, fcm=NULL, fcv=NULL, SMALL=1e-6, maxtry=100, ...){
    if(inherits(H,"suplist")){return(maxplist(Hlist=H,startp=startp,give=give,fcm=fcm,fcv=fcv,...))}
    
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
    out <- constrOptim(theta = startp, f = objective, grad = NULL, 
        ui = UI, ci = CI, ...)
    out$value <- -out$value
    if (give) {
        return(out)
    }
    else {
        jj <- fillup(out$par)
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

`head.hyper2` <- function(x,...){ x[head(brackets(x),...)] }

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
    v <- rev(M[i,,drop=TRUE])
    jj1 <- hyper2(as.list(v),times[i])   # numerators
    jj2 <- hyper2(sapply(seq_along(v),function(i){v[seq_len(i)]}),-times[i]) # denominators
    out <- out + jj1 + jj2
  }  # i loop closes
  return(out)
}

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

`.rrank_single` <- function(p){
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

`rrank` <- function(n=1, p, pnames=NULL, fill=FALSE, rnames=NULL){
    if(fill){ p <- fillup(p) }
    if(is.null(pnames)){pnames <- names(p)}
    out <- t(replicate(n, .rrank_single(p)))
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
    out <- apply(out,2,function(rank){H+rank_likelihood(rank)})
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

`elimination` <- function(all_players){
    if(is.numeric(all_players) & (length(all_players)==1)){
      all_players <- letters[seq_len(all_players)]
    }
    all_players <- rev(all_players)
    H <- choose_losers(hyper2(pnames=sort(all_players)),all_players,all_players[length(all_players)])
    players <- all_players[-length(all_players)]
    while(length(players)>1){
        for(i in seq_along(H)){
            H[[i]] <- choose_losers(H[[i]],players,players[length(players)])
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

`dirichlet` <-  function(powers, alpha, pnames=NA){
    if(!xor(missing(powers),missing(alpha))){
        stop("supply exactly one of powers, alpha")
    }

    if(missing(powers)){
        powers <- alpha-1
    }
    
    if(isTRUE(is.na(pnames)) & !is.null(names(powers))){
        pnames <- names(powers)
    }
    hyper2(as.list(seq_along(powers)),d=powers,pnames=pnames)
}

`GD` <- function(alpha, beta, beta0=0, pnames=NA){
  k <- length(alpha)
  stopifnot(length(beta) == k-1)
  
  H <- dirichlet(powers=alpha-1,pnames=pnames)
  for(i in 2:(k-1)){
    H[(i:k)] <- beta[i-1] -(alpha[i]+beta[i])
  }

  H[k] <- beta[k-1]-1
  H[seq_along(k)] <- beta0-(alpha[1]+beta[1])
  return(H)   
}

`GD_wong` <- function(alpha, beta, pnames=NA){
  k <- length(alpha)
  stopifnot(length(beta) == k)

  gamma <- beta[-k]-(alpha[-1]+beta[-1])
  gamma <- c(gamma, beta[k]-1)
  H <- dirichlet(powers=alpha-1,pnames=pnames)
  for(i in 1:k){
    H[(i+1):(k+1)] <- gamma[i]
  }
  return(H)   
}

`equalp` <- function(H){
    n <- size(H)
    rep(1/n,n)
}

`all_pnames` <- function(L){  # needs a list
  L %>% lapply(function(x){x %>% pnames %>% as.character}) %>% c(recursive=TRUE) %>% unique %>% sort
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
    if(!is.null(colnames(M))){pnames(out) <- colnames(M)}
    for(i in seq_len(nrow(M))){
        onerow <- M[i,]
        winning_side <- onerow==1
        losing_side  <- onerow==0
        playing <- !is.na(onerow)
        if(any(playing) & any(winning_side) & any(losing_side)){
          out[which(winning_side)] %<>% `+`(1)
          out[which(playing)] %<>% `-`(1)
        }
    }
    return(out)
}

`inc` <- function(H,val=1){ H %<>% `+`(val)}   # increment
`dec` <- function(H,val=1){ H %<>% `-`(val)}   # decrement

`trial` <- function(H,winners,players,val=1){
  H[winners] %<>% `+`(val)
  H[players] %<>% `-`(val)
  return(H)
}   

`rhyper2` <- function(n=8, s=5,  pairs=TRUE, teams=TRUE, race=TRUE, pnames){
  n <- n - n%%2  # Force 'n' to be even
  H <- hyper2()
  if(pairs){
    M <- replicate(s,sample(n,2,replace=FALSE),simplify=TRUE)
    H <- hyper2(split(M,rep(seq_len(ncol(M)),each=nrow(M))),-1)
    H <- H + hyper2(as.list(M[1,]),1)
  }

  if(teams){
    M <- replicate(s,sample(n))
    H <- H+hyper2(split(M,rep(seq_len(ncol(M)),each=nrow(M)/2)),1)  # winners
  }

  if(race){
    for(i in seq_len(s)){
      H <- H + rank_likelihood(sample(n))
    }
  }

  if(missing(pnames)){pnames(H) <- letters[seq_len(size(H))]}
  return(H)
}

`ordervec2supp` <- function(d,pnames){
    wanted <- d!=0
    if(any(sort(d[wanted]) != seq_len(sum(wanted)))){
        stop("nonzero elements of d should be 1,2,3,4...,n")
    }
    nd <- names(d)
    out <- hyper2(d=length(d))
    while(any(d>0)){
        eligible <- which(d>=0)  #NB inclusive inequality; zero is DNC etc who

        ## Increment numerator power of the first choice among eligible players:
        out[which(d==1)] %<>% inc
        
        ## Power of set of all eligible players decrements:
        out[eligible] %<>% dec
        
        ## once you've come first in the field, you are ineligible to be first again:
        d[d==1] <- -1  # NB strictly <0
        
        ## Now, everyone moves down the list, so who *was* in
        ## second place becomes first place, who *was* third place
        ## becomes second, and so on:
        
        d[d>0] %<>% dec
        
    } # while() loop closes
    if(!is.null(nd)){ pnames(out) <- nd}
    if(!missing(pnames)){pnames(out) <- pnames}
    return(out)
}

`order_obs` <- function(H,d){
    ## 'd' is a named integer vector with c(a=1,b=0,c=2) meaning 'a'
    ## came first, 'b' did not finish, and 'c' came second.  'H' is a
    ## hyper2 object that must include all names of 'd'.  It uses the
    ## same algorithm as ordervec2supp() but is based on names.
    
    stopifnot(all(names(d) %in% pnames(H)))

    wanted <- d!=0
    if(any(sort(d[wanted]) != seq_len(sum(wanted)))){
        stop("nonzero elements of d should be 1,2,3,4...,n")
    }

    while(any(d>0)){
        eligible <- which(d>=0)
        H[names(d[d==1])] %<>% inc
        H[names(d)[eligible]] %<>% dec
        d[d==1] <- -1  # NB strictly <0
        d[d>0] %<>% dec
    } # while() loop closes
    return(H)
}

`ordertable2supp` <- function(x, noscore, misslast=TRUE){
    if(missing(noscore)){
        noscore <- c("Ret", "WD", "DNS", "DSQ", "DNP", "NC")
    }

    if(misslast){
        venues <- colnames(x)[-ncol(x)]
    } else {
        venues <- colnames(x)
    }

    ## Now create a numeric matrix, fmat.  Two steps: first, count
    ## any no-score as zero:
  
    jj <- apply(x,2,function(y){
        if(any(y %in% noscore)){y[y%in%noscore] <- 0}
        return(y)
    })
    
    ## Second, convert to numeric and strip out names; transpose of
    ## x (because we want each row to be a venue):
    if(misslast){jj <- jj[,-ncol(x)]}
    fmat <- matrix(as.numeric(jj),byrow=TRUE,ncol=nrow(x)) 
    colnames(fmat) <- rownames(x)
    rownames(fmat) <- venues
    
    out <- hyper2(d=ncol(fmat))
    
    ## Now cycle through the rows; each row is a venue [voter]
    for(i in seq_len(nrow(fmat))){
        out %<>% `+`(ordervec2supp(fmat[i,,drop=TRUE]))
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
      H %<>% discard(names(m[which.min(m)]))
    } else {
     break
    }
  }
  return(H)
}

`pwa` <- function(H,pwa,chameleon='S'){  # idea is pwa(ordervec2supp(a,...),c("a","b"))

  if(is.character(pwa)){
    stopifnot(pwa %in% pnames(H))
    pwa <- which(pnames(H) %in% pwa)
  }  # now pwa is numeric

  if(!isTRUE(is.na(pnames(H)))){            # that is, if H has pnames() then ...
    stopifnot(!(chameleon %in% pnames(H)))  # ... check that the chameleon isn't already a competitor, and
    pnames(H) <- c(pnames(H),chameleon)     # add the chameleon's name to pnames
  }
  
  B <- brackets(H)
  ## overwrite B in place:
  for(i in seq_along(B)){
    if(any(pwa %in% B[[i]])){  # person with advantage 
      B[[i]] <- c(B[[i]],size(H))  # NB off-by-one error
    } 
  }
  return(hyper2(B,powers(H),pnames=pnames(H)))
}

`wikitable_to_ranktable`  <- function(wikitable, points=TRUE, strict=FALSE){
  if(points){wikitable <- wikitable[,-ncol(wikitable)]}

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

`is.dirichlet` <- function(H){all(lapply(brackets(H),length)==1)}

`rdirichlet` <- function(n, H){
    if(is.hyper2(H)){
        if(is.dirichlet(H)){
            pn <- pnames(H)
            s <- size(H)
            alpha <- (
                sapply(seq_len(s),function(i){powers(H[i])}) # NB not powers(H); zero powers are discarded!
                +1  # sic: power_i = alpha_i+1
                )
        } else { # hyper2, but not dirichlet
            warning("hyper2 object supplied but is not a Dirichlet distribution: sample from uniform distribution returned")
            pn <- pnames(H)
            s <- size(H)
            alpha <- rep(1,s)
        }
    } else {   # H is vector of alpha
        pn <- NA
        s <- length(H)
        alpha <- H
    }
    out <- t(apply(matrix(rgamma(n*s,shape=alpha),s,n),2,function(x){x/sum(x)}))
    if(!identical(pn,NA)){colnames(out) <- pn}
    return(out)
}

