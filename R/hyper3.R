`hyper3` <- function(B=list(), W=list(), powers=0,pnames){
    if(is.matrix(B)){
      return(hyper3_m(B,W))
    } else if(all(unlist(lapply(B,is_ok_weightedplayers)))){
      return(hyper3_nv(B,powers,pnames))
    } else {
      return(hyper3_bw(B,W,powers,pnames))
    }
}

`hyper3_bw` <- function(B=list(), W=list(), powers=0,pnames){
                                        # hyper3_bw(list(c("a","b"),c("b","c","e")),list(c(1,4),c(1,3,9)),1:2,letters[1:5]) 
  
    stopifnot(length(B) == length(W))
    stopifnot(all(unlist(lapply(B,length)) == unlist(lapply(W,length))))

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

`hyper3_m` <- function(M,p,stripzeros=TRUE){
  if(stripzeros){
    wanted <- apply(M,1,function(x){any(x!=0)})
    M <- M[wanted,]
    p <- p[wanted]
  }

  hyper3(
      B = rep(list(colnames(M)),nrow(M)),
      W = unclass(as.data.frame(t(M))),
      powers=p[wanted],
      pnames = colnames(M)
  )
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

setGeneric("weights",function(object, ...){standardGeneric("weights")})
`weights.hyper3` <- function(object, ...){object$weights}

`hyper3_to_hyper2` <- function(x){
    if(any(c(weights(x),recursive=TRUE) != 1)){
        warning("not all weights are equal to one")
    }
    return(hyper2(L=elements(brackets(x)),d=elements(powers(x)),pnames=pnames(x)))
}

`as.hyper3` <- function(x){
    if(is.hyper3(x)){return(x)}
    if(is.hyper2(x)){
        return(hyper3_bw(
            B=brackets(x),
            W=lapply(brackets(x),function(x){rep(1,length(x))}),
            powers=powers(x),
            pnames=pnames(x)
        ))
    } else {
        stop("only hyper2 objects can be coerced at this time")
    }
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
    return(disord(out,h=hashcal(H3)))
}

`print.hyper3` <- function(x,...){
    b <- elements(as.namedvectorlist(x))
    p <- elements(powers(x))
    if(!isFALSE(getOption("give_warning_on_nonzero_power_sum"))){
      if(sum(powers(x)) !=0){
          warning("powers have nonzero sum")
      }
    }
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

`hyper3_prod` <- function(e1,n){
    powers(e1) <- powers(e1)*n
    return(e1)
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
          return(hyper3_prod(e1,-1))
      } else {
          stop(gettextf("Unary operator %s not defined for hyper3 objects", dQuote(.Generic)))
      }
  }  # if(unary) closes

  if (!is.element(.Generic, c("+", "-", "==", "!=", "*"))){
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
        return(hyper3_prod(e1,e2))
      } else if (!lclass && rclass){
        return(hyper3_prod(e2,e1))
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
        return(hyper2_sum_numeric(e1,-e2))
      } else if (!lclass && rclass){
        return(hyper2_sum_numeric(e2,-e1))
      } else {
        stop("this cannot happen")
      }
    }
  }
}

# accessor3 <- function(L, W, powers, Lwanted, Wwanted) {}

char2nv <- function(x){
    jj <- table(x)
    out <- as.numeric(jj)
    names(out) <- names(jj)
    return(out)
}
    


`[.hyper3` <- function(x, ...){ # H3[list(c(a=1),c(a=1,b=2))]
    stopifnot(nargs() == 2)
    dots <- list(...)[[1]]
    if(!is.list(dots)){
        if(is.character(dots)){dots <- char2nv(dots)}
        dots <- list(dots)
    }

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
    stopifnot('hyper3' %in% class(x))
    
    if(is_ok_weightedplayers(index)){
        index <- list(index)
    }
    value <- elements(value)
    stopifnot(is.numeric(value)) # coercion to integer is done in C
    stopifnot(is.vector(value))
    if(length(value)==1){
        value <- rep(value, length(index))
    }
    ## assigner3 <- function(L, W, p, L2, W2, value)
    return(assigner3(elements(brackets(x)),elements(weights(x)),elements(powers(x)),
                     lapply(index,names),lapply(index,as.vector),value))
}

`overwrite_lowlevel3` <- function(x,value){
  stopifnot(class(x    ) == 'hyper3')
  stopifnot(class(value) == 'hyper3')

  overwrite3(
      elements(brackets(x    )), elements(weights(x    )), elements(powers(x    )), 
      elements(brackets(value)), elements(weights(value)), elements(powers(value))
  )
}

`[<-.hyper3` <- function(x, index, ..., value){  # index must be a named vector or a list of named vectors
    if(inherits(value,"weight")){
        out <- setweight(x,index,value)
    } else if(missing(index)){  # A[] <- B
        jj <- overwrite_lowlevel3(x,value)
        if(all(pnames(value) %in% pnames(x))){
            out <- hyper3_bw(jj[[1]],jj[[2]],jj[[3]],pnames=pnames(x))
        } else {
            out <- hyper3(jj[[1]],jj[[2]],jj[[3]])
        }
    } else { # index supplied
        if(is.character(index)){index <- char2nv(index)}
        jj <- assign_lowlevel3(x,index,value)
        if(all(c(index,recursive=TRUE) %in% pnames(x))){
            out <- hyper3_bw(jj[[1]],jj[[2]],jj[[3]],pnames=pnames(x)) # do not change pnames
        } else { # index introduces a new pname
            out <- hyper3_bw(jj[[1]],jj[[2]],jj[[3]])
            pnames(out) <- sort(unique(c(pnames(x),names(index))))
        }
    }
    return(out)
}


`loglik_single_redundant` <- function(p,H,log=TRUE){
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
  out <- evaluate3(brackets(H), weights(H),powers(H), probs=probs, pnames=pnames(H))
  if(log){
    return(out)
  } else {
    return(exp(out))
  }
}

`rpair3` <- function(n=5,s=3,lambda=1.3){ # s = number of matches
    players <- letters[seq_len(n)]
    strengths <- zipf(n)
    names(strengths) <- players
    out <- hyper3()
    for(i in seq_len(s)){  # iterate through matches
        jj <- sample(players,2,replace=FALSE)
        white <- jj[1]
        black <- jj[2]
       
        prob_white_win <- strengths[white]*lambda / (strengths[white]*lambda + strengths[black])
        if(runif(1) < prob_white_win){ # white wins
            jj <- lambda
            names(jj) <- white
            out[jj] %<>% inc  # e.g. jj: c(a=1.1)
            jj <- c(jj,1)
            names(jj)[2] <- black
            out[jj] %<>% dec # e.g. jj: c(a=1.1,b=1)
        } else { # black wins
            jj <- lambda
            names(jj) <- black
            out[jj] %<>% inc
            jj <- c(jj,1)
            names(jj)[2] <- white
            out[jj] %<>% dec # e.g. jj: c(a=1.1,b=1)
        }
    }
    return(out)
}

       
`rhyper3` <- function(n=5,s=4,type='race',...){
    switch(type,
           race = rracehyper3(n=n,size=s,...),
           pair = rpair3(n=n,s=s,...)
           )
    }

`maxp3` <- function(H3,startp,give=FALSE,fcm = NULL, fcv = NULL, 
                  SMALL = 1e-06, maxtry = 100, ...){

stop("not yet written")


}


`num3` <- function(v,helped=NULL,lambda=1){  # numerators
    out <- hyper3()
    tv <- table(v)
    for(i in seq_along(tv)){
        ntv <- names(tv)[i]
        if(ntv %in% helped){
          jj <- lambda
        } else {
          jj <- 1
        }
        names(jj) <- ntv
        out[jj] <- tv[i]
    }
    return(out)
}
    
`den3` <- function(v,helped=NULL,lambda=1){  # denominators
    tv <- table(v)
    out <- as.vector(tv)
    names(out) <- names(tv)
    out[names(tv) %in% helped] %<>% prod(lambda)
    return(out)
}
       
`ordervec2supp3` <- function(v,nonfinishers=NULL){ # v = c("a","b","a","a","c","a")
    out <- num3(v)
    for(i in seq_along(v)){
        out[den3(c(v[i:length(v)],nonfinishers))] %<>% dec
    }
    return(out)
}

`rankvec_likelihood3` <- `ordervec2supp3`

`ordervec2supp3a` <- function(v,nonfinishers=NULL,helped=NULL,lambda=1){
  out <- num3(v,helped=helped,lambda=lambda)  # numerator
    for(i in seq_along(v)){
      out[den3(c(v[i:length(v)],nonfinishers))] %<>% dec  # denominator
    }
  return(out)
}

`rwinner3` <- function(pn=c(a=2,b=4,c=2,d=1),ps=c(a=0.3,b=0.1,c=0.2,d=0.4)){
    stopifnot(all(table(names(pn))<=1))
    stopifnot(all(table(names(ps))<=1))
    stopifnot(identical(names(pn),names(ps)))

    sample(names(ps),1,prob=(pn*ps)/sum(pn*ps))
}

`rrace3` <- function(pn=c(a=2,b=4,c=2,d=1),ps=c(a=0.3, b=0.1,c=0.2,d=0.4)){
    ## pn: c(a=2,b=4,c=2,d=1) # player numbers (two "a"s, four "b"s etc)
    ## ps: c(a=0.3, b=0.1,c=0.2,d=0.4)  # player strengths
    
    out <- NULL
    for(i in seq_len(sum(pn))){
        nextfinisher <- rwinner3(pn,ps)
        out <- c(out,nextfinisher)
        pn[nextfinisher] <- pn[nextfinisher] - 1
    }
    return(out)
}

`rracehyper3` <- function(n=4,size=9,ps=NULL,races=3){
    players <- letters[seq_len(n)]
    if(is.null(ps)){
        ps <- zipf(n)
        names(ps) <- players
    }
    pn <- as.vector(table(factor(sample(players,size,replace=TRUE),levels=players)))  
    names(pn) <- players

    out <- hyper3()
    for(i in seq_len(races)){
        out <- out + ordervec2supp3(rrace3(pn,ps))
    }
    return(out)
}

`pnames<-.hyper3` <- function(x,value){
    hyper3(
        elements(brackets(x)),
        elements(weights(x)),
        elements(powers(x)),
        pnames=value)
}

## a <- read.table("constructor_2021.txt",header=TRUE)
## constructor(a[,-ncol(a)])  # final column is points

`ordertable2supp3` <- function(a){  
    out <- hyper3()
    n <- a[,1]  # names of constructors
    for(i in seq(from=2,to=ncol(a)-1)){
        jj <- n[order(suppressWarnings(as.numeric(a[,i])),na.last=TRUE)]
        n_finishers    <- sum(!is.na(jj))
        n_nonfinishers <- sum( is.na(jj))
        if(n_nonfinishers==0){ # technically not necessary:  "else" clause works for ==0
            out <- out + ordervec2supp3(jj)
        } else { 
            out <- out + ordervec2supp3(jj[seq_len(n_finishers)],jj[n_finishers + seq_len(n_finishers)])
        }
    }
    return(out)
}

`attemptstable2supp3` <- function(a, decreasing, give.supp=TRUE, dnf.last=TRUE){
    o <- c(matrix(suppressWarnings(as.numeric(c(a,recursive=TRUE))),nrow(a),ncol(a)))
    names(o) <- c(matrix(rownames(a),nrow(a),ncol(a)))
    o <- sort(o,na.last=TRUE,decreasing=TRUE)
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

`args2ordervec` <- function(...){
    l <- list(...)
    if(any(names(l) == "") & !all(names(l)=="")){
        stop("either name all of the arguments, or none of them")
    }
    if(all(names(l)=="")){
        names(l) <- paste("X",seq_len(nargs()),sep="")
    }
    x <- unlist(l,use.names=FALSE)
    if(any(table(x)>1)){stop("ties not implemented")}
    names(x) <- rep(names(l),lapply(l,length))
    return(names(sort(x)))
}
   

`home_away3` <- function(home_games_won,away_games_won,lambda){

    if(is.complex(home_games_won)){
        if(missing(lambda)){lambda <- away_games_won}
        away_games_won <- Im(home_games_won)
        home_games_won <- Re(home_games_won) 
    }

    teams <- rownames(home_games_won)
    stopifnot(identical(teams,colnames(home_games_won)))
    stopifnot(identical(teams,rownames(away_games_won)))
    stopifnot(identical(teams,colnames(away_games_won)))

    H <- hyper3(pnames=c(teams,"lambda"))

    for(i in seq_len(nrow(home_games_won))){
        for(j in seq_len(ncol(home_games_won))){
            if(i != j){  
                home_team <- teams[i]
                away_team <- teams[j]

		home_wins <- home_games_won[i,j]
		away_wins <- away_games_won[i,j] 
                no_of_matches <- home_wins + away_wins 

                ## home wins:
                jj <- lambda
                names(jj) <- home_team
                H[jj] %<>% inc(home_wins)
                
                ## away wins:
                jj <- 1
                names(jj) <- away_team
                H[jj] %<>% inc(away_wins)

                ## denominator
                jj <- c(lambda,1)
                names(jj) <- c(home_team,away_team)
                H[jj] %<>% dec(no_of_matches)
            } # if(i != j) closes
        } # j loop closes
    } # i loop closes
    return(H)
}

## kka_array is arranged differently from the previous; it uses
## white_wins and white_losses, not white wins and white losses

`white_draw3` <- function(A,lambda,D){ # white_draw3(kka_array,1.1,0.2)
    stopifnot(identical(dimnames(A)[[1]],dimnames(A)[[2]]))
    players <- dimnames(A)[[1]] 
    H <- hyper3(pnames=c(players,"lambda","D"))
    d <- dim(A)
    
    for(i in seq_len(d[1])){
        for(j in seq_len(d[2])[-i]){
            white_player <- players[i]
            black_player <- players[j]
            
            white_player_wins  <- A[i,j,1]
            white_player_draws <- A[i,j,2]
            white_player_loses <- A[i,j,3]
            
            total_matches <- white_player_wins + white_player_draws + white_player_loses 
            
            ## white player wins:
            jj <- lambda
            names(jj) <- white_player
            H[jj] %<>% inc(white_player_wins)
            
            ## white player loses (so the black player wins):
            jj <- 1
            names(jj) <- black_player
            H[jj] %<>% inc(white_player_loses)
            
            ## draw:
            jj <- c(D,D)
            names(jj) <- c(white_player,black_player)
            H[jj] %<>% inc(white_player_draws)
            
            ## lastly, change the denominator so the powers sum to zero:
            jj <- c(lambda+D , 1+D)
            names(jj) <- c(white_player,black_player)
            H[jj] %<>% dec(total_matches)
        } # j loop closes
    } # i loop closes
    return(H)
}


`list2nv` <- function(L,integers=TRUE){
  if(integers){
      jj <- seq_along(L)
  } else {
    jj <- names(L)
  }
  out <- rep(jj,times=unlist(lapply(L,length)))
  names(out) <- c(L,recursive=TRUE)
  return(out)
}

`sum.hyper3` <- function(x, ..., na.rm=FALSE){
  if(nargs()==1){
    return(x)
  } else if (nargs()==2){
    return(hyper3_add(x, ...))
  } else {
    return(hyper3_add(x, Recall(...)))
  }
}

`as.weight` <- function(x){
    stopifnot(is.numeric(x))
    stopifnot(x >= 0)
    class(x) <- "weight" # this is the only place that class is set to weight
    return(x)
}

`setweight` <- function(x,index,value){
    x <- as.hyper3(x)
    if(length(value)==1){value <- rep(value,length(index))}
    stopifnot(length(index) == length(value))
    for(i in seq_along(index)){
        x <- hyper3_nv(
            lapply(as.namedvectorlist(x),
                   function(p){
                       p[names(p)==index[i]] <- value[i] # The meat
                       return(p)
                   }),
            powers=powers(x),
            pnames=pnames(x))
    }
    return(x)
}
