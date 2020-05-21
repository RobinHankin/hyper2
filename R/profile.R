`profsupp` <- function(H, i, p, relative=TRUE, ...){
    out <- sapply(p,function(x){profile_support_single(H=H, i=i, p=x, evaluate=FALSE, ...)})
    if(relative){out <- out-max(out)}
    return(out)
}
    
`profile_support_single` <- function(H, i, p, evaluate=FALSE, ...){
    if(is.character(i)){
      i <- which(pnames(H)==i)
    }
    
    n <- size(H)
    delta <- 1e-4

    if(i<size(H)){  # regular, non-fillup value
        UI <- rep(0,n-1)
        UI[i] <- 1
        CI <- p
        start_min <- rep((1-p-delta)/(n-1),n-1)
        start_min[i] <- p+delta ## so p > p
        start_max <-  rep((1-p+delta)/(n-1),n-1)
        start_max[i] <- p-delta ## so p < p
    } else {   # fillup tested
        UI <- rep(-1,size(H)-1)
        CI <- p-1
        start_min <- rep(delta/n,n-1)         # all regular values small, fillup 1-delta
        start_max <- rep((1-delta)/(n-1),n-1) # all regular values big, fillup delta
    }
    
    m_min <- maxp(H,startp=start_min, fcm=+UI, fcv=+CI, ..., give=TRUE) # p_i >= p
    m_max <- maxp(H,startp=start_max, fcm=-UI, fcv=-CI, ..., give=TRUE) # p_i <= p
    ## in the above, 'm_min' interprets p as a minimum (that
    ## is, a lower bound) and 'm_max' interprets p as a
    ## maximum (that is, an upper bound).

    if(m_min$value < m_max$value){
        a <- m_min
    } else {
        a <- m_max
    }

    if(evaluate){
        jj <- fillup(a$par)
        names(jj) <- pnames(H)
        return(jj)
    } else {
        return(a$value)
    }
}

