## Function losers() gives a likelihood function for situations where
## the _weakest_ player is eliminated at each stage; it returns a
## list.  It is intended for situations like the Great British
## Bake-off and Masterchef.  Function maxp2(), which is very similar
## to maxp(), calculates the maximum likelihood estimate for such an
## object.


`losers` <- function(players){
    H <- choose_losers(hyper2(pnames=players),players,players[length(players)])
    players <- players[-length(players)]
    while(length(players)>1){
        for(i in seq_along(H)){
            H[[i]] <- choose_losers(H[[i]],players,players[length(players)])
        }
        H <- unlist(H,recursive=FALSE)
        players <- players[-length(players)]
    }
    return(H)
}


maxp2 <- function (H, startp = NULL, give = FALSE, fcm = NULL, fcv = NULL, ...){
    SMALL <- 1e-06
    n <- size(H[[1]])
    if (is.null(startp)) {
        startp <- rep(1/n, n - 1)
    }
    objective <- function(p) {
        -like_single_list(p, H)
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
