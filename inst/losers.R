## Function elimination() gives a likelihood function for situations where
## the _weakest_ player is eliminated at each stage; it returns a
## list.  It is intended for situations like the Great British
## Bake-off and Masterchef.  Function maxp2(), which is very similar
## to maxp(), calculates the maximum likelihood estimate for such an
## object.


`elimination` <- function(players){
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




o <- maxplist(elimination(players=1:4))  # 12 terms
dotchart(o,pch=16)


