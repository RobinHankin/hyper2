elastic <- function(v,l){  # v is a named vector; l = lambda
    players <- names(v) # p = players
    stopifnot(all(table(players)==1))
    n <- length(v)  # number of runners
    out <- hyper3(pnames=players)
    out[players[1]] %<>% inc
    jj <- v
    jj[] <- 1
    out[jj] %<>% dec

    for(i in seq(from=2,to=length(v)-1)){
        still_running <- players[i:n]
        if(v[i] == v[i-1]){ # that is, runner placed i is in the same group as i-1
            jj <- c(1,1)
            names(jj) <- c(players[i],"M")
            out[jj] %<>% inc
            print(jj)
        } else {  # no elastic, no monster
            print(v[i])
            out[v[i]] %<>% inc
        }
        jj <- rep(1,length(still_running))
        names(jj) <- names(still_running)
        jj <- c(jj,M=sum(names(still_running) == players[i]))
        out[jj] %<>% dec
    } # i loop closes
    return(out)
}
            

    
