`race_cheering3` <- function(v,e,help){
  ## competitors abcdef, with (abc) 1.1 mutual friends with helpfulness 1.1 and (de) helpfulness 1.3:
  ## race_cheering3(c("a","b","c","d","e","f"),c(a=1,b=1,c=1,d=2,e=2,f=3), help=c(1.1,1.3))

  stopifnot(all(table(v)==1))
  stopifnot(all(table(names(e))==1))
  stopifnot(all(sort(names(e)) == sort(v)))
    
  out <- hyper3()

  finished <- NULL # these are competitors who have finished and are cheering their friends
  still_running <- v
  while(length(still_running) > 0){
    winner <- still_running[1]  # winner is the first element of the field
    winners_friends <- names(e[e==e[winner]])
    jjn <- 1  # numerator
    names(jjn) <- winner

    winner_ec_number <- e[names(e)==winner]  # this would be an integer; "ec" means "equivalence class"
    finished_ec_number <- e[names(e) %in% finished]
    if(winner_ec_number %in% finished_ec_number){  # that is, if the winner has a friend who has finished...
      jjn[] <- help[winner_ec_number]   # ...then he gets extra strength.
    }

    jjd <- rep(1,length(still_running)) # denominator
    names(jjd) <- still_running
    for(i in seq_along(still_running)){
      runner_ec_number <- e[names(e) == still_running[i]]
      if(length(finished_ec_number)>0){
        if(length(runner_ec_number) ==0){browser()}
        out
        if(runner_ec_number %in% finished_ec_number){
          jjd[i] <- help[runner_ec_number]
        }
      }
    }
    out[jjn] %<>% inc   # numerator
    out[jjd] %<>% dec   # denominator
    
    finished <- c(finished,winner)     # the winner is now finished ...
    still_running <- still_running[-1] # ... and not still running
  } #while loop closes
  return(out)
}

H3maker  <- function(help){
    (
        race_cheering3(c("a","b","c"),c(a=1,b=1,c=2), help=help)*7    +
        race_cheering3(c("a","c","b"),c(a=1,b=1,c=2), help=help)*5    +
        race_cheering3(c("c","b","a"),c(a=1,b=1,c=2), help=help)*6    +
        race_cheering3(c("b","a","c"),c(a=1,b=1,c=2), help=help)*2   
    )
}

optimand <- function(help){
    H3 <- H3maker(help)
    maxp(H3,give=TRUE,n=1)$likes
}


# function H3makera() is a "red bus-blue bus" likelihood function.
# Players `a` and `b` mutually support one another.  Thus they are
# likely to have consecutive rankings.  We see that `a` and `b` place
# consecutively all except the last one, where `c` intervenes between
# `a` and `b`.  This would suggest that the help is quite strong.


H3makera <- function(help){
    jj <- c(a=1,b=1,c=2,d=3,e=4)
    help <- c(help,1,1,1)
    (
        race_cheering3(v=c("a","b","c","d","e"),e=jj,help=help)*7 + 
        race_cheering3(v=c("b","a","e","c","d"),e=jj,help=help)*4 + 
        race_cheering3(v=c("c","a","b","d","e"),e=jj,help=help)*2 + 
        race_cheering3(v=c("d","c","a","b","e"),e=jj,help=help)*8 + 
        race_cheering3(v=c("e","b","a","c","d"),e=jj,help=help)*4 + 
        race_cheering3(v=c("e","a","c","b","d"),e=jj,help=help)*3
    )
}


o <- function(lam){maxp(H3makera(lam),give=T,n=1)$likes}
lam <- c(3:10,seq(from=20,by=10,to=120)) 
L <- sapply(lam,o)
