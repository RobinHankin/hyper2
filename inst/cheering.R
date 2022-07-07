

`race_cheering3` <- function(v,e,help){
  ## competitors abcdef, with (abc) 1.1 mutual friends with helpfulness 1.1 and (de) helpfulness 1.3:
  ## race_cheering3(c("a","b","c","d","e","f"),c(a=1,b=1,c=1,d=2,e=2,f=3), help=c(1.1,1.3))

  stopifnot(all(table(v)==1))
  stopifnot(all(table(names(e))==1))
  stopifnot(all(sort(names(e)) == sort(v)))
  get_eq <- function(player){ names(e[e==e[player]]) }
    
  out <- hyper3()

  finished <- NULL
  still_running <- v
  while(length(still_running) > 0){
    winner <- still_running[1]  # winner is the first element of the field
    winners_friends <- get_eq(winner)
    jjn <- 1  # numerator
    names(jjn) <- winner

    winner_ec_number <- e[names(e)==winner]  # this would be an integer
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
    out[jjn] %<>% inc
    out[jjd] %<>% dec
    
    finished <- c(finished,winner)
    still_running <- still_running[still_running != winner]
  } #while loop closes
  return(out)
}






  
