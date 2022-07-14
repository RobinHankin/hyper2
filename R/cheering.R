`cheering3` <- function(v,e,help){

  ## competitors abcdef, with two groups of mutual friends: (abd) with
  ## mutual helpfulness 1.1, and (cf) with mutual helpfulness 1.3;
  ## competitors e and g are in groups of 1.

  ## use-case:
  ## cheering3(v=letters[1:7],e=c(a=1,b=1,c=2,d=1,f=2),help=c(1.88,1.66))


  stopifnot(all(table(names(e))==1))
  stopifnot(all(e>0))
  stopifnot(all(e==round(e)))
  stopifnot(all(table(v)==1))
  stopifnot(all(names(e) %in% v)) 

  no_group <- !(v %in% names(e))
  if(any(no_group)){
    jj <- seq(from=max(e)+1,len=sum(no_group))
    names(jj) <- v[no_group]   # a character vector
    e <- c(e,jj)
  }

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

