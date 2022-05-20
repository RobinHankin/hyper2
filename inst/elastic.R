`elastic_monster` <- function(v){  # v is a numeric named vector; l = lambda.  Use-case:  elastic_monster(c(a=1,b=1,c=1,d=2,e=2))
    players <- names(v)
    group   <- as.vector(v)
    stopifnot(all(table(players)==1))
    n <- length(v)  # number of runners
    out <- hyper3(pnames=players)


    ## First past the post is a bit different, he has no predecessor
    overall_winner <- v[ 1]    # e.g c(a=3) means competitor 'a', who is in group 3, won overall
    still_running  <- v[-1]  # all still running
    jj <- 1
    names(jj) <- names(overall_winner)
    out[jj] %<>% inc

    jj <- v
    jj[] <- 1
    out[jj] %<>% dec   # now out is the first term of a Plackett-Luce likelihood function

    while(length(v)>1){
       now_resting <- v[1]    # e.g now_resting == c(a=3) means competitor 'a', who is in group 3, is now resting.
       next_finisher <- v[2]  # e.g now_resting == c(b=2) means competitor 'b', who is in group 2, is now crossing the finishing line
       still_running <- v[-1]  # all still running  (NB: still running _includes_ the next finisher!)

       if(any(still_running %in% now_resting)){  # that is, if the field of running runners has anyone in the same group as the resting guy; if so, the next_finisher is helped by the monster
          ## numerator first
	  if(next_finisher == now_resting){  # that is, if the next finisher is in the same group as the guy now resting
            jj <- c(1,1)
  	    names(jj) <- c(names(next_finisher),"M")
	  } else {  # next finisher not in the same group as the resting guy, he won on his own
            jj <- 1
  	    names(jj) <- names(next_finisher)
          }
	  out[jj] %<>% inc

          ## denominator second
          jj <- still_running # includes next_finisher!
          jj[] <- 1
	  names(jj) <- names(still_running)
          jj <- c(jj,M=sum(still_running %in% now_resting))
          out[jj] %<>% dec
       } else {  # noone is helped by the monster
          ## numerator first
          jj <- 1
	  names(jj) <- names(next_finisher)
	  out[jj] %<>% inc

          ## denominator second
          jj <- still_running # includes next_finisher!
          jj[] <- 1
	  names(jj) <- names(still_running)
          out[jj] %<>% dec
       }

       v <- v[-1]  # field reduces by one

  } # while loop closes
  return(out)
}

`elastic_lambda` <- function(v,lambda){  # v is a numeric named vector;   Use-case:  elastic_lambda(c(a=1,b=1,c=1,d=2,e=2),1.3)
    players <- names(v)
    group   <- as.vector(v)
    stopifnot(all(table(players)==1))
    n <- length(v)  # number of runners
    out <- hyper3(pnames=players)


    ## First past the post is a bit different, he has no predecessor
    overall_winner <- v[ 1]    # e.g c(a=3) means competitor 'a', who is in group 3, won overall
    still_running  <- v[-1]  # all still running
    jj <- 1
    names(jj) <- names(overall_winner)
    out[jj] %<>% inc

    jj <- v
    jj[] <- 1
    out[jj] %<>% dec   # now out is the first term of a Plackett-Luce likelihood function

    while(length(v)>1){
       now_resting <- v[1]    # e.g now_resting == c(a=3) means competitor 'a', who is in group 3, is now resting.
       next_finisher <- v[2]  # e.g now_resting == c(b=2) means competitor 'b', who is in group 2, is now crossing the finishing line
       still_running <- v[-1]  # all still running  (NB: still running _includes_ the next finisher!)

       if(any(still_running %in% now_resting)){  # that is, if the field of running runners has anyone in the same group as the resting guy; if so, the next_finisher is helped by the monster
          ## numerator first
	  if(next_finisher == now_resting){  # that is, if the next finisher is in the same group as the guy now resting
            jj <- lambda
  	    names(jj) <- names(next_finisher)
	  } else {  # next finisher not in the same group as the resting guy, he won on his own
            jj <- 1
  	    names(jj) <- names(next_finisher)
          }
	  out[jj] %<>% inc

          ## denominator second
          jj <- still_running # includes next_finisher!
          jj[] <- 1
	  names(jj) <- names(still_running)
          jj[names(jj) == names(next_finisher)] <- lambda
          out[jj] %<>% dec
       } else {  # noone is helped by the monster
          ## numerator first
          jj <- 1
	  names(jj) <- names(next_finisher)
	  out[jj] %<>% inc

          ## denominator second
          jj <- still_running # includes next_finisher!
          jj[] <- 1
	  names(jj) <- names(still_running)
          out[jj] %<>% dec
       }

       v <- v[-1]  # field reduces by one

  } # while loop closes
  return(out)
}