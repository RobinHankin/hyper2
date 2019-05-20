# Likelihood functions for either Curacao 1962: testing
# the three players Keres, Petrosian, Geller for collusion.  Note that
# this analysis differs from that in curacao1962_individual_games.R,
# which analyses all Soviet players together.

# The majority of this file is identical to
# curacao1962_individual_games.R, except that we encode the
# possibility of a player's being collusive as the "nationality" of a
# player being "coll"

library("hyper2")

tab <- read.table("curacao1962_candidates.txt",header=FALSE)

  players <- 
    c("Petrosian", "Keres", "Geller", "Fischer", "Korchnoi",
      "Benko", "Tal", "Filip")
  names(players) <-   # accused of collusion or not
    c("coll", "coll", "coll", "USA", "USSR", "USA", "USSR", "TCH")


results <- as.character(tab$V3)
d <- as.matrix(cbind(as.character(tab$V1),as.character(tab$V2)))
colnames(d) <- c("white","black")

stopifnot(all(c(d) %in% players))

white <- "white"
draw <- "draw"
colldraw <- "colldraw"

H <- hyper2(pnames=c(draw,colldraw,white,players))

for(i in seq_len(nrow(d))){
  white_player <- d[i,1]
  black_player <- d[i,2]
  result <- results[i]
  nationality_white <- names(players)[which(players == white_player)]
  nationality_black <- names(players)[which(players == black_player)]

  if((nationality_black=="coll") & (nationality_white=="coll")){  # two colluding players
    drawmonster <- "colldraw"  # Collusive draw
  } else {
    drawmonster <- "draw"  # regular draw
  }
  if(result == "1-0"){  # white victory
    winner <- white_player
    loser  <- black_player
    H[c(winner,white                  )] %<>% inc
    H[c(winner,white,drawmonster,loser)] %<>% dec
  } else if(result == "0-1"){ # black victory
    winner <- black_player
    loser <-  white_player
    H[c(winner                        )] %<>% inc
    H[c(winner,white,drawmonster,loser)] %<>% dec
  } else if(result == "1/2-1/2"){
    H[c(drawmonster                                )] %<>% inc
    H[c(drawmonster,white_player,black_player,white)] %<>% dec
  } else {
    stop(paste("result = ", result, ". Should be 1-0, 0-1, or 1/2-1/2",sep=""))
  }
}  # i loop closes

## First calculate maximum support for free optimization:
support_free <- maxp(H,give=TRUE)$value


## Now calculate maximum support constrained so draw >= colldraw:
jj <- maxp(H,
           startp=c(0.3,0.2,0.2,rep(0.3/(size(H)-3),size(H)-4)),
           give=TRUE,
           fcm=c(1,-1,rep(0,size(H)-3)),
           fcv=0)
support_constrained <- jj$value

## Difference in support:
extra_support <- support_free-support_constrained
print(extra_support)   # support
print(exp(extra_support))  # likelihood ratio


## Wilks:
print(pchisq(2*extra_support, df=1, lower.tail=FALSE))
