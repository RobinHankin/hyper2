library("hyper2")
jj <- read.table("curacao1962_candidates.txt",header=FALSE)

players <- 
c("Petrosian", "Keres", "Geller", "Fischer", "Korchnoi", "Benko", "Tal", "Filip")
names(players) <-   # nationality
c("USSR", "USSR", "USSR", "USA", "USSR", "USA", "USSR", "TCH")

d <- as.matrix(cbind(as.character(jj$V1),as.character(jj$V2)))
results <- as.character(jj$V3)
colnames(d) <- c("white","black")

stopifnot(all(c(d) %in% players))

white <- "white"
draw <- "draw"
sovdraw <- "sovdraw"

H <- hyper2(pnames=c(draw,sovdraw,white,players))


for(i in seq_len(nrow(d))){
  white_player <- d[i,1]
  black_player <- d[i,2]
  result <- results[i]
  nationality_white <- names(players)[which(players == white_player)]
  nationality_black <- names(players)[which(players == black_player)]
  
  if((nationality_black=="USSR") &   (nationality_white=="USSR")){  # two Soviets
    drawmonster <- "sovdraw"  # Soviet draw
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
    H[c(winner,loser,white,drawmonster)] %<>% dec
  } else if(result == "1/2-1/2"){
    H[c(drawmonster                               )] %<>% inc
    H[c(drawmonster,winner,loser,white,drawmonster)] %<>% inc
  } else {
    stop(paste("result = ", result, ". Should be 1-0, 0-1, or 1/2-1/2",sep=""))
  }
}  # i loop closes

## First calculate maximum support for free optimization:
support_free <- maxp(H,give=TRUE)$value


## Now calculate maximum support constrained so draw >= sovdraw:
jj <- maxp(H,
           startp=c(0.3,0.2,0.2,rep(0.3/8,7)),
           give=TRUE,
           fcm=c(1,-1,rep(0,8)),
           fcv=0)
support_constrained <- jj$value

## Difference in support:
extra_support <- support_free-support_constrained
print(extra_support)   # support
print(exp(extra_support))  # likelihood ratio


## Wilks:
print(pchisq(2*extra_support, df=1, lower.tail=FALSE))
