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

H <- hyper2(pnames=c(white,draw,sovdraw,players))


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
    loser <- black_player
    H[c(winner,white                  )] %<>% inc
    H[c(winner,white,drawmonster,loser)] %<>% dec
  } else if(result == "0-1"){ # black victory
    winner <- black_player
    loser <- white_player
    H[c(winner                        )] %<>% inc
    H[c(winner,loser,white,drawmonster)] %<>% dec
  } else if(result == "1/2-1/2"){
    H[c(drawmonster                               )] %<>% inc
    H[c(drawmonster,winner,loser,white,drawmonster)] %<>% inc
  } else {
    stop(paste("result = ", result, ". Should be 1-0, 0-1, or 1/2-1/2",sep=""))
  }
}  # i loop closes



    

 
