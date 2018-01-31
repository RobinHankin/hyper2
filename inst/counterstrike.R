"https://www.youtube.com/watch?v=XKWzlG4jDnI"

library(hyper2)
library(partitions) ## needed for perms()
library(magrittr) 

team1  <- c("autimatic","tarik", "Skadoodle","Stewie2k","RUSH")
team2 <- c("NiKo","olofmeister","karrigan","GuardiaN","rain")

## assumes that players are always killed by the opposing team.  If a
## player is killed by his own team, counterstrike() may return an
## error.

`counterstrike` <- function(team1,team2,deathorder){

  if(identical(sort(c(team1,team2)),sort(deathorder))){
    deathorder <- deathorder[-length(deathorder)]  # last player not killed
  }

  H <- hyper2(pnames=c(team1,team2))
  
  for(killed_player in deathorder){
    if(killed_player %in% team1){
      H[team2] <- H[team2] + 1
      H[c(team1,team2)] <- H[c(team1,team2)] - 1
      team1 %<>% "["(team1 != killed_player)      
    } else {
      H[team1] <- H[team1] + 1
      H[c(team1,team2)] <- H[c(team1,team2)] - 1
      team2 %<>% "["(team2 != killed_player)      
    }
  }
  return(H)
}

`rdeath` <- function(team1,team2){
  out <- NULL
  while(length(team1)>0 & length(team2)>0){
    if(runif(1)>length(team1)/(length(c(team1,team2)))){ ## killed person is on team1
      killed_player <- sample(team1,1)
      team1 %<>% "["(team1 != killed_player)      
    } else { ## killed person is on team2
      killed_player <- sample(team2,1)
      team2 %<>% "["(team2 != killed_player) 
    }
    out <- c(out,killed_player)
  }
  return(out)
}

## all rounds from the match, thanks to Zachary Hankin:
zachslist <- list(
    round1 =
      c("Skadoodle","olofmeister","tarik",
        "GuardiaN", "RUSH", "rain","Stewie2k",
        "karrigan","autimatic","NiKo"
        ),
    round2 =
      c("karrigan", "NiKo", "Stewie2K",
        "RUSH", "rain", "GuardiaN",
        "autimatic", "olofmeister"
        ),
    round3 =
      c("rain","tarik","autimatic",
        "karrigan","RUSH","GuardiaN",
        "Stewie2K","NiKo","olofmeister"
        ),
    round4 =
      c("rain","GuardiaN","karrigan",
        "NiKo","olofmeister"
        ),
    round5 =
      c("olofmeister","rain","karrigan",
        "tarik","Stewie2K","autimatic"
        ),
    round6 =
      c("GuardiaN","karrigan")
)


H <- hyper2(pnames=c(team1,team2))
for(i in zachslist){
  H <- H + counterstrike(team1,team2, deathorder=i)
}
dotchart(maxp(H),pch=16,main='observed data')

Hrand <- hyper2(pnames=c(team1,team2))
for(i in 1:6){
  pp <- rdeath(team1,team2)
  Hrand <- Hrand + counterstrike(team1,team2, pp)
}
dev.new()
dotchart(maxp(Hrand),pch=16,main='synthetic data')

