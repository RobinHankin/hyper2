## This file runs a randomised version of Boston 2018:

## https://www.youtube.com/watch?v=XKWzlG4jDnI

## It then creates, and maximizes the likelihood function for the
## strength of the players.

library(hyper2)
library(partitions) ## needed for perms()
library(magrittr) 

team1  <- c("autimatic","tarik", "Skadoodle","Stewie2k","RUSH")   #Cloud9
team2 <- c("NiKo","olofmeister","karrigan","GuardiaN","rain")  # FaZe Clan

## Function counterstrike_maker() needs the identities of all players
## on each team.  It assumes that players are always killed by the
## opposing team; if a player is killed by his own team,
## counterstrike_maker() may return an error.

## In the function, the 'deathorder' argument specifies the order in
## which players were killed (the first element is the first player to
## be killed and so on).  Note that the identity of the killer is not
## needed as it is assumed that a death is due to the combined
## strength of the team, rather than the individual shooter who
## actually fired the shot.

## Note that it is not required for all the players to be killed;
## short rounds are OK (but are not as statistically informative).

## In the function, the main loop iterates through the deathorder
## vector.  Suppose team1 = (a,b,c,d) and team2 = (x,y,z); suppose the
## deathorder were (a,b,y,c).  Then the likelihood function for a's
## death would be (x+y+z)/(a+b+c+d+x+y+z) [that is, the likelihood
## function for a Bernoulli trial between team (x,y,z) and (a,b,c,d).
## Note the appearance of (a) in the denominator: he [surely!] was on
## the losing team.

## The '%<>%' line in the main loop removes the killed player from the
## appropriate team.

## The likelihood function for the full deathorder is then

## (x+y+z)/(a+b+c+d+x+y+z) * (x+y+z)/(b+c+d+x+y+z) * (c+d)/(c+d+x+y+z) * (x+z)/(c+d+x+z)

## Where the four terms correspond to the deaths of a,b,y,c
## respectively.  See how the denominator gets shorter as the teams
## die one by one.

## The function does not have strong error-checking functionality.

## File man/counterstrike.Rd has more details on the data's origin.

## Object Hrand is a randomly generated version of
## hyper2::counterstrike, created by running an in-silico deathmatch
## on the assumption of equal player strengths.

`counterstrike_maker` <- function(team1,team2,deathorder){

  if(identical(sort(c(team1,team2)),sort(deathorder))){
    deathorder <- deathorder[-length(deathorder)]  # last player not killed
  }

  H <- hyper2(pnames=c(team1,team2))
  
  for(killed_player in deathorder){
    if(killed_player %in% team1){
      H[team2] %<>% inc
      H[c(team1,team2)] %<>% dec
      team1 %<>% "["(team1 != killed_player)      
    } else {
      H[team1] %<>% inc
      H[c(team1,team2)] %<>% dec
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

Hrand <- hyper2(pnames=c(team1,team2))
for(i in 1:6){
  pp <- rdeath(team1,team2)
  Hrand <- Hrand + counterstrike_maker(team1,team2, pp)
}
dev.new()
dotchart(maxp(Hrand),pch=16,main='synthetic data')

