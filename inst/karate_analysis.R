## Analysis of the 2018 World Karate Championships, men's 67kg.

## Wikipedia contributors. (2020, February 2). 2018 World Karate
## Championships – Men's 67 kg. In Wikipedia, The Free
## Encyclopedia. Retrieved 00:21, February 24, 2020, from
## https://en.wikipedia.org/w/index.php?title=2018_World_Karate_Championships_%E2%80%93_Men%27s_67_kg&oldid=938728758

library("hyper2")
library("magrittr")

a <- read.table("karate.txt",colClasses = "character")


comp1 <- a$V1
wins1 <- as.numeric(a$V2)

comp2 <- a$V3
wins2 <- as.numeric(a$V4)


H <- hyper2(pnames=sort(unique(c(comp1,comp2))))
for(i in seq_len(nrow(a))){

  comp1 <- a$V1[i]
  comp2 <- a$V3[i]
  wins1 <- as.numeric(a$V2)[i]
  wins2 <- as.numeric(a$V4)[i]
  
  H[comp1] %<>% inc(wins1)
  H[comp2] %<>% inc(wins2)
  H[c(comp1,comp2)] %<>% dec(wins1+wins2)
}

