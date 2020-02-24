## Analysis of the 2018 World Karate Championships, men's 67kg.

## Wikipedia contributors. (2020, February 2). 2018 World Karate
## Championships – Men's 67 kg. In Wikipedia, The Free
## Encyclopedia. Retrieved 00:21, February 24, 2020, from
## https://en.wikipedia.org/w/index.php?title=2018_World_Karate_Championships_%E2%80%93_Men%27s_67_kg&oldid=938728758



set.seed(0)
library("hyper2")
library("magrittr")

a <- read.table("karate.txt",colClasses = "character",header=TRUE)
a$wins1 <-  as.numeric(a$wins1)
a$wins2 <-  as.numeric(a$wins2)

H <- hyper2(pnames=sort(unique(c(a$karateka1,a$karateka2))))
for(i in seq_len(nrow(a))){

  p1 <- a$karateka1[i]
  p2 <- a$karateka2[i]

  w1 <- a$wins1[i]
  w2 <- a$wins2[i]

  
  H[p1] %<>% inc(w1)
  H[p2] %<>% inc(w2)
  H[c(p1,p2)] %<>% dec(w1+w2)
}


Hcut <- H
for(i in 1:30){
  print(paste("iteration ",i,", size(H) = ",size(Hcut),sep=""))
  m <- maxp(Hcut)
  too_small <- (m < 1e-3)
  if(any(too_small)){
    Hcut %<>% discard(names(m[too_small]))
  } else {
    break
  }
}
