## Tennis example on p15 of Hankin 2010


library(hyper2)


## write a throwaway function that accounts for the regular players:


p1 <- "p1"
p2 <- "p2"
p3 <- "p3"
p4 <- "p4"
G <- "ghost"

H <- hyper2()

## 1&3 vs 2&4, scoreline 4-4:
H[c(p1,p3)] %<>% inc(4)
H[c(p2,p4)] %<>% inc(4)
H[c(p1,p2,p3,p4)] %<>% dec(8)

## 1&4 vs 2&3, scoreline 6-7:
H[c(p1,p4)] %<>% inc(6)
H[c(p2,p3)] %<>% inc(7)
H[c(p1,p2,p3,p4)] %<>% dec(13)

## 1 vs 3, scoreline 10-14:
H[p1] %<>% inc(10)
H[p3] %<>% inc(14)
H[c(p1,p3)] %<>% dec(24)

## 2 vs 3, scoreline 12-14:
H[p2] %<>% inc(12)
H[p3] %<>% inc(14)
H[c(p2,p3)] %<>% dec(26)

## 1 vs 4, scoreline 10-14:
H[p1] %<>% inc(10)
H[p4] %<>% inc(14)
H[c(p1,p4)] %<>% dec(24)

## 2 vs 4, scoreline 11-10:
H[p2] %<>% inc(11)
H[p4] %<>% inc(10)
H[c(p2,p4)] %<>% dec(21)

## 3 vs 4, scoreline 13-13:
H[p3] %<>% inc(13)
H[p4] %<>% inc(13)
H[c(p3,p4)] %<>% dec(26)

# First, analysis with no ghost, H_noghost:


H_noghost <- H

## 1&2 vs 3&4, scoreline 9-2:
H_noghost[c(p1,p2)] %<>% inc(9)
H_noghost[c(p3,p4)] %<>% inc(2)
H_noghost[c(p1,p2,p3,p4)] %<>% dec(11)

## Now the ghost, which is player number 5:

H_ghost <- H

## 1&2 vs 3&4 (NB: includes ghost!), scoreline 9-2 (again):
H_ghost[c(p1,p2,G)] %<>% inc(9)
H_ghost[c(p3,p4)] %<>% inc(2)
H_ghost[c(p1,p2,p3,p4,G)] %<>% dec(11)

