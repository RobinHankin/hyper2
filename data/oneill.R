ic <- c("NB", "L", "PB", "THC", "OA", "WAIS")

icons <- 
    structure(list(
        brackets = list(ic[1], c(ic[1], ic[2], ic[4], ic[5]),
                        c(ic[1], ic[2], ic[4], ic[6]),
                        c(ic[1], ic[2], ic[5], ic[6]),
                        c(ic[1], ic[3], ic[4], ic[5]),
                        c(ic[1], ic[3], ic[4], ic[6]),
                        c(ic[1], ic[3], ic[5], ic[6]),
                        ic[2],
                        c(ic[2], ic[3], ic[4], ic[5]),
                        c(ic[2], ic[3], ic[4], ic[6]),
                        c(ic[2], ic[3], ic[5], ic[6]),
                        ic[3], ic[4], ic[5], ic[6]
                        ),
        powers = c(32, -20, -15, -9, -18, -18, -8, 24, -11, -16, 
                   -18, 30, 24, 14, 9),
        pnames = ic))

class(icons) <- "hyper2"

`icons_matrix` <- matrix(c(
    5 , 3 , NA,  4, NA,   3,
    3 , NA,  5,  8, NA,   2,
    NA,  4,  9,  2, NA,   1,
    10,  3, NA,  3,  4,  NA,
    4 , NA,  5,  6,  3,  NA,
    NA,  4,  3,  1,  3,  NA,
    5 ,  1, NA, NA,  1,   2,
    5 , NA,  1, NA,  1,   1,
    NA,  9,  7, NA,  2,   0)
  , byrow=TRUE,ncol=6)
colnames(icons_matrix) <- ic

rm(ic)


# NB icons == saffy(icons_matrix)
