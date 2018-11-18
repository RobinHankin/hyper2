`icons` <-
structure(list(brackets = list(1, c(1, 2, 4, 5), c(1, 2, 4, 6
), c(1, 2, 5, 6), c(1, 3, 4, 5), c(1, 3, 4, 6), c(1, 3, 5, 6), 
    2, c(2, 3, 4, 5), c(2, 3, 4, 6), c(2, 3, 5, 6), 3, 4, 5, 
    6), powers = c(32, -20, -15, -9, -18, -18, -8, 24, -11, -16, 
-18, 30, 24, 14, 9), pnames = c("NB", "L", "PB", "THC", "OA", 
"WAIS")), .Names = c("brackets", "powers", "pnames"), class = "hyper2")


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
colnames(icons_matrix) <- c("NB","L","PB","THC","OA","WAIS")


# NB icons == saffy(icons_matrix)
