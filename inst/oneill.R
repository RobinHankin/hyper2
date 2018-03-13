## This file creates 'icons' as appearing on page 10 of RKS Hankin
## 2010, 'A Generalization of the Dirichlet distribution', Journal of
## Statistical Software, volume 33, issue 11.  Note that this file
## corrects a typo appearing in the paper.


## First specify the matrix as appearing in Hankin 2010
M <- matrix(c(
    5 , 3 , NA,   4, NA,   3,
    3 , NA,  5,   8, NA,   2,
    NA,  4,  9,   2, NA,   1,
    10,  3, NA,   3,  4,  NA,
    4 , NA,  5,   6,  3,  NA,
    NA,  4,  3,   1,  3,  NA,
    5 ,  1, NA,  NA,  1,   2,
    5 , NA,  1,  NA,  1,   1,
    NA,  9,  7,  NA,  2,   0)
  , byrow=TRUE,ncol=6)
colnames(M) <- c("NB","L","PB","THC","OA","WAIS")

## set up the hyper2 object with correct names:
icons <- hyper2(pnames=colnames(M))

## iterate through each row to work out the denominators:
for(i in seq_len(nrow(M))){
  onerow <- M[i,]
  ## variable 'choices' = icons presented to that row of respondents:
  choices <- names(onerow[!is.na(onerow)])
  icons[choices] <-   icons[choices] - sum(onerow,na.rm=TRUE)
}

## Now iterate through columns to find numerators:
for(j in seq_len(ncol(M))){
  icons[colnames(M)[j]] <- icons[colnames(M)[j]] + sum(M[,j],na.rm=TRUE)
}



