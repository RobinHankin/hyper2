setGeneric("suppfun",function(x){standardGeneric("suppfun")})

`suppfun` <- function(x, ...){UseMethod("suppfun")}
`suppfun.ordertable` <- function(x, ...){ordertable2supp(x, ...)}
`suppfun.character` <- function(x, nonfinishers=NULL, ...){rankvec_likelihood(x, nonfinishers=nonfinishers, ...)}
`suppfun.numeric` <- function(x, ...){ordervec2supp(x)}
