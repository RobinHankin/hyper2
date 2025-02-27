setGeneric("suppfun",function(x){standardGeneric("suppfun")})

`suppfun` <- function(x){UseMethod("suppfun")}
`suppfun.ordertable` <- function(x){ordertable2supp(x)}
`suppfun.ranktable` <- function(x){"not yet implemented"}
`suppfun.preftable` <- function(x){"not yet implemented"}
