"summary.hyper2" <- function(object, ...){
    out <- list(
        pnames = pnames(object),
        no.of.brackets = length(brackets(object)),
        tableoflengths = table(c(lapply(brackets(object),length),recursive=TRUE)),
        sumofpowers = sum(powers(object)),
        tableofpowers = table(powers(object))
  )
  class(out) <- "summary.hyper2"
  return(out)
}

"print.summary.hyper2" <- function(x, ...){
    cat("A hyper2 object.\n")
    cat("pnames: ", x[[1]],"\n") 
    cat("Number of brackets:", x[[2]],"\n") 
    cat("Sum of powers:", x[[4]],"\n") 
    cat("Table of bracket lengths:")
    print(x[[3]])
    cat("Table of powers:\n")
    print(x[[5]])
}

