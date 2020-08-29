"summary.hyper2" <- function(object, ...){
    out <- list(
        size   = size(object),
        pnames = pnames(object),
        no.of.brackets = length(brackets(object)),
        sumofpowers = sum(powers(object)),
        tableoflengths = table(c(lapply(brackets(object),length),recursive=TRUE)),
        tableofpowers = table(powers(object))
  )
  class(out) <- "summary.hyper2"
  return(out)
}

"print.summary.hyper2" <- function(x, ...){
    cat(paste("A hyper2 object of size ", x[[1]], ".\n",sep=""))
    cat("pnames: ", x[[2]],"\n") 
    cat("Number of brackets:", x[[3]],"\n") 
    cat("Sum of powers:", x[[4]],"\n\n") 
    cat("Table of bracket lengths:")
    print(x[[5]])
    cat("\nTable of powers:\n")
    print(x[[6]])
}

