setwd("~/rstudio/hyper2/inst")

f <- function(a){
  a <- a[, -ncol(a)]  
  a <- as.matrix(a)
  suppressWarnings(storage.mode(a) <- "numeric")
  a
}

test <- function(a){
    a[a == 0] <- NA
    a <- apply(a,2,sort)
    out <- rep(FALSE,length(a))
    for(i in seq_along(a)){
        out[i] <- all(diff(a[i][[1]]) == 1)
    }
    return(all(out))
}

p <- c(
    "formula1_1995.txt", "formula1_1996.txt", "formula1_1997.txt",
    "formula1_1998.txt", "formula1_1999.txt", "formula1_2000.txt",
    "formula1_2001.txt", "formula1_2002.txt", "formula1_2003.txt",
    "formula1_2004.txt", "formula1_2005.txt", "formula1_2006.txt",
    "formula1_2007.txt", "formula1_2008.txt", "formula1_2009.txt",
    "formula1_2010.txt", "formula1_2011.txt", "formula1_2012.txt",
    "formula1_2013.txt", "formula1_2014.txt", "formula1_2015.txt",
    "formula1_2016.txt", "formula1_2017.txt", "formula1_2018.txt",
    "formula1_2019.txt", "formula1_2020.txt", "formula1_2021.txt",
    "formula1_2022.txt", "formula1_2023.txt", "formula1_2024.txt")

ans <- rep(FALSE, length(p))
for(i in seq_along(p)){
    ans[i] <- test(f(read.table(p[i], header=TRUE)))
}

stopifnot(all(ans))
