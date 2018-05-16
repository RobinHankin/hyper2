library(hyper2)
## Analysis of different years of F1 results

files <- c(
    "formula1_2014.txt",
    "formula1_2015.txt",
    "formula1_2016.txt",
    "formula1_2017.txt"
    )

d <- list()
drivers <- c()
for(i in files){
  jj <- read.table(i,header=TRUE)
  d <- list(d,jj)
  drivers <- c(drivers, as.character(jj$driver))
}

drivers <- sort(unique(drivers))

F1a <- hyper2(pnames=drivers)

likelihood_from_finishing_order <- function(H, df){
  ## 'H' a pre-existing hyper2 object, 'df' a dataframe such as
  ## read.table("formula1_2017.txt",header=T)


}








