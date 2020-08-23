library("hyper2")
library("magrittr")

## This script takes about ten minutes to run.  It creates hyper2
## objects 'F1_2014' through 'F1_2017', which are the likelihood
## functions for the F1 results table for each of the years.

## At the end of the script, hyper2 object 'F1_total' is created,
## which is a likelihood function for all years' data combined.


## use-case for F1_likelihood():

## R> F1_likelihood(wiki_table=read.table("formula1_2017.txt",header=TRUE))

## Files like 'formula1_2017.txt' are directly copied from Wikipedia
## (with slight whitespace changes)

f <- function(M){M[,-ncol(M)]} # removes 'points' column
o <- function(k){ordertable2supp(f(read.table(k)))}

F1_2012 <- o("formula1_2012.txt")
F1_2013 <- o("formula1_2013.txt")
F1_2014 <- o("formula1_2014.txt")
F1_2015 <- o("formula1_2015.txt")
F1_2016 <- o("formula1_2016.txt")
F1_2017 <- o("formula1_2017.txt")
F1_2018 <- o("formula1_2018.txt")
F1_2019 <- o("formula1_2019.txt")


## Do the 2017 season:
m <- maxp(F1_2017)
dotchart(m,pch=16,main='2017 season')

dev.new()

F1_total <-
    F1_2012 + F1_2013 + F1_2014 + F1_2015 + F1_2016 + F1_2017 + F1_2018 + F1_2019 


mallyears <- maxp(F1_total)

dev.new()
dotchart(mallyears,pch=16,main='Formula 1, 2012-9')
