#' ---
#' title: "Replication script"
#' author: Robin Hankin
#' date: 26 June 2023
#' output:
#'   html_document:
#'     keep_tex: true
#' ---
#'
#' This R script aims at reproducing all figures, tables and other output presented in the manuscript.
#' It includes the R functions and usage examples.  We understand the likelihood functions to  be defined only for non-negative strengths.
#'
#'
#+ include = FALSE
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
options(scipen = 1, digits = 3)


#' Load packages:
library("hyper2")
date()

###################################################
### code chunk number 1: setup
###################################################
suppressMessages(library("knitr",quietly=TRUE,verbose=FALSE))
suppressMessages(library("hyper2",quietly=TRUE,verbose=FALSE))
opts_chunk$set(cache=TRUE, autodep=TRUE)


###################################################
### code chunk number 2: jss_hyper3.Rnw:220-224
###################################################


## Illustration: creating a simple hyper3 object
## using named vectors.  Here the likelihood function is a/(3a+2b+c),
## a+b+c=1.

LL <- hyper3()                   ## initialization:  LL is an empty hyper3 object.
LL[c(a = 1)] <- 1                ## First term:  numerator of 'a'
LL[c(a = 3, b = 2, c = 1)] <- -1 ## Second term: denominator (3a+2b+c), with power (-1)
LL                               ## illustration of print method



###################################################
### code chunk number 3: jss_hyper3.Rnw:234-236
###################################################

## Use case of log-likelihood as implemented by function loglik().
## We will evaluate log-likelihood function LL at two points on the
## two-simplex:

loglik(c(a = 0.01, b = 0.01, c = 0.98), LL)    # L(0.01,0.01,0.98)
loglik(c(a = 0.90, b = 0.05, c = 0.05), LL)    # L(0.90,0.05,0.05)



###################################################
### code chunk number 4: jss_hyper3.Rnw:251-252
###################################################

## Illustration of an order statistic, with preferred interpretation
## of a race between three clones of strength "a", two of strength
## "b", and a singleton of strength "c".


(H <- ordervec2supp3(c("a", "c", "b", "a", "a", "b")))  


###################################################
### code chunk number 5: maxexamp
###################################################

## function maxp() is S3 generic, here returning the maximum
## likelihood estimate for competitors a,b,c

(mH <- maxp(H))  



###################################################
### code chunk number 6: testequality
###################################################

## builtin function equalp.test() is one of a family of functions for
## testing a range of interesting nulls for compositional data

equalp.test(H) 



###################################################
### code chunk number 7: maxpaba
###################################################

## function maxp() used to illustrate a simple use-case of two twins
## and a singleton:

maxp(ordervec2supp3(c("a", "b", "a")))  



###################################################
### code chunk number 8: jss_hyper3.Rnw:346-348
###################################################

## likelihood function for one-simplex {a,b|a+b=1}, in a form suitable
## for plotting

a <- 1/2 # null hypothesis H0
(S_delta <- log(a * (1 - a)/(1 + a)) - log(3 - 2 * sqrt(2)))


###################################################
### code chunk number 9: jss_hyper3.Rnw:358-359
###################################################

## calculate the p-value of H0 above, using the asymptotic
## distribution of the log-likelihood:

pchisq(-2 * S_delta, df = 1, lower.tail = FALSE)



###################################################
### code chunk number 10: figaba
###################################################


## plot a support function for the observation a>b>a over the
## one-simplex {a,b|a+b=1} [figure 1]

a <- seq(from = 0, by = 0.005, to = 1)                             # specify horizontal axis
S <- function(a){log(a * (1 - a) / ((1 + a) * (3 - 2 * sqrt(2))))} # likelihood function for a>b>a
plot(a, S(a), type = 'b',xlab=expression(p[a]),ylab="support")     # plot
abline(h = c(0, -2))                                               # annotations [two units-of-support]
abline(v = c(0.02438102, 0.9524271), col = 'red')                  # annotations [credible interval]
abline(v = sqrt(2) - 1)                                            # annotations [evaluate]

###################################################
### code chunk number 11: figabbabb
###################################################

##  plot harmonised likelihood functions for the three possible order
##  statistics [viz a>a>b, a>b>a, b>a>a]

f_aab <- function(a){a^2 / (1 + a)}            # L(a>a>b)
f_aba <- function(a){a * (1 - a) / (1 + a)^2}  # L(a>b>a)
f_baa <- function(a){(1 - a) / (1 + a)}        # L(b>a>a)
p <- function(f, ...){                         # generic plot routine
  a <- seq(from = 0, by = 0.005, to = 1)
  points(a, f(a) / max(f(a)), ...)
  }
plot(0:1, 0:1, xlab = expression(p[a]), ylab = "Likelihood", type = "n")  # empty plot
p(f_aab, type = "l", col = "black")        # L(a>a>b)  
p(f_aba, type = "l", col = "red")          # L(a>b>a)
p(f_baa, type = "l", col = "blue")         # L(b>a>a)
text(0.8,0.8,"AAB")                        # annotation
text(0.8,0.5,"ABA",col="red")              # annotation
text(0.8,0.15,"BAA",col="blue")            # annotation
abline(h = exp(-2), lty = 2)               # two units-of-support criterion



###################################################
### code chunk number 12: jss_hyper3.Rnw:453-454
###################################################

## illustrate a more general observation, here a>b>>{a,b}; function
## ordervec2supp3() allows the user to specify competitors who did not
## finish.

maxp(ordervec2supp3(c("a", "b"), nonfinishers=c("a", "b")))



###################################################
### code chunk number 13: define_xy_wilcox
###################################################

## placenta dataset as used in base::wilcox.Rd, here used to
## illustrate Plackett-Luce approach to nonparametric tests:

x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)



###################################################
### code chunk number 14: hyper3osdef
###################################################

## package idiom to test null of equal Plackett-Luce strength of x and
## y:

names(x) <- rep("x", length(x))  
names(y) <- rep("y", length(y))
(os <- names(sort(c(x, y))))  # here "os" means "order statistic"



###################################################
### code chunk number 15: hyper3xytest
###################################################

## Again use package idiom ordervec2supp3() but wit the the order
## statistic specified above for the placenta dataset:

Hxy <- ordervec2supp3(os)    # create the likelihood function Hxy...
equalp.test(Hxy)             # ... and test the null that p_x=p_y



###################################################
### code chunk number 16: plotwilcoxlike
###################################################

## show the likelihood function for the Plackett-Luce strength of "a"

a <- seq(from = 0.02, to = 0.8, len = 40)   # horizontal axis
L <- sapply(a, function(p){loglik(p, Hxy)}) # vectorized idiom for loglikelihood function
plot(a, L - max(L), type = 'b',xlab=expression(p[a]),ylab="likelihood") # plot normalized loglikelihood
abline(h = c(0, -2))    # two-units-of-support criterion
abline(v = c(0.24))     # evaluate
abline(v=c(0.5), lty=2) # null



###################################################
### code chunk number 17: javelintable
###################################################

## Show explicitly the dataset used for the Plackett-Luce strength of
## the javelin competitors:

javelin_table



###################################################
### code chunk number 18: converttosupp3
###################################################

## Use more sophisticated bespoke package idiom, here
## attemptstable2supp3(), which returns a hyper3 likelihood function
## for the entire dataset, 


javelin_vector <-
  attemptstable2supp3(
      javelin_table,     # primary dataset
      decreasing = TRUE, # decreasing=TRUE specifies that high
                         # numerical values win [compare race times,
                         # where low numerical values win]
      give.supp = FALSE) # return the order statistic, not its support function

options(width = 60)      # formatting for output
javelin_vector           # return order statistic for inspection



###################################################
### code chunk number 19: dothething2
###################################################

## Now use ordervec2supp3() with the javelin order statistic
## calculated above to give a support function; discard no-throws

javelin <- ordervec2supp3(v = names(javelin_vector)[!is.na(javelin_vector)])



###################################################
### code chunk number 20: setdigits
###################################################

## formatting

options(digits = 3)



###################################################
### code chunk number 21: testthejav
###################################################

## Now maximize the likelihood over the 7-simplex corresponding to the
## javelin throwers' Plackett-Luce strengths:

(mj <- maxp(javelin))                                           # use optimization to find evaluate
dotchart(mj, pch = 16,xlab="Estimated Bradley-Terry strength")  # visual plot of evaluate



###################################################
### code chunk number 22: havealook
###################################################

## generate a log-contrast plot for LC=log(p_Vadlejch / p_Vesely),
## using a bespoke function f(), which leverages output from
## specificp.test() when used to assess a null of Vesely having a
## particular strength.

f <- function(s){  
  jj <- specificp.test(javelin, "Vesely", s, n = 2)  
  p <- jj$null_estimate
  return( # return a vector of two numeric values.  The first is the
          # log-contrast LC [used as a horizontal axis in the next
          # chunk] and the second is the maximum support for the
          # particular value of p_Vesely
      c(log(p[6] / p[7]), jj$null_support)
  )
}
Ves <- seq(from = 0.0199, to = 0.33, len = 16)  # specify Vesely's Plackett-Luce strength
M <- sapply(Ves, f)  # apply function f() defined above to return LC and its support
M[2,] <- M[2,] - max(M[2,]) # normalize 
rownames(M) <- c("logcontrast", "support")  # cosmetic 



###################################################
### code chunk number 23: plottheloglikcont
###################################################

## Plot  the log-contrast dataset calculated in the previous chunk


colnames(M) <-  as.character(Ves)
plot(t(M), type = "b")   # plot the figure
abline(h = c(0, -2))     # two-units-of support criterion
abline(v = 0, lty = 2)   # null 
abline(v = log(0.32062833 / 0.11402735)) # evaluate



###################################################
### code chunk number 24: showconstructortable
###################################################

# show the constructors' championship dataset used in the manuscript

constructor_2021_table[, 1:9]



###################################################
### code chunk number 25: maxpconstructor2021
###################################################

## Now Assess whether Mercedes have in fact decreased in strength
## between 2020 and 2021.  Determine hyper3 likelihood functions
# for the two years:

const2020 <- ordertable2supp3(constructor_2020_table)  # likelihood function for constructors 2020
const2021 <- ordertable2supp3(constructor_2021_table)  # likelihood function for constructors 2021
options(digits = 4)     # formatting 
maxp(const2020, n = 1)  # show maximum likelihood estimate for 2020
maxp(const2021, n = 1)  # show maximum likelihood estimate for 2021



###################################################
### code chunk number 26: definecombinedlikelihoodfunction
###################################################

## Now use psubs() to distinguish 2020 results from 2021.  Effectively
## define two teams, "Merc2020" for Mercedes 2020, and "Merc2021" for
## 2021.  Note that the resulting likelihood function is very long and
## difficult to interpret, which is why it is not printed in the
## manuscript.

H <-
  (
    psubs(constructor_2020, "Merc", "Merc2020")   ## psubs() substitutes "Merc" for "Merc2020"
         ## "+" is overloaded in the package.   Here, it
    +    ## corresponds to addition of (independent) log-likelihood
         ## functions.
    psubs(constructor_2021, "Merc", "Merc2021")  # psubs() used again but for 2021
  )



###################################################
### code chunk number 27: usesamep
###################################################

## Test the null that Mercedes had the same strength in 2020 as 2021, viz
## H0:p_Merc2020 == p_Merc2021:


options(digits = 4)
samep.test(H, c("Merc2020", "Merc2021"))

date()

