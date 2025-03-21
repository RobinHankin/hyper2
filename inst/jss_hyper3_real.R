### R code from vignette source 'jss_hyper3_real.Rnw' [under version
### control at github, in directory hyper2/inst] with occasional
### additional documentation and instructions.  This file,
### jss_hyper3_real.R, is also under version control in the same
### directory.


### This file takes about an hour to run without cache.



###################################################
### code chunk number 1: preliminaries
###################################################
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
library("hyper2")
library("magrittr")
howmany <- 23  # set to a smaller value to save time


###################################################
### code chunk number 2: defineoldfirm
###################################################

## Below we see hyper3 idiom for creating a hyper3 likelihood
## function, in this case using natural language and magrittr pipes.
## The function takes a single numeric argument, lambda, representing
## the home advantage.  We start with an empty likelihood function H
## with two Bradley-Terry entities [Rangers and Celtic] and populate
## it with two sets of results, each with three terms.  The first set
## of three corresponds to Rangers at home, the second to Celtic at
## home.  Looking at first set of three: these correspond to 81+42=123
## matches with Rangers at home, of which they won 81 and lost 42.  We
## see that the 81 won games are won with lambda helping (if indeed
## lambda is greater than 1).

oldfirm <- function(lambda){
    H <- hyper3(pnames = c("Rangers", "Celtic"))
    H[c(Rangers = lambda            )] %<>%      add(81     )
    H[c(                  Celtic = 1)] %<>%      add(     42)
    H[c(Rangers = lambda, Celtic = 1)] %<>% subtract(81 + 42)

    H[c(Celtic = lambda             )] %<>%      add(     76)
    H[c(                 Rangers = 1)] %<>%      add(46     )
    H[c(Celtic = lambda, Rangers = 1)] %<>% subtract(46 + 76)
    return(H)
}

###################################################
### code chunk number 3: useoldfirm
###################################################

## Below we see function oldfirm() being called with a lambda value of
## 1.88 [results pasted for convenience].  We see the bespoke print
## method using named vectors.  Observe that the order of the terms is
## implementation-specific, for the package follows disordR
## discipline.

oldfirm(1.88)

## Output reproduced below for convenience:

## log( (Celtic=1)^42 * (Celtic=1, Rangers=1.88)^-123 *
## (Celtic=1.88)^76 * (Celtic=1.88, Rangers=1)^-122 *
## (Rangers=1)^46 * (Rangers=1.88)^81)




###################################################
### code chunk number 4: profilelikeoldfirm
###################################################

## Below we define a function of multiplicative term lambda, which
## returns the log-likelihood, maximized over Bradley-Terry strengths,
## for the supplied value of lambda.  This furnishes a profile support
## function mapping lambda to support.  We apply this function to a
## range of lambda values, from 0.9 to 3.  


f <- function(lambda){maxp(oldfirm(lambda), give = 1)$`Log-likelihood`}
lam <- seq(from = 0.9, to = 3, len = howmany)
logLike <- sapply(lam, f)



###################################################
### code chunk number 5: plotproflikeoldfirm
###################################################

## Below we plot the log-likelihood function defined above.

plot(lam, logLike - max(logLike), type = 'b')
abline(v = 1, col = 'gray')
segments(x0 = 1.785, y0 = 0.1, y1 = -1, col = 'red')
abline(h = c(0, -2), col = 'gray')


###################################################
### code chunk number 6: optimoldfirm
###################################################

## Below we maximize the log-likelihood using base R function
## optimize(), for use in likelihood ratio tests.


(lambda_max <-  optimize(f, c(1.5, 2.0), maximum = TRUE)$maximum)


###################################################
### code chunk number 7: mleoldfirm
###################################################

## Below we explicitly display the evaluate at the maximized value of
## lambda.

maxp(oldfirm(lambda_max))


###################################################
### code chunk number 8: eg_RCLF3_lf
###################################################

## Below, we apply standard package function home_away3() to the
## Rangers-Celtic-Livingston-Falkirk results table, with arbitrary
## illustrative values for the multiplicative parameters lambda and D
## of 1.7 and 0.3 respectively.

home_draw_away3(RCLF3_table, lambda = 1.7, D = 0.3)


###################################################
### code chunk number 9: defineF
###################################################

## Below we define function f(), which takes a length-two numeric
## vector as argument v.  v[1] is lambda and v[2] is D; the function
## returns the profile log-likelihood at (lambda, D).

f <- function(v){
    H <- home_draw_away3(RCLF3_table, lambda = v[1], D = v[2])
    maxp(H, give = 1)$`Log-likelihood`
}


###################################################
### code chunk number 10: findmaxlambdaD
###################################################

## Below we optimize function f() [defined immediately above] to find
## the maximum likelihood estimate for (lambda, D)


maxv <- optim(par=c(2, 0.3), fn = f, control = list(fnscale = -1))$par
maxv


###################################################
### code chunk number 11: showevaluate
###################################################

## Below we find the maximum likelihood estimate for Bradley-Terry
## strengths conditional on the estimated values of (lambda, D)

maxp(home_draw_away3(RCLF3_table, lambda = maxv[1], D = maxv[2]))


###################################################
### code chunk number 12: makecontourdata
###################################################

## Below we calculate log-likelihoods as a function of lambda and D on
## a regular grid of values given by expand.grid() 

n <- howmany
lambda <- seq(from = 1, to = 3, len = n)
D <- seq(from = 0.1, to = 0.6, len = n)
V <- as.matrix(expand.grid(lambda, D))
LL <- apply(V, 1, f)
LL <- pmax(-20, LL - max(LL))


###################################################
### code chunk number 13: docontourplot
###################################################

## Below we plot a contour diagram showing log-likelihood as function
## of lambda on the x-axis and D on the y-axis

contour(lambda, D, matrix(LL, n, n), levels = -2*(1:9),
        xlab = expression(lambda), ylab = expression(D))
points(maxv[1], maxv[2], pch = 16, col = 'red')
abline(v=1, col = 'gray')


###################################################
### code chunk number 14: testlambdazero
###################################################

## Below we find the maximum likelihood estimate for the above
## problem, constrained so that lambda=1

jj <- optimize(f = function(D){f(c(lambda=1, D))}, interval = c(0.1, 0.4), maximum = TRUE)


###################################################
### code chunk number 15: defineredbuslf
###################################################

## Below we define bespoke function RB_BB_LF() which returns a hyper3
## log-likelihood function for our fictitious dataset on transport
## preferences, conditional on the supplied version of lambda.


`RB_BB_LF` <- function(lambda){
    ec <- c(C = 1, T = 2, RB = 3, BB = 3, W = 4) 
    h <- c(1, 1, lambda, 1)           
    (
        cheering3(v=c("RB", "BB", "C" , "T", "W"), e = ec, h = h) * 3 + 
        cheering3(v=c("BB", "RB", "T" , "C", "W"), e = ec, h = h) * 2 + 
        cheering3(v=c("T" , "BB", "RB", "C", "W"), e = ec, h = h) * 5 + 
        cheering3(v=c("W" , "BB", "RB", "T", "C"), e = ec, h = h) * 4 + 
        cheering3(v=c("C" , "RB", "BB", "W", "T"), e = ec, h = h) * 3 + 
        cheering3(v=c("BB", "C" , "RB", "T", "W"), e = ec, h = h) * 3
    )
}


###################################################
### code chunk number 16: definefunctiono
###################################################

## below we define function o() that takes a numerical value of lambda
## and returns the log-likelihood at the MLE of Bradley Terry
## strengths.

o <- function(lambda){maxp(RB_BB_LF(lambda), give = 1)$`Log-likelihood`} 


###################################################
### code chunk number 17: maxlikesim
###################################################

## Below we find the best-supported value of lambda for the fictitious
## red bus blue bus dataset.

(osup <- optimize(o, c(10, 40), maximum = TRUE))


###################################################
### code chunk number 18: LRT_RB
###################################################

## Below we find the difference in support between the support at the
## MLE for lambda and the null of lambda=1

(suppdiff <- osup$objective - o(lambda = 1))


###################################################
### code chunk number 19: plotproflike
###################################################

## Code below would plot a profile support curve for lambda (but was
## cut from the final version for reasons of brevity)

if(FALSE){
    lambda <- exp(seq(from = log(0.9), to = log(67), len = 19)) 
    L <- sapply(lambda, o)
    plot(log(lambda), L - max(L), type = "b")
    abline(h = c(0, -2))
    abline(v = 0)
}


###################################################
### code chunk number 20: showsushitable
###################################################

## Below we show part of the sushi preference dataset

noquote(head(sushi_table))


###################################################
### code chunk number 21: show_sushi_eq_classes
###################################################

## Below we show the equivalence classes of the different types of
## sushi

sushi_eq_classes


###################################################
### code chunk number 22: defcalculate_sushi_H
###################################################

## Below we define function make_sushi_H() which takes a particular
## numerical value of lambda and returns a hyper3 log-likelihood
## function.

make_sushi_H <- function(lambda){
    H <- hyper3()
    for(i in seq_len(nrow(sushi_table))){
        H <- H + cheering3(sushi_table[i,], e = sushi_eq_classes, h = c(1, lambda))
    }
    return(H)
}


###################################################
### code chunk number 23: calclikesushi
###################################################

## Below we define function f() which takes a particular value of
## lambda and returns the log-likelihood at the optimized value of
## Bradley-Terry strengths of the sushi


f <- function(lambda){
   H <- make_sushi_H(lambda)
   maxp(H, give = 1)$`Log-likelihood`
}
s <- exp(seq(from = log(0.9), to = log(10), len = howmany))
L <- sapply(s, f)


###################################################
### code chunk number 24: findstuffsushi
###################################################

## Below we find the maximum-likelihood estimate for lambda

osushi <- optimize(f, c(2, 4), maximum = TRUE)


###################################################
### code chunk number 25: tinysushi
###################################################

## Below we define the maximum likelihood estimate for lambda as
## 'sushimaxlambda' and also find the difference in support between
## this value of lambda and the null of lambda=1

sushimaxlambda <- osushi$maximum
sushisupport <- osushi$objective - f(1)


###################################################
### code chunk number 26: findothersushistuff
###################################################

## below we define function g() in such a way that g(x)=0 gives -2
## units of support relative to the evaluate.  This is useful to find
## the credible interval from sushi_lower to sushi_upper


g <- function(lambda){f(lambda) - osushi$objective + 2}
sushi_lower <- uniroot(g, interval = exp(c(0, 1)), tol = 0.01)$root
sushi_upper <- uniroot(g, interval = exp(c(1, 2)), tol = 0.01)$root


###################################################
### code chunk number 27: plotlikesushi
###################################################

## Below we plot the likelihood function L calculated in chunk
## calclikesushi and add some embellishments such as marking the
## credible interval, the null, and the evaluate

plot(log(s), L - max(L), type = 'b',
     xlab = expression(log(lambda)), ylab = 'Log-likelihood')
abline(v = 0, col = 'gray')
abline(h = c(0, -2), col = 'gray')
segments(x0 = log(sushimaxlambda), y0 =  0.1, y1 = -1.0, col = 'red' )
segments(x0 = log(sushi_lower    ), y0 = -2.2, y1 = -1.8, col = 'blue')
segments(x0 = log(sushi_upper    ), y0 = -2.2, y1 = -1.8, col = 'blue')


###################################################
### code chunk number 28: jss_hyper3_real.Rnw:1013-1015
###################################################

## Below we read the Boston 2018 dataset in and display some of it

a <- read.table("cloud9_boston_2018.txt", header = TRUE)
a[, -1]


###################################################
### code chunk number 29: defredbusfunc
###################################################

## Below we define function mk() which returns a hyper3 log-likelihood
## function for the counterstrike dataset given lambda, which
## quantifies the red bus-blue bus effect for the given equivalence
## classes of {{Skadoodle, Stewie2k}, {<everyone else>}}.  Function
## f() finds the support at the maximum likelihood BT strengths for
## the Cloud9 team


mk <- function(lam){
    e <- c(Skadoodle = 2, RUSH = 1, tarik = 1, autimatic = 1, Stewie2K = 2)
    H3 <- hyper3()
    for(i in seq_len(nrow(a))){
        d <- strsplit(a$deathorder[i],", ")[[1]]
        H3 <- H3 + cheering3(d, e = e, help = c(1, lam))
    }
    return(H3)
}
f <- function(l){maxp(mk(l), give=1)$`Log-likelihood`}


###################################################
### code chunk number 30: csgo_findopt
###################################################

## Below we  optimize f() defined in chunk defredbusfunc

csgo_optimize <- optimize(f, c(0.09, 0.14), maximum = TRUE, tol = 0.01)
csgo_optlam   <- csgo_optimize$maximum
csgo_support  <- csgo_optimize$objective - f(1)
csgo_pvalue   <- pchisq(csgo_support, df = 1, lower.tail = FALSE)


###################################################
### code chunk number 31: csgo_calcklikelambda
###################################################

## Below we evaluate function f() defined in defredbusfunc for a
## sequence of lambda values.

lam <- exp(seq(from = -4, to = log(1.1), len = howmany))
L <- sapply(lam, f)


###################################################
### code chunk number 32: cgso_findinterval
###################################################

## Below we find the evaluate and credible interval for the
## counterstrike dataset

g <- function(l){f(l) - csgo_optimize$objective  + 2}
csgo_lower <- uniroot(g, interval = exp(c(-4, -3)), tol = 0.01)$root
csgo_upper <- uniroot(g, interval = exp(c(-1, -0)), tol = 0.01)$root


###################################################
### code chunk number 33: csgo_plotlikelam
###################################################

## Below we plot the profile support function for lambda in the
## counterstrike dataset

plot(log(lam), L - max(L), type='b')
segments(x0 = log(csgo_optlam),y0 = -0.5, y1 =  0.1, col = 'red')
segments(x0 = log(csgo_lower), y0 = -2.2, y1 = -1.8, col = 'blue')
segments(x0 = log(csgo_upper), y0 = -2.2, y1 = -1.8, col = 'blue')
abline(v = 0, col = 'gray')
abline(h = c(0, -2), col = 'gray')


