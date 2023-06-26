### R code from vignette source '/Users/rhankin/rstudio/hyper2/inst/jss_hyper3.Rnw'

###################################################
### code chunk number 1: setup
###################################################
suppressMessages(library("knitr",quietly=TRUE,verbose=FALSE))
suppressMessages(library("hyper2",quietly=TRUE,verbose=FALSE))
opts_chunk$set(cache=TRUE, autodep=TRUE)


###################################################
### code chunk number 2: jss_hyper3.Rnw:220-224
###################################################
LL <- hyper3()
LL[c(a = 1)] <- 1
LL[c(a = 3, b = 2, c = 1)] <- -1
LL


###################################################
### code chunk number 3: jss_hyper3.Rnw:234-236
###################################################
loglik(c(a = 0.01, b = 0.01, c = 0.98), LL)
loglik(c(a = 0.90, b = 0.05, c = 0.05), LL)


###################################################
### code chunk number 4: jss_hyper3.Rnw:251-252
###################################################
(H <- ordervec2supp3(c("a", "c", "b", "a", "a", "b")))


###################################################
### code chunk number 5: maxexamp
###################################################
(mH <- maxp(H))


###################################################
### code chunk number 6: testequality
###################################################
equalp.test(H)


###################################################
### code chunk number 7: maxpaba
###################################################
maxp(ordervec2supp3(c("a", "b", "a")))


###################################################
### code chunk number 8: jss_hyper3.Rnw:346-348
###################################################
a <- 1/2 # null
(S_delta <- log(a * (1 - a)/(1 + a)) - log(3 - 2 * sqrt(2)))


###################################################
### code chunk number 9: jss_hyper3.Rnw:358-359
###################################################
pchisq(-2 * S_delta, df = 1, lower.tail = FALSE)


###################################################
### code chunk number 10: figaba
###################################################
a <- seq(from = 0, by = 0.005, to = 1)
S <- function(a){log(a * (1 - a) / ((1 + a) * (3 - 2 * sqrt(2))))}
plot(a, S(a), type = 'b',xlab=expression(p[a]),ylab="support")
abline(h = c(0, -2))
abline(v = c(0.02438102, 0.9524271), col = 'red')
abline(v = sqrt(2) - 1)


###################################################
### code chunk number 11: figabbabb
###################################################
f_aab <- function(a){a^2 / (1 + a)}
f_aba <- function(a){a * (1 - a) / (1 + a)^2}
f_baa <- function(a){(1 - a) / (1 + a)}
p <- function(f, ...){
  a <- seq(from = 0, by = 0.005, to = 1)
  points(a, f(a) / max(f(a)), ...)
  }
plot(0:1, 0:1, xlab = expression(p[a]), ylab = "Likelihood", type = "n")
p(f_aab, type = "l", col = "black")
p(f_aba, type = "l", col = "red")
p(f_baa, type = "l", col = "blue")
text(0.8,0.8,"AAB")
text(0.8,0.5,"ABA",col="red")
text(0.8,0.15,"BAA",col="blue")
abline(h = exp(-2), lty = 2)


###################################################
### code chunk number 12: jss_hyper3.Rnw:453-454
###################################################
maxp(ordervec2supp3(c("a", "b"), nonfinishers=c("a", "b")))


###################################################
### code chunk number 13: define_xy_wilcox
###################################################
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)


###################################################
### code chunk number 14: hyper3osdef
###################################################
names(x) <- rep("x", length(x))
names(y) <- rep("y", length(y))
(os <- names(sort(c(x, y))))


###################################################
### code chunk number 15: hyper3xytest
###################################################
Hxy <- ordervec2supp3(os)
equalp.test(Hxy)


###################################################
### code chunk number 16: plotwilcoxlike
###################################################
a <- seq(from = 0.02, to = 0.8, len = 40)
L <- sapply(a, function(p){loglik(p, Hxy)})
plot(a, L - max(L), type = 'b',xlab=expression(p[a]),ylab="likelihood")
abline(h = c(0, -2))
abline(v = c(0.24))
abline(v=c(0.5), lty=2)


###################################################
### code chunk number 17: javelintable
###################################################
javelin_table


###################################################
### code chunk number 18: converttosupp3
###################################################
javelin_vector <- attemptstable2supp3(javelin_table,
       decreasing = TRUE, give.supp = FALSE)
options(width = 60)
javelin_vector


###################################################
### code chunk number 19: dothething2
###################################################
javelin <- ordervec2supp3(v = names(javelin_vector)[!is.na(javelin_vector)])


###################################################
### code chunk number 20: setdigits
###################################################
options(digits = 3)


###################################################
### code chunk number 21: testthejav
###################################################
(mj <- maxp(javelin))
dotchart(mj, pch = 16,xlab="Estimated Bradley-Terry strength")


###################################################
### code chunk number 22: havealook
###################################################
f <- function(s){
  jj <- specificp.test(javelin, "Vesely", s, n = 2)
  p <- jj$null_estimate
 return(c(log(p[6] / p[7]), jj$null_support))
}
Ves <- seq(from = 0.0199, to = 0.33, len = 16)
M <- sapply(Ves, f)
M[2,] <- M[2,] - max(M[2,])
rownames(M) <- c("logcontrast", "support")


###################################################
### code chunk number 23: plottheloglikcont
###################################################
colnames(M) <-  as.character(Ves)
plot(t(M), type = "b")
abline(h = c(0, -2))
abline(v = 0, lty = 2)
abline(v = log(0.32062833 / 0.11402735)) # these from mp


###################################################
### code chunk number 24: showconstructortable
###################################################
constructor_2021_table[, 1:9]


###################################################
### code chunk number 25: maxpconstructor2021
###################################################
const2020 <- ordertable2supp3(constructor_2020_table)
const2021 <- ordertable2supp3(constructor_2021_table)
options(digits = 4)
maxp(const2020, n = 1)
maxp(const2021, n = 1)


###################################################
### code chunk number 26: definecombinedlikelihoodfunction
###################################################
H <- (
      psubs(constructor_2020, "Merc", "Merc2020") +
      psubs(constructor_2021, "Merc", "Merc2021")
     )


###################################################
### code chunk number 27: usesamep
###################################################
options(digits = 4)
samep.test(H, c("Merc2020", "Merc2021"))


