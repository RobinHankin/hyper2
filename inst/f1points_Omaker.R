## This file creates 1000 (variable "howmany") simulated Formula 1
## results and compares the simulated ranks with the actual
## points-based ranks according to different points systems such as
## the real one or Zipf's law.  The R commands here are copied from
## f1points.Rmd.  Some of the lines are redundant.

## The simulations take quite a long time to run and at the bottom the
## results are saved to file "~/rstudio/hyper2/inst/formula1_results_2017.rda"

## The "point" of this file is that you can open it in emacs and
## globally replace "2017" with "2018" and everything still works
## (note that this would save the results to file
## "~/rstudio/hyper2/inst/formula1_results_2017.rda": the year in the
## filename will change too.

howmany <- 1000
top <- 11
points_inaugural <- c(8,6,4,3,2)

library(hyper2,quietly=TRUE)
H <- ordertable2supp(F1_table_2017[seq_len(top),])



p <- c(25, 18, 15, 12, 10, 8, 6, 4, 2, 1, 0, 0)
points_real <- p

points_inaugural <- c(8,6,4,3,2)


f2017 <- read.table("formula1_2017.txt",header=TRUE)[seq_len(top),1:20]
m <- maxp(ordertable2supp(as.ordertable(f2017)))


resampling_multiple <- function(m,f2017,pointslist){
    random_table <- rrank(n=ncol(f2017), p=m)
    rownames(random_table) <- colnames(f2017)
    ## "random_table" is a random table; now calculate lstar and pstar for
    ## likelihood ranks and points ranks respectively:

    l_star <- maxp(ordertable2supp(ranktable_to_ordertable(random_table)))
    l_star[] <- seq_along(l_star)
    ## l_star is the likelihood order statistic.

	goodnesses <- seq_along(pointslist)
	for(i in seq_along(pointslist)){
		p_star <- ordertable2points(as.ordertable(ranktable_to_ordertable(random_table)),pointslist[[i]])
		p_star <- sort(p_star,decreasing=TRUE)
		p_star[] <- seq_along(p_star)
		goodness <- sum(cumprod(names(p_star)==names(l_star)))
		goodnesses[i] <- goodness
		}
	return(goodnesses)
}
pointslist <- list(
	real = points_real,
	inau = points_inaugural,
	top1 = rep(1,1),
	top2 = rep(1,2),
	top3 = rep(1,3),
	top4 = rep(1,4),
	top5 = rep(1,5),
	top6 = rep(1,6),
	top7 = rep(1,7),
	top8 = rep(1,8),
	top9 = rep(1,9),
	topt = rep(1,10),  # effectively punishing the last finisher
   	lin1 = 1,   # same as top1
	lin2 = 2:1,
	lin3 = 3:1,
	lin4 = 4:1,
	lin5 = 5:1,
	lin6 = 6:1,
	lin7 = 7:1,
	lin8 = 8:1,
	lin9 = 9:1,
	lint = 10:1,
	line = 11:1,
	zipf = 1/seq_len(top),
	expa = 1/1.01^seq_len(top),
	expb = 1/1.10^seq_len(top),
	expc = 1/1.20^seq_len(top),
	expd = 1/1.50^seq_len(top),
	exp2 = 1/2^seq_len(top),
	exp3 = 1/3^seq_len(top),
	exp4 = 1/4^seq_len(top),
	fis1 = c(1,1,1),
	fis2 = c(1,1,0.9),
	fis3 = c(1,1,1,0.1),
	fis4 = c(1,1,0.9,0.1),
	fis5 = c(1,1,0.5),
	fis6 = c(1,1,1,0.5),
	fis7 = c(1,1,0.5,0.5)
)



set.seed(9)
f2017 <- read.table("formula1_2017.txt",header=TRUE)[seq_len(top),]
f2017 <- f2017[,-ncol(f2017)]
m <- maxp(ordertable2supp(as.ordertable(f2017)))
OO <- replicate(howmany,resampling_multiple(m,f2017,pointslist=pointslist))
rownames(OO) <- names(pointslist)


summarytable <- data.frame(
means = apply(OO,1,mean),
winner_correct = rowSums(OO>1),
all_correct = rowSums(OO==top))
rownames(summarytable) <- names(pointslist)

table(real_winner_ok=OO[1,]>0,top3_winner_ok=OO[3,]>0)
save(OO,pointslist,file="~/rstudio/hyper2/inst/formula1_results_2017.rda")
