## Files like "f2012.R" are generated from file f1points_Omaker.R
## using sed as discussed below.

## This file creates 1000 (variable "howmany") simulated Formula 1
## results and compares the simulated ranks with the actual
## points-based ranks according to different points systems such as
## the real one or Zipf's law.  The R commands here are copied from
## f1points.Rmd.  Some of the lines are redundant.

## The simulations take quite a long time to run and at the bottom the
## results are saved to file "~/rstudio/hyper2/inst/formula1_results_2017.rda"
## and this file is needed by 'f1points.Rmd'.

## The "point" of this file is that you can 
## globally replace "2017" with "2018" and the R commands still work
## (note that this would save the results to file
## "~/rstudio/hyper2/inst/formula1_results_2018.rda": the year in the
## filename will change too).  The following shellscript:

ignore <- "
 cat f1points_Omaker.R | sed -e 's/2017/2012/g' > f2012.R
 cat f1points_Omaker.R | sed -e 's/2017/2013/g' > f2013.R
 cat f1points_Omaker.R | sed -e 's/2017/2014/g' > f2014.R
 cat f1points_Omaker.R | sed -e 's/2017/2014/g' > f2015.R
 cat f1points_Omaker.R | sed -e 's/2017/2015/g' > f2015.R
 cat f1points_Omaker.R | sed -e 's/2017/2016/g' > f2016.R
 cat f1points_Omaker.R | sed -e 's/2017/2017/g' > f2017.R
 cat f1points_Omaker.R | sed -e 's/2017/2018/g' > f2018.R
 cat f1points_Omaker.R | sed -e 's/2017/2019/g' > f2019.R

 R CMD BATCH f2012.R &
 R CMD BATCH f2013.R &
 R CMD BATCH f2014.R &
 R CMD BATCH f2015.R &
 R CMD BATCH f2016.R &
 R CMD BATCH f2017.R &
 R CMD BATCH f2018.R &
 R CMD BATCH f2019.R &
"

# creates all the different .rda files needed by f1points.Rmd.




howmany <- 1000  # howmany=1000 takes about 15 minutes to run
top <- 11
points_inaugural <- c(8,6,4,3,2)

library(hyper2,quietly=TRUE)

pointslist <- formula1_points_systems(top)

f2017 <- read.table("formula1_2017.txt",header=TRUE)
f2017 <- f2017[seq_len(top),seq_len(ncol(f2017)-1)]
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



set.seed(10)
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
