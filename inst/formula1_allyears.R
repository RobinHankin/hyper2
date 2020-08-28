## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
library("hyper2")


## ----label=f1yearsseparate,cache=TRUE-----------------------------------------
f <- function(M){M[,-ncol(M)]} # removes 'points' column
o <- function(k){ordertable2supp(f(read.table(k)))}
years <- 2001:2003
F1_list <- list()
for(year in years){
	filename <- paste("formula1_",year,".txt", sep="")
	jj <- paste("y",year,sep="")
	F1_list[[jj]] <- o(filename)
}


## ----label=combineallyears,cache=TRUE-----------------------------------------
F1_total <- hyper2()
for(i in F1_list){F1_total <- F1_total + i}


## ----calculatemaxlikelhood,cache=TRUE-----------------------------------------
m <- maxp(F1_total)
pie(m)
dotchart(m,pch=16,main='Formula 1, 2012-9')


## ----workwithM----------------------------------------------------------------
M <- matrix(NA,length(pnames(F1_total)),2019-2010+1)
rownames(M) <- pnames(F1_total)
colnames(M) <- paste("y",10:19,sep="")

for(i in seq_len(nrow(M))){
    for(j in seq_len(ncol(M))){
        if(rownames(M)[i] %in% pnames(F1_list[[j]])){M[i,j] <- 1}
    }
}
jj <- order(rowSums(M,na.rm=T),decreasing=T)
M <- M[jj,]

image(x=2010:2019,y=seq_along(pnames(F1_total)),t(M))


## -----------------------------------------------------------------------------
howmanyraces <- sort(rowSums(M,na.rm=TRUE),decreasing=TRUE)
plot(howmanyraces)


## -----------------------------------------------------------------------------
select <- function(F1tab,minraces=5){
	wanted <- rownames(M)[howmanyraces >= minraces]
	ordertable2supp(F1tab[rownames(F1tab) %in% wanted,])
}


## ----definefreqrace-----------------------------------------------------------
freqracers <- function(minraces){
(
	select(f(read.table("formula1_2010.txt")),minraces) +
	select(f(read.table("formula1_2011.txt")),minraces) +
	select(f(read.table("formula1_2012.txt")),minraces) +
	select(f(read.table("formula1_2013.txt")),minraces) +
	select(f(read.table("formula1_2014.txt")),minraces) +
	select(f(read.table("formula1_2015.txt")),minraces) +
	select(f(read.table("formula1_2016.txt")),minraces) +
	select(f(read.table("formula1_2017.txt")),minraces) +
	select(f(read.table("formula1_2018.txt")),minraces)
	)
}

comp <- function(m,mr){
	par(pty="s")
#	plot(m,mr,asp=1,xlim=c(0,0.2),ylim=c(0,0.2))
	plot(m,mr,asp=1)
	abline(0,1)
}


## ----freqtop1,cache=TRUE------------------------------------------------------
F1_top1 <- freqracers(1)
m1 <- maxp(F1_top1)
pnames(F1_top1) <- rev(pnames(F1_top1))
m1r <- rev(maxp(F1_top1)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m1)
comp(m1,m1r)


## ----freqtop2,cache=TRUE------------------------------------------------------
F1_top2 <- freqracers(2)
m2 <- maxp(F1_top2)
pnames(F1_top2) <- rev(pnames(F1_top2))
m2r <- rev(maxp(F1_top2)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m2)
comp(m2,m2r)


## ----freqtop3,cache=TRUE------------------------------------------------------
F1_top3 <- freqracers(3)
m3 <- maxp(F1_top3)
pnames(F1_top3) <- rev(pnames(F1_top3))
m3r <- rev(maxp(F1_top3)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m3)
comp(m3,m3r)


## ----freqtop4,cache=TRUE------------------------------------------------------
F1_top4 <- freqracers(4)
m4 <- maxp(F1_top4)
pnames(F1_top4) <- rev(pnames(F1_top4))
m4r <- rev(maxp(F1_top4)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m4)
comp(m4,m4r)


## ----freqtop5,cache=TRUE------------------------------------------------------
F1_top5 <- freqracers(5)
m5 <- maxp(F1_top5)
pnames(F1_top5) <- rev(pnames(F1_top5))
m5r <- rev(maxp(F1_top5)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m5)
comp(m5,m5r)


## ----freqtop6,cache=TRUE------------------------------------------------------
F1_top6 <- freqracers(6)
m6 <- maxp(F1_top6)
pnames(F1_top6) <- rev(pnames(F1_top6))
m6r <- rev(maxp(F1_top6)) # reverse of MLE with names reversed


## -----------------------------------------------------------------------------
pie(m6)
comp(m6,m6r)

