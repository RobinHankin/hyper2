library("hyper2")
years <- 1995:2015
f <- function(M){M[,-ncol(M)]} # removes 'points' column
o <- function(k){ordertable2supp(f(read.table(k)))}
filenamemaker <- function(year){paste("formula1_",year,".txt", sep="")}
F1_list <- list()
for(year in years){
	filename <- filenamemaker(year)
	jj <- paste("y",year,sep="")
	F1_list[[jj]] <- o(filename)
}
F1_total <- hyper2()
for(i in F1_list){F1_total <- F1_total + i}
