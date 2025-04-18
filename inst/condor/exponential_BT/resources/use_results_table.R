#!/usr/bin/env Rscript

library("hyper2")
howmany <- 5000


args = commandArgs(trailingOnly=TRUE)
argument_from_condor <- as.numeric(args[1]) + 1

wanted_line <- argument_from_condor
results_table <- read.table("imo_nz_results.txt", header=TRUE)

a_try <- seq(from = 0.2, to = 0.7, len = 71)
x_try <- seq(from = 0.8, to = 1.0, len = 97)

v <- as.matrix(expand.grid(a_try, x_try))[wanted_line,]

str_pad <- function(x, width=6){sprintf(paste0("%0", width, "d"), x)}

filename <- paste(
    "/home/rha/condor/exponential_BT/answerfiles/ans",
    str_pad(argument_from_condor,  width = 6),
    ".txt",
    sep="")

BT <- function(n, x){
    out <- x^(0:(n-1))
    if(x != 1){
        out <- out*(1-x)/(1-x^n)
    } else {
        out <- out/n
    }
    names(out) <- paste0("p",str_pad(1:n, 3))
    out
}

like <- function(n, a_obs, v, N=1e4){
               
 		                       # n competitors in the race
				       # a_obs is the observed (normalized) rank of
                                       # the focal competitor
 				       # v=c(a_try, x_try),  the parameters
				       # N is the number of trials
                                       # notionally, r=3
                                       # means "third place"

    a_suggested <- v[1]
    x_suggested <- v[2]

    which_suggested <- ceiling(n * a_suggested) 
    rank_observed <- ceiling(n * a_obs) 

    name_suggested <-  # this is the name of the suggested focal competitor
        paste0("p", str_pad(which_suggested, 3))             # focal competitor
                                                             # (something like "p45")
    out <- 0
    for(i in 1:N){
        jj <- rrace(BT(n, x_suggested))
        if(jj[rank_observed] == name_suggested){
            out <- out + 1}
        }
    return(out)
}

out <- c()
for(i in seq_len(nrow(results_table))){
    this_place <- results_table[i,1]
    no_of_comp <- results_table[i,2]
    out <- c(out, like(n = no_of_comp , a = this_place/no_of_comp, v , N = howmany))
    }

out <- c(v,out)

print(out)  # these go to outfiles/out$(Process).txt 
write(out, file=filename, ncolumns = length(out), append=FALSE)
