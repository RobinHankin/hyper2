library(hyper2)
library(magrittr)

## use-case:

## R> F1_likelihood(wiki_table=read.table("formula1_2017.txt",header=TRUE))

## File 'formula1_2017.txt' is directly copied from Wikipedia (with
## slight whitespace changes)


`F1_likelihood` <- function(wiki_table, noscore=c("Ret","WD","DNS","DSQ", "DNP")){

  ## columns of wiki_table are assumed to be: driver, venue_1,
  ## venue_2, ..., venue_n, points

  first_and_last <- c(1,ncol(wiki_table))
  
  racers <- wiki_table[,1]
  venues <- colnames(wiki_table)[-first_and_last]

  

  ## Now create a numeric matrix, fmat.  Two steps.  First step, strip
  ## out no-score entries;  we need to count any of noscore
  ## [Ret=retired, WD=withdrawn, DNS=did not start, etc] as a zero:
  
  f <- function(x){
    if(any(x %in% noscore)){x[x%in%noscore] <- 0}
    return(x)
  }

  jj <- apply(wiki_table,2,f)
  

  ## Second step: convert to numeric and strip out names; transpose of
  ## wiki_table (because we want each row to be a venue):
  fmat <- matrix(as.numeric(jj[,-first_and_last]),byrow=TRUE,ncol=nrow(wiki_table)) 
  colnames(fmat) <- racers
  rownames(fmat) <- venues


  ## Considering Formula1, 2017 as an example: taking the first row of
  ## fmat is AUS (Australia), in which Hamilton camge second, Vettel
  ## came first, etc.  The first column of fmat is Hamilton's
  ## results. He came second in AUS, first in CHN, second in BHR, etc.


  ## Following is similar to, but slightly different from, the analysis in eurovision.R:
  ## Define an empty hyper2 object:

  F1 <- hyper2(d=ncol(fmat))

  for(i in seq_len(nrow(fmat))){   # cycle through the rows; each row is a venue [voter]
    d <- fmat[i,,drop=TRUE]
    while(any(d>0)){
      eligible <- which(d>=0)  
      
      ## The first choice among eligible players has +1 power on the
      ## numerator:
      F1[which(d==1)] %<>% "+"(1)

      ## denominator of all eligible players; power -1
      F1[eligible] %<>% "-"(1)

      ## once you've come first in the field, you are ineligible to be first again:
      d[d==1] <- -1  
      
      ## everyone moves down the list, so who *was* in second place
      ## becomes first place, who *was* third place becomes second,
      ## and so on:
      d[d>0] %<>% "-"(1)

    } # while() loop closes
  } # i loop closes
  

  ## syntatic sugar:
  pnames(F1) <- racers

  return(F1)
}  # function F1_likelihood() closes





wiki_table <- read.table("formula1_2016.txt",header=TRUE)

points <- wiki_table$points
names(points) <- wiki_table$driver
F1 <- F1_likelihood(wiki_table)

m <- maxp(F1)
pdf(file="f.pdf")
dotchart(m,pch=16)
dev.off()

pdf(file="g.pdf")

ox <- order(points,decreasing=TRUE)
oy <- order(m,decreasing=TRUE)
par(pty='s') # square plot
plot(ox,oy,asp=1,pty='s',xlim=c(0,25),ylim=c(0,25),pch=16,xlab="official order",ylab="my order")
par(xpd=TRUE) # allow drivers' names to appear outside plotting region
for(i in seq_along(ox)){  text(ox[i],oy[i],names(m)[ox[i]],pos=4,col='gray') }
par(xpd=FALSE) # stop diagonal line from protruding beyond plotting region
abline(0,1)
dev.off()
