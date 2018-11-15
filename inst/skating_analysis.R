## This script creates hyper2 object 'skating' which is a likelihood
## function for the strengths of the competitors in the Ladies' Figure
## Skating at the 2002 Winter Olympics.  File 'skating.txt' is copied
## from Lock and Lock (see skating.Rd for more details).

##  Note that file is structured so that each competitor is a row, and
##  each judge is a column.  Function \code{order_likelihood()}
##  considers each row to be a race [or a judge]

library("hyper2")


## The following R idiom is plausible but incorrect:


a <- as.matrix(read.table("skating.txt"))
skating_incorrect <- order_likelihood(t(a))

## the above is incorrect because we want the order, and we have the
## ranks.   Correct analysis follows.

b <- apply(a,2,order)


## Column 1 of 'a' and column 1 of 'b' are confusingly simular. So
## consider column 2, corresponding to judge number 2. If aa=a[,2] and
## bb=b[,2] we have aa=c(4,1,3,2, ...) and bb=c(2,4,3,1, ...).  Thus
## aa means that hughes was judged to be fourth best, slutskaya was
## judged to be the best, kwan third best, cohen second best, and so
## on.  Conversely, bb means that the best competitor was number two
## [slutskaya], the second best number was number four [cohen], the
## third best was number three [kwan], the fourth best number 1
## [hughes], and so on.  Note that this means the rows of b cannot be
## named.

## The R idiom requires the transpose of the matrix because function
## order_likelihood() treats the rows as independent observations
## [here order statistics].

skating <- order_likelihood(t(b))
pnames(skating) <- rownames(a)



## Now some data visualization.  First the MLE for the strengths:
dotchart(maxp(skating))


## Now compare my rating with the official point-tallying method:
dev.new()
par(pty='s')
lik <- order(maxp(skating),decreasing=TRUE)
plot(1:23,lik,asp=1,pch=16,xlab='official order',ylab='likelihood order')
text(1:23,lik,asp=1,pch=16,pos=c(rep(4,19),rep(2,4)),rownames(a))
abline(0,1)



