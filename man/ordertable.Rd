\name{ordertable}
\alias{ordertable}
\alias{order_table}
\title{Order tables}
\description{Order tables}
\details{

The package makes extensive use of order tables and these are discussed
here together with a list of order tables available in the package as
data.  Consider the \code{pentathlon_ordertable}:

\preformatted{
 > pentathlon_ordertable
              shooting fencing swimming riding running
Moiseev              5       1        1      6       5
Zadneprovskis        6       2        5      5       1
Capalini             4       6        2      3       4
Cerkovskis           3       3        7      7       2
Meliakh              1       7        4      1       6
Michalik             2       4        6      2       7
Walther              7       5        3      4       3
}

This is a very simple order table.  Each row is a competitor, each
column an event (venue, judge,\ldots).  The first row shows Moiseev's
ranking in shooting (5th), fencing (1st), and so on.  The first column
shows the ranks of the competitors in shooting .  Thus Moisieev came
fifth, Zadnesprovskis cames 6th, and so on.

\preformatted{
> ordertable_to_ranktable(pentathlon_ordertable)
         c1            c2            c3         c4       c5           
shooting Meliakh       Michalik      Cerkovskis Capalini Moiseev      
fencing  Moiseev       Zadneprovskis Cerkovskis Michalik Walther      
swimming Moiseev       Capalini      Walther    Meliakh  Zadneprovskis
riding   Meliakh       Michalik      Capalini   Walther  Zadneprovskis
running  Zadneprovskis Cerkovskis    Walther    Capalini Moiseev      
         c6            c7        
shooting Zadneprovskis Walther   
fencing  Capalini      Meliakh   
swimming Michalik      Cerkovskis
riding   Moiseev       Cerkovskis
running  Meliakh       Michalik  
}

We see the same data in a different format (a discussion is given in
\code{rrank.Rd}).  The reason for this system is that we can generate
synthetic datasets using estimated strengths:

\preformatted{
> jj <- pentathlon_ordertable
> p <- maxp(ordertable2supp(jj))
> rrank(n=5,p=p,rnames=colnames(jj))
         c1            c2       c3            c4            c5      
shooting Michalik      Meliakh  Capalini      Zadneprovskis Moiseev 
fencing  Zadneprovskis Moiseev  Capalini      Michalik      Meliakh 
swimming Zadneprovskis Moiseev  Meliakh       Michalik      Capalini
riding   Walther       Moiseev  Zadneprovskis Capalini      Michalik
running  Meliakh       Capalini Moiseev       Cerkovskis    Walther 
         c6            c7        
shooting Walther       Cerkovskis
fencing  Walther       Cerkovskis
swimming Walther       Cerkovskis
riding   Meliakh       Cerkovskis
running  Zadneprovskis Michalik  
}

Thus we can get a sense of the uncertainties in the estimates using a
bootstrap technique:

\preformatted{
> maxp(ordertable2supp(ranktable_to_ordertable(rrank(n=5,p=p,rnames=colnames(jj)))))
      Moiseev Zadneprovskis      Capalini    Cerkovskis       Meliakh      Michalik       Walther 
   0.19790755    0.18643631    0.16518800    0.15563143    0.14226130    0.13168772    0.02088768 
> maxp(ordertable2supp(ranktable_to_ordertable(rrank(n=5,p=p,rnames=colnames(jj)))))
      Moiseev Zadneprovskis      Capalini    Cerkovskis       Meliakh      Michalik       Walther 
  0.017793227   0.073832862   0.030459669   0.008706624   0.039666078   0.795045823   0.034495718 
> maxp(ordertable2supp(ranktable_to_ordertable(rrank(n=5,p=p,rnames=colnames(jj)))))
      Moiseev Zadneprovskis      Capalini    Cerkovskis       Meliakh      Michalik       Walther 
   0.09478758    0.03804362    0.08552271    0.02340856    0.65100588    0.08401648    0.02321516 
}


  
}
\author{Robin K. S Hankin}
\seealso{\code{\link{ordertable2supp}}}
\examples{
1+4
}