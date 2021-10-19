\name{ordertable}
\alias{ordertable}
\alias{order_table}
\title{Order tables}
\description{Order tables}
\details{

The package makes extensive use of order tables and these are discussed
here together with a list of order tables available in the package as
data.  See also \code{ranktable.Rd}.

Consider \code{pentathlon_ordertable}:

\preformatted{
 > pentathlon_table
              shooting fencing swimming riding running
Moiseev              5       1        1      6       5
Zadneprovskis        6       2        5      5       1
Capalini             4       6        2      3       4
Cerkovskis           3       3        7      7       2
Meliakh              1       7        4      1       6
Michalik             2       4        6      2       7
Walther              7       5        3      4       3
}

Although \code{pentathlon_table} is a dataset in the package, the source
dataset is also included in the \code{inst/} directory as file
\code{pentathlon.txt}; use idiom like
\code{read.table("inst/pentathlon.txt")} to load the order table.

Object \code{pentathlon_table} is a representative example of an
ordertable.  Each row is a competitor, each column an event (venue,
judge, \ldots).  The first row shows Moiseev's ranking in shooting
(5th), fencing (1st), and so on.  The first column shows the ranks of
the competitors in shooting.  Thus Moiseev came fifth, Zadneprovskis
came 6th, and so on.

However, to create a likelihood function we need ranks, not orders.  We
need to know, for a given event, who came first, who came second, and so
on (an extended discussion on the difference between rank and order is
given at \code{rrank.Rd}).  We can convert from an order table to a rank
table using \code{ordertable_to_ranktable()} (see also
\code{ranktable.Rd}):

\preformatted{
> ordertable_to_ranktable(pentathlon_table)
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

Above, we see the same data in a different format (an extended
discussion on the difference between rank and order is given in
\code{rrank.Rd}).

Many of the order tables in the package include entries that correspond
to some variation on \dQuote{did not finish}.  Consider the \code{volvo}
dataset:

\preformatted{
> volvo_table_2014
           leg1 leg2 leg3 leg4 leg5 leg6 leg7 leg8 leg9
AbuDhabi      1    3    2    2    1    2    5    3    5
Brunel        3    1    5    5    4    3    1    5    2
Dongfeng      2    2    1    3  DNF    1    4    7    4
MAPFRE        7    4    4    1    2    4    2    4    3
Alvimedica    5    5    3    4    3    5    3    6    1
SCA           6    6    6    6    5    6    6    1    7
Vestas        4  DNF  DNS  DNS  DNS  DNS  DNS    2    6
}

In the above order table, we have \code{DNF} for \dQuote{did not finish}
and \code{DNS} for \dQuote{did not start}.  The \code{formula1} order
table has other similar entries such as \code{DSQ} for
\dQuote{disqualified} and a discussion is given at
\code{ordertable2supp.Rd}.

Links are given below to all the order tables in the package.  Note that
the table in \code{inst/eurovision.Rmd} (\code{wiki_matrix}) is not an
order table because no country is allowed to vote for itself.

To coerce a table like the Volvo dataset shown above into an order table
[that is, replace \code{DNS} with zeros, and also force nonzero entries
to be contiguous], use \code{as.ordertable()}.

}
\author{Robin K. S Hankin}
\seealso{
  \code{\link{ordertable2supp}},\code{\link{rrank}},
  \code{\link{ranktable}},\code{\link{as.ordertable}}
}
\examples{

ordertable_to_ranktable(soling_table)
ordertable2supp(soling_table) == soling  # should be TRUE


}