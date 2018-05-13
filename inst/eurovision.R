## This file creates hyper2 object 'eurovision2009'.

## The dataset is copied from "Eurovision Song Contest 2009," Wikipedia,
## accessed May 13, 2018.


 library("hyper2")
## might be needed.


abbreviated <- TRUE
## change to FALSE to use full country names rather than two-letter abbreviations.


## First specify the matrix as appearing in the Wikipedia page:
wiki_matrix <- matrix(c(
    NA, 00, 00, 03, 00, 05, 01, 02, 05, 01, 00, 00, 08, 00, 00, 01, 06, 10, 02, 00,
    00, NA, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    00, 00, NA, 00, 00, 01, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00,
    02, 01, 00, NA, 01, 04, 00, 00, 00, 04, 01, 01, 06, 00, 04, 00, 01, 00, 00, 00,
    00, 06, 04, 07, NA, 08, 07, 04, 04, 07, 00, 10, 03, 04, 10, 08, 08, 04, 04, 07,
    04, 12, 10, 10, 05, NA, 00, 01, 10, 10, 08, 02, 02, 08, 01, 00, 00, 01, 10, 05,
    00, 00, 00, 00, 00, 00, NA, 00, 01, 00, 00, 00, 00, 00, 00, 04, 03, 00, 00, 00,
    00, 00, 00, 02, 02, 00, 02, NA, 00, 00, 00, 00, 00, 00, 05, 02, 00, 02, 00, 00,
    08, 05, 12, 06, 07, 10, 05, 12, NA, 06, 12, 07, 12, 12, 07, 05, 10, 12, 12, 12,
    05, 04, 03, 04, 06, 07, 08, 05, 03, NA, 04, 06, 01, 03, 06, 00, 04, 00, 05, 01,
    00, 00, 00, 00, 00, 00, 00, 00, 02, 00, NA, 00, 05, 00, 00, 00, 00, 00, 00, 00,
    07, 10, 07, 12, 12, 12, 10, 07, 08, 12, 06, NA, 04, 10, 12, 12, 12, 07, 06, 08,
    10, 03, 00, 00, 00, 00, 00, 06, 06, 00, 10, 00, NA, 02, 00, 00, 00, 08, 00, 00,
    06, 00, 02, 01, 00, 02, 04, 00, 07, 08, 05, 04, 07, NA, 00, 10, 02, 06, 01, 02,
    03, 00, 01, 00, 10, 00, 03, 00, 00, 00, 00, 12, 00, 01, NA, 03, 05, 00, 00, 04,
    00, 02, 06, 00, 03, 00, 12, 10, 00, 02, 02, 08, 00, 07, 02, NA, 00, 03, 07, 06,
    01, 07, 08, 08, 04, 03, 06, 03, 00, 05, 03, 05, 00, 06, 03, 06, NA, 05, 03, 10,
    12, 08, 05, 05, 08, 06, 00, 08, 12, 03, 07, 03, 10, 05, 08, 07, 07, NA, 08, 03)
, nrow=18,byrow=TRUE)

points <- c(12,10,8,7,6,5,4,3,2,1)  # The number of points awarded to
                                    # voters' first, econd, third, etc
                                    # choice.  The numerical values
                                    # themselves do not affect the
                                    # likelihood function; only the
                                    # order of the voters' preferences
                                    # matters.

preference <- wiki_matrix*0  # matrix 'preference' records voters'
                             # first, second, third, etc choice.

for(i in seq_along(points)){
    preference[wiki_matrix == points[i]] <- i
}


countries <- data.frame(
    fullname =
        c("Montenegro", "Czech rep", "Belgium", "Belarus", "Sweden",
          "Armenia", "Andorra", "Switzerland", "Turkey", "Israel",
          "Bulgaria", "Iceland", "Macedonia", "Romania", "Finland",
          "Portugal", "Malta", "Bosnia Herz", "Germany", "UK"),
    abbreviation = c("ME","CZ","BE", "BY", "SW", "AM", "AD", "CH",
                     "TR", "IL", "BG", "IS", "MK", "RO", "FI", "PT",
                     "MT", "BA", "DE", "UK")
)

if(abbreviated){ 
    jj <- countries$abbreviation
} else { 
    jj <- countries$fullname
}

competitors <- jj[1:18]
colnames(preference) <- jj      # voters; 20 countries (18 + DE + UK)

rownames(preference) <- competitors  # The competitors were the first
                                     # 18 countries (the last two
                                     # countries, Germany and the UK,
                                     # voted but did not compete)



preference <- t(preference)   # we need the voters' choices to be the *rows*


## Now, take the first row of 'preference'.  This represents the votes
## cast BY (sic) Montenegro ("ME").  Their favourite was [last column]
## Bosnia & Herzegovina who they gave rank 1 to.  Their second
## favourite was Macedonia, their third was Turkey, and so on.  They
## were not allowed to vote for themselves, which is why the first
## column is NA.  So the order was: bh, mc, tu,ic,ro,is, ar,fi,br,ma

## define an empty hyper2 object:
H <- hyper2(d=18)

for(i in seq_len(nrow(preference))){   # cycle through the rows; each row is a voter
    d <- preference[i,,drop=TRUE]
    d[is.na(d)] <- -1  # kludge: make the voting country ineligible to vote.
    while(any(d>0)){
        eligible <- which(d>=0)   # this is why we set NA values to -1
        H[which(d==1)] <- H[which(d==1)] +1  # The first choice among
                                             # eligible players has +1
                                             # power on the numerator
        
        H[eligible] <- H[eligible] - 1  # denominator of all eligible
                                        # players; power -1

        d[d==1] <- -1  # once you've won, you are ineligible to be chosen again

        d[d>0] <- d[d>0]-1  # everyone moves down the list, so who
                            # *was* second choice becomes first
                            # choice, who *was* third choice becomes
                            # second, and so on.

    } # while() loop closes
} # i loop closes


## syntatic sugar:
pnames(H) <- competitors
