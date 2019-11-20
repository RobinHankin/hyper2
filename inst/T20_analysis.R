## Calculates the distribution of Lambda under the null, and
## calculates a p-value for the hypothesis that all teams in the T20
## dataset are of equal strength.  It does some resampling to verify
## that the asymptotic distribution of 2*Lambda is attained in the case
## of T20;


## This takes quite a long time to run, which is why it's not included
## in the package.

library("hyper2")

# Observed value of Lambda:
Lambda_observed <-
    maxp(T20,give=TRUE)$value-loglik(indep(equalp(T20)),T20)

## Function to choose a random team from two teams:
randomteam <- function(team1,team2){
    as.vector(
        cbind(team1,team2)[
            cbind(seq_along(team1),
                  sample(1:2,length(team1),replace=TRUE)
                  )
    ])
}

n <- 1000   # number of in-silico simulations
Lambda <- rep(0,n)
evaluate <- rep(0,n)
equ <- rep(0,n)

for(i in seq_len(n)){

    ## Shuffle the results (but not the games):
    team1 <- as.vector(T20_table$team1)  # not shuffled
    team2 <- as.vector(T20_table$team2)  # not shuffled
    toss_winner <- randomteam(team1,team2)
    toss_decision <- as.vector(sample(T20$toss_decision))
    match_winner <- randomteam(team1,team2)

    # Now create virtual likelihood functions T20star:
    T20star <- hyper2(pnames=c(levels(T20_table$team1)))
    for(j in seq_along(team1)){
        T20star[match_winner[j]] %<>% inc
        T20star[c(team1[j],team2[j])] %<>% dec
    }
    evaluate[i] <- maxp(T20star,give=TRUE)$value
    equ[i] <- loglik(indep(equalp(T20star)),T20star)
    Lambda[i] <- evaluate[i] - equ[i]
    print(i)
}		       


hist(Lambda)
abline(v=Lambda_observed, lwd=7)
cat(paste("resampled pvalue = ", sum(Lambda > Lambda_observed)/length(Lambda)),"\n")
cat(paste("parametric pvalue = ", pchisq(Lambda_observed,df=13,lower.tail=FALSE)),"\n")



## Now a qqplot of 2*Lambda vs random samples from chisq(df=13).
## Wilks's theorem states that the *asymptotic* distribution of
## 2*Lambda is rchisq(df=13)....but is there enough data for the
## asymptotic approximation to be good?

par(pty="s")
plot(NA,xlim=c(0,30),ylim=c(0,30),asp=1)
points(sort(Lambda*2),sort(rchisq(n,df=13)))
abline(0,1)
