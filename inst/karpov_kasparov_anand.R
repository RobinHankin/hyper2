## This file creates likelihood function 'H' which is identical to
## data object 'karpov_kasparov_anand' in the package.

## In this file, the dataset from chessgames is set up first in
## objects like 'karpov_plays_white_beats_kasparov'.

## Then object 'H' is created, and used to test two hypotheses:
## firstly, that all three players have the same strength, and
## secondly that playing white confers no strength.


library("hyper2")
results <- as.list(kka)
attach(results)
H <- hyper2(pnames=c("Karpov","Kasparov","Anand","white","draw"))


## Kasparov vs Karpov
karpov_vs_kasparov <- c("Karpov","Kasparov","white","draw")
H[c("Karpov","white")]   %<>% inc(karpov_plays_white_beats_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_plays_white_beats_kasparov)
H[c("Kasparov","white")] %<>% inc(kasparov_plays_white_beats_karpov)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_white_beats_karpov)
H[c("Karpov")]           %<>% inc(karpov_plays_black_beats_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_plays_black_beats_kasparov)
H[c("Kasparov")]         %<>% inc(kasparov_plays_black_beats_karpov)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_black_beats_karpov)
H[c("draw")]             %<>% inc(karpov_draws_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_draws_kasparov)

## Kasparov vs Anand
kasparov_vs_anand <- c("Kasparov","Anand","white","draw")
H[c("Kasparov","white")] %<>% inc(kasparov_plays_white_beats_anand)
H[kasparov_vs_anand]     %<>% dec(kasparov_plays_white_beats_anand)
H[c("Anand","white")]    %<>% inc(anand_plays_white_beats_kasparov)
H[kasparov_vs_anand]     %<>% dec(anand_plays_white_beats_kasparov)
H[c("Kasparov")]         %<>% inc(kasparov_plays_black_beats_anand)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_black_beats_anand)
H[c("Anand")]            %<>% inc(anand_plays_black_beats_kasparov)
H[kasparov_vs_anand]     %<>% dec(anand_plays_black_beats_kasparov)
H[c("draw")]             %<>% inc(kasparov_draws_anand)
H[kasparov_vs_anand]     %<>% dec(kasparov_draws_anand)
 

karpov_vs_anand <- c("Karpov","Anand","white","draw")

H[c("Karpov","white")] %<>% inc(karpov_plays_white_beats_anand)
H[karpov_vs_anand]     %<>% dec(karpov_plays_white_beats_anand)
H[c("Anand","white")]  %<>% inc(anand_plays_white_beats_karpov)
H[karpov_vs_anand]     %<>% dec(anand_plays_white_beats_karpov)
H[c("Karpov")]         %<>% inc(karpov_plays_black_beats_anand)
H[karpov_vs_anand]     %<>% dec(karpov_plays_black_beats_anand)
H[c("Anand")]          %<>% inc(anand_plays_black_beats_karpov)
H[karpov_vs_anand]     %<>% dec(anand_plays_black_beats_karpov)
H[c("draw")]           %<>% inc(karpov_draws_anand)
H[karpov_vs_anand]     %<>% dec(karpov_draws_anand)

detach(results)

## Test the hypothesis that all three players have the same strength

## First do the free optimization:
max_support_free <- maxp(H,give=TRUE)$value
ml_p_free    <- maxp(H)

## Now the constrained optimization.  We enforce that
## Karpov==Anand==Kasparov==p but allow the white ghost and the draw
## monster to range freely, subject to the unit sum constraint,


objective <- function(x){
  p <- x[1]
  w <- x[2]
  loglik(H,c(p,p,p,w))
}

constrained_optimization <-
  constrOptim(
      theta = rep(0.1,2),
      f     = objective,
      grad  =  NULL,
      ui    = rbind(diag(2),c(-3,-1)),
      ci    = c(0,0,-1),
      control=list(fnscale= -1)
    )

max_support_constrained <- constrained_optimization$value
jj <- constrained_optimization$par
jj <- fillup(c(jj[c(1,1,1,2)]))
names(jj) <- pnames(H)
ml_p_constrained <- jj

support_different_strengths <- max_support_free - max_support_constrained

print(paste("support for different strengths = ", support_different_strengths, sep=""))
if(support_different_strengths > 2){
  print("two units of support criterion exceeded: strong evidence that the three players have different strengths")
} else {
  print("less than two units of support: no evidence for differing players' strengths")
}

print(paste("p-value = ",pchisq(2*support_different_strengths,df=1,lower.tail=FALSE)))


## Now test the hypothesis that playing white confers no advantage:


small <- 1e-4

objective <- function(x){
  loglik(H,c(x,small)) # draw is the fillup, white advantage set to 'small'
}

constrained_optimization <-
  constrOptim(
      theta = c(0.1,0.1,0.1),
      f       = objective,
      grad    =  NULL,
      ui      = rbind(diag(3),-1),
      ci      = c(0,0,0,-1),
      control = list(fnscale= -1)
  )


max_support_nowhite <- constrained_optimization$value

support_no_white_advantage <- max_support_free - max_support_nowhite

print(paste("support for white advantage = ", support_no_white_advantage, sep=""))
if(support_no_white_advantage > 2){
  print("two units of support criterion exceeded: strong evidence that playing white is an advantage")
} else {
  print("less than two units of support: no evidence for white being an advantage")
}

print(paste("p-value = ",pchisq(2*support_no_white_advantage,df=1,lower.tail=FALSE)))

