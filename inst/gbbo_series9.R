library("hyper2")

bakers <-
  c("Rahul","Kim-Joy","Ruby","Briony","Manon","Jon",
    "Dan","Karen","Terry","Antony","Luke","Imelda")
 ## NB game order: Rahul won, Kim-Joy runner-up, through to Imelda who
 ## was first to be eliminated.

H <- hyper2(pnames = bakers)


 ## variable 'doo' is a Boolean, with entries governing whether a
 ## particular round is included in L or not.

doo <- c(
    week1 = TRUE, 
    week2 = TRUE, 
    week3 = TRUE, 
    week4 = TRUE, 
    week5 = TRUE, 
    week6 = TRUE, 
    week7 = TRUE, 
    week8 = TRUE, 
    week9 = TRUE, 
    weekt = TRUE  # weekt = "week 10"
)

L <- list()  # overall list

## In the following the key is:
##
## SB = star baker
## FB = favourite baker
## GT = got through
## LF = least favourite
## EL = eliminated


if(doo[["week1"]]){
    L$week1 <-
        ggrl(H,
             SB = c("Manon"),  
             FB = c("Briony"),
             GT = c("Rahul","Kim-Joy","Jon","Dan","Karen","Antony","Luke"),
             LF = c("Ruby","Terry"),
             EL = c("Imelda")
             )
}

if(doo[["week2"]]){
    L$week2 <-
        ggrl(H,
             SB = c("Rahul"),  
             FB = c("Jon","Dan"),
             GT = c("Kim-Joy","Ruby","Manon","Karen","Antony"),
             LF = c("Briony","Terry"),
             EL = c("Luke")
             )
}

if(doo[["week3"]]){
    L$week3 <-
        ggrl(H,
             SB = c("Rahul"),  
             FB = c("Dan"),
             GT = c("Kim-Joy","Ruby","Manon","Jon","Karen","Terry"),
             LF = c("Briony"),
             EL = c("Antony")
             )
}

if(doo[["week4"]]){
    L$week4 <-
        ggrl(H,
             SB = c("Dan"),  
             FB = c("Rahul","Jon"),
             GT = c("Kim-Joy","Ruby","Manon"),
             LF = c("Briony","Karen")
             ## NB: noone eliminated in week 4 (Terry was ill)
             )
}

if(doo[["week5"]]){
    L$week5 <-
        ggrl(H,
             SB = c("Kim-Joy"),  
             FB = c("Rahul"),
             GT = c("Ruby","Manon","Dan"),
             LF = c("Briony","Jon"),
             EL = c("Karen","Terry")
             )
}

if(doo[["week6"]]){
    L$week6 <-
        ggrl(H,
             SB = c("Briony"),  
             FB = c("Rahul","Ruby"),
             GT = c("Kim-Joy"),
             LF = c("Manon"),
             EL = c("Dan")
             )
}

if(doo[["week7"]]){
    L$week7 <-
        ggrl(H,
             SB = c("Kim-Joy"),  
             FB = c("Rahul"),
             GT = c("Briony","Manon"),
             LF = c("Ruby"),
             EL = c("Jon")
             )
}

if(doo[["week8"]]){
    L$week8 <-
        ggrl(H,
             SB = c("Ruby"),  
             FB = c("Briony"),
             ##   noone "got through" in week 8
             LF = c("Rahul","Kim-Joy"),
             EL = c("Manon")
             )
}

if(doo[["week9"]]){
    L$week9 <-
        ggrl(H,
             SB = c("Ruby"),  
             FB = c("Kim-Joy"),
             ##   noone "got through" in week 9
             LF = c("Rahul"),
             EL = c("Briony")
             )
}
if(doo[["weekt"]]){
    L$weekt <-
        ggrl(H,
             SB = c("Rahul"),  
             ##  
             ##   noone "got through" in week 9
             ## 
             EL = c("Kim-Joy","Ruby")
             )
}

n <- 12   # 13 players; now specify constraints:
UI <- rbind(diag(n-1),-1)
CI <- c(rep(0,n-1),-1)

hotstart <- # Here is one I made earlier (takes about an hour)
c(0.262994207219248, 0.151179869681624, 0.0819097491529236, 0.0484873812623128, 
0.106987005779039, 0.0751548811388982, 0.181100160787176, 0.0347288257183941, 
0.0151307039659374, 0.0308533385753615, 0.0114738763276801)
## If hotstart not available, use theta = indep(equalp(H)) below

ans_unconstrained <-   # takes about an hour to run without hotstart, 1 minute with hotstart
    constrOptim(
        theta = hotstart,
        f = function(p){-like_series(p,L)},  # 'L' created sequentially above
        grad = NULL,
        ui = UI, ci=CI,
        control=list(trace=100,maxit=100000)
    )


evaluate <- fillup(ans_unconstrained$par)
names(evaluate) <- bakers
dotchart(evaluate,pch=16)
