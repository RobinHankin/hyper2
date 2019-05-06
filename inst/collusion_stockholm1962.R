# Analysis of the 1962 world chess championship, Curacao.  Reference:
# C. C. Moul and J. V. C. Nye 2009. "Did the Soviets collude? A
# statistical analysis of championship chess 1940-1978.  Journal of
# Economic Behaviour and Organization 70(2009) 10-21


library("hyper2")

jj <- read.table("stockholm1962.txt",header=FALSE)
results <- as.matrix(jj[,-(1:2)])
players <- as.character(jj$V1)
nationality <- as.character(jj$V2)
rownames(results) <- players
colnames(results) <- players
points <- rowSums(results,na.rm=TRUE)

H <- hyper2(pnames=c("draw","collusion",players))

for(i in seq_len(nrow(results)-1)){  # i=row
    for(j in seq(from=i+1,to=ncol(results))){ # j = col
        o <- results[i,j]
        if((nationality[i]=="USSR") & (nationality[j]=="USSR")){
            monster <- "collusion"
        } else {
            monster <- "draw" 
        }
        if(o==2){ # row player wins
            H[players[i]] %<>% inc(1)
        } else if(o==0){  # col player wins
            H[players[j]] %<>% inc(1)
        } else if(o==1){ # game drawn
            H[monster] %<>% inc(1)
        } else {
            stop("this cannot happen")
        }
        H[c(players[i],players[j],monster)] %<>% dec(1)
    }  # j loop closes
} # i loop closes

## Now some optimization.  First optimize freely:
max_support_free <- maxp(H,give=TRUE)$value
ml_p   <- maxp(H)

s <- indep(equalp(H))
small <- 1e-3
s[1] <- s[1] + small
s[2] <- s[2] - small

## Perform the constrained optimization:
ml_p_constrained <- maxp(H,fcm=c(1,-1,rep(0,22)),fcv=0,startp=s)
max_support_constrained <- loglik(H,indep(ml_p_constrained))

support <- max_support_free - max_support_constrained

print(paste("support = ", support,sep=""))
if(support>2){print("two units of support criterion exceeded: strong evidence that the Soviets colluded")}

print(paste("p-value = ",pchisq(2*support,df=1,lower.tail=FALSE)))


## Now some comparison between stockholm1962.txt (results) and
## stockholm1962_matches.txt (results2)
Hbw <- hyper2(pnames=c("white","draw",players))
Hbwcoll <- hyper2(pnames=c("white","draw","collusion",players))

restab <- read.table("stockholm1962_matches.txt",header=FALSE)
stopifnot(all(unique(sort(c(as.character(restab$V1),as.character(restab$V2)))) == sort(players)))
results2 <- matrix(0,length(players),length(players))
rownames(results2) <- players
colnames(results2) <- players
diag(results2) <- NA

for(i in seq_len(nrow(restab))){
    white_player <- as.character(restab[i,1])
    black_player <- as.character(restab[i,2])
    match_result <- as.character(restab[i,3])

    nw <- which(players==white_player)
    nb <- which(players==black_player)

    if(match_result == "1-0"){ # white win
        results2[nw,nb] <- 2
        results2[nb,nw] <- 0

        Hbw[c(white_player             ,"white"       )] %<>% inc()
        Hbw[c(white_player,black_player,"white","draw")] %<>% dec()

        if(nationality[nw]=="USSR" & nationality[nb]=="USSR"){
            Hbwcoll[c(white_player             ,"white"       )] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","collusion")] %<>% dec()
        } else {
            Hbwcoll[c(white_player             ,"white"       )] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","draw")] %<>% dec()
        }
    } else if(match_result == "0-1"){ # black wins
        results2[nw,nb] <- 0
        results2[nb,nw] <- 2
        Hbw[c(             black_player               )] %<>% inc()
        Hbw[c(white_player,black_player,"white","draw")] %<>% dec()

        if(nationality[nw]=="USSR" & nationality[nb]=="USSR"){
            Hbwcoll[c(             black_player                    )] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","collusion")] %<>% dec()
        } else {  # collusion not playing
            Hbwcoll[c(             black_player               )] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","draw")] %<>% dec()
        }

    } else if (match_result == "1/2-1/2"){ # draw
        results2[nw,nb] <- 1
        results2[nb,nw] <- 1
        Hbw[c(                                  "draw")] %<>% inc()
        Hbw[c(white_player,black_player,"white","draw")] %<>% dec()

        if(nationality[nw]=="USSR" & nationality[nb]=="USSR"){
            Hbwcoll[c(                                  "collusion")] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","collusion")] %<>% dec()
        } else {  # collusion not playing
            Hbwcoll[c(                                  "draw")] %<>% inc()
            Hbwcoll[c(white_player,black_player,"white","draw")] %<>% dec()
        }



    } else {
        stop("not possible")
    }
}

## results [from the square matrix] and results2 [from the 3-column
## dataframe] should match:
stopifnot(all(results[!is.na(results)] == results2[!is.na(results2)]))
small <- 0.01


maxlike_free <- maxp(Hbwcoll,startp=c(small,small,small/2,rep(small,22)),give=TRUE)
print("l")
dput(maxlike_free$value)

jj <- maxlike_free$par
maxlike_free <- maxp(Hbwcoll,startp=jj,give=TRUE)
dput(maxlike_free$value)

jj <- maxlike_free$par
maxlike_free <- maxp(Hbwcoll,startp=jj,give=TRUE,hessian=TRUE)
dput(maxlike_free$value)


freemp <- maxlike_free$par
freemp[2:3] <- mean(freemp[3:2]) +c(small,-small)*0.7

startp <- freemp

for(i in 1:2){
    maxlike_constrained <-
        maxp(Hbwcoll,startp=startp,fcm=c(0,1,-1,rep(0,22)),fcv=0,give=TRUE)
    startp <- maxlike_constrained$par
    dput(maxlike_constrained$value)
}

maxlike_constrained <-
    maxp(Hbwcoll,startp=startp,fcm=c(0,1,-1,rep(0,22)),fcv=0,give=TRUE)

print(maxlike_constrained$value - maxlike_free$value)
