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
