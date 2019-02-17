library("hyper2")

jj <- read.table("table.txt",header=FALSE)
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



free <- loglik(H,indep(maxp(H)))

s <- indep(equalp(H))
s[1] <- s[1] + 0.001
s[2] <- s[2] - 0.001


mle <- maxp(H,fcm=c(1,-1,rep(0,22)),fcv=0,startp=s)
constrained <- loglik(H,indep(mle))

support <- free-constrained

print(paste("support = ", support,sep=""))
if(support>2){print("two units of support criterion exceeded: the Soviets *did* collude!")}

print(paste("p-value = ",pchisq(2*support,df=1,lower.tail=FALSE)))
