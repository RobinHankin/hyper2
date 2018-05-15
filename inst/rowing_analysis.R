library(hyper2)

filename <- "rowing.txt"  # could be rowing_minimal.txt
o <- strsplit(readLines(filename)," ")

rowers <- sort(unique(unlist(o)))
H <- hyper2(list(),0,pnames=rowers)

for(v in o){
    v <- rev(v)
    for(i in seq_along(v)){
        H[v[i]] <- powers(H[v[i]]) + 1
        H[v[seq_len(i)]] <- powers(H[v[seq_len(i)]])-1
    }
}

I <- hyper2(pnames=rowers)
for(v in o){
  I <- I+order_likelihood(character_to_number(v,rowers))
}
