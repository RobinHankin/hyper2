## This file creates a 3x3x3 array of results for the
## karpov/kasparov/anand dataset.

library("hyper2")
attach(as.list(kka))
        
players <- c("Anand","Karpov","Kasparov")

jj_white_wins <- matrix(NA,3,3)
dimnames(jj_white_wins) <- list(plays_white_wins=players,plays_black_loses=players)
jj_white_wins["Anand"   ,"Karpov"  ] <- anand_plays_white_beats_karpov
jj_white_wins["Anand"   ,"Kasparov"] <- anand_plays_white_beats_kasparov
jj_white_wins["Karpov"  ,"Anand"   ] <- karpov_plays_white_beats_anand
jj_white_wins["Karpov"  ,"Kasparov"] <- karpov_plays_white_beats_kasparov
jj_white_wins["Kasparov","Anand"   ] <- kasparov_plays_white_beats_anand
jj_white_wins["Kasparov","Karpov"  ] <- kasparov_plays_white_beats_karpov


jj_draw <- matrix(NA,3,3)
dimnames(jj_draw) <- list(plays_white_draws=players,plays_black_draws=players)

jj_black_wins <- matrix(NA,3,3)
dimnames(jj_black_wins) <- list(plays_black_wins=players,plays_black_loses=players)

out <- abind(
    white_wins=jj_white_wins,
    draw=jj_draw,
    black_wins=jj_black_wins,
    along=3)



