## This file creates a 3x3x3 array of results for the
## karpov/kasparov/anand dataset.

library("hyper2")
library("abind")
attach(as.list(kka))
        
players <- c("Anand","Karpov","Kasparov")

white_wins <- matrix(NA,3,3)
dimnames(white_wins) <- list(plays_white_wins=players,plays_black_loses=players)
white_wins["Anand"   ,"Karpov"  ] <- anand_plays_white_beats_karpov
white_wins["Anand"   ,"Kasparov"] <- anand_plays_white_beats_kasparov
white_wins["Karpov"  ,"Anand"   ] <- karpov_plays_white_beats_anand
white_wins["Karpov"  ,"Kasparov"] <- karpov_plays_white_beats_kasparov
white_wins["Kasparov","Anand"   ] <- kasparov_plays_white_beats_anand
white_wins["Kasparov","Karpov"  ] <- kasparov_plays_white_beats_karpov

drawn_games <- matrix(NA,3,3)
dimnames(drawn_games) <- list(plays_white_draws=players,plays_black_draws=players)
drawn_games["Anand"   ,"Karpov"  ] <- anand_plays_white_draws_karpov
drawn_games["Anand"   ,"Kasparov"] <- anand_plays_white_draws_kasparov
drawn_games["Karpov"  ,"Anand"   ] <- karpov_plays_white_draws_anand
drawn_games["Karpov"  ,"Kasparov"] <- karpov_plays_white_draws_kasparov
drawn_games["Kasparov","Anand"   ] <- kasparov_plays_white_draws_anand
drawn_games["Kasparov","Karpov"  ] <- kasparov_plays_white_draws_karpov


black_wins <- matrix(NA,3,3)
dimnames(black_wins) <- list(plays_black_wins=players,plays_black_loses=players)
black_wins["Anand"   ,"Karpov"  ] <- anand_plays_black_beats_karpov
black_wins["Anand"   ,"Kasparov"] <- anand_plays_black_beats_kasparov
black_wins["Karpov"  ,"Anand"   ] <- karpov_plays_black_beats_anand
black_wins["Karpov"  ,"Kasparov"] <- karpov_plays_black_beats_kasparov
black_wins["Kasparov","Anand"   ] <- kasparov_plays_black_beats_anand
black_wins["Kasparov","Karpov"  ] <- kasparov_plays_black_beats_karpov

kka_array <- abind(white_wins,drawn_games,black_wins,along=3)
detach(as.list(kka))
