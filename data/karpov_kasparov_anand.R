## This  dataset is used in inst/karpov_kasparov_anand.R.


kka <- c(
## Kasparov vs Karpov
karpov_plays_white_beats_kasparov = 18,  # 12 on p1, 6 on p2
kasparov_plays_white_beats_karpov = 30, # 13 on p1, 17 on p2
karpov_plays_black_beats_kasparov =  9,
kasparov_plays_black_beats_karpov =  7,
karpov_draws_kasparov = 121,

## Kasparov vs Anand
kasparov_plays_white_beats_anand = 15,
anand_plays_white_beats_kasparov =  6,
kasparov_plays_black_beats_anand =  2,
anand_plays_black_beats_kasparov = 11,
kasparov_draws_anand = 31,

## Karpov vs Anand
karpov_plays_white_beats_anand =  7,
anand_plays_white_beats_karpov = 18,
karpov_plays_black_beats_anand = 13,
anand_plays_black_beats_karpov =  5,
karpov_draws_anand = 28
)


`karpov_kasparov_anand` <-
structure(list(brackets = list(1, c(1, 2, 4, 5), c(1, 3, 4, 5
), c(1, 4), 2, c(2, 3, 4, 5), c(2, 4), 3, c(3, 4), 5), powers = c(22, 
-185, -71, 25, 9, -65, 45, 16, 24, 180), pnames = c("Karpov", 
"Kasparov", "Anand", "white", "draw")), class = "hyper2")
