## This file defines dataset kka which is the raw dataset.  It is used
## calculate two likelihood functions: `karpov_kasparov_anand` and
## `kka_3draws` which are assigned here.  These functions are
## calculated in files inst/karpov_kasparov_anand.R and
## inst/kka_3draws.R respectively.

kka <- c(
## Kasparov vs Karpov
karpov_plays_white_beats_kasparov = 18,  # 12 on p1, 6 on p2
kasparov_plays_white_beats_karpov = 30, # 13 on p1, 17 on p2
karpov_plays_black_beats_kasparov = 07,
kasparov_plays_black_beats_karpov = 09,
karpov_plays_white_draws_kasparov = 11+13+14+17+14+03,
kasparov_plays_white_draws_karpov = 14+12+11+08+11+01,

## Kasparov vs Anand
kasparov_plays_white_beats_anand = 15,
anand_plays_white_beats_kasparov = 06,
kasparov_plays_black_beats_anand = 11,
anand_plays_black_beats_kasparov = 02,
kasparov_plays_white_draws_anand = 26,
anand_plays_white_draws_kasparov = 20,

## Karpov vs Anand
karpov_plays_white_beats_anand = 07,
anand_plays_white_beats_karpov = 18,
karpov_plays_black_beats_anand = 05,
anand_plays_black_beats_karpov = 13,
karpov_plays_white_draws_anand = 29,
anand_plays_white_draws_karpov = 20
)




`karpov_kasparov_anand` <-
structure(list(brackets = list(1, c(1, 2, 4, 5), c(1, 3, 4, 5
), c(1, 4), 2, c(2, 3, 4, 5), c(2, 4), 3, c(3, 4), 5), powers = c(12, 
-193, -92, 25, 20, -80, 45, 15, 24, 224), pnames = c("Karpov", 
"Kasparov", "Anand", "white", "draw")), class = "hyper2")

`kka_3draws` <-
  structure(list(brackets = list(1, c(1, 2, 4, 5, 6), c(1, 3, 4, 
5, 7), c(1, 4), 2, c(2, 3, 4, 6, 7), c(2, 4), 3, c(3, 4), c(5, 
6), c(5, 7), c(6, 7)), powers = c(12, -193, -92, 25, 20, -80, 
45, 15, 24, 129, 49, 46), pnames = c("Karpov", "Kasparov", "Anand", 
"white", "Karpov_draw", "Kasparov_draw", "Anand_draw")), class = "hyper2")
