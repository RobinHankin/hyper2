## This file defines dataset kka which is the raw dataset.  It is used
## calculate two likelihood functions: `karpov_kasparov_anand` and
## `kka_3draws` which are assigned here.  These functions are
## calculated in files inst/karpov_kasparov_anand.R and
## inst/kka_3draws.R respectively.

## The kka dataset is also used, in inst/kka_array.R, to calculate
## tables such as `white_wins` and `kka_array`.

## Help on these objects is given in man/karpov_kasparov_anand.Rd.

kka <- c(
## Kasparov vs Karpov
karpov_plays_white_beats_kasparov = 18,  # 12 on p1, 6 on p2
kasparov_plays_white_beats_karpov = 30, # 13 on p1, 17 on p2
kasparov_plays_white_losesto_karpov = 07,
karpov_plays_white_losesto_kasparov = 09,
karpov_plays_white_draws_kasparov = 11+13+14+17+14+03,
kasparov_plays_white_draws_karpov = 14+12+11+08+11+01,

## Kasparov vs Anand
kasparov_plays_white_beats_anand = 15,
anand_plays_white_beats_kasparov = 06,
anand_plays_white_losesto_kasparov = 11,
kasparov_plays_white_losesto_anand = 02,
kasparov_plays_white_draws_anand = 26,
anand_plays_white_draws_kasparov = 20,

## Karpov vs Anand
karpov_plays_white_beats_anand = 07,
anand_plays_white_beats_karpov = 18,
anand_plays_white_losesto_karpov = 05,
karpov_plays_white_losesto_anand = 13,
karpov_plays_white_draws_anand = 29,
anand_plays_white_draws_karpov = 20
)


# Likelihood functions calculated in inst/kka_3draws.R follows:
`karpov_kasparov_anand` <-
structure(list(brackets = list(1, c(1, 2, 4, 5), c(1, 3, 4, 5
), c(1, 4), 2, c(2, 3, 4, 5), c(2, 4), 3, c(3, 4), 5), powers = c(12, 
-193, -92, 25, 20, -80, 45, 15, 24, 224), pnames = c("Karpov", 
"Kasparov", "Anand", "white", "draw")), class = "hyper2")

# Likelihood functions calculated in inst/karpov_kasparov_anand.R follows:
`kka_3draws` <-
structure(list(brackets = list(1, c(1, 2, 4, 5, 6), c(1, 3, 4, 
5, 7), c(1, 4), 2, c(2, 3, 4, 6, 7), c(2, 4), 3, c(3, 4), c(5, 
6), c(5, 7), c(6, 7)), powers = c(12, -193, -92, 25, 20, -80, 
45, 15, 24, 129, 49, 46), pnames = c("Karpov", "Kasparov", "Anand", 
"white", "Karpov_draw", "Kasparov_draw", "Anand_draw")), class = "hyper2")


## Matrices of scorelines calculated in ist/kka_array.R follows:
`plays_white_wins` <-
  structure(c(NA, 7, 15, 18, NA, 30, 6, 18, NA),
            .Dim = c(3L, 3L),
            .Dimnames = list(
                plays_white_wins = c("Anand", "Karpov", "Kasparov"),
                plays_black_loses = c("Anand", "Karpov", "Kasparov")))

`plays_white_draws` <-
  structure(c(NA, 29, 26, 20, NA, 57, 20, 72, NA),
            .Dim = c(3L, 3L),
            .Dimnames = list(
                plays_white_draws = c("Anand", "Karpov", "Kasparov"),
                plays_black_draws = c("Anand", "Karpov", "Kasparov")))

`plays_white_loses` <-
  structure(c(NA, 5, 11, 13, NA, 9, 2, 7, NA),
            .Dim = c(3L, 3L),
            .Dimnames = list(
                plays_black_wins = c("Anand", "Karpov", "Kasparov"),
                plays_black_loses = c("Anand", "Karpov", "Kasparov")))

`kka_array` <-
structure(c(NA, 7, 15, 18, NA, 30, 6, 18, NA, NA, 29, 26, 20, 
            NA, 57, 20, 72, NA, NA, 5, 11, 13, NA, 9, 2, 7, NA),
          .Dim = c(3L, 3L, 3L),
          .Dimnames = list(
              c("Anand", "Karpov", "Kasparov"),
              c("Anand", "Karpov", "Kasparov"),
              c("plays_white_wins",
                "plays_white_draws",
                "plays_white_loses")))
