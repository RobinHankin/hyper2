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


# Likelihood functions calculated in inst/karpov_kasparov_anand.R follows:
`karpov_kasparov_anand` <-
structure(list(brackets = list("Anand", c("Anand", "Karpov", 
"draw", "white"), c("Anand", "Kasparov", "draw", "white"), c("Anand", 
"white"), "Karpov", c("Karpov", "Kasparov", "draw", "white"), 
    c("Karpov", "white"), "Kasparov", c("Kasparov", "white"), 
    "draw"), powers = c(15, -92, -80, 24, 12, -193, 25, 20, 45, 
224), pnames = c("Karpov", "Kasparov", "Anand", "white", "draw"
)), class = "hyper2")

# Likelihood functions calculated in inst/kka_3whites.R follows:
`kka_3whites` <-
structure(list(brackets = list("Anand", c("Anand", "Anand_white"
), c("Anand", "Anand_white", "Karpov", "draw"), c("Anand", "Anand_white", 
"Kasparov", "draw"), c("Anand", "Karpov", "Karpov_white", "draw"
), c("Anand", "Kasparov", "Kasparov_white", "draw"), "Karpov", 
    c("Karpov", "Karpov_white"), c("Karpov", "Karpov_white", 
    "Kasparov", "draw"), c("Karpov", "Kasparov", "Kasparov_white", 
    "draw"), "Kasparov", c("Kasparov", "Kasparov_white"), "draw"), 
    powers = c(15, 24, -43, -37, -49, -43, 12, 25, -99, -94, 
    20, 45, 224), pnames = c("Karpov", "Kasparov", "Anand", "Karpov_white", 
    "Kasparov_white", "Anand_white", "draw")), class = "hyper2")

# Likelihood functions calculated in inst/kka_3draws.R follows:
`kka_3draws` <-
structure(list(brackets = list("Anand", c("Anand", "Anand_draw", 
"Karpov", "Karpov_draw", "white"), c("Anand", "Anand_draw", "Kasparov", 
"Kasparov_draw", "white"), c("Anand", "white"), c("Anand_draw", 
"Karpov_draw"), c("Anand_draw", "Kasparov_draw"), "Karpov", c("Karpov", 
"Karpov_draw", "Kasparov", "Kasparov_draw", "white"), c("Karpov", 
"white"), c("Karpov_draw", "Kasparov_draw"), "Kasparov", c("Kasparov", 
"white")), powers = c(15, -92, -80, 24, 49, 46, 12, -193, 25, 
129, 20, 45), pnames = c("Karpov", "Kasparov", "Anand", "white", 
"Karpov_draw", "Kasparov_draw", "Anand_draw")), class = "hyper2")

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
