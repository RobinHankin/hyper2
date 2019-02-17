library("hyper2")
H <- hyper2(pnames=c("Karpov","Kasparov","Anand","white","draw"))


## Kasparov vs Karpov
karpov_plays_white_beats_kasparov <- 18  # 12 on p1, 6 on p2
kasparov_plays_white_beats_karpov <- 30  # 13 on p1, 17 on p2
karpov_plays_black_beats_kasparov <- 9
kasparov_plays_black_beats_karpov <- 7
karpov_draws_kasparov <- 121
karpov_vs_kasparov <- c("Karpov","Kasparov","white","draw")

H[c("Karpov","white")]   %<>% inc(karpov_plays_white_beats_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_plays_white_beats_kasparov)
H[c("Kasparov","white")] %<>% inc(kasparov_plays_white_beats_karpov)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_white_beats_karpov)
H[c("Karpov")]           %<>% inc(karpov_plays_black_beats_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_plays_black_beats_kasparov)
H[c("Kasparov")]         %<>% inc(kasparov_plays_black_beats_karpov)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_black_beats_karpov)
H[c("draw")]             %<>% inc(karpov_draws_kasparov)
H[karpov_vs_kasparov]    %<>% dec(karpov_draws_kasparov)

## Kasparov vs Anand
kasparov_plays_white_beats_anand <- 15
anand_plays_white_beats_kasparov <- 6
kasparov_plays_black_beats_anand <- 2
anand_plays_black_beats_kasparov <- 11
kasparov_draws_anand <- 31
kasparov_vs_anand <- c("Kasparov","Anand","white","draw")

H[c("Kasparov","white")] %<>% inc(kasparov_plays_white_beats_anand)
H[kasparov_vs_anand]     %<>% dec(kasparov_plays_white_beats_anand)
H[c("Anand","white")]    %<>% inc(anand_plays_white_beats_kasparov)
H[kasparov_vs_anand]     %<>% dec(anand_plays_white_beats_kasparov)
H[c("Kasparov")]         %<>% inc(kasparov_plays_black_beats_anand)
H[karpov_vs_kasparov]    %<>% dec(kasparov_plays_black_beats_anand)
H[c("Anand")]            %<>% inc(anand_plays_black_beats_kasparov)
H[kasparov_vs_anand]     %<>% dec(anand_plays_black_beats_kasparov)
H[c("draw")]             %<>% inc(kasparov_draws_anand)
H[kasparov_vs_anand]     %<>% dec(kasparov_draws_anand)

## Karpov vs Anand
karpov_plays_white_beats_anand <- 7
anand_plays_white_beats_karpov <- 18
karpov_plays_black_beats_anand <- 13
anand_plays_black_beats_karpov <- 5
karpov_draws_anand <- 28
karpov_vs_anand <- c("Karpov","Anand","white","draw")

H[c("Karpov","white")] %<>% inc(karpov_plays_white_beats_anand)
H[karpov_vs_anand]     %<>% dec(karpov_plays_white_beats_anand)
H[c("Anand","white")]  %<>% inc(anand_plays_white_beats_karpov)
H[karpov_vs_anand]     %<>% dec(anand_plays_white_beats_karpov)
H[c("Karpov")]         %<>% inc(karpov_plays_black_beats_anand)
H[karpov_vs_anand]     %<>% dec(karpov_plays_black_beats_anand)
H[c("Anand")]          %<>% inc(anand_plays_black_beats_karpov)
H[karpov_vs_anand]     %<>% dec(anand_plays_black_beats_karpov)
H[c("draw")]           %<>% inc(karpov_draws_anand)
H[karpov_vs_anand]     %<>% dec(karpov_draws_anand)

