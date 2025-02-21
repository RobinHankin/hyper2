library("hexSticker")
library("magick")
library("magrittr")

transparent_background <- TRUE


sticker("monster_blue.png",package="hyper2",p_size=24,s_x=1,s_y=0.84,
s_width=0.7, asp=sqrt(3)/2, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="tempsticker.png")


if(transparent_background){
    ## Following methodology shamelessly ripped off from the excellent watson package:
    fuzz <- 50
    p <- image_read("tempsticker.png")
    w <- image_info(p)$width-1
    pp <- p %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+1"                   ) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+" , w  , "+1"  )) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+1", "+", w     )) %>%
        image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+" , w  , "+", w)) %>%
        image_write(path = "hyper2.png")
} else {
    p <- image_read("tempsticker.png")
    image_write(p, path = "hyper2.png")
}
