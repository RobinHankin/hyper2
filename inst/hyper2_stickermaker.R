library("hexSticker")
library("hyper2")

imgurl <- system.file("monster_blue.png", package="hyper2")
sticker(imgurl, package="hyper2", p_size=8, s_x=1, s_y=0.84,
s_width=0.7,asp=0.85, white_around_sticker=TRUE, h_fill="#7733FF",
h_color="#000000", filename="hyper2.png")
