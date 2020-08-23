`tennis` <-
    structure(list(brackets = list(
                       "P1", c("P1", "P2"), c("P1", "P2", "P3", "P4"),
                       c("P1","P3"), c("P1", "P4"), "P2", c("P2", "P3"),
                       c("P2", "P4"), "P3", c("P3", "P4"), "P4"),
                   powers = c(20, 9, -32, -20, -18, 23, -19, -17, 41, 2, 37),
                   pnames = c("P1", "P2", "P3", "P4")), .Names = c("brackets", "powers", "pnames"
), class = "hyper2")

`tennis_ghost` <-
structure(list(brackets = list(
                   "P1", c("P1", "P2", "P3", "P4"),
                   c("P1", "P2", "P3", "P4", "ghost"), c("P1", "P2", "ghost"),
                   c("P1", "P3"), c("P1", "P4"), "P2", c("P2", "P3"), c("P2", "P4"),
                   "P3", c("P3", "P4"), "P4"),
               powers = c(20, -21, -11, 9, -20, -18, 23, -19, -17, 41, -24, 37),
               pnames = c("P1", "P2", "P3", "P4", "ghost")),
          .Names = c("brackets", "powers", "pnames"), class = "hyper2")


