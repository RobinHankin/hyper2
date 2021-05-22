## Processes all the Rmd and Rnw files in the inst/ directory.


library("hyper2")
library("rmarkdown")

files <- system("ls *.Rmd *.Rnw", intern=TRUE)
for(file in files){ render(file) }
    
