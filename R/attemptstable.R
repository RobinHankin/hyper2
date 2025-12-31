`as.attemptstable` <- function(x){
    x <- as.data.frame(x)
    class(x) <- c("attemptstable", "data.frame")  # the only place
    return(x)
}

`print.attemptstable` <- function(x, ...){
    cat("An attemptstable:\n")
    co <- capture.output(as.data.frame(x))
    
    cat(co[1]) # colnames
    cat("\n")
    for(i in 2:length(co)){
        string <- gsub("NA", " X", co[i])
        jj <- strsplit(string, " ")[[1]][-1]
        suppressWarnings(p <- as.numeric(jj[nchar(jj) > 0]))
        if(!isFALSE(getOption("bold_personal_best"))){
            string <- rowfiddle(string, which.max(p))
        }
        cat(string)
        cat("\n")
    }
}

rowfiddle <- function(v, n){  # takes a string, eg v="Vesely     79.73   80.3  85.44      X  84.98      X"
    bold_start <- "\033[1m"
    bold_end   <- "\033[22m"

    jj <- rle(strsplit(v, "")[[1]] == " ")
    start_position <- sum(jj$lengths[seq_len(n*2    )])
      end_position <- sum(jj$lengths[seq_len(n*2 + 1)])

    paste(
        substr(v,1,start_position),
        bold_start,
        substr(v,start_position+1, end_position),
        bold_end,
        substr(v,end_position+1, nchar(v)),
        sep = ""
    )
}

`suppfun.attemptstable` <- function(x, ...){
    attemptstable2supp3(x, ...)
}

as.vector.attemptstable <- function(x, mode){
    attemptstable2supp3(x, decreasing=TRUE, give.supp=FALSE, dnf.last=TRUE)
}

`attemptstable2supp3` <- function(x, decreasing=TRUE, give.supp=TRUE, dnf.last=TRUE){
    o <- c(matrix(suppressWarnings(as.numeric(c(x, recursive=TRUE))), nrow(x), ncol(x)))
    names(o) <- c(matrix(rownames(x), nrow(x), ncol(x)))
    o <- sort(o, na.last=TRUE, decreasing=decreasing)
    if(give.supp){
        if(dnf.last){
            nf <- names(o)[is.na(o)]
        } else {
            nf <- NULL
        }
        return(ordervec2supp3(v=names(o)[!is.na(o)], nonfinishers=nf))
    } else {
        return(o)
    }
}


