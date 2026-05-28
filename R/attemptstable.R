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
            if(any(!is.na(p))){
                string <- rowfiddle(string, which.max(p))
            }
        }
        cat(string)
        cat("\n")
    }
}

rowfiddle <- function(v, n){  # takes a string, eg v="Vesely     79.73   80.3  85.44      X  84.98      X"

    jj <- rle(strsplit(v, "")[[1]] == " ")
    start_position <- sum(jj$lengths[seq_len(n*2    )])
      end_position <- sum(jj$lengths[seq_len(n*2 + 1)])

    paste(
        substr(v, 1, start_position),
        crayon::bold(substr(v, start_position+1, end_position)),
        substr(v, end_position+1, nchar(v)),
        sep = ""
    )
}

`suppfun.attemptstable` <- function(x, ...){
    attemptstable2supp3(x, ...)
}

as.vector.attemptstable <- function(x, mode){
    sort(setNames(c(x,recursive=TRUE), rep(rownames(x),ncol(x))), decreasing=TRUE)
}

`attemptstable2supp3` <- function(x, decreasing=TRUE, give.supp=TRUE, nothrow_loses=FALSE){
    o <- c(matrix(suppressWarnings(as.numeric(c(x, recursive=TRUE))), nrow(x), ncol(x)))
    names(o) <- c(matrix(rownames(x), nrow(x), ncol(x)))
    o <- sort(o, na.last=TRUE, decreasing=decreasing)
    if(give.supp){
        if(nothrow_loses){
            nf <- names(o)[is.na(o)]
        } else {
            nf <- NULL
        }
        return(ordervec2supp3(v=names(o)[!is.na(o)], nonfinishers=nf))
    } else {
        return(o)
    }
}

`rattemptstable` <- function(ncompetitors=8, nthrows=6, prob=0.23){   
    n <- ncompetitors*nthrows
    out <- matrix(round(runif(n),3), ncol=nthrows)
    out[sample(seq_len(n), round(n*prob), replace=FALSE)] <- -Inf
    m <- apply(out, 1, max, na.rm=TRUE)
    out <- out[order(-m),]
    out[is.infinite(out)] <- NA
    rownames(out) <- letters[seq_len(ncompetitors)]
    colnames(out) <- paste("throw", seq_len(nthrows), sep="")
    return(as.attemptstable(out))
}
