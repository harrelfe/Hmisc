na.pattern<-function(x)
{
        if(is.list(x)) {
                k <- length(x)
                n <- length(x[[1]])
                x <- matrix(unlist(x), n, k)
        }
        n <- dim(x)[1]
        k <- dim(x)[2]
        y <- matrix(as.integer(is.na(x)), n, k)
        pattern <- y[, 1]
        for(i in 2:k) {
                pattern <- paste(pattern, y[, i], sep = "")
        }
        table(pattern)
}



