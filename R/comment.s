if(!.R.) {
"comment<-"  <- function(x, value) {
    if (inherits(value,"file"))
      attr(value,'class') <- c("comment.file", attr(value, 'class'))
    attr(x, "comment") <- value
    x
}

comment <- function(x)  {
    lab <- attr(x, "comment")
    if (inherits(lab,"comment.file"))
       attr(lab,'class') <- attr(lab,'class')[attr(lab,'class') != "comment.file"]
    lab
}

print.comment.file <- function(x, ...) {
        invisible(print(oldUnclass(x)))
}
}
