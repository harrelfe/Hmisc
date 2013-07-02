.tmpfunction <- function(...)
    stop("This function callback has not been defined yet")

Key <- function(...) {
    .tmpfunction(...)
}
environment(Key) <- new.env()

.setKey <- function(x) {
    environment(Key)$.tmpfunction <- x
}

Key2 <- function(...)
    .tmpfunction(...)
environment(Key2) <- new.env()

.setKey2 <- function(x)
    environment(Key2)$.tmpfunction <- x

sKey <- function(...)
    .tmpfunction(...)
environment(sKey) <- new.env()

.setsKey <- function(x)
    environment(sKey)$.tmpfunction <- x

Points <- function(...)
    .tmpfunction(...)
environment(Points) <- new.env()

.setPoints <- function(x)
    environment(Key)$.tmpfunction <- x

Abline <- function(...)
    .tmpfunction(...)
environment(Abline) <- new.env()

.setAbline <- function(x)
    environment(Key)$.tmpfunction <- x

Curve <- function(...)
    .tmpfunction(...)
environment(Curve) <- new.env()

.setCurve <- function(x)
    environment(Key)$.tmpfunction <- x
