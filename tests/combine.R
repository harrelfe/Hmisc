library(Hmisc)

a <- c(a = 5, b = 2, c = 4)
b <- c(c = 3, d = 4, e = 12)
c <- list(q = 5, h = 2, b = 14)
d <- list(w = 2, h = 3, e = 21)

a1 <- structure(c(5, 2, 3, 4, 12),
                .Names = c("a", "b", "c", "d", "e"))
a2 <- structure(list(a = 5, b = 14, c = 4, q = 5, h = 2),
                .Names = c("a", "b", "c", "q", "h"))
a3 <- structure(list(q = 5, h = 2, b = 2, a = 5, c = 4),
                .Names = c("q", "h", "b", "a", "c"))
a4 <- structure(list(q = 5, h = 2, b = 3, a = 2, c = 21),
                .Names = c("q", "h", "b", "a", "c"))


r1 <- combine(a, b, protect=FALSE)
r2 <- combine(a, c, protect=FALSE)
r3 <- combine(c, a, protect=FALSE)
r4 <- combine(c, d, protect=FALSE)

is.vector(r1)
is.list(r2)
is.list(r3)
is.list(r4)

all.equal(r1, a1)
all.equal(r2, a2)
all.equal(r3, a3)
all.equal(r4, a4)


combine(a, b, protect=TRUE)
combine(a, c, protect=TRUE)
combine(c, a, protect=TRUE)
combine(c, d, protect=TRUE)


e <- a
combine(e) <- b
e

e <- a
combine(e, protect = TRUE) <- b
e
