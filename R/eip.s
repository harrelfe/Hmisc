eip <- function(name) {
name <- as.character(substitute(name))
f <- find(name)
if(length(f)!=1) stop('object must exist in exactly one place')
## g <- if(under.unix) jove(get(name)) else edit(get(name))  16Apr02
g <- edit(get(name))
if(.R.) assign(name, g, pos=match(f,search())) else assign(name, g, where=f)
cat('Object', name, 'stored in', f, '\n')
invisible()
}
