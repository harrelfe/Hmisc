\name{ffCompress}
\alias{ffCompress}
\alias{as.data.frame.ffdflabel}
\alias{[.ffdflabel}
\title{Transform Data Frame Into a Compact \code{ff} Object}
\description{
	The \code{ff} package implements a wide variety of atomic data types
	down to 2 bits, allowing for compact storage of large datasets and
	requiring memory usage in R for only those rows and columns of the
	dataset that are needed at one time.  It is best to create a compact
	\code{ffdf} data frame object while initially reading the external
	data file, using for example an input \code{.csv} file and specifying
	the \code{ff} \code{vmode}s for all the columns.  If you can get
	enough memory to read the entire dataset into an R \code{data.frame}
	you can use \code{ffCompress} after the fact to analyze the data frame
	and use the most compact data representation possible.  This entails
	using single precision for floating point numbers (which can be easily
	overridden to use R's usual double precision) and a variety of integer
	types depend on the number of bits used by the maximum absolute value
	of the variable, whether or not \code{NA}s exist in the data, and
	whether or not negative values are possible.

	Since \code{ff} does not allow variable labels and units, any such
	attributes are stripped off of variables and stored as attributes on the
	entire \code{ffdf} object.  An \code{as.data.frame} and subscripting
	method retrieves these attributes and restores them to individual
	variables when needed.
}
\usage{
ffCompress(obj, float=c('single', 'double'), print=FALSE)

\method{as.data.frame}{ffdflabel}(x, \dots)
}
\arguments{
  \item{obj}{a data frame}
	\item{float}{representation to use for floating point vectors.  The
		default is single precision (4 bytes, 7 significant digits).
		Specify \code{float='double'} to use double precision (8 bytes, 15
		significant digits)}
	\item{print}{set to \code{TRUE} to get progress output and passed as
		the \code{VERBOSE} argument to \code{ff} functions}
	\item{x}{an \code{ffdf} object}
	\item{\dots}{ignored}
}
\value{
	an \code{ffdf} object for \code{ffCompress}, a \code{data.frame} for
	\code{as.data.frame}, and either one of these for subscripting.  If
	subscripting results in a single variable and \code{drop=FALSE} is not
	specified, the result is of \code{ff} type.
}
\author{Frank Harrell, Vanderbilt University}
\seealso{\code{\link[Hmisc]{cleanup.import}}}
\examples{
\dontrun{
require(ff)
require(survival)
n <- 1e6
d <- data.frame(x=rnorm(n), y=sample(0:1, n, TRUE),
                i=as.Date('2013-01-02'), S=Surv(runif(n)),
                z=factor(sample(1:3, n, TRUE), 1:3,
                  c('elephant','giraffe','dog')))
## Cannot have labels for variables; ff will reject as non-atomic vectors
storage.mode(d$y)
object.size(d)
n * (8 + 4 + 4 + 4)
f <- as.ffdf(d, vmode=c('single', 'quad', 'integer', 'single', 'quad'))
vmode(f)
n * (4 + 0.25 + 4 + 0.25)
object.size(as.data.frame(f))
f[1:10,]
hist(d[,'x'] - f[,'x'], nclass=100)
table(d[,'z'], f[,'z'])

system.time(subset(f, z == 'dog'))
system.time({i <- ffwhich(f, z == 'dog'); f[i,]})
table(subset(f, z == 'dog')[,'z'])
class(subset(f, z == 'dog'))

ffsave(f, file='/tmp/f')  # creates /tmp/f.ffData /tmp/f.RData
## To load: ffload('/tmp/f')

d <- upData(d, labels=c(y='Y'), units=c(z='units z'))
f <- ffCompress(d)
vmode(f)

load('ras.rda')   # dataset is not available
r <- ffCompress(ras)
vmode(r)
attr(r, 'label')
attr(r, 'units')
all.equal(ras, as.data.frame(r))
dr <- as.data.frame(r)
g <- function(x) names(attributes(x))
nam <- names(dr)
for(i in 1 : ncol(dr)) {
  a <- ras[[i]]
  b <- dr[[i]]
  cat(nam[i], '\n')
  cat(g(a), '\n', g(b), '\n')
  cat(max(w <- abs(unclass(a) - unclass(b)), na.rm=TRUE), '\n')
  if(nam[i] == 'ldl') {
    j <- which.max(abs(w))
    cat(a[j], b[j], '\n')
  }
}

dr <- as.data.frame(r)
xless(contents(dr))
xless(contents(r[1:10,]))
xless(contents(r[,1:10]))

table(r[, 'gender'])
## subset invokes [] so uses method from ffdflabel
m <- subset(r, gender == 'Male')
class(m)
dim(m)
attr(m, 'label')
attributes(m[,'age'])
df <- as.data.frame(m)
class(df$age)
label(df$age)
## But if subset again things are not OK
k <- subset(m, age < 3)
class(k)
contents(k[, 'age', drop=FALSE])
invisible(ffsave(r, file='/tmp/r'))

## w <- read.csv.ffdf(file='/tmp/data.csv', first.rows=10000)
## table(vmode(w))

## From ff manual:  vmode definitions
# boolean 1 bit logical without NA
# logical 2 bit logical with NA
# quad 2 bit unsigned integer without NA
# nibble 4 bit unsigned integer without NA
# byte 8 bit signed integer with NA
# ubyte 8 bit unsigned integer without NA
# short 16 bit signed integer with NA
# ushort 16 bit unsigned integer without NA
# integer 32 bit signed integer with NA
# single 32 bit float
# double 64 bit float
# complex 2x64 bit float
# raw 8 bit unsigned char
# character character
}
}
\keyword{attribute}
\keyword{misc}
\keyword{utilities}
