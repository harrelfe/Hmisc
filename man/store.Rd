\name{store}
\alias{store}
\alias{stores}
\alias{storeTemp}
\title{
Store an Object Permanently
}
\description{
By default, \code{store} will copy the object to \code{.Data} under the
same name. 
This function is most useful when you have attached a data frame or a
temporary directory
in position 1.  \code{store} is also useful for setting up to store later
objects in a temporary work area (\code{.Data.tempnnnn}, where \code{nnnn} is a
number computed by the system) so that they are not stored on
disk.  For this usage, just invoke \code{store} with no arguments, i.e.,
\code{store()}.  After that, you can still invoke \code{store} with arguments
so that the object is copied to permanent storage.  Another function,
\code{stores} is useful for storing a series of temporary objects in
\code{.Data} with one call.  \code{store} and \code{stores} are not available
For \R.  See Details below for a method of approximating the use of
\code{store} in \R.

\code{storeTemp} stores an object in frame 0 for S-Plus or in a temporary
environment \code{.GlobalTemp} in \R, attaching that environment if it is
not already attached, so that the objects are easily available.
}
\usage{
store(object, name=as.character(substitute(object)),
      where=if (under.unix || .SV4.) ".Data" else "_Data")
stores(\dots)
storeTemp(object, name=deparse(substitute(object)))
}
\arguments{
\item{object}{
object to store (omit to set search list position one to a 
temporary directory created by \code{store})
}
\item{name}{
name under which to store the object. Default is name of object in
call to \code{store()}.
}
\item{where}{
directory in which to store object. Default is \code{.Data} underneath current
directory (for UNIX) or position 2 in the search list (for Windows).
For \R the default is \code{.GlobalEnv}.
}
\item{...}{
a list of objects to store in \code{.Data} or \code{.GlobalEnv} permanently,
using names which are the same as the argument names
}}
\section{Side Effects}{
uses \code{assign} and \code{attach} functions.  \code{store} with no arguments also
stores a function \code{.Last} in \code{.Data.tempnnnn}, which
will cause \code{.Data.tempnnnn} to be removed when the S session ends.
For S-Plus, \code{store()}
causes creation of a system option named \code{.store.temp} which contains
the name of the temporary directory created.
}
\seealso{
\code{\link{assign}}, \code{\link{.Last}}, \code{\link{attach}}, \code{\link{search}}
}
\details{
To almost mimic the functionality of \code{store} or \code{stores} in \R,
you can do the following.  Use \code{save(x,y,z,file="Permdata")} to save
permanent objects in \code{"permdata"}.  When you exit \R, do not save the
workspace.  Then all temporary objects will disappear.  In your
\code{.Rprofile} put the command \code{load("Permdata")} so that the next time
you invoke \R the permanent objects will be available.
}
\examples{
\dontrun{
attach(database, 1)     #this database takes precedence
store()                 #new objects to go under database in memory
                        #this doesn't work in R
f <- lm(y ~ x)
store(f)                #store f under name "f" in .Data or .GlobalEnv
                        #uses assign() with immediate=T
store(f, "final.model") #immediately store f under "final.model" in .Data
store()                 #store future objects in .Data.tempnnnn
x <- runif(1000)        #x will disappear at session end unless
store(x)                #this statement appears -> store in .Data
stores(x, y, z)         #store x,y,z in .Data under names x,y,z
storeTemp(x)            #put x as name 'x' in frame 0
                        #for R, attach .GlobalTemp and store it there
storeTemp(x,'X')        #same as previous but under the name X
}
}
\keyword{data}


