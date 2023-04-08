\name{rlegend}
\alias{rlegend}
\alias{rlegendg}
\title{Special Version of legend for R}
\description{
  \code{rlegend} is a version of \code{\link{legend}} for \R that implements
  \code{plot=FALSE}, adds \code{grid=TRUE}, and defaults \code{lty},
  \code{lwd}, \code{pch} to \code{NULL} and checks for \code{length>0}
  rather than \code{missing()}, so it's easier to deal with
  non-applicable parameters.  But when \pkg{grid} is in effect, the
  preferred function to use is \code{rlegendg}, which calls the
  \pkg{lattice} \code{\link[lattice]{draw.key}} function.
}
\usage{
rlegend(x, y, legend, fill, col = "black", lty = NULL, lwd = NULL,
        pch = NULL, angle = NULL, density = NULL, bty = "o",
        bg = par("bg"), pt.bg = NA, cex = 1, xjust = 0, yjust = 1,
        x.intersp = 1, y.intersp = 1, adj = 0, text.width = NULL,
        merge = do.lines && has.pch, trace = FALSE, ncol = 1,
        horiz = FALSE, plot = TRUE, grid = FALSE, \dots)

rlegendg(x, y, legend, col=pr$col[1], lty=NULL,
         lwd=NULL, pch=NULL, cex=pr$cex[1], other=NULL) 
}
\arguments{
  \item{x,y,legend,fill,col,lty,lwd,pch,angle,density,bty,bg,pt.bg,cex,xjust,
    yjust,x.intersp,y.intersp,adj,text.width,merge,trace,ncol,horiz}{
    see \code{\link{legend}}
  }
  \item{plot}{set to \code{FALSE} to suppress drawing the legend.  This
    is used the compute the size needed for when the legend is drawn
    with a later call to \code{rlegend}.
  }
  \item{grid}{set to \code{TRUE} if the \pkg{grid} package is in effect}
  \item{\dots}{see \code{\link{legend}}}
  \item{other}{
    a list containing other arguments to pass to
    \code{draw.key}.  See the help file for \code{\link[lattice]{xyplot}}.
  }
}
\value{
  a list with elements \code{rect} and \code{text}.  \code{rect} has
  elements \code{w, h, left, top} with size/position information.
}
\author{Frank Harrell and R-Core}
\seealso{
  \code{\link{legend}}, \code{\link[lattice]{draw.key}}, \code{\link[lattice]{xyplot}}
}
\keyword{aplot}
