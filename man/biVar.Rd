\name{biVar}
\alias{biVar}
\alias{print.biVar}
\alias{plot.biVar}
\alias{spearman2}
\alias{spearman2.default}
\alias{spearman2.formula}
\alias{spearman}
\alias{spearman.test}
\alias{chiSquare}
\title{Bivariate Summaries Computed Separately by a Series of Predictors}
\description{
  \code{biVar} is a generic function that accepts a formula and usual
  \code{data}, \code{subset}, and \code{na.action} parameters plus a
  list \code{statinfo} that specifies a function of two variables to
  compute along with information about labeling results for printing and
  plotting.  The function is called separately with each right hand side
  variable and the same left hand variable.  The result is a matrix of
  bivariate statistics and the \code{statinfo} list that drives printing
  and plotting.  The plot method draws a dot plot with x-axis values by
  default sorted in order of one of the statistics computed by the function.

  \code{spearman2} computes the square of Spearman's rho rank correlation
  and a generalization of it in which \code{x} can relate
  non-monotonically to \code{y}.  This is done by computing the Spearman
  multiple rho-squared between \code{(rank(x), rank(x)^2)} and \code{y}.
  When \code{x} is categorical, a different kind of Spearman correlation
  used in the Kruskal-Wallis test is computed (and \code{spearman2} can do
  the Kruskal-Wallis test).  This is done by computing the ordinary
  multiple \code{R^2} between \code{k-1} dummy variables and
  \code{rank(y)}, where \code{x} has \code{k} categories.  \code{x} can
  also be a formula, in which case each predictor is correlated separately
  with \code{y}, using non-missing observations for that predictor.
  \code{biVar} is used to do the looping and bookkeeping.  By default the
  plot shows the adjusted \code{rho^2}, using the same formula used for
  the ordinary adjusted \code{R^2}.  The \code{F} test uses the unadjusted
  R2.

  \code{spearman} computes Spearman's rho on non-missing values of two
  variables.  \code{spearman.test} is a simple version of
  \code{spearman2.default}.

  \code{chiSquare} is set up like \code{spearman2} except it is intended
  for a categorical response variable.  Separate Pearson chi-square tests
  are done for each predictor, with optional collapsing of infrequent
  categories.  Numeric predictors having more than \code{g} levels are
  categorized into \code{g} quantile groups.  \code{chiSquare} uses
  \code{biVar}.
}
\usage{
biVar(formula, statinfo, data=NULL, subset=NULL,
      na.action=na.retain, exclude.imputed=TRUE, ...)

\method{print}{biVar}(x, ...)

\method{plot}{biVar}(x, what=info$defaultwhat,
                       sort.=TRUE, main, xlab,
                       vnames=c('names','labels'), ...)

spearman2(x, ...)

\method{spearman2}{default}(x, y, p=1, minlev=0, na.rm=TRUE, exclude.imputed=na.rm, ...)

\method{spearman2}{formula}(formula, data=NULL,
          subset, na.action=na.retain, exclude.imputed=TRUE, ...)

spearman(x, y)

spearman.test(x, y, p=1)

chiSquare(formula, data=NULL, subset=NULL, na.action=na.retain,
          exclude.imputed=TRUE, ...)
}
\arguments{
  \item{formula}{a formula with a single left side variable}
  \item{statinfo}{see \code{spearman2.formula} or \code{chiSquare} code}
  \item{data, subset, na.action}{
    the usual options for models.  Default for \code{na.action} is to retain
    all values, NA or not, so that NAs can be deleted in only a pairwise
    fashion.
  }
  \item{exclude.imputed}{
    set to \code{FALSE} to include imputed values (created by
    \code{impute}) in the calculations.
  }
  \item{...}{other arguments that are passed to the function used to
    compute the bivariate statistics or to \code{dotchart3} for
    \code{plot}.
  }
  \item{na.rm}{logical; delete NA values?}
  \item{x}{
	a numeric matrix with at least 5 rows and at least 2 columns (if
	\code{y} is absent).  For \code{spearman2}, the first argument may
	be a vector of any type, including character or factor.  The first
	argument may also be a formula, in which case all predictors are
	correlated individually with 
	the response variable.  \code{x} may be a formula for \code{spearman2}
	in which case \code{spearman2.formula} is invoked.  Each
	predictor in the right hand side of the formula is separately correlated
	with the response variable.  For \code{print} or \code{plot}, \code{x}
	is an object produced by \code{biVar}.  For \code{spearman} and
	\code{spearman.test} \code{x} is a numeric vector, as is \code{y}.  For
	\code{chiSquare}, \code{x} is a formula.
  }
%  \item{type}{
%	specifies the type of correlations to compute.  Spearman correlations
%	are the Pearson linear correlations computed on the ranks of non-missing
%	elements, using midranks for ties.
%  }
  \item{y}{
	a numeric vector
  }
  \item{p}{
	for numeric variables, specifies the order of the Spearman \code{rho^2} to
	use.  The default is \code{p=1} to compute the ordinary
	\code{rho^2}.  Use \code{p=2} to compute the quadratic rank
	generalization to allow non-monotonicity.  \code{p} is ignored for
	categorical predictors.
  }
  \item{minlev}{
	minimum relative frequency that a level of a categorical predictor
	should have before it is pooled with other categories (see
	\code{combine.levels}) in \code{spearman2} and \code{chiSquare} (in
	which case it also applies to the response).  The default,
	\code{minlev=0} causes no pooling.
  }
  \item{what}{
	specifies which statistic to plot.  Possibilities include the
	column names that appear with the print method is used.
  }
  \item{sort.}{
	set \code{sort.=FALSE} to suppress sorting variables by the
	statistic being plotted
  }
  \item{main}{
	main title for plot.  Default title shows the name of the response
	variable.
  }
  \item{xlab}{
	x-axis label.  Default constructed from \code{what}.
  }
  \item{vnames}{
	set to \code{"labels"} to use variable labels in place of names for
	plotting.  If a variable does not have a label the name is always
	used.}
%  \item{g}{number of quantile groups into which to categorize continuous
%	predictors having more than \code{g} unique values, for \code{chiSquare}}
}
\value{
  \code{spearman2.default} (the
  function that is called for a single \code{x}, i.e., when there is no
  formula) returns a vector of statistics for the variable.
  \code{biVar}, \code{spearman2.formula}, and \code{chiSquare} return a
  matrix with rows corresponding to predictors.
}
\details{
  Uses midranks in case of ties, as described by Hollander and Wolfe.
  P-values for Spearman, Wilcoxon, or Kruskal-Wallis tests are
  approximated by using the \code{t} or \code{F} distributions.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University
\cr
\email{fh@fharrell.com}
}
\references{
Hollander M. and Wolfe D.A. (1973).  Nonparametric Statistical Methods.
New York: Wiley.

Press WH, Flannery BP, Teukolsky SA, Vetterling, WT (1988): Numerical
Recipes in C.  Cambridge: Cambridge University Press.
}
\seealso{
\code{\link{combine.levels}},
\code{\link{varclus}}, \code{\link{dotchart3}}, \code{\link{impute}},
\code{\link{chisq.test}}, \code{\link{cut2}}.
}
\examples{
x <- c(-2, -1, 0, 1, 2)
y <- c(4,   1, 0, 1, 4)
z <- c(1,   2, 3, 4, NA)
v <- c(1,   2, 3, 4, 5)

spearman2(x, y)
plot(spearman2(z ~ x + y + v, p=2))

f <- chiSquare(z ~ x + y + v)
f
}
\keyword{nonparametric}
\keyword{htest}
\keyword{category}
