\name{popower}
\alias{multEventChart}
\alias{popower}
\alias{posamsize}
\alias{print.popower}
\alias{print.posamsize}
\alias{pomodm}
\alias{simPOcuts}
\alias{propsPO}
\alias{propsTrans}
\title{Power and Sample Size for Ordinal Response}
\description{
\code{popower} computes the power for a two-tailed two sample comparison
of ordinal outcomes under the proportional odds ordinal logistic
model.  The power is the same as that of the Wilcoxon test but with
ties handled properly.  \code{posamsize} computes the total sample size
needed to achieve a given power.  Both functions compute the efficiency
of the design compared with a design in which the response variable
is continuous.  \code{print} methods exist for both functions.  Any of the
input arguments may be vectors, in which case a vector of powers or
sample sizes is returned.  These functions use the methods of
Whitehead (1993).

\code{pomodm} is a function that assists in translating odds ratios to
differences in mean or median on the original scale.

\code{simPOcuts} simulates simple unadjusted two-group comparisons under
a PO model to demonstrate the natural sampling variability that causes
estimated odds ratios to vary over cutoffs of Y.

\code{propsPO} uses \code{ggplot2} to plot a stacked bar chart of
proportions stratified by a grouping variable (and optionally a stratification variable), with an optional
additional graph showing what the proportions would be had proportional
odds held and an odds ratio was applied to the proportions in a
reference group.  If the result is passed to \code{ggplotly}, customized
tooltip hover text will appear.

\code{propsTrans} uses \code{ggplot2} to plot all successive
transition proportions.  \code{formula} has the state variable on the
left hand side, the first right-hand variable is time, and the second
right-hand variable is a subject ID variable.\

\code{multEventChart} uses \code{ggplot2} to plot event charts
showing state transitions, account for absorbing states/events.  It is
based on code written by Lucy D'Agostino McGowan posted at \url{https://livefreeordichotomize.com/posts/2020-05-21-survival-model-detective-1/}.

}
\usage{
popower(p, odds.ratio, n, n1, n2, alpha=0.05)
\method{print}{popower}(x, \dots)
posamsize(p, odds.ratio, fraction=.5, alpha=0.05, power=0.8)
\method{print}{posamsize}(x, \dots)
pomodm(x=NULL, p, odds.ratio=1)
simPOcuts(n, nsim=10, odds.ratio=1, p)
propsPO(formula, odds.ratio=NULL, ref=NULL, data=NULL, ncol=NULL, nrow=NULL )
propsTrans(formula, data=NULL, labels=NULL, arrow='\u2794',
           maxsize=12, ncol=NULL, nrow=NULL)
multEventChart(formula, data=NULL, absorb=NULL, sortbylast=FALSE,
   colorTitle=label(y), eventTitle='Event',
   palette='OrRd',
   eventSymbols=c(15, 5, 1:4, 6:10),
   timeInc=min(diff(unique(x))/2))
}
\arguments{
\item{p}{
a vector of marginal cell probabilities which must add up to one.
For \code{popower} and \code{posamsize}, The \code{i}th element specifies the probability that a patient will be
in response level \code{i}, averaged over the two treatment groups.  For
\code{pomodm} and \code{simPOcuts}, \code{p} is the vector of cell
probabilities to be translated under a given odds ratio.  For
\code{simPOcuts}, if \code{p} has names, those names are taken as the
ordered distinct Y-values.  Otherwise Y-values are taken as the integers
1, 2, ... up to the length of \code{p}.
}
\item{odds.ratio}{
the odds ratio to be able to detect.  It doesn't
matter which group is in the numerator.  For \code{propsPO},
\code{odds.ratio} is a function of the grouping (right hand side)
variable value.  The value of the function specifies the odds ratio to
apply to the refernce group to get all other group's expected proportions
were proportional odds to hold against the first group.  Normally the
function should return 1.0 when its \code{x} argument corresponds to the
\code{ref} group.  For \code{pomodm} and \code{simPOcuts} is the odds
ratio to apply to convert the given cell probabilities.}
\item{n}{
total sample size for \code{popower}.  You must specify either \code{n} or
\code{n1} and \code{n2}.  If you specify \code{n}, \code{n1} and
\code{n2} are set to \code{n/2}. For \code{simPOcuts} is a single number
equal to the combined sample sizes of two groups.
}
\item{n1}{for \code{popower}, the number of subjects in treatment group 1}
\item{n2}{for \code{popower}, the number of subjects in group 2}
\item{nsim}{number of simulated studies to create by \code{simPOcuts}}
\item{alpha}{type I error}
\item{x}{an object created by \code{popower} or \code{posamsize}, or a
	vector of data values given to \code{pomodm} that corresponds to the
	vector \code{p} of probabilities.  If \code{x} is omitted for
	\code{pomodm}, the \code{odds.ratio} will be applied and the new
	vector of individual probabilities will be returned.  Otherwise if
	\code{x} is given to \code{pomodm}, a 2-vector with the mean and
	median \code{x} after applying the odds ratio is returned.}
\item{fraction}{
for \code{posamsize}, the fraction of subjects that will be allocated to group 1
}
\item{power}{
for \code{posamsize}, the desired power (default is 0.8)
}
\item{formula}{an R formula expressure for \code{proposPO} where the
	outcome categorical variable is on the left hand side and the grouping
	variable is on the right.  It is assumed that the left hand variable is
	either already a factor or will have its levels in the right order for
	an ordinal model when it is converted to factor.  For
	\code{multEventChart} the left hand variable is a categorial status
	variable, the first right hand side variable represents time, and the
	second right side variable is a unique subject ID.  One line is
	produced per subject.}
\item{ref}{for \code{propsPO} specifies the reference group (value of
	the right hand side \code{formula} variable) to use in computing
	proportions on which too translate proportions in other groups, under
	the proportional odds assumption.}
\item{data}{a data frame or \code{data.table}}
\item{labels}{for \code{propsTrans} is an optional character vector
	corresponding to y=1,2,3,... that is used to construct \code{plotly}
	hovertext as a \code{label} attribute in the \code{ggplot2}
	aesthetic.  Used with y is integer on axes but you want long labels in
	hovertext.}
\item{arrow}{character to use as the arrow symbol for transitions in
	\code{propsTrans.  The default is the dingbats heavy wide-headed
		rightwards arror.}}
\item{nrow,ncol}{see \code{\link[ggplot2]{facet_wrap}}}
\item{maxsize}{maximum symbol size}
\item{\dots}{unused}
\item{absorb}{character vector specifying the subset of levels of the
	left hand side variable that are absorbing states such as death or
	hospital discharge}
\item{sortbylast}{set to \code{TRUE} to sort the subjects by the
	severity of the status at the last time point}
\item{colorTitle}{label for legend for status}
\item{eventTitle}{label for legend for \code{absorb}}
\item{palette}{a single character string specifying the
	\code{\link[ggplot2]{scale_fill_brewer}} color palette}
\item{eventSymbols}{vector of symbol codes.  Default for first two
	symbols is a solid square and an open diamond.}
\item{timeInc}{time increment for the x-axis.  Default is 1/2 the
	shortest gap between any two distincttimes in the data.}
}
\value{
a list containing \code{power}, \code{eff} (relative efficiency), and
\code{approx.se} (approximate standard error of log odds ratio) for
\code{popower}, or containing \code{n} and \code{eff} for \code{posamsize}.
}
\author{
Frank Harrell
\cr
Department of Biostatistics
\cr
Vanderbilt University School of Medicine
\cr
\email{fh@fharrell.com}
}
\references{
Whitehead J (1993): Sample size calculations for ordered categorical
data.  Stat in Med 12:2257--2271.


Julious SA, Campbell MJ (1996): Letter to the Editor.  Stat in Med 15:
1065--1066.  Shows accuracy of formula for binary response case.
}
\seealso{
\code{\link{simRegOrd}}, \code{\link{bpower}}, \code{\link{cpower}}, \code{\link[rms]{impactPO}}
}
\examples{
# For a study of back pain (none, mild, moderate, severe) here are the
# expected proportions (averaged over 2 treatments) that will be in
# each of the 4 categories:


p <- c(.1,.2,.4,.3)
popower(p, 1.2, 1000)   # OR=1.2, total n=1000
posamsize(p, 1.2)
popower(p, 1.2, 3148)
# If p was the vector of probabilities for group 1, here's how to
# compute the average over the two groups:
# p2   <- pomodm(p=p, odds.ratio=1.2)
# pavg <- (p + p2) / 2

# Compare power to test for proportions for binary case,
# proportion of events in control group of 0.1
p <- 0.1; or <- 0.85; n <- 4000
popower(c(1 - p, p), or, n)    # 0.338
bpower(p, odds.ratio=or, n=n)  # 0.320
# Add more categories, starting with 0.1 in middle
p <- c(.8, .1, .1)
popower(p, or, n)   # 0.543
p <- c(.7, .1, .1, .1)
popower(p, or, n)   # 0.67
# Continuous scale with final level have prob. 0.1
p <- c(rep(1 / n, 0.9 * n), 0.1)
popower(p, or, n)   # 0.843

# Compute the mean and median x after shifting the probability
# distribution by an odds ratio under the proportional odds model
x <- 1 : 5
p <- c(.05, .2, .2, .3, .25)
# For comparison make up a sample that looks like this
X <- rep(1 : 5, 20 * p)
c(mean=mean(X), median=median(X))
pomodm(x, p, odds.ratio=1)  # still have to figure out the right median
pomodm(x, p, odds.ratio=0.5)

# Show variation of odds ratios over possible cutoffs of Y even when PO
# truly holds.  Run 5 simulations for a total sample size of 300.
# The two groups have 150 subjects each.
s <- simPOcuts(300, nsim=5, odds.ratio=2, p=p)
round(s, 2)

# An ordinal outcome with levels a, b, c, d, e is measured at 3 times
# Show the proportion of values in each outcome category stratified by
# time.  Then compute what the proportions would be had the proportions
# at times 2 and 3 been the proportions at time 1 modified by two odds ratios 

set.seed(1)
d   <- expand.grid(time=1:3, reps=1:30)
d$y <- sample(letters[1:5], nrow(d), replace=TRUE)
propsPO(y ~ time, data=d, odds.ratio=function(time) c(1, 2, 4)[time])
# To show with plotly, save previous result as object p and then:
# plotly::ggplotly(p, tooltip='label')

# Add a stratification variable and don't consider an odds ratio
d   <- expand.grid(time=1:5, sex=c('female', 'male'), reps=1:30)
d$y <- sample(letters[1:5], nrow(d), replace=TRUE)
propsPO(y ~ time + sex, data=d)  # may add nrow= or ncol=

# Show all successive transition proportion matrices
d   <- expand.grid(id=1:30, time=1:10)
d$state <- sample(LETTERS[1:4], nrow(d), replace=TRUE)
propsTrans(state ~ time + id, data=d)

pt1 <- data.frame(pt=1, day=0:3,
   status=c('well', 'well', 'sick', 'very sick'))
pt2 <- data.frame(pt=2, day=c(1,2,4,6),
   status=c('sick', 'very sick', 'coma', 'death'))
pt3 <- data.frame(pt=3, day=1:5,
   status=c('sick', 'very sick', 'sick', 'very sick', 'discharged'))
pt4 <- data.frame(pt=4, day=c(1:4, 10),
   status=c('well', 'sick', 'very sick', 'well', 'discharged'))
d <- rbind(pt1, pt2, pt3, pt4)
d$status <- factor(d$status, c('discharged', 'well', 'sick',
                               'very sick', 'coma', 'death'))
label(d$day) <- 'Day'
require(ggplot2)
multEventChart(status ~ day + pt, data=d,
               absorb=c('death', 'discharged'),
               colorTitle='Status', sortbylast=TRUE) +
               theme_classic() +
               theme(legend.position='bottom')
}
\keyword{htest}
\keyword{category}
\concept{power}
\concept{study design}
\concept{ordinal logistic model}
\concept{ordinal response}
\concept{proportional odds model}
