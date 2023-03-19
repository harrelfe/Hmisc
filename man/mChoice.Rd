\name{mChoice}
\alias{mChoice}
\alias{format.mChoice}
\alias{print.mChoice}
\alias{summary.mChoice}
\alias{as.character.mChoice}
\alias{as.double.mChoice}
\alias{inmChoice}
\alias{inmChoicelike}
\alias{nmChoice}
\alias{match.mChoice}
\alias{[.mChoice}
\alias{print.summary.mChoice}
\alias{is.mChoice}
\alias{Math.mChoice}
\alias{Ops.mChoice}
\alias{Summary.mChoice}
\title{Methods for Storing and Analyzing Multiple Choice Variables}
\description{
  \code{mChoice} is a function that is useful for grouping 
  variables that represent
  individual choices on a multiple choice question.  These choices are
  typically factor or character values but may be of any type.  Levels
  of component factor variables need not be the same; all unique levels
  (or unique character values) are collected over all of the multiple
  variables.  Then a new character vector is formed with integer choice
  numbers separated by semicolons.  Optimally, a database system would
  have exported the semicolon-separated character strings with a
  \code{levels} attribute containing strings defining value labels
  corresponding to the integer choice numbers.  \code{mChoice} is a
  function for creating a multiple-choice variable after the fact.
  \code{mChoice} variables are explicitly handed by the \code{describe}
  and \code{summary.formula} functions. \code{NA}s or blanks in input
  variables are ignored. 

  \code{format.mChoice} will convert the multiple choice representation
  to text form by substituting \code{levels} for integer codes.
  \code{as.double.mChoice} converts the \code{mChoice} object to a
  binary numeric matrix, one column per used level (or all levels of
  \code{drop=FALSE}.  This is called by
  the user by invoking \code{as.numeric}.  There is a
  \code{print} method and a \code{summary} method, and a \code{print}
  method for the \code{summary.mChoice} object.  The \code{summary}
  method computes frequencies of all two-way choice combinations, the
  frequencies of the top 5 combinations, information about which other
  choices are present when each given choice is present, and the
  frequency distribution of the number of choices per observation.  This
  \code{summary} output is used in the \code{describe} function.  The
  \code{print} method returns an html character string if
  \code{options(prType='html')} is in effect if \code{render=FALSE} or
  renders the html otherwise.  This is used by \code{print.describe} and
  is most effective when \code{short=TRUE} is specified to \code{summary}.

  \code{in.mChoice} creates a logical vector the same length as \code{x}
  whose elements are \code{TRUE} when the observation in \code{x}
  contains at least one of the codes or value labels in the second
  argument.

  \code{match.mChoice} creates an integer vector of the indexes of all
  elements in \code{table} which contain any of the speicified levels

	\code{nmChoice} returns an integer vector of the number of choices
	that were made

  \code{is.mChoice} returns \code{TRUE} is the argument is a multiple
  choice variable.
}
\usage{
mChoice(\dots, label='',
        sort.levels=c('original','alphabetic'), 
        add.none=FALSE, drop=TRUE, ignoreNA=TRUE)

\method{format}{mChoice}(x, minlength=NULL, sep=";", \dots)

\method{as.double}{mChoice}(x, drop=FALSE, ...)

\method{print}{mChoice}(x, quote=FALSE, max.levels=NULL,
       width=getOption("width"), ...)

\method{as.character}{mChoice}(x, ...)

\method{summary}{mChoice}(object, ncombos=5, minlength=NULL,
  drop=TRUE, short=FALSE, ...)

\method{print}{summary.mChoice}(x, prlabel=TRUE, render=TRUE, ...)

\method{[}{mChoice}(x, ..., drop=FALSE)

match.mChoice(x, table, nomatch=NA, incomparables=FALSE)

inmChoice(x, values, condition=c('any', 'all'))

inmChoicelike(x, values, condition=c('any', 'all'),
              ignore.case=FALSE, fixed=FALSE)

nmChoice(object)

is.mChoice(x)

\method{Summary}{mChoice}(..., na.rm)
}
\arguments{
  \item{na.rm}{
    Logical: remove \code{NA}'s from data
  }
  \item{table}{
    a vector (mChoice) of values to be matched against.
  }
  \item{nomatch}{
    value to return if a value for \code{x} does not exist in
    \code{table}.
  }
  \item{incomparables}{
    logical whether incomparable values should be compaired.
  }
  \item{...}{
    a series of vectors
  }
  \item{label}{
    a character string \code{label} attribute to attach to the matrix created
    by \code{mChoice}
  }
  \item{sort.levels}{
    set \code{sort.levels="alphabetic"} to sort the columns of the matrix
    created by \code{mChoice} alphabetically by category rather than by the
    original order of levels in component factor variables (if there were
    any input variables that were factors)
  }
  \item{add.none}{
    Set \code{add.none} to \code{TRUE} to make a new category
    \code{'none'} if it doesn't already exist and if there is an
    observations with no choices selected.
  }
  \item{drop}{
    set \code{drop=FALSE} to keep unused factor levels as columns of the matrix
    produced by \code{mChoice}
  }
  \item{ignoreNA}{set to \code{FALSE} to keep any \code{NA}s present in
data as a real level.  Prior to Hmisc 4.7-2 \code{FALSE} was the
default.}
  \item{x}{
    an object of class \code{"mchoice"} such as that created by
    \code{mChoice}.  For \code{is.mChoice} is any object.
  }
  \item{object}{
    an object of class \code{"mchoice"} such as that created by
    \code{mChoice}
  }
  \item{ncombos}{
    maximum number of combos.
  }
  \item{width}{
    With of a line of text to be formated
  }
  \item{quote}{
    quote the output
  }
  \item{max.levels}{max levels to be displayed}
  \item{minlength}{
    By default no abbreviation of levels is done in
    \code{format} and \code{summary}.  Specify a positive integer to use
    abbreviation in those functions.  See \code{\link{abbreviate}}.
  }
  \item{short}{set to \code{TRUE} to have \code{summary.mChoice} use
integer choice numbers in its tables, and to print the choice level
definitions at the top}
  \item{sep}{character to use to separate levels when formatting}
  \item{prlabel}{
    set to \code{FALSE} to keep
    \code{print.summary.mChoice} from printing the variable label and
    number of unique values.  Ignore for html output.
  }
  \item{render}{applies of \code{options(prType='html')} is in
  effect. Set to \code{FALSE} to return the html text instead of
  rendering the html.}
  \item{values}{
    a scalar or vector.  If \code{values} is integer, it is
    the choice codes, and if it is a character vector, it is assumed to
    be value labels.  For \code{inmChoicelike} \code{values} must be
    character strings which are pieces of choice labels.
  }
  \item{condition}{set to \code{'all'} for \code{inmChoice} to require
that all choices in \code{values} be present instead of the default of
any of them present.}
  \item{ignore.case}{set to \code{TRUE} to have \code{inmChoicelike}
ignore case in the data when matching on \code{values}}
  \item{fixed}{see \code{grep}}
}
\value{
  \code{mChoice} returns a character vector of class \code{"mChoice"}
  plus attributes \code{"levels"} and \code{"label"}.
  \code{summary.mChoice} returns an object of class
  \code{"summary.mChoice"}.  \code{inmChoice} and \code{inmChoicelike}
  return a logical vector.
  \code{format.mChoice} returns a character vector, and
  \code{as.double.mChoice} returns a binary numeric matrix.
  \code{nmChoice} returns an integer vector.
  \code{print.summary.mChoice} returns an html character string if
  \code{options(prType='html')} is in effect.
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
\seealso{
  \code{\link{label}}, \code{\link{combplotp}}
}
\examples{
options(digits=3)
set.seed(3)
n <- 20
sex <- factor(sample(c("m","f"), n, rep=TRUE))
age <- rnorm(n, 50, 5)
treatment <- factor(sample(c("Drug","Placebo"), n, rep=TRUE))


# Generate a 3-choice variable; each of 3 variables has 5 possible levels
symp <- c('Headache','Stomach Ache','Hangnail',
          'Muscle Ache','Depressed')
symptom1 <- sample(symp, n, TRUE)
symptom2 <- sample(symp, n, TRUE)
symptom3 <- sample(symp, n, TRUE)
cbind(symptom1, symptom2, symptom3)[1:5,]
Symptoms <- mChoice(symptom1, symptom2, symptom3, label='Primary Symptoms')
Symptoms
print(Symptoms, long=TRUE)
format(Symptoms[1:5])
inmChoice(Symptoms,'Headache')
inmChoicelike(Symptoms, 'head', ignore.case=TRUE)
levels(Symptoms)
inmChoice(Symptoms, 3)
# Find all subjects with either of two symptoms
inmChoice(Symptoms, c('Headache','Hangnail'))
# Note: In this example, some subjects have the same symptom checked
# multiple times; in practice these redundant selections would be NAs
# mChoice will ignore these redundant selections
# Find all subjects with both symptoms
inmChoice(Symptoms, c('Headache', 'Hangnail'), condition='all')

meanage <- N <- numeric(5)
for(j in 1:5) {
 meanage[j] <- mean(age[inmChoice(Symptoms,j)])
 N[j] <- sum(inmChoice(Symptoms,j))
}
names(meanage) <- names(N) <- levels(Symptoms)
meanage
N

# Manually compute mean age for 2 symptoms
mean(age[symptom1=='Headache' | symptom2=='Headache' | symptom3=='Headache'])
mean(age[symptom1=='Hangnail' | symptom2=='Hangnail' | symptom3=='Hangnail'])

summary(Symptoms)

#Frequency table sex*treatment, sex*Symptoms
summary(sex ~ treatment + Symptoms, fun=table)
# Check:
ma <- inmChoice(Symptoms, 'Muscle Ache')
table(sex[ma])

# could also do:
# summary(sex ~ treatment + mChoice(symptom1,symptom2,symptom3), fun=table)

#Compute mean age, separately by 3 variables
summary(age ~ sex + treatment + Symptoms)


summary(age ~ sex + treatment + Symptoms, method="cross")

f <- summary(treatment ~ age + sex + Symptoms, method="reverse", test=TRUE)
f
# trio of numbers represent 25th, 50th, 75th percentile
print(f, long=TRUE)
}
\keyword{category}
\keyword{manip}
\concept{multiple choice}
