\name{data.frame.create.modify.check}
\alias{data.frame.create.modify.check}
\title{
  Tips for Creating, Modifying, and Checking Data Frames
}
\description{
  This help file contains a template for importing data to create an R
  data frame, correcting some problems resulting from the import and
  making the data frame be stored more efficiently, modifying the data
  frame (including better annotating it and changing the names of some
  of its variables), and checking and inspecting the data frame for
  reasonableness of the values of its variables and to describe patterns
  of missing data.  Various built-in functions and functions in the
  Hmisc library are used.  At the end some methods for creating data
  frames \dQuote{from scratch} within \R are presented.
  

  The examples below attempt to clarify the separation of operations
  that are done on a data frame as a whole, operations that are done on
  a small subset of its variables without attaching the whole data
  frame, and operations that are done on many variables after attaching
  the data frame in search position one.  It also tries to clarify that
  for analyzing several separate variables using \R commands that do not
  support a \code{data} argument, it is helpful to attach the data frame
  in a search position later than position one.

  It is often useful to create, modify, and process datasets in the
  following order.
  \enumerate{
    \item{
      Import external data into a data frame (if the raw data do not
      contain column names, provide these during the import if possible)
    }
    \item{
      Make global changes to a data frame (e.g., changing variable
      names)
    }
    \item{
      Change attributes or values of variables within a data frame
    }
    \item{
      Do analyses involving the whole data frame (without attaching it)\cr
      (Data frame still in .Data)
    }
    \item{
      Do analyses of individual variables (after attaching the data
      frame in search position two or later)
    }
  }
}
\details{
  The examples below use the \code{FEV} dataset from
  \cite{Rosner 1995}. Almost any dataset would do.  The jcetable data
  are taken from \cite{Galobardes, etal.}
  
  Presently, giving a variable the \code{"units"} attribute (using the
  \pkg{Hmisc} \code{\link{units}} function) only benefits the
  \pkg{Hmisc} \code{\link{describe}} function and the \pkg{rms}
  library's version of the \code{link[rms]{Surv}} function.  Variables
  labels defined with the Hmisc \code{\link{label}} function are used by
  \code{\link{describe}}, \code{\link{summary.formula}},  and many of
  the plotting functions in \pkg{Hmisc} and \pkg{rms}.
}
\references{
  Alzola CF, Harrell FE (2006):
  \emph{An Introduction to S and the Hmisc and Design Libraries.}
  Chapters 3 and 4,
  \url{https://hbiostat.org/R/doc/sintro.pdf}.

  Galobardes, et al. (1998), \emph{J Clin Epi} 51:875-881.

  Rosner B (1995): \emph{Fundamentals of Biostatistics, 4th Edition.  }
  New York: Duxbury Press.
}
\seealso{
  \code{\link{scan}}, \code{\link{read.table}},
  \code{\link{cleanup.import}}, \code{\link{sas.get}},
  \code{\link{data.frame}}, \code{\link{attach}}, \code{\link{detach}},
  \code{\link{describe}}, \code{\link{datadensity}},
  \code{\link{plot.data.frame}}, \code{\link{hist.data.frame}},
  \code{\link{naclus}}, \code{\link{factor}}, \code{\link{label}},
  \code{\link{units}}, \code{\link{names}}, \code{\link{expand.grid}},
  \code{\link{summary.formula}}, \code{\link{summary.data.frame}},
  \code{\link{casefold}}, \code{\link{edit}}, \code{\link{page}},
  \code{\link{plot.data.frame}}, \code{\link{Cs}},
  \code{\link{combine.levels}},\code{\link{upData}}
}
\examples{
\dontrun{
# First, we do steps that create or manipulate the data
# frame in its entirety.  For S-Plus, these are done with
# .Data in search position one (the default at the
# start of the session).
#
# -----------------------------------------------------------------------
# Step 1: Create initial draft of data frame
# 
# We usually begin by importing a dataset from
# # another application.  ASCII files may be imported
# using the scan and read.table functions.  SAS
# datasets may be imported using the Hmisc sas.get
# function (which will carry more attributes from
# SAS than using File \dots  Import) from the GUI
# menus.  But for most applications (especially
# Excel), File \dots Import will suffice.  If using
# the GUI, it is often best to provide variable
# names during the import process, using the Options
# tab, rather than renaming all fields later Of
# course, if the data to be imported already have
# field names (e.g., in Excel), let S use those
# automatically.  If using S-Plus, you can use a
# command to execute File \dots  Import, e.g.:


import.data(FileName = "/windows/temp/fev.asc",
            FileType = "ASCII", DataFrame = "FEV")


# Here we name the new data frame FEV rather than
# fev, because we wanted to distinguish a variable
# in the data frame named fev from the data frame
# name.  For S-Plus the command will look
# instead like the following:


FEV <- importData("/tmp/fev.asc")




# -----------------------------------------------------------------------
# Step 2: Clean up data frame / make it be more
# efficiently stored
# 
# Unless using sas.get to import your dataset
# (sas.get already stores data efficiently), it is
# usually a good idea to run the data frame through
# the Hmisc cleanup.import function to change
# numeric variables that are always whole numbers to
# be stored as integers, the remaining numerics to
# single precision, strange values from Excel to
# NAs, and character variables that always contain
# legal numeric values to numeric variables.
# cleanup.import typically halves the size of the
# data frame.  If you do not specify any parameters
# to cleanup.import, the function assumes that no
# numeric variable needs more than 7 significant
# digits of precision, so all non-integer-valued
# variables will be converted to single precision.


FEV <- cleanup.import(FEV)




# -----------------------------------------------------------------------
# Step 3: Make global changes to the data frame
# 
# A data frame has attributes that are "external" to
# its variables.  There are the vector of its
# variable names ("names" attribute), the
# observation identifiers ("row.names"), and the
# "class" (whose value is "data.frame").  The
# "names" attribute is the one most commonly in need
# of modification.  If we had wanted to change all
# the variable names to lower case, we could have
# specified lowernames=TRUE to the cleanup.import
# invocation above, or type


names(FEV) <- casefold(names(FEV))


# The upData function can also be used to change
# variable names in two ways (see below).
# To change names in a non-systematic way we use
# other options.  Under Windows/NT the most
# straigtforward approach is to change the names
# interactively.  Click on the data frame in the
# left panel of the Object Browser, then in the
# right pane click twice (slowly) on a variable.
# Use the left arrow and other keys to edit the
# name.  Click outside that name field to commit the
# change.  You can also rename columns while in a
# Data Sheet.  To instead use programming commands
# to change names, use something like:


names(FEV)[6] <- 'smoke'   # assumes you know the positions!  
names(FEV)[names(FEV)=='smoking'] <- 'smoke' 
names(FEV) <- edit(names(FEV))


# The last example is useful if you are changing
# many names.  But none of the interactive
# approaches such as edit() are handy if you will be
# re-importing the dataset after it is updated in
# its original application.  This problem can be
# addressed by saving the new names in a permanent
# vector in .Data:


new.names <- names(FEV)


# Then if the data are re-imported, you can type


names(FEV) <- new.names


# to rename the variables.




# -----------------------------------------------------------------------
# Step 4: Delete unneeded variables
# 
# To delete some of the variables, you can
# right-click on variable names in the Object
# Browser's right pane, then select Delete.  You can
# also set variables to have NULL values, which
# causes the system to delete them.  We don't need
# to delete any variables from FEV but suppose we
# did need to delete some from mydframe.


mydframe$x1 <- NULL 
mydframe$x2 <- NULL
mydframe[c('age','sex')] <- NULL   # delete 2 variables 
mydframe[Cs(age,sex)]    <- NULL   # same thing


# The last example uses the Hmisc short-cut quoting
# function Cs.  See also the drop parameter to upData.




# -----------------------------------------------------------------------
# Step 5: Make changes to individual variables
#         within the data frame
# 
# After importing data, the resulting variables are
# seldom self - documenting, so we commonly need to
# change or enhance attributes of individual
# variables within the data frame.
# 
# If you are only changing a few variables, it is
# efficient to change them directly without
# attaching the entire data frame.


FEV$sex   <- factor(FEV$sex,   0:1, c('female','male')) 
FEV$smoke <- factor(FEV$smoke, 0:1, 
                    c('non-current smoker','current smoker')) 
units(FEV$age)    <- 'years'
units(FEV$fev)    <- 'L' 
label(FEV$fev)    <- 'Forced Expiratory Volume' 
units(FEV$height) <- 'inches'


# When changing more than one or two variables it is
# more convenient change the data frame using the
# Hmisc upData function.


FEV2 <- upData(FEV,
  rename=c(smoking='smoke'), 
  # omit if renamed above
  drop=c('var1','var2'),
  levels=list(sex  =list(female=0,male=1),
              smoke=list('non-current smoker'=0,
                         'current smoker'=1)),
  units=list(age='years', fev='L', height='inches'),
  labels=list(fev='Forced Expiratory Volume'))


# An alternative to levels=list(\dots) is for example
# upData(FEV, sex=factor(sex,0:1,c('female','male'))).
# 
# Note that we saved the changed data frame into a
# new data frame FEV2.  If we were confident of the
# correctness of our changes we could have stored
# the new data frame on top of the old one, under
# the original name FEV.


# -----------------------------------------------------------------------
# Step 6:  Check the data frame
# 
# The Hmisc describe function is perhaps the first
# function that should be used on the new data
# frame.  It provides documentation of all the
# variables and the frequency tabulation, counts of
# NAs,  and 5 largest and smallest values are
# helpful in detecting data errors.  Typing
# describe(FEV) will write the results to the
# current output window.  To put the results in a
# new window that can persist, even upon exiting
# S, we use the page function.  The describe
# output can be minimized to an icon but kept ready
# for guiding later steps of the analysis.


page(describe(FEV2), multi=TRUE) 
# multi=TRUE allows that window to persist while
# control is returned to other windows


# The new data frame is OK.  Store it on top of the
# old FEV and then use the graphical user interface
# to delete FEV2 (click on it and hit the Delete
# key) or type rm(FEV2) after the next statement.


FEV <- FEV2


# Next, we can use a variety of other functions to
# check and describe all of the variables.  As we
# are analyzing all or almost all of the variables,
# this is best done without attaching the data
# frame.  Note that plot.data.frame plots inverted
# CDFs for continuous variables and dot plots
# showing frequency distributions of categorical
# ones.


summary(FEV)
# basic summary function (summary.data.frame) 


plot(FEV)                # plot.data.frame 
datadensity(FEV)         
# rug plots and freq. bar charts for all var.


hist.data.frame(FEV)     
# for variables having > 2 values 


by(FEV, FEV$smoke, summary)  
# use basic summary function with stratification




# -----------------------------------------------------------------------
# Step 7:  Do detailed analyses involving individual
#          variables
# 
# Analyses based on the formula language can use
# data= so attaching the data frame may not be
# required.  This saves memory.  Here we use the
# Hmisc summary.formula function to compute 5
# statistics on height, stratified separately by age
# quartile and by sex.


options(width=80) 
summary(height ~ age + sex, data=FEV,
        fun=function(y)c(smean.sd(y),
                         smedian.hilow(y,conf.int=.5)))
# This computes mean height, S.D., median, outer quartiles


fit <- lm(height ~ age*sex, data=FEV) 
summary(fit)


# For this analysis we could also have attached the
# data frame in search position 2.  For other
# analyses, it is mandatory to attach the data frame
# unless FEV$ prefixes each variable name.
# Important: DO NOT USE attach(FEV, 1) or
# attach(FEV, pos=1, \dots) if you are only analyzing
# and not changing the variables, unless you really
# need to avoid conflicts with variables in search
# position 1 that have the same names as the
# variables in FEV.  Attaching into search position
# 1 will cause S-Plus to be more of a memory hog.


attach(FEV)
# Use e.g. attach(FEV[,Cs(age,sex)]) if you only
# want to analyze a small subset of the variables
# Use e.g. attach(FEV[FEV$sex=='male',]) to
# analyze a subset of the observations


summary(height ~ age + sex,
        fun=function(y)c(smean.sd(y),
          smedian.hilow(y,conf.int=.5)))
fit <- lm(height ~ age*sex)


# Run generic summary function on height and fev, 
# stratified by sex
by(data.frame(height,fev), sex, summary)


# Cross-classify into 4 sex x smoke groups
by(FEV, list(sex,smoke), summary)


# Plot 5 quantiles
s <- summary(fev ~ age + sex + height,
              fun=function(y)quantile(y,c(.1,.25,.5,.75,.9)))


plot(s, which=1:5, pch=c(1,2,15,2,1), #pch=c('=','[','o',']','='), 
     main='A Discovery', xlab='FEV')


# Use the nonparametric bootstrap to compute a 
# 0.95 confidence interval for the population mean fev
smean.cl.boot(fev)    # in Hmisc


# Use the Statistics \dots Compare Samples \dots One Sample 
# keys to get a normal-theory-based C.I.  Then do it 
# more manually.  The following method assumes that 
# there are no NAs in fev


sd <- sqrt(var(fev))
xbar <- mean(fev)
xbar
sd
n <- length(fev)
qt(.975,n-1)     
# prints 0.975 critical value of t dist. with n-1 d.f.


xbar + c(-1,1)*sd/sqrt(n)*qt(.975,n-1)   
# prints confidence limits


# Fit a linear model
# fit <- lm(fev ~ other variables \dots)


detach()


# The last command is only needed if you want to
# start operating on another data frame and you want
# to get FEV out of the way.




# -----------------------------------------------------------------------
# Creating data frames from scratch
# 
# Data frames can be created from within S.  To
# create a small data frame containing ordinary
# data, you can use something like


dframe <- data.frame(age=c(10,20,30), 
                     sex=c('male','female','male'),
                     stringsAsFactors=TRUE)


# You can also create a data frame using the Data
# Sheet.  Create an empty data frame with the
# correct variable names and types, then edit in the
# data.


dd <- data.frame(age=numeric(0),sex=character(0),
                 stringsAsFactors=TRUE)


# The sex variable will be stored as a factor, and
# levels will be automatically added to it as you
# define new values for sex in the Data Sheet's sex
# column.
# 
# When the data frame you need to create is defined
# by systematically varying variables (e.g., all
# possible combinations of values of each variable),
# the expand.grid function is useful for quickly
# creating the data.  Then you can add
# non-systematically-varying variables to the object
# created by expand.grid, using programming
# statements or editing the Data Sheet.  This
# process is useful for creating a data frame
# representing all the values in a printed table.
# In what follows we create a data frame
# representing the combinations of values from an 8
# x 2 x 2 x 2 (event x method x sex x what) table,
# and add a non-systematic variable percent to the
# data.


jcetable <- expand.grid(
 event=c('Wheezing at any time',
         'Wheezing and breathless',
         'Wheezing without a cold',
         'Waking with tightness in the chest',
         'Waking with shortness of breath',
         'Waking with an attack of cough',
         'Attack of asthma',
         'Use of medication'),
 method=c('Mail','Telephone'), 
 sex=c('Male','Female'),
 what=c('Sensitivity','Specificity'))


jcetable$percent <- 
c(756,618,706,422,356,578,289,333,
  576,421,789,273,273,212,212,212,
  613,763,713,403,377,541,290,226,
  613,684,632,290,387,613,258,129,
  656,597,438,780,732,679,938,919,
  714,600,494,877,850,703,963,987,
  755,420,480,794,779,647,956,941,
  766,423,500,833,833,604,955,986) / 10


# In jcetable, event varies most rapidly, then
# method, then sex, and what.
}
}
\keyword{data}
\keyword{manip}
\keyword{programming}
\keyword{interface}
\keyword{htest}
\concept{overview}
