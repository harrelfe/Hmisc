\name{transcan}
\alias{transcan}
\alias{summary.transcan}
\alias{print.transcan}
\alias{plot.transcan}
\alias{ggplot.transcan}
\alias{impute.transcan}
\alias{predict.transcan}
\alias{Function}
\alias{Function.transcan}
\alias{fit.mult.impute}
\alias{vcov.default}
\alias{vcov.fit.mult.impute}
\alias{[.transcan}
\alias{invertTabulated}
\title{
  Transformations/Imputations using Canonical Variates
}
\description{
  \code{transcan} is a nonlinear additive transformation and imputation
  function, and there are several functions for using and operating on
  its results.  \code{transcan} automatically transforms continuous and
  categorical variables to have maximum correlation with the best linear
  combination of the other variables.  There is also an option to use a
  substitute criterion - maximum correlation with the first principal
  component of the other variables.  Continuous variables are expanded
  as restricted cubic splines and categorical variables are expanded as
  contrasts (e.g., dummy variables).  By default, the first canonical
  variate is used to find optimum linear combinations of component
  columns.  This function is similar to \code{\link[acepack]{ace}} except that
  transformations for continuous variables are fitted using restricted
  cubic splines, monotonicity restrictions are not allowed, and
  \code{NA}s are allowed.  When a variable has any \code{NA}s,
  transformed scores for that variable are imputed using least squares
  multiple regression incorporating optimum transformations, or
  \code{NA}s are optionally set to constants.  Shrinkage can be used to
  safeguard against overfitting when imputing.  Optionally, imputed
  values on the original scale are also computed and returned.  For this
  purpose, recursive partitioning or multinomial logistic models can
  optionally be used to impute categorical variables, using what is
  predicted to be the most probable category.
  
  By default, \code{transcan} imputes \code{NA}s with \dQuote{best
  guess} expected values of transformed variables, back transformed to
  the original scale. Values thus imputed are most like conditional
  medians assuming the transformations make variables' distributions
  symmetric (imputed values are similar to conditionl modes for
  categorical variables).  By instead specifying \code{n.impute},
  \code{transcan} does approximate multiple imputation from the
  distribution of each variable conditional on all other variables.
  This is done by sampling \code{n.impute} residuals from the
  transformed variable, with replacement (a la bootstrapping), or by
  default, using Rubin's approximate Bayesian bootstrap, where a sample
  of size n with replacement is selected from the residuals on
  n non-missing values of the target variable, and then a sample
  of size m with replacement is chosen from this sample, where
  m is the number of missing values needing imputation for the
  current multiple imputation  repetition.  Neither of these bootstrap
  procedures assume normality or even symmetry of residuals. For
  sometimes-missing categorical variables, optimal scores are computed
  by adding the \dQuote{best guess} predicted mean score to random
  residuals off this score.  Then categories having scores closest to
  these predicted scores are taken as the random multiple imputations
  (\code{impcat = "rpart"} is not currently allowed
  with \code{n.impute}).  The literature recommends using \code{n.impute
  = 5} or greater. \code{transcan} provides only an approximation to
  multiple imputation, especially since it \dQuote{freezes} the
  imputation model before drawing the multiple imputations rather than
  using different estimates of regression coefficients for each
  imputation.  For multiple imputation, the \code{\link{aregImpute}} function
  provides a much better approximation to the full Bayesian approach
  while still not requiring linearity assumptions.
  
  When you specify \code{n.impute} to \code{transcan} you can use
  \code{fit.mult.impute} to re-fit any model \code{n.impute} times based
  on \code{n.impute} completed datasets (if there are any sometimes
  missing variables not specified to \code{transcan}, some observations
  will still be dropped from these fits).  After fitting \code{n.impute}
  models, \code{fit.mult.impute} will return the fit object from the
  last imputation, with \code{coefficients} replaced by the average of
  the \code{n.impute} coefficient vectors and with a component
  \code{var} equal to the imputation-corrected variance-covariance
  matrix using Rubin's rule.  \code{fit.mult.impute} can also use the object created by the
  \code{\link[mice]{mice}} function in the \pkg{mice} library to draw the
  multiple imputations, as well as objects created by
  \code{\link{aregImpute}}.  The following components of fit objects are
  also replaced with averages over the \code{n.impute} model fits:
  \code{linear.predictors}, \code{fitted.values}, \code{stats},
  \code{means}, \code{icoef}, \code{scale}, \code{center},
  \code{y.imputed}.

	By specifying \code{fun} to \code{fit.mult.impute} you can run any
	function on the fit objects from completed datasets, with the results
	saved in an element named \code{funresults}.  This facilitates
	running bootstrap or cross-validation separately on each completed
	dataset and storing all these results in a list for later processing,
	e.g., with the \code{rms} package \code{processMI} function.  Note that for
	\code{rms}-type validation you will need to specify
	\code{fitargs=list(x=TRUE,y=TRUE)} to \code{fit.mult.impute} and to
	use special names for \code{fun} result components, such as
	\code{validate} and \code{calibrate} so that the result can be
	processed with \code{processMI}. When simultaneously running multiple
	imputation and resampling model validation you may not need values for
	\code{n.impute} or \code{B} (number of bootstraps) as high as usual,
	as the total number of repetitions will be \code{n.impute * B}.
  
  \code{fit.mult.impute} can incorporate robust sandwich variance estimates into
  Rubin's rule if \code{robust=TRUE}.

	For \code{ols} models fitted by \code{fit.mult.impute} with stacking,
	the \eqn{R^2} measure in the stacked model fit is OK, and
	\code{print.ols} computes adjusted \eqn{R^2} using the real sample
	size so it is also OK because \code{fit.mult.compute} corrects the
	stacked error degrees of freedom in the stacked fit object to reflect
	the real sample size. 

  The \code{\link{summary}} method for \code{transcan} prints the function
  call, \eqn{R^2} achieved in transforming each variable, and for each
  variable the coefficients of all other transformed variables that are
  used to estimate the transformation of the initial variable.  If
  \code{imputed=TRUE} was used in the call to transcan, also uses the
  \code{describe} function to print a summary of imputed values.  If
  \code{long = TRUE}, also prints all imputed values with observation
  identifiers.  There is also a simple function \code{print.transcan}
  which merely prints the transformation matrix and the function call.
  It has an optional argument \code{long}, which if set to \code{TRUE}
  causes detailed parameters to be printed.  Instead of plotting while
  \code{transcan} is running, you can plot the final transformations
  after the fact using \code{plot.transcan} or \code{ggplot.transcan},
  if the option \code{trantab  = TRUE} was specified to \code{transcan}.
  If in addition the option 
  \code{imputed = TRUE} was specified to \code{transcan},
  \code{plot} and \code{ggplot} will show the location of imputed values
  (including multiples) along the axes.  For \code{ggplot}, imputed
  values are shown as red plus signs.
  
  \code{\link{impute}} method for \code{transcan} does imputations for a
  selected original data variable, on the original scale (if
  \code{imputed=TRUE} was given to \code{transcan}).  If you do not
  specify a variable to \code{impute}, it will do imputations for all
  variables given to \code{transcan} which had at least one missing
  value.  This assumes that the original variables are accessible (i.e.,
  they have been attached) and that you want the imputed variables to
  have the same names are the original variables. If \code{n.impute} was
  specified to \code{transcan} you must tell \code{\link{impute}} which
  \code{imputation} to use.   Results are stored in \code{.GlobalEnv}
  when \code{list.out} is not specified  (it is recommended to use
  \code{list.out=TRUE}).   
  
  The \code{\link{predict}} method for \code{transcan} computes
  predicted variables and imputed values from a matrix of new data.
  This matrix should have the same column variables as the original
  matrix used with \code{transcan}, and in the same order (unless a
  formula was used with \code{transcan}).
  
  The \code{\link{Function}} function is a generic function
  generator. \code{Function.transcan} creates \R functions to transform
  variables using transformations created by \code{transcan}. These
  functions are useful for getting predicted values with predictors set
  to values on the original scale.
  
  The \code{\link{vcov}} methods are defined here so that
  imputation-corrected variance-covariance matrices are readily
  extracted from \code{fit.mult.impute} objects, and so that
  \code{fit.mult.impute} can easily compute traditional covariance
  matrices for individual completed datasets.
  
  The subscript method for \code{transcan} preserves attributes.

  The \code{invertTabulated} function does either inverse linear
  interpolation or uses sampling to sample qualifying x-values having
  y-values near the desired values.  The latter is used to get inverse
  values having a reasonable distribution (e.g., no floor or ceiling
  effects) when the transformation has a flat or nearly flat segment,
  resulting in a many-to-one transformation in that region.  Sampling
  weights are a combination of the frequency of occurrence of x-values
  that are within \code{tolInverse} times the range of \code{y} and the
  squared distance between the associated y-values and the target
  y-value (\code{aty}).
}
\usage{
transcan(x, method=c("canonical","pc"),
         categorical=NULL, asis=NULL, nk, imputed=FALSE, n.impute,
         boot.method=c('approximate bayesian', 'simple'),
         trantab=FALSE, transformed=FALSE, 
         impcat=c("score", "multinom", "rpart"),
         mincut=40, 
         inverse=c('linearInterp','sample'), tolInverse=.05,
         pr=TRUE, pl=TRUE, allpl=FALSE, show.na=TRUE, 
         imputed.actual=c('none','datadensity','hist','qq','ecdf'),
         iter.max=50, eps=.1, curtail=TRUE, 
         imp.con=FALSE, shrink=FALSE, init.cat="mode", 
         nres=if(boot.method=='simple')200 else 400,
         data, subset, na.action, treeinfo=FALSE, 
         rhsImp=c('mean','random'), details.impcat='', \dots)

\method{summary}{transcan}(object, long=FALSE, digits=6, \dots)

\method{print}{transcan}(x, long=FALSE, \dots)

\method{plot}{transcan}(x, \dots)

\method{ggplot}{transcan}(data, mapping, scale=FALSE, \dots, environment)

\method{impute}{transcan}(x, var, imputation, name, pos.in, data, 
       list.out=FALSE, pr=TRUE, check=TRUE, \dots)

fit.mult.impute(formula, fitter, xtrans, data, n.impute, fit.reps=FALSE,
                dtrans, derived, fun, vcovOpts=NULL,
                robust=FALSE, cluster, robmethod=c('huber', 'efron'),
                method=c('ordinary', 'stack', 'only stack'),
                funstack=TRUE, lrt=FALSE,
                pr=TRUE, subset, fitargs)

\method{predict}{transcan}(object, newdata, iter.max=50, eps=0.01, curtail=TRUE,
        type=c("transformed","original"),
        inverse, tolInverse, check=FALSE, \dots)

Function(object, \dots)

\method{Function}{transcan}(object, prefix=".", suffix="", pos=-1, \dots)

invertTabulated(x, y, freq=rep(1,length(x)), 
                aty, name='value',
                inverse=c('linearInterp','sample'),
                tolInverse=0.05, rule=2)

\method{vcov}{default}(object, regcoef.only=FALSE, \dots)

\method{vcov}{fit.mult.impute}(object, regcoef.only=TRUE,
                intercepts='mid', \dots)

}
\arguments{
  \item{x}{
    a matrix containing continuous variable values and codes for
    categorical variables.  The matrix must have column names
    (\code{dimnames}).  If row names are present, they are used in
    forming the \code{names} attribute of imputed values if
    \code{imputed = TRUE}.  \code{x} may also be a formula, in which
    case the model matrix is created automatically, using data in the
    calling frame.  Advantages of using a formula are that
    \verb{categorical} variables can be determined automatically by a
    variable being a \code{\link{factor}} variable, and variables with
    two unique levels are modeled \verb{asis}. Variables with 3 unique
    values are considered to be \verb{categorical} if a formula is
    specified.  For a formula you may also specify that a variable is to
    remain untransformed by enclosing its name with the identify
    function, e.g. \code{I(x3)}.  The user may add other variable names
    to the \code{asis} and \code{categorical} vectors.  For
    \code{invertTabulated}, \code{x} is a vector or a list with three
    components: the x vector, the corresponding vector of transformed
    values, and the corresponding vector of frequencies of the pair of
    original and transformed variables. For \code{print}, \code{plot},
    \code{ggplot}, \code{impute}, and \code{predict}, \code{x} is an
		object created by \code{transcan}.
  }
  \item{formula}{
    any \R model formula
  }
  \item{fitter}{
    any \R, \code{rms}, modeling function (not in quotes) that computes
    a vector of \code{\link{coefficients}} and for which
    \code{\link{vcov}} will return a variance-covariance matrix.  E.g.,
    \code{fitter = \link{lm}}, \code{\link{glm}},
		\code{\link[rms]{ols}}. At present models 
    involving non-regression parameters (e.g., scale parameters in
    parametric survival models) are not handled fully.
  }
  \item{xtrans}{
    an object created by \code{transcan}, \code{\link{aregImpute}}, or
    \code{\link[mice]{mice}}
  }
  \item{method}{
    use \code{method="canonical"} or any abbreviation thereof, to use
    canonical variates (the default). \code{method="pc"} transforms a
    variable instead so as to maximize the correlation with the first
    principal component of the other variables.  For
		\code{fit.mult.impute}, \code{method} specifies whether to use
		standard multiple imputation (the default \code{method='ordinary'})
		or whether to get final coefficients from stacking all 
		completed datasets and fitting one model.  Stacking is required if
		likelihood ratio tests accounting for imputation are to be done.
		\code{method='stack'} means to do regular MI and stacking, which
		results in more valid standard errors of coefficient estimates.
		\code{method='only stack'} means that model fits are not done on
		individual completed datasets, and standard errors will not be very
		accurate.
  }
  \item{categorical}{
    a character vector of names of variables in \code{x} which are
    categorical, for which the ordering of re-scored values is not
    necessarily preserved. If \code{categorical} is omitted, it is
    assumed that all variables are continuous (or binary).  Set
    \code{categorical="*"} to treat all variables as categorical.
  }
  \item{asis}{
    a character vector of names of variables that are not to be
    transformed. For these variables, the guts of
    \code{\link[stats]{lm.fit}} \code{method="qr"} is used to impute
    missing values. You may want to treat binary variables \verb{asis}
    (this is automatic if using a formula).  If \code{imputed = TRUE},
    you may want to use \samp{"categorical"} for binary variables if you
    want to force imputed values to be one of the original data
    values. Set \code{asis="*"} to treat all variables \verb{asis}.
  }
  \item{nk}{
    number of knots to use in expanding each continuous variable (not
    listed in \code{asis}) in a restricted cubic spline function.
    Default is 3 (yielding 2 parameters for a variable) if
    \eqn{n < 30}, 4 if
    \eqn{30 <= n < 100}{30 \eq n < 100}, and 5 if
    \eqn{n \ge 100}{n >= 100} (4 parameters).
  }
  \item{imputed}{
    Set to \code{TRUE} to return a list containing imputed values on the
    original scale. If the transformation for a variable is
    non-monotonic, imputed values are not unique.  \code{transcan} uses
    the \code{\link{approx}} function, which returns the highest value
    of the variable with the transformed score equalling the imputed
    score. \code{imputed=TRUE} also causes original-scale imputed values
    to be shown as tick marks on the top margin of each graph when
    \code{show.na=TRUE} (for the final iteration only). For categorical
    predictors, these imputed values are passed through the
    \code{\link{jitter}} function so that their frequencies can be
    visualized.  When \code{n.impute} is used, each \code{NA} will have
    \code{n.impute} tick marks.
  }
  \item{n.impute}{
    number of multiple imputations.  If omitted, single predicted
    expected value imputation is used.  \code{n.impute=5} is frequently
    recommended.
  }
  \item{boot.method}{
    default is to use the approximate Bayesian bootstrap (sample with
    replacement from sample with replacement of the vector of residuals).
    You can also specify \code{boot.method="simple"} to use the usual
    bootstrap one-stage sampling with replacement.
  }
  \item{trantab}{
    Set to \code{TRUE} to add an attribute \code{trantab} to the
    returned matrix. This contains a vector of lists each with
    components \code{x} and \code{y} containing the unique values and
    corresponding transformed values for the columns of \code{x}.  This
    is set up to be used easily with the \code{\link{approx}} function.
    You must specify \code{trantab=TRUE} if you want to later use the
    \code{predict.transcan} function with \code{type = "original"}.
  }
  \item{transformed}{
    set to \code{TRUE} to cause \code{transcan} to return an object
    \code{transformed} containing the matrix of transformed variables  
  }
  \item{impcat}{
    This argument tells how to impute categorical variables on the
    original scale.  The default is \code{impcat="score"} to impute the
    category whose canonical variate score is closest to the predicted
    score. Use \code{impcat="rpart"} to impute categorical variables
    using the values of all other transformed predictors in conjunction
	with the \code{\link[rpart]{rpart}} function.  A better but somewhat
	slower approach is to 
    use \code{impcat="multinom"} to fit a multinomial logistic model to
    the categorical variable, at the last iteraction of the
    \code{transcan} algorithm.  This uses the \code{\link[nnet]{multinom}}
    function in the \pkg{nnet} library of the \pkg{MASS} package (which
    is assumed to have been installed by the user) to fit a polytomous
    logistic model to the current working transformations of all the
    other variables (using conditional mean imputation for missing
    predictors).  Multiple imputations are made by drawing multinomial
    values from the vector of predicted probabilities of category
    membership for the missing categorical values.
  }
  \item{mincut}{
    If \code{imputed=TRUE}, there are categorical variables, and
    \code{impcat = "rpart"}, \code{mincut} specifies the lowest node size
    that will be allowed to be split.  The default is 40.
  }
  \item{inverse}{
    By default, imputed values are back-solved on the original scale
    using inverse linear interpolation on the fitted tabulated
    transformed values. This will cause distorted distributions of
    imputed values (e.g., floor and ceiling effects) when the estimated
    transformation has a flat or nearly flat section.  To instead use
    the \code{invertTabulated} function (see above) with the
    \code{"sample"} option, specify \code{inverse="sample"}.  
  }
  \item{tolInverse}{
    the multiplyer of the range of transformed values, weighted by
    \code{freq} and by the distance measure, for determining the set of
    x values having y values within a tolerance of the value of
    \code{aty} in \code{invertTabulated}.  For \code{predict.transcan},
    \code{inverse} and \code{tolInverse} are obtained from options that
    were specified to \code{transcan} by default.  Otherwise, if not
    specified by the user, these default to the defaults used to
    \code{invertTabulated}.
  }
  \item{pr}{
    For \code{transcan}, set to \code{FALSE} to suppress printing
    \eqn{R^2} and shrinkage factors.  Set \code{impute.transcan=FALSE}
    to suppress messages concerning the number of \code{NA} values
    imputed. Set \code{fit.mult.impute=FALSE} to suppress printing
    variance inflation factors accounting for imputation, rate of
    missing information, and degrees of freedom.
  }
  \item{pl}{
    Set to \code{FALSE} to suppress plotting the final transformations
    with distribution of scores for imputed values (if
    \code{show.na=TRUE}).
  }
  \item{allpl}{
    Set to \code{TRUE} to plot transformations for intermediate iterations.
  }
  \item{show.na}{
    Set to \code{FALSE} to suppress the distribution of scores assigned
    to missing values (as tick marks on the right margin of each
    graph). See also \code{imputed}.
  }
  \item{imputed.actual}{
    The default is \samp{"none"} to suppress plotting of actual
    vs. imputed values for all variables having any \code{NA} values.
    Other choices are \samp{"datadensity"} to use
    \code{\link{datadensity}} to make a single plot, \samp{"hist"} to
    make a series of back-to-back histograms, \samp{"qq"} to make a
    series of q-q plots, or \samp{"ecdf"} to make a series of empirical
    cdfs.  For \code{imputed.actual="datadensity"} for example you get a
    rug plot of the non-missing values for the variable with beneath it
    a rug plot of the imputed values. When \code{imputed.actual} is not
    \samp{"none"}, \code{imputed} is automatically set to \code{TRUE}.
  }
  \item{iter.max}{
    maximum number of iterations to perform for \code{transcan} or
    \code{predict}. For \code{\link{predict}}, only one iteration is
    used if there are no \code{NA} values in the data or if
    \code{imp.con} was used.
  }
  \item{eps}{
    convergence criterion for \code{transcan} and \code{\link{predict}}.
    \code{eps} is the maximum change in transformed values from one
    iteration to the next.  If for a given iteration all new
    transformations of variables differ by less than \code{eps} (with or
    without negating the transformation to allow for \dQuote{flipping})
    from the transformations in the previous iteration, one more
    iteration is done for \code{transcan}. During this last iteration,
    individual transformations are not updated but coefficients of
    transformations are.  This improves stability of coefficients of
    canonical variates on the right-hand-side. \code{eps} is ignored
    when \code{rhsImp="random"}.
  }
  \item{curtail}{
    for \code{transcan}, causes imputed values on the transformed scale
    to be truncated so that their ranges are within the ranges of
    non-imputed transformed values. For \code{\link{predict}},
    \code{curtail} defaults to \code{TRUE} to truncate predicted
    transformed values to their ranges in the original fit (\code{xt}).
  }
  \item{imp.con}{
    for \code{transcan}, set to \code{TRUE} to impute \code{NA} values
    on the original scales with constants (medians or most frequent
    category codes).  Set to a vector of constants to instead always use
    these constants for imputation. These imputed values are ignored
    when fitting the current working transformation for asingle
    variable.
  }
  \item{shrink}{
    default is \code{FALSE} to use ordinary least squares or canonical
    variate estimates. For the purposes of imputing \code{NA}s, you may
    want to set \code{shrink=TRUE} to avoid overfitting when developing
    a prediction equation to predict each variables from all the others
    (see details below).
  }
  \item{init.cat}{
    method for initializing scorings of categorical variables.  Default
    is \samp{"mode"} to use a dummy variable set to 1 if the value is
    the most frequent value (this is the default). Use \samp{"random"}
    to use a random 0-1 variable.  Set to \samp{"asis"} to use the
    original integer codes asstarting scores.
  }
  \item{nres}{
    number of residuals to store if \code{n.impute} is specified.  If
    the dataset has fewer than \code{nres} observations, all residuals
    are saved. Otherwise a random sample of the residuals of length
    \code{nres} without replacement is saved.  The default for
    \code{nres} is higher if \code{boot.method="approximate bayesian"}.
  }
  \item{data}{
    Data frame used to fill the formula.  For \code{ggplot} is the
		result of \code{transcan} with \code{trantab=TRUE}.
  }
  \item{subset}{
    an integer or logical vector specifying the subset of observations
    to fit
  }
  \item{na.action}{
    These may be used if \code{x} is a formula.  The default
    \code{na.action} is \code{na.retain} (defined by \code{transcan})
    which keeps all observations with any \code{NA} values. For
    \code{impute.transcan}, \code{data} is a data frame to use as the
    source of variables to be imputed, rather than using
    \code{pos.in}.  For \code{fit.mult.impute}, \code{data} is
    mandatory and is a data frame containing the data to be used in
    fitting the model but before imputations are applied.  Variables
    omitted from \code{data} are assumed to be available from frame1
    and do not need to be imputed.
  }
  \item{treeinfo}{
    Set to \code{TRUE} to get additional information printed when
    \code{impcat="rpart"}, such as the predicted probabilities of
    category membership.
  }
  \item{rhsImp}{
    Set to \samp{"random"} to use random draw imputation when a
    sometimes missing variable is moved to be a predictor of other
    sometimes missing variables.  Default is \code{rhsImp="mean"}, which
    uses conditional mean imputation on the transformed scale.
    Residuals used are residuals from the transformed scale.  When
    \samp{"random"} is used, \code{transcan} runs 5 iterations and
    ignores \code{eps}.
  }
  \item{details.impcat}{
    set to a character scalar that is the name of a category variable to
    include in the resulting \code{transcan} object an element
    \code{details.impcat} containing details of how the categorical
    variable was multiply imputed.
  }
  \item{\dots}{
    arguments passed to \code{\link{scat1d}}. For \code{ggplot.transcan},
		these arguments are passed to \code{facet_wrap}, e.g. \code{ncol=2}.
  }
  \item{long}{
    for \code{\link{summary}}, set to \code{TRUE} to print all imputed
    values. For \code{\link{print}}, set to \code{TRUE} to print details
    of transformations/imputations.  
  }
  \item{digits}{
    number of significant digits for printing values by
    \code{\link{summary}}
  }
	\item{scale}{for \code{ggplot.transcan} set \code{scale=TRUE} to
		scale transformed values to [0,1] before plotting.}
	\item{mapping,environment}{not used; needed because of rules about generics}
  \item{var}{
    For \code{\link{impute}}, is a variable that was originally a column
    in \code{x}, for which imputated values are to be filled
    in. \code{imputed=TRUE} must have been used in \code{transcan}.
    Omit \code{var} to impute all variables, creating new variables in
    position \code{pos} (see \code{\link{assign}}).
  }
  \item{imputation}{
    specifies which of the multiple imputations to use for filling in
    \code{NA} values
  }
  \item{name}{
    name of variable to impute, for \code{\link{impute}} function.
    Default is character string version of the second argument
    (\code{var}) in the call to \code{\link{impute}}. For
    \code{invertTabulated}, is the name of variable being transformed
    (used only for warning messages).
  }
  \item{pos.in}{
    location as defined by \code{\link{assign}} to find variables that
	need to be 
    imputed, when all variables are to be imputed automatically by
    \code{impute.transcan} (i.e., when no input variable name is
    specified).  Default is position that contains
    the first variable to be imputed.
  }
  \item{list.out}{
    If \code{var} is not specified, you can set \code{list.out=TRUE} to
    have \code{impute.transcan} return a list containing variables with
    needed values imputed.  This list will contain a single imputation.
    Variables not needing imputation are copied to the list as-is.  You
    can use this list for analysis just like a data frame.
  }
  \item{check}{
    set to \code{FALSE} to suppress certain warning messages
  }
  \item{newdata}{
    a new data matrix for which to compute transformed
    variables. Categorical variables must use the same integer codes as
    were used in the call to \code{transcan}.  If a formula was
    originally specified to \code{transcan} (instead of a data matrix),
    \code{newdata} is optional and if given must be a data frame; a
    model frame is generated automatically from the previous formula.
    The \code{na.action} is handled automatically, and the levels for
    factor variables must be the same and in the same order as were used
    in the original variables specified in the formula given to
    \code{transcan}.
  }
  \item{fit.reps}{
    set to \code{TRUE} to save all fit objects from the fit for each
    imputation in \code{fit.mult.impute}.  Then the object returned will
    have a component \code{fits} which is a list whose i'th
    element is the i'th fit object. 
  }
  \item{dtrans}{
    provides an approach to creating derived variables from a single
    filled-in dataset.  The function specified as \code{dtrans} can even
    reshape the imputed dataset.  An example of such usage is fitting
    time-dependent covariates in a Cox model that are created by
    \dQuote{start,stop} intervals.  Imputations may be done on a one
    record per subject data frame that is converted by \code{dtrans} to
    multiple records per subject.  The imputation can enforce
    consistency of certain variables across records so that for example
    a missing value of sex will not be imputed as \samp{male} for
    one of the subject's records and \samp{female} as another.  An
    example of how \code{dtrans} might be specified is
    \code{dtrans=function(w) \{w$age <- w$years + w$months/12; w\}}
    where \code{months} might havebeen imputed but \code{years} was
    never missing.  An outline for using `dtrans` to impute missing
		baseline variables in a longitudinal analysis appears in Details below.
  }
  \item{derived}{
    an expression containing \R expressions for computing derived
    variables that are used in the model formula.  This is useful when
    multiple imputations are done for component variables but the actual
    model uses combinations of these (e.g., ratios or other
    derivations). For a single derived variable you can specify for
    example \code{derived=expression(ratio <- weight/height)}.  For
    multiple derived variables use the form
    \code{derived=expression(\{ratio <- weight/height; product <-
      weight*height\})} or put the expression on separate input lines.
    To monitor the multiply-imputed derived variables you can add to the
    \code{expression} a command such as \code{print(describe(ratio))}.
    See the example below.  Note that \code{derived} is not yet
    implemented.  
  }
	\item{fun}{a function of a fit made on one of the completed datasets.
		Typical uses are bootstrap model validations.  The result of
		\code{fun} for imputation \code{i} is placed in the \code{i}th
		element of a list that is returned in the \code{fit.mult.impute}
		object element named \code{funresults}. See
		the \code{rms} \code{processMI} function for help in processing
			these results for the cases of \code{validate} and \code{calibrate}.}
	\item{vcovOpts}{a list of named additional arguments to pass to the
		\code{vcov} method for \code{fitter}.  Useful for \code{orm} models
		for retaining all intercepts
		(\code{vcovOpts=list(intercepts='all')}) instead of just the middle
		one.}
  \item{robust}{set to \code{TRUE} to have \code{fit.mult.impute} call the
    \code{rms} package \code{robcov} function on each fit on a
    completed dataset.  When \code{cluster} is given, \code{robust} is
    forced to \code{TRUE}.}
  \item{cluster}{a vector of cluster IDs that is the same length of the number
    of rows in the dataset being analyzed.  When specified, \code{robust} is
    assumed to be \code{TRUE}, and the \code{rms} \code{robcov} function is 
    called with the \code{cluster} vector given as its second argument.}
  \item{robmethod}{see the \code{robcov} function's \code{method}
		argument}
	\item{funstack}{set to \code{FALSE} to not run \code{fun} on the
		stacked dataset, making an \code{n.impute}+1 element of
		\code{funresults}}
	\item{lrt}{set to \code{TRUE} to have \code{method, fun, fitargs} set
		appropriately automatically so that \code{processMI} can be used to
		get likelihood ratio tests.  When doing this, \code{fun} may not be specified by the user.}
	\item{fitargs}{a list of extra arguments to pass to \code{fitter},
		used especially with \code{fun}.  When \code{robust=TRUE} the arguments
    \code{x=TRUE, y=TRUE} are automatically added to \code{fitargs}.}
  \item{type}{
    By default, the matrix of transformed variables is returned, with
    imputed values on the transformed scale.  If you had specified
    \code{trantab=TRUE} to \code{transcan}, specifying
    \code{type="original"} does the table look-ups with linear
    interpolation to return the input matrix \code{x} but with imputed
    values on the original scale inserted for \code{NA} values.  For
    categorical variables, the method used here is to select the
    category code having a corresponding scaled value closest to the
    predicted transformed value.  This corresponds to the default
    \code{impcat}.  Note: imputed values
    thus returned when \code{type="original"} are single expected value
   imputations even in \code{n.impute} is given.
 }
 \item{object}{
   an object created by \code{transcan}, or an object to be converted to
   \R function code, typically a model fit object of some sort  
 }
 \item{prefix, suffix}{
   When creating separate \R functions for each variable in \code{x},
   the name of the new function will be \code{prefix} placed in front of
   the variable name, and \code{suffix} placed in back of the name.  The
   default is to use names of the form \samp{.varname}, where
   varname is the variable name.
 }
 \item{pos}{
   position as in \code{\link{assign}} at which to store new functions
   (for \code{\link{Function}}). Default is \code{pos=-1}.
 }
 \item{y}{
   a vector corresponding to \code{x} for \code{invertTabulated}, if its
   first argument \code{x} is not a list
 }
 \item{freq}{
   a vector of frequencies corresponding to cross-classified \code{x}
   and \code{y} if \code{x} is not a list.  Default is a vector of ones.
 }
 \item{aty}{
   vector of transformed values at which inverses are desired
 }
 \item{rule}{
   see \code{\link{approx}}.  \code{transcan} assumes \code{rule} is
   always 2.
 }
 \item{regcoef.only}{
   set to \code{TRUE} to make \code{vcov.default} delete positions in
   the covariance matrix for any non-regression coefficients (e.g., log
   scale parameter from \code{\link[rms]{psm}} or \code{\link[survival]{survreg}})
 }
 \item{intercepts}{this is primarily for \code{\link[rms]{orm}}
	 objects.  Set to \code{"none"} to discard all intercepts from the
	 covariance matrix, or to \code{"all"} or \code{"mid"} to keep all
	 elements generated by \code{orm} (\code{orm} only outputs the
	 covariance matrix for the intercept corresponding to the median).
	 You can also set \code{intercepts} to a vector of subscripts for
	 selecting particular intercepts in a multi-intercept model.}
}
\value{
  For \code{transcan}, a list of class \samp{transcan} with elements
  \item{call}{ (with the function call)}
  \item{iter}{ (number of  iterations done)}
  \item{rsq, rsq.adj}{
    containing the \eqn{R^2}{R-square}s and adjusted
    \eqn{R^2}{R-square}s achieved in predicting each variable from all
    the others
  }
  \item{categorical}{
    the values supplied for \code{categorical}
  }
  \item{asis}{
    the values supplied for \code{asis}
  }
  \item{coef}{
    the within-variable coefficients used to compute the first
    canonical variate
  }
  \item{xcoef}{
    the (possibly shrunk) across-variables coefficients of the first
    canonical variate that predicts each variable in-turn.
  }
  \item{parms}{
    the parameters of the transformation (knots for splines, contrast
    matrix for categorical variables)  
  }
  \item{fillin}{
    the initial estimates for missing values (\code{NA} if variable
    never missing)
  }
  \item{ranges}{
    the matrix of ranges of the transformed variables (min and max in
    first and secondrow)
  }
  \item{scale}{
    a vector of scales used to determine convergence for a
    transformation.
  }
  \item{formula}{
    the formula (if \code{x} was a formula)
  }
  , and optionally a vector of shrinkage factors used for predicting
  each variable from the others.  For \code{asis} variables, the scale
  is the average absolute difference about the median.  For other
  variables it is unity, since canonical variables are standardized.
  For \code{xcoef}, row i has the coefficients to predict
  transformed variable i, with the column for the coefficient of
  variable i set to \code{NA}.  If \code{imputed=TRUE} was given,
  an optional element \code{imputed} also appears.  This is a list with
  the vector of imputed values (on the original scale) for each variable
  containing \code{NA}s.  Matrices rather than vectors are returned if
  \code{n.impute} is given.  If \code{trantab=TRUE}, the \code{trantab}
  element also appears, as described above.  If \code{n.impute > 0},
  \code{transcan} also returns a list \code{residuals} that can be used
  for future multiple imputation.
  
  \code{impute} returns a vector (the same length as \code{var}) of
  class \samp{impute} with \code{NA} values imputed.  

  \code{predict} returns a matrix with the same number of columns or
  variables as were in \code{x}.
  
  \code{fit.mult.impute} returns a fit object that is a modification of
  the fit object created by fitting the completed dataset for the final
  imputation.  The \code{var} matrix in the fit object has the
  imputation-corrected variance-covariance matrix.  \code{coefficients}
  is the average (over imputations) of the coefficient vectors,
  \code{variance.inflation.impute} is a vector containing the ratios of
  the diagonals of the between-imputation variance matrix to the
  diagonals of the average apparent (within-imputation) variance
  matrix. \code{missingInfo} is
  \cite{Rubin's rate of missing information} and \code{dfmi} is
  \cite{Rubin's degrees of freedom for a t-statistic}
  for testing a single parameter.  The last two objects are vectors
  corresponding to the diagonal of the variance matrix.  The class
  \code{"fit.mult.impute"} is prepended to the other classes produced by
  the fitting function.

	When \code{method} is not \code{'ordinary'}, i.e., stacking is used,
	\code{fit.mult.impute} returns a modified fit object that is computed
	on all completed datasets combined, with most all statistics that are
	functions of the sample size corrected to the real sample size.
	Elements in the fit such as \code{residuals} will have length equal to
	the real sample size times the number of imputations.

	\code{fit.mult.impute} stores \code{intercepts} attributes in the
	coefficient matrix and in \code{var} for \code{orm} fits.
}
\section{Side Effects}{
  prints, plots, and \code{impute.transcan} creates new variables.
}
\details{
  The starting approximation to the transformation for each variable is
  taken to be the original coding of the variable.  The initial
  approximation for each missing value is taken to be the median of the
  non-missing values for the variable (for continuous ones) or the most
  frequent category (for categorical ones).  Instead, if \code{imp.con}
  is a vector, its values are used for imputing \code{NA} values.  When
  using each variable as a dependent variable, \code{NA} values on that
  variable cause all observations to be temporarily deleted.  Once a new
  working transformation is found for the variable, along with a model
  to predict that transformation from all the other variables, that
  latter model is used to impute \code{NA} values in the selected
  dependent variable if \code{imp.con} is not specified.
  
  When that variable is used to predict a new dependent variable, the
  current working imputed values are inserted.  Transformations are
  updated after each variable becomes a dependent variable, so the order
  of variables on \code{x} could conceivably make a difference in the
  final estimates.  For obtaining out-of-sample
  predictions/transformations, \code{\link{predict}} uses the same
  iterative procedure as \code{transcan} for imputation, with the same
  starting values for fill-ins as were used by \code{transcan}.  It also
  (by default) uses a conservative approach of curtailing transformed
  variables to be within the range of the original ones. Even when
  \code{method = "pc"} is specified, canonical variables are used for
  imputing missing values.
  
  Note that fitted transformations, when evaluated at imputed variable
  values (on the original scale), will not precisely match the
  transformed imputed values returned in \code{xt}.  This is because
  \code{transcan} uses an approximate method based on linear
  interpolation to back-solve for imputed values on the original scale.
  
  Shrinkage uses the method of
  \cite{Van Houwelingen and Le Cessie (1990)} (similar to
  \cite{Copas, 1983}).  The shrinkage factor is
  \deqn{\frac{1-\frac{(1-R2)(n-1)}{n-k-1}}{R2}}{%
    [1 - (1 - R2)(n - 1)/(n - k - 1)]/R2}
  where R2 is the apparent \eqn{R^2}{R-square}d for predicting the
  variable, n is the number of non-missing values, and k is
  the effective number of degrees of freedom (aside from intercepts).  A
  heuristic estimate is used for k:
  \code{A - 1 + sum(max(0,Bi - 1))/m + m}, where
  A is the number of d.f. required to represent the variable being
  predicted, the Bi are the number of columns required to
  represent all the other variables, and m is the number of all
  other variables.  Division by m is done because the
  transformations for the other variables are fixed at their current
  transformations the last time they were being predicted.  The
  \eqn{+ m} term comes from the number of coefficients estimated
  on the right hand side, whether by least squares or canonical
  variates.  If a shrinkage factor is negative, it is set to 0.  The
  shrinkage factor is the ratio of the adjusted \eqn{R^2}{R-square}d to
  the ordinary \eqn{R^2}{R-square}d. The adjusted \eqn{R^2}{R-square}d is
  \deqn{1-\frac{(1-R2)(n-1)}{n-k-1}}{
    1 - (1 - R2)(n - 1)/(n - k - 1)}
  which is also set to zero if it is negative.  If \code{shrink=FALSE}
  and the adjusted \eqn{R^2}{R-square}s are much smaller than the
  ordinary \eqn{R^2}{R-square}s, you may want to run \code{transcan}
  with \code{shrink=TRUE}.
 
  Canonical variates are scaled to have variance of 1.0, by multiplying
  canonical coefficients from \code{\link{cancor}} by 
  \eqn{\sqrt{n-1}}{sqrt(n - 1)}.

  When specifying a non-\pkg{rms} library fitting function to
  \code{fit.mult.impute} (e.g., \code{\link{lm}}, \code{\link{glm}}),
  running the result of \code{fit.mult.impute} through that fit's
  \code{\link{summary}} method will not use the imputation-adjusted
  variances.  You may obtain the new variances using \code{fit$var} or
  \code{vcov(fit)}.
  
  When you specify a \pkg{rms} function to \code{fit.mult.impute} (e.g.
  \code{\link[rms]{lrm}}, \code{\link[rms]{ols}}, \code{\link[rms]{cph}},
  \code{\link[rms]{psm}}, \code{\link[rms]{bj}}, \code{\link[rms]{Rq}},
  \code{\link[rms]{Gls}}, \code{\link[rms]{Glm}}), automatically computed
  transformation  parameters (e.g., knot locations for
  \code{\link[rms]{rcs}}) that are estimated for the first imputation are
  used for all other imputations.  This ensures that knot locations will
  not vary, which would change the meaning of the regression
  coefficients.
  
  Warning: even though \code{fit.mult.impute} takes imputation into
  account when estimating variances of regression coefficient, it does
  not take into account the variation that results from estimation of
  the shapes and regression coefficients of the customized imputation
  equations. Specifying \code{shrink=TRUE} solves a small part of this
  problem.  To fully account for all sources of variation you should
  consider putting the \code{transcan} invocation inside a bootstrap or
  loop, if execution time allows.  Better still, use
  \code{\link{aregImpute}} or a package such as  as \pkg{mice} that uses
  real Bayesian posterior realizations to multiply impute missing values
  correctly.
  
  It is strongly recommended that you use the \pkg{Hmisc} \code{\link{naclus}}
  function to determine is there is a good basis for imputation.
  \code{\link{naclus}} will tell you, for example, if systolic blood
  pressure is missing whenever diastolic blood pressure is missing.  If
  the only variable that is well correlated with diastolic bp is
  systolic bp, there is no basis for imputing diastolic bp in this case.
  
  At present, \code{predict} does not work with multiple imputation.

  When calling \code{fit.mult.impute} with \code{\link{glm}} as the
  \code{fitter} argument, if you need to pass a \code{family} argument
  to \code{\link{glm}} do it by quoting the family, e.g.,
  \code{family="binomial"}.
  
  \code{fit.mult.impute} will not work with proportional odds models
  when regression imputation was used (as opposed to predictive mean
  matching).  That's because regression imputation will create values of
  the response variable that did not exist in the dataset, altering the
  intercept terms in the model.
  
  You should be able to use a variable in the formula given to
  \code{fit.mult.impute} as a numeric variable in the regression model
  even though it was a factor variable in the invocation of
  \code{transcan}.  Use for example \code{fit.mult.impute(y ~ codes(x),
    lrm, trans)} (thanks to Trevor Thompson
  \email{trevor@hp5.eushc.org}).

	Here is an outline of the steps necessary to impute baseline variables
	using the \code{dtrans} argument, when the analysis to be repeated by
	\code{fit.mult.impute} is a longitudinal analysis (using
	e.g. \code{Gls}).
	\enumerate{
		\item Create a one row per subject data frame containing baseline
	variables plus follow-up variables that are assigned to windows.  For
	example, you may have dozens of repeated measurements over years but
	you capture the measurements at the times measured closest to 1, 2,
	and 3 years after study entry
	\item Make sure the dataset contains the subject ID
	\item This dataset becomes the one passed to \code{aregImpute} as
	\code{data=}.  You will be imputing missing baseline variables from
	follow-up measurements defined at fixed times.
	\item Have another dataset with all the non-missing follow-up values
	on it, one record per measurement time per subject.  This dataset
	should not have the baseline variables on it, and the follow-up
	measurements should not be named the same as the baseline variable(s);
	the subject ID must also appear
	\item Add the dtrans argument to \code{fit.mult.impute} to define a
	function with one argument representing the one record per subject
	dataset with missing values filled it from the current imputation.
	This function merges the above 2 datasets; the returned value of this
	function is the merged data frame.
	\item This merged-on-the-fly dataset is the one handed by \code{fit.mult.impute} to your fitting function, so  variable names in the formula given to \code{fit.mult.impute} must matched the names created by the merge
		}
}
\author{
  Frank Harrell  \cr
  Department of Biostatistics  \cr
  Vanderbilt University  \cr
  \email{fh@fharrell.com}
}
\references{
  Kuhfeld, Warren F: The PRINQUAL Procedure.  SAS/STAT User's Guide, Fourth
  Edition, Volume 2, pp. 1265--1323, 1990.

  Van Houwelingen JC, Le Cessie S: Predictive value of statistical models.
  Statistics in Medicine 8:1303--1325, 1990.

  Copas JB: Regression, prediction and shrinkage. JRSS B 45:311--354, 1983.

  He X, Shen L: Linear regression after spline transformation.
  Biometrika 84:474--481, 1997.

  Little RJA, Rubin DB: Statistical Analysis with Missing Data.  New
  York: Wiley, 1987.

  Rubin DJ, Schenker N: Multiple imputation in health-care databases: An
  overview and some applications.  Stat in Med 10:585--598, 1991.

  Faris PD, Ghali WA, et al:Multiple imputation versus data enhancement
  for dealing with missing data in observational health care outcome
  analyses.  J Clin Epidem 55:184--191, 2002.
}
\seealso{
  \code{\link{aregImpute}}, \code{\link{impute}}, \code{\link{naclus}},
  \code{\link{naplot}}, \code{\link[acepack]{ace}},
  \code{\link[acepack]{avas}}, \code{\link{cancor}},
  \code{\link{prcomp}}, \code{\link{rcspline.eval}},
  \code{\link{lsfit}}, \code{\link{approx}}, \code{\link{datadensity}},
  \code{\link[mice]{mice}}, \code{\link[ggplot2]{ggplot}},
	\code{\link[rms]{processMI}}
}
\examples{
\dontrun{
x <- cbind(age, disease, blood.pressure, pH)  
#cbind will convert factor object `disease' to integer
par(mfrow=c(2,2))
x.trans <- transcan(x, categorical="disease", asis="pH",
                    transformed=TRUE, imputed=TRUE)
summary(x.trans)  #Summary distribution of imputed values, and R-squares
f <- lm(y ~ x.trans$transformed)   #use transformed values in a regression
#Now replace NAs in original variables with imputed values, if not
#using transformations
age            <- impute(x.trans, age)
disease        <- impute(x.trans, disease)
blood.pressure <- impute(x.trans, blood.pressure)
pH             <- impute(x.trans, pH)
#Do impute(x.trans) to impute all variables, storing new variables under
#the old names
summary(pH)       #uses summary.impute to tell about imputations
                  #and summary.default to tell about pH overall
# Get transformed and imputed values on some new data frame xnew
newx.trans     <- predict(x.trans, xnew)
w              <- predict(x.trans, xnew, type="original")
age            <- w[,"age"]            #inserts imputed values
blood.pressure <- w[,"blood.pressure"]
Function(x.trans)  #creates .age, .disease, .blood.pressure, .pH()
#Repeat first fit using a formula
x.trans <- transcan(~ age + disease + blood.pressure + I(pH), 
                    imputed=TRUE)
age <- impute(x.trans, age)
predict(x.trans, expand.grid(age=50, disease="pneumonia",
        blood.pressure=60:260, pH=7.4))
z <- transcan(~ age + factor(disease.code),  # disease.code categorical
              transformed=TRUE, trantab=TRUE, imputed=TRUE, pl=FALSE)
ggplot(z, scale=TRUE)
plot(z$transformed)
}


# Multiple imputation and estimation of variances and covariances of
# regression coefficient estimates accounting for imputation
set.seed(1)
x1 <- factor(sample(c('a','b','c'),100,TRUE))
x2 <- (x1=='b') + 3*(x1=='c') + rnorm(100)
y  <- x2 + 1*(x1=='c') + rnorm(100)
x1[1:20] <- NA
x2[18:23] <- NA
d <- data.frame(x1,x2,y)
n <- naclus(d)
plot(n); naplot(n)  # Show patterns of NAs
f  <- transcan(~y + x1 + x2, n.impute=10, shrink=FALSE, data=d)
options(digits=3)
summary(f)


f  <- transcan(~y + x1 + x2, n.impute=10, shrink=TRUE, data=d)
summary(f)


h <- fit.mult.impute(y ~ x1 + x2, lm, f, data=d)
# Add ,fit.reps=TRUE to save all fit objects in h, then do something like:
# for(i in 1:length(h$fits)) print(summary(h$fits[[i]]))


diag(vcov(h))


h.complete <- lm(y ~ x1 + x2, na.action=na.omit)
h.complete
diag(vcov(h.complete))


# Note: had the rms ols function been used in place of lm, any
# function run on h (anova, summary, etc.) would have automatically
# used imputation-corrected variances and covariances


# Example demonstrating how using the multinomial logistic model
# to impute a categorical variable results in a frequency
# distribution of imputed values that matches the distribution
# of non-missing values of the categorical variable


\dontrun{
set.seed(11)
x1 <- factor(sample(letters[1:4], 1000,TRUE))
x1[1:200] <- NA
table(x1)/sum(table(x1))
x2 <- runif(1000)
z  <- transcan(~ x1 + I(x2), n.impute=20, impcat='multinom')
table(z$imputed$x1)/sum(table(z$imputed$x1))

# Here is how to create a completed dataset
d <- data.frame(x1, x2)
z <- transcan(~x1 + I(x2), n.impute=5, data=d)
imputed <- impute(z, imputation=1, data=d,
                  list.out=TRUE, pr=FALSE, check=FALSE)
sapply(imputed, function(x)sum(is.imputed(x)))
sapply(imputed, function(x)sum(is.na(x)))
}

# Do single imputation and create a filled-in data frame
z <- transcan(~x1 + I(x2), data=d, imputed=TRUE)
imputed <- as.data.frame(impute(z, data=d, list.out=TRUE))

# Example where multiple imputations are for basic variables and
# modeling is done on variables derived from these


set.seed(137)
n <- 400
x1 <- runif(n)
x2 <- runif(n)
y  <- x1*x2 + x1/(1+x2) + rnorm(n)/3
x1[1:5] <- NA
d <- data.frame(x1,x2,y)
w <- transcan(~ x1 + x2 + y, n.impute=5, data=d)
# Add ,show.imputed.actual for graphical diagnostics
\dontrun{
g <- fit.mult.impute(y ~ product + ratio, ols, w,
                     data=data.frame(x1,x2,y),
                     derived=expression({
                       product <- x1*x2
                       ratio   <- x1/(1+x2)
                       print(cbind(x1,x2,x1*x2,product)[1:6,])}))
}


# Here's a method for creating a permanent data frame containing
# one set of imputed values for each variable specified to transcan
# that had at least one NA, and also containing all the variables
# in an original data frame.  The following is based on the fact
# that the default output location for impute.transcan is
# given by the global environment


\dontrun{
xt <- transcan(~. , data=mine,
               imputed=TRUE, shrink=TRUE, n.impute=10, trantab=TRUE)
attach(mine, use.names=FALSE)
impute(xt, imputation=1) # use first imputation
# omit imputation= if using single imputation
detach(1, 'mine2')
}


# Example of using invertTabulated outside transcan
x    <- c(1,2,3,4,5,6,7,8,9,10)
y    <- c(1,2,3,4,5,5,5,5,9,10)
freq <- c(1,1,1,1,1,2,3,4,1,1)
# x=5,6,7,8 with prob. .1 .2 .3 .4 when y=5
# Within a tolerance of .05*(10-1) all y's match exactly
# so the distance measure does not play a role
set.seed(1)      # so can reproduce
for(inverse in c('linearInterp','sample'))
 print(table(invertTabulated(x, y, freq, rep(5,1000), inverse=inverse)))


# Test inverse='sample' when the estimated transformation is
# flat on the right.  First show default imputations
set.seed(3)
x <- rnorm(1000)
y <- pmin(x, 0)
x[1:500] <- NA
for(inverse in c('linearInterp','sample')) {
par(mfrow=c(2,2))
  w <- transcan(~ x + y, imputed.actual='hist',
                inverse=inverse, curtail=FALSE,
                data=data.frame(x,y))
  if(inverse=='sample') next
# cat('Click mouse on graph to proceed\n')
# locator(1)
}

\dontrun{
# While running multiple imputation for a logistic regression model
# Run the rms package validate and calibrate functions and save the
# results in w$funresults
a <- aregImpute(~ x1 + x2 + y, data=d, n.impute=10)
require(rms)
g <- function(fit)
  list(validate=validate(fit, B=50), calibrate=calibrate(fit, B=75))
w <- fit.mult.impute(y ~ x1 + x2, lrm, a, data=d, fun=g,
                     fitargs=list(x=TRUE, y=TRUE))
# Get all validate results in it's own list of length 10
r <- w$funresults
val <- lapply(r, function(x) x$validate)
cal <- lapply(r, function(x) x$calibrate)
# See rms processMI and https://hbiostat.org/rmsc/validate.html#sec-val-mival
}

\dontrun{
# Account for within-subject correlation using the robust cluster sandwich
# covariance estimate in conjunction with Rubin's rule for multiple imputation
# rms package must be installed
a <- aregImpute(..., data=d)
f <- fit.mult.impute(y ~ x1 + x2, lrm, a, n.impute=30, data=d, cluster=d$id)
# Get likelihood ratio chi-square tests accounting for missingness
a <- aregImpute(..., data=d)
h <- fit.mult.impute(y ~ x1 + x2, lrm, a, n.impute=40, data=d, lrt=TRUE)
processMI(h, which='anova')   # processMI is in rms
}
}
\keyword{smooth}
\keyword{regression}
\keyword{multivariate}
\keyword{methods}
\keyword{models}
\concept{bootstrap}
