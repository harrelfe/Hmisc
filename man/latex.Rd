\encoding{latin1}
\name{latex}
\alias{latex}
\alias{latex.default}
\alias{latex.function}
\alias{latex.list}
\alias{latexTranslate}
\alias{latexSN}
\alias{latexVerbatim}
\alias{dvi}
\alias{print.dvi}
\alias{dvi.latex}
\alias{dvips}
\alias{dvips.latex}
\alias{dvips.dvi}
\alias{dvigv}
\alias{dvigv.latex}
\alias{dvigv.dvi}
\alias{print.latex}
\alias{show.latex}
\alias{show.dvi}
\title{
  Convert an S object to LaTeX, and Related Utilities
}
\description{
\code{latex} converts its argument to a \code{.tex} file appropriate
for inclusion in a LaTeX2e document.  \code{latex} is a generic
function that calls one of \code{latex.default},
\code{latex.function}, \code{latex.list}. 

\code{latex.default}
does appropriate rounding and decimal alignment and produces a
file containing a LaTeX tabular environment to print the matrix or data.frame
\code{x} as a table.

\code{latex.function} prepares an S function for printing by issuing \code{sed}
commands that are similar to those in the
\code{S.to.latex} procedure in the \code{s.to.latex} package (Chambers and Hastie, 1993).

\code{latex.list} calls \code{latex} recursively for each element in the argument.

\code{latexTranslate} translates particular items in character
strings to LaTeX format, e.g., makes \code{a^2 = a$^2$} for superscript within
variable labels.   LaTeX names of greek letters (e.g., \code{"alpha"})
will have backslashes added if \code{greek==TRUE}.  Math mode is
inserted as needed. 
\code{latexTranslate} assumes that input text always has matches,
e.g. \code{[) [] (] ()}, and that surrounding  by \code{$$} is OK.

\code{latexSN} converts a vector floating point numbers to character
strings using LaTeX exponents.  Dollar signs to enter math mode are not
added.

\code{latexVerbatim} on an object executes the object's \code{print} method,
capturing the output for a file inside a LaTeX verbatim environment.

\code{dvi} uses the system \code{latex} command to compile LaTeX code produced
by \code{latex}, including any needed styles.  \code{dvi}
will put a documentclass\{report\} and end\{document\} wrapper
around a file produced by \code{latex}.  By default, the \code{geometry} LaTeX package is
used to omit all margins and to set the paper size to a default of
5.5in wide by 7in tall.  The result of \code{dvi} is a .dvi file.  To both
format and screen display a non-default size, use for example
\code{print(dvi(latex(x), width=3, height=4),width=3,height=4)}.  Note that
you can use something like \code{xdvi -geometry 460x650 -margins 2.25in
file} without changing LaTeX defaults to emulate this.

\code{dvips} will use the system \code{dvips} command to print the .dvi file to
the default system printer, or create a postscript file if \code{file}
is specified.

\code{dvigv} uses the system \code{dvips} command to convert the input object
to a .dvi file, and uses the system \code{dvips} command to convert it to
postscript.  Then the postscript file is displayed using Ghostview
(assumed to be the system command \code{gv}).

There are \code{show} methods for displaying typeset LaTeX
on the screen using the system \code{xdvi}
command.   If you \code{show} a LaTeX file created by
\code{latex} without running it through \code{dvi} using
\code{show.dvi(object)}, the 
\code{show} method will run it through \code{dvi} automatically.
These \code{show} 
methods are not S Version 4 methods so you have to use full names such
as \code{show.dvi} and \code{show.latex}.  Use the \code{print} methods for
more automatic display of typesetting, e.g. typing \code{latex(x)} will
invoke xdvi to view the typeset document.
}
\usage{
latex(object, title=first.word(deparse(substitute(object))), \dots)

\method{latex}{default}(object,
    title=first.word(deparse(substitute(object))),
    file=paste(title, ".tex", sep=""),
    append=FALSE, label=title,
    rowlabel=title, rowlabel.just="l", cgroup=NULL, n.cgroup=NULL,
    rgroup=NULL, n.rgroup=NULL,
    cgroupTexCmd="bfseries",
    rgroupTexCmd="bfseries",
    rownamesTexCmd=NULL,
    colnamesTexCmd=NULL,
    cellTexCmds=NULL,
    rowname, cgroup.just=rep("c",length(n.cgroup)),
    colheads=dimnames(cx)[[2]],
    extracolheads=NULL, extracolsize='scriptsize',
    dcolumn=FALSE, numeric.dollar=!dcolumn, cdot=FALSE,
    longtable=FALSE, draft.longtable=TRUE, ctable=FALSE, booktabs=FALSE,
    table.env=TRUE, here=FALSE, lines.page=40,
    caption=NULL, caption.lot=NULL, caption.loc=c('top','bottom'),
    double.slash=FALSE,
    vbar=FALSE, collabel.just=rep("c",nc), na.blank=TRUE,
    insert.bottom=NULL, first.hline.double=!(booktabs | ctable),
    where='!tbp', size=NULL,
    center=c('center','centering','none'),
    landscape=FALSE,
    multicol=TRUE,
    math.row.names=FALSE, math.col.names=FALSE,
    ...) # x is a matrix or data.frame

\method{latex}{function}(
	object,
	title=first.word(deparse(substitute(object))),
	file=paste(title, ".tex", sep=""),
	append=FALSE,
	assignment=TRUE,  type=c('example','verbatim'), \dots)

\method{latex}{list}(
           object,
           title=first.word(deparse(substitute(object))),
           file=paste(title, ".tex", sep=""),
           append=FALSE,
           label,
           caption,
           caption.lot,
           caption.loc=c('top','bottom'),
           \dots)

\method{print}{latex}(x, ...)

latexTranslate(object, inn=NULL, out=NULL, pb=FALSE, greek=FALSE, \dots)

latexSN(x)

latexVerbatim(x, title=first.word(deparse(substitute(x))),
    file=paste(title, ".tex", sep=""),
    append=FALSE, size=NULL, hspace=NULL,
    width=.Options$width, length=.Options$length, \dots)

dvi(object, \dots)
\method{dvi}{latex}(object, prlog=FALSE, nomargins=TRUE, width=5.5, height=7, \dots)
\method{print}{dvi}(x, \dots)
dvips(object, \dots)
\method{dvips}{latex}(object, \dots)
\method{dvips}{dvi}(object, file, \dots)
\method{show}{latex}(object)  # or show.dvi(object) or just object
dvigv(object, \dots)
\method{dvigv}{latex}(object, \dots)       # or gvdvi(dvi(object))
\method{dvigv}{dvi}(object, \ldots)
}
\arguments{
  \item{object}{
    For \code{latex}, any S object.  For \code{dvi} or \code{dvigv}, an object
    created by \code{latex}.  For \code{latexTranslate} is a vector of
    character strings to translate.
  }
  \item{x}{
    any object to be \code{print}ed verbatim for \code{latexVerbatim}.  For
    \code{latexSN} \code{x} is a numeric vector.
  }
  \item{title}{
    name of file to create without the \code{.tex} extension.  If this
    option is not set, value/string of \code{x} (see above) is printed
    in the top left corner of the table.  Set \code{title=''} to
    suppress this output.
  }
  \item{file}{
    name of the file to create.  The default file name is \code{x.tex} where
    \code{x} is the first word in the name of the argument for \code{x}.
    Set \code{file=""} to have the generated LaTeX code just printed to
    standard output.  This is especially useful when running under Sweave in
    R using its \code{results=tex} tag, to save having to manage many
    small external files.  When \code{file=""}, \code{latex} keeps track of
    LaTeX styles that are called for by creating or modifying an object
    \code{latexStyles} (in \code{.GlobalTemp} in R or in frame 0 in
    S-Plus).  \code{latexStyles} is a vector containing the base names of
    all the unique LaTeX styles called for so far in the current session.
    See the end of the examples section for a way to use this object to good
    effect.  For \code{dvips}, \code{file} is the name of an output
    postscript file.
  }
  \item{append}{
    defaults to \code{FALSE}. Set to \code{TRUE} to append output to an existing file.
  }
  \item{label}{
    a text string representing a symbolic label for the table for referencing
    in the LaTeX \code{\\label} and \code{\\ref} commands.
    \code{label} is only used if \code{caption} is given.
  }
  \item{rowlabel}{
    If \code{x} has row dimnames, \code{rowlabel} is a character string containing the
    column heading for the row dimnames. The default is the name of the
    argument for \code{x}.
  }
  \item{rowlabel.just}{
    If \code{x} has row dimnames, specifies the justification for printing them.
    Possible values are \code{"l"}, \code{"r"}, \code{"c"}. The heading (\code{rowlabel}) itself
    is left justified if \code{rowlabel.just="l"}, otherwise it is centered.
  }
  \item{cgroup}{
    a vector of character strings defining major column headings. The default is
    to have none.
  }
  \item{n.cgroup}{
    a vector containing the number of columns for which each element in
    cgroup is a heading.  For example, specify \code{cgroup=c("Major 1","Major 2")},
    \code{n.cgroup=c(3,3)} if \code{"Major 1"} is to span columns 1-3 and \code{"Major 2"} is
    to span columns 4-6.  \code{rowlabel} does not count in the column numbers.
    You can omit \code{n.cgroup} if all groups have the same number of columns.
  }
  \item{rgroup}{
    a vector of character strings containing headings for row groups.
    \code{n.rgroup} must be present when \code{rgroup} is given. The first \code{n.rgroup[1]}
    rows are sectioned off and \code{rgroup[1]} is used as a bold heading for
    them. The usual row dimnames (which must be present if \code{rgroup} is) are 
    indented. The next \code{n.rgroup[2]} rows are treated likewise, etc.
  }
  \item{n.rgroup}{
    integer vector giving the number of rows in each grouping. If \code{rgroup}
    is not specified, \code{n.rgroup} is just used to divide off blocks of
    rows by horizontal lines. If \code{rgroup} is given but \code{n.rgroup} is omitted,
    \code{n.rgroup} will default so that each row group contains the same number
    of rows.
  }
  \item{cgroupTexCmd}{A character string specifying a LaTeX command to be
    used to format column group labels.  The default, "bfseries", sets
    the current font to 'bold'.  It is possible to supply a vector of
    strings so that each column group label is formatted differently.
    Please note that the first item of the vector is used to format the
    title (even if a title is not used), and that there is an empty
    column between each column group. Currently the user needs to handle
    these issues.  Multiple effects can be achieved by creating custom
    LaTeX commands; for example,
    \code{\providecommand{\redscshape}{\color{red}\scshape}} creates a
    LaTeX command called \code{redscshape} that formats the text in red
    small-caps.
  }
  \item{rgroupTexCmd}{A character string specifying a LaTeX command to be
    used to format row group labels.  The default, "bfseries", sets the
    current font to 'bold'.  A vector of strings can be supplied to
    format each row group label differently.  Normal recycling applies
    if the vector is shorter than \code{n.rgroups}.  See also
    \code{cgroupTexCmd} above regarding multiple effects.
  }
  \item{rownamesTexCmd}{A character string specifying a LaTeX
    command to be used to format rownames. The default, NULL, applies no
    command.  A vector of different commands can also be supplied.
    See also \code{cgroupTexCmd} above regarding multiple effects.
  }
  \item{colnamesTexCmd}{A character string specifying a LaTeX command to be
    used to format column labels. The default, NULL, applies no command.
    It is possible to supply a vector of strings to format each column
    label differently. If column groups are not used, the first item in
    the vector will be used to format the title. Please note that if
    column groups are used the first item of \code{cgroupTexCmd} and not
    \code{colnamesTexCmd} is used to format the title, and that there
    are empty columns between each group. The user needs to allow for
    these issues when supplying a vector of commands.  See also
    \code{cgroupTexCmd} above regarding multiple effects.
  }
  \item{cellTexCmds}{A matrix of character strings which are LaTeX
    commands to be
    used to format each element, or cell, of the object.  The matrix
    must have the same NROW() and NCOL() as the object.  The default,
    NULL, applies no formats.  Empty strings also apply no formats, and
    one way to start might be to create a matrix of empty strings with
    \code{matrix(rep("", NROW(x) * NCOL(x)), nrow=NROW(x))} and then
    selectively change appropriate elements of the matrix.  Note that
    you might need to set \code{numeric.dollar=FALSE} (to disable math
    mode) for some effects to work. See also \code{cgroupTexCmd} above
    regarding multiple effects.
  }
  \item{na.blank}{
    Set to \code{TRUE} to use blanks rather than \code{NA} for missing values.
    This usually looks better in \code{latex}.
  }
  \item{insert.bottom}{
    an optional character string to typeset at the bottom of the table.
    For \code{"ctable"} style tables, this is placed in an unmarked footnote.
  }
  \item{first.hline.double}{
    set to \code{FALSE} to use single horizontal rules for styles other than
    \code{"bookmark"} or \code{"ctable"}
  }
  \item{rowname}{
    rownames for \code{tabular} environment.  Default is rownames of matrix or
    data.frame.  Specify \code{rowname=NULL} to suppress the use of row names.
  }
  \item{cgroup.just}{
    justification for labels for column groups.  Defaults to \code{"c"}.
  }
  \item{colheads}{a character vector of column headings if you don't want
    to use \code{dimnames(object)[[2]]}.  Specify \code{colheads=NULL} to
    suppress column headings.}
  \item{extracolheads}{
    an optional vector of extra column headings that will appear under the
    main headings (e.g., sample sizes).  This character vector does not
    need to include an empty space for any \code{rowname} in effect, as
    this will be added automatically.  You can also form subheadings by
    splitting character strings defining the column headings using the
    usual backslash \code{n} newline character.}
  \item{extracolsize}{
    size for \code{extracolheads} or for any second lines in column names;
    default is \code{"scriptsize"} 
  }
  \item{dcolumn} {see \code{format.df}}
  \item{numeric.dollar}{
    logical, default \code{!dcolumn}.  Set to \code{TRUE} to place dollar
    signs around numeric values when \code{dcolumn=FALSE}.  This 
    assures that \code{latex} will use minus signs rather than hyphens to indicate
    negative numbers.  Set to \code{FALSE} when \code{dcolumn=TRUE}, as
    \code{dcolumn.sty} automatically uses minus signs.
  }
  \item{math.row.names}{
    logical, set true to place dollar signs around the row names.
  }
  \item{math.col.names}{
    logical, set true to place dollar signs around the column names.
  }
  \item{cdot}{see \code{\link{format.df}}}
  \item{longtable}{
    Set to \code{TRUE} to use David Carlisle's LaTeX \code{longtable} style, allowing
    long tables to be split over multiple pages with headers repeated on
    each page.
    The \code{"style"} element is set to \code{"longtable"}. The \code{latex} \code{\\usepackage}
    must reference \code{[longtable]}.
    The file \code{longtable.sty} will
    need to be in a directory in your \code{$TEXINPUTS} path.
  }
  \item{draft.longtable}{
    I forgot what this does.
  }
  \item{ctable}{
    set to \code{TRUE} to use Wybo Dekker's \code{ctable} style from
    \code{CTAN}.  Even though for historical reasons it is not the
    default, it is generally the preferred method.  Thicker but not
    doubled \code{hline}s are used to start a table when \code{ctable} is
    in effect.
  }
  \item{booktabs}{
    set \code{booktabs=TRUE} to use the \code{booktabs} style of horizontal
    rules for better tables.  In this case, double \code{hline}s are not
    used to start a table.
  }
  \item{table.env}{
    Set \code{table.env=FALSE} to suppress enclosing the table in a LaTeX
    \code{table} environment.  \code{table.env} only applies when
    \code{longtable=FALSE}.  You may not specify a \code{caption} if
    \code{table.env=FALSE}.
  }
  \item{here}{
    Set to \code{TRUE} if you are using \code{table.env=TRUE} with \code{longtable=FALSE} and you
    have installed David Carlisle's \code{here.sty} LaTeX style. This will cause
    the LaTeX \code{table} environment to be set up with option \code{H} to guarantee
    that the table will appear exactly where you think it will in the text.
    The \code{"style"} element is set to \code{"here"}. The \code{latex} \code{\\usepackage}
    must reference \code{[here]}.  The file \code{here.sty} will
    need to be in a directory in your \code{$TEXINPUTS} path.  \code{here} is
    largely obsolete with LaTeX2e.
  }
  \item{lines.page}{
    Applies if \code{longtable=TRUE}. No more than \code{lines.page} lines in the body
    of a table will be placed on a single page. Page breaks will only
    occur at \code{rgroup} boundaries.
  }
  \item{caption}{
    a text string to use as a caption to print at the top of the first
    page of the table. Default is no caption.
  }
  \item{caption.lot}{
    a text string representing a short caption to be used in the "List of Tables".
    By default, LaTeX will use \code{caption}.  If you get inexplicable \code{latex} errors,
    you may need to supply \code{caption.lot} to make the errors go away.
  }
  \item{caption.loc}{set to \code{"bottom"} to position a caption below
    the table instead of the default of \code{"top"}.}
  \item{double.slash}{
    set to \code{TRUE} to output \code{\\} as \code{\\\\} in LaTeX commands. Useful when you
    are reading the output file back into an S vector for later output.
  }
  \item{vbar}{
    logical. When \code{vbar==TRUE}, columns in the tabular environment are separated with
    vertical bar characters.  When \code{vbar==FALSE}, columns are separated with white
    space.  The default, \code{vbar==FALSE}, produces tables consistent with the style sheet
    for the Journal of the American Statistical Association.
  }
  \item{collabel.just}{
    justification for column labels.
  }
  \item{assignment}{
    logical.  When \code{TRUE}, the default, the name of the function and the assignment
    arrow are printed to the file.
  }
  \item{where}{
    specifies placement of floats if a table environment is used.  Default
    is \code{"!tbp"}.  To allow tables to appear in the middle of a page of
    text you might specify \code{where="!htbp"} to \code{latex.default}.
  }
  \item{size}{
    size of table text if a size change is needed (default is no change).
    For example you might specify \code{size="small"} to use LaTeX font size
    "small".
  }
  \item{center}{
    default is \code{"center"} to enclose the table in a \code{center}
    environment.  Use \code{center="centering"} to instead use a LaTeX
    \code{centering} directive, or \code{center="none"} to use no
    centering.  This option was implemented by Markus Jäntti
    \email{markus.jantti@iki.fi} of Abo Akademi University.
  }
  \item{landscape}{
    set to \code{TRUE} to enclose the table in a \code{landscape}
    environment.  When \code{ctable} is \code{TRUE}, will use the
    \code{rotate} argument to \code{ctable}.
  }
  \item{type}{
    The default uses the S \code{Example} environment for \code{latex.function},
    assuming you have installed \code{S.sty} in a location that the system
    latex command automatically accesses.  Set \code{type="verbatim"} to
    instead use the LaTeX \code{verbatim} environment.
  }
  \item{\dots}{
    other arguments are accepted and ignored except that \code{latex}
    passes arguments to \code{format.df} (e.g., \code{col.just} and other
    formatting options like \code{dec}, \code{rdec}, and \code{cdec}).  For
    \code{latexVerbatim} these arguments are passed to the \code{print}
    function.  Ignored for \code{latexTranslate}.
  }
  \item{inn, out}{
    specify additional input and translated strings over the usual
    defaults
  }
  \item{pb}{
    If \code{pb}=\code{TRUE}, \code{latexTranslate} also translates \code{[()]} to math mode using
    \code{\left, \right}.
  }
  \item{greek}{set to \code{TRUE} to have \code{latexTranslate} put names
    for greek letters in math mode and add backslashes}
  \item{hspace}{
    horizontal space, e.g., extra left margin for verbatim text.  Default
    is none.  Use e.g. \code{hspace="10ex"} to add 10 extra spaces to the left
    of the text.
  }
  \item{length}{for S-Plus only; is the length of the output page for
    printing and capturing verbatim text}
  \item{width}{}
  \item{height}{
    are the \code{options( )} to have in effect only for when \code{print} is
    executed.  Defaults are current \code{options}.  For \code{dvi} these specify
    the paper width and height in inches if \code{nomargins=TRUE}, with
    defaults of 5.5 and 7, respectively.
  }
  \item{prlog}{
    set to \code{TRUE} to have \code{dvi} print, to the S-Plus session, the LaTeX .log
    file.
  }
  \item{multicol}{
    set  to \code{FALSE} to not use \code{\multicolumn} in header
    of table
  }

  \item{nomargins}{
    set to \code{FALSE} to use default LaTeX margins when making the .dvi file
  }
}
\value{
\code{latex} and \code{dvi} return a
list of class \code{latex} or \code{dvi} containing character string
elements \code{file} and \code{style}.  \code{file} contains the name of the
generated file, and \code{style} is a vector (possibly empty) of styles to
be included using the LaTeX2e \code{\usepackage} command.

\code{latexTranslate} returns a vector of character strings
}

\section{Side Effects}{
creates various system files and runs various Linux/UNIX system
commands which are assumed to be in the system path.
}
\details{
  If running under Windows and using MikTeX, \code{latex} and \code{yap}
  must be in your system path, and \code{yap} is used to browse
  \code{.dvi} files created by \code{latex}.  You should install the
  \code{geometry} and \code{ctable} styles in MikTeX to make optimum use
  of \code{latex()}.

  System options can be used to specify external commands to be used.
  Defaults are given by \code{options(xdvicmd='xdvi')} or
  \code{options(xdvicmd='yap')}, \code{options(dvipscmd='dvips')},
  \code{options(latexcmd='latex')}.  For MacOS specify
  \code{options(xdvicmd='MacdviX')}.
  
  If running S-Plus and your directory for temporary files is not
  \code{/tmp} (Unix/Linux) or \code{\\windows\\temp} (Windows), add your
  own \code{tempdir} function such as \code{
	tempdir <- function() "/yourmaindirectory/yoursubdirectory"}

  To prevent the latex file from being displayed store the result of
  \code{latex} in an object, e.g. \code{w <- latex(object, file='foo.tex')}.
  }
\author{
Frank E. Harrell, Jr.,
\cr
Department of Biostatistics,
\cr
Vanderbilt University,
\cr
\code{f.harrell@vanderbilt.edu}


Richard M. Heiberger,
\cr
Department of Statistics,
\cr
Temple University, Philadelphia, PA.
\cr
\code{rmh@astro.ocis.temple.edu}

David R. Whiting,
\cr
School of Clinical Medical Sciences (Diabetes),
\cr
University of Newcastle upon Tyne, UK.
\cr
\code{david.whiting@ncl.ac.uk}


}
\seealso{
\code{\link{html}}, \code{\link{format.df}}, \code{\link[tools]{texi2dvi}}
}
\examples{
x <- matrix(1:6, nrow=2, dimnames=list(c('a','b'),c('c','d','enLine 2')))
\dontrun{
latex(x)   # creates x.tex in working directory
w <- latex(x, file='/tmp/my.tex')
d <- dvi(w)  # compile LaTeX document, make .dvi
             # latex assumed to be in path
d            # or show(d) : run xdvi (assumed in path) to display
w            # or show(w) : run dvi then xdvi
dvips(d)     # run dvips to print document
dvips(w)     # run dvi then dvips
library(tools)
texi2dvi('/tmp/my.tex')   # compile and produce pdf file in working dir.
}
latex(x, file="")   # just write out LaTeX code to screen

\dontrun{
# After running latex( ) multiple times with different special styles in
# effect, make a file that will call for the needed LaTeX packages when
# latex is run (especially when using Sweave with R)
if(exists(latexStyles))
  cat(paste('\\usepackage{',latexStyles,'}',sep=''),
      file='stylesused.tex', sep='\n')
# Then in the latex job have something like:
# \documentclass{article}
# \input{stylesused}
# \begin{document}
# ...
}
}
\keyword{utilities}
\keyword{interface}
\keyword{methods}
\keyword{file}
\keyword{character}
\keyword{manip}
