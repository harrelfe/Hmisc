\name{print.char.list}
\alias{print.char.list}
\title{ prints a list of lists in a visually readable format. }
\description{
  Takes a list that is composed of other lists and matrixes and prints
  it in a visually readable format.
}
\usage{
\method{print}{char.list}(x, \dots, hsep = c("|"), vsep = c("-"), csep = c("+"), print.it = TRUE,
                rowname.halign = c("left", "centre", "right"),
                rowname.valign = c("top", "centre", "bottom"),
                colname.halign = c("centre", "left", "right"),
                colname.valign = c("centre", "top", "bottom"),
                text.halign = c("right", "centre", "left"),
                text.valign = c("top", "centre", "bottom"),
                rowname.width, rowname.height,
                min.colwidth = .Options$digits, max.rowheight = NULL,
                abbreviate.dimnames = TRUE, page.width = .Options$width,
                colname.width, colname.height, prefix.width,
                superprefix.width = prefix.width)
}
\arguments{
  \item{x}{
    list object to be printed
  }
  \item{\dots}{
    place for extra arguments to reside.
  }
  \item{hsep}{
    character used to separate horizontal fields
  }
  \item{vsep}{
    character used to separate veritcal feilds
  }
  \item{csep}{
    character used where horizontal and veritcal separators meet.
  }
  \item{print.it}{
    should the value be printed to the console or returned as a string.
  }
  \item{rowname.halign}{
    horizontal justification of row names.
  }
  \item{rowname.valign}{
    verical justification of row names.
  }
  \item{colname.halign}{
    horizontal justification of column names.
  }
  \item{colname.valign}{
    verical justification of column names.
  }
  \item{text.halign}{
    horizontal justification of cell text.
  }
  \item{text.valign}{
    vertical justification of cell text.
  }
  \item{rowname.width}{
    minimum width of row name strings.
  }
  \item{rowname.height}{
    minimum height of row name strings.
  }
  \item{min.colwidth}{
    minimum column width.
  }
  \item{max.rowheight}{
    maximum row height.
  }
  \item{abbreviate.dimnames}{
    should the row and column names be abbreviated.
  }
  \item{page.width}{
    width of the page being printed on.
  }
  \item{colname.width}{
    minimum width of the column names.
  }
  \item{colname.height}{
    minimum height of the column names
  }
  \item{prefix.width}{
    maximum width of the rowname columns
  }
  \item{superprefix.width}{
    maximum width of the super rowname columns
  }
}
\value{
  String that formated table of the list object.
}
\author{ Charles Dupont }
\keyword{ print }
\keyword{ list }
