default.arg <- function(arg) {
  formal.args <- formals(sys.function(sys.parent()))
  eval(formal.args[[deparse(substitute(arg))]])
}

