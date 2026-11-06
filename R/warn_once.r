## Session-level record of warnings already issued by warn_once().  An
## environment is used because its contents can be modified even though the
## package namespace binding is locked.  It is empty at install time and at
## the start of every R session.
.warned <- new.env(parent = emptyenv())

#' Issue a Warning Only Once Per Session
#'
#' Issues a warning the first time a given warning is requested in an R
#' session and silently ignores later requests for the same warning.  This is
#' useful for advisory warnings (e.g., recommending a better method) that
#' would otherwise be repeated many times, for example when a function is
#' called inside a bootstrap loop.
#'
#' @details
#' Warnings are identified by `id`, which by default is the message text
#' itself, so two calls with identical text are treated as the same warning.
#' Supply `id` explicitly when the message text varies from call to call (for
#' example, when it contains a sample size) but the warning should still be
#' shown only once.
#'
#' The record of warnings already issued is kept in an environment inside the
#' package namespace, so it lasts for the current R session only and is empty
#' when R starts.  Nothing is written to the global environment.
#'
#' Unlike `options(warn = -1)`, which silences all warnings, only repeats of
#' the warning identified by `id` are suppressed.  Warnings issued by
#' [base::warning()] and other functions are unaffected.
#'
#' @param msg character string containing the warning message.  Multiple
#'   strings are pasted together with no separator, as in [base::warning()].
#'   May be omitted when `reset = TRUE` and all records are to be cleared.
#' @param id character string identifying the warning for the purpose of
#'   deciding whether it has already been issued.  Defaults to `msg`.
#' @param reset set to `TRUE` to forget that a warning has been issued so that
#'   it will be issued again the next time it is requested.  If `msg` and `id`
#'   are both omitted, the records of all warnings are cleared; otherwise only
#'   the warning identified by `id` is cleared.  No warning is issued when
#'   `reset = TRUE`.  Mainly useful in examples and tests.
#' @param call. logical; if `TRUE` the call that invoked `warn_once` is
#'   included in the warning.  Default is `FALSE`.
#' @param immediate. logical; if `TRUE` the warning is printed immediately
#'   rather than being deferred until the top-level call completes.  Passed to
#'   [base::warning()].
#'
#' @return Invisibly, `TRUE` if a warning was issued and `FALSE` if it was
#'   suppressed because it had already been issued.  When `reset = TRUE`,
#'   invisibly `NULL`.
#'
#' @author Frank Harrell
#'
#' @seealso [base::warning()], [base::suppressWarnings()]
#'
#' @examples
#' warn_once('method="loess" is recommended', reset = TRUE)
#' f <- function() {
#'   warn_once('method="loess" is recommended')
#'   invisible()
#' }
#' f()   # issues the warning
#' f()   # silent: same text has already been warned about
#' f()   # silent
#'
#' ## Different text, but treated as the same warning because of id
#' g <- function(n) {
#'   warn_once(paste('n =', n, 'is small'), id = 'small.n')
#'   invisible()
#' }
#' g(10)  # issues the warning
#' g(12)  # silent
#'
#' ## Make the warning appear again
#' warn_once(reset = TRUE)
#' f()   # issues the warning again
#'
#' @keywords utilities
#' @concept warnings
#' @export
warn_once <- function(msg, id = msg, reset = FALSE, call. = FALSE,
                      immediate. = FALSE) {
  if (reset) {
    if (missing(msg) && missing(id)) {
      rm(list = ls(.warned, all.names = TRUE), envir = .warned)
    } else {
      key <- paste0('w:', paste0(id, collapse = ''))
      if (exists(key, envir = .warned, inherits = FALSE))
        rm(list = key, envir = .warned)
    }
    return(invisible(NULL))
  }

  msg <- paste0(msg, collapse = '')
  key <- paste0('w:', paste0(id, collapse = ''))

  if (exists(key, envir = .warned, inherits = FALSE))
    return(invisible(FALSE))

  assign(key, TRUE, envir = .warned)
  warning(msg, call. = call., immediate. = immediate.)
  invisible(TRUE)
}
