escapeBS <- function(string) {
  gsub('\\\\', '\\\\\\\\\\', string)
}

escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}
