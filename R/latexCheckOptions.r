latexCheckOptions <- function(...) {
if (any(sapply(options()[c("latexcmd","dviExtension","xdvicmd")], is.null)))
  stop("This example uses the pdflatex system command and R's pdf() graphics\n",
       "device and therefore requires that the three options\n",
       "      options()[c(\"latexcmd\",\"dviExtension\",\"xdvicmd\")]\n",
       "all be set to non-NULL values.  Please see the comments in the \"Details\"\n",
       "section of ?microplot::microplot for some recommendations, and the\n",
       "\"System options\" paragraph in the \"Details\" section of ?Hmisc::latex\n",
       "for full discussion of the options available and suggested values for\n",
       "several operating systems.  If you wish to use the latex system command\n",
       "and a compatible graphics device, see the discussion in ?Hmisc::latex",
       call.=FALSE)
}
