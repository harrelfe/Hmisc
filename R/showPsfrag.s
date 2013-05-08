showPsfrag <- function(filename)
{
  file <- paste(as.character(substitute(filename)),'ps',sep='.')
  out <- "TEMPltx"
  cat('\\documentclass{article}',
      '\\usepackage{graphics}',
      '\\usepackage[scanall]{psfrag}',
      '\\begin{document}',
      paste('\\includegraphics{',file,'}',sep=''),
      '\\end{document}',sep='\n', file=paste(out,'tex',sep='.'))
  sys(paste('latex "\\scrollmode\\input" ',out,';dvips -o ',out,'.ps ',out,
            '; gv ',out,'.ps  &',
            sep=''))
  unlink(paste(out,c('tex','log','dvi','ps','aux','pfg'),sep='.'))
  invisible()
}
