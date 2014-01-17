show.pch <- function(object=par('font'))
{
  plot(0,0,xlim=c(-1,11),ylim=c(0,26),type='n',axes=FALSE,xlab='',ylab='')
  j <- -1
  for(i in 0:253) {
    if(i %% 25==0) {
      j <- j+1;
      k <- 26
    }

    k <- k-1
    points(j, k, pch=i, font=object)
    text(j+.45, k, i)
  }

  invisible()
}


character.table <- function(font=1)
{
  ## Prints numeric equivalents to all latin characters
  ## Usage: graphsheet(orientation = "portrait")
  ##        character.table()
  ## Print the resulting graphsheet.  The printed version doesn't allways
  ## corresponds to the screen display.  The character on line "xy" and column "z"
  ## of the table has code "xyz".
  ## These codes can be used as any other characters. e.g.
  ##  title("\347\340 et \340")
  ## As the command line window of Splus can't print special characters
  ##  cat("\347\340 et \340")
  ## will not print the special characters, at least under 4.5 and under 2000.
  ##
  ## Author:
  ## Pierre Joyet / Aktuariat                  pierre.joyet@bluewin.ch

  v <- 40:377
  v <- v[v %% 100 < 80 & v %% 10 < 8]
  opar <- par(mar = c(5, 5, 4, 2) + 0.1, xpd=NA)
  plot(0:7, seq(4, 31, length = 8), type = "n", axes = FALSE, xlab = "",
       ylab = "")
  k <- 1
  for(i in 4:31)
    for(j in 0:7) {
      text(j, 35 - i, eval(parse(text = paste("\"\\", v[k], "\"",
                                              sep = ""))), font = font)
      k <- k + 1
    }

  text(0:7, rep(33, 7), as.character(0:7), font = 3)
  text(rep(-1, 28), 31:4, as.character(c(4:7, 10:17, 20:27, 30:37)),
       font = 3)
  par(opar)
  invisible()
}


show.col <- function(object=NULL)
{
  plot(0,0,xlim=c(-1,10),ylim=c(0,10),type='n',axes=FALSE,xlab='',ylab='')
  j <- -1
  for(i in 0:99) {
    if(i %% 10==0) {
      j <- j+1;
      k <- 10
    }

    k <- k-1
    points(j, k, pch=15, col=i, cex=3)
    text(j+.45, k, i)
  }

  invisible()
}
