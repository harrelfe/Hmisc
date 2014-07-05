subplot <- function(fun, x, y=NULL, size=c(1,1), vadj=0.5, hadj=0.5,
                    inset=c(0,0), type=c('plt','fig'), pars=NULL){

#  old.par <- par(no.readonly=TRUE)

    type <- match.arg(type)
    old.par <- par( c(type, 'usr', names(pars) ) )
    on.exit(par(old.par))

  if(missing(x)) x <- locator(2)

  if(is.character(x)) {
      if(length(inset) == 1) inset <- rep(inset,2)
      x.char <- x
      tmp <- par('usr')
      x <- (tmp[1]+tmp[2])/2
      y <- (tmp[3]+tmp[4])/2

      if( length(grep('left',x.char, ignore.case=TRUE))) {
          x <- tmp[1] + inset[1]*(tmp[2]-tmp[1])
          if(missing(hadj)) hadj <- 0
      }
      if( length(grep('right',x.char, ignore.case=TRUE))) {
          x <- tmp[2] - inset[1]*(tmp[2]-tmp[1])
          if(missing(hadj)) hadj <- 1
      }
      if( length(grep('top',x.char, ignore.case=TRUE))) {
          y <- tmp[4] - inset[2]*(tmp[4]-tmp[3])
          if(missing(vadj)) vadj <- 1
      }
      if( length(grep('bottom',x.char, ignore.case=TRUE))) {
          y <- tmp[3] + inset[2]*(tmp[4]-tmp[3])
          if(missing(vadj)) vadj <- 0
      }
  }

  xy <- xy.coords(x,y)

  if(length(xy$x) != 2){
    pin <- par('pin')
 #   tmp <- cnvrt.coords(xy$x[1],xy$y[1],'usr')$plt
    tmpx <- grconvertX( xy$x[1], to='npc' )
    tmpy <- grconvertY( xy$y[1], to='npc' )

    x <- c( tmpx - hadj*size[1]/pin[1],
            tmpx + (1-hadj)*size[1]/pin[1] )
    y <- c( tmpy - vadj*size[2]/pin[2],
            tmpy + (1-vadj)*size[2]/pin[2] )

 #   xy <- cnvrt.coords(x,y,'plt')$fig
    xyx <- grconvertX(x, from='npc', to='nfc')
    xyy <- grconvertY(y, from='npc', to='nfc')
  } else {
#    xy <- cnvrt.coords(xy,,'usr')$fig
      xyx <- grconvertX(x, to='nfc')
      xyy <- grconvertY(y, to='nfc')
  }

  par(pars)
  if(type=='fig'){
      par(fig=c(xyx,xyy), new=TRUE)
  } else {
      par(plt=c(xyx,xyy), new=TRUE)
  }
  fun
  tmp.par <- par(no.readonly=TRUE)

  return(invisible(tmp.par))
}
