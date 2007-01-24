rcspline.restate <- function(knots, coef, type=c("ordinary","integral"),
                             x="X", lx=nchar(x),norm=2, 
                             columns=65, before="& &", after="\\", 
                             begin="", nbegin=0,
                             digits=max(8,.Options$digits))
{
  type <- match.arg(type)
  k <- length(knots)
  if(k<3)
    stop("must have >=3 knots in a restricted cubic spline")
  
  p <- length(coef)
  if(p == k) {
    Intc <- coef[1]
    coef <- coef[-1]
    p <- p-1
  } else Intc <- 0
  
  if(k-1 != p)
    stop("coef must be of length # knots - 1")

  knotnk <- knots[k];
  knotnk1 <- knots[k-1];
  knot1 <- knots[1]
  
  if(norm==0)
    kd <- 1
  else if(norm==1)
    kd <- (knotnk-knotnk1)^3
  else
    kd <- (knotnk-knot1)^2
  
  coef[-1] <- coef[-1]/kd

  d <- c(0, knots-knotnk)[1:p]
  coefk <- sum(coef*d)/(knotnk-knotnk1)

  d <- c(0, knots-knotnk1)[1:p]
  coefk1 <- sum(coef*d)/(knotnk1-knotnk)

  if(is.null(names(coef)))
    names(coef) <- paste(x,1:length(coef),sep="")
  
  coef <- c(coef, coefk, coefk1)
  names(coef)[k] <- "1st restricted coef"
  names(coef)[k+1] <- "2nd restricted coef"

  if(type=="integral")
    coef <- c(.5*coef[1],.25*coef[-1])

  cof <- format.sep(coef, digits)
kn <- format.sep(-knots, digits)
  if(Intc!=0) {
    txt <- txt2 <- format.sep(Intc, digits)
    if(type=="integral") {
      txt <- paste(txt, "* x")
      txt2 <- paste(txt2, '*', x)
    }
    
    if(coef[1]>=0) {
      txt <- paste(txt, "+");
      txt2 <- paste(txt2, '+')
    }
  } else txt <- txt2 <- ""

  if(cof[1]!=0) {
    txt <- paste(txt, cof[1],
                 if(type=="ordinary")"* x"
                 else "* x^2",
                 sep="")
    
    txt2 <- paste(txt2, cof[1],
                  if(type=="ordinary") paste("*",x)
                  else paste("*",x,"^2"),
                  sep="")
  }
  
  for(i in 2:(p+2)) {
    nam <- paste("pmax(x",
                 if(knots[i-1]<0) "+"
                 else NULL, 
                 if(knots[i-1]!=0) kn[i-1]
                 else NULL,
                 ",0)^",
                 if(type=="ordinary")"3"
                 else "4",
                 sep="")
    
    nam2 <- paste("pmax(",x,
                  if(knots[i-1]<0) "+"
                  else NULL,
                  if(knots[i-1]!=0) kn[i-1]
                  else NULL,
                  ",0)^",
                  if(type=="ordinary")"3"
                  else "4",
                  sep="")
    
    z <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+"
               else NULL,
               cof[i], "*", nam, sep="")
    
    z2 <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+"
                else NULL,
		cof[i], "*", nam2, sep="")
    
    txt <- paste(txt , z,  sep="")
    txt2<- paste(txt2, z2, sep="")
  }

  ## func <- function(x) NULL
  ## func[[2]] <- parse(text=txt)[[1]]
  func <- parse(text=paste('function(x)', txt))  ## 11Apr02

  cof <- format.sep(coef, digits)
  kn <- format.sep(-knots, digits)

  lcof <- nchar(cof)
  ##cof <- sys('sed -e "s/e+00//" -e "s/e-0\\(.\\)/\\\\!\\\\times\\\\!10^{-\\1}/" 
  ##-e "s/e-\\(..\\)/\\\\!\\\\times\\\\!10^{-\\1}/" 
  ##-e "s/e+0\\(.\\)/\\\\!\\\\times\\\\!10^{\\1}/" 
  ##-e "s/e+\\(..\\)/\\\\!\\\\times\\\\!10^{\\1}/"', cof)
  cof <- sedit(cof, c('e+00','e-0*',                'e-*',
                      'e+0*',               'e+*'),
               c('',    '\\\\!\\times\\\\!10^{-*}','\\\\!\\times\\\\!10^{-*}',
                 '\\\\!\\times\\\\!10^{*}','\\\\!\\times\\\\!10^{*}'))
  
  cur <- begin; colcnt <- nbegin; tex <- NULL
  if(Intc!=0) {
    fint <- format.sep(Intc, digits)
    if(type=="integral") {
      fint <- paste(fint, x);
      colcnt <- colcnt+2
    }
    
    cur <- paste(cur, fint, sep="")
    colcnt <- colcnt + nchar(fint)
    if(coef[1]>0) {
      cur <- paste(cur, " + ", sep="");
      colcnt <- colcnt+3
    }
  }
  
  if(coef[1]!=0) {
    ##   sp <- if(length(grep("times",cof[1]))==0) "\\:" else NULL
    sp <- if(substring.location(cof[1],"times")$first > 0) "\\:"
          else NULL
    
    cur <- paste(cur, cof[1], sp, x,
                 if(type=="integral") "^2",
                 sep="")
    
    ##\:=medium space in LaTeX
    colcnt <- colcnt+lcof[1]+lx+(type=="integral")
  }

  tex.names <- character(p+2)
  size <- lx+lcof[-1]+nchar(kn)+3

  for(i in 2:(p+2)) {
    nam <- paste("(", x,
                 if(knots[i-1]<0) "+"
                 else NULL,
                 if(knots[i-1]!=0) kn[i-1]
                 else NULL, 
                 ")_{+}^{",
                 if(type=="ordinary")"3}"
                 else "4}",
                 sep="")
    
    q <- paste(if(coef[i]>0 & (i>2 | coef[1]!=0 | Intc!=0)) "+"
               else NULL,
               cof[i], nam, sep="")
    
    n <- size[i-1]
    if(colcnt+n > columns) {
      tex <- c(tex, cur)
      cur <- ""
      colcnt <- 0
    }
    
    cur <- paste(cur, q, sep="")
    colcnt <- colcnt+n
  }

  tex <- c(tex, cur)
  tex <- paste(before, tex, after)

  if(Intc!=0) coef <- c(Intercept=Intc, coef)

  attr(coef, "knots") <- knots
  attr(coef, "function") <- func
  attr(coef, "function.text") <- txt2
  ##attr(tex, "class")  <- "TeX"
  attr(coef, "latex")   <- tex
  names(colcnt) <- NULL
  attr(coef, "columns.used") <- colcnt
  
  coef
}
