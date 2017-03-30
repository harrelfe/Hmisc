t.test.cluster <- function(y, cluster, group, conf.int=.95)
{
  ## See:
  ## Donner A, Birkett N, Buck C, Am J Epi 114:906-914, 1981.
  ## Donner A, Klar N, J Clin Epi 49:435-439, 1996.
  ## Hsieh FY, Stat in Med 8:1195-1201, 1988.

  group <- as.factor(group)
  cluster <- as.factor(cluster)
  s <- !(is.na(y)|is.na(cluster)|is.na(group))
  y <- y[s];
  cluster <- cluster[s];
  group <- group[s]
  n <- length(y)

  if(n<2)
    stop("n<2")

  gr <- levels(group)
  if(length(gr)!=2)
    stop("must have exactly two treatment groups")

  n <- table(group)
  nc <- tapply(cluster, group, function(x)length(unique(x)))
  bar <- tapply(y, group, mean)

  u <- unclass(group)
  y1 <- y[u==1];
  y2 <- y[u==2]
  
  c1 <- factor(cluster[u==1]);
  c2 <- factor(cluster[u==2]) #factor rids unused lev
  
  b1 <- tapply(y1, c1, mean);
  b2 <- tapply(y2, c2, mean)
  
  m1 <- table(c1);
  m2 <- table(c2)
  
  if(any(names(m1)!=names(b1)))
    stop("logic error 1")
  
  if(any(names(m2)!=names(b2)))
    stop("logic error 2")
  
  if(any(m2 < 2))
    stop(paste('The following clusters contain only one observation:',
               paste(names(m2[m2 < 2]), collapse=' ')))

  M1 <- mean(y1);
  M2 <- mean(y2)
  
  ssc1 <- sum(m1*((b1-M1)^2));
  ssc2 <- sum(m2*((b2-M2)^2))
  
  if(nc[1]!=length(m1))
    stop("logic error 3")
  
  if(nc[2]!=length(m2))
    stop("logic error 4")
  
  df.msc <- sum(nc)-2
  msc <- (ssc1+ssc2)/df.msc
  v1 <- tapply(y1,c1,var);
  v2 <- tapply(y2,c2,var)
  
  ssw1 <- sum((m1-1)*v1);
  ssw2 <- sum((m2-1)*v2)
  
  df.mse <- sum(n)-sum(nc)
  mse <- (ssw1+ssw2)/df.mse
  na <- (sum(n)-(sum(m1^2)/n[1]+sum(m2^2)/n[2]))/(sum(nc)-1)
  rho <- (msc-mse)/(msc+(na-1)*mse)
  r <- max(rho, 0)
  C1 <- sum(m1*(1+(m1-1)*r))/n[1]
  C2 <- sum(m2*(1+(m2-1)*r))/n[2]
  v <- mse*(C1/n[1]+C2/n[2])
  v.unadj <- mse*(1/n[1]+1/n[2])
  de <- v/v.unadj
  dif <- diff(bar)
  se <- sqrt(v)
  zcrit <- qnorm((1+conf.int)/2)
  cl <- c(dif-zcrit*se, dif+zcrit*se)
  z <- dif/se
  P <- 2*pnorm(-abs(z))

  
  stats <-
    matrix(NA, nrow=20, ncol=2,
           dimnames=list(c("N","Clusters","Mean",
                           "SS among clusters within groups",
                           "SS within clusters within groups",
                           "MS among clusters within groups","d.f.",
                           "MS within clusters within groups","d.f.",
                           "Na","Intracluster correlation",
                           "Variance Correction Factor","Variance of effect",
                           "Variance without cluster adjustment","Design Effect",
                           "Effect (Difference in Means)",
                           "S.E. of Effect",paste(format(conf.int),"Confidence limits"),
                           "Z Statistic","2-sided P Value"), gr))

  stats[1,] <- n
  stats[2,] <- nc
  stats[3,] <- bar
  stats[4,] <- c(ssc1, ssc2)
  stats[5,] <- c(ssw1, ssw2)
  stats[6,1] <- msc
  stats[7,1] <- df.msc
  stats[8,1] <- mse
  stats[9,1] <- df.mse
  stats[10,1] <- na
  stats[11,1] <- rho
  stats[12,] <- c(C1, C2)
  stats[13,1] <- v
  stats[14,1] <- v.unadj
  stats[15,1] <- de
  stats[16,1] <- dif
  stats[17,1] <- se
  stats[18,] <- cl
  stats[19,1] <- z
  stats[20,1] <- P

  attr(stats,'class') <- "t.test.cluster"
  stats  
}

print.t.test.cluster <- function(x, digits, ...)
{
  ##   if(!missing(digits)).Options$digits <- digits      6Aug00
  if(!missing(digits)) {
	oldopt <- options('digits')
    options(digits=digits)
    on.exit(options(oldopt))
  }

  cstats <- t(apply(x,1,format))
  ##   cstats <- format(x)
  attr(cstats,'class') <- NULL
  cstats[is.na(x)] <- ""
  invisible(print(cstats, quote=FALSE))
}
