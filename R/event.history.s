## event.history-sim-request.txt: s-plus code to make event history graphs
##   (for distribution, including SIM readers)
##   last edited: 09-28-01

## start event.history function 
## --> assume data is approporately pre-processed (e.g., smoothed) 
##     prior to function call


event.history <- function(data, survtime.col, surv.col, 
                          surv.ind = c(1,0), 
                          subset.rows = NULL, 
                          covtime.cols = NULL, cov.cols = NULL, 
                          num.colors = 1, cut.cov = NULL, colors = 1, 
                          cens.density = 10, mult.end.cens = 1.05,
                          cens.mark.right = FALSE, cens.mark = '-', 
                          cens.mark.ahead = .5, cens.mark.cutoff = -1e-8, cens.mark.cex = 1.0, 
                          x.lab = 'time under observation', 
                          y.lab = 'estimated survival probability', 
                          title = 'event history graph', 
                          ...)
{
  ## if covtime.cols was assigned a single zero, then
  ##  make it a one-column matrix of zeroes:
  if(is.null(covtime.cols))
    covtime.cols <- as.matrix(rep(0, dim(data)[1]))

  ## do necessary subsetting
  if(!is.null(subset.rows)) {
    data <- data[subset.rows,]
    surv.col  <- surv.col[subset.rows]
    survtime.col  <- survtime.col[subset.rows]
    covtime.cols <- covtime.cols[subset.rows,]
    if(!is.null(cov.cols))
      cov.cols  <- cov.cols[subset.rows,]
  }

  ## put in stops signifying 'illegal' data
  if(any(is.na(surv.col)))
    stop('cannot have NA entries in surv.col column \n')

  if(any(is.na(survtime.col)))
    stop('cannot have NA entries in survtime.col column \n')

  if(min(survtime.col) < 0)
    stop('survtime.col observations cannot be < 0 \n')

  if(min(covtime.cols, na.rm = TRUE) < 0)
    stop('covtime.cols observations cannot be < 0 \n')

  ## create color-covariate cutting based on subset data, as desired
  if(is.null(cov.cols))
    colors.cat <- matrix(1, nrow=dim(data)[1])
  else {
    if(is.null(cut.cov))
      colors.cat <- matrix(as.numeric(cut(cov.cols, breaks = num.colors)), 
                           ncol=dim(cov.cols)[2])
    else colors.cat <- matrix(as.numeric(cut(cov.cols, breaks = cut.cov)), 
                              ncol=dim(cov.cols)[2])
  }

  ## order the entire dataframe such that
  ##  time is in descending order and, when tied, then, 
  ##  survival comes before censoring 

  if(surv.ind[1] > surv.ind[2])
    data <- data[order(unlist(survtime.col), unlist(-surv.col)),]
  else if(surv.ind[1] < surv.ind[2])
    data <- data[order(unlist(survtime.col), unlist(surv.col)),]

  ## determine vector of upcoming consecutive censored objects if current is censored
  cens.consec.vec <- rep(NA, dim(data)[1])
  cnt <- 0
  for(i in dim(data)[1]:1) {
    if(surv.col[i] == surv.ind[1]) {
      cnt <- 0
      cens.consec.vec[i] <- 0
      next
    } else if(surv.col[i] == surv.ind[2]) {
      cnt <- cnt + 1
      cens.consec.vec[i] <- cnt - 1
    }
  }

  ## some pre-processing here before plotting:
  ## determine vector of upcoming events (possibly tied events) following
  ##  any censored time or string of consecutive censored times;
  ##  also, determine upcoming event times (or, by default,
  ##  5% beyond final censored time if no event times
  ##  eventually follow a censored time)
  ##  --> also, determine string size of censored obs followed by event(s)

  n <- dim(data)[1]
  cnt <- 0
  seq.events <- (1:n)[surv.col == surv.ind[1]]
  upcoming.events <- time.ahead <- string <- split <- rep(NA, dim(data)[1])
  table.temp <- table(survtime.col[surv.col == surv.ind[1]]) 

  for(i in 1:n) {
    if(surv.col[i] == surv.ind[2]) {
      if((n - cens.consec.vec[i]) > i) {
        cnt <- cnt + 1
        upcoming.events[i] <-
          table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1]
        time.ahead[i] <-
          as.numeric(names(table.temp[as.numeric(names(table.temp)) > survtime.col[i]])[1])
    
        seq.event.after <- seq.events[seq.events > i][1]
        if(i == 1  | (cnt == i)) {
          string[i] <-
            table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1] + 
              (seq.event.after - 1)
	} else {
          seq.event.before <- rev(seq.events[seq.events < i])[1]
          string[i] <- table.temp[as.numeric(names(table.temp)) > survtime.col[i]][1] +
                       (seq.event.after  - seq.event.before - 1)
        }
	
        split[i] <- cnt
        if(surv.col[i+1] == surv.ind[1])
          cnt <- 0
      } else if((n - cens.consec.vec[i]) <= i) {
	cnt <- cnt + 1
	time.ahead[i] <- survtime.col[n] * mult.end.cens
	split[i] <- cnt
	seq.event.before <- rev(seq.events[seq.events < i])[1]
	string[i] <- n - seq.event.before
      }
    ## end censored if statement
    } else if(surv.col[i] == surv.ind[1]) {
      if(i > 1) {
        if(surv.col[i-1] == surv.ind[2]) {
          split[i] <- split[i-1] + 1
          string[i] <- string[i-1]
        } else if((surv.col[i-1] == surv.ind[1]) &
                  (survtime.col[i-1] == survtime.col[i]) & 
                  !is.na(split[i-1])) {
          split[i] <- split[i-1] + 1
          string[i] <- string[i-1]
        }
      }
    }
    ## end event if statement
  }
  ## end pre-processing for loop
  

  ## set up plotting region, axis labels, title, etc.
  plot(x=c(0, max(survtime.col, na.rm=TRUE) * mult.end.cens), y=c(0,1), type='n', 
       xlab=x.lab, ylab=y.lab, main=title, ...)


  ## definitions needed in below for loop
  temp.prob.c <- temp.prob.e <- NA
  temp.prob.old <- 1
  temp.prob.e.old <- 1
  cens.cnt <- 0
  cumsum.e <- cumsum(surv.col)


  ## main function for loop to create plotting lines for each patient

  for(i in 1:n) {
    len.cov <- sum(!is.na(covtime.cols[i,])) 	## number of intervals to draw for patient i

    if(len.cov < 1)
      stop('can have only non-NA covariate observations in iteration', i, '\n')

    if(surv.col[i] == surv.ind[1]) { ## event
      temp.prob.e <- temp.prob.e.old * (n - i) / (n - i + 1)
      if(!is.na(split[i])) {
        upcoming.prob.e <- (n - (i + (string[i] - split[i]))) / 
                           (n + upcoming.event.old - (i + (string[i] - split[i]))) *
                           temp.prob.e.old
        temp.prob.plot <- temp.prob.e.old - 
                          ((temp.prob.e.old - upcoming.prob.e) *
                          split[i]/string[i])
      } else temp.prob.plot <- temp.prob.e

      ## perform plotting for uncensored obs i 	
      if(len.cov > 1) {
        for(j in (1:(len.cov - 1))) {
          color <- switch(colors.cat[i, j], colors[1], colors[2], colors[3], colors[4], colors[5],
                          colors[6], colors[7], colors[8], colors[9], colors[10],
                          colors[11], colors[12], colors[13], colors[14], colors[15],
                          colors[16], colors[17], colors[18], colors[19], colors[20])

          polygon(x=c(covtime.cols[i,j], covtime.cols[i,j+1], covtime.cols[i,j+1], covtime.cols[i,j]), 
		  y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
        }
      }
		
      color <- switch(colors.cat[i, len.cov], colors[1], colors[2], colors[3], colors[4], colors[5],
                      colors[6], colors[7], colors[8], colors[9], colors[10],
                      colors[11], colors[12], colors[13], colors[14], colors[15],
                      colors[16], colors[17], colors[18], colors[19], colors[20])
										
      polygon(x=c(covtime.cols[i,len.cov], survtime.col[i], survtime.col[i], covtime.cols[i,len.cov]), 
              y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)

      if(!is.na(string[i]) & (split[i] < string[i])) 
        temp.prob.old <- temp.prob.plot
      else 
        temp.prob.e.old <- temp.prob.old <- temp.prob.plot	   	
    ## end event if statement for plotting
    } else if(surv.col[i] == surv.ind[2]) { ## censored
      if((n - cens.consec.vec[i]) > i) {
        upcoming.prob.c <- (n - (i + (string[i] - split[i]))) / 
                           (n + upcoming.events[i] - (i + (string[i] - split[i]))) *
                           temp.prob.e.old
        temp.prob.plot <- temp.prob.e.old - 
                          ((temp.prob.e.old - upcoming.prob.c) * split[i]/string[i]) 
        upcoming.event.old <- upcoming.events[i]
      } else if((n - cens.consec.vec[i]) <= i) {
        temp.prob.plot <- temp.prob.e.old - (temp.prob.e.old * split[i]/string[i])	
      }
	
      ## perform plotting for censored obs i 	
      if(len.cov > 1) {
        for(j in (1:(len.cov - 1))) {
          color <- switch(colors.cat[i, j], colors[1], colors[2], colors[3], colors[4], colors[5],
                          colors[6], colors[7], colors[8], colors[9], colors[10],
                          colors[11], colors[12], colors[13], colors[14], colors[15],
                          colors[16], colors[17], colors[18], colors[19], colors[20])
          polygon(x=c(covtime.cols[i,j], covtime.cols[i,j+1], covtime.cols[i,j+1], covtime.cols[i,j]), 
                  y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
        }
      }
	
      color <- switch(colors.cat[i, len.cov], colors[1], colors[2], colors[3], colors[4], colors[5],
                      colors[6], colors[7], colors[8], colors[9], colors[10],
                      colors[11], colors[12], colors[13], colors[14], colors[15],
                      colors[16], colors[17], colors[18], colors[19], colors[20])
      polygon(x=c(covtime.cols[i,len.cov], survtime.col[i], survtime.col[i], covtime.cols[i,len.cov]), 
              y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), col=color)
      polygon(x=c(survtime.col[i], time.ahead[i], time.ahead[i], survtime.col[i]), 
              y=c(temp.prob.plot, temp.prob.plot, temp.prob.old, temp.prob.old), 
              density=cens.density, border=TRUE)	 

      ## Following was if(cens.mark.right == TRUE)  FEH 31jan03
      if(cens.mark.right & temp.prob.plot >= cens.mark.cutoff)
        text(x = time.ahead[i] + cens.mark.ahead, 
             y = temp.prob.old,  	
             labels = cens.mark, cex = cens.mark.cex) 
	
      temp.prob.c <- temp.prob.old <- temp.prob.plot
	    
      ## end censored if statement for plotting
    }
    ## end of function's major for loop
  }
  ## end of function itself
}
