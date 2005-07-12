minor.tick <- function(nx=2, ny=2, tick.ratio=.5)
{
  ax <- function(w, n, tick.ratio)
  {
    range <- par("usr")[if(w=="x") 1:2
                        else 3:4]
    
    tick.pos <-
      if(w=="x")
        par("xaxp")
      else par("yaxp")

    ## Solve for first and last minor tick mark positions that are on the graph

    distance.between.minor <- (tick.pos[2]-tick.pos[1])/tick.pos[3]/n
    possible.minors <- tick.pos[1]-(0:100)*distance.between.minor  #1:100 13may02
    low.minor <- min(possible.minors[possible.minors>=range[1]])
    if(is.na(low.minor)) low.minor <- tick.pos[1]
    possible.minors <- tick.pos[2]+(0:100)*distance.between.minor  #1:100 13may02
    hi.minor <- max(possible.minors[possible.minors<=range[2]])
    if(is.na(hi.minor))
      hi.minor <- tick.pos[2]

    if(.R.)
      axis(if(w=="x") 1
           else 2,
           seq(low.minor,hi.minor,by=distance.between.minor),
           labels=FALSE, tcl=par('tcl')*tick.ratio)
    else
      axis(if(w=="x") 1
           else 2,
           seq(low.minor,hi.minor,by=distance.between.minor),
           labels=FALSE, tck=par('tck')*tick.ratio)
  }

  if(nx>1)
    ax("x", nx, tick.ratio=tick.ratio)
  
  if(ny>1)
    ax("y", ny, tick.ratio=tick.ratio)

  invisible()
}
