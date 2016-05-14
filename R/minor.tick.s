minor.tick <- function (nx = 2, ny = 2, tick.ratio = 0.5, x.args = list(), y.args = list()) {
  ax <- function(w, n, tick.ratio, add.args) {
    range <- par("usr")[if (w == "x") 1 : 2  else 3 : 4]
    tick.pos <- if (w == "x") par("xaxp") else par("yaxp")
    distance.between.minor <- (tick.pos[2] - tick.pos[1])/tick.pos[3]/n
    possible.minors <- tick.pos[1] - (0 : 100) * distance.between.minor
    low.candidates <- possible.minors >= range[1]
    low.minor <- if (any(low.candidates))
                   min(possible.minors[low.candidates])
                 else 
                   tick.pos[1]
    possible.minors <- tick.pos[2] + (0 : 100) * distance.between.minor
    hi.candidates <- possible.minors <= range[2]
    hi.minor <- if (any(hi.candidates)) 
                  max(possible.minors[hi.candidates])
                else
                  tick.pos[2]
    axis.args <- c(list(if (w == "x") 1 else 2,
                        seq(low.minor, hi.minor, by = distance.between.minor), 
                        labels = FALSE, tcl = par("tcl") * tick.ratio),
                        add.args);
	do.call(axis, axis.args);
    }
  if (nx > 1) 
    ax("x", nx, tick.ratio = tick.ratio, x.args)
  if (ny > 1) 
    ax("y", ny, tick.ratio = tick.ratio, y.args)
  invisible()
}
