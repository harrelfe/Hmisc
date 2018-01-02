SUBROUTINE wclosest(w, x, lw, lx, j)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER lw, lx, j(lw)
DOUBLE PRECISION w(lw), x(lx)

do i=1,lw {
  wi=w(i)
  dmin=1d40
  m=0
  do k=1,lx {
    d = dabs(x(k) - wi)
    if(d < dmin) {
      dmin = d
      m = k
    }
  }
  j(i) = m
}
return
end

SUBROUTINE wclosepw(w, x, r, f, lw, lx, xd, j)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION w(lw),x(lx),r(lw),xd(lx)
INTEGER lw, lx, j(lw)
do i=1, lw {
  wi = w(i)
  dmean = 0d0
  do k=1, lx {
    xd(k) = dabs(x(k) - wi)
    dmean = dmean + xd(k)
  }
  dmean = f * dmean / lx
  sump = 0d0
  do k=1, lx {
    z = min(xd(k)/dmean, 1d0)
    xd(k) = (1d0 - z**3)**3
    sump = sump + xd(k)
  }
  prob = 0d0
  ri = r(i)
  m = 1
  do k=1, lx {
    prob = prob + xd(k) / sump
    if(ri > prob) m = m + 1
  }
  j(i) = m
}
return
end
