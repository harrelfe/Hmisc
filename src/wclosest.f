      subroutine wclosest(w, x, lw, lx, j)
      implicit real*8 (a-h,o-z)
      integer lw, lx, j(lw)
      real*8 w(lw), x(lx)
      do 23000 i=1,lw 
      wi=w(i)
      dmin=1d40
      m=0
      do 23002 k=1,lx 
      d = dabs(x(k) - wi)
      if(.not.(d .lt. dmin))goto 23004
      dmin = d
      m = k
23004 continue
23002 continue
      j(i) = m
23000 continue
      return
      end
      subroutine wclosepw(w, x, r, f, lw, lx, xd, j)
      implicit real*8 (a-h,o-z)
      real*8 w(lw),x(lx),r(lw),xd(lx)
      integer lw, lx, j(lw)
      do 23006 i=1, lw 
      wi = w(i)
      dmean = 0d0
      do 23008 k=1, lx 
      xd(k) = dabs(x(k) - wi)
      dmean = dmean + xd(k)
23008 continue
      dmean = f*dmean/dfloat(lx)
      sump = 0d0
      do 23010 k=1, lx 
      z = min(xd(k)/dmean, 1d0)
      xd(k) = (1d0 - z**3)**3
      sump = sump + xd(k)
23010 continue
      prob = 0d0
      ri = r(i)
      m = 1
      do 23012 k=1, lx 
      prob = prob + xd(k) / sump
      if(.not.(ri .gt. prob))goto 23014
      m = m + 1
23014 continue
23012 continue
      j(i) = m
23006 continue
      return
      end
