C Output from Public domain Ratfor, version 1.01
      subroutine wclosest(w, x, lw, lx, j)
      implicit double precision (a-h,o-z)
      integer lw, lx, j(lw)
      double precision w(lw), x(lx)
      do23000 i=1,lw 
      wi=w(i)
      dmin=1d40
      m=0
      do23002 k=1,lx 
      d = dabs(x(k) - wi)
      if(d .lt. dmin)then
      dmin = d
      m = k
      endif
23002 continue
23003 continue
      j(i) = m
23000 continue
23001 continue
      return
      end
      subroutine wclosepw(w, x, r, f, lw, lx, xd, j)
      implicit double precision (a-h,o-z)
      double precision w(lw),x(lx),r(lw),xd(lx)
      integer lw, lx, j(lw)
      do23006 i=1, lw 
      wi = w(i)
      dmean = 0d0
      do23008 k=1, lx 
      xd(k) = dabs(x(k) - wi)
      dmean = dmean + xd(k)
23008 continue
23009 continue
      dmean = f * dmean / lx
      sump = 0d0
      do23010 k=1, lx 
      z = min(xd(k)/dmean, 1d0)
      xd(k) = (1d0 - z**3)**3
      sump = sump + xd(k)
23010 continue
23011 continue
      prob = 0d0
      ri = r(i)
      m = 1
      do23012 k=1, lx 
      prob = prob + xd(k) / sump
      if(ri .gt. prob)then
      m = m + 1
      endif
23012 continue
23013 continue
      j(i) = m
23006 continue
23007 continue
      return
      end
