C Output from Public domain Ratfor, version 1.01
      subroutine hoeffd(xx, n, p, dmat, aadmat, madmat, npair, x, y, rx,
     * ry, rj)
      implicit double precision (a-h,o-z)
      integer n, p, npair(p,p)
      double precision xx(n,p), dmat(p,p), aadmat(p,p), madmat(p,p), 
     *x(n), y(n), rx(n), ry(n), rj(n), maxad
      do23000 i=1, p 
      np=0
      do23002 k=1, n 
      if(xx(k,i) .lt. 1e30)then
      np = np + 1
      endif
23002 continue
23003 continue
      npair(i,i) = np
      do23006 j=(i+1),p 
      m = 0
      do23008 k=1,n 
      xk = xx(k,i)
      yk = xx(k,j)
      if(xk .lt. 1e30 .and. yk .lt. 1e30)then
      m = m + 1
      x(m) = xk
      y(m) = yk
      endif
23008 continue
23009 continue
      npair(i,j) = m
      if(m .gt. 4)then
      call hoeff(x, y, m, d, aad, maxad, rx, ry, rj)
      dmat(i,j) = d
      aadmat(i,j) = aad
      madmat(i,j) = maxad
      else
      dmat(i,j) = 1e30
      endif
23006 continue
23007 continue
23000 continue
23001 continue
      do23014 i=1,p 
      dmat(i,i) = 1d0/30d0
      do23016 j=(i+1),p 
      dmat(j,i) = dmat(i,j)
      npair(j,i) = npair(i,j)
      aadmat(j,i) = aadmat(i,j)
      madmat(j,i) = madmat(i,j)
23016 continue
23017 continue
23014 continue
23015 continue
      return
      end
      subroutine hoeff(x, y, n, d, aad, maxad, rx, ry, rj)
      implicit double precision (a-h,o-z)
      double precision x(n), y(n), rx(n), ry(n), rj(n), maxad
      call jrank(x, y, n, rx, ry, rj)
      q = 0d0
      r = 0d0
      s = 0d0
      aad = 0d0
      maxad = 0d0
      z = dfloat(n)
      do23018 i=1,n 
      rxi = rx(i)
      ryi = ry(i)
      rji = rj(i)
      ad = dabs((rji/z) - (rxi/z)*(ryi/z))
      aad = aad + ad
      maxad = dmax1(maxad, ad)
      q = q + (rxi-1d0)*(rxi-2d0)*(ryi-1d0)*(ryi-2d0)
      r = r + (rxi-2d0)*(ryi-2d0)*(rji-1d0)
      s = s + (rji-1d0)*(rji-2d0)
23018 continue
23019 continue
      aad = aad / z
      d = (q-2d0*(z-2d0)*r+(z-2d0)*(z-3d0)*s)/z/(z-1d0)/(z-2d0)/(z-3d0)/
     *(z-4d0)
      return
      end
      subroutine jrank(x, y, n, rx, ry, r)
      integer n
      double precision x(n), y(n), rx(n), ry(n), r(n), cx, cy, ri, rix, 
     *riy, xi, yi
      do23020 i=1,n 
      xi = x(i)
      yi = y(i)
      ri = 1d0
      rix = 1d0
      riy = 1d0
      do23022 j=1,n 
      if(i .ne. j)then
      cx = 0d0
      if(x(j) .lt. xi)then
      cx = 1d0
      endif
      if(x(j) .eq. xi)then
      cx = .5d0
      endif
      cy = 0d0
      if(y(j) .lt. yi)then
      cy = 1d0
      endif
      if(y(j) .eq. yi)then
      cy = .5d0
      endif
      rix = rix + cx
      riy = riy + cy
      ri = ri + cx*cy
      endif
23022 continue
23023 continue
      rx(i) = rix
      ry(i) = riy
      r(i) = ri
23020 continue
23021 continue
      return
      end
