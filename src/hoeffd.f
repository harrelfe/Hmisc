      subroutine hoeffd(xx, n, p, dmat, npair, x, y, rx, ry, rj, work, 
     &iwork)
      implicit real*8 (a-h,o-z)
      integer p, npair(p,p)
      real*8 xx(n,p), dmat(p,p), x(1), y(1), rx(1), ry(1), rj(1), work(
     &1)
      integer*4 iwork(1)
      do 23000 i=1, p 
      np=0
      do 23002 k=1, n 
      if(.not.(xx(k,i).lt.1d30))goto 23004
      np=np+1
23004 continue
23002 continue
      npair(i,i)=np
      do 23006 j=(i+1),p 
      m=0
      do 23008 k=1,n 
      xk=xx(k,i)
      yk=xx(k,j)
      if(.not.(xk.lt.1d30 .and. yk.lt.1d30))goto 23010
      m=m+1
      x(m)=xk
      y(m)=yk
23010 continue
23008 continue
      npair(i,j)=m
      if(.not.(m.gt.4))goto 23012
      call hoeff(x, y, m, d, rx, ry, rj, work, iwork)
      dmat(i,j)=d
      goto 23013
23012 continue
      dmat(i,j)=1d30
23013 continue
23006 continue
23000 continue
      do 23014 i=1,p 
      dmat(i,i)=1./30d0
      do 23016 j=(i+1),p 
      dmat(j,i)=dmat(i,j)
      npair(j,i)=npair(i,j)
23016 continue
23014 continue
      return
      end
      subroutine hoeff(x, y, n, d, rx, ry, rj, work, iwork)
      implicit real*8 (a-h,o-z)
      real*8 x(1), y(1), rx(1), ry(1), rj(1)
      integer*4 iwork(1)
      call rank(n, x, work, iwork, rx)
      call rank(n, y, work, iwork, ry)
      call jrank(x, y, n, rj)
      q=0d0
      r=0d0
      s=0d0
      do 23018 i=1,n 
      rxi=rx(i)
      ryi=ry(i)
      rji=rj(i)
      q=q+(rxi-1d0)*(rxi-2d0)*(ryi-1d0)*(ryi-2d0)
      r=r+(rxi-2d0)*(ryi-2d0)*rji
      s=s+rji*(rji-1d0)
23018 continue
      z=float(n)
      d=(q-2d0*(z-2d0)*r+(z-2d0)*(z-3d0)*s)/z/(z-1d0)/
     &      (z-2d0)/(z-3d0)/(z-4d0)
      return
      end
      subroutine jrank(x, y, n, r)
      implicit real*8 (a-h,o-z)
      real*8 x(1), y(1), r(1)
      do 23020 i=1,n 
      xi=x(i)
      yi=y(i)
      ri=0d0
      do 23022 j=1,n 
      if(.not.(i.ne.j))goto 23024
      cx=0d0
      if(.not.(x(j).lt.xi))goto 23026
      cx=1d0
23026 continue
      if(.not.(x(j).eq.xi))goto 23028
      cx=.5d0
23028 continue
      cy=0d0
      if(.not.(y(j).lt.yi))goto 23030
      cy=1d0
23030 continue
      if(.not.(y(j).eq.yi))goto 23032
      cy=.5d0
23032 continue
      ri=ri+cx*cy
23024 continue
23022 continue
      r(i)=ri
23020 continue
      return
      end
