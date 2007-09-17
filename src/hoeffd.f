C Output from Public domain Ratfor, version 1.01
      subroutine hoeffd(xx, n, p, dmat, npair, x, y, rx, ry, rj, work, i
     *work)
      integer p, npair(p,p)
      double precision xx(n,p), dmat(p,p), x(1), y(1), rx(1), ry(1), rj(
     *1), work(1)
      integer iwork(1)
      do23000 i=1, p 
      np=0
      do23002 k=1, n 
      if(xx(k,i).lt.1e30)then
      np=np+1
      endif
23002 continue
23003 continue
      npair(i,i)=np
      do23006 j=(i+1),p 
      m=0
      do23008 k=1,n 
      xk=xx(k,i)
      yk=xx(k,j)
      if(xk.lt.1e30 .and. yk.lt.1e30)then
      m=m+1
      x(m)=xk
      y(m)=yk
      endif
23008 continue
23009 continue
      npair(i,j)=m
      if(m.gt.4)then
      call hoeff(x, y, m, d, rx, ry, rj, work, iwork)
      dmat(i,j)=d
      else
      dmat(i,j)=1e30
      endif
23006 continue
23007 continue
23000 continue
23001 continue
      do23014 i=1,p 
      dmat(i,i)=1./30.
      do23016 j=(i+1),p 
      dmat(j,i)=dmat(i,j)
      npair(j,i)=npair(i,j)
23016 continue
23017 continue
23014 continue
23015 continue
      return
      end
      subroutine hoeff(x, y, n, d, rx, ry, rj, work, iwork)
      double precision x(1), y(1), rx(1), ry(1), rj(1)
      integer iwork(1)
      call rank(n, x, work, iwork, rx)
      call rank(n, y, work, iwork, ry)
      call jrank(x, y, n, rj)
      q=0.
      r=0.
      s=0.
      do23018 i=1,n 
      rxi=rx(i)
      ryi=ry(i)
      rji=rj(i)
      q=q+(rxi-1.)*(rxi-2.)*(ryi-1.)*(ryi-2.)
      r=r+(rxi-2.)*(ryi-2.)*rji
      s=s+rji*(rji-1.)
23018 continue
23019 continue
      z=float(n)
      d=(q-2.*(z-2.)*r+(z-2.)*(z-3.)*s)/z/(z-1.)/(z-2.)/(z-3.)/(z-4.)
      return
      end
C	Use C version of this which is much faster (since uses a sort)
C	SUBROUTINE rank(x, n, r)   # simple rank with midranks for ties
C	REAL*4 x(1), r(1)
C	DO i=1,n {
C	  xi=x(i)
C	  ir=2   # will be 2*rank(x(i))
C	  DO j=1,n {
C	    if(i.ne.j) {
C	      if(x(j)<xi) ir=ir+2
C	      if(x(j)==xi) ir=ir+1
C	    }
C	  }
C	  r(i)=float(ir)/2.0
C	}
C	RETURN
C	END
C joint rank with midranks for ties
      subroutine jrank(x, y, n, r)
      double precision x(1), y(1), r(1)
      do23020 i=1,n 
      xi=x(i)
      yi=y(i)
      ri=0.
      do23022 j=1,n 
      if(i.ne.j)then
      cx=0.
      if(x(j).lt.xi)then
      cx=1.
      endif
      if(x(j).eq.xi)then
      cx=.5
      endif
      cy=0.
      if(y(j).lt.yi)then
      cy=1.
      endif
      if(y(j).eq.yi)then
      cy=.5
      endif
      ri=ri+cx*cy
      endif
23022 continue
23023 continue
      r(i)=ri
23020 continue
23021 continue
      return
      end
