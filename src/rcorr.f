      subroutine rcorr(xx, n, p, itype, dmat, npair, x, y, rx, ry, work,
     & iwork)
      implicit real*8 (a-h,o-z)
      integer p, npair(p,p)
      real*8 xx(n,p), dmat(p,p), x(1), y(1), rx(1), ry(1), work(1)
      integer*4 iwork(1)
      real*8 sumx,sumx2,sumy,sumy2,sumxy,z,a,b
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
      if(.not.(itype.eq.1))goto 23008
      sumx=0d0
      sumy=0d0
      sumx2=0d0
      sumy2=0d0
      sumxy=0d0
23008 continue
      do 23010 k=1,n 
      xk=xx(k,i)
      yk=xx(k,j)
      if(.not.(xk.lt.1d30 .and. yk.lt.1d30))goto 23012
      m=m+1
      if(.not.(itype.eq.1))goto 23014
      a=xk
      b=yk
      sumx=sumx+a
      sumx2=sumx2+a*a
      sumy=sumy+b
      sumy2=sumy2+b*b
      sumxy=sumxy+a*b
      goto 23015
23014 continue
      x(m)=xk
      y(m)=yk
23015 continue
23012 continue
23010 continue
      npair(i,j)=m
      if(.not.(m.gt.1))goto 23016
      if(.not.(itype.eq.1))goto 23018
      z=m
      d=(sumxy-sumx*sumy/z)/dsqrt((sumx2-sumx*sumx/z)*(sumy2-sumy*sumy/
     &z))
      goto 23019
23018 continue
      call docorr(x, y, m, d, rx, ry, work, iwork)
23019 continue
      dmat(i,j)=d
      goto 23017
23016 continue
      dmat(i,j)=1d30
23017 continue
23006 continue
23000 continue
      do 23020 i=1,p 
      dmat(i,i)=1d0
      do 23022 j=(i+1),p 
      dmat(j,i)=dmat(i,j)
      npair(j,i)=npair(i,j)
23022 continue
23020 continue
      return
      end
      subroutine docorr(x, y, n, d, rx, ry, work, iwork)
      implicit real*8 (a-h,o-z)
      real*8 x(1), y(1), rx(1), ry(1)
      integer*4 iwork(1)
      real*8 sumx,sumx2,sumy,sumy2,sumxy,a,b,z
      call rank(n, x, work, iwork, rx)
      call rank(n, y, work, iwork, ry)
      sumx=0d0
      sumx2=0d0
      sumy=0d0
      sumy2=0d0
      sumxy=0d0
      do 23024 i=1,n 
      a=rx(i)
      b=ry(i)
      sumx=sumx+a
      sumx2=sumx2+a*a
      sumy=sumy+b
      sumy2=sumy2+b*b
      sumxy=sumxy+a*b
23024 continue
      z=n
      d=(sumxy-sumx*sumy/z)/dsqrt((sumx2-sumx*sumx/z)*(sumy2-sumy*sumy/
     &z))
      return
      end
