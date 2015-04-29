C Output from Public domain Ratfor, version 1.01
      subroutine rcorr(xx, n, p, itype, dmat, npair, x, y, rx, ry, work,
     * iwork)
      integer p, npair(p,p)
      double precision xx(n,p), dmat(p,p), x(n), y(n), rx(n), ry(n), wor
     *k(n)
      integer iwork(n)
      double precision sumx,sumx2,sumy,sumy2,sumxy,z,a,b
      do23000 i=1, p 
      np=0
      do23002 k=1, n 
      if(xx(k,i) .lt. 1d49)then
      np = np + 1
      endif
23002 continue
23003 continue
      npair(i,i) = np
      do23006 j=(i+1), p 
      m=0
      if(itype.eq.1)then
      sumx=0d0
      sumy=0d0
      sumx2=0d0
      sumy2=0d0
      sumxy=0d0
      endif
      do23010 k=1, n 
      xk=xx(k,i)
      yk=xx(k,j)
      if(xk .lt. 1d49 .and. yk .lt. 1d49)then
      m=m + 1
      if(itype.eq.1)then
      a=xk
      b=yk
      sumx =sumx+a
      sumx2=sumx2+a*a
      sumy =sumy+b
      sumy2=sumy2+b*b
      sumxy=sumxy+a*b
      else
      x(m)=xk
      y(m)=yk
      endif
      endif
23010 continue
23011 continue
      npair(i,j) = m
      if(m .gt. 1)then
      if(itype.eq.1)then
      z=m
      d=(sumxy-sumx*sumy/z)/dsqrt((sumx2-sumx*sumx/z)*(sumy2-sumy*sumy/z
     *))
      else
      call docorr(x, y, m, d, rx, ry, work, iwork)
      endif
      dmat(i,j)=d
      else
      dmat(i,j)=1d50
      endif
23006 continue
23007 continue
23000 continue
23001 continue
      do23020 i=1,p 
      dmat(i,i)=1.
      do23022 j=(i+1),p 
      dmat(j,i)=dmat(i,j)
      npair(j,i)=npair(i,j)
23022 continue
23023 continue
23020 continue
23021 continue
      return
      end
      subroutine docorr(x, y, n, d, rx, ry, work, iwork)
      double precision x(1), y(1), rx(n), ry(n), work(1)
      integer iwork(1)
      double precision sumx,sumx2,sumy,sumy2,sumxy,a,b,z
      call rank(n, x, work, iwork, rx)
      call rank(n, y, work, iwork, ry)
      sumx=0d0
      sumx2=0d0
      sumy=0d0
      sumy2=0d0
      sumxy=0d0
      do23024 i=1,n 
      a=rx(i)
      b=ry(i)
      sumx =sumx+a
      sumx2=sumx2+a*a
      sumy =sumy+b
      sumy2=sumy2+b*b
      sumxy=sumxy+a*b
23024 continue
23025 continue
      z=n
      d=(sumxy-sumx*sumy/z)/dsqrt((sumx2-sumx*sumx/z)*(sumy2-sumy*sumy/z
     *))
      return
      end
