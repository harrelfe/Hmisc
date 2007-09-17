C Output from Public domain Ratfor, version 1.01
C Given w, an n-1 by k matrix of weights, say for linear statistics,
C computes all n leave-out-one linear statistics separately by each
C column of w for the data n-vector x
      subroutine jacklins(x, w, n, k, res)
      integer n, k, l
      double precision x(n), w(n-1,k), res(n,k)
      do23000 l=1,k 
      do23002 j=1,n 
      sj=0d0
      do23004 i=1,n 
      if(i.lt.j)then
      sj=sj+w(i,l)*x(i)
      endif
      if(i.gt.j)then
      sj=sj+w(i-1,l)*x(i)
      endif
23004 continue
23005 continue
      res(j,l)=sj
23002 continue
23003 continue
23000 continue
23001 continue
      return
      end
