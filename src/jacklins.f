      subroutine jacklins(x, w, n, k, res)
      integer n, k, l
      real*8 x(n), w(n-1,k), res(n,k)
      do 23000 l=1,k 
      do 23002 j=1,n 
      sj=0d0
      do 23004 i=1,n 
      if(.not.(i.lt.j))goto 23006
      sj=sj+w(i,l)*x(i)
23006 continue
      if(.not.(i.gt.j))goto 23008
      sj=sj+w(i-1,l)*x(i)
23008 continue
23004 continue
      res(j,l)=sj
23002 continue
23000 continue
      return
      end
