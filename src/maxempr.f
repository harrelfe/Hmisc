C Output from Public domain Ratfor, version 1.01
      subroutine maxempr(ax, ay, x, y, n, w, h, z, area, rect)
      implicit double precision (a-h,o-z)
      integer n
      double precision ax(2), ay(2), x(n), y(n), z(3), rect(4), maxr, li
      maxr = z(1) * dabs(ay(2) - ay(1))
      rect(1) = z(2)
      rect(2) = ay(1)
      rect(3) = z(3)
      rect(4) = ay(2)
      do23000 i=1,n 
      tl = ax(1)
      tr = ax(2)
      if(i .lt. n)then
      do23004 j=(i+1),n 
      if(x(j) .gt. tl .and. x(j) .lt. tr)then
      area = (tr - tl) * (y(j) - y(i))
      if(area .gt. maxr .and. ((tr - tl) .gt. w) .and. ((y(j) - y(i)) .g
     *t. h))then
      maxr = area
      rect(1) = tl
      rect(2) = y(i)
      rect(3) = tr
      rect(4) = y(j)
      endif
      if(x(j) .gt. x(i))then
      tr = x(j)
      else
      tl = x(j)
      endif
      endif
23004 continue
23005 continue
      endif
      area = (tr - tl) * (ay(2) - y(i))
      if(area .gt. maxr .and. ((tr - tl) .gt. w) .and. ((ay(2) - y(i)) .
     *gt. h))then
      maxr = area
      rect(1) = tl
      rect(2) = y(i)
      rect(3) = tr
      rect(4) = ay(2)
      endif
      ri = ax(2)
      li = ax(1)
      do23014 k=1,n 
      if(y(k) .lt. y(i) .and. x(k) .gt. x(i))then
      ri = dmin1(ri, x(k))
      endif
      if(y(k) .lt. y(i) .and. x(k) .lt. x(i))then
      li = dmax1(li, x(k))
      endif
23014 continue
23015 continue
      area = (ri - li) * (ay(2) - y(i))
      if(area .gt. maxr .and. ((ri - li) .gt. w) .and. ((y(i) - ay(1)) .
     *gt. h))then
      maxr = area
      rect(1) = li
      rect(2) = ay(1)
      rect(3) = ri
      rect(4) = y(i)
      endif
23000 continue
23001 continue
      area = maxr
      return
      end
