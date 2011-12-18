# Converted from R code provided by Hans Werner Borchers <hwborchers@googlemail.com>
# ax = x-limits for region of interest
# ay = y-limits " "
# x, y = coordinates of points to avoid
# Assume x, y are sorted in y-order, e.g
# o = order(y); x <- x[o]; y <- y[o]
# n = length(x) = length(y)
# z = c(D[m], d[m], d[m+1]), d=sort(c(ax,x)), D=diff(d), m=which.max(D)
# Output: area, rect[4]
# To convert to Fortran:
#  sudo apt-get install ratfor
#  ratfor -o ../maxempr.f maxempr.r
SUBROUTINE maxempr(ax, ay, x, y, n, w, h, z, area, rect)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER n
DOUBLE PRECISION ax(2), ay(2), x(n), y(n), z(3), rect(4), maxr, li
# check vertical slices
maxr = z(1) * dabs(ay(2) - ay(1))
rect(1) = z(2)
rect(2) = ay(1)
rect(3) = z(3)
rect(4) = ay(2)

do i=1,n {
  tl = ax(1); tr = ax(2)
  if (i < n) {
    do j=(i+1),n {
      if (x(j) > tl & x(j) < tr) {
        ## check horizontal slices (j == i+1)
        ## and (all) rectangles above (x(i), y(i))
        area = (tr - tl) * (y(j) - y(i))
        if (area > maxr & ((tr - tl) > w) & ((y(j) - y(i)) > h)) {
          maxr = area
          rect(1) = tl
          rect(2) = y(i)
          rect(3) = tr
          rect(4) = y(j)
        }
        if (x(j) > x(i)) tr = x(j)
        else             tl = x(j)
      }
    }
  }
  ## check open rectangles above (x(i), y(i))
  area = (tr - tl) * (ay(2) - y(i))
  if (area > maxr & ((tr - tl) > w) &
            ((ay(2) - y(i)) > h)) {
    maxr = area
    rect(1) = tl
    rect(2) = y(i)
    rect(3) = tr
    rect(4) = ay(2)
  }
  ## check open rectangles below (x(i), y(i))
  ri = ax(2); li = ax(1)
  do k=1,n {
    if(y(k) < y(i) & x(k) > x(i)) ri = dmin1(ri, x(k))
    if(y(k) < y(i) & x(k) < x(i)) li = dmax1(li, x(k))
  }
  area = (ri - li) * (ay(2) - y(i))
  if (area > maxr & ((ri - li) > w) &
            ((y(i) - ay(1)) > h)) {
    maxr = area
    rect(1) = li
    rect(2) = ay(1)
    rect(3) = ri
    rect(4) = y(i)
  }
}
area = maxr
return
end
