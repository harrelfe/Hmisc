# Given w, an n-1 by k matrix of weights, say for linear statistics,
# computes all n leave-out-one linear statistics separately by each
# column of w for the data n-vector x

subroutine jacklins(x, w, n, k, res)
INTEGER n, k, l
DOUBLE PRECISION x(n), w(n-1,k), res(n,k), sj

do l = 1, k {
  do j = 1, n {
    sj = 0d0
    do i = 1, n {
      if(i < j) sj = sj + w(i,l) * x(i)
      if(i > j) sj = sj + w(i-1,l) * x(i)
    }
    res(j,l) = sj
  }
}
return
end

