SUBROUTINE rcorr(xx, n, p, itype, dmat, npair, x, y, rx, ry, work, iwork)
INTEGER p, npair(p,p)
DOUBLE PRECISION xx(n,p), dmat(p,p), x(n), y(n), rx(n), ry(n), work(n)
INTEGER iwork(n)
DOUBLE PRECISION sumx, sumx2, sumy, sumy2, sumxy, z, a, b, xk, yk, d

# The following is just to prevent a gfortran possible uninitialized var warning 
sumx = 0d0; sumy = 0d0; sumx2 = 0d0; sumy2 = 0d0; sumxy = 0d0

DO i = 1, p {
  np = 0
  DO k = 1, n {
    if(xx(k, i) < 1d49) np  =  np + 1
  }
  npair(i, i) = np

  DO j = (i + 1), p {
    m = 0
    if(itype == 1) {
      sumx = 0d0; sumy = 0d0; sumx2 = 0d0; sumy2 = 0d0; sumxy = 0d0
    }
    DO k = 1, n {
      xk = xx(k, i)
      yk = xx(k, j)
      if(xk < 1d49 & yk < 1d49) {
        m = m + 1
        if(itype == 1) {
          a = xk; b = yk
          sumx  = sumx + a
          sumx2 = sumx2 + a * a
          sumy  = sumy + b
          sumy2 = sumy2 + b * b
          sumxy = sumxy + a * b
        } else {
          x(m) = xk
          y(m) = yk
        }
      }
    }
    npair(i, j) = m
    if(m > 1) {
      if(itype == 1) {
        z = m
        d = (sumxy - sumx * sumy / z) / dsqrt((sumx2 - sumx * sumx / z) * (sumy2 - sumy * sumy / z))
      } else CALL docorr(x, y, m, d, rx, ry, work, iwork)
      dmat(i, j) = d
    } else dmat(i, j) = 1d50
  }
}
DO i = 1, p {
  dmat(i, i) = 1d0
  DO j = (i + 1), p {
    dmat(j, i)  = dmat(i, j)
    npair(j, i) = npair(i, j)
  }
}
RETURN
END  

	SUBROUTINE docorr(x, y, n, d, rx, ry, work, iwork)
	DOUBLE PRECISION x(1), y(1), rx(n), ry(n), work(1)
	INTEGER iwork(1)
	DOUBLE PRECISION sumx, sumx2, sumy, sumy2, sumxy, a, b, z, d
	CALL rank(n, x, work, iwork, rx)
	CALL rank(n, y, work, iwork, ry)
	sumx = 0d0; sumx2 = 0d0; sumy = 0d0; sumy2 = 0d0; sumxy = 0d0
	DO i = 1, n {
	  a = rx(i)
	  b = ry(i)
	  sumx  = sumx + a
	  sumx2 = sumx2 + a * a
	  sumy  = sumy + b
	  sumy2 = sumy2 + b * b
	  sumxy = sumxy + a * b
	}
	z = n
	d = (sumxy - sumx * sumy / z) / dsqrt((sumx2 - sumx * sumx / z) * (sumy2 - sumy * sumy / z))	
	RETURN
	END	
