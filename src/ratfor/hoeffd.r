#  ratfor -o ../hoeffd.f hoeffd.r
#
SUBROUTINE hoeffd(xx, n, p, dmat, aadmat, madmat, npair, x, y, rx, ry, rj)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
INTEGER p, npair(p,p)
DOUBLE PRECISION xx(n,p), dmat(p,p), aadmat(p,p), madmat(p,p), x(n), y(n), rx(n), ry(n), rj(n), maxad

    DO i=1, p {
        np=0
        DO k=1, n {
            if(xx(k,i) < 1d49) np = np + 1
        }
        npair(i,i) = np

        DO j=(i+1),p {
            m = 0
            DO k=1,n {
                xk = xx(k,i)
                yk = xx(k,j)
                if(xk < 1d49 & yk < 1d49) {
                    m = m + 1
                    x(m) = xk
                    y(m) = yk
                }
            }
            npair(i,j) = m
            if(m > 4) {
                CALL hoeff(x, y, m, d, aad, maxad, rx, ry, rj)
                dmat(i,j) = d
                aadmat(i,j) = aad
                madmat(i,j) = maxad
            }
            else dmat(i,j) = 1d50
        }
    }
    DO i=1,p {
        dmat(i,i) = 1d0/30d0
        DO j=(i+1),p {
            dmat(j,i) = dmat(i,j)
            npair(j,i) = npair(i,j)
            aadmat(j,i) = aadmat(i,j)
            madmat(j,i) = madmat(i,j)
        }
    }
    RETURN
END  

SUBROUTINE hoeff(x, y, n, d, aad, maxad, rx, ry, rj)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION x(n), y(n), rx(n), ry(n), rj(n), maxad
#	INTEGER iwork(1)
#	CALL rank(n, x, work, iwork, rx)
#	CALL rank(n, y, work, iwork, ry)
    CALL jrank(x, y, n, rx, ry, rj)
    q = 0d0
    r = 0d0
    s = 0d0
    aad = 0d0
    maxad = 0d0
    z = n
    DO i=1,n {
        rxi = rx(i)
        ryi = ry(i)
        rji = rj(i)
        ad  = dabs((rji/z) - (rxi/z)*(ryi/z))
        aad = aad + ad
        maxad = dmax1(maxad, ad)
	q = q + (rxi-1d0)*(rxi-2d0)*(ryi-1d0)*(ryi-2d0)
	r = r + (rxi-2d0)*(ryi-2d0)*(rji-1d0)
	s = s + (rji-1d0)*(rji-2d0)
    }
    aad = aad / z
    d = (q-2d0*(z-2d0)*r+(z-2d0)*(z-3d0)*s)/z/(z-1d0)/(z-2d0)/(z-3d0)/(z-4d0)
    RETURN
END

#	Use C version of this which is much faster (since uses a sort)
#	SUBROUTINE rank(x, n, r)   # simple rank with midranks for ties
#	REAL*4 x(1), r(1)
#	DO i=1,n {
#	  xi=x(i)
#	  ir=2   # will be 2*rank(x(i))
#	  DO j=1,n {
#	    if(i.ne.j) {
#	      if(x(j)<xi) ir=ir+2
#	      if(x(j)==xi) ir=ir+1
#	    }
#	  }
#	  r(i)=float(ir)/2.0
#	}
#	RETURN
#	END

SUBROUTINE jrank(x, y, n, rx, ry, r) # joint rank with midranks for ties
INTEGER n
DOUBLE PRECISION x(n), y(n), rx(n), ry(n), r(n), cx, cy, ri, rix, riy, xi, yi
    DO i=1,n {
        xi  = x(i)
	yi  = y(i)
        ri  = 1d0
        rix = 1d0
        riy = 1d0
        DO j=1,n {
            if(i .ne. j) {
	        cx = 0d0
                if(x(j) < xi) cx = 1d0
                if(x(j) == xi) cx = .5d0
                cy = 0d0
                if(y(j) < yi) cy = 1d0
                if(y(j) == yi) cy = .5d0
                rix = rix + cx
                riy = riy + cy
	        ri  = ri + cx*cy
            }
        }
        rx(i) = rix
        ry(i) = riy
        r(i) = ri
    }
    RETURN
END
