SUBROUTINE hoeffd(xx, n, p, dmat, npair, x, y, rx, ry, rj, work, iwork)
INTEGER p, npair(p,p)
DOUBLE PRECISION xx(n,p), dmat(p,p), x(1), y(1), rx(1), ry(1), rj(1), work(1)
INTEGER iwork(1)

DO i=1, p {
  np=0
  DO k=1, n {
    if(xx(k,i)<1e30) np=np+1
  }
  npair(i,i)=np

  DO j=(i+1),p {
    m=0
    DO k=1,n {
      xk=xx(k,i)
      yk=xx(k,j)
      if(xk<1e30 & yk<1e30) {
        m=m+1
        x(m)=xk
        y(m)=yk
      }
    }
    npair(i,j)=m
    if(m>4) {
      CALL hoeff(x, y, m, d, rx, ry, rj, work, iwork)
      dmat(i,j)=d
      }
    else dmat(i,j)=1e30
  }
}
DO i=1,p {
  dmat(i,i)=1./30.
  DO j=(i+1),p {
    dmat(j,i)=dmat(i,j)
    npair(j,i)=npair(i,j)
  }
}
RETURN
END  

	SUBROUTINE hoeff(x, y, n, d, rx, ry, rj, work, iwork)
	DOUBLE PRECISION x(1), y(1), rx(1), ry(1), rj(1)
	INTEGER iwork(1)
	CALL rank(n, x, work, iwork, rx)
	CALL rank(n, y, work, iwork, ry)
	CALL jrank(x, y, n, rj)
	q=0.
	r=0.
	s=0.
	DO i=1,n {
	  rxi=rx(i)
	  ryi=ry(i)
	  rji=rj(i)
	  q=q+(rxi-1.)*(rxi-2.)*(ryi-1.)*(ryi-2.)
	  r=r+(rxi-2.)*(ryi-2.)*rji
	  s=s+rji*(rji-1.)
	}
	z=float(n)
	d=(q-2.*(z-2.)*r+(z-2.)*(z-3.)*s)/z/(z-1.)/(z-2.)/(z-3.)/(z-4.)
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

	SUBROUTINE jrank(x, y, n, r) # joint rank with midranks for ties
	DOUBLE PRECISION x(1), y(1), r(1)
	DO i=1,n {
	  xi=x(i)
	  yi=y(i)
	  ri=0.
	  DO j=1,n {
	    if(i.ne.j) {
	      cx=0.
	      if(x(j)<xi) cx=1.
	      if(x(j)==xi) cx=.5
	      cy=0.
	      if(y(j)<yi) cy=1.
	      if(y(j)==yi) cy=.5
	      ri=ri+cx*cy
	    }
	  }
	  r(i)=ri
	}
	RETURN
	END

	      
