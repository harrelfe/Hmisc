      subroutine cidxcp(x1,x2,y,e,n,method,outx,nrel,nuncer,c1,c2,
     &     gamma1,gamma2,gamma,sd,c12,c21)
      implicit real*8 (a-h,o-z)
      real*8 x1(n),x2(n),y(n)
      logical*4 e(n),outx
      integer n,method,i,j
      real*8 nrel,nuncer,nconc1,nconc2,c1,c2,gamma1,gamma2,sumr,sumr2,
     &     sumw,sumw2,sumrw,wi,ri,sumc,c12,c21,gamma,sd
      real*8 dx,dx2,dy
      nconc1=0d0
      nconc2=0d0
      nrel=0d0
      nuncer=0d0
      sumr=0d0
      sumr2=0d0
      sumw=0d0
      sumw2=0d0
      sumrw=0d0
      sumc=0d0
      do 23000 i=1,n
      wi=0d0
      ri=0d0
      do 23002 j=1,n
      dx=x1(i)-x1(j)
      dx2=x2(i)-x2(j)
      if(.not.((i.ne.j) .and. 
     &  (.not.outx .or. dx.ne.0d0 .or. dx2.ne.0d0)))
     &     goto 23004
      dy=y(i)-y(j)
      if(.not.((e(i).and.(dy.lt.0d0)).or.(e(i).and.
     &   .not.e(j).and.(dy.eq. 0d0))))goto 23006
      nrel=nrel+1d0
      nconc1=nconc1+(z(dx.lt.0d0)+.5d0*z(dx.eq.0d0))
      nconc2=nconc2+(z(dx2.lt.0d0)+.5d0*z(dx2.eq.0d0))
      ri=ri+1d0
      if(.not.(method.eq.1))goto 23008
      wi=wi+(z(dx.lt.dx2)-z(dx.gt.dx2))
      sumc=sumc+z(dx.lt.dx2)
      goto 23009
23008 continue
      wi=wi+(z(dx.lt.0d0.and.dx2.ge.0d0)-z(dx.gt.0d0.and.dx2.le.0d0))
      sumc=sumc+z(dx.lt.0d0.and.dx2.ge.0d0)
23009 continue
      goto 23007
23006 continue
      if(.not.((e(j).and.(dy.gt.0d0)).or.(e(j).and..not.e(i).and.(dy.eq.
     &     0d0))))goto 23010
      nrel=nrel+1d0
      nconc1=nconc1+(z(dx.gt.0d0)+.5d0*z(dx.eq.0d0))
      nconc2=nconc2+(z(dx2.gt.0d0)+.5d0*z(dx2.eq.0d0))
      ri=ri+1d0
      if(.not.(method.eq.1))goto 23012
      wi=wi+(z(dx.gt.dx2)-z(dx.lt.dx2))
      sumc=sumc+z(dx.gt.dx2)
      goto 23013
23012 continue
      wi=wi+(z(dx.gt.0d0.and.dx2.le.0d0)-z(dx.lt.0d0.and.dx2.ge.0d0))
      sumc=sumc+z(dx.gt.0d0.and.dx2.le.0d0)
23013 continue
      goto 23011
23010 continue
      if(.not.(.not.(e(i).and.e(j))))goto 23014
      nuncer=nuncer+1d0
23014 continue
23011 continue
23007 continue
23004 continue
23002 continue
      sumr=sumr+ri
      sumr2=sumr2+ri*ri
      sumw=sumw+wi
      sumw2=sumw2+wi*wi
      sumrw=sumrw+ri*wi
23000 continue
      c1=nconc1/nrel
      gamma1=2d0*(c1-.5d0)
      c2=nconc2/nrel
      gamma2=2d0*(c2-.5d0)
      gamma=sumw/sumr
      sd=sumr2*sumw**2-2d0*sumr*sumw*sumrw+sumw2*sumr**2
      sd=2d0*dsqrt(sd)/sumr/sumr
      c12=sumc/sumr
      c21=sumc/sumr-gamma
      return
      end
      function z(a)
      real*8 z
      logical*4 a
      if(.not.(a))goto 23016
      z=1d0
      goto 23017
23016 continue
      z=0d0
23017 continue
      return
      end
