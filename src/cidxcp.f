C Output from Public domain Ratfor, version 1.01
      subroutine cidxcp(x1,x2,y,e,n,method,outx, nrel,nuncer,c1,c2,gamma
     *1,gamma2,gamma,sd,c12,c21)
      implicit double precision (a-h,o-z)
      double precision x1(n),x2(n),y(n)
      logical e(n),outx
      integer n,method,i,j
      double precision nrel,nuncer,nconc1,nconc2,c1,c2,gamma1,gamma2, su
     *mr,sumr2,sumw,sumw2,sumrw, wi,ri,sumc,c12,c21,gamma,sd
      double precision dx,dx2,dy
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
      do23000 i=1,n 
      wi=0d0
      ri=0d0
      do23002 j=1,n 
      dx=x1(i)-x1(j)
      dx2=x2(i)-x2(j)
      if((i.ne.j) .and. (.not.outx .or. dx.ne.0. .or. dx2.ne.0.))then
      dy=y(i)-y(j)
      if((e(i).and.(dy.lt.0.)).or.(e(i).and..not.e(j).and.(dy.eq.0.)))th
     *en
      nrel=nrel+1d0
      nconc1=nconc1+(z(dx.lt.0.)+.5d0*z(dx.eq.0.))
      nconc2=nconc2+(z(dx2.lt.0.)+.5d0*z(dx2.eq.0.))
      ri=ri+1d0
      if(method.eq.1)then
      wi=wi+(z(dx.lt.dx2)-z(dx.gt.dx2))
      sumc=sumc+z(dx.lt.dx2)
      else
      wi=wi+(z(dx.lt.0..and.dx2.ge.0.)-z(dx.gt.0..and.dx2.le.0.))
      sumc=sumc+z(dx.lt.0..and.dx2.ge.0.)
      endif
      else
      if((e(j).and.(dy.gt.0.)).or.(e(j).and..not.e(i).and.(dy.eq.0.)))th
     *en
      nrel=nrel+1d0
      nconc1=nconc1+(z(dx.gt.0.)+.5d0*z(dx.eq.0.))
      nconc2=nconc2+(z(dx2.gt.0.)+.5d0*z(dx2.eq.0.))
      ri=ri+1d0
      if(method.eq.1)then
      wi=wi+(z(dx.gt.dx2)-z(dx.lt.dx2))
      sumc=sumc+z(dx.gt.dx2)
      else
      wi=wi+(z(dx.gt.0..and.dx2.le.0.)-z(dx.lt.0..and.dx2.ge.0.))
      sumc=sumc+z(dx.gt.0..and.dx2.le.0.)
      endif
      else
      if(.not.(e(i).and.e(j)))then
      nuncer=nuncer+1d0
      endif
      endif
      endif
      endif
23002 continue
23003 continue
      sumr=sumr+ri
      sumr2=sumr2+ri*ri
      sumw=sumw+wi
      sumw2=sumw2+wi*wi
      sumrw=sumrw+ri*wi
23000 continue
23001 continue
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
      double precision z
      logical a
      if(a)then
      z=1d0
      else
      z=0d0
      endif
      return
      end
