C Output from Public domain Ratfor, version 1.01
C------------------------------------------------------------------------
C       Compute c-index (c) and Brown-Hollander-Krowar-Goodman-Kruskal-Somer
C       rank correlation (gamma) between X and Y with censoring indicator E
C       Also returns number of relevant, concordant, and uncertain pairs
C       (nrel, nconc, nuncert) and estimated s.d. of gamma (sd) using
C       Quade formula (see SAS PROC MATPAR).  Pairs with tied x are
C       excluded if outx=.TRUE.
C
C       F. Harrell  27Nov90
C                   Modification of SAS Procedure KGKC (1980)
C-------------------------------------------------------------------------
      subroutine cidxcn(x,y,e,n,nrel,nconc,nuncert,c,gamma,sd,outx)
      implicit double precision (a-h,o-z)
      double precision x(n),y(n),dx,dy
      logical e(n),outx
      double precision nrel,nuncert,nconc
      nconc=0d0
      nrel=0d0
      nuncert=0d0
      sumr=0d0
      sumr2=0d0
      sumw=0d0
      sumw2=0d0
      sumrw=0d0
      do23000 i=1,n 
      wi=0d0
      ri=0d0
      do23002 j=1,n
      if(j.ne.i)then
      dx=x(i)-x(j)
      dy=y(i)-y(j)
      if(dx.ne.0. .or. .not.outx)then
      if((e(i).and.dy.lt.0.).or.(e(i).and..not.e(j).and.dy.eq.0.))then
      if(dx.lt.0.)then
      nconc=nconc+1d0
      wi=wi+1d0 
      else
      if(dx.eq.0.)then
      nconc=nconc+.5d0
      else
      wi=wi-1d0
      endif
      endif
      nrel=nrel+1d0
      ri=ri+1d0 
      else
      if((e(j).and.dy.gt.0.).or.(e(j).and..not.e(i).and.dy.eq.0.))then
      if(dx.gt.0.)then
      nconc=nconc+1d0
      wi=wi+1d0 
      else
      if(dx.eq.0.)then
      nconc=nconc+.5d0
      else
      wi=wi-1d0
      endif
      endif
      nrel=nrel+1d0
      ri=ri+1d0 
      else
      if(.not.(e(i).and.e(j)))then
      nuncert=nuncert+1d0 
      endif
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
      c=nconc/nrel
      gamma=2.*(c-.5)
Ccall dblepr('sumr',4,sumr,1)
Ccall dblepr('sumw',4,sumw,1)
Ccall dblepr('sumr2',5,sumr2,1)
Ccall dblepr('sumw2',5,sumw2,1)
Ccall dblepr('sumrw',5,sumrw,1)
      sd=sumr2*sumw**2-2d0*sumr*sumw*sumrw+sumw2*sumr**2
      sd=2.*dsqrt(sd)/sumr/sumr
      return
      end
