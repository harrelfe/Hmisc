      subroutine cidxcn(x,y,e,n,nrel,nconc,nuncert,c,gamma,sd,outx)
      implicit real*8 (a-h,o-z)
      real*8 x(n),y(n),dx,dy
      logical e(n),outx
      real*8 nrel,nuncert,nconc
      nconc=0d0
      nrel=0d0
      nuncert=0d0
      sumr=0d0
      sumr2=0d0
      sumw=0d0
      sumw2=0d0
      sumrw=0d0
      do 23000 i=1,n
      wi=0d0
      ri=0d0
      do 23002 j=1,n
      if(.not.(j.ne.i))goto 23004
      dx=x(i)-x(j)
      dy=y(i)-y(j)
      if(.not.(dx.ne.0d0 .or. .not.outx))goto 23006
      if(.not.((e(i).and.dy.lt.0d0).or.(e(i).and.
     & .not.e(j).and.dy.eq.0d0)) ) goto 23008
      if(.not.(dx.lt.0d0))goto 23010
      nconc=nconc+1d0
      wi=wi+1d0
      goto 23011
23010 continue
      if(.not.(dx.eq.0d0))goto 23012
      nconc=nconc+.5d0
      goto 23013
23012 continue
      wi=wi-1d0
23013 continue
23011 continue
      nrel=nrel+1d0
      ri=ri+1d0
      goto 23009
23008 continue
      if(.not.((e(j).and.dy.gt.0d0).or.(e(j).and.
     & .not.e(i).and.dy.eq.0d0)) ) goto 23014
      if(.not.(dx.gt.0d0))goto 23016
      nconc=nconc+1d0
      wi=wi+1d0
      goto 23017
23016 continue
      if(.not.(dx.eq.0d0))goto 23018
      nconc=nconc+.5d0
      goto 23019
23018 continue
      wi=wi-1d0
23019 continue
23017 continue
      nrel=nrel+1d0
      ri=ri+1d0
      goto 23015
23014 continue
      if(.not.(.not.(e(i).and.e(j))))goto 23020
      nuncert=nuncert+1d0 
23020 continue
23015 continue
23009 continue
23006 continue
23004 continue
23002 continue
      sumr=sumr+ri
      sumr2=sumr2+ri*ri
      sumw=sumw+wi
      sumw2=sumw2+wi*wi
      sumrw=sumrw+ri*wi
23000 continue
      c=nconc/nrel
      gamma=2.*(c-.5d0)
      sd=sumr2*sumw**2-2d0*sumr*sumw*sumrw+sumw2*sumr**2
      sd=2d0*dsqrt(sd)/sumr/sumr
      return
      end
