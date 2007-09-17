subroutine cidxcp(x1,x2,y,e,n,method,outx,
	nrel,nuncer,c1,c2,gamma1,gamma2,gamma,sd,c12,c21)
implicit DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION x1(n),x2(n),y(n)
logical e(n),outx
integer n,method,i,j
DOUBLE PRECISION nrel,nuncer,nconc1,nconc2,c1,c2,gamma1,gamma2,
	sumr,sumr2,sumw,sumw2,sumrw,
	wi,ri,sumc,c12,c21,gamma,sd
DOUBLE PRECISION dx,dx2,dy
nconc1=0d0;nconc2=0d0;nrel=0d0;nuncer=0d0;sumr=0d0;sumr2=0d0;sumw=0d0;
sumw2=0d0;sumrw=0d0;sumc=0d0;
do i=1,n {
  wi=0d0;ri=0d0;
  do j=1,n {
    dx=x1(i)-x1(j);dx2=x2(i)-x2(j);
    if((i!=j) & (!outx | dx!=0. | dx2!=0.)) {
      dy=y(i)-y(j);
      if ((e(i)&(dy<0.))|(e(i)&^e(j)&(dy==0.)))	{
        nrel=nrel+1d0;
        nconc1=nconc1+(z(dx<0.)+.5D0*z(dx==0.));
        nconc2=nconc2+(z(dx2<0.)+.5D0*z(dx2==0.));
        ri=ri+1d0;
        if (method==1) {
          wi=wi+(z(dx<dx2)-z(dx>dx2));
          sumc=sumc+z(dx<dx2);
        }
        else {
          wi=wi+(z(dx<0.&dx2>=0.)-z(dx>0.&dx2<=0.));
          sumc=sumc+z(dx<0.&dx2>=0.);
        }
      }
      else if ((e(j)&(dy>0.))|(e(j)&^e(i)&(dy==0.))) {
        nrel=nrel+1d0;
        nconc1=nconc1+(z(dx>0.)+.5D0*z(dx==0.));
        nconc2=nconc2+(z(dx2>0.)+.5D0*z(dx2==0.));
        ri=ri+1d0;
        if (method==1) {
          wi=wi+(z(dx>dx2)-z(dx<dx2));
          sumc=sumc+z(dx>dx2);
        }
        else {
          wi=wi+(z(dx>0.&dx2<=0.)-z(dx<0.&dx2>=0.));
          sumc=sumc+z(dx>0.&dx2<=0.);
        }
      }
      else if (^(e(i)&e(j))) nuncer=nuncer+1d0;
    }
  }
  sumr=sumr+ri; sumr2=sumr2+ri*ri
  sumw=sumw+wi; sumw2=sumw2+wi*wi; sumrw=sumrw+ri*wi
}
c1=nconc1/nrel; gamma1=2D0*(c1-.5D0);
c2=nconc2/nrel; gamma2=2D0*(c2-.5D0);
gamma=sumw/sumr
sd=sumr2*sumw**2-2D0*sumr*sumw*sumrw+sumw2*sumr**2;
sd=2D0*dsqrt(sd)/sumr/sumr;
c12=sumc/sumr; c21=sumc/sumr-gamma
return
end

function z(a)
DOUBLE PRECISION z
logical a
if(a)z=1d0
else z=0d0
return
end

