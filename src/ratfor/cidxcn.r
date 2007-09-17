#------------------------------------------------------------------------
#       Compute c-index (c) and Brown-Hollander-Krowar-Goodman-Kruskal-Somer
#       rank correlation (gamma) between X and Y with censoring indicator E
#       Also returns number of relevant, concordant, and uncertain pairs
#       (nrel, nconc, nuncert) and estimated s.d. of gamma (sd) using
#       Quade formula (see SAS PROC MATPAR).  Pairs with tied x are
#       excluded if outx=.TRUE.
#
#       F. Harrell  27Nov90
#                   Modification of SAS Procedure KGKC (1980)
#-------------------------------------------------------------------------
SUBROUTINE cidxcn(x,y,e,n,nrel,nconc,nuncert,c,gamma,sd,outx)
IMPLICIT DOUBLE PRECISION (a-h,o-z)
DOUBLE PRECISION x(n),y(n),dx,dy
LOGICAL e(n),outx
DOUBLE PRECISION nrel,nuncert,nconc
nconc=0d0
nrel=0d0
nuncert=0d0
sumr=0d0
sumr2=0d0
sumw=0d0
sumw2=0d0
sumrw=0d0
do i=1,n                                        {
        wi=0d0
        ri=0d0
        do j=1,n
        if(j^=i)        {
                dx=x(i)-x(j)
                dy=y(i)-y(j)
                if(dx!=0. | !outx)      {
                        if((e(i)&dy<0.)|(e(i)&!e(j)&dy==0.))    {
                                if(dx<0.)       {
                                  nconc=nconc+1d0
                                  wi=wi+1d0     } 
                               else
                                if(dx==0.)nconc=nconc+.5d0 
                               else
                                wi=wi-1d0
                                nrel=nrel+1d0
                                ri=ri+1d0       } 
                       else     
                        if((e(j)&dy>0.)|(e(j)&!e(i)&dy==0.))    {
                                if(dx>0.)       {
                                   nconc=nconc+1d0
                                   wi=wi+1d0    } 
                               else
                                if(dx==0.) nconc=nconc+.5d0 
                               else 
                                   wi=wi-1d0
                                nrel=nrel+1d0
                                ri=ri+1d0               } 
                       else
                        if(!(e(i)&e(j)))nuncert=nuncert+1d0  }
                                                                } 
                sumr=sumr+ri
                sumr2=sumr2+ri*ri
                sumw=sumw+wi
                sumw2=sumw2+wi*wi
                sumrw=sumrw+ri*wi
                                                }
c=nconc/nrel
gamma=2.*(c-.5)
#call dblepr('sumr',4,sumr,1)
#call dblepr('sumw',4,sumw,1)
#call dblepr('sumr2',5,sumr2,1)
#call dblepr('sumw2',5,sumw2,1)
#call dblepr('sumrw',5,sumrw,1)
sd=sumr2*sumw**2-2d0*sumr*sumw*sumrw+sumw2*sumr**2
sd=2.*dsqrt(sd)/sumr/sumr
return
end




