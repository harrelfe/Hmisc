#include "R.h"

void sort2(np,ra,rb)
int *np;
double ra[];
int rb[];
{
  int l,j,ir,i,n,rrb,*xrb;
  double rra,*xra;

  n = *np;
  xra=ra-1;
  xrb=rb-1;

  l=(n >> 1)+1;
  ir=n;
  for(;;) {
    if(l > 1) {
      rra=xra[--l];
      rrb=xrb[l];
    } else {
      rra=xra[ir];
      rrb=xrb[ir];
      xra[ir]=xra[1];
      xrb[ir]=xrb[1];
      if(--ir == 1) {
        xra[1]=rra;
        xrb[1]=rrb;
        return;
      }
    }
    i=l;
    j=l << 1;
    while (j <= ir) {
      if ( j < ir && xra[j] < xra[j+1]) ++j;
      if (rra < xra[j]) {
        xra[i]=xra[j];
        xrb[i]=xrb[j];
        j += (i=j);
      }
      else j=ir+1;
    }
    xra[i]=rra;
    xrb[i]=rrb;
  }
}


void crank(np, w)
int *np;
double w[];

{
  int n,j=1,ji,jt;
  double rank,*xw;

  n = *np;
  xw = w-1;

  while (j < n) {
    if(xw[j+1] != xw[j]) {
      xw[j]=j;
      ++j;
    } else {
      for (jt=j+1;jt<=n;jt++)
        if (xw[jt] != xw[j]) break;
      rank=0.5*(j+jt-1);
      for (ji=j;ji<=(jt-1);ji++) xw[ji]=rank;
      j=jt;
    }
  }
  if (j == n) xw[n]=n;
}


void F77_SUB(rank)(np, x, w, ix, r)
int *np, ix[];
double x[],r[],w[];

{
  int n, *xix, i;
  double *xx, *xr, *xw;
  n = *np;
  xx = x-1;
  xix = ix-1;
  xr = r-1;
  xw = w-1;

  for(i=1; i<=n; i++) {
    xix[i]=i;
    xw[i]=xx[i];
  }
  sort2(np, w, ix);
  crank(np, w);
  for(i=1; i<=n; i++) xr[xix[i]] = xw[i];
}


