      SUBROUTINE largrec(x, y, n, xlim, ylim, width, height,
     &                   numbins, itype, rx, ry)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 x(*),y(*),xlim(2),ylim(2),rx(2),ry(2)
      INTEGER*4 numbins,itype
C
      xd   = xlim(2)-xlim(1)
      yd   = ylim(2)-ylim(1)
      xinc = xd/DFLOAT(numbins)
      yinc = yd/DFLOAT(numbins)
      x1   = 1d30
      x2   = 1d30
      y1   = 1d30
      y2   = 1d30
      IF(width .GE. xd .OR. height .GE. yd) THEN
         rx(1) = 1d30
         rx(2) = 1d30
         ry(1) = 1d30
         ry(2) = 1d30
         RETURN
      ENDIF
C
      w = 0d0
      h = 0d0
      area = 0d0
C
      DO xl=xlim(1),xlim(2)-width,xinc
         DO yl=ylim(1),ylim(2)-height,yinc
            DO xr=xl+width,xlim(2),xinc
               DO yu=yl+height,ylim(2),yinc
                  DO i=1,n
                     IF(x(i) .GE. xl .AND. x(i) .LE. xr .AND.
     &                    y(i) .GE. yl .AND. y(i) .LE. yu) GO TO 1
                  ENDDO
                  ar = (yu-yl)*(xr-xl)
                  if((itype.EQ.1 .AND. ar .GT. area) .OR. 
     &               (itype.EQ.2 .AND. yu-yl .GE. h .AND. 
     &                                 xr-xl .GE. w)) THEN
                     area = ar
                     w = xr - xl
                     h = yu - yl
                     x1 = xl
                     x2 = xr
                     y1 = yl
                     y2 = yu
                  ENDIF
               ENDDO
            ENDDO
 1          CONTINUE
         ENDDO
      ENDDO
      rx(1)=x1
      rx(2)=x2
      ry(1)=y1
      ry(2)=y2
      RETURN
      END

 
