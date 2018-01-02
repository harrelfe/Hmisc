      SUBROUTINE largrec(x, y, n, xlim, ylim, width, height,
     &                   numbins, itype, rx, ry)
C     *********************************************************
C     * x(n) - array of x values of data points
      DOUBLE PRECISION x(*)
C     * y(n) - array of y values of data points
      DOUBLE PRECISION y(*)
C     * n - number of data points
      INTEGER n
C     * xlim(2) - contains left and right limits of x axis
      DOUBLE PRECISION xlim(2)
C     * ylim(2) - contains bottom and top limits of y axis
      DOUBLE PRECISION ylim(2)
C     * width - minimum allowable width of empty space
      DOUBLE PRECISION width
C     * height - minimum allowable height of empty space
      DOUBLE PRECISION height
C     * numbins - number of blocks to chunk axis ranges into
      INTEGER numbins
C     * itype - how to favor box size
      INTEGER itype
C     * rx(2) - left and right limits of found box
      DOUBLE PRECISION rx(2)
C     * ry(2) - top and bottom limits of found box
      DOUBLE PRECISION ry(2)
C     * xd - x distance between x limits
      DOUBLE PRECISION xd
C     * yd - y distance between y limits
      DOUBLE PRECISION yd
C     * xinc - amount to add to x axis search box bounds
      DOUBLE PRECISION xinc
C     * yinc - amount to add to y axis search box bounds
      DOUBLE PRECISION yinc
C     * xl - left search box bound
      DOUBLE PRECISION xl
C     * xr - right search box bound
      DOUBLE PRECISION xr
C     * yb - bottom search box bound
      DOUBLE PRECISION yb
C     * yt - top search box bound
      DOUBLE PRECISION yt
C     * i - itterator variable
      INTEGER i
C     * area - area of empty space
      DOUBLE PRECISION area
C     * w - width of empty space
      DOUBLE PRECISION w
C     * h - height of empty space
      DOUBLE PRECISION h
C     * ar - tempory area storage
      DOUBLE PRECISION ar
C
      xd   = xlim(2)-xlim(1)
      yd   = ylim(2)-ylim(1)
      xinc = xd / numbins
      yinc = yd / numbins
      rx(1) = 1d30
      rx(2) = 1d30
      ry(1) = 1d30
      ry(2) = 1d30
      IF(width .GE. xd .OR. height .GE. yd) THEN
         RETURN
      ENDIF
C
      w = 0d0
      h = 0d0
      area = 0d0
C
      xl=xlim(1)
      DO WHILE (xl .LE. xlim(2)-width)
         yb = ylim(1)
         DO WHILE (yb .LE. ylim(2)-height)
            xr = xl + width
            DO WHILE (xr .LE. xlim(2))
               yt = yb + height
               DO WHILE (yt .LE. ylim(2))
                  DO i=1,n
                     IF(x(i) .GE. xl .AND. x(i) .LE. xr .AND.
     &                    y(i) .GE. yb .AND. y(i) .LE. yt) GO TO 1
                  ENDDO
                  ar = (yt-yb)*(xr-xl)
                  if((itype.EQ.1 .AND. ar .GT. area) .OR. 
     &               (itype.EQ.2 .AND. yt-yb .GE. h .AND. 
     &                                 xr-xl .GE. w)) THEN
                     area = ar
                     w = xr - xl
                     h = yt - yb
                     rx(1) = xl
                     rx(2) = xr
                     ry(1) = yb
                     ry(2) = yt
                  ENDIF
                  yt = yt + yinc
               ENDDO
               xr = xr + xinc
            ENDDO
 1          CONTINUE
            yb = yb + yinc
         ENDDO
         xl = xl + xinc
      ENDDO
      RETURN
      END
