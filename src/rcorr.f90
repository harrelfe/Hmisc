! Converted from Ratfor 2024-10-26 using ChatGPT and the following request:
! Convert the following Ratfor code to Fortran 2018 using Fortran ISO environment without using module

subroutine rcorr(xx, n, p, itype, dmat, npair, x, y, rx, ry, work, iwork)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n, p, itype
    real(REAL64), intent(in) :: xx(n, p)
    real(REAL64), intent(out) :: dmat(p, p)
    integer, intent(out) :: npair(p, p)
    real(REAL64), intent(out) :: x(n), y(n), rx(n), ry(n), work(n)
    integer, intent(out) :: iwork(n)
    real(REAL64) :: sumx, sumx2, sumy, sumy2, sumxy, z, a, b, xk, yk, d
    integer :: i, j, k, m, np
  
    ! Initialize sums to prevent warnings
    sumx = 0.0_real64
    sumy = 0.0_real64
    sumx2 = 0.0_real64
    sumy2 = 0.0_real64
    sumxy = 0.0_real64
  
    ! Loop through each pair of variables
    do i = 1, p
      np = 0
      do k = 1, n
        if (xx(k, i) < 1.0e49_real64) np = np + 1
      end do
      npair(i, i) = np
  
      do j = i + 1, p
        m = 0
        if (itype == 1) then
          sumx = 0.0_real64
          sumy = 0.0_real64
          sumx2 = 0.0_real64
          sumy2 = 0.0_real64
          sumxy = 0.0_real64
        end if
  
        do k = 1, n
          xk = xx(k, i)
          yk = xx(k, j)
          if (xk < 1.0e49_real64 .and. yk < 1.0e49_real64) then
            m = m + 1
            if (itype == 1) then
              a = xk
              b = yk
              sumx = sumx + a
              sumx2 = sumx2 + a * a
              sumy = sumy + b
              sumy2 = sumy2 + b * b
              sumxy = sumxy + a * b
            else
              x(m) = xk
              y(m) = yk
            end if
          end if
        end do
  
        npair(i, j) = m
        if (m > 1) then
          if (itype == 1) then
            z = real(m, REAL64)
            d = (sumxy - sumx * sumy / z) / sqrt((sumx2 - sumx * sumx / z) * (sumy2 - sumy * sumy / z))
          else
            call docorr(x, y, m, d, rx, ry, work, iwork)
          end if
          dmat(i, j) = d
        else
          dmat(i, j) = 1.0e50_real64
        end if
      end do
    end do
  
    ! Set the diagonal and symmetry
    do i = 1, p
      dmat(i, i) = 1.0_real64
      do j = i + 1, p
        dmat(j, i) = dmat(i, j)
        npair(j, i) = npair(i, j)
      end do
    end do
  
    return
  end subroutine rcorr
  
  subroutine docorr(x, y, n, d, rx, ry, work, iwork)
    use ISO_FORTRAN_ENV
    implicit none
    integer, intent(in) :: n
    real(REAL64), intent(in) :: x(n), y(n)
    real(REAL64), intent(out) :: d
    real(REAL64), intent(out) :: rx(n), ry(n), work(n)
    integer, intent(out) :: iwork(n)
    real(REAL64) :: sumx, sumx2, sumy, sumy2, sumxy, a, b, z
    integer :: i
  
    call rank(n, x, work, iwork, rx)
    call rank(n, y, work, iwork, ry)
  
    sumx = 0.0_real64
    sumx2 = 0.0_real64
    sumy = 0.0_real64
    sumy2 = 0.0_real64
    sumxy = 0.0_real64
  
    do i = 1, n
      a = rx(i)
      b = ry(i)
      sumx = sumx + a
      sumx2 = sumx2 + a * a
      sumy = sumy + b
      sumy2 = sumy2 + b * b
      sumxy = sumxy + a * b
    end do
  
    z = real(n, REAL64)
    d = (sumxy - sumx * sumy / z) / sqrt((sumx2 - sumx * sumx / z) * (sumy2 - sumy * sumy / z))
  
    return
  end subroutine docorr
  