! Converted from Ratfor to Fortran 2018 by ChatGPT 2024-10-26
! Instruction to ChatGPT:
! Convert the following Ratfor code to Fortran 2018 using Fortran ISO environment without using module, and make the z function strictly internal to the subroutine

subroutine cidxcp(x1, x2, y, e, n, method, outx, nrel, nuncer, c1, c2, gamma1, gamma2, gamma, sd, c12, c21)
    use ISO_FORTRAN_ENV, only: REAL64, INT32
    implicit none
    integer(INT32), intent(in) :: n, method
    real(REAL64), intent(in) :: x1(n), x2(n), y(n)
    logical, intent(in) :: e(n), outx
    real(REAL64), intent(out) :: nrel, nuncer, c1, c2, gamma1, gamma2, gamma, sd, c12, c21
  
    integer(INT32) :: i, j
    real(REAL64) :: nconc1, nconc2, sumr, sumr2, sumw, sumw2, sumrw, sumc
    real(REAL64) :: wi, ri, dx, dx2, dy
  
    ! Initialize accumulation variables
    nconc1 = 0.0_REAL64; nconc2 = 0.0_REAL64; nrel = 0.0_REAL64; nuncer = 0.0_REAL64
    sumr = 0.0_REAL64; sumr2 = 0.0_REAL64; sumw = 0.0_REAL64
    sumw2 = 0.0_REAL64; sumrw = 0.0_REAL64; sumc = 0.0_REAL64
  
    do i = 1, n
      wi = 0.0_REAL64; ri = 0.0_REAL64
      do j = 1, n
        dx = x1(i) - x1(j)
        dx2 = x2(i) - x2(j)
        
        if ((i /= j) .and. (.not. outx .or. dx /= 0.0_REAL64 .or. dx2 /= 0.0_REAL64)) then
          dy = y(i) - y(j)
          
          if ((e(i) .and. (dy < 0.0_REAL64)) .or. (e(i) .and. .not. e(j) .and. (dy == 0.0_REAL64))) then
            nrel = nrel + 1.0_REAL64
            nconc1 = nconc1 + (z(dx < 0.0_REAL64) + 0.5_REAL64 * z(dx == 0.0_REAL64))
            nconc2 = nconc2 + (z(dx2 < 0.0_REAL64) + 0.5_REAL64 * z(dx2 == 0.0_REAL64))
            ri = ri + 1.0_REAL64
            
            if (method == 1) then
              wi = wi + (z(dx < dx2) - z(dx > dx2))
              sumc = sumc + z(dx < dx2)
            else
              wi = wi + (z(dx < 0.0_REAL64 .and. dx2 >= 0.0_REAL64) - z(dx > 0.0_REAL64 .and. dx2 <= 0.0_REAL64))
              sumc = sumc + z(dx < 0.0_REAL64 .and. dx2 >= 0.0_REAL64)
            endif
  
          else if ((e(j) .and. (dy > 0.0_REAL64)) .or. (e(j) .and. .not. e(i) .and. (dy == 0.0_REAL64))) then
            nrel = nrel + 1.0_REAL64
            nconc1 = nconc1 + (z(dx > 0.0_REAL64) + 0.5_REAL64 * z(dx == 0.0_REAL64))
            nconc2 = nconc2 + (z(dx2 > 0.0_REAL64) + 0.5_REAL64 * z(dx2 == 0.0_REAL64))
            ri = ri + 1.0_REAL64
            
            if (method == 1) then
              wi = wi + (z(dx > dx2) - z(dx < dx2))
              sumc = sumc + z(dx > dx2)
            else
              wi = wi + (z(dx > 0.0_REAL64 .and. dx2 <= 0.0_REAL64) - z(dx < 0.0_REAL64 .and. dx2 >= 0.0_REAL64))
              sumc = sumc + z(dx > 0.0_REAL64 .and. dx2 <= 0.0_REAL64)
            endif
  
          else if (.not. (e(i) .and. e(j))) then
            nuncer = nuncer + 1.0_REAL64
          endif
        endif
      end do
      
      sumr = sumr + ri
      sumr2 = sumr2 + ri * ri
      sumw = sumw + wi
      sumw2 = sumw2 + wi * wi
      sumrw = sumrw + ri * wi
    end do
  
    ! Calculate outputs
    c1 = nconc1 / nrel
    gamma1 = 2.0_REAL64 * (c1 - 0.5_REAL64)
    c2 = nconc2 / nrel
    gamma2 = 2.0_REAL64 * (c2 - 0.5_REAL64)
    gamma = sumw / sumr
    sd = 2.0_REAL64 * sqrt(sumr2 * sumw**2 - 2.0_REAL64 * sumr * sumw * sumrw + sumw2 * sumr**2) / sumr / sumr
    c12 = sumc / sumr
    c21 = sumc / sumr - gamma
  
  contains
  
    real(REAL64) function z(a)
      logical, intent(in) :: a
      if (a) then
        z = 1.0_REAL64
      else
        z = 0.0_REAL64
      endif
    end function z
  
  end subroutine cidxcp
  