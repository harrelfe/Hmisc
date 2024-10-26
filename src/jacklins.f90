! Converted from Ratfor 2024-10-26 using ChatGPT and the following request:
! Convert the following Ratfor code to Fortran 2018 using Fortran ISO environment without using module

subroutine jacklins(x, w, n, k, res)
    use ISO_FORTRAN_ENV, only: REAL64, INT32
    implicit none
  
    integer(INT32), intent(in) :: n, k
    real(REAL64), intent(in) :: x(n), w(n-1, k)
    real(REAL64), intent(out) :: res(n, k)
    integer(INT32) :: l, j, i
    real(REAL64) :: sj
  
    ! Compute leave-out-one linear statistics for each column
    do l = 1, k
      do j = 1, n
        sj = 0.0_REAL64
        do i = 1, n
          if (i < j) then
            sj = sj + w(i, l) * x(i)
          else if (i > j) then
            sj = sj + w(i - 1, l) * x(i)
          endif
        end do
        res(j, l) = sj
      end do
    end do
  
    return
  end subroutine jacklins

