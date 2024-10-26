! Converted from Ratfor 2024-10-26 using ChatGPT and the following request:
! Convert the following Ratfor code to Fortran 2018 using Fortran ISO environment without using module

subroutine wclosest(w, x, lw, lx, j)
    use ISO_FORTRAN_ENV, only: REAL64
    implicit none
    integer, intent(in) :: lw, lx
    integer, intent(out) :: j(lw)
    real(REAL64), intent(in) :: w(lw), x(lx)
    real(REAL64) :: wi, dmin, d
    integer :: i, k, m
  
    do i = 1, lw
      wi = w(i)
      dmin = 1.0e40_real64
      m = 0
      do k = 1, lx
        d = abs(x(k) - wi)
        if (d < dmin) then
          dmin = d
          m = k
        end if
      end do
      j(i) = m
    end do
  
    return
  end subroutine wclosest
  
  subroutine wclosepw(w, x, r, f, lw, lx, xd, j)
    use ISO_FORTRAN_ENV, only: REAL64
    implicit none
    integer, intent(in) :: lw, lx
    integer, intent(out) :: j(lw)
    real(REAL64), intent(in) :: w(lw), x(lx), r(lw), f
    real(REAL64), intent(out) :: xd(lx)
    real(REAL64) :: wi, dmean, sump, z, prob, ri
    integer :: i, k, m
  
    do i = 1, lw
      wi = w(i)
      dmean = 0.0_real64
  
      ! Calculate mean distance
      do k = 1, lx
        xd(k) = abs(x(k) - wi)
        dmean = dmean + xd(k)
      end do
      dmean = f * dmean / real(lx, REAL64)
  
      ! Adjust probabilities
      sump = 0.0_real64
      do k = 1, lx
        z = min(xd(k) / dmean, 1.0_real64)
        xd(k) = (1.0_real64 - z**3)**3
        sump = sump + xd(k)
      end do
  
      ! Determine index based on probabilities
      prob = 0.0_real64
      ri = r(i)
      m = 1
      do k = 1, lx
        prob = prob + xd(k) / sump
        if (ri > prob) m = m + 1
      end do
      j(i) = m
    end do
  
    return
  end subroutine wclosepw
  