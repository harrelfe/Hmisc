! Converted from Ratfor to Fortran 2018 by Claude AI 2024-10-26
!
! Hoeffding's D Statistics Implementation
! Modern Fortran without module structure

subroutine hoeffd(xx, n, p, dmat, aadmat, madmat, npair, x, y, rx, ry, rj)
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  
  ! Arguments
  integer, intent(in) :: n, p
  real(real64), intent(in) :: xx(n,p)
  real(real64), intent(out) :: dmat(p,p), aadmat(p,p), madmat(p,p)
  integer, intent(out) :: npair(p,p)
  real(real64), intent(out) :: x(n), y(n), rx(n), ry(n), rj(n)
  
  ! Local variables
  integer :: i, j, k, m, np
  real(real64) :: xk, yk, d, aad, maxad
  real(real64), parameter :: LARGE_VAL = 1.0e49_real64
  
  ! Initialize arrays
  dmat = 0.0_real64
  aadmat = 0.0_real64
  madmat = 0.0_real64
  npair = 0
  
  do i = 1, p
    np = count(xx(:,i) < LARGE_VAL)
    npair(i,i) = np
    
    do j = i+1, p
      m = 0
      do k = 1, n
        xk = xx(k,i)
        yk = xx(k,j)
        if (xk < LARGE_VAL .and. yk < LARGE_VAL) then
          m = m + 1
          x(m) = xk
          y(m) = yk
        end if
      end do
      
      npair(i,j) = m
      if (m > 4) then
        call hoeff(x, y, m, d, aad, maxad, rx, ry, rj)
        dmat(i,j) = d
        aadmat(i,j) = aad
        madmat(i,j) = maxad
      else
        dmat(i,j) = LARGE_VAL
      end if
    end do
  end do
  
  ! Fill lower triangle
  do i = 1, p
    dmat(i,i) = 1.0_real64/30.0_real64
    do j = i+1, p
      dmat(j,i) = dmat(i,j)
      npair(j,i) = npair(i,j)
      aadmat(j,i) = aadmat(i,j)
      madmat(j,i) = madmat(i,j)
    end do
  end do
  
end subroutine hoeffd

subroutine hoeff(x, y, n, d, aad, maxad, rx, ry, rj)
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  
  ! Arguments
  integer, intent(in) :: n
  real(real64), intent(in) :: x(n), y(n)
  real(real64), intent(out) :: d, aad, maxad
  real(real64), intent(out) :: rx(n), ry(n), rj(n)
  
  ! Local variables
  integer :: i
  real(real64) :: q, r, s, z, rxi, ryi, rji, ad
  
  call jrank(x, y, n, rx, ry, rj)
  
  q = 0.0_real64
  r = 0.0_real64
  s = 0.0_real64
  aad = 0.0_real64
  maxad = 0.0_real64
  z = real(n, real64)
  
  do i = 1, n
    rxi = rx(i)
    ryi = ry(i)
    rji = rj(i)
    ad = abs((rji/z) - (rxi/z)*(ryi/z))
    aad = aad + ad
    maxad = max(maxad, ad)
    q = q + (rxi-1.0_real64)*(rxi-2.0_real64)*(ryi-1.0_real64)*(ryi-2.0_real64)
    r = r + (rxi-2.0_real64)*(ryi-2.0_real64)*(rji-1.0_real64)
    s = s + (rji-1.0_real64)*(rji-2.0_real64)
  end do
  
  aad = aad / z
  d = (q-2.0_real64*(z-2.0_real64)*r+(z-2.0_real64)*(z-3.0_real64)*s) / &
      (z*(z-1.0_real64)*(z-2.0_real64)*(z-3.0_real64)*(z-4.0_real64))
  
end subroutine hoeff

subroutine jrank(x, y, n, rx, ry, r)
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  
  ! Arguments
  integer, intent(in) :: n
  real(real64), intent(in) :: x(n), y(n)
  real(real64), intent(out) :: rx(n), ry(n), r(n)
  
  ! Local variables
  integer :: i, j
  real(real64) :: xi, yi, ri, rix, riy, cx, cy
  
  do i = 1, n
    xi = x(i)
    yi = y(i)
    ri = 1.0_real64
    rix = 1.0_real64
    riy = 1.0_real64
    
    do j = 1, n
      if (i /= j) then
        cx = merge(0.5_real64, merge(1.0_real64, 0.0_real64, x(j) < xi), x(j) == xi)
        cy = merge(0.5_real64, merge(1.0_real64, 0.0_real64, y(j) < yi), y(j) == yi)
        
        rix = rix + cx
        riy = riy + cy
        ri = ri + cx*cy
      end if
    end do
    
    rx(i) = rix
    ry(i) = riy
    r(i) = ri
  end do
  
end subroutine jrank
