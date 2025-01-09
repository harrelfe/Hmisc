subroutine cutgn(x, n, m, g)
  use ISO_FORTRAN_ENV
  implicit none
  integer(int32), intent(in)  :: n, m
  real(real64),   intent(in)  :: x(n)
  integer(int32), intent(out) :: g(n)
  integer(int32)              :: is, ie, ng, k, i, lte
  real(real64)                :: lastval

  ie = 0_int32
  g  = 0_int32
  ng = 0_int32

  do
   is  = ie + 1_int32
   lte = is + m - 1_int32   ! last targeted element in group
   ! Just use the insufficient group < m obs.; pool it with previous group
   ! by not incrementing ng
    if(lte > n) then
      g(is : n) = ng
      return
    end if

    ! If the mth observation is the nth, finish with
    ! the current group as the final group
    ng = ng + 1_int32
    if(lte == n) then
      g(is : n) = ng
      return
    end if

    ! There are observations beyond the last of the m in the current group
    ! See if the values beyond the mth current group's value are tied
    ! with the last value in the current group.  If so, pool observations
    ! into the current group up intil the observation that differs from
    ! the last of the m
    lastval = x(lte)
    if(x(lte + 1_int32) == lastval) then
      ! See how far the tied values go, and consume all of those tied at lastval
      k = 1_int32
      if(lte + 2_int32 <= n) then
        do i=lte + 2_int32, n
          if(x(i) /= lastval) exit
          k = k + 1_int32
        end do
      end if
      ie = lte + k
      g(is : ie) = ng
    else
      ie = lte
      g(is : ie) = ng
    end if
  if(ie == n) return
  end do

end subroutine cutgn
