! Converted from Ratfor 2024-10-26 using ChatGPT and the following request:
! Convert the following Ratfor code to Fortran 2018 using Fortran ISO environment without using module

subroutine maxempr(ax, ay, x, y, n, w, h, z, area, rect)
    use ISO_FORTRAN_ENV, only: REAL64, INT32
    implicit none
  
    integer(INT32), intent(in) :: n
    real(REAL64), intent(in) :: ax(2), ay(2), x(n), y(n), w, h, z(3)
    real(REAL64), intent(out) :: area, rect(4)
    real(REAL64) :: maxr, tl, tr, li, ri
    real(REAL64) :: area_candidate
    integer(INT32) :: i, j, k
  
    ! Initialize max area and set initial rectangle boundaries
    maxr = z(1) * abs(ay(2) - ay(1))
    rect(1) = z(2)
    rect(2) = ay(1)
    rect(3) = z(3)
    rect(4) = ay(2)
  
    ! Iterate through points to determine largest rectangles
    do i = 1, n
      tl = ax(1)
      tr = ax(2)
  
      if (i < n) then
        do j = i + 1, n
          if (x(j) > tl .and. x(j) < tr) then
            ! Check horizontal slices (j == i + 1)
            area_candidate = (tr - tl) * (y(j) - y(i))
            if (area_candidate > maxr .and. (tr - tl) > w .and. (y(j) - y(i)) > h) then
              maxr = area_candidate
              rect(1) = tl
              rect(2) = y(i)
              rect(3) = tr
              rect(4) = y(j)
            end if
  
            ! Update boundaries based on current point
            if (x(j) > x(i)) then
              tr = x(j)
            else
              tl = x(j)
            end if
          end if
        end do
      end if
  
      ! Check open rectangles above (x(i), y(i))
      area_candidate = (tr - tl) * (ay(2) - y(i))
      if (area_candidate > maxr .and. (tr - tl) > w .and. (ay(2) - y(i)) > h) then
        maxr = area_candidate
        rect(1) = tl
        rect(2) = y(i)
        rect(3) = tr
        rect(4) = ay(2)
      end if
  
      ! Check open rectangles below (x(i), y(i))
      ri = ax(2)
      li = ax(1)
      do k = 1, n
        if (y(k) < y(i) .and. x(k) > x(i)) ri = min(ri, x(k))
        if (y(k) < y(i) .and. x(k) < x(i)) li = max(li, x(k))
      end do
  
      area_candidate = (ri - li) * (y(i) - ay(1))
      if (area_candidate > maxr .and. (ri - li) > w .and. (y(i) - ay(1)) > h) then
        maxr = area_candidate
        rect(1) = li
        rect(2) = ay(1)
        rect(3) = ri
        rect(4) = y(i)
      end if
    end do
  
    area = maxr
    return
  end subroutine maxempr
  