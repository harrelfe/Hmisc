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
    real(REAL64) :: meanx, meany, vx, vy, xyn
    integer :: i, j, m
    logical, allocatable :: nna(:)
    real(REAL64), parameter :: nab = 1.0e49_real64   ! signifies missing value
  
    allocate(nna(n))
  
    dmat = 1.0e50_real64

    ! Loop through each pair of variables
    do i = 1, p
      dmat(i, i)  = 1e0_real64
      npair(i, i) = count(xx(:, i) < nab)
  
      do j = i + 1, p
        nna = (xx(:, i) < nab) .and. (xx(:, j) < nab)
        m   = count(nna)
        npair(i, j) = m
        if(m < 2) cycle

        x(1:m) = pack(xx(:, i), nna)
        y(1:m) = pack(xx(:, j), nna)

        if(itype == 2) then     ! replace x and y with their ranks
          call rank(m, x, work, iwork, rx)
          call rank(m, y, work, iwork, ry)
          x(1:m) = rx(1:m)
          y(1:m) = ry(1:m)
        end if

        xyn = real(m, REAL64)
        meanx = sum(x(1:m)) / xyn
        meany = sum(y(1:m)) / xyn
        x(1:m) = x(1:m) - meanx
        y(1:m) = y(1:m) - meany
        vx    = sum(x(1:m) ** 2)
        vy    = sum(y(1:m) ** 2)
        if(vx .eq. 0e0_real64 .or. vy .eq. 0e0_real64) cycle
        dmat(i, j) = sum(x(1:m) * y(1:m)) / sqrt(vx * vy)
      end do
    end do
  
    ! Finish symmetric matrices
    do i = 1, p
      do j = i + 1, p
        dmat(j, i)  = dmat(i, j)
        npair(j, i) = npair(i, j)
      end do
    end do
  
    deallocate(nna)
    return
  end subroutine rcorr
  