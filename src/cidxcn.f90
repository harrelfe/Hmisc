! Converted from Ratfor to Fortran 2018 by Claude AI 2024-10-26
subroutine cidxcn(x, y, e, n, nrel, nconc, nuncert, c, gamma, sd, outx)
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    ! Arguments
    integer, intent(in) :: n
    real(real64), intent(in) :: x(n), y(n)
    logical, intent(in) :: e(n), outx
    real(real64), intent(out) :: nrel, nconc, nuncert, c, gamma, sd
    
    ! Local variables
    integer :: i, j
    real(real64) :: dx, dy, wi, ri
    real(real64) :: sumr, sumr2, sumw, sumw2, sumrw
    
    ! Initialize counters and sums
    nconc = 0.0_real64
    nrel = 0.0_real64
    nuncert = 0.0_real64
    sumr = 0.0_real64
    sumr2 = 0.0_real64
    sumw = 0.0_real64
    sumw2 = 0.0_real64
    sumrw = 0.0_real64
    
    ! Main computation loop
    do i = 1, n
        wi = 0.0_real64
        ri = 0.0_real64
        
        do j = 1, n
            if (j /= i) then
                dx = x(i) - x(j)
                dy = y(i) - y(j)
                
                ! Check if pair should be considered based on x ties
                if (dx /= 0.0_real64 .or. (.not. outx)) then
                    ! Case 1: i is uncensored and y(i) < y(j)
                    ! or i is uncensored, j is censored, and y values are equal
                    if ((e(i) .and. dy < 0.0_real64) .or. &
                        (e(i) .and. .not. e(j) .and. dy == 0.0_real64)) then
                        
                        if (dx < 0.0_real64) then
                            nconc = nconc + 1.0_real64
                            wi = wi + 1.0_real64
                        else if (dx == 0.0_real64) then
                            nconc = nconc + 0.5_real64
                        else
                            wi = wi - 1.0_real64
                        end if
                        nrel = nrel + 1.0_real64
                        ri = ri + 1.0_real64
                        
                    ! Case 2: j is uncensored and y(j) > y(i)
                    ! or j is uncensored, i is censored, and y values are equal
                    else if ((e(j) .and. dy > 0.0_real64) .or. &
                            (e(j) .and. .not. e(i) .and. dy == 0.0_real64)) then
                        
                        if (dx > 0.0_real64) then
                            nconc = nconc + 1.0_real64
                            wi = wi + 1.0_real64
                        else if (dx == 0.0_real64) then
                            nconc = nconc + 0.5_real64
                        else
                            wi = wi - 1.0_real64
                        end if
                        nrel = nrel + 1.0_real64
                        ri = ri + 1.0_real64
                        
                    ! Case 3: Uncertain pair (both censored)
                    else if (.not. (e(i) .and. e(j))) then
                        nuncert = nuncert + 1.0_real64
                    end if
                end if
            end if
        end do
        
        ! Accumulate sums for standard deviation calculation
        sumr = sumr + ri
        sumr2 = sumr2 + ri * ri
        sumw = sumw + wi
        sumw2 = sumw2 + wi * wi
        sumrw = sumrw + ri * wi
    end do
    
    ! Calculate c-index and gamma
    c = nconc / nrel
    gamma = 2.0_real64 * (c - 0.5_real64)
    
    ! Calculate standard deviation using Quade formula
    sd = sumr2 * sumw**2 - 2.0_real64 * sumr * sumw * sumrw + sumw2 * sumr**2
    sd = 2.0_real64 * sqrt(sd) / (sumr * sumr)
    
end subroutine cidxcn
