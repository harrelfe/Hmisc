    subroutine hlqest(x, ran, nn, retval)
      use ISO_FORTRAN_ENV
        implicit none
        integer(int32), intent(in)  :: nn
        real(real64),   intent(in)  :: x(nn)
        real(real64),   intent(in)  :: ran(1000)
        real(real64),   intent(out) :: retval
        
        integer(int64)  :: n, np, sm, l, k1, k2, sq, mdlrow, k, i, j, iran
        real   (real64) :: am, amn, amx
        integer(int64), allocatable :: lb(:), rb(:), q(:)

        n    = nn
        iran = 0

        ! Handle small n
        if (n < 3) then
            retval = (x(1) + x(n)) / 2.0
            return
        endif

        ! Allocate memory for lb, rb, and q
        allocate(lb(n), rb(n), q(n))

        ! Total number of pairs and the median(s) index
        np = n  * (n  + 1) / 2
        k1 = (np + 1) / 2
        k2 = (np + 2) / 2

        ! Initialize lb and rb
        lb = [(i, i=1, n)]       ! Vectorized initialization of lb
        rb = n                   ! Vectorized initialization of rb to n for all elements

        sm = np                ! Total number of pairs
        l  = 0                 ! Number of pairs less than those in set at step m
        am = x((n + 1) / 2) + x((n + 2) / 2)

        ! Main iterative process
        do
            j   = n

            ! Vectorized loop to compute q and sq
            q  = 0
            sq = 0

            do i = 1, n
                do while (j >= i)
                    if (x(i) + x(j) < am) then
                        q(i) = j - i + 1
                        sq = sq + q(i)
                        exit
                    endif
                    j = j - 1
                end do
            end do

            ! Handle case where partitions are the same (ties)
            if (sq == l) then
                amx = x(1) + x(1)
                amn = x(n) + x(n)
                do i = 1, n
                    if (lb(i) > rb(i)) cycle
                    amn = min(amn, x(lb(i)) + x(i))  ! Vectorized min within bounds
                    amx = max(amx, x(rb(i)) + x(i))  ! Vectorized max within bounds
                end do
                am = (amn + amx) / 2.0
                if ((am <= amn) .or. (am > amx)) am = amx
                if ((amn /= amx) .and. (sm /= 2)) cycle
                retval = am / 2.0
                exit
            endif  ! end if(sq == l)

            ! Handle if we're nearly done (values on the border)
            if (sq == k2 - 1 .or. sq == k1) then
              amx = x(1) + x(1)
              amn = x(n) + x(n)
            
                ! Loop over all rows to find max of those < am and min of those >= am, considering q
                do i = 1, n
                    if (q(i) > 0) amx = max(amx, x(i) + x(i + q(i) - 1))
                        ! There are q(i) pairs in row i that satisfy x[i] + x[j] < am
                    if (q(i) < (n - i + 1)) amn = min(amn, x(i) + x(i + q(i)))
                        ! There are (n - i + 1 - q(i)) pairs in row i that satisfy x[i] + x[j] >= am
                end do

                ! Handle case where k1 < k2 (when we need the average of two values)
                if (k1 < k2) then
                    retval = (amn + amx) / 4.0
                else if (sq == k1) then
                  retval = amx / 2.0
                else if (sq == k1 - 1) then
                  retval = amn / 2.0
                else
                  retval = (amn + amx) / 4.0
                end if
                exit
            endif  ! end if(sq == k2 - 1 .or ...)

            ! Update bounds based on the size of sq using vectorization
            if (sq < k1) then
                lb = [(i + q(i), i=1, n)]  ! Vectorized update of left bounds
            else
                rb = [(i + q(i) - 1, i=1, n)]  ! Vectorized update of right bounds
            endif

            ! Recalculate sm and l using vectorized array operations
            l = sum(lb - [(i, i=1, n)])          ! Vectorized summation of lb - i
            sm = sum(rb - lb + 1)                ! Vectorized summation of pairs still in set

            ! If we still have more than 2 pairs, pick a random row and continue
            if (sm > 2) then
                iran = iran + 1
                if(iran > 1000) iran = 1
                k = int(ran(iran) * dble(sm))  
                do i = 1, n
                    j = i
                    if (k <= rb(i) - lb(i)) exit
                    k = k - rb(i) + lb(i) - 1
                end do
                mdlrow = (lb(j) + rb(j)) / 2
                am = x(j) + x(mdlrow)
                cycle
            endif

            amx = x(1) + x(1)     ! ?
            amn = x(n) + x(n)     ! ?
      
            ! Otherwise, finalize and exit
            do i = 1, n
                if (lb(i) > rb(i)) cycle
                amn = min(amn, x(lb(i)) + x(i))
                amx = max(amx, x(rb(i)) + x(i))
            end do
            am = (amx + amn) / 2.0
            if ((am <= amn) .or. (am > amx)) am = amx
            if ((am /= amx .and. sm /= 2))   cycle
            retval = am / 2.0
            exit
        end do

        ! Deallocate memory
        deallocate(lb, rb, q)
        return
    end subroutine hlqest
