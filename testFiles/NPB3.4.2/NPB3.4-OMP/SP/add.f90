
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine  add

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! addition of update to the vector u
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer i,j,k,m

       if (timeron) call timer_start(t_add)
!$omp parallel do default(shared) private(i,j,k,m) collapse(2)
       do k = 1, nz2
          do j = 1, ny2
             do i = 1, nx2
                do m = 1, 5
                   u(m,i,j,k) = u(m,i,j,k) + rhs(m,i,j,k)
                end do
             end do
          end do
       end do
       if (timeron) call timer_stop(t_add)

       return
       end

