!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine  add

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     addition of update to the vector u
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i, j, k, m

      if (timeron) call timer_start(t_add)
!$omp parallel do default(shared) private(i,j,k,m) collapse(2)
      do     k = 1, grid_points(3)-2
         do     j = 1, grid_points(2)-2
            do     i = 1, grid_points(1)-2
               do    m = 1, 5
                  u(m,i,j,k) = u(m,i,j,k) + rhs(m,i,j,k)
               enddo
            enddo
         enddo
      enddo
      if (timeron) call timer_stop(t_add)

      return
      end
