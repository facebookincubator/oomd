!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  work_lhs module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module work_lhs

      use bt_data, only : problem_size

      double precision fjac(5, 5,    0:problem_size),  &
     &                 njac(5, 5,    0:problem_size),  &
     &                 lhs (5, 5, 3, 0:problem_size)
!$omp threadprivate( fjac, njac, lhs )

      end module work_lhs
      

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine lhsinit(lhs, ni)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      integer i, m, n, ni
      double precision lhs(5,5,3,0:ni)

!---------------------------------------------------------------------
!     zero the whole left hand side for starters
!     set all diagonal values to 1. This is overkill, but convenient
!---------------------------------------------------------------------
      i = 0
      do m = 1, 5
         do n = 1, 5
            lhs(m,n,1,i) = 0.0d0
            lhs(m,n,2,i) = 0.0d0
            lhs(m,n,3,i) = 0.0d0
         end do
         lhs(m,m,2,i) = 1.0d0
      end do
      i = ni
      do m = 1, 5
         do n = 1, 5
            lhs(m,n,1,i) = 0.0d0
            lhs(m,n,2,i) = 0.0d0
            lhs(m,n,3,i) = 0.0d0
         end do
         lhs(m,m,2,i) = 1.0d0
      end do

      return
      end

