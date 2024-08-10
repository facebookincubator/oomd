!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  work_lhs module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module work_lhs

      use sp_data, only : problem_size

!-----------------------------------------------------------------------
!   Working array for LHS
!-----------------------------------------------------------------------

      integer, parameter :: IMAXP=problem_size/2*2
      double precision  &
     &      lhs (5,0:IMAXP),  &
     &      lhsp(5,0:IMAXP),  &
     &      lhsm(5,0:IMAXP),  &
     &      cv  (0:problem_size-1),  &
     &      rhov(0:problem_size-1)
!$omp threadprivate(lhs, lhsp, lhsm, cv, rhov)

      end module work_lhs


!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine lhsinit(ni, lhs, lhsp, lhsm)

       implicit none

       integer ni
       double precision lhs(5,0:*), lhsp(5,0:*), lhsm(5,0:*)

       integer m

!---------------------------------------------------------------------
!     zap the whole left hand side for starters
!     set all diagonal values to 1. This is overkill, but convenient
!---------------------------------------------------------------------
       do   m = 1, 5
          lhs (m,0) = 0.0d0
          lhsp(m,0) = 0.0d0
          lhsm(m,0) = 0.0d0
          lhs (m,ni) = 0.0d0
          lhsp(m,ni) = 0.0d0
          lhsm(m,ni) = 0.0d0
       end do
       lhs (3,0) = 1.0d0
       lhsp(3,0) = 1.0d0
       lhsm(3,0) = 1.0d0
       lhs (3,ni) = 1.0d0
       lhsp(3,ni) = 1.0d0
       lhsm(3,ni) = 1.0d0
 
       return
       end
