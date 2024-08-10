!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  work_lhs module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module work_lhs

      use bt_data, only : problem_size

      include 'blk_par.h'

      double precision fjac(blkdim, 5, 5, 0:2),  &
     &                 njac(blkdim, 5, 5, 0:2),  &
     &                 lhsa(blkdim, 5, 5, 0:2),  &
     &                 lhsb(blkdim, 5, 5, 0:2),  &
     &                 lhsc(blkdim, 5, 5, 0:problem_size-1),  &
     &                 rhsx(blkdim, 5, 0:problem_size-1)
!$omp threadprivate( fjac, njac, lhsa, lhsb, lhsc, rhsx )

      end module work_lhs
      

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine lhsinit(ni)

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      use work_lhs
      implicit none

      integer ni

      integer i, m, jb

!---------------------------------------------------------------------
!     zero the whole left hand side for starters
!     set all diagonal values to 1. This is overkill, but convenient
!---------------------------------------------------------------------
      if (ni .gt. 0) goto 20

      do i = 0, 2, 2
!dir$ vector always
         do jb = 1, bsize
!dir$ unroll
            do m = 1, 5
               lhsa(jb,m,1,i) = 0.0d0
               lhsa(jb,m,2,i) = 0.0d0
               lhsa(jb,m,3,i) = 0.0d0
               lhsa(jb,m,4,i) = 0.0d0
               lhsa(jb,m,5,i) = 0.0d0
               lhsb(jb,m,1,i) = 0.0d0
               lhsb(jb,m,2,i) = 0.0d0
               lhsb(jb,m,3,i) = 0.0d0
               lhsb(jb,m,4,i) = 0.0d0
               lhsb(jb,m,5,i) = 0.0d0
               lhsb(jb,m,m,i) = 1.0d0
            end do
         end do
      end do

      return

  20  continue
      do i = 0, ni, ni
!dir$ vector always
         do jb = 1, bsize
!dir$ unroll
            do m = 1, 5
               lhsc(jb,m,1,i) = 0.0d0
               lhsc(jb,m,2,i) = 0.0d0
               lhsc(jb,m,3,i) = 0.0d0
               lhsc(jb,m,4,i) = 0.0d0
               lhsc(jb,m,5,i) = 0.0d0
            end do
         end do
      end do

      return
      end

