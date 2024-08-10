!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      include 'mpif.h'

!--------------------------------------------------------------------
! 'np' number of processors, 'np_min' min number of processors
!--------------------------------------------------------------------
      integer np_min, np

! we need a bunch of logic to keep track of how
! arrays are laid out. 
! coords of this processor
      integer me, me1, me2

! need a communicator for row/col in processor grid
      integer comm_solve, commslice1, commslice2
      logical active

! mpi data types
      integer dc_type

      end module mpinpb

