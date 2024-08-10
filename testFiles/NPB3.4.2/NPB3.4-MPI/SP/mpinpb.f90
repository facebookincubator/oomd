!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      include 'mpif.h'

      integer   node, no_nodes, total_nodes, root, comm_setup,  &
     &          comm_solve, comm_rhs, dp_type
      logical   active

      integer   DEFAULT_TAG
      parameter (DEFAULT_TAG = 0)

      end module mpinpb
