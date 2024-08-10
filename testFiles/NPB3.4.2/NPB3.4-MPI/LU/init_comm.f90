
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine init_comm 

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   initialize MPI and establish rank and size
!
! This is a module in the MPI implementation of LUSSOR
! pseudo application from the NAS Parallel Benchmarks. 
!
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

      integer nodedim
      integer IERROR


!---------------------------------------------------------------------
!    initialize MPI communication
!---------------------------------------------------------------------
      call MPI_INIT( IERROR )

!---------------------------------------------------------------------
!     get a process grid that requires a (nx*ny) number of procs.
!     excess ranks are marked as inactive.
!---------------------------------------------------------------------
      call get_active_nprocs(2, xdim, ydim, no_nodes,  &
     &                       total_nodes, node, comm_solve, active)

      if (.not. active) return

!---------------------------------------------------------------------
!   establish the global rank of this process and the group size
!---------------------------------------------------------------------
      id = node
      num = no_nodes
      root = 0

      ndim   = nodedim(num)

      if (.not. convertdouble) then
         dp_type = MPI_DOUBLE_PRECISION
      else
         dp_type = MPI_REAL
      endif


      return
      end
