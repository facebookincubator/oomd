
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setup_mpi

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! set up MPI stuff
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer error, nc

      call mpi_init(error)

      if (.not. convertdouble) then
         dp_type = MPI_DOUBLE_PRECISION
      else
         dp_type = MPI_REAL
      endif

!---------------------------------------------------------------------
!     get a process grid that requires a square number of procs.
!     excess ranks are marked as inactive.
!---------------------------------------------------------------------
      call get_active_nprocs(1, nc, maxcells, no_nodes,  &
     &                       total_nodes, node, comm_setup, active)

      if (.not. active) return

      call mpi_comm_dup(comm_setup, comm_solve, error)
      call mpi_comm_dup(comm_setup, comm_rhs, error)

!---------------------------------------------------------------------
!     let node 0 be the root for the group (there is only one)
!---------------------------------------------------------------------
      root = 0

      return
      end

