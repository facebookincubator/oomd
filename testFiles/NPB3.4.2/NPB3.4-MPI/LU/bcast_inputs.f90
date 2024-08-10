!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine bcast_inputs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb
      use timing

      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer ierr

!---------------------------------------------------------------------
!   root broadcasts the data
!   The data isn't contiguous or of the same type, so it's not
!   clear how to send it in the "MPI" way. 
!   We could pack the info into a buffer or we could create
!   an obscene datatype to handle it all at once. Since we only
!   broadcast the data once, just use a separate broadcast for
!   each piece. 
!---------------------------------------------------------------------
      call MPI_BCAST(ipr, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(inorm, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(itmax, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(dt, 1, dp_type, root, comm_solve, ierr)
      call MPI_BCAST(omega, 1, dp_type, root, comm_solve, ierr)
      call MPI_BCAST(tolrsd, 5, dp_type, root, comm_solve, ierr)
      call MPI_BCAST(nx0, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(ny0, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(nz0, 1, MPI_INTEGER, root, comm_solve, ierr)
      call MPI_BCAST(timeron, 1, MPI_LOGICAL, root, comm_solve,  &
     &               ierr)

      return
      end


