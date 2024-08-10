!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! return the largest np1 and np2 such that np1 * np2 <= nprocs
! pkind = 1, np1 = np2                 (square number)
!         2, np1/2 <= np2 <= np1
!         3, np1 = np2 or np1 = np2*2  (power of 2)
! other outputs:
!     npa = np1 * np2 (active number of processes)
!     nprocs   - total number of processes
!     rank     - rank of this process
!     comm_out - MPI communicator
!     active   - .true. if this process is active; .false. otherwise
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      subroutine get_active_nprocs(pkind, np1, np2, npa,  &
     &                             nprocs, rank, comm_out, active)
      implicit none
      integer pkind, np1, np2, npa
      integer nprocs, rank, comm_out
      logical active

      include 'mpif.h'

      integer comm_in, n, np1w, np2w, npaw
      integer ic, ios
      character(40) val

! nprocs and rank in COMM_WORLD
      comm_in = MPI_COMM_WORLD
      call mpi_comm_size(comm_in, nprocs, ios)
      call mpi_comm_rank(comm_in, rank, ios)

      if (pkind <= 1) then
! square number of processes (add small number to allow for roundoff)
         np2 = int(sqrt(dble(nprocs) + 1.0d-3))
         np1 = np2
      else
! power-of-two processes
         np1 = int(log(dble(nprocs) + 1.0d-3) / log(2.0d0))
         np2 = np1 / 2
         np1 = np1 - np2
         np1 = 2**np1
         np2 = 2**np2
      endif
      npa = np1 * np2

! for option 2, go further to get the best (np1 * np2) proc grid
      if (pkind == 2 .and. npa < nprocs) then
         np1w = int(sqrt(dble(nprocs) + 1.0d-3))
         np2w = int(sqrt(dble(nprocs*2) + 1.0d-3))
         do n = np1w, np2w
            npaw = nprocs / n * n
            if (n == 1 .and. nprocs == 3) npaw = 2
            if (npaw > npa) then
               npa = npaw
               np1 = npa / n
               if (np1 < n) then
                  np2 = np1
                  np1 = n
               else
                  np2 = n
               endif
            endif
         end do
      endif

! all good if calculated is the same as requested
      comm_out = comm_in
      active = .true.
      if (nprocs == npa) return

! npa < nprocs, need to check if a strict NPROCS enforcement is required
      if (rank == 0) then
         call get_environment_variable('NPB_NPROCS_STRICT',  &
     &                                 val, ic, ios)
         if (ios == 0 .and. ic > 0) then
            if (val == '0' .or. val(1:1) == '-') then
               active = .false.
            else if (val == 'off' .or. val == 'OFF' .or.  &
     &            val(1:1) == 'n' .or. val(1:1) == 'N' .or.  &
     &            val(1:1) == 'f' .or. val(1:1) == 'F') then
               active = .false.
            endif
         endif
      endif
      call mpi_bcast(active, 1, MPI_LOGICAL, 0, comm_in, ios)

! abort if a strict NPROCS enforcement is required
      if (active) then
         if (rank == 0) then
            print 100, nprocs
  100       format(' *** ERROR determining processor topology for ',  &
     &             i0,' processes')
            if (pkind <= 1) then
               print 110, 'square', npa
            else if (pkind == 2) then
               print 110, 'grid (nx*ny, nx/2<=ny<=nx)', npa
            else
               print 110, 'power-of-two', npa
            endif
  110       format('     Expecting a ', a, ' number of processes',  &
     &             ' (such as ', i0, ')')
         endif
         call mpi_abort(comm_in, MPI_ERR_OTHER, ios)
         stop
      endif

! mark excess ranks as inactive
! split communicator based on rank value
      if (rank >= npa) then
         active = .false.
         ic = 1
      else
         active = .true.
         ic = 0
      endif
      call mpi_comm_split(comm_in, ic, rank, comm_out, ios)

      end subroutine
