
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine read_input(class)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb
      use timing

      implicit none

      character class
      integer IERROR, fstatus


!---------------------------------------------------------------------
!    only root reads the input file
!    if input file does not exist, it uses defaults
!       ipr = 1 for detailed progress output
!       inorm = how often the norm is printed (once every inorm iterations)
!       itmax = number of pseudo time steps
!       dt = time step
!       omega 1 over-relaxation factor for SSOR
!       tolrsd = steady state residual tolerance levels
!       nx, ny, nz = number of grid points in x, y, z directions
!---------------------------------------------------------------------
      if (id .eq. root) then

         write(*, 1000)

         call check_timer_flag( timeron )

         open (unit=3,file='inputlu.data',status='old',  &
     &         access='sequential',form='formatted', iostat=fstatus)
         if (fstatus .eq. 0) then

            write(*, *) 'Reading from input file inputlu.data'

            read (3,*)
            read (3,*)
            read (3,*) ipr, inorm
            read (3,*)
            read (3,*)
            read (3,*) itmax
            read (3,*)
            read (3,*)
            read (3,*) dt
            read (3,*)
            read (3,*)
            read (3,*) omega
            read (3,*)
            read (3,*)
            read (3,*) tolrsd(1),tolrsd(2),tolrsd(3),tolrsd(4),tolrsd(5)
            read (3,*)
            read (3,*)
            read (3,*) nx0, ny0, nz0
            close(3)
         else
            ipr = ipr_default
            inorm = inorm_default
            itmax = itmax_default
            dt = dt_default
            omega = omega_default
            tolrsd(1) = tolrsd1_def
            tolrsd(2) = tolrsd2_def
            tolrsd(3) = tolrsd3_def
            tolrsd(4) = tolrsd4_def
            tolrsd(5) = tolrsd5_def
            nx0 = isiz01
            ny0 = isiz02
            nz0 = isiz03
         endif

!---------------------------------------------------------------------
!   check problem size
!---------------------------------------------------------------------
         if ( ( nx0 .lt. 4 ) .or.  &
     &        ( ny0 .lt. 4 ) .or.  &
     &        ( nz0 .lt. 4 ) ) then

            write (*,2001)
 2001       format (5x,'PROBLEM SIZE IS TOO SMALL - ',  &
     &           /5x,'SET EACH OF NX, NY AND NZ AT LEAST EQUAL TO 5')
            CALL MPI_ABORT( MPI_COMM_WORLD, MPI_ERR_OTHER, IERROR )

         end if

         if ( ( nx0 .gt. isiz01 ) .or.  &
     &        ( ny0 .gt. isiz02 ) .or.  &
     &        ( nz0 .gt. isiz03 ) ) then

            write (*,2002)
 2002       format (5x,'PROBLEM SIZE IS TOO LARGE - ',  &
     &           /5x,'NX, NY AND NZ SHOULD BE LESS THAN OR EQUAL TO ',  &
     &           /5x,'ISIZ01, ISIZ02 AND ISIZ03 RESPECTIVELY')
            CALL MPI_ABORT( MPI_COMM_WORLD, MPI_ERR_OTHER, IERROR )

         end if

         call set_class(class)

         write(*, 1001) nx0, ny0, nz0, class
         write(*, 1002) itmax

         write(*, 1003) total_nodes
         if (total_nodes .ne. no_nodes) write (*, 1004) no_nodes
         write(*, *)

 1000 format(//, ' NAS Parallel Benchmarks 3.4 -- LU Benchmark',/)
 1001    format(' Size: ', i4, 'x', i4, 'x', i4, '  (class ', a, ')')
 1002    format(' Iterations: ', i4)
 1003    format(' Total number of processes: ', i6)
 1004    format(' WARNING: Number of processes is not in a form of',  &
     &          ' (n1*n2, n1/n2 <= 2).'/  &
     &          ' Number of active processes: ', i6)


      end if

      call bcast_inputs

      return
      end


