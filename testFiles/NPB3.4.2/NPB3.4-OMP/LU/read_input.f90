
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine read_input

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer  fstatus
!$    integer  omp_get_max_threads
!$    external omp_get_max_threads


!---------------------------------------------------------------------
!    if input file does not exist, it uses defaults
!       ipr = 1 for detailed progress output
!       inorm = how often the norm is printed (once every inorm iterations)
!       itmax = number of pseudo time steps
!       dt = time step
!       omega 1 over-relaxation factor for SSOR
!       tolrsd = steady state residual tolerance levels
!       nx, ny, nz = number of grid points in x, y, z directions
!---------------------------------------------------------------------

         write(*, 1000)

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
            nx0 = isiz1
            ny0 = isiz2
            nz0 = isiz3
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
            stop

         end if

         if ( ( nx0 .gt. isiz1 ) .or.  &
     &        ( ny0 .gt. isiz2 ) .or.  &
     &        ( nz0 .gt. isiz3 ) ) then

            write (*,2002)
 2002       format (5x,'PROBLEM SIZE IS TOO LARGE - ',  &
     &           /5x,'NX, NY AND NZ SHOULD BE EQUAL TO ',  &
     &           /5x,'ISIZ1, ISIZ2 AND ISIZ3 RESPECTIVELY')
            stop

         end if


         write(*, 1001) nx0, ny0, nz0
         write(*, 1002) itmax
!$       write(*, 1003) omp_get_max_threads()
         write(*, *)


 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &          ' - LU Benchmark', /)
 1001    format(' Size: ', i4, 'x', i4, 'x', i4)
 1002    format(' Iterations:                  ', i5)
 1003    format(' Number of available threads: ', i5)
         


      return
      end


