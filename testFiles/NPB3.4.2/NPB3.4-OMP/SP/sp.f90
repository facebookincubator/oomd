!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                       O p e n M P     V E R S I O N                     !
!                                                                         !
!                                   S P                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB SP code.              !
!    It is described in NAS Technical Report 99-011.                      !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 3.4. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.4, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (650) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!

!---------------------------------------------------------------------
!
! Authors: R. Van der Wijngaart
!          W. Saphir
!          H. Jin
!---------------------------------------------------------------------

!---------------------------------------------------------------------
       program SP
!---------------------------------------------------------------------

       use sp_data
       implicit none

       include 'blk_par.h'

       integer          i, niter, step, fstatus
       external         timer_read
       double precision mflops, n3, t, tmax, timer_read, trecs(t_last)
       logical          verified
       character        class
       character        t_names(t_last)*8
!$     integer  omp_get_max_threads
!$     external omp_get_max_threads

!---------------------------------------------------------------------
!      Read input file (if it exists), else take
!      defaults from parameters
!---------------------------------------------------------------------
          
       call check_timer_flag( timeron )
       if (timeron) then
         t_names(t_total) = 'total'
         t_names(t_rhsx) = 'rhsx'
         t_names(t_rhsy) = 'rhsy'
         t_names(t_rhsz) = 'rhsz'
         t_names(t_rhs) = 'rhs'
         t_names(t_xsolve) = 'xsolve'
         t_names(t_ysolve) = 'ysolve'
         t_names(t_zsolve) = 'zsolve'
         t_names(t_rdis1) = 'redist1'
         t_names(t_rdis2) = 'redist2'
         t_names(t_tzetar) = 'tzetar'
         t_names(t_ninvr) = 'ninvr'
         t_names(t_pinvr) = 'pinvr'
         t_names(t_txinvr) = 'txinvr'
         t_names(t_add) = 'add'
       endif

       write(*, 1000)
       open (unit=2,file='inputsp.data',status='old', iostat=fstatus)

       if (fstatus .eq. 0) then
         write(*,233) 
 233     format(' Reading from input file inputsp.data')
         read (2,*) niter
         read (2,*) dt
         read (2,*) grid_points(1), grid_points(2), grid_points(3)
         close(2)
       else
         write(*,234) 
         niter = niter_default
         dt    = dt_default
         grid_points(1) = problem_size
         grid_points(2) = problem_size
         grid_points(3) = problem_size
       endif
 234   format(' No input file inputsp.data. Using compiled defaults')

       write(*, 1001) grid_points(1), grid_points(2), grid_points(3)
       write(*, 1002) niter, dt
       if (blkdim .gt. 0) write(*, 1004) blkdim
!$     write(*, 1003) omp_get_max_threads()
       write(*, *)

 1000  format(//, ' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &            ' - SP Benchmark', /)
 1001  format(' Size: ', i4, 'x', i4, 'x', i4)
 1002  format(' Iterations: ', i4, '    dt:  ', f11.7)
 1003  format(' Number of available threads: ', i5)
 1004  format(' Dimension blocking size: ', i5)

       if ( (grid_points(1) .gt. IMAX) .or.  &
     &      (grid_points(2) .gt. JMAX) .or.  &
     &      (grid_points(3) .gt. KMAX) ) then
             print *, (grid_points(i),i=1,3)
             print *,' Problem size too big for compiled array sizes'
             goto 999
       endif
       nx2 = grid_points(1) - 2
       ny2 = grid_points(2) - 2
       nz2 = grid_points(3) - 2

       do i = 1, t_last
          call timer_clear(i)
       end do

       call alloc_space

       call set_constants

       call exact_rhs

       call initialize

!---------------------------------------------------------------------
!      do one time step to touch all code, and reinitialize
!---------------------------------------------------------------------
       call adi
       call initialize

       do i = 1, t_last
          call timer_clear(i)
       end do
       call timer_start(1)

       do  step = 1, niter

          if (mod(step, 20) .eq. 0 .or. step .eq. 1) then
             write(*, 200) step
 200         format(' Time step ', i4)
          endif

          call adi

       end do

       call timer_stop(1)
       tmax = timer_read(1)
       
       call verify(niter, class, verified)

       if( tmax .ne. 0. ) then
          n3 = dble(grid_points(1))*grid_points(2)*grid_points(3)
          t = (grid_points(1)+grid_points(2)+grid_points(3))/3.d0
          mflops = 1.0d-6*dble( niter )*(881.174 * n3  &
     &             -4683.91 * t**2  &
     &             +11484.5 * t  &
     &             -19272.4) / tmax
       else
          mflops = 0.d0
       endif

      call print_results('SP', class, grid_points(1),  &
     &     grid_points(2), grid_points(3), niter,  &
     &     tmax, mflops, '          floating point',  &
     &     verified, npbversion,compiletime, cs1, cs2, cs3, cs4, cs5,  &
     &     cs6, '(none)')

!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
       if (.not.timeron) goto 999

       do i=1, t_last
          trecs(i) = timer_read(i)
       end do
       if (tmax .eq. 0.0) tmax = 1.0

       write(*,800)
 800   format('  SECTION   Time (secs)')

       do i=1, t_last
          write(*,810) t_names(i), trecs(i), trecs(i)*100./tmax
          if (i.eq.t_rhs) then
             t = trecs(t_rhsx) + trecs(t_rhsy) + trecs(t_rhsz)
             write(*,820) 'sub-rhs', t, t*100./tmax
             t = trecs(t_rhs) - t
             write(*,820) 'rest-rhs', t, t*100./tmax
          elseif (i.eq.t_zsolve) then
             t = trecs(t_zsolve) - trecs(t_rdis1) - trecs(t_rdis2)
             write(*,820) 'sub-zsol', t, t*100./tmax
          elseif (i.eq.t_rdis2) then
             t = trecs(t_rdis1) + trecs(t_rdis2)
             write(*,820) 'redist', t, t*100./tmax
          endif
 810      format(2x,a8,':',f9.3,'  (',f6.2,'%)')
 820      format('    --> ',a8,':',f9.3,'  (',f6.2,'%)')
       end do

 999   continue

       end
