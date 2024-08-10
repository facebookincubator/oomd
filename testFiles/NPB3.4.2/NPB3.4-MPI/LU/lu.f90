!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                                   L U                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is part of the NAS Parallel Benchmark 3.4 suite.      !
!    It is described in NAS Technical Reports 95-020 and 02-007           !
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
! Authors: S. Weeratunga
!          V. Venkatakrishnan
!          E. Barszcz
!          M. Yarrow
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------
      program applu
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   driver for the performance evaluation of the solver for
!   five coupled parabolic/elliptic partial differential equations.
!
!---------------------------------------------------------------------

      use lu_data
      use mpinpb
      use timing

      implicit none

      character class
      logical verified
      double precision mflops, timer_read
      integer i, ierr
      double precision tsum(t_last+2), t1(t_last+2),  &
     &                 tming(t_last+2), tmaxg(t_last+2)
      character        t_recs(t_last+2)*8

      data t_recs/'total', 'rhs', 'blts', 'buts', '#jacld', '#jacu',  &
     &            'exch', 'lcomm', 'ucomm', 'rcomm',  &
     &            ' totcomp', ' totcomm'/

!---------------------------------------------------------------------
!   initialize communications
!---------------------------------------------------------------------
      call init_comm()
      if (.not. active) goto 999

!---------------------------------------------------------------------
!   read input data
!---------------------------------------------------------------------
      call read_input(class)

      do i = 1, t_last
         call timer_clear(i)
      end do

!---------------------------------------------------------------------
!   set up processor grid
!---------------------------------------------------------------------
      call proc_grid()

!---------------------------------------------------------------------
!   allocate space
!---------------------------------------------------------------------
      call alloc_space()

!---------------------------------------------------------------------
!   determine the neighbors
!---------------------------------------------------------------------
      call neighbors()

!---------------------------------------------------------------------
!   set up sub-domain sizes
!---------------------------------------------------------------------
      call subdomain()

!---------------------------------------------------------------------
!   set up coefficients
!---------------------------------------------------------------------
      call setcoeff()

!---------------------------------------------------------------------
!   set the boundary values for dependent variables
!---------------------------------------------------------------------
      call setbv()

!---------------------------------------------------------------------
!   set the initial values for dependent variables
!---------------------------------------------------------------------
      call setiv()

!---------------------------------------------------------------------
!   compute the forcing term based on prescribed exact solution
!---------------------------------------------------------------------
      call erhs()

!---------------------------------------------------------------------
!   perform one SSOR iteration to touch all data and program pages 
!---------------------------------------------------------------------
      call ssor(1)

!---------------------------------------------------------------------
!   reset the boundary and initial values
!---------------------------------------------------------------------
      call setbv()
      call setiv()

!---------------------------------------------------------------------
!   perform the SSOR iterations
!---------------------------------------------------------------------
      call ssor(itmax)

!---------------------------------------------------------------------
!   compute the solution error
!---------------------------------------------------------------------
      call error()

!---------------------------------------------------------------------
!   compute the surface integral
!---------------------------------------------------------------------
      call pintgr()

!---------------------------------------------------------------------
!   verification test
!---------------------------------------------------------------------
      IF (id.eq.0) THEN
         call verify ( rsdnm, errnm, frc, class, verified )
         mflops = 1.0d-6*dble(itmax)*(1984.77*dble( nx0 )  &
     &        *dble( ny0 )  &
     &        *dble( nz0 )  &
     &        -10923.3*(dble( nx0+ny0+nz0 )/3.)**2  &
     &        +27770.9* dble( nx0+ny0+nz0 )/3.  &
     &        -144010.)  &
     &        / maxtime

         call print_results('LU', class, nx0,  &
     &     ny0, nz0, itmax, no_nodes, total_nodes,  &
     &     maxtime, mflops, '          floating point', verified,  &
     &     npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6,  &
     &     '(none)')

      END IF

      if (.not.timeron) goto 999

      do i = 1, t_last
         t1(i) = timer_read(i)
      end do
      t1(t_rhs) = t1(t_rhs) - t1(t_exch)
      t1(t_last+2) = t1(t_lcomm)+t1(t_ucomm)+t1(t_rcomm)+t1(t_exch)
      t1(t_last+1) = t1(t_total) - t1(t_last+2)

      call MPI_Reduce(t1, tsum,  t_last+2, dp_type, MPI_SUM,  &
     &                0, comm_solve, ierr)
      call MPI_Reduce(t1, tming, t_last+2, dp_type, MPI_MIN,  &
     &                0, comm_solve, ierr)
      call MPI_Reduce(t1, tmaxg, t_last+2, dp_type, MPI_MAX,  &
     &                0, comm_solve, ierr)

      if (id .eq. 0) then
         write(*, 800) no_nodes
         do i = 1, t_last+2
            if (t_recs(i)(1:1) .ne. '#') then
               tsum(i) = tsum(i) / no_nodes
               write(*, 810) i, t_recs(i), tming(i), tmaxg(i), tsum(i)
            endif
         end do
      endif
 800  format(' nprocs =', i6, 11x, 'minimum', 5x, 'maximum',  &
     &       5x, 'average')
 810  format(' timer ', i2, '(', A8, ') :', 3(2x,f10.4))

 999  continue
      call mpi_finalize(ierr)
      end


