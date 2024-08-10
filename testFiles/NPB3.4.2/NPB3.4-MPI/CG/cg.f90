!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                                   C G                                   !
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
! Authors: M. Yarrow
!          C. Kuszmaul
!          R. F. Van der Wijngaart
!          H. Jin
!
!---------------------------------------------------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      program cg
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use, intrinsic :: ieee_arithmetic, only : ieee_is_nan

      use cg_data
      use mpinpb
      use timing

      implicit none

      integer status(MPI_STATUS_SIZE), request, ierr

      integer            i, j, k, it

      double precision   zeta, randlc
      external           randlc
      double precision   rnorm
      double precision   norm_temp1(2), norm_temp2(2)

      double precision   t, tmax, mflops
      external           timer_read
      double precision   timer_read
      character          class
      logical            verified
      double precision   zeta_verify_value, epsilon, err

      double precision tsum(t_last+2), t1(t_last+2),  &
     &                 tming(t_last+2), tmaxg(t_last+2)
      character        t_recs(t_last+2)*8

      data t_recs/'total', 'conjg', 'rcomm', 'ncomm',  &
     &            ' totcomp', ' totcomm'/


!---------------------------------------------------------------------
!  Set up mpi initialization and number of proc testing
!---------------------------------------------------------------------
      call initialize_mpi
      if (.not. active) goto 999

!---------------------------------------------------------------------
!  Set up processor info, such as whether sq num of procs, etc
!---------------------------------------------------------------------
      call setup_proc_info( )

!---------------------------------------------------------------------
!  Allocate space for work arrays
!---------------------------------------------------------------------
      call alloc_space( )


      if( na .eq. 1400 .and.  &
     &    nonzer .eq. 7 .and.  &
     &    niter .eq. 15 .and.  &
     &    shift .eq. 10.d0 ) then
         class = 'S'
         zeta_verify_value = 8.5971775078648d0
      else if( na .eq. 7000 .and.  &
     &         nonzer .eq. 8 .and.  &
     &         niter .eq. 15 .and.  &
     &         shift .eq. 12.d0 ) then
         class = 'W'
         zeta_verify_value = 10.362595087124d0
      else if( na .eq. 14000 .and.  &
     &         nonzer .eq. 11 .and.  &
     &         niter .eq. 15 .and.  &
     &         shift .eq. 20.d0 ) then
         class = 'A'
         zeta_verify_value = 17.130235054029d0
      else if( na .eq. 75000 .and.  &
     &         nonzer .eq. 13 .and.  &
     &         niter .eq. 75 .and.  &
     &         shift .eq. 60.d0 ) then
         class = 'B'
         zeta_verify_value = 22.712745482631d0
      else if( na .eq. 150000 .and.  &
     &         nonzer .eq. 15 .and.  &
     &         niter .eq. 75 .and.  &
     &         shift .eq. 110.d0 ) then
         class = 'C'
         zeta_verify_value = 28.973605592845d0
      else if( na .eq. 1500000 .and.  &
     &         nonzer .eq. 21 .and.  &
     &         niter .eq. 100 .and.  &
     &         shift .eq. 500.d0 ) then
         class = 'D'
         zeta_verify_value = 52.514532105794d0
      else if( na .eq. 9000000 .and.  &
     &         nonzer .eq. 26 .and.  &
     &         niter .eq. 100 .and.  &
     &         shift .eq. 1.5d3 ) then
         class = 'E'
         zeta_verify_value = 77.522164599383d0
      else if( na .eq. 54000000 .and.  &
     &         nonzer .eq. 31 .and.  &
     &         niter .eq. 100 .and.  &
     &         shift .eq. 5.0d3 ) then
         class = 'F'
         zeta_verify_value = 107.3070826433d0
      else
         class = 'U'
      endif

      if( me .eq. root )then
         write( *,1000 ) 
         write( *,1001 ) na, class
         write( *,1002 ) niter
         write( *,1003 ) nonzer
         write( *,1004 ) shift
         write( *,1005 ) total_nodes
         if (total_nodes .ne. nprocs) write (*, 1006) nprocs

 1000 format(//,' NAS Parallel Benchmarks 3.4 -- CG Benchmark', /)
 1001 format(' Size: ', i10, '  (class ', a, ')' )
 1002 format(' Iterations: ', i5 )
 1003 format(' Number of nonzeroes per row: ', i8)
 1004 format(' Eigenvalue shift: ', f9.3)
 1005 format(' Total number of processes: ', i6)
 1006 format(' WARNING: Number of processes is not power of two (',  &
     &       i0, ' active)')
      endif


!---------------------------------------------------------------------
!  Set up partition's submatrix info: firstcol, lastcol, firstrow, lastrow
!---------------------------------------------------------------------
      call setup_submatrix_info( )


      do i = 1, t_last
         call timer_clear(i)
      end do

!---------------------------------------------------------------------
!  Inialize random number generator
!---------------------------------------------------------------------
      tran    = 314159265.0D0
      amult   = 1220703125.0D0
      zeta    = randlc( tran, amult )

!---------------------------------------------------------------------
!  Set up partition's sparse random matrix for given class size
!---------------------------------------------------------------------
      call makea(na, nz, a, colidx, rowstr, nonzer,  &
     &           firstrow, lastrow, firstcol, lastcol,  &
     &           rcond, arow, acol, aelt, v, iv, shift)



!---------------------------------------------------------------------
!  Note: as a result of the above call to makea:
!        values of j used in indexing rowstr go from 1 --> lastrow-firstrow+1
!        values of colidx which are col indexes go from firstcol --> lastcol
!        So:
!        Shift the col index vals from actual (firstcol --> lastcol ) 
!        to local, i.e., (1 --> lastcol-firstcol+1)
!---------------------------------------------------------------------
      do j=1,lastrow-firstrow+1
         do k=rowstr(j),rowstr(j+1)-1
            colidx(k) = colidx(k) - firstcol + 1
         enddo
      enddo

!---------------------------------------------------------------------
!  set starting vector to (1, 1, .... 1)
!---------------------------------------------------------------------
      do i = 1, naa+1
         x(i) = 1.0D0
      enddo

      zeta  = 0.0d0

!---------------------------------------------------------------------
!---->
!  Do one iteration untimed to init all code and data page tables
!---->                    (then reinit, start timing, to niter its)
!---------------------------------------------------------------------
      do it = 1, 1

!---------------------------------------------------------------------
!  The call to the conjugate gradient routine:
!---------------------------------------------------------------------
         call conj_grad ( rnorm )

!---------------------------------------------------------------------
!  zeta = shift + 1/(x.z)
!  So, first: (x.z)
!  Also, find norm of z
!  So, first: (z.z)
!---------------------------------------------------------------------
         norm_temp1(1) = 0.0d0
         norm_temp1(2) = 0.0d0
         do j=1, lastcol-firstcol+1
            norm_temp1(1) = norm_temp1(1) + x(j)*z(j)
            norm_temp1(2) = norm_temp1(2) + z(j)*z(j)
         enddo

         if (timeron) call timer_start(t_ncomm)
         do i = 1, l2npcols
            call mpi_irecv( norm_temp2,  &
     &                      2,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )
            call mpi_send(  norm_temp1,  &
     &                      2,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      ierr )
            call mpi_wait( request, status, ierr )

            norm_temp1(1) = norm_temp1(1) + norm_temp2(1)
            norm_temp1(2) = norm_temp1(2) + norm_temp2(2)
         enddo
         if (timeron) call timer_stop(t_ncomm)

         norm_temp1(2) = 1.0d0 / sqrt( norm_temp1(2) )


!---------------------------------------------------------------------
!  Normalize z to obtain x
!---------------------------------------------------------------------
         do j=1, lastcol-firstcol+1      
            x(j) = norm_temp1(2)*z(j)    
         enddo                           


      enddo                              ! end of do one iteration untimed


!---------------------------------------------------------------------
!  set starting vector to (1, 1, .... 1)
!---------------------------------------------------------------------
!
!  NOTE: a questionable limit on size:  should this be na/num_proc_cols+1 ?
!
      do i = 1, naa+1
         x(i) = 1.0d0
      enddo

      zeta  = 0.0d0

!---------------------------------------------------------------------
!  Synchronize and start timing
!---------------------------------------------------------------------
      do i = 1, t_last
         call timer_clear(i)
      end do
      call mpi_barrier( comm_solve, ierr )

      call timer_clear( 1 )
      call timer_start( 1 )

!---------------------------------------------------------------------
!---->
!  Main Iteration for inverse power method
!---->
!---------------------------------------------------------------------
      do it = 1, niter

!---------------------------------------------------------------------
!  The call to the conjugate gradient routine:
!---------------------------------------------------------------------
         call conj_grad ( rnorm )


!---------------------------------------------------------------------
!  zeta = shift + 1/(x.z)
!  So, first: (x.z)
!  Also, find norm of z
!  So, first: (z.z)
!---------------------------------------------------------------------
         norm_temp1(1) = 0.0d0
         norm_temp1(2) = 0.0d0
         do j=1, lastcol-firstcol+1
            norm_temp1(1) = norm_temp1(1) + x(j)*z(j)
            norm_temp1(2) = norm_temp1(2) + z(j)*z(j)
         enddo

         if (timeron) call timer_start(t_ncomm)
         do i = 1, l2npcols
            call mpi_irecv( norm_temp2,  &
     &                      2,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )
            call mpi_send(  norm_temp1,  &
     &                      2,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      ierr )
            call mpi_wait( request, status, ierr )

            norm_temp1(1) = norm_temp1(1) + norm_temp2(1)
            norm_temp1(2) = norm_temp1(2) + norm_temp2(2)
         enddo
         if (timeron) call timer_stop(t_ncomm)

         norm_temp1(2) = 1.0d0 / sqrt( norm_temp1(2) )


         if( me .eq. root )then
            zeta = shift + 1.0d0 / norm_temp1(1)
            if( it .eq. 1 ) write( *,9000 )
            write( *,9001 ) it, rnorm, zeta
         endif
 9000 format( /,'   iteration           ||r||                 zeta' )
 9001 format( 4x, i5, 6x, e21.14, f20.13 )

!---------------------------------------------------------------------
!  Normalize z to obtain x
!---------------------------------------------------------------------
         do j=1, lastcol-firstcol+1      
            x(j) = norm_temp1(2)*z(j)    
         enddo                           


      enddo                              ! end of main iter inv pow meth

      call timer_stop( 1 )

!---------------------------------------------------------------------
!  End of timed section
!---------------------------------------------------------------------

      t = timer_read( 1 )

      call mpi_reduce( t,  &
     &                 tmax,  &
     &                 1,  &
     &                 dp_type,  &
     &                 MPI_MAX,  &
     &                 root,  &
     &                 comm_solve,  &
     &                 ierr )

      if( me .eq. root )then
         write(*,100)
 100     format(' Benchmark completed ')

         epsilon = 1.d-10
         if (class .ne. 'U') then

            err = abs( zeta - zeta_verify_value )/zeta_verify_value
            if( (.not.ieee_is_nan(err)) .and. (err .le. epsilon) ) then
               verified = .TRUE.
               write(*, 200)
               write(*, 201) zeta
               write(*, 202) err
 200           format(' VERIFICATION SUCCESSFUL ')
 201           format(' Zeta is    ', E20.13)
 202           format(' Error is   ', E20.13)
            else
               verified = .FALSE.
               write(*, 300) 
               write(*, 301) zeta
               write(*, 302) zeta_verify_value
 300           format(' VERIFICATION FAILED')
 301           format(' Zeta                ', E20.13)
 302           format(' The correct zeta is ', E20.13)
            endif
         else
            verified = .FALSE.
            write (*, 400)
            write (*, 401)
            write (*, 201) zeta
 400        format(' Problem size unknown')
 401        format(' NO VERIFICATION PERFORMED')
         endif


         if( tmax .ne. 0. ) then
            mflops = 1.0d-6 * 2*niter*dble( na )  &
     &                  * ( 3.+nonzer*dble(nonzer+1)  &
     &                    + 25.*(5.+nonzer*dble(nonzer+1))  &
     &                    + 3. ) / tmax
         else
            mflops = 0.d0
         endif

         call print_results('CG', class, na, 0, 0,  &
     &                      niter, nprocs, total_nodes, tmax,  &
     &                      mflops, '          floating point',  &
     &                      verified, npbversion, compiletime,  &
     &                      cs1, cs2, cs3, cs4, cs5, cs6, cs7)


      endif


      if (.not.timeron) goto 999

      do i = 1, t_last
         t1(i) = timer_read(i)
      end do
      t1(t_conjg) = t1(t_conjg) - t1(t_rcomm)
      t1(t_last+2) = t1(t_rcomm) + t1(t_ncomm)
      t1(t_last+1) = t1(t_total) - t1(t_last+2)

      call MPI_Reduce(t1, tsum,  t_last+2, dp_type, MPI_SUM,  &
     &                0, comm_solve, ierr)
      call MPI_Reduce(t1, tming, t_last+2, dp_type, MPI_MIN,  &
     &                0, comm_solve, ierr)
      call MPI_Reduce(t1, tmaxg, t_last+2, dp_type, MPI_MAX,  &
     &                0, comm_solve, ierr)

      if (me .eq. 0) then
         write(*, 800) nprocs
         do i = 1, t_last+2
            tsum(i) = tsum(i) / nprocs
            write(*, 810) i, t_recs(i), tming(i), tmaxg(i), tsum(i)
         end do
      endif
 800  format(' nprocs =', i6, 11x, 'minimum', 5x, 'maximum',  &
     &       5x, 'average')
 810  format(' timer ', i2, '(', A8, ') :', 3(2x,f10.4))

 999  continue
      call mpi_finalize(ierr)



      end                              ! end main





!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine initialize_mpi
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use cg_data
      use mpinpb
      use timing

      implicit none

      integer   ierr


      call mpi_init( ierr )

!---------------------------------------------------------------------
!     get a process grid that requires a pwr-2 number of procs.
!     excess ranks are marked as inactive.
!---------------------------------------------------------------------
      call get_active_nprocs(3, num_proc_cols, num_proc_rows, nprocs,  &
     &                       total_nodes, me, comm_solve, active)

      if (.not. active) return

      if (.not. convertdouble) then
         dp_type = MPI_DOUBLE_PRECISION
      else
         dp_type = MPI_REAL
      endif

      root = 0

      if (me .eq. root) then
         call check_timer_flag( timeron )
      endif

      call mpi_bcast(timeron, 1, MPI_LOGICAL, 0, comm_solve, ierr)

      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine setup_proc_info( )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use cg_data
      use mpinpb

      implicit none

      integer   i, ierr


!---------------------------------------------------------------------
!  set up dimension parameters after partition
!  num_proc_rows & num_proc_cols are set by get_active_nprocs
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  num_procs must be a power of 2, and num_procs=num_proc_cols*num_proc_rows.
!  num_proc_cols and num_proc_cols are to be found in npbparams.h.
!  When num_procs is not square, then num_proc_cols must be = 2*num_proc_rows.
!---------------------------------------------------------------------
      num_procs = num_proc_cols * num_proc_rows

!---------------------------------------------------------------------
!  num_procs must be a power of 2, and num_procs=num_proc_cols*num_proc_rows
!  When num_procs is not square, then num_proc_cols = 2*num_proc_rows
!---------------------------------------------------------------------
!  First, number of procs must be power of two. 
!---------------------------------------------------------------------
      if( nprocs .ne. num_procs )then
          if( me .eq. root ) write( *,9000 ) nprocs, num_procs
 9000     format( /,'ERROR: Number of processes (',  &
     &             i0, ') is not a power of two (', i0, '?)'/ )
          call mpi_abort(mpi_comm_world, mpi_err_other, ierr)
          stop
      endif

      
      npcols = num_proc_cols
      nprows = num_proc_rows


      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine setup_submatrix_info( )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use cg_data
      use mpinpb

      implicit none

      integer   col_size, row_size
      integer   i, j
      integer   div_factor


      proc_row = me / npcols
      proc_col = me - proc_row*npcols


!---------------------------------------------------------------------
!  If na evenly divisible by npcols, then it is evenly divisible 
!  by nprows 
!---------------------------------------------------------------------

      if( na/npcols*npcols .eq. na )then
          col_size = na/npcols
          firstcol = proc_col*col_size + 1
          lastcol  = firstcol - 1 + col_size
          row_size = na/nprows
          firstrow = proc_row*row_size + 1
          lastrow  = firstrow - 1 + row_size
!---------------------------------------------------------------------
!  If na not evenly divisible by npcols, then first subdivide for nprows
!  and then, if npcols not equal to nprows (i.e., not a sq number of procs), 
!  get col subdivisions by dividing by 2 each row subdivision.
!---------------------------------------------------------------------
      else
          if( proc_row .lt. na - na/nprows*nprows)then
              row_size = na/nprows+ 1
              firstrow = proc_row*row_size + 1
              lastrow  = firstrow - 1 + row_size
          else
              row_size = na/nprows
              firstrow = (na - na/nprows*nprows)*(row_size+1)  &
     &                 + (proc_row-(na-na/nprows*nprows))  &
     &                     *row_size + 1
              lastrow  = firstrow - 1 + row_size
          endif
          if( npcols .eq. nprows )then
              if( proc_col .lt. na - na/npcols*npcols )then
                  col_size = na/npcols+ 1
                  firstcol = proc_col*col_size + 1
                  lastcol  = firstcol - 1 + col_size
              else
                  col_size = na/npcols
                  firstcol = (na - na/npcols*npcols)*(col_size+1)  &
     &                     + (proc_col-(na-na/npcols*npcols))  &
     &                         *col_size + 1
                  lastcol  = firstcol - 1 + col_size
              endif
          else
              if( (proc_col/2) .lt.  &
     &                           na - na/(npcols/2)*(npcols/2) )then
                  col_size = na/(npcols/2) + 1
                  firstcol = (proc_col/2)*col_size + 1
                  lastcol  = firstcol - 1 + col_size
              else
                  col_size = na/(npcols/2)
                  firstcol = (na - na/(npcols/2)*(npcols/2))  &
     &                                                 *(col_size+1)  &
     &               + ((proc_col/2)-(na-na/(npcols/2)*(npcols/2)))  &
     &                         *col_size + 1
                  lastcol  = firstcol - 1 + col_size
              endif
!C               write( *,* ) col_size,firstcol,lastcol
              if( mod( me,2 ) .eq. 0 )then
                  lastcol  = firstcol - 1 + (col_size-1)/2 + 1
              else
                  firstcol = firstcol + (col_size-1)/2 + 1
                  lastcol  = firstcol - 1 + col_size/2
!C                   write( *,* ) firstcol,lastcol
              endif
          endif
      endif



      if( npcols .eq. nprows )then
          send_start = 1
          send_len   = lastrow - firstrow + 1
      else
          if( mod( me,2 ) .eq. 0 )then
              send_start = 1
              send_len   = (1 + lastrow-firstrow+1)/2
          else
              send_start = (1 + lastrow-firstrow+1)/2 + 1
              send_len   = (lastrow-firstrow+1)/2
          endif
      endif
          



!---------------------------------------------------------------------
!  Transpose exchange processor
!---------------------------------------------------------------------

      if( npcols .eq. nprows )then
          exch_proc = mod( me,nprows )*nprows + me/nprows
      else
          exch_proc = 2*(mod( me/2,nprows )*nprows + me/2/nprows)  &
     &                 + mod( me,2 )
      endif



      i = npcols / 2
      l2npcols = 0
      do while( i .gt. 0 )
         l2npcols = l2npcols + 1
         i = i / 2
      enddo


!---------------------------------------------------------------------
!  Set up the reduce phase schedules...
!---------------------------------------------------------------------

      div_factor = npcols
      do i = 1, l2npcols

         j = mod( proc_col+div_factor/2, div_factor )  &
     &     + proc_col / div_factor * div_factor
         reduce_exch_proc(i) = proc_row*npcols + j

         div_factor = div_factor / 2

      enddo


      do i = l2npcols, 1, -1

            if( nprows .eq. npcols )then
               reduce_send_starts(i)  = send_start
               reduce_send_lengths(i) = send_len
               reduce_recv_lengths(i) = lastrow - firstrow + 1
            else
               reduce_recv_lengths(i) = send_len
               if( i .eq. l2npcols )then
                  reduce_send_lengths(i) = lastrow-firstrow+1 - send_len
                  if( me/2*2 .eq. me )then
                     reduce_send_starts(i) = send_start + send_len
                  else
                     reduce_send_starts(i) = 1
                  endif
               else
                  reduce_send_lengths(i) = send_len
                  reduce_send_starts(i)  = send_start
               endif
            endif
            reduce_recv_starts(i) = send_start

      enddo


      exch_recv_length = lastcol - firstcol + 1


      return
      end




!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine conj_grad ( rnorm )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  Floaging point arrays here are named as in NPB1 spec discussion of 
!  CG algorithm
!---------------------------------------------------------------------
 
      use cg_data
      use mpinpb
      use timing

      implicit none

      double precision rnorm

      integer status(MPI_STATUS_SIZE ), request

      integer   i, j, k, ierr
      integer   cgit, cgitmax

      double precision d, sum, rho, rho0, alpha, beta

      external         timer_read
      double precision timer_read

      data      cgitmax / 25 /


      if (timeron) call timer_start(t_conjg)
!---------------------------------------------------------------------
!  Initialize the CG algorithm:
!---------------------------------------------------------------------
      do j=1,naa+1
         q(j) = 0.0d0
         z(j) = 0.0d0
         r(j) = x(j)
         p(j) = r(j)
         w(j) = 0.0d0                 
      enddo


!---------------------------------------------------------------------
!  rho = r.r
!  Now, obtain the norm of r: First, sum squares of r elements locally...
!---------------------------------------------------------------------
      sum = 0.0d0
      do j=1, lastcol-firstcol+1
         sum = sum + r(j)*r(j)
      enddo

!---------------------------------------------------------------------
!  Exchange and sum with procs identified in reduce_exch_proc
!  (This is equivalent to mpi_allreduce.)
!  Sum the partial sums of rho, leaving rho on all processors
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_rcomm)
      do i = 1, l2npcols
         call mpi_irecv( rho,  &
     &                   1,  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   request,  &
     &                   ierr )
         call mpi_send(  sum,  &
     &                   1,  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   ierr )
         call mpi_wait( request, status, ierr )

         sum = sum + rho
      enddo
      if (timeron) call timer_stop(t_rcomm)
      rho = sum



!---------------------------------------------------------------------
!---->
!  The conj grad iteration loop
!---->
!---------------------------------------------------------------------
      do cgit = 1, cgitmax


!---------------------------------------------------------------------
!  q = A.p
!  The partition submatrix-vector multiply: use workspace w
!---------------------------------------------------------------------
         do j=1,lastrow-firstrow+1
            sum = 0.d0
            do k=rowstr(j),rowstr(j+1)-1
               sum = sum + a(k)*p(colidx(k))
            enddo
            w(j) = sum
         enddo

!---------------------------------------------------------------------
!  Sum the partition submatrix-vec A.p's across rows
!  Exchange and sum piece of w with procs identified in reduce_exch_proc
!---------------------------------------------------------------------
         if (timeron) call timer_start(t_rcomm)
         do i = l2npcols, 1, -1
            call mpi_irecv( q(reduce_recv_starts(i)),  &
     &                      reduce_recv_lengths(i),  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )
            call mpi_send(  w(reduce_send_starts(i)),  &
     &                      reduce_send_lengths(i),  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      ierr )
            call mpi_wait( request, status, ierr )
            do j=send_start,send_start + reduce_recv_lengths(i) - 1
               w(j) = w(j) + q(j)
            enddo
         enddo
         if (timeron) call timer_stop(t_rcomm)


!---------------------------------------------------------------------
!  Exchange piece of q with transpose processor:
!---------------------------------------------------------------------
         if( l2npcols .ne. 0 )then
            if (timeron) call timer_start(t_rcomm)
            call mpi_irecv( q,               &
     &                      exch_recv_length,  &
     &                      dp_type,  &
     &                      exch_proc,  &
     &                      1,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )

            call mpi_send(  w(send_start),   &
     &                      send_len,  &
     &                      dp_type,  &
     &                      exch_proc,  &
     &                      1,  &
     &                      comm_solve,  &
     &                      ierr )
            call mpi_wait( request, status, ierr )
            if (timeron) call timer_stop(t_rcomm)
         else
            do j=1,exch_recv_length
               q(j) = w(j)
            enddo
         endif


!---------------------------------------------------------------------
!  Clear w for reuse...
!---------------------------------------------------------------------
         do j=1, max( lastrow-firstrow+1, lastcol-firstcol+1 )
            w(j) = 0.0d0
         enddo
         

!---------------------------------------------------------------------
!  Obtain p.q
!---------------------------------------------------------------------
         sum = 0.0d0
         do j=1, lastcol-firstcol+1
            sum = sum + p(j)*q(j)
         enddo

!---------------------------------------------------------------------
!  Obtain d with a sum-reduce
!---------------------------------------------------------------------
         if (timeron) call timer_start(t_rcomm)
         do i = 1, l2npcols
            call mpi_irecv( d,  &
     &                      1,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )
            call mpi_send(  sum,  &
     &                      1,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      ierr )

            call mpi_wait( request, status, ierr )

            sum = sum + d
         enddo
         if (timeron) call timer_stop(t_rcomm)
         d = sum


!---------------------------------------------------------------------
!  Obtain alpha = rho / (p.q)
!---------------------------------------------------------------------
         alpha = rho / d

!---------------------------------------------------------------------
!  Save a temporary of rho
!---------------------------------------------------------------------
         rho0 = rho

!---------------------------------------------------------------------
!  Obtain z = z + alpha*p
!  and    r = r - alpha*q
!---------------------------------------------------------------------
         do j=1, lastcol-firstcol+1
            z(j) = z(j) + alpha*p(j)
            r(j) = r(j) - alpha*q(j)
         enddo
            
!---------------------------------------------------------------------
!  rho = r.r
!  Now, obtain the norm of r: First, sum squares of r elements locally...
!---------------------------------------------------------------------
         sum = 0.0d0
         do j=1, lastcol-firstcol+1
            sum = sum + r(j)*r(j)
         enddo

!---------------------------------------------------------------------
!  Obtain rho with a sum-reduce
!---------------------------------------------------------------------
         if (timeron) call timer_start(t_rcomm)
         do i = 1, l2npcols
            call mpi_irecv( rho,  &
     &                      1,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      request,  &
     &                      ierr )
            call mpi_send(  sum,  &
     &                      1,  &
     &                      dp_type,  &
     &                      reduce_exch_proc(i),  &
     &                      i,  &
     &                      comm_solve,  &
     &                      ierr )
            call mpi_wait( request, status, ierr )

            sum = sum + rho
         enddo
         if (timeron) call timer_stop(t_rcomm)
         rho = sum

!---------------------------------------------------------------------
!  Obtain beta:
!---------------------------------------------------------------------
         beta = rho / rho0

!---------------------------------------------------------------------
!  p = r + beta*p
!---------------------------------------------------------------------
         do j=1, lastcol-firstcol+1
            p(j) = r(j) + beta*p(j)
         enddo



      enddo                             ! end of do cgit=1,cgitmax



!---------------------------------------------------------------------
!  Compute residual norm explicitly:  ||r|| = ||x - A.z||
!  First, form A.z
!  The partition submatrix-vector multiply
!---------------------------------------------------------------------
      do j=1,lastrow-firstrow+1
         sum = 0.d0
         do k=rowstr(j),rowstr(j+1)-1
            sum = sum + a(k)*z(colidx(k))
         enddo
         w(j) = sum
      enddo



!---------------------------------------------------------------------
!  Sum the partition submatrix-vec A.z's across rows
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_rcomm)
      do i = l2npcols, 1, -1
         call mpi_irecv( r(reduce_recv_starts(i)),  &
     &                   reduce_recv_lengths(i),  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   request,  &
     &                   ierr )
         call mpi_send(  w(reduce_send_starts(i)),  &
     &                   reduce_send_lengths(i),  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   ierr )
         call mpi_wait( request, status, ierr )

         do j=send_start,send_start + reduce_recv_lengths(i) - 1
            w(j) = w(j) + r(j)
         enddo
      enddo
      if (timeron) call timer_stop(t_rcomm)
      

!---------------------------------------------------------------------
!  Exchange piece of q with transpose processor:
!---------------------------------------------------------------------
      if( l2npcols .ne. 0 )then
         if (timeron) call timer_start(t_rcomm)
         call mpi_irecv( r,               &
     &                   exch_recv_length,  &
     &                   dp_type,  &
     &                   exch_proc,  &
     &                   1,  &
     &                   comm_solve,  &
     &                   request,  &
     &                   ierr )
   
         call mpi_send(  w(send_start),   &
     &                   send_len,  &
     &                   dp_type,  &
     &                   exch_proc,  &
     &                   1,  &
     &                   comm_solve,  &
     &                   ierr )
         call mpi_wait( request, status, ierr )
         if (timeron) call timer_stop(t_rcomm)
      else
         do j=1,exch_recv_length
            r(j) = w(j)
         enddo
      endif


!---------------------------------------------------------------------
!  At this point, r contains A.z
!---------------------------------------------------------------------
         sum = 0.0d0
         do j=1, lastcol-firstcol+1
            d   = x(j) - r(j)         
            sum = sum + d*d
         enddo
         
!---------------------------------------------------------------------
!  Obtain d with a sum-reduce
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_rcomm)
      do i = 1, l2npcols
         call mpi_irecv( d,  &
     &                   1,  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   request,  &
     &                   ierr )
         call mpi_send(  sum,  &
     &                   1,  &
     &                   dp_type,  &
     &                   reduce_exch_proc(i),  &
     &                   i,  &
     &                   comm_solve,  &
     &                   ierr )
         call mpi_wait( request, status, ierr )

         sum = sum + d
      enddo
      if (timeron) call timer_stop(t_rcomm)
      d = sum


      if( me .eq. root ) rnorm = sqrt( d )

      if (timeron) call timer_stop(t_conjg)


      return
      end                               ! end of routine conj_grad



!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine makea( n, nz, a, colidx, rowstr, nonzer,  &
     &                  firstrow, lastrow, firstcol, lastcol,  &
     &                  rcond, arow, acol, aelt, v, iv, shift )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit            none
      integer             n, nz, nonzer
      integer             firstrow, lastrow, firstcol, lastcol
      integer             colidx(nz), rowstr(n+1)
      integer             iv(2*n+1), arow(nz), acol(nz)
      double precision    v(n+1), aelt(nz)
      double precision    rcond, a(nz), shift

!---------------------------------------------------------------------
!       generate the test problem for benchmark 6
!       makea generates a sparse matrix with a
!       prescribed sparsity distribution
!
!       parameter    type        usage
!
!       input
!
!       n            i           number of cols/rows of matrix
!       nz           i           nonzeros as declared array size
!       rcond        r*8         condition number
!       shift        r*8         main diagonal shift
!
!       output
!
!       a            r*8         array for nonzeros
!       colidx       i           col indices
!       rowstr       i           row pointers
!
!       workspace
!
!       iv, arow, acol i
!       v, aelt        r*8
!---------------------------------------------------------------------

      integer i, nnza, iouter, ivelt, ivelt1, irow, nzv, jcol

!---------------------------------------------------------------------
!      nonzer is approximately  (int(sqrt(nnza /n)));
!---------------------------------------------------------------------

      double precision  size, ratio, scale
      external          sparse, sprnvc, vecset

      size = 1.0D0
      ratio = rcond ** (1.0D0 / dfloat(n))
      nnza = 0

!---------------------------------------------------------------------
!  Initialize iv(n+1 .. 2n) to zero.
!  Used by sprnvc to mark nonzero positions
!---------------------------------------------------------------------

      do i = 1, n
           iv(n+i) = 0
      enddo
      do iouter = 1, n
         nzv = nonzer
         call sprnvc( n, nzv, v, colidx, iv(1), iv(n+1) )
         call vecset( n, v, colidx, nzv, iouter, .5D0 )
         do ivelt = 1, nzv
              jcol = colidx(ivelt)
              if (jcol.ge.firstcol .and. jcol.le.lastcol) then
                 scale = size * v(ivelt)
                 do ivelt1 = 1, nzv
                    irow = colidx(ivelt1)
                    if (irow.ge.firstrow .and. irow.le.lastrow) then
                       nnza = nnza + 1
                       if (nnza .gt. nz) goto 9999
                       acol(nnza) = jcol
                       arow(nnza) = irow
                       aelt(nnza) = v(ivelt1) * scale
                    endif
                 enddo
              endif
         enddo
         size = size * ratio
      enddo


!---------------------------------------------------------------------
!       ... add the identity * rcond to the generated matrix to bound
!           the smallest eigenvalue from below by rcond
!---------------------------------------------------------------------
        do i = firstrow, lastrow
           if (i.ge.firstcol .and. i.le.lastcol) then
              iouter = n + i
              nnza = nnza + 1
              if (nnza .gt. nz) goto 9999
              acol(nnza) = i
              arow(nnza) = i
              aelt(nnza) = rcond - shift
           endif
        enddo


!---------------------------------------------------------------------
!       ... make the sparse matrix from list of elements with duplicates
!           (v and iv are used as  workspace)
!---------------------------------------------------------------------
      call sparse( a, colidx, rowstr, n, arow, acol, aelt,  &
     &             firstrow, lastrow,  &
     &             v, iv(1), iv(n+1), nnza )
      return

 9999 continue
      write(*,*) 'Space for matrix elements exceeded in makea'
      write(*,*) 'nnza, nzmax = ',nnza, nz
      write(*,*) ' iouter = ',iouter

      stop
      end
!-------end   of makea------------------------------

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine sparse( a, colidx, rowstr, n, arow, acol, aelt,  &
     &                   firstrow, lastrow,  &
     &                   x, mark, nzloc, nnza )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit           none
      integer            colidx(*), rowstr(*)
      integer            firstrow, lastrow
      integer            n, arow(*), acol(*), nnza
      double precision   a(*), aelt(*)

!---------------------------------------------------------------------
!       rows range from firstrow to lastrow
!       the rowstr pointers are defined for nrows = lastrow-firstrow+1 values
!---------------------------------------------------------------------
      integer            nzloc(n), nrows
      double precision   x(n)
      integer            mark(n)

!---------------------------------------------------
!       generate a sparse matrix from a list of
!       [col, row, element] tri
!---------------------------------------------------

      integer            i, j, jajp1, nza, k, nzrow
      double precision   xi

!---------------------------------------------------------------------
!    how many rows of result
!---------------------------------------------------------------------
      nrows = lastrow - firstrow + 1

!---------------------------------------------------------------------
!     ...count the number of triples in each row
!---------------------------------------------------------------------
      do j = 1, n
         rowstr(j) = 0
         mark(j) = 0
      enddo
      rowstr(n+1) = 0

      do nza = 1, nnza
         j = (arow(nza) - firstrow + 1) + 1
         rowstr(j) = rowstr(j) + 1
      enddo

      rowstr(1) = 1
      do j = 2, nrows+1
         rowstr(j) = rowstr(j) + rowstr(j-1)
      enddo


!---------------------------------------------------------------------
!     ... rowstr(j) now is the location of the first nonzero
!           of row j of a
!---------------------------------------------------------------------


!---------------------------------------------------------------------
!     ... do a bucket sort of the triples on the row index
!---------------------------------------------------------------------
      do nza = 1, nnza
         j = arow(nza) - firstrow + 1
         k = rowstr(j)
         a(k) = aelt(nza)
         colidx(k) = acol(nza)
         rowstr(j) = rowstr(j) + 1
      enddo


!---------------------------------------------------------------------
!       ... rowstr(j) now points to the first element of row j+1
!---------------------------------------------------------------------
      do j = nrows, 1, -1
          rowstr(j+1) = rowstr(j)
      enddo
      rowstr(1) = 1


!---------------------------------------------------------------------
!       ... generate the actual output rows by adding elements
!---------------------------------------------------------------------
      nza = 0
      do i = 1, n
          x(i)    = 0.0
          mark(i) = 0
      enddo

      jajp1 = rowstr(1)
      do j = 1, nrows
         nzrow = 0

!---------------------------------------------------------------------
!          ...loop over the jth row of a
!---------------------------------------------------------------------
         do k = jajp1 , rowstr(j+1)-1
            i = colidx(k)
            x(i) = x(i) + a(k)
            if ( (mark(i) .eq. 0) .and. (x(i) .ne. 0.D0)) then
             mark(i) = 1
             nzrow = nzrow + 1
             nzloc(nzrow) = i
            endif
         enddo

!---------------------------------------------------------------------
!          ... extract the nonzeros of this row
!---------------------------------------------------------------------
         do k = 1, nzrow
            i = nzloc(k)
            mark(i) = 0
            xi = x(i)
            x(i) = 0.D0
            if (xi .ne. 0.D0) then
             nza = nza + 1
             a(nza) = xi
             colidx(nza) = i
            endif
         enddo
         jajp1 = rowstr(j+1)
         rowstr(j+1) = nza + rowstr(1)
      enddo
!C       write (*, 11000) nza
      return
11000   format ( //,'final nonzero count in sparse ',  &
     &            /,'number of nonzeros       = ', i16 )
      end
!-------end   of sparse-----------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine sprnvc( n, nz, v, iv, nzloc, mark )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use cg_data, only : amult, tran
      implicit           none

      double precision   v(*)
      integer            n, nz, iv(*), nzloc(n), nn1
      integer            mark(n)


!---------------------------------------------------------------------
!       generate a sparse n-vector (v, iv)
!       having nzv nonzeros
!
!       mark(i) is set to 1 if position i is nonzero.
!       mark is all zero on entry and is reset to all zero before exit
!       this corrects a performance bug found by John G. Lewis, caused by
!       reinitialization of mark on every one of the n calls to sprnvc
!---------------------------------------------------------------------

        integer            nzrow, nzv, ii, i, icnvrt

        external           randlc, icnvrt
        double precision   randlc, vecelt, vecloc


        nzv = 0
        nzrow = 0
        nn1 = 1
 50     continue
          nn1 = 2 * nn1
          if (nn1 .lt. n) goto 50

!---------------------------------------------------------------------
!    nn1 is the smallest power of two not less than n
!---------------------------------------------------------------------

100     continue
        if (nzv .ge. nz) goto 110
         vecelt = randlc( tran, amult )

!---------------------------------------------------------------------
!   generate an integer between 1 and n in a portable manner
!---------------------------------------------------------------------
         vecloc = randlc(tran, amult)
         i = icnvrt(vecloc, nn1) + 1
         if (i .gt. n) goto 100

!---------------------------------------------------------------------
!  was this integer generated already?
!---------------------------------------------------------------------
         if (mark(i) .eq. 0) then
            mark(i) = 1
            nzrow = nzrow + 1
            nzloc(nzrow) = i
            nzv = nzv + 1
            v(nzv) = vecelt
            iv(nzv) = i
         endif
         goto 100
110      continue
      do ii = 1, nzrow
         i = nzloc(ii)
         mark(i) = 0
      enddo
      return
      end
!-------end   of sprnvc-----------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      function icnvrt(x, ipwr2)
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit           none
      double precision   x
      integer            ipwr2, icnvrt

!---------------------------------------------------------------------
!    scale a double precision number x in (0,1) by a power of 2 and chop it
!---------------------------------------------------------------------
      icnvrt = int(ipwr2 * x)

      return
      end
!-------end   of icnvrt-----------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine vecset(n, v, iv, nzv, i, val)
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit           none
      integer            n, iv(*), nzv, i, k
      double precision   v(*), val

!---------------------------------------------------------------------
!       set ith element of sparse vector (v, iv) with
!       nzv nonzeros to val
!---------------------------------------------------------------------

      logical set

      set = .false.
      do k = 1, nzv
         if (iv(k) .eq. i) then
            v(k) = val
            set  = .true.
         endif
      enddo
      if (.not. set) then
         nzv     = nzv + 1
         v(nzv)  = val
         iv(nzv) = i
      endif
      return
      end
!-------end   of vecset-----------------------------

