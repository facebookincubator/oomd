!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                       O p e n M P     V E R S I O N                     !
!                                                                         !
!                                   C G                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB CG code.              !
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
! Authors: M. Yarrow
!          C. Kuszmaul
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

      implicit none


      integer            i, j, it
      integer(kz)        k

      double precision   zeta, randlc
      external           randlc
      double precision   rnorm
      double precision   norm_temp1,norm_temp2,norm_temp3

      double precision   t, mflops, tmax
      character          class
      logical            verified
      double precision   zeta_verify_value, epsilon, err

      character t_names(t_last)*8
!$    integer   omp_get_max_threads
!$    external  omp_get_max_threads

      do i = 1, T_last
         call timer_clear( i )
      end do

      call check_timer_flag( timeron )
      if (timeron) then
         t_names(t_init) = 'init'
         t_names(t_bench) = 'benchmk'
         t_names(t_conj_grad) = 'conjgd'
      endif

      call timer_start( T_init )

      firstrow = 1
      lastrow  = na
      firstcol = 1
      lastcol  = na


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

      write( *,1000 ) 
      write( *,1001 ) na
      write( *,1002 ) niter
!$    write( *,1003 ) omp_get_max_threads()
      write( *,* )
 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &          ' - CG Benchmark', /)
 1001 format(' Size: ', i11 )
 1002 format(' Iterations:                  ', i5 )
 1003 format(' Number of available threads: ', i5)

      naa = na
      nzz = nz

      call alloc_space

!---------------------------------------------------------------------
!  Inialize random number generator
!---------------------------------------------------------------------
!$omp parallel default(shared) private(i,j,k,zeta)
      tran    = 314159265.0D0
      amult   = 1220703125.0D0
      zeta    = randlc( tran, amult )

!---------------------------------------------------------------------
!  
!---------------------------------------------------------------------
      call makea(naa, nzz, a, colidx, rowstr,  &
     &           firstrow, lastrow, firstcol, lastcol,  &
     &           arow, acol, aelt, v, iv)
!$omp barrier


!---------------------------------------------------------------------
!  Note: as a result of the above call to makea:
!        values of j used in indexing rowstr go from 1 --> lastrow-firstrow+1
!        values of colidx which are col indexes go from firstcol --> lastcol
!        So:
!        Shift the col index vals from actual (firstcol --> lastcol ) 
!        to local, i.e., (1 --> lastcol-firstcol+1)
!---------------------------------------------------------------------
!$omp do
      do j=1,lastrow-firstrow+1
         do k=rowstr(j),rowstr(j+1)-1
            colidx(k) = colidx(k) - firstcol + 1
         enddo
      enddo
!$omp end do nowait

!---------------------------------------------------------------------
!  set starting vector to (1, 1, .... 1)
!---------------------------------------------------------------------
!$omp do
      do i = 1, na+1
         x(i) = 1.0D0
      enddo
!$omp end do nowait
!$omp do
      do j=1, lastcol-firstcol+1
         q(j) = 0.0d0
         z(j) = 0.0d0
         r(j) = 0.0d0
         p(j) = 0.0d0
      enddo
!$omp end do nowait
!$omp end parallel

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
         norm_temp1 = 0.0d0
         norm_temp2 = 0.0d0
!$omp parallel default(shared) private(j,norm_temp3)
!$omp do reduction(+:norm_temp1,norm_temp2)
         do j=1, lastcol-firstcol+1
            norm_temp1 = norm_temp1 + x(j)*z(j)
            norm_temp2 = norm_temp2 + z(j)*z(j)
         enddo
!$omp end do

         norm_temp3 = 1.0d0 / sqrt( norm_temp2 )


!---------------------------------------------------------------------
!  Normalize z to obtain x
!---------------------------------------------------------------------
!$omp do
         do j=1, lastcol-firstcol+1      
            x(j) = norm_temp3*z(j)    
         enddo                           
!$omp end do nowait
!$omp end parallel


      enddo                              ! end of do one iteration untimed


!---------------------------------------------------------------------
!  set starting vector to (1, 1, .... 1)
!---------------------------------------------------------------------
!
!  
!
!$omp parallel do default(shared) private(i)
      do i = 1, na+1
         x(i) = 1.0D0
      enddo
!$omp end parallel do

      zeta  = 0.0d0

      call timer_stop( T_init )

      write (*, 2000) timer_read(T_init)
 2000 format(' Initialization time = ',f15.3,' seconds')

      call timer_start( T_bench )

!---------------------------------------------------------------------
!---->
!  Main Iteration for inverse power method
!---->
!---------------------------------------------------------------------
      do it = 1, niter

!---------------------------------------------------------------------
!  The call to the conjugate gradient routine:
!---------------------------------------------------------------------
         if ( timeron ) call timer_start( T_conj_grad )
         call conj_grad ( rnorm )
         if ( timeron ) call timer_stop( T_conj_grad )


!---------------------------------------------------------------------
!  zeta = shift + 1/(x.z)
!  So, first: (x.z)
!  Also, find norm of z
!  So, first: (z.z)
!---------------------------------------------------------------------
         norm_temp1 = 0.0d0
         norm_temp2 = 0.0d0
!$omp parallel default(shared) private(j,norm_temp3)
!$omp do reduction(+:norm_temp1,norm_temp2)
         do j=1, lastcol-firstcol+1
            norm_temp1 = norm_temp1 + x(j)*z(j)
            norm_temp2 = norm_temp2 + z(j)*z(j)
         enddo
!$omp end do


         norm_temp3 = 1.0d0 / sqrt( norm_temp2 )


!$omp master
         zeta = shift + 1.0d0 / norm_temp1
         if( it .eq. 1 ) write( *,9000 )
         write( *,9001 ) it, rnorm, zeta
!$omp end master

 9000    format( /,'   iteration           ||r||                 zeta' )
 9001    format( 4x, i5, 6x, e21.14, f20.13 )

!---------------------------------------------------------------------
!  Normalize z to obtain x
!---------------------------------------------------------------------
!$omp do
         do j=1, lastcol-firstcol+1      
            x(j) = norm_temp3*z(j)    
         enddo                           
!$omp end do nowait
!$omp end parallel


      enddo                              ! end of main iter inv pow meth

      call timer_stop( T_bench )

!---------------------------------------------------------------------
!  End of timed section
!---------------------------------------------------------------------

      t = timer_read( T_bench )


      write(*,100)
 100  format(' Benchmark completed ')

      epsilon = 1.d-10
      if (class .ne. 'U') then

!         err = abs( zeta - zeta_verify_value)
         err = abs( zeta - zeta_verify_value )/zeta_verify_value
         if( (.not.ieee_is_nan(err)) .and. (err .le. epsilon) ) then
            verified = .TRUE.
            write(*, 200)
            write(*, 201) zeta
            write(*, 202) err
 200        format(' VERIFICATION SUCCESSFUL ')
 201        format(' Zeta is    ', E20.13)
 202        format(' Error is   ', E20.13)
         else
            verified = .FALSE.
            write(*, 300) 
            write(*, 301) zeta
            write(*, 302) zeta_verify_value
 300        format(' VERIFICATION FAILED')
 301        format(' Zeta                ', E20.13)
 302        format(' The correct zeta is ', E20.13)
         endif
      else
         verified = .FALSE.
         write (*, 400)
         write (*, 401)
         write (*, 201) zeta
 400     format(' Problem size unknown')
 401     format(' NO VERIFICATION PERFORMED')
      endif


      if( t .ne. 0. ) then
         mflops = 1.0d-6 * 2*niter*dble( na )  &
     &               * ( 3.+nonzer*dble(nonzer+1)  &
     &                 + 25.*(5.+nonzer*dble(nonzer+1))  &
     &                 + 3. ) / t
      else
         mflops = 0.d0
      endif


         call print_results('CG', class, na, 0, 0,  &
     &                      niter, t,  &
     &                      mflops, '          floating point',  &
     &                      verified, npbversion, compiletime,  &
     &                      cs1, cs2, cs3, cs4, cs5, cs6, cs7)



 600  format( i4, 2e19.12)


!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
      if (.not.timeron) goto 999

      tmax = timer_read(T_bench)
      if (tmax .eq. 0.0) tmax = 1.0

      write(*,800)
 800  format('  SECTION   Time (secs)')
      do i=1, t_last
         t = timer_read(i)
         if (i.eq.t_init) then
            write(*,810) t_names(i), t
         else
            write(*,810) t_names(i), t, t*100./tmax
            if (i.eq.t_conj_grad) then
               t = tmax - t
               write(*,820) 'rest', t, t*100./tmax
            endif
         endif
 810     format(2x,a8,':',f9.3:'  (',f6.2,'%)')
 820     format('    --> ',a8,':',f9.3,'  (',f6.2,'%)')
      end do

 999  continue


      end                              ! end main



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
      implicit none

      integer   j
      integer   cgit, cgitmax
      integer(kz) k
!C    integer(kz) i, iresidue  

      double precision   d, sum, rho, rho0, alpha, beta, rnorm, suml

      data      cgitmax / 25 /


      rho = 0.0d0
      sum = 0.0d0

!$omp parallel default(shared) private(j,k,cgit,suml,alpha,beta)  &
!$omp&  shared(d,rho0,rho,sum)

!---------------------------------------------------------------------
!  Initialize the CG algorithm:
!---------------------------------------------------------------------
!$omp do
      do j=1,naa+1
         q(j) = 0.0d0
         z(j) = 0.0d0
         r(j) = x(j)
         p(j) = r(j)
      enddo
!$omp end do


!---------------------------------------------------------------------
!  rho = r.r
!  Now, obtain the norm of r: First, sum squares of r elements locally...
!---------------------------------------------------------------------
!$omp do reduction(+:rho)
      do j=1, lastcol-firstcol+1
         rho = rho + r(j)*r(j)
      enddo
!$omp end do

!---------------------------------------------------------------------
!---->
!  The conj grad iteration loop
!---->
!---------------------------------------------------------------------
      do cgit = 1, cgitmax

!$omp master
!---------------------------------------------------------------------
!  Save a temporary of rho and initialize reduction variables
!---------------------------------------------------------------------
         rho0 = rho
         d = 0.d0
         rho = 0.d0
!$omp end master
!$omp barrier

!---------------------------------------------------------------------
!  q = A.p
!  The partition submatrix-vector multiply: use workspace w
!---------------------------------------------------------------------
!
!  NOTE: this version of the multiply is actually (slightly: maybe %5) 
!        faster on the sp2 on 16 nodes than is the unrolled-by-2 version 
!        below.   On the Cray t3d, the reverse is true, i.e., the 
!        unrolled-by-two version is some 10% faster.  
!        The unrolled-by-8 version below is significantly faster
!        on the Cray t3d - overall speed of code is 1.5 times faster.
!
!$omp do
         do j=1,lastrow-firstrow+1
            suml = 0.d0
            do k=rowstr(j),rowstr(j+1)-1
               suml = suml + a(k)*p(colidx(k))
            enddo
            q(j) = suml
         enddo
!$omp end do

!C          do j=1,lastrow-firstrow+1
!C             i = rowstr(j) 
!C             iresidue = mod( rowstr(j+1)-i, 2 )
!C             suml = 0.d0
!C             if( iresidue .eq. 1 )  &
!C      &          suml = suml + a(i)*p(colidx(i))
!C             do k=i+iresidue, rowstr(j+1)-2, 2
!C                suml = suml + a(k)  *p(colidx(k))  &
!C      &                     + a(k+1)*p(colidx(k+1))
!C             enddo
!C             q(j) = suml
!C          enddo

!C          do j=1,lastrow-firstrow+1
!C             i = rowstr(j) 
!C             iresidue = mod( rowstr(j+1)-i, 8 )
!C             suml = 0.d0
!C             do k=i,i+iresidue-1
!C                suml = suml +  a(k)*p(colidx(k))
!C             enddo
!C             do k=i+iresidue, rowstr(j+1)-8, 8
!C                suml = suml + a(k  )*p(colidx(k  ))  &
!C      &                   + a(k+1)*p(colidx(k+1))  &
!C      &                   + a(k+2)*p(colidx(k+2))  &
!C      &                   + a(k+3)*p(colidx(k+3))  &
!C      &                   + a(k+4)*p(colidx(k+4))  &
!C      &                   + a(k+5)*p(colidx(k+5))  &
!C      &                   + a(k+6)*p(colidx(k+6))  &
!C      &                   + a(k+7)*p(colidx(k+7))
!C             enddo
!C             q(j) = suml
!C          enddo


!---------------------------------------------------------------------
!  Obtain p.q
!---------------------------------------------------------------------
!$omp do reduction(+:d)
         do j=1, lastcol-firstcol+1
            d = d + p(j)*q(j)
         enddo
!$omp end do


!---------------------------------------------------------------------
!  Obtain alpha = rho / (p.q)
!---------------------------------------------------------------------
         alpha = rho0 / d

!---------------------------------------------------------------------
!  Obtain z = z + alpha*p
!  and    r = r - alpha*q
!---------------------------------------------------------------------
!$omp do reduction(+:rho)
         do j=1, lastcol-firstcol+1
            z(j) = z(j) + alpha*p(j)
            r(j) = r(j) - alpha*q(j)
!         enddo
            
!---------------------------------------------------------------------
!  rho = r.r
!  Now, obtain the norm of r: First, sum squares of r elements locally...
!---------------------------------------------------------------------
!         do j=1, lastcol-firstcol+1
            rho = rho + r(j)*r(j)
         enddo
!$omp end do

!---------------------------------------------------------------------
!  Obtain beta:
!---------------------------------------------------------------------
         beta = rho / rho0

!---------------------------------------------------------------------
!  p = r + beta*p
!---------------------------------------------------------------------
!$omp do
         do j=1, lastcol-firstcol+1
            p(j) = r(j) + beta*p(j)
         enddo
!$omp end do


      enddo                             ! end of do cgit=1,cgitmax


!---------------------------------------------------------------------
!  Compute residual norm explicitly:  ||r|| = ||x - A.z||
!  First, form A.z
!  The partition submatrix-vector multiply
!---------------------------------------------------------------------
!$omp do
      do j=1,lastrow-firstrow+1
         suml = 0.d0
         do k=rowstr(j),rowstr(j+1)-1
            suml = suml + a(k)*z(colidx(k))
         enddo
         r(j) = suml
      enddo
!$omp end do


!---------------------------------------------------------------------
!  At this point, r contains A.z
!---------------------------------------------------------------------
!$omp do reduction(+:sum)
      do j=1, lastcol-firstcol+1
         suml = x(j) - r(j)         
         sum  = sum + suml*suml
      enddo
!$omp end do nowait
!$omp end parallel

      rnorm = sqrt( sum )



      return
      end                               ! end of routine conj_grad



!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine makea( n, nz, a, colidx, rowstr,  &
     &                  firstrow, lastrow, firstcol, lastcol,  &
     &                  arow, acol, aelt, v, iv )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use tinfo
      use cg_data, only : nonzer, rcond, shift

      implicit none

      integer             n
      integer(kz)         nz, rowstr(n+1)
      integer             firstrow, lastrow, firstcol, lastcol
      integer             colidx(nz)
      integer             iv(n+nz), arow(n), acol(nonzer+1,n)
      double precision    aelt(nonzer+1,n), v(nz)
      double precision    a(nz)

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

      integer          i, iouter, ivelt, nzv, nn1
      integer          ivc(nonzer+1)
      double precision vc(nonzer+1)

!---------------------------------------------------------------------
!      nonzer is approximately  (int(sqrt(nnza /n)));
!---------------------------------------------------------------------

      external          sparse, sprnvc, vecset
!$    integer           omp_get_num_threads, omp_get_thread_num
!$    external          omp_get_num_threads, omp_get_thread_num
      integer           work


!---------------------------------------------------------------------
!    nn1 is the smallest power of two not less than n
!---------------------------------------------------------------------

      nn1 = 1
 50   continue
        nn1 = 2 * nn1
        if (nn1 .lt. n) goto 50

!---------------------------------------------------------------------
!  Generate nonzero positions and save for the use in sparse.
!---------------------------------------------------------------------
      num_threads = 1
!$    num_threads = omp_get_num_threads()
      myid = 0
!$    myid  = omp_get_thread_num()
      if (num_threads .gt. max_threads) then
         if (myid .eq. 0) write(*,100) num_threads, max_threads
100      format(' Warning: num_threads',i6,  &
     &          ' exceeded an internal limit',i6)
         num_threads = max_threads
      endif
      work  = (n + num_threads - 1)/num_threads
      ilow  = work * myid + 1
      ihigh = ilow + work - 1
      if (ihigh .gt. n) ihigh = n

      do iouter = 1, ihigh
         nzv = nonzer
         call sprnvc( n, nzv, nn1, vc, ivc )
         if ( iouter .ge. ilow ) then
            call vecset( n, vc, ivc, nzv, iouter, .5D0 )
            arow(iouter) = nzv
            do ivelt = 1, nzv
               acol(ivelt, iouter) = ivc(ivelt)
               aelt(ivelt, iouter) = vc(ivelt)
            enddo
         endif
      enddo
!$omp barrier

!---------------------------------------------------------------------
!       ... make the sparse matrix from list of elements with duplicates
!           (v and iv are used as  workspace)
!---------------------------------------------------------------------
      call sparse( a, colidx, rowstr, n, nz, nonzer, arow, acol,  &
     &             aelt, firstrow, lastrow,  &
     &             v, iv(1), iv(nz+1), rcond, shift )
      return

      end
!-------end   of makea------------------------------

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine sparse( a, colidx, rowstr, n, nz, nonzer, arow, acol,  &
     &                   aelt, firstrow, lastrow,  &
     &                   v, iv, nzloc, rcond, shift )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use tinfo

      implicit           none

      integer            colidx(*), iv(*)
      integer            firstrow, lastrow
      integer            n, nonzer, arow(*), acol(nonzer+1,*)
      integer(kz)        nz, rowstr(*)
      double precision   a(*), aelt(nonzer+1,*), v(*), rcond, shift

!---------------------------------------------------------------------
!       rows range from firstrow to lastrow
!       the rowstr pointers are defined for nrows = lastrow-firstrow+1 values
!---------------------------------------------------------------------
      integer            nzloc(n), nrows

!---------------------------------------------------
!       generate a sparse matrix from a list of
!       [col, row, element] tri
!---------------------------------------------------

      integer            i, j, jcol
      integer(kz)        j1, j2, nza, k, kk, nzrow
      double precision   xi, size, scale, ratio, va

!---------------------------------------------------------------------
!    how many rows of result
!---------------------------------------------------------------------
      nrows = lastrow - firstrow + 1
      j1 = ilow + 1
      j2 = ihigh + 1

!---------------------------------------------------------------------
!     ...count the number of triples in each row
!---------------------------------------------------------------------
      do j = j1, j2
         rowstr(j) = 0
      enddo

      do i = 1, n
         do nza = 1, arow(i)
            j = acol(nza, i)
            if (j.ge.ilow .and. j.le.ihigh) then
               j = j + 1
               rowstr(j) = rowstr(j) + arow(i)
            endif
         end do
      end do

      if (myid .eq. 0) then
         rowstr(1) = 1
         j1 = 1
      endif
      do j = j1+1, j2
         rowstr(j) = rowstr(j) + rowstr(j-1)
      enddo
      if (myid .lt. num_threads) last_n(myid) = rowstr(j2)
!$omp barrier

      nzrow = 0
      if (myid .lt. num_threads) then
         do i = 0, myid-1
            nzrow = nzrow + last_n(i)
         end do
      endif
      if (nzrow .gt. 0) then
         do j = j1, j2
            rowstr(j) = rowstr(j) + nzrow
         enddo
      endif
!$omp barrier
      nza = rowstr(nrows+1) - 1

!---------------------------------------------------------------------
!     ... rowstr(j) now is the location of the first nonzero
!           of row j of a
!---------------------------------------------------------------------

      if (nza .gt. nz) then
!$omp master
         write(*,*) 'Space for matrix elements exceeded in sparse'
         write(*,*) 'nza, nzmax = ',nza, nz
!$omp end master
         stop
      endif


!---------------------------------------------------------------------
!     ... preload data pages
!---------------------------------------------------------------------
      do j = ilow, ihigh
         do k = rowstr(j), rowstr(j+1)-1
             v(k) = 0.d0
             iv(k) = 0
         enddo
         nzloc(j) = 0
      enddo

!---------------------------------------------------------------------
!     ... generate actual values by summing duplicates
!---------------------------------------------------------------------

      size = 1.0D0
      ratio = rcond ** (1.0D0 / dfloat(n))

      do i = 1, n
         do nza = 1, arow(i)
            j = acol(nza, i)

            if (j .lt. ilow .or. j .gt. ihigh) goto 60

            scale = size * aelt(nza, i)
            do nzrow = 1, arow(i)
               jcol = acol(nzrow, i)
               va = aelt(nzrow, i) * scale

!---------------------------------------------------------------------
!       ... add the identity * rcond to the generated matrix to bound
!           the smallest eigenvalue from below by rcond
!---------------------------------------------------------------------
               if (jcol .eq. j .and. j .eq. i) then
                  va = va + rcond - shift
               endif

               do k = rowstr(j), rowstr(j+1)-1
                  if (iv(k) .gt. jcol) then
!---------------------------------------------------------------------
!       ... insert colidx here orderly
!---------------------------------------------------------------------
                     do kk = rowstr(j+1)-2, k, -1
                        if (iv(kk) .gt. 0) then
                           v(kk+1)  = v(kk)
                           iv(kk+1) = iv(kk)
                        endif
                     enddo
                     iv(k) = jcol
                     v(k)  = 0.d0
                     goto 40
                  else if (iv(k) .eq. 0) then
                     iv(k) = jcol
                     goto 40
                  else if (iv(k) .eq. jcol) then
!---------------------------------------------------------------------
!       ... mark the duplicated entry
!---------------------------------------------------------------------
                     nzloc(j) = nzloc(j) + 1
                     goto 40
                  endif
               enddo
               print *,'internal error in sparse: i=',i
               stop
   40          continue
               v(k) = v(k) + va
            enddo
   60       continue
         enddo
         size = size * ratio
      enddo
!$omp barrier


!---------------------------------------------------------------------
!       ... remove empty entries and generate final results
!---------------------------------------------------------------------
      do j = ilow+1, ihigh
         nzloc(j) = nzloc(j) + nzloc(j-1)
      enddo
      if (myid .lt. num_threads) last_n(myid) = nzloc(ihigh)
!$omp barrier

      nzrow = 0
      if (myid .lt. num_threads) then
         do i = 0, myid-1
            nzrow = nzrow + last_n(i)
         end do
      endif
      if (nzrow .gt. 0) then
         do j = ilow, ihigh
            nzloc(j) = nzloc(j) + nzrow
         enddo
      endif
!$omp barrier

!$omp do
      do j = 1, nrows
         if (j .gt. 1) then
            j1 = rowstr(j) - nzloc(j-1)
         else
            j1 = 1
         endif
         j2 = rowstr(j+1) - nzloc(j) - 1
         nza = rowstr(j)
         do k = j1, j2
            a(k) = v(nza)
            colidx(k) = iv(nza)
            nza = nza + 1
         enddo
      enddo
!$omp end do
!$omp do
      do j = 2, nrows+1
         rowstr(j) = rowstr(j) - nzloc(j-1)
      enddo
!$omp end do
      nza = rowstr(nrows+1) - 1


!C       write (*, 11000) nza
      return
11000   format ( //,'final nonzero count in sparse ',  &
     &            /,'number of nonzeros       = ', i16 )
      end
!-------end   of sparse-----------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine sprnvc( n, nz, nn1, v, iv )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use cg_data, only : amult, tran

      implicit           none

      double precision   v(*)
      integer            n, nz, nn1, iv(*)


!---------------------------------------------------------------------
!       generate a sparse n-vector (v, iv)
!       having nzv nonzeros
!
!       mark(i) is set to 1 if position i is nonzero.
!       mark is all zero on entry and is reset to all zero before exit
!       this corrects a performance bug found by John G. Lewis, caused by
!       reinitialization of mark on every one of the n calls to sprnvc
!---------------------------------------------------------------------

        integer            nzv, ii, i, icnvrt

        external           randlc, icnvrt
        double precision   randlc, vecelt, vecloc


        nzv = 0

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
         do ii = 1, nzv
            if (iv(ii) .eq. i) goto 100
         enddo
         nzv = nzv + 1
         v(nzv) = vecelt
         iv(nzv) = i
         goto 100
110     continue

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



