!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                       O p e n M P     V E R S I O N                     !
!                                                                         !
!                                   F T                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB FT code.              !
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
! Authors: D. Bailey
!          W. Saphir
!          H. Jin
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------

!---------------------------------------------------------------------
! FT benchmark
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      program ft

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! Module ft_fields defines main arrays (u0, u1, u2) in the problem
!---------------------------------------------------------------------

      use ft_data
      use ft_fields

      implicit none

      integer i

      integer iter
      double precision total_time, mflops
      logical verified
      character class


!---------------------------------------------------------------------
! Run the entire problem once to make sure all data is touched. 
! This reduces variable startup costs, which is important for such a 
! short benchmark. The other NPB 2 implementations are similar. 
!---------------------------------------------------------------------
      do i = 1, t_max
         call timer_clear(i)
      end do

      call alloc_space

      call setup()
      call init_ui(u0, u1, twiddle, dims(1), dims(2), dims(3))
      call compute_indexmap(twiddle, dims(1), dims(2), dims(3))
      call compute_initial_conditions(u1, dims(1), dims(2), dims(3))
      call fft_init (dims(1))
      call fft(1, u1, u0)

!---------------------------------------------------------------------
! Start over from the beginning. Note that all operations must
! be timed, in contrast to other benchmarks. 
!---------------------------------------------------------------------
      do i = 1, t_max
         call timer_clear(i)
      end do

      call timer_start(T_total)
      if (timers_enabled) call timer_start(T_setup)

      call compute_indexmap(twiddle, dims(1), dims(2), dims(3))

      call compute_initial_conditions(u1, dims(1), dims(2), dims(3))

      call fft_init (dims(1))

      if (timers_enabled) call timer_stop(T_setup)
      if (timers_enabled) call timer_start(T_fft)
      call fft(1, u1, u0)
      if (timers_enabled) call timer_stop(T_fft)

      do iter = 1, niter
         if (timers_enabled) call timer_start(T_evolve)
         call evolve(u0, u1, twiddle, dims(1), dims(2), dims(3))
         if (timers_enabled) call timer_stop(T_evolve)
         if (timers_enabled) call timer_start(T_fft)
!         call fft(-1, u1, u2)
         call fft(-1, u1, u1)
         if (timers_enabled) call timer_stop(T_fft)
         if (timers_enabled) call timer_start(T_checksum)
!         call checksum(iter, u2, dims(1), dims(2), dims(3))
         call checksum(iter, u1, dims(1), dims(2), dims(3))
         if (timers_enabled) call timer_stop(T_checksum)
      end do

      call verify(nx, ny, nz, niter, verified, class)

      call timer_stop(t_total)
      total_time = timer_read(t_total)

      if( total_time .ne. 0. ) then
         mflops = 1.0d-6*ntotal_f *  &
     &             (14.8157+7.19641*log(ntotal_f)  &
     &          +  (5.23518+7.21113*log(ntotal_f))*niter)  &
     &                 /total_time
      else
         mflops = 0.0
      endif
      call print_results('FT', class, nx, ny, nz, niter,  &
     &  total_time, mflops, '          floating point', verified,  &
     &  npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6, cs7)
      if (timers_enabled) call print_timers()

      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine init_ui(u0, u1, twiddle, d1, d2, d3)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! touch all the big data
!---------------------------------------------------------------------

      implicit none
      integer d1, d2, d3
      double complex   u0(d1+1,d2,d3)
      double complex   u1(d1+1,d2,d3)
      double precision twiddle(d1+1,d2,d3)
      integer i, j, k

!$omp parallel do default(shared) private(i,j,k) collapse(2)
      do k = 1, d3
         do j = 1, d2
            do i = 1, d1
               u0(i,j,k) = 0.d0
               u1(i,j,k) = 0.d0
               twiddle(i,j,k) = 0.d0
            end do
         end do
      end do

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine evolve(u0, u1, twiddle, d1, d2, d3)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! evolve u0 -> u1 (t time steps) in fourier space
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer d1, d2, d3
      double complex   u0(d1+1,d2,d3)
      double complex   u1(d1+1,d2,d3)
      double precision twiddle(d1+1,d2,d3)
      integer i, j, k

!$omp parallel do default(shared) private(i,j,k) collapse(2)
      do k = 1, d3
         do j = 1, d2
            do i = 1, d1
               u0(i,j,k) = u0(i,j,k) * twiddle(i,j,k)
               u1(i,j,k) = u0(i,j,k)
            end do
         end do
      end do

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine compute_initial_conditions(u0, d1, d2, d3)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! Fill in array u0 with initial conditions from 
! random number generator 
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer d1, d2, d3
      double complex u0(d1+1, d2, d3)
      integer k, j
      double precision x0, start, an, dummy, starts(nz)
      

      start = seed
!---------------------------------------------------------------------
! Jump to the starting element for our first plane.
!---------------------------------------------------------------------
      call ipow46(a, 0, an)
      dummy = randlc(start, an)
      call ipow46(a, 2*nx*ny, an)

      starts(1) = start
      do k = 2, dims(3)
         dummy = randlc(start, an)
         starts(k) = start
      end do
      
!---------------------------------------------------------------------
! Go through by z planes filling in one square at a time.
!---------------------------------------------------------------------
!$omp parallel do default(shared) private(k,j,x0)
      do k = 1, dims(3) 
         x0 = starts(k)
         do j = 1, dims(2) 
            call vranlc(2*nx, x0, a, u0(1, j, k))
         end do
      end do

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine ipow46(a, exponent, result)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! compute a^exponent mod 2^46
!---------------------------------------------------------------------

      implicit none
      double precision a, result, dummy, q, r
      integer exponent, n, n2
      external randlc
      double precision randlc
!---------------------------------------------------------------------
! Use
!   a^n = a^(n/2)*a^(n/2) if n even else
!   a^n = a*a^(n-1)       if n odd
!---------------------------------------------------------------------
      result = 1
      if (exponent .eq. 0) return
      q = a
      r = 1
      n = exponent


      do while (n .gt. 1)
         n2 = n/2
         if (n2 * 2 .eq. n) then
            dummy = randlc(q, q) 
            n = n2
         else
            dummy = randlc(r, q)
            n = n-1
         endif
      end do
      dummy = randlc(r, q)
      result = r
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setup

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

!$    integer  omp_get_max_threads
!$    external omp_get_max_threads
      debug = .FALSE.

      call check_timer_flag( timers_enabled )

      write(*, 1000)

      niter = niter_default

      write(*, 1001) nx, ny, nz
      write(*, 1002) niter
!$    write(*, 1003) omp_get_max_threads()
      write(*, *)


 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &          ' - FT Benchmark', /)
 1001 format(' Size                : ', i4, 'x', i4, 'x', i4)
 1002 format(' Iterations                  :', i7)
 1003 format(' Number of available threads :', i7)

      dims(1) = nx
      dims(2) = ny
      dims(3) = nz


!---------------------------------------------------------------------
! Set up info for blocking of ffts and transposes.  This improves
! performance on cache-based systems. Blocking involves
! working on a chunk of the problem at a time, taking chunks
! along the first, second, or third dimension. 
!
! - In cffts1 blocking is on 2nd dimension (with fft on 1st dim)
! - In cffts2/3 blocking is on 1st dimension (with fft on 2nd and 3rd dims)

! Since 1st dim is always in processor, we'll assume it's long enough 
! (default blocking factor is 16 so min size for 1st dim is 16)
! The only case we have to worry about is cffts1 in a 2d decomposition. 
! so the blocking factor should not be larger than the 2nd dimension. 
!---------------------------------------------------------------------

      fftblock = fftblock_default
      fftblockpad = fftblockpad_default

      if (fftblock .ne. fftblock_default) fftblockpad = fftblock+3

      return
      end

      
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine compute_indexmap(twiddle, d1, d2, d3)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! compute function from local (i,j,k) to ibar^2+jbar^2+kbar^2 
! for time evolution exponent. 
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer d1, d2, d3
      double precision twiddle(d1+1, d2, d3)
      integer i, j, k, kk, kk2, jj, kj2, ii
      double precision ap

!---------------------------------------------------------------------
! basically we want to convert the fortran indices 
!   1 2 3 4 5 6 7 8 
! to 
!   0 1 2 3 -4 -3 -2 -1
! The following magic formula does the trick:
! mod(i-1+n/2, n) - n/2
!---------------------------------------------------------------------

      ap = - 4.d0 * alpha * pi *pi

!$omp parallel do default(shared) private(i,j,k,kk,kk2,jj,kj2,ii)  &
!$omp&  collapse(2)
      do k = 1, dims(3)
         do j = 1, dims(2)
            kk =  mod(k-1+nz/2, nz) - nz/2
            kk2 = kk*kk
            jj = mod(j-1+ny/2, ny) - ny/2
            kj2 = jj*jj+kk2
            do i = 1, dims(1)
               ii = mod(i-1+nx/2, nx) - nx/2
               twiddle(i,j,k) = dexp(ap*dble(ii*ii+kj2))
            end do
         end do
      end do

      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine print_timers()

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer i
      double precision t, t_m
      character*25 tstrings(T_max)
      data tstrings / '          total ',  &
     &                '          setup ',  &
     &                '            fft ',  &
     &                '         evolve ',  &
     &                '       checksum ',  &
     &                '           fftx ',  &
     &                '           ffty ',  &
     &                '           fftz ' /

      t_m = timer_read(T_total)
      if (t_m .le. 0.0d0) t_m = 1.0d0
      do i = 1, t_max
         t = timer_read(i)
         write(*, 100) i, tstrings(i), t, t*100.0/t_m
      end do
 100  format(' timer ', i2, '(', A16,  ') :', F9.4, ' (',F6.2,'%)')
      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine fft(dir, x1, x2)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer dir
      double complex x1(ntotalp), x2(ntotalp)

      double complex y1(fftblockpad_default*maxdim),  &
     &               y2(fftblockpad_default*maxdim)

!---------------------------------------------------------------------
! note: args x1, x2 must be different arrays
! note: args for cfftsx are (direction, layout, xin, xout, scratch)
!       xin/xout may be the same and it can be somewhat faster
!       if they are
!---------------------------------------------------------------------

      if (dir .eq. 1) then
         call cffts1(1, dims(1), dims(2), dims(3), x1, x1, y1, y2)
         call cffts2(1, dims(1), dims(2), dims(3), x1, x1, y1, y2)
         call cffts3(1, dims(1), dims(2), dims(3), x1, x2, y1, y2)
      else
         call cffts3(-1, dims(1), dims(2), dims(3), x1, x1, y1, y2)
         call cffts2(-1, dims(1), dims(2), dims(3), x1, x1, y1, y2)
         call cffts1(-1, dims(1), dims(2), dims(3), x1, x2, y1, y2)
      endif
      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine cffts1(is, d1, d2, d3, x, xout, y1, y2)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer is, d1, d2, d3, logd1
      double complex x(d1+1,d2,d3)
      double complex xout(d1+1,d2,d3)
      double complex y1(fftblockpad, d1), y2(fftblockpad, d1)
      integer i, j, k, jj, jn

      logd1 = ilog2(d1)

      if (timers_enabled) call timer_start(T_fftx)
!$omp parallel do default(shared) private(i,j,k,jj,y1,y2,jn)  &
!$omp&  shared(is,logd1,d1) collapse(2)
      do k = 1, d3
         do jn = 0, d2/fftblock - 1
!         do jj = 0, d2 - fftblock, fftblock
            jj = jn*fftblock
            do j = 1, fftblock
               do i = 1, d1
                  y1(j,i) = x(i,j+jj,k)
               enddo
            enddo
            
            call cfftz (is, logd1, d1, y1, y2)


            do j = 1, fftblock
               do i = 1, d1
                  xout(i,j+jj,k) = y1(j,i)
               enddo
            enddo
         enddo
      enddo
      if (timers_enabled) call timer_stop(T_fftx)

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine cffts2(is, d1, d2, d3, x, xout, y1, y2)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer is, d1, d2, d3, logd2
      double complex x(d1+1,d2,d3)
      double complex xout(d1+1,d2,d3)
      double complex y1(fftblockpad, d2), y2(fftblockpad, d2)
      integer i, j, k, ii, in

      logd2 = ilog2(d2)

      if (timers_enabled) call timer_start(T_ffty)
!$omp parallel do default(shared) private(i,j,k,ii,y1,y2,in)  &
!$omp&  shared(is,logd2,d2) collapse(2)
      do k = 1, d3
        do in = 0, d1/fftblock - 1
!        do ii = 0, d1 - fftblock, fftblock
           ii = in*fftblock
           do j = 1, d2
              do i = 1, fftblock
                 y1(i,j) = x(i+ii,j,k)
              enddo
           enddo

           call cfftz (is, logd2, d2, y1, y2)
           
           do j = 1, d2
              do i = 1, fftblock
                 xout(i+ii,j,k) = y1(i,j)
              enddo
           enddo
        enddo
      enddo
      if (timers_enabled) call timer_stop(T_ffty)

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine cffts3(is, d1, d2, d3, x, xout, y1, y2)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer is, d1, d2, d3, logd3
      double complex x(d1+1,d2,d3)
      double complex xout(d1+1,d2,d3)
      double complex y1(fftblockpad, d3), y2(fftblockpad, d3)
      integer i, j, k, ii, in

      logd3 = ilog2(d3)

      if (timers_enabled) call timer_start(T_fftz)
!$omp parallel do default(shared) private(i,j,k,ii,y1,y2,in)  &
!$omp&  shared(is) collapse(2)
      do j = 1, d2
        do in = 0, d1/fftblock - 1
!        do ii = 0, d1 - fftblock, fftblock
           ii = in*fftblock
           do k = 1, d3
              do i = 1, fftblock
                 y1(i,k) = x(i+ii,j,k)
              enddo
           enddo

           call cfftz (is, logd3, d3, y1, y2)

           do k = 1, d3
              do i = 1, fftblock
                 xout(i+ii,j,k) = y1(i,k)
              enddo
           enddo
        enddo
      enddo
      if (timers_enabled) call timer_stop(T_fftz)

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine fft_init (n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! compute the roots-of-unity array that will be used for subsequent FFTs. 
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer m,n,nu,ku,i,j,ln
      double precision t, ti


!---------------------------------------------------------------------
!   Initialize the U array with sines and cosines in a manner that permits
!   stride one access at each FFT iteration.
!---------------------------------------------------------------------
      nu = n
      m = ilog2(n)
      u(1) = m
      ku = 2
      ln = 1

      do j = 1, m
         t = pi / ln
         
         do i = 0, ln - 1
            ti = i * t
            u(i+ku) = dcmplx (cos (ti), sin(ti))
         enddo
         
         ku = ku + ln
         ln = 2 * ln
      enddo
      
      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine cfftz (is, m, n, x, y)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   Computes NY N-point complex-to-complex FFTs of X using an algorithm due
!   to Swarztrauber.  X is both the input and the output array, while Y is a 
!   scratch array.  It is assumed that N = 2^M.  Before calling CFFTZ to 
!   perform FFTs, the array U must be initialized by calling CFFTZ with IS 
!   set to 0 and M set to MX, where MX is the maximum value of M for any 
!   subsequent call.
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer is,m,n,i,j,l,mx
      double complex x, y

      dimension x(fftblockpad,n), y(fftblockpad,n)

!---------------------------------------------------------------------
!   Check if input parameters are invalid.
!---------------------------------------------------------------------
      mx = u(1)
      if ((is .ne. 1 .and. is .ne. -1) .or. m .lt. 1 .or. m .gt. mx)    &
     &  then
        write (*, 1)  is, m, mx
 1      format ('CFFTZ: Either U has not been initialized, or else'/    &
     &    'one of the input parameters is invalid', 3I5)
        stop
      endif

!---------------------------------------------------------------------
!   Perform one variant of the Stockham FFT.
!---------------------------------------------------------------------
      do l = 1, m, 2
        call fftz2 (is, l, m, n, fftblock, fftblockpad, u, x, y)
        if (l .eq. m) goto 160
        call fftz2 (is, l + 1, m, n, fftblock, fftblockpad, u, y, x)
      enddo

      goto 180

!---------------------------------------------------------------------
!   Copy Y to X.
!---------------------------------------------------------------------
 160  do j = 1, n
        do i = 1, fftblock
          x(i,j) = y(i,j)
        enddo
      enddo

 180  continue

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine fftz2 (is, l, m, n, ny, ny1, u, x, y)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   Performs the L-th iteration of the second variant of the Stockham FFT.
!---------------------------------------------------------------------

      implicit none

      integer is,k,l,m,n,ny,ny1,n1,li,lj,lk,ku,i,j,i11,i12,i21,i22
      double complex u,x,y,u1,x11,x21
      dimension u(n), x(ny1,n), y(ny1,n)


!---------------------------------------------------------------------
!   Set initial parameters.
!---------------------------------------------------------------------

      n1 = n / 2
      lk = 2 ** (l - 1)
      li = 2 ** (m - l)
      lj = 2 * lk
      ku = li + 1

      do i = 0, li - 1
        i11 = i * lk + 1
        i12 = i11 + n1
        i21 = i * lj + 1
        i22 = i21 + lk
        if (is .ge. 1) then
          u1 = u(ku+i)
        else
          u1 = dconjg (u(ku+i))
        endif

!---------------------------------------------------------------------
!   This loop is vectorizable.
!---------------------------------------------------------------------
        do k = 0, lk - 1
          do j = 1, ny
            x11 = x(j,i11+k)
            x21 = x(j,i12+k)
            y(j,i21+k) = x11 + x21
            y(j,i22+k) = u1 * (x11 - x21)
          enddo
        enddo
      enddo

      return
      end

!---------------------------------------------------------------------


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      integer function ilog2(n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit none
      integer n, nn, lg
      if (n .eq. 1) then
         ilog2=0
         return
      endif
      lg = 1
      nn = 2
      do while (nn .lt. n)
         nn = nn*2
         lg = lg+1
      end do
      ilog2 = lg
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine checksum(i, u1, d1, d2, d3)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use ft_data
      implicit none

      integer i, d1, d2, d3
      double complex u1(d1+1,d2,d3)
      integer j, q,r,s
      double complex chk
      chk = (0.0,0.0)

!$omp parallel do default(shared) private(i,q,r,s) reduction(+:chk)
      do j=1,1024
         q = mod(j, nx)+1
         r = mod(3*j,ny)+1
         s = mod(5*j,nz)+1
         chk=chk+u1(q,r,s)
      end do

      chk = chk/ntotal_f
      
      write (*, 30) i, chk
 30   format (' T =',I5,5X,'Checksum =',1P2D22.12)
      sums(i) = chk
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine verify (d1, d2, d3, nt, verified, class)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use, intrinsic :: ieee_arithmetic, only : ieee_is_nan

      use ft_data

      implicit none

      integer d1, d2, d3, nt
      character class
      logical verified
      integer i
      double precision err, epsilon

!---------------------------------------------------------------------
!   Reference checksums
!---------------------------------------------------------------------
      double complex csum_ref(25)


      class = 'U'

      epsilon = 1.0d-12
      verified = .FALSE.

      if (d1 .eq. 64 .and.  &
     &    d2 .eq. 64 .and.  &
     &    d3 .eq. 64 .and.  &
     &    nt .eq. 6) then
!---------------------------------------------------------------------
!   Sample size reference checksums
!---------------------------------------------------------------------
         class = 'S'
         csum_ref(1) = dcmplx(5.546087004964D+02, 4.845363331978D+02)
         csum_ref(2) = dcmplx(5.546385409189D+02, 4.865304269511D+02)
         csum_ref(3) = dcmplx(5.546148406171D+02, 4.883910722336D+02)
         csum_ref(4) = dcmplx(5.545423607415D+02, 4.901273169046D+02)
         csum_ref(5) = dcmplx(5.544255039624D+02, 4.917475857993D+02)
         csum_ref(6) = dcmplx(5.542683411902D+02, 4.932597244941D+02)

      else if (d1 .eq. 128 .and.  &
     &    d2 .eq. 128 .and.  &
     &    d3 .eq. 32 .and.  &
     &    nt .eq. 6) then
!---------------------------------------------------------------------
!   Class W size reference checksums
!---------------------------------------------------------------------
         class = 'W'
         csum_ref(1) = dcmplx(5.673612178944D+02, 5.293246849175D+02)
         csum_ref(2) = dcmplx(5.631436885271D+02, 5.282149986629D+02)
         csum_ref(3) = dcmplx(5.594024089970D+02, 5.270996558037D+02)
         csum_ref(4) = dcmplx(5.560698047020D+02, 5.260027904925D+02)
         csum_ref(5) = dcmplx(5.530898991250D+02, 5.249400845633D+02)
         csum_ref(6) = dcmplx(5.504159734538D+02, 5.239212247086D+02)

      else if (d1 .eq. 256 .and.  &
     &    d2 .eq. 256 .and.  &
     &    d3 .eq. 128 .and.  &
     &    nt .eq. 6) then
!---------------------------------------------------------------------
!   Class A size reference checksums
!---------------------------------------------------------------------
         class = 'A'
         csum_ref(1) = dcmplx(5.046735008193D+02, 5.114047905510D+02)
         csum_ref(2) = dcmplx(5.059412319734D+02, 5.098809666433D+02)
         csum_ref(3) = dcmplx(5.069376896287D+02, 5.098144042213D+02)
         csum_ref(4) = dcmplx(5.077892868474D+02, 5.101336130759D+02)
         csum_ref(5) = dcmplx(5.085233095391D+02, 5.104914655194D+02)
         csum_ref(6) = dcmplx(5.091487099959D+02, 5.107917842803D+02)
      
      else if (d1 .eq. 512 .and.  &
     &    d2 .eq. 256 .and.  &
     &    d3 .eq. 256 .and.  &
     &    nt .eq. 20) then
!---------------------------------------------------------------------
!   Class B size reference checksums
!---------------------------------------------------------------------
         class = 'B'
         csum_ref(1)  = dcmplx(5.177643571579D+02, 5.077803458597D+02)
         csum_ref(2)  = dcmplx(5.154521291263D+02, 5.088249431599D+02)
         csum_ref(3)  = dcmplx(5.146409228649D+02, 5.096208912659D+02)
         csum_ref(4)  = dcmplx(5.142378756213D+02, 5.101023387619D+02)
         csum_ref(5)  = dcmplx(5.139626667737D+02, 5.103976610617D+02)
         csum_ref(6)  = dcmplx(5.137423460082D+02, 5.105948019802D+02)
         csum_ref(7)  = dcmplx(5.135547056878D+02, 5.107404165783D+02)
         csum_ref(8)  = dcmplx(5.133910925466D+02, 5.108576573661D+02)
         csum_ref(9)  = dcmplx(5.132470705390D+02, 5.109577278523D+02)
         csum_ref(10) = dcmplx(5.131197729984D+02, 5.110460304483D+02)
         csum_ref(11) = dcmplx(5.130070319283D+02, 5.111252433800D+02)
         csum_ref(12) = dcmplx(5.129070537032D+02, 5.111968077718D+02)
         csum_ref(13) = dcmplx(5.128182883502D+02, 5.112616233064D+02)
         csum_ref(14) = dcmplx(5.127393733383D+02, 5.113203605551D+02)
         csum_ref(15) = dcmplx(5.126691062020D+02, 5.113735928093D+02)
         csum_ref(16) = dcmplx(5.126064276004D+02, 5.114218460548D+02)
         csum_ref(17) = dcmplx(5.125504076570D+02, 5.114656139760D+02)
         csum_ref(18) = dcmplx(5.125002331720D+02, 5.115053595966D+02)
         csum_ref(19) = dcmplx(5.124551951846D+02, 5.115415130407D+02)
         csum_ref(20) = dcmplx(5.124146770029D+02, 5.115744692211D+02)

      else if (d1 .eq. 512 .and.  &
     &    d2 .eq. 512 .and.  &
     &    d3 .eq. 512 .and.  &
     &    nt .eq. 20) then
!---------------------------------------------------------------------
!   Class C size reference checksums
!---------------------------------------------------------------------
         class = 'C'
         csum_ref(1)  = dcmplx(5.195078707457D+02, 5.149019699238D+02)
         csum_ref(2)  = dcmplx(5.155422171134D+02, 5.127578201997D+02)
         csum_ref(3)  = dcmplx(5.144678022222D+02, 5.122251847514D+02)
         csum_ref(4)  = dcmplx(5.140150594328D+02, 5.121090289018D+02)
         csum_ref(5)  = dcmplx(5.137550426810D+02, 5.121143685824D+02)
         csum_ref(6)  = dcmplx(5.135811056728D+02, 5.121496764568D+02)
         csum_ref(7)  = dcmplx(5.134569343165D+02, 5.121870921893D+02)
         csum_ref(8)  = dcmplx(5.133651975661D+02, 5.122193250322D+02)
         csum_ref(9)  = dcmplx(5.132955192805D+02, 5.122454735794D+02)
         csum_ref(10) = dcmplx(5.132410471738D+02, 5.122663649603D+02)
         csum_ref(11) = dcmplx(5.131971141679D+02, 5.122830879827D+02)
         csum_ref(12) = dcmplx(5.131605205716D+02, 5.122965869718D+02)
         csum_ref(13) = dcmplx(5.131290734194D+02, 5.123075927445D+02)
         csum_ref(14) = dcmplx(5.131012720314D+02, 5.123166486553D+02)
         csum_ref(15) = dcmplx(5.130760908195D+02, 5.123241541685D+02)
         csum_ref(16) = dcmplx(5.130528295923D+02, 5.123304037599D+02)
         csum_ref(17) = dcmplx(5.130310107773D+02, 5.123356167976D+02)
         csum_ref(18) = dcmplx(5.130103090133D+02, 5.123399592211D+02)
         csum_ref(19) = dcmplx(5.129905029333D+02, 5.123435588985D+02)
         csum_ref(20) = dcmplx(5.129714421109D+02, 5.123465164008D+02)

      else if (d1 .eq. 2048 .and.  &
     &    d2 .eq. 1024 .and.  &
     &    d3 .eq. 1024 .and.  &
     &    nt .eq. 25) then
!---------------------------------------------------------------------
!   Class D size reference checksums
!---------------------------------------------------------------------
         class = 'D'
         csum_ref(1)  = dcmplx(5.122230065252D+02, 5.118534037109D+02)
         csum_ref(2)  = dcmplx(5.120463975765D+02, 5.117061181082D+02)
         csum_ref(3)  = dcmplx(5.119865766760D+02, 5.117096364601D+02)
         csum_ref(4)  = dcmplx(5.119518799488D+02, 5.117373863950D+02)
         csum_ref(5)  = dcmplx(5.119269088223D+02, 5.117680347632D+02)
         csum_ref(6)  = dcmplx(5.119082416858D+02, 5.117967875532D+02)
         csum_ref(7)  = dcmplx(5.118943814638D+02, 5.118225281841D+02)
         csum_ref(8)  = dcmplx(5.118842385057D+02, 5.118451629348D+02)
         csum_ref(9)  = dcmplx(5.118769435632D+02, 5.118649119387D+02)
         csum_ref(10) = dcmplx(5.118718203448D+02, 5.118820803844D+02)
         csum_ref(11) = dcmplx(5.118683569061D+02, 5.118969781011D+02)
         csum_ref(12) = dcmplx(5.118661708593D+02, 5.119098918835D+02)
         csum_ref(13) = dcmplx(5.118649768950D+02, 5.119210777066D+02)
         csum_ref(14) = dcmplx(5.118645605626D+02, 5.119307604484D+02)
         csum_ref(15) = dcmplx(5.118647586618D+02, 5.119391362671D+02)
         csum_ref(16) = dcmplx(5.118654451572D+02, 5.119463757241D+02)
         csum_ref(17) = dcmplx(5.118665212451D+02, 5.119526269238D+02)
         csum_ref(18) = dcmplx(5.118679083821D+02, 5.119580184108D+02)
         csum_ref(19) = dcmplx(5.118695433664D+02, 5.119626617538D+02)
         csum_ref(20) = dcmplx(5.118713748264D+02, 5.119666538138D+02)
         csum_ref(21) = dcmplx(5.118733606701D+02, 5.119700787219D+02)
         csum_ref(22) = dcmplx(5.118754661974D+02, 5.119730095953D+02)
         csum_ref(23) = dcmplx(5.118776626738D+02, 5.119755100241D+02)
         csum_ref(24) = dcmplx(5.118799262314D+02, 5.119776353561D+02)
         csum_ref(25) = dcmplx(5.118822370068D+02, 5.119794338060D+02)

      else if (d1 .eq. 4096 .and.  &
     &    d2 .eq. 2048 .and.  &
     &    d3 .eq. 2048 .and.  &
     &    nt .eq. 25) then
!---------------------------------------------------------------------
!   Class E size reference checksums
!---------------------------------------------------------------------
         class = 'E'
         csum_ref(1)  = dcmplx(5.121601045346D+02, 5.117395998266D+02)
         csum_ref(2)  = dcmplx(5.120905403678D+02, 5.118614716182D+02)
         csum_ref(3)  = dcmplx(5.120623229306D+02, 5.119074203747D+02)
         csum_ref(4)  = dcmplx(5.120438418997D+02, 5.119345900733D+02)
         csum_ref(5)  = dcmplx(5.120311521872D+02, 5.119551325550D+02)
         csum_ref(6)  = dcmplx(5.120226088809D+02, 5.119720179919D+02)
         csum_ref(7)  = dcmplx(5.120169296534D+02, 5.119861371665D+02)
         csum_ref(8)  = dcmplx(5.120131225172D+02, 5.119979364402D+02)
         csum_ref(9)  = dcmplx(5.120104767108D+02, 5.120077674092D+02)
         csum_ref(10) = dcmplx(5.120085127969D+02, 5.120159443121D+02)
         csum_ref(11) = dcmplx(5.120069224127D+02, 5.120227453670D+02)
         csum_ref(12) = dcmplx(5.120055158164D+02, 5.120284096041D+02)
         csum_ref(13) = dcmplx(5.120041820159D+02, 5.120331373793D+02)
         csum_ref(14) = dcmplx(5.120028605402D+02, 5.120370938679D+02)
         csum_ref(15) = dcmplx(5.120015223011D+02, 5.120404138831D+02)
         csum_ref(16) = dcmplx(5.120001570022D+02, 5.120432068837D+02)
         csum_ref(17) = dcmplx(5.119987650555D+02, 5.120455615860D+02)
         csum_ref(18) = dcmplx(5.119973525091D+02, 5.120475499442D+02)
         csum_ref(19) = dcmplx(5.119959279472D+02, 5.120492304629D+02)
         csum_ref(20) = dcmplx(5.119945006558D+02, 5.120506508902D+02)
         csum_ref(21) = dcmplx(5.119930795911D+02, 5.120518503782D+02)
         csum_ref(22) = dcmplx(5.119916728462D+02, 5.120528612016D+02)
         csum_ref(23) = dcmplx(5.119902874185D+02, 5.120537101195D+02)
         csum_ref(24) = dcmplx(5.119889291565D+02, 5.120544194514D+02)
         csum_ref(25) = dcmplx(5.119876028049D+02, 5.120550079284D+02)

      else if (d1 .eq. 8192 .and.  &
     &    d2 .eq. 4096 .and.  &
     &    d3 .eq. 4096 .and.  &
     &    nt .eq. 25) then
!---------------------------------------------------------------------
!   Class F size reference checksums
!---------------------------------------------------------------------
         class = 'F'
         csum_ref( 1) = dcmplx(5.119892866928D+02, 5.121457822747D+02)
         csum_ref( 2) = dcmplx(5.119560157487D+02, 5.121009044434D+02)
         csum_ref( 3) = dcmplx(5.119437960123D+02, 5.120761074285D+02)
         csum_ref( 4) = dcmplx(5.119395628845D+02, 5.120614320496D+02)
         csum_ref( 5) = dcmplx(5.119390371879D+02, 5.120514085624D+02)
         csum_ref( 6) = dcmplx(5.119405091840D+02, 5.120438117102D+02)
         csum_ref( 7) = dcmplx(5.119430444528D+02, 5.120376348915D+02)
         csum_ref( 8) = dcmplx(5.119460702242D+02, 5.120323831062D+02)
         csum_ref( 9) = dcmplx(5.119492377036D+02, 5.120277980818D+02)
         csum_ref(10) = dcmplx(5.119523446268D+02, 5.120237368268D+02)
         csum_ref(11) = dcmplx(5.119552825361D+02, 5.120201137845D+02)
         csum_ref(12) = dcmplx(5.119580008777D+02, 5.120168723492D+02)
         csum_ref(13) = dcmplx(5.119604834177D+02, 5.120139707209D+02)
         csum_ref(14) = dcmplx(5.119627332821D+02, 5.120113749334D+02)
         csum_ref(15) = dcmplx(5.119647637538D+02, 5.120090554887D+02)
         csum_ref(16) = dcmplx(5.119665927740D+02, 5.120069857863D+02)
         csum_ref(17) = dcmplx(5.119682397643D+02, 5.120051414260D+02)
         csum_ref(18) = dcmplx(5.119697238718D+02, 5.120034999132D+02)
         csum_ref(19) = dcmplx(5.119710630664D+02, 5.120020405355D+02)
         csum_ref(20) = dcmplx(5.119722737384D+02, 5.120007442976D+02)
         csum_ref(21) = dcmplx(5.119733705802D+02, 5.119995938652D+02)
         csum_ref(22) = dcmplx(5.119743666226D+02, 5.119985735001D+02)
         csum_ref(23) = dcmplx(5.119752733481D+02, 5.119976689792D+02)
         csum_ref(24) = dcmplx(5.119761008382D+02, 5.119968675026D+02)
         csum_ref(25) = dcmplx(5.119768579280D+02, 5.119961575929D+02)

      endif


      if (class .ne. 'U') then

         do i = 1, nt
            err = abs( (sums(i) - csum_ref(i)) / csum_ref(i) )
            if (ieee_is_nan(err) .or. (err .gt. epsilon)) goto 100
         end do
         verified = .TRUE.
 100     continue

      endif

         
      if (class .ne. 'U') then
         if (verified) then
            write(*,2000)
 2000       format(' Result verification successful')
         else
            write(*,2001)
 2001       format(' Result verification failed')
         endif
      endif
      print *, 'class = ', class

      return
      end


