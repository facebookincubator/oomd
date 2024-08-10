!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!                       O p e n M P     V E R S I O N                     !
!                                                                         !
!                                   E P                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB EP code.              !
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
! Author: P. O. Frederickson 
!         D. H. Bailey
!         A. C. Woo
!         H. Jin
!---------------------------------------------------------------------

!---------------------------------------------------------------------
      program EMBAR
!---------------------------------------------------------------------

!   This is the OpenMP version of the APP Benchmark 1,
!   the "embarassingly parallel" benchmark.

      use ep_data

      implicit none

      double precision Mops, t1, t2, t3, t4, x1, x2,  &
     &                 sx, sy, tm, an, tt, gc, dum(3)

      integer          i, ik, kk, l, k, nit,  &
     &                 np, k_offset, j

      logical          verified, timers_enabled

      external         randlc, timer_read
      double precision randlc, timer_read

      character        size*15, classv

!$    integer  omp_get_max_threads
!$    external omp_get_max_threads

      data             dum /1.d0, 1.d0, 1.d0/


      call check_timer_flag( timers_enabled )

!   Because the size of the problem is too large to store in a 32-bit
!   integer for some classes, we put it into a string (for printing).
!   Have to strip off the decimal point put in there by the floating
!   point print statement (internal file)

      write(*, 1000)
      write(size, '(f15.0)' ) 2.d0**(m+1)
      j = 15
      if (size(j:j) .eq. '.') j = j - 1
      write (*,1001) size(1:j)
!$    write (*,1003) omp_get_max_threads()
      write (*,*)

 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &          ' - EP Benchmark', /)
 1001 format(' Number of random numbers generated: ', a15)
 1003 format(' Number of available threads:        ', 2x,i13)


!   Compute the number of "batches" of random number pairs generated 
!   per processor. Adjust if the number of processors does not evenly 
!   divide the total number

      np = nn 


!   Call the random number generator functions and initialize
!   the x-array to reduce the effects of paging on the timings.
!   Also, call all mathematical functions that are used. Make
!   sure these initializations cannot be eliminated as dead code.

      call vranlc(0, dum(1), dum(2), dum(3))
      dum(1) = randlc(dum(2), dum(3))
      Mops = log(sqrt(abs(max(1.d0,1.d0))))

!$omp parallel default(shared) private(i)
      do 5    i = 1, 2*nk
         x(i) = -1.d99
 5    continue

      call timer_clear(1)
      if (timers_enabled) call timer_clear(2)
      if (timers_enabled) call timer_clear(3)
!$omp end parallel

      call timer_start(1)

      t1 = a
      call vranlc(0, t1, a, x)

!   Compute AN = A ^ (2 * NK) (mod 2^46).

      t1 = a

      do 100 i = 1, mk + 1
         t2 = randlc(t1, t1)
 100  continue

      an = t1
      tt = s
      gc = 0.d0
      sx = 0.d0
      sy = 0.d0

      do 110 i = 0, nq - 1
         q(i) = 0.d0
 110  continue

!   Each instance of this loop may be performed independently. We compute
!   the k offsets separately to take into account the fact that some nodes
!   have more numbers to generate than others

      k_offset = -1

!$omp parallel default(shared) reduction(+:sx,sy)  &
!$omp&  private(k,kk,t1,t2,t3,t4,i,ik,x1,x2,l)
      do 115 i = 0, nq - 1
         qq(i) = 0.d0
 115  continue

!$omp do schedule(static)
      do 150 k = 1, np
         kk = k_offset + k 
         t1 = s
         t2 = an

!        Find starting seed t1 for this kk.

         if (timers_enabled) call timer_start(3)
         do 120 i = 1, 100
            ik = kk / 2
            if (2 * ik .ne. kk) t3 = randlc(t1, t2)
            if (ik .eq. 0) goto 130
            t3 = randlc(t2, t2)
            kk = ik
 120     continue

!        Compute uniform pseudorandom numbers.
 130     continue

         call vranlc(2 * nk, t1, a, x)
         if (timers_enabled) call timer_stop(3)

!        Compute Gaussian deviates by acceptance-rejection method and 
!        tally counts in concentric square annuli.  This loop is not 
!        vectorizable. 

         if (timers_enabled) call timer_start(2)

         do 140 i = 1, nk
            x1 = 2.d0 * x(2*i-1) - 1.d0
            x2 = 2.d0 * x(2*i) - 1.d0
            t1 = x1 ** 2 + x2 ** 2
            if (t1 .le. 1.d0) then
               t2   = sqrt(-2.d0 * log(t1) / t1)
               t3   = abs(x1 * t2)
               t4   = abs(x2 * t2)
               l    = max(t3, t4)
               qq(l) = qq(l) + 1.d0
               sx   = sx + t3
               sy   = sy + t4
            endif
 140     continue

         if (timers_enabled) call timer_stop(2)

 150  continue
!$omp end do nowait

      do 155 i = 0, nq - 1
!$omp atomic
         q(i) = q(i) + qq(i)
 155  continue
!$omp end parallel

      do 160 i = 0, nq - 1
         gc = gc + q(i)
 160  continue

      call timer_stop(1)
      tm  = timer_read(1)

      call verify(m, sx, sy, gc, verified, classv)

      nit=0
      Mops = 2.d0**(m+1)/tm/1000000.d0

      write (6,11) tm, m, gc, sx, sy, (i, q(i), i = 0, nq - 1)
 11   format ('EP Benchmark Results:'//'CPU Time =',f10.3/'N = 2^',  &
     &        i5/'No. Gaussian Pairs =',f15.0/'Sums = ',1p,2d25.15/  &
     &        'Counts:'/(i3,0p,f15.0))

      call print_results('EP', class, m+1, 0, 0, nit,  &
     &                   tm, Mops,  &
     &                   'Random numbers generated',  &
     &                   verified, npbversion, compiletime, cs1,  &
     &                   cs2, cs3, cs4, cs5, cs6, cs7)


      if (timers_enabled) then
         if (tm .le. 0.d0) tm = 1.0
         tt = timer_read(1)
         print 810, 'Total time:    ', tt, tt*100./tm
         tt = timer_read(2)
         print 810, 'Gaussian pairs:', tt, tt*100./tm
         tt = timer_read(3)
         print 810, 'Random numbers:', tt, tt*100./tm
810      format(1x,a,f9.3,' (',f6.2,'%)')
      endif


      end
