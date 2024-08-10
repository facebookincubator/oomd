      double precision function randlc(x, a)

!---------------------------------------------------------------------
!
!   This routine returns a uniform pseudorandom double precision number in the
!   range (0, 1) by using the linear congruential generator
!
!   x_{k+1} = a x_k  (mod 2^46)
!
!   where 0 < x_k < 2^46 and 0 < a < 2^46.  This scheme generates 2^44 numbers
!   before repeating.  The argument A is the same as 'a' in the above formula,
!   and X is the same as x_0.  A and X must be odd double precision integers
!   in the range (1, 2^46).  The returned value RANDLC is normalized to be
!   between 0 and 1, i.e. RANDLC = 2^(-46) * x_1.  X is updated to contain
!   the new seed x_1, so that subsequent calls to RANDLC using the same
!   arguments will generate a continuous sequence.

      implicit none
      double precision x, a
      integer(kind=8) i246m1, Lx, La
      double precision d2m46

      parameter(d2m46=0.5d0**46)

      parameter(i246m1=INT(Z'00003FFFFFFFFFFF',8))

      Lx = X
      La = A

      Lx   = iand(Lx*La,i246m1)
      randlc = d2m46*dble(Lx)
      x    = dble(Lx)
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------


      SUBROUTINE VRANLC (N, X, A, Y)
      implicit none
      integer n, i
      double precision x, a, y(*)
      integer(kind=8) i246m1, Lx, La
      double precision d2m46

! This doesn't work, because the compiler does the calculation in 32
! bits and overflows. No standard way (without f90 stuff) to specify
! that the rhs should be done in 64 bit arithmetic. 
!      parameter(i246m1=2**46-1)

      parameter(d2m46=0.5d0**46)

      parameter(i246m1=INT(Z'00003FFFFFFFFFFF',8))

      Lx = X
      La = A
      do i = 1, N
         Lx   = iand(Lx*La,i246m1)
         y(i) = d2m46*dble(Lx)
      end do
      x    = dble(Lx)

      return
      end

