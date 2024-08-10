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
      integer(kind=8) Lx, La, a1, a2, x1, x2, xa
      double precision d2m46
      parameter(d2m46=0.5d0**46)

      Lx = x
      La = A
      a1 = ibits(La, 23, 23)
      a2 = ibits(La, 0, 23)
      x1 = ibits(Lx, 23, 23)
      x2 = ibits(Lx, 0, 23)
      xa = ishft(ibits(a1*x2+a2*x1, 0, 23), 23) + a2*x2
      Lx   = ibits(xa,0, 46)
      x    = dble(Lx)
      randlc = d2m46*x
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------


      SUBROUTINE VRANLC (N, X, A, Y)
      implicit none
      integer n, i
      double precision x, a, y(*)
      integer(kind=8) Lx, La, a1, a2, x1, x2, xa
      double precision d2m46
      parameter(d2m46=0.5d0**46)

      Lx = X
      La = A
      a1 = ibits(La, 23, 23)
      a2 = ibits(La, 0, 23)
      do i = 1, N
         x1 = ibits(Lx, 23, 23)
         x2 = ibits(Lx, 0, 23)
         xa = ishft(ibits(a1*x2+a2*x1, 0, 23), 23) + a2*x2
         Lx   = ibits(xa,0, 46)
         y(i) = d2m46*dble(Lx)
      end do
      x = dble(Lx)
      return
      end

