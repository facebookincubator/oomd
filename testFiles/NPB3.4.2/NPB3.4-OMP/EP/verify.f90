!---------------------------------------------------------------------
      subroutine verify(m, sx, sy, gc, verified, class)
!---------------------------------------------------------------------

      use, intrinsic :: ieee_arithmetic, only : ieee_is_nan

      implicit none
      integer m
      double precision sx, sy, gc
      logical verified
      character class

      double precision sx_verify_value, sy_verify_value
      double precision gc_verify_value
      double precision sx_err, sy_err, gc_err

      double precision, parameter :: epsilon = 1.d-8

      verified = .true.
      if (m.eq.24) then
         class = 'S'
         sx_verify_value = 1.051299420395306D+07
         sy_verify_value = 1.051517131857535D+07
         gc_verify_value = 13176389.D0
      elseif (m.eq.25) then
         class = 'W'
         sx_verify_value = 2.102505525182392D+07
         sy_verify_value = 2.103162209578822D+07
         gc_verify_value = 26354769.D0
      elseif (m.eq.28) then
         class = 'A'
         sx_verify_value = 1.682235632304711D+08
         sy_verify_value = 1.682195123368299D+08
         gc_verify_value = 210832767.D0
      elseif (m.eq.30) then
         class = 'B'
         sx_verify_value = 6.728927543423024D+08
         sy_verify_value = 6.728951822504275D+08
         gc_verify_value = 843345606.D0
      elseif (m.eq.32) then
         class = 'C'
         sx_verify_value = 2.691444083862931D+09
         sy_verify_value = 2.691519118724585D+09
         gc_verify_value = 3373275903.D0
      elseif (m.eq.36) then
         class = 'D'
         sx_verify_value = 4.306350280812112D+10
         sy_verify_value = 4.306347571859157D+10
         gc_verify_value = 53972171957.D0
      elseif (m.eq.40) then
         class = 'E'
         sx_verify_value = 6.890169663167274D+11
         sy_verify_value = 6.890164670688535D+11
         gc_verify_value = 863554308186.D0
      elseif (m.eq.44) then
         class = 'F'
         sx_verify_value = 1.102426773788175D+13
         sy_verify_value = 1.102426773787993D+13
         gc_verify_value = 13816870608324.D0
      else
         class = 'U'
         verified = .false.
      endif
      if (verified) then
         sx_err = abs((sx - sx_verify_value)/sx_verify_value)
         sy_err = abs((sy - sy_verify_value)/sy_verify_value)
         if (ieee_is_nan(sx_err) .or. ieee_is_nan(sy_err)) then
            verified = .false.
         else
            verified = ((sx_err.le.epsilon) .and. (sy_err.le.epsilon))
         endif
      endif
      if (verified) then
         gc_err = abs((gc - gc_verify_value)/gc_verify_value)
         if (ieee_is_nan(gc_err) .or. gc_err.gt.epsilon) then
            verified = .false.
         endif
      endif

      return
      end

