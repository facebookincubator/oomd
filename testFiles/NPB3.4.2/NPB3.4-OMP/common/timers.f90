!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      module timers

      double precision start(64), elapsed(64)
!$omp threadprivate(start, elapsed)

      double precision, external :: elapsed_time

      end module timers


!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      subroutine timer_clear(n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use timers
      implicit none

      integer n

      elapsed(n) = 0.0
      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine timer_start(n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use timers
      implicit none

      integer n

      start(n) = elapsed_time()

      return
      end
      

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine timer_stop(n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use timers
      implicit none

      integer n

      double precision t, now

      now = elapsed_time()
      t = now - start(n)
      elapsed(n) = elapsed(n) + t

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      double precision function timer_read(n)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use timers
      implicit none

      integer n
      
      timer_read = elapsed(n)

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      double precision function elapsed_time()

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit none
!$    external         omp_get_wtime
!$    double precision omp_get_wtime

      double precision t
      logical          mp

! ... Use the OpenMP timer if we can (via C$ conditional compilation)
      mp = .false.
!$    mp = .true.
!$    t = omp_get_wtime()

      if (.not.mp) then
! This function must measure wall clock time, not CPU time. 
! Since there is no portable timer in Fortran (77)
! we call a routine compiled in C (though the C source may have
! to be tweaked). 
         call wtime(t)
! The following is not ok for "official" results because it reports
! CPU time not wall clock time. It may be useful for developing/testing
! on timeshared Crays, though. 
!        call second(t)
      endif

      elapsed_time = t

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine check_timer_flag( timeron )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit none
      logical timeron

      integer nc, ios
      character(len=20) val

      timeron = .false.

! ... Check environment variable "NPB_TIMER_FLAG"
      call get_environment_variable('NPB_TIMER_FLAG', val, nc, ios)
      if (ios .eq. 0) then
         if (nc .le. 0) then
            timeron = .true.
         else if (val(1:1) .ge. '1' .and. val(1:1) .le. '9') then
            timeron = .true.
         else if (val .eq. 'on' .or. val .eq. 'ON' .or.  &
     &            val .eq. 'yes' .or. val .eq. 'YES' .or.  &
     &            val .eq. 'true' .or. val .eq. 'TRUE') then
            timeron = .true.
         endif

      else

! ... Check if the "timer.flag" file exists
         open (unit=2, file='timer.flag', status='old', iostat=ios)
         if (ios .eq. 0) then
            close(2)
            timeron = .true.
         endif

      endif

      return
      end
