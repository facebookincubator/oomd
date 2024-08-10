      subroutine verify(class, verified)

      use, intrinsic :: ieee_arithmetic, only : ieee_is_nan

      use ua_data

      implicit none

      double precision norm, calc_norm, epsilon, norm_dif, norm_ref
      external         calc_norm
      character        class
      logical          verified
       
!.....tolerance level
      epsilon = 1.0d-08

!.....compute the temperature integral over the whole domain
      norm = calc_norm()

      verified = .true.
      if     ( class .eq. 'S' ) then
        norm_ref = 0.1890013110962D-02
      elseif ( class .eq. 'W' ) then
        norm_ref = 0.2569794837076D-04
      elseif ( class .eq. 'A' ) then
        norm_ref = 0.8939996281443D-04
      elseif ( class .eq. 'B' ) then
        norm_ref = 0.4507561922901D-04
      elseif ( class .eq. 'C' ) then
        norm_ref = 0.1544736587100D-04
      elseif ( class .eq. 'D' ) then
        norm_ref = 0.1577586272355D-05
      else
        class = 'U'
        norm_ref = 1.d0
        verified = .false.
      endif         

      norm_dif = dabs((norm - norm_ref)/norm_ref)

!---------------------------------------------------------------------
!    Output the comparison of computed results to known cases.
!---------------------------------------------------------------------

      print *

      if (class .ne. 'U') then
         write(*, 1990) class
 1990    format(' Verification being performed for class ', a)
         write (*,2000) epsilon
 2000    format(' accuracy setting for epsilon = ', E20.13)
      else 
         write(*, 1995)
 1995    format(' Unknown class')
      endif

      if (class .ne. 'U') then
         write (*,2001) 
      else
         write (*, 2005)
      endif

 2001 format(' Comparison of temperature integrals')
 2005 format(' Temperature integral')
      if (class .eq. 'U') then
         write(*, 2015) norm
      else if ((.not.ieee_is_nan(norm_dif)) .and.  &
     &         norm_dif .le. epsilon) then
         write (*,2011) norm, norm_ref, norm_dif
      else 
         verified = .false.
         write (*,2010) norm, norm_ref, norm_dif
      endif

 2010 format(' FAILURE: ', E20.13, E20.13, E20.13)
 2011 format('          ', E20.13, E20.13, E20.13)
 2015 format('          ', E20.13)
        
      if (class .eq. 'U') then
        write(*, 2022)
        write(*, 2023)
 2022   format(' No reference values provided')
 2023   format(' No verification performed')
      else if (verified) then
        write(*, 2020)
 2020   format(' Verification Successful')
      else
        write(*, 2021)
 2021   format(' Verification failed')
      endif

      return
      end
