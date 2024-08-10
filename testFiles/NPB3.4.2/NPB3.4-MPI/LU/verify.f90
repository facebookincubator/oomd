
!---------------------------------------------------------------------
!---------------------------------------------------------------------

        subroutine set_class(class)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  set problem class based on problem size
!---------------------------------------------------------------------

        use lu_data
        implicit none

        character class


        if ( (nx0  .eq. 12     ) .and.  &
     &       (ny0  .eq. 12     ) .and.  &
     &       (nz0  .eq. 12     ) .and.  &
     &       (itmax   .eq. 50    ))  then

           class = 'S'

        elseif ( (nx0 .eq. 33) .and.  &
     &           (ny0 .eq. 33) .and.  &
     &           (nz0 .eq. 33) .and.  &
     &           (itmax  .eq. 300) ) then

           class = 'W'   !SPEC95fp size

        elseif ( (nx0 .eq. 64) .and.  &
     &           (ny0 .eq. 64) .and.  &
     &           (nz0 .eq. 64) .and.  &
     &           (itmax  .eq. 250) ) then

           class = 'A'

        elseif ( (nx0 .eq. 102) .and.  &
     &           (ny0 .eq. 102) .and.  &
     &           (nz0 .eq. 102) .and.  &
     &           (itmax  .eq. 250) ) then

           class = 'B'

        elseif ( (nx0 .eq. 162) .and.  &
     &           (ny0 .eq. 162) .and.  &
     &           (nz0 .eq. 162) .and.  &
     &           (itmax  .eq. 250) ) then

           class = 'C'

        elseif ( (nx0 .eq. 408) .and.  &
     &           (ny0 .eq. 408) .and.  &
     &           (nz0 .eq. 408) .and.  &
     &           (itmax  .eq. 300) ) then

           class = 'D'

        elseif ( (nx0 .eq. 1020) .and.  &
     &           (ny0 .eq. 1020) .and.  &
     &           (nz0 .eq. 1020) .and.  &
     &           (itmax  .eq. 300) ) then

           class = 'E'

        elseif ( (nx0 .eq. 2560) .and.  &
     &           (ny0 .eq. 2560) .and.  &
     &           (nz0 .eq. 2560) .and.  &
     &           (itmax  .eq. 300) ) then

           class = 'F'

        else

           class = 'U'

        endif

        return
        end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

        subroutine verify(xcr, xce, xci, class, verified)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!  verification routine                         
!---------------------------------------------------------------------

        use, intrinsic :: ieee_arithmetic, only : ieee_is_nan

        use lu_data
        implicit none

        double precision xcr(5), xce(5), xci
        double precision xcrref(5),xceref(5),xciref,  &
     &                   xcrdif(5),xcedif(5),xcidif,  &
     &                   epsilon, dtref
        integer m
        character class
        logical verified

!---------------------------------------------------------------------
!   tolerance level
!---------------------------------------------------------------------
        epsilon = 1.0d-08

        verified = .true.

        do m = 1,5
           xcrref(m) = 1.0
           xceref(m) = 1.0
        end do
        xciref = 1.0

        if ( class .eq. 'S' ) then

           dtref = 5.0d-1
!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (12X12X12) grid,
!   after 50 time steps, with  DT = 5.0d-01
!---------------------------------------------------------------------
         xcrref(1) = 1.6196343210976702d-02
         xcrref(2) = 2.1976745164821318d-03
         xcrref(3) = 1.5179927653399185d-03
         xcrref(4) = 1.5029584435994323d-03
         xcrref(5) = 3.4264073155896461d-02

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (12X12X12) grid,
!   after 50 time steps, with  DT = 5.0d-01
!---------------------------------------------------------------------
         xceref(1) = 6.4223319957960924d-04
         xceref(2) = 8.4144342047347926d-05
         xceref(3) = 5.8588269616485186d-05
         xceref(4) = 5.8474222595157350d-05
         xceref(5) = 1.3103347914111294d-03

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (12X12X12) grid,
!   after 50 time steps, with DT = 5.0d-01
!---------------------------------------------------------------------
         xciref = 7.8418928865937083d+00


        elseif ( class .eq. 'W' ) then

           dtref = 1.5d-3
!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (33x33x33) grid,
!   after 300 time steps, with  DT = 1.5d-3
!---------------------------------------------------------------------
           xcrref(1) =   0.1236511638192d+02
           xcrref(2) =   0.1317228477799d+01
           xcrref(3) =   0.2550120713095d+01
           xcrref(4) =   0.2326187750252d+01
           xcrref(5) =   0.2826799444189d+02


!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (33X33X33) grid,
!---------------------------------------------------------------------
           xceref(1) =   0.4867877144216d+00
           xceref(2) =   0.5064652880982d-01
           xceref(3) =   0.9281818101960d-01
           xceref(4) =   0.8570126542733d-01
           xceref(5) =   0.1084277417792d+01


!---------------------------------------------------------------------
!   Reference value of surface integral, for the (33X33X33) grid,
!   after 300 time steps, with  DT = 1.5d-3
!---------------------------------------------------------------------
           xciref    =   0.1161399311023d+02

        elseif ( class .eq. 'A' ) then

           dtref = 2.0d+0
!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (64X64X64) grid,
!   after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xcrref(1) = 7.7902107606689367d+02
         xcrref(2) = 6.3402765259692870d+01
         xcrref(3) = 1.9499249727292479d+02
         xcrref(4) = 1.7845301160418537d+02
         xcrref(5) = 1.8384760349464247d+03

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (64X64X64) grid,
!   after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xceref(1) = 2.9964085685471943d+01
         xceref(2) = 2.8194576365003349d+00
         xceref(3) = 7.3473412698774742d+00
         xceref(4) = 6.7139225687777051d+00
         xceref(5) = 7.0715315688392578d+01

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (64X64X64) grid,
!   after 250 time steps, with DT = 2.0d+00
!---------------------------------------------------------------------
         xciref = 2.6030925604886277d+01


        elseif ( class .eq. 'B' ) then

           dtref = 2.0d+0

!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (102X102X102) grid,
!   after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xcrref(1) = 3.5532672969982736d+03
         xcrref(2) = 2.6214750795310692d+02
         xcrref(3) = 8.8333721850952190d+02
         xcrref(4) = 7.7812774739425265d+02
         xcrref(5) = 7.3087969592545314d+03

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (102X102X102) 
!   grid, after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xceref(1) = 1.1401176380212709d+02
         xceref(2) = 8.1098963655421574d+00
         xceref(3) = 2.8480597317698308d+01
         xceref(4) = 2.5905394567832939d+01
         xceref(5) = 2.6054907504857413d+02

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (102X102X102) grid,
!   after 250 time steps, with DT = 2.0d+00
!---------------------------------------------------------------------
         xciref = 4.7887162703308227d+01

        elseif ( class .eq. 'C' ) then

           dtref = 2.0d+0

!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (162X162X162) grid,
!   after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xcrref(1) = 1.03766980323537846d+04
         xcrref(2) = 8.92212458801008552d+02
         xcrref(3) = 2.56238814582660871d+03
         xcrref(4) = 2.19194343857831427d+03
         xcrref(5) = 1.78078057261061185d+04

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (162X162X162) 
!   grid, after 250 time steps, with  DT = 2.0d+00
!---------------------------------------------------------------------
         xceref(1) = 2.15986399716949279d+02
         xceref(2) = 1.55789559239863600d+01
         xceref(3) = 5.41318863077207766d+01
         xceref(4) = 4.82262643154045421d+01
         xceref(5) = 4.55902910043250358d+02

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (162X162X162) grid,
!   after 250 time steps, with DT = 2.0d+00
!---------------------------------------------------------------------
         xciref = 6.66404553572181300d+01

        elseif ( class .eq. 'D' ) then

           dtref = 1.0d+0

!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (408X408X408) grid,
!   after 300 time steps, with  DT = 1.0d+00
!---------------------------------------------------------------------
         xcrref(1) = 0.4868417937025d+05
         xcrref(2) = 0.4696371050071d+04
         xcrref(3) = 0.1218114549776d+05 
         xcrref(4) = 0.1033801493461d+05
         xcrref(5) = 0.7142398413817d+05

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (408X408X408) 
!   grid, after 300 time steps, with  DT = 1.0d+00
!---------------------------------------------------------------------
         xceref(1) = 0.3752393004482d+03
         xceref(2) = 0.3084128893659d+02
         xceref(3) = 0.9434276905469d+02
         xceref(4) = 0.8230686681928d+02
         xceref(5) = 0.7002620636210d+03

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (408X408X408) grid,
!   after 300 time steps, with DT = 1.0d+00
!---------------------------------------------------------------------
         xciref =    0.8334101392503d+02

        elseif ( class .eq. 'E' ) then

           dtref = 0.5d+0

!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (1020X1020X1020) grid,
!   after 300 time steps, with  DT = 0.5d+00
!---------------------------------------------------------------------
         xcrref(1) = 0.2099641687874d+06
         xcrref(2) = 0.2130403143165d+05
         xcrref(3) = 0.5319228789371d+05 
         xcrref(4) = 0.4509761639833d+05
         xcrref(5) = 0.2932360006590d+06

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (1020X1020X1020) 
!   grid, after 300 time steps, with  DT = 0.5d+00
!---------------------------------------------------------------------
         xceref(1) = 0.4800572578333d+03
         xceref(2) = 0.4221993400184d+02
         xceref(3) = 0.1210851906824d+03
         xceref(4) = 0.1047888986770d+03
         xceref(5) = 0.8363028257389d+03

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (1020X1020X1020) grid,
!   after 300 time steps, with DT = 0.5d+00
!---------------------------------------------------------------------
         xciref =    0.9512163272273d+02

        elseif ( class .eq. 'F' ) then

           dtref = 0.2d+0

!---------------------------------------------------------------------
!   Reference values of RMS-norms of residual, for the (2560X2560X2560) grid,
!   after 300 time steps, with  DT = 0.2d+00
!---------------------------------------------------------------------
         xcrref(1) = 0.8505125358152d+06
         xcrref(2) = 0.8774655318044d+05
         xcrref(3) = 0.2167258198851d+06
         xcrref(4) = 0.1838245257371d+06
         xcrref(5) = 0.1175556512415d+07

!---------------------------------------------------------------------
!   Reference values of RMS-norms of solution error, for the (2560X2560X2560)
!   grid, after 300 time steps, with  DT = 0.2d+00
!---------------------------------------------------------------------
         xceref(1) = 0.5293914132486d+03
         xceref(2) = 0.4784861621068d+02
         xceref(3) = 0.1337701281659d+03
         xceref(4) = 0.1154215049655d+03
         xceref(5) = 0.8956266851467d+03

!---------------------------------------------------------------------
!   Reference value of surface integral, for the (2560X2560X2560) grid,
!   after 300 time steps, with DT = 0.2d+00
!---------------------------------------------------------------------
         xciref =    0.1002509436546d+03

        else

           verified = .FALSE.

        endif

!---------------------------------------------------------------------
!    verification test for residuals if gridsize is one of 
!    the defined grid sizes above (class .ne. 'U')
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!    Compute the difference of solution values and the known reference values.
!---------------------------------------------------------------------
        do m = 1, 5
           
           xcrdif(m) = dabs((xcr(m)-xcrref(m))/xcrref(m)) 
           xcedif(m) = dabs((xce(m)-xceref(m))/xceref(m))
           
        enddo
        xcidif = dabs((xci - xciref)/xciref)


!---------------------------------------------------------------------
!    Output the comparison of computed results to known cases.
!---------------------------------------------------------------------

        if (class .ne. 'U') then
           write(*, 1990) class
 1990      format(/, ' Verification being performed for class ', a)
           write (*,2000) epsilon
 2000      format(' Accuracy setting for epsilon = ', E20.13)
           verified = (dabs(dt-dtref) .le. epsilon)
           if (.not.verified) then  
              class = 'U'
              write (*,1000) dtref
 1000         format(' DT does not match the reference value of ',  &
     &                 E15.8)
           endif
        else 
           write(*, 1995)
 1995      format(' Unknown class')
        endif


        if (class .ne. 'U') then
           write (*,2001) 
        else
           write (*, 2005)
        endif

 2001   format(' Comparison of RMS-norms of residual')
 2005   format(' RMS-norms of residual')
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xcr(m)
           else if ((.not.ieee_is_nan(xcrdif(m))) .and.  &
     &              xcrdif(m) .le. epsilon) then
              write (*,2011) m,xcr(m),xcrref(m),xcrdif(m)
           else 
              verified = .false.
              write (*,2010) m,xcr(m),xcrref(m),xcrdif(m)
           endif
        enddo

        if (class .ne. 'U') then
           write (*,2002)
        else
           write (*,2006)
        endif
 2002   format(' Comparison of RMS-norms of solution error')
 2006   format(' RMS-norms of solution error')
        
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xce(m)
           else if ((.not.ieee_is_nan(xcedif(m))) .and.  &
     &              xcedif(m) .le. epsilon) then
              write (*,2011) m,xce(m),xceref(m),xcedif(m)
           else
              verified = .false.
              write (*,2010) m,xce(m),xceref(m),xcedif(m)
           endif
        enddo
        
 2010   format(' FAILURE: ', i2, 2x, E20.13, E20.13, E20.13)
 2011   format('          ', i2, 2x, E20.13, E20.13, E20.13)
 2015   format('          ', i2, 2x, E20.13)
        
        if (class .ne. 'U') then
           write (*,2025)
        else
           write (*,2026)
        endif
 2025   format(' Comparison of surface integral')
 2026   format(' Surface integral')


        if (class .eq. 'U') then
           write(*, 2030) xci
        else if (xcidif .le. epsilon) then
           write(*, 2032) xci, xciref, xcidif
        else
           verified = .false.
           write(*, 2031) xci, xciref, xcidif
        endif

 2030   format('          ', 4x, E20.13)
 2031   format(' FAILURE: ', 4x, E20.13, E20.13, E20.13)
 2032   format('          ', 4x, E20.13, E20.13, E20.13)



        if (class .eq. 'U') then
           write(*, 2022)
           write(*, 2023)
 2022      format(' No reference values provided')
 2023      format(' No verification performed')
        else if (verified) then
           write(*, 2020)
 2020      format(' Verification Successful')
        else
           write(*, 2021)
 2021      format(' Verification failed')
        endif

        return


        end
