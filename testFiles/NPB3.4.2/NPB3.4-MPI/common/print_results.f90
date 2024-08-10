
      subroutine print_results(name, class, n1, n2, n3, niter,  &
     &               nprocs_active, nprocs_total,  &
     &               t, mops, optype, verified, npbversion,  &
     &               compiletime, cs1, cs2, cs3, cs4, cs5, cs6, cs7)
      
      implicit none
      character name*2
      character class
      integer n1, n2, n3, niter, nprocs_active, nprocs_total, j
      double precision t, mops
      character optype*24, size*15
      logical verified
      character(len=*) npbversion, compiletime,  &
     &              cs1, cs2, cs3, cs4, cs5, cs6, cs7

         write (*, 2) name 
 2       format(//, ' ', A2, ' Benchmark Completed.')

         write (*, 3) Class
 3       format(' Class           = ', 12x, a12)

!   If this is not a grid-based problem (EP, FT, CG), then
!   we only print n1, which contains some measure of the
!   problem size. In that case, n2 and n3 are both zero.
!   Otherwise, we print the grid size n1xn2xn3

         if ((n2 .eq. 0) .and. (n3 .eq. 0)) then
            if (name(1:2) .eq. 'EP') then
               write(size, '(f15.0)' ) 2.d0**n1
               j = 15
               if (size(j:j) .eq. '.') j = j - 1
               write (*,42) size(1:j)
 42            format(' Size            = ',9x, a15)
            else
               write (*,44) n1
 44            format(' Size            = ',12x, i12)
            endif
         else
            write (*, 4) n1,n2,n3
 4          format(' Size            =  ',9x, i4,'x',i4,'x',i4)
         endif

         write (*, 5) niter
 5       format(' Iterations      = ', 12x, i12)
         
         write (*, 6) t
 6       format(' Time in seconds = ', 12x, f12.2)
         
         write (*,7) nprocs_total
 7       format(' Total processes = ', 12x, i12)
         
         if (nprocs_active .ne. 0) write (*,8) nprocs_active
 8       format(' Active processes= ', 12x, i12)

         write (*,9) mops
 9       format(' Mop/s total     = ', 12x, f12.2)

         write (*,10) mops/dble( nprocs_total )
 10      format(' Mop/s/process   = ', 12x, f12.2)        
         
         write(*, 11) optype
 11      format(' Operation type  = ', a24)

         if (verified) then 
            write(*,12) '  SUCCESSFUL'
         else
            write(*,12) 'UNSUCCESSFUL'
         endif
 12      format(' Verification    = ', 12x, a)

         write(*,13) npbversion
 13      format(' Version         = ', 12x, a12)

         write(*,14) compiletime
 14      format(' Compile date    = ', 12x, a12)


         write (*,121) cs1
 121     format(/, ' Compile options:', /,  &
     &          '    MPIFC        = ', A)

         write (*,122) cs2
 122     format('    FLINK        = ', A)

         write (*,123) cs3
 123     format('    FMPI_LIB     = ', A)

         write (*,124) cs4
 124     format('    FMPI_INC     = ', A)

         write (*,125) cs5
 125     format('    FFLAGS       = ', A)

         write (*,126) cs6
 126     format('    FLINKFLAGS   = ', A)

         write(*, 127) cs7
 127     format('    RAND         = ', A)
        
         write (*,130)
 130     format(//' Please send feedbacks and/or',  &
     &            ' the results of this run to:'//  &
     &            ' NPB Development Team '/  &
     &            ' Internet: npb@nas.nasa.gov'//)
! 130     format(//' Please send the results of this run to:'//
!     >            ' NPB Development Team '/
!     >            ' Internet: npb@nas.nasa.gov'/
!     >            ' '/
!     >            ' If email is not available, send this to:'//
!     >            ' MS T27A-1'/
!     >            ' NASA Ames Research Center'/
!     >            ' Moffett Field, CA  94035-1000'//
!     >            ' Fax: 650-604-3957'//)


         return
         end

