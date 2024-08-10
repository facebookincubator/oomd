!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine buts( ldmx, ldmy, ldmz,  &
     &                 nx, ny, nz,  &
     &                 omega,  &
     &                 v,  &
     &                 d, udx, udy, udz,  &
     &                 ist, iend, j, k )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   compute the regular-sparse, block upper triangular solution:
!
!                     v <-- ( U-inv ) * v
!
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer ldmx, ldmy, ldmz
      integer nx, ny, nz
      double precision  omega
!---------------------------------------------------------------------
!   To improve cache performance, second two dimensions padded by 1 
!   for even number sizes only.  Only needed in v.
!---------------------------------------------------------------------
      double precision  v( 5,ldmx/2*2+1, ldmy/2*2+1, ldmz),  &
     &        d  ( 5, 5, ldmx ),  &
     &        udx( 5, 5, ldmx ),  &
     &        udy( 5, 5, ldmx ),  &
     &        udz( 5, 5, ldmx )
      integer ist, iend, j, k

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, m
      double precision  tmp, tmp1
      double precision  tmat(5,5), tv(5)


         do i = iend, ist, -1
            do m = 1, 5
                  tv( m ) =  &
     &      omega * (  udz( m, 1, i ) * v( 1, i, j, k+1 )  &
     &               + udz( m, 2, i ) * v( 2, i, j, k+1 )  &
     &               + udz( m, 3, i ) * v( 3, i, j, k+1 )  &
     &               + udz( m, 4, i ) * v( 4, i, j, k+1 )  &
     &               + udz( m, 5, i ) * v( 5, i, j, k+1 ) )

                  tv( m ) = tv( m )  &
     & + omega * ( udy( m, 1, i ) * v( 1, i, j+1, k )  &
     &           + udx( m, 1, i ) * v( 1, i+1, j, k )  &
     &           + udy( m, 2, i ) * v( 2, i, j+1, k )  &
     &           + udx( m, 2, i ) * v( 2, i+1, j, k )  &
     &           + udy( m, 3, i ) * v( 3, i, j+1, k )  &
     &           + udx( m, 3, i ) * v( 3, i+1, j, k )  &
     &           + udy( m, 4, i ) * v( 4, i, j+1, k )  &
     &           + udx( m, 4, i ) * v( 4, i+1, j, k )  &
     &           + udy( m, 5, i ) * v( 5, i, j+1, k )  &
     &           + udx( m, 5, i ) * v( 5, i+1, j, k ) )
            end do

!---------------------------------------------------------------------
!   diagonal block inversion
!---------------------------------------------------------------------
            do m = 1, 5
               tmat( m, 1 ) = d( m, 1, i )
               tmat( m, 2 ) = d( m, 2, i )
               tmat( m, 3 ) = d( m, 3, i )
               tmat( m, 4 ) = d( m, 4, i )
               tmat( m, 5 ) = d( m, 5, i )
            end do

            tmp1 = 1.0d+00 / tmat( 1, 1 )
            tmp = tmp1 * tmat( 2, 1 )
            tmat( 2, 2 ) =  tmat( 2, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 2, 3 ) =  tmat( 2, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 2, 4 ) =  tmat( 2, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 2, 5 ) =  tmat( 2, 5 )  &
     &           - tmp * tmat( 1, 5 )
            tv( 2 ) = tv( 2 )  &
     &        - tv( 1 ) * tmp

            tmp = tmp1 * tmat( 3, 1 )
            tmat( 3, 2 ) =  tmat( 3, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )  &
     &           - tmp * tmat( 1, 5 )
            tv( 3 ) = tv( 3 )  &
     &        - tv( 1 ) * tmp

            tmp = tmp1 * tmat( 4, 1 )
            tmat( 4, 2 ) =  tmat( 4, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 1, 5 )
            tv( 4 ) = tv( 4 )  &
     &        - tv( 1 ) * tmp

            tmp = tmp1 * tmat( 5, 1 )
            tmat( 5, 2 ) =  tmat( 5, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 1, 5 )
            tv( 5 ) = tv( 5 )  &
     &        - tv( 1 ) * tmp



            tmp1 = 1.0d+00 / tmat( 2, 2 )
            tmp = tmp1 * tmat( 3, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )  &
     &           - tmp * tmat( 2, 5 )
            tv( 3 ) = tv( 3 )  &
     &        - tv( 2 ) * tmp

            tmp = tmp1 * tmat( 4, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 2, 5 )
            tv( 4 ) = tv( 4 )  &
     &        - tv( 2 ) * tmp

            tmp = tmp1 * tmat( 5, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 2, 5 )
            tv( 5 ) = tv( 5 )  &
     &        - tv( 2 ) * tmp



            tmp1 = 1.0d+00 / tmat( 3, 3 )
            tmp = tmp1 * tmat( 4, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 3, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 3, 5 )
            tv( 4 ) = tv( 4 )  &
     &        - tv( 3 ) * tmp

            tmp = tmp1 * tmat( 5, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 3, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 3, 5 )
            tv( 5 ) = tv( 5 )  &
     &        - tv( 3 ) * tmp



            tmp1 = 1.0d+00 / tmat( 4, 4 )
            tmp = tmp1 * tmat( 5, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 4, 5 )
            tv( 5 ) = tv( 5 )  &
     &        - tv( 4 ) * tmp

!---------------------------------------------------------------------
!   back substitution
!---------------------------------------------------------------------
            tv( 5 ) = tv( 5 )  &
     &                      / tmat( 5, 5 )

            tv( 4 ) = tv( 4 )  &
     &           - tmat( 4, 5 ) * tv( 5 )
            tv( 4 ) = tv( 4 )  &
     &                      / tmat( 4, 4 )

            tv( 3 ) = tv( 3 )  &
     &           - tmat( 3, 4 ) * tv( 4 )  &
     &           - tmat( 3, 5 ) * tv( 5 )
            tv( 3 ) = tv( 3 )  &
     &                      / tmat( 3, 3 )

            tv( 2 ) = tv( 2 )  &
     &           - tmat( 2, 3 ) * tv( 3 )  &
     &           - tmat( 2, 4 ) * tv( 4 )  &
     &           - tmat( 2, 5 ) * tv( 5 )
            tv( 2 ) = tv( 2 )  &
     &                      / tmat( 2, 2 )

            tv( 1 ) = tv( 1 )  &
     &           - tmat( 1, 2 ) * tv( 2 )  &
     &           - tmat( 1, 3 ) * tv( 3 )  &
     &           - tmat( 1, 4 ) * tv( 4 )  &
     &           - tmat( 1, 5 ) * tv( 5 )
            tv( 1 ) = tv( 1 )  &
     &                      / tmat( 1, 1 )

            v( 1, i, j, k ) = v( 1, i, j, k ) - tv( 1 )
            v( 2, i, j, k ) = v( 2, i, j, k ) - tv( 2 )
            v( 3, i, j, k ) = v( 3, i, j, k ) - tv( 3 )
            v( 4, i, j, k ) = v( 4, i, j, k ) - tv( 4 )
            v( 5, i, j, k ) = v( 5, i, j, k ) - tv( 5 )

        enddo

 
      return
      end
