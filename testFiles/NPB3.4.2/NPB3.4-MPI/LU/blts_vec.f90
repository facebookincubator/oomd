!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine blts ( ldmx, ldmy, ldmz,  &
     &                  nx, ny, nz, k,  &
     &                  omega,  &
     &                  v,  &
     &                  ldz, ldy, ldx, d,  &
     &                  ist, iend, jst, jend,  &
     &                  nx0, ny0, ipt, jpt)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   compute the regular-sparse, block lower triangular solution:
!
!                     v <-- ( L-inv ) * v
!
!---------------------------------------------------------------------

      use timing
      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer ldmx, ldmy, ldmz
      integer nx, ny, nz
      integer k
      double precision  omega
      double precision  v( 5, -1:ldmx+2, -1:ldmy+2, *),  &
     &        ldz( 5, 5, ldmx, ldmy),  &
     &        ldy( 5, 5, ldmx, ldmy),  &
     &        ldx( 5, 5, ldmx, ldmy),  &
     &        d( 5, 5, ldmx, ldmy)
      integer ist, iend
      integer jst, jend
      integer nx0, ny0
      integer ipt, jpt


!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, m, l, istp, iendp
      integer iex
      double precision  tmp, tmp1
      double precision  tmat(5,5)


!---------------------------------------------------------------------
!   receive data from north and west
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_lcomm)
      iex = 0
      call exchange_1( v,k,iex )
      if (timeron) call timer_stop(t_lcomm)


      if (timeron) call timer_start(t_blts)
      do j = jst, jend
         do i = ist, iend
            do m = 1, 5

                  v( m, i, j, k ) =  v( m, i, j, k )  &
     &    - omega * (  ldz( m, 1, i, j ) * v( 1, i, j, k-1 )  &
     &               + ldz( m, 2, i, j ) * v( 2, i, j, k-1 )  &
     &               + ldz( m, 3, i, j ) * v( 3, i, j, k-1 )  &
     &               + ldz( m, 4, i, j ) * v( 4, i, j, k-1 )  &
     &               + ldz( m, 5, i, j ) * v( 5, i, j, k-1 )  )

            end do
         end do
      end do


      do l = ist+jst, iend+jend
         istp  = max(l - jend, ist)
         iendp = min(l - jst, iend)

!dir$ ivdep
         do i = istp, iendp
            j = l - i

!!dir$ unroll 5
!   manually unroll the loop
!            do m = 1, 5

                  v( 1, i, j, k ) =  v( 1, i, j, k )  &
     & - omega * ( ldy( 1, 1, i, j ) * v( 1, i, j-1, k )  &
     &           + ldx( 1, 1, i, j ) * v( 1, i-1, j, k )  &
     &           + ldy( 1, 2, i, j ) * v( 2, i, j-1, k )  &
     &           + ldx( 1, 2, i, j ) * v( 2, i-1, j, k )  &
     &           + ldy( 1, 3, i, j ) * v( 3, i, j-1, k )  &
     &           + ldx( 1, 3, i, j ) * v( 3, i-1, j, k )  &
     &           + ldy( 1, 4, i, j ) * v( 4, i, j-1, k )  &
     &           + ldx( 1, 4, i, j ) * v( 4, i-1, j, k )  &
     &           + ldy( 1, 5, i, j ) * v( 5, i, j-1, k )  &
     &           + ldx( 1, 5, i, j ) * v( 5, i-1, j, k ) )
                  v( 2, i, j, k ) =  v( 2, i, j, k )  &
     & - omega * ( ldy( 2, 1, i, j ) * v( 1, i, j-1, k )  &
     &           + ldx( 2, 1, i, j ) * v( 1, i-1, j, k )  &
     &           + ldy( 2, 2, i, j ) * v( 2, i, j-1, k )  &
     &           + ldx( 2, 2, i, j ) * v( 2, i-1, j, k )  &
     &           + ldy( 2, 3, i, j ) * v( 3, i, j-1, k )  &
     &           + ldx( 2, 3, i, j ) * v( 3, i-1, j, k )  &
     &           + ldy( 2, 4, i, j ) * v( 4, i, j-1, k )  &
     &           + ldx( 2, 4, i, j ) * v( 4, i-1, j, k )  &
     &           + ldy( 2, 5, i, j ) * v( 5, i, j-1, k )  &
     &           + ldx( 2, 5, i, j ) * v( 5, i-1, j, k ) )
                  v( 3, i, j, k ) =  v( 3, i, j, k )  &
     & - omega * ( ldy( 3, 1, i, j ) * v( 1, i, j-1, k )  &
     &           + ldx( 3, 1, i, j ) * v( 1, i-1, j, k )  &
     &           + ldy( 3, 2, i, j ) * v( 2, i, j-1, k )  &
     &           + ldx( 3, 2, i, j ) * v( 2, i-1, j, k )  &
     &           + ldy( 3, 3, i, j ) * v( 3, i, j-1, k )  &
     &           + ldx( 3, 3, i, j ) * v( 3, i-1, j, k )  &
     &           + ldy( 3, 4, i, j ) * v( 4, i, j-1, k )  &
     &           + ldx( 3, 4, i, j ) * v( 4, i-1, j, k )  &
     &           + ldy( 3, 5, i, j ) * v( 5, i, j-1, k )  &
     &           + ldx( 3, 5, i, j ) * v( 5, i-1, j, k ) )
                  v( 4, i, j, k ) =  v( 4, i, j, k )  &
     & - omega * ( ldy( 4, 1, i, j ) * v( 1, i, j-1, k )  &
     &           + ldx( 4, 1, i, j ) * v( 1, i-1, j, k )  &
     &           + ldy( 4, 2, i, j ) * v( 2, i, j-1, k )  &
     &           + ldx( 4, 2, i, j ) * v( 2, i-1, j, k )  &
     &           + ldy( 4, 3, i, j ) * v( 3, i, j-1, k )  &
     &           + ldx( 4, 3, i, j ) * v( 3, i-1, j, k )  &
     &           + ldy( 4, 4, i, j ) * v( 4, i, j-1, k )  &
     &           + ldx( 4, 4, i, j ) * v( 4, i-1, j, k )  &
     &           + ldy( 4, 5, i, j ) * v( 5, i, j-1, k )  &
     &           + ldx( 4, 5, i, j ) * v( 5, i-1, j, k ) )
                  v( 5, i, j, k ) =  v( 5, i, j, k )  &
     & - omega * ( ldy( 5, 1, i, j ) * v( 1, i, j-1, k )  &
     &           + ldx( 5, 1, i, j ) * v( 1, i-1, j, k )  &
     &           + ldy( 5, 2, i, j ) * v( 2, i, j-1, k )  &
     &           + ldx( 5, 2, i, j ) * v( 2, i-1, j, k )  &
     &           + ldy( 5, 3, i, j ) * v( 3, i, j-1, k )  &
     &           + ldx( 5, 3, i, j ) * v( 3, i-1, j, k )  &
     &           + ldy( 5, 4, i, j ) * v( 4, i, j-1, k )  &
     &           + ldx( 5, 4, i, j ) * v( 4, i-1, j, k )  &
     &           + ldy( 5, 5, i, j ) * v( 5, i, j-1, k )  &
     &           + ldx( 5, 5, i, j ) * v( 5, i-1, j, k ) )

!            end do
       
!---------------------------------------------------------------------
!   diagonal block inversion
!
!   forward elimination
!---------------------------------------------------------------------
!!dir$ unroll 5
!   manually unroll the loop
!            do m = 1, 5
               tmat( 1, 1 ) = d( 1, 1, i, j )
               tmat( 1, 2 ) = d( 1, 2, i, j )
               tmat( 1, 3 ) = d( 1, 3, i, j )
               tmat( 1, 4 ) = d( 1, 4, i, j )
               tmat( 1, 5 ) = d( 1, 5, i, j )
               tmat( 2, 1 ) = d( 2, 1, i, j )
               tmat( 2, 2 ) = d( 2, 2, i, j )
               tmat( 2, 3 ) = d( 2, 3, i, j )
               tmat( 2, 4 ) = d( 2, 4, i, j )
               tmat( 2, 5 ) = d( 2, 5, i, j )
               tmat( 3, 1 ) = d( 3, 1, i, j )
               tmat( 3, 2 ) = d( 3, 2, i, j )
               tmat( 3, 3 ) = d( 3, 3, i, j )
               tmat( 3, 4 ) = d( 3, 4, i, j )
               tmat( 3, 5 ) = d( 3, 5, i, j )
               tmat( 4, 1 ) = d( 4, 1, i, j )
               tmat( 4, 2 ) = d( 4, 2, i, j )
               tmat( 4, 3 ) = d( 4, 3, i, j )
               tmat( 4, 4 ) = d( 4, 4, i, j )
               tmat( 4, 5 ) = d( 4, 5, i, j )
               tmat( 5, 1 ) = d( 5, 1, i, j )
               tmat( 5, 2 ) = d( 5, 2, i, j )
               tmat( 5, 3 ) = d( 5, 3, i, j )
               tmat( 5, 4 ) = d( 5, 4, i, j )
               tmat( 5, 5 ) = d( 5, 5, i, j )
!            end do

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
            v( 2, i, j, k ) = v( 2, i, j, k )  &
     &        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 3, 1 )
            tmat( 3, 2 ) =  tmat( 3, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )  &
     &           - tmp * tmat( 1, 5 )
            v( 3, i, j, k ) = v( 3, i, j, k )  &
     &        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 4, 1 )
            tmat( 4, 2 ) =  tmat( 4, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 1, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )  &
     &        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 1 )
            tmat( 5, 2 ) =  tmat( 5, 2 )  &
     &           - tmp * tmat( 1, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )  &
     &           - tmp * tmat( 1, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 1, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 1, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )  &
     &        - v( 1, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 2, 2 )
            tmp = tmp1 * tmat( 3, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )  &
     &           - tmp * tmat( 2, 5 )
            v( 3, i, j, k ) = v( 3, i, j, k )  &
     &        - v( 2, i, j, k ) * tmp

            tmp = tmp1 * tmat( 4, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 2, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )  &
     &        - v( 2, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )  &
     &           - tmp * tmat( 2, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 2, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 2, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )  &
     &        - v( 2, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 3, 3 )
            tmp = tmp1 * tmat( 4, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )  &
     &           - tmp * tmat( 3, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )  &
     &           - tmp * tmat( 3, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )  &
     &        - v( 3, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )  &
     &           - tmp * tmat( 3, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 3, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )  &
     &        - v( 3, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 4, 4 )
            tmp = tmp1 * tmat( 5, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )  &
     &           - tmp * tmat( 4, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )  &
     &        - v( 4, i, j, k ) * tmp

!---------------------------------------------------------------------
!   back substitution
!---------------------------------------------------------------------
            v( 5, i, j, k ) = v( 5, i, j, k )  &
     &                      / tmat( 5, 5 )

            v( 4, i, j, k ) = v( 4, i, j, k )  &
     &           - tmat( 4, 5 ) * v( 5, i, j, k )
            v( 4, i, j, k ) = v( 4, i, j, k )  &
     &                      / tmat( 4, 4 )

            v( 3, i, j, k ) = v( 3, i, j, k )  &
     &           - tmat( 3, 4 ) * v( 4, i, j, k )  &
     &           - tmat( 3, 5 ) * v( 5, i, j, k )
            v( 3, i, j, k ) = v( 3, i, j, k )  &
     &                      / tmat( 3, 3 )

            v( 2, i, j, k ) = v( 2, i, j, k )  &
     &           - tmat( 2, 3 ) * v( 3, i, j, k )  &
     &           - tmat( 2, 4 ) * v( 4, i, j, k )  &
     &           - tmat( 2, 5 ) * v( 5, i, j, k )
            v( 2, i, j, k ) = v( 2, i, j, k )  &
     &                      / tmat( 2, 2 )

            v( 1, i, j, k ) = v( 1, i, j, k )  &
     &           - tmat( 1, 2 ) * v( 2, i, j, k )  &
     &           - tmat( 1, 3 ) * v( 3, i, j, k )  &
     &           - tmat( 1, 4 ) * v( 4, i, j, k )  &
     &           - tmat( 1, 5 ) * v( 5, i, j, k )
            v( 1, i, j, k ) = v( 1, i, j, k )  &
     &                      / tmat( 1, 1 )


        enddo
      enddo
      if (timeron) call timer_stop(t_blts)

!---------------------------------------------------------------------
!   send data to east and south
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_lcomm)
      iex = 2
      call exchange_1( v,k,iex )
      if (timeron) call timer_stop(t_lcomm)

      return
      end


