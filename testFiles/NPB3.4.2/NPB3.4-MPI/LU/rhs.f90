
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   compute the right hand sides
!---------------------------------------------------------------------

      use lu_data
      use timing

      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      integer iex
      integer L1, L2
      integer ist1, iend1
      integer jst1, jend1
      double precision  q
      double precision  u21, u31, u41
      double precision  tmp
      double precision  u21i, u31i, u41i, u51i
      double precision  u21j, u31j, u41j, u51j
      double precision  u21k, u31k, u41k, u51k
      double precision  u21im1, u31im1, u41im1, u51im1
      double precision  u21jm1, u31jm1, u41jm1, u51jm1
      double precision  u21km1, u31km1, u41km1, u51km1


      if (timeron) call timer_start(t_rhs)

      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
               do m = 1, 5
                  rsd(m,i,j,k) = - frct(m,i,j,k)
               end do
            end do
         end do
      end do

!---------------------------------------------------------------------
!   xi-direction flux differences
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   iex = flag : iex = 0  north/south communication
!              : iex = 1  east/west communication
!---------------------------------------------------------------------
      iex   = 0

!---------------------------------------------------------------------
!   communicate and receive/send two rows of data
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_exch)
      call exchange_3(u,iex)
      if (timeron) call timer_stop(t_exch)

      L1 = 0
      if (north.eq.-1) L1 = 1
      L2 = nx + 1
      if (south.eq.-1) L2 = nx

      ist1 = 1
      iend1 = nx
      if (north.eq.-1) ist1 = 4
      if (south.eq.-1) iend1 = nx - 3

      do k = 2, nz - 1
         do j = jst, jend
            do i = L1, L2
               flux(1,i,j,k) = u(2,i,j,k)
               u21 = u(2,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)  &
     &                         + u(3,i,j,k) * u(3,i,j,k)  &
     &                         + u(4,i,j,k) * u(4,i,j,k) )  &
     &                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u21 + c2 *  &
     &                        ( u(5,i,j,k) - q )
               flux(3,i,j,k) = u(3,i,j,k) * u21
               flux(4,i,j,k) = u(4,i,j,k) * u21
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u21
            end do

            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)  &
     &                 - tx2 * ( flux(m,i+1,j,k) - flux(m,i-1,j,k) )
               end do
            end do

            do i = ist, L2
               tmp = 1.0d+00 / u(1,i,j,k)

               u21i = tmp * u(2,i,j,k)
               u31i = tmp * u(3,i,j,k)
               u41i = tmp * u(4,i,j,k)
               u51i = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i-1,j,k)

               u21im1 = tmp * u(2,i-1,j,k)
               u31im1 = tmp * u(3,i-1,j,k)
               u41im1 = tmp * u(4,i-1,j,k)
               u51im1 = tmp * u(5,i-1,j,k)

               flux(2,i,j,k) = (4.0d+00/3.0d+00) * tx3 * (u21i-u21im1)
               flux(3,i,j,k) = tx3 * ( u31i - u31im1 )
               flux(4,i,j,k) = tx3 * ( u41i - u41im1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * tx3 * ( ( u21i  **2 + u31i  **2 + u41i  **2 )  &
     &                      - ( u21im1**2 + u31im1**2 + u41im1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * tx3 * ( u21i**2 - u21im1**2 )  &
     &              + c1 * c5 * tx3 * ( u51i - u51im1 )
            end do

            do i = ist, iend
               rsd(1,i,j,k) = rsd(1,i,j,k)  &
     &              + dx1 * tx1 * (            u(1,i-1,j,k)  &
     &                             - 2.0d+00 * u(1,i,j,k)  &
     &                             +           u(1,i+1,j,k) )
               rsd(2,i,j,k) = rsd(2,i,j,k)  &
     &          + tx3 * c3 * c4 * ( flux(2,i+1,j,k) - flux(2,i,j,k) )  &
     &              + dx2 * tx1 * (            u(2,i-1,j,k)  &
     &                             - 2.0d+00 * u(2,i,j,k)  &
     &                             +           u(2,i+1,j,k) )
               rsd(3,i,j,k) = rsd(3,i,j,k)  &
     &          + tx3 * c3 * c4 * ( flux(3,i+1,j,k) - flux(3,i,j,k) )  &
     &              + dx3 * tx1 * (            u(3,i-1,j,k)  &
     &                             - 2.0d+00 * u(3,i,j,k)  &
     &                             +           u(3,i+1,j,k) )
               rsd(4,i,j,k) = rsd(4,i,j,k)  &
     &          + tx3 * c3 * c4 * ( flux(4,i+1,j,k) - flux(4,i,j,k) )  &
     &              + dx4 * tx1 * (            u(4,i-1,j,k)  &
     &                             - 2.0d+00 * u(4,i,j,k)  &
     &                             +           u(4,i+1,j,k) )
               rsd(5,i,j,k) = rsd(5,i,j,k)  &
     &          + tx3 * c3 * c4 * ( flux(5,i+1,j,k) - flux(5,i,j,k) )  &
     &              + dx5 * tx1 * (            u(5,i-1,j,k)  &
     &                             - 2.0d+00 * u(5,i,j,k)  &
     &                             +           u(5,i+1,j,k) )
            end do

!---------------------------------------------------------------------
!   Fourth-order dissipation
!---------------------------------------------------------------------
            IF (north.eq.-1) then
             do m = 1, 5
               rsd(m,2,j,k) = rsd(m,2,j,k)  &
     &           - dssp * ( + 5.0d+00 * u(m,2,j,k)  &
     &                      - 4.0d+00 * u(m,3,j,k)  &
     &                      +           u(m,4,j,k) )
               rsd(m,3,j,k) = rsd(m,3,j,k)  &
     &           - dssp * ( - 4.0d+00 * u(m,2,j,k)  &
     &                      + 6.0d+00 * u(m,3,j,k)  &
     &                      - 4.0d+00 * u(m,4,j,k)  &
     &                      +           u(m,5,j,k) )
             end do
            END IF

            do i = ist1,iend1
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)  &
     &              - dssp * (            u(m,i-2,j,k)  &
     &                        - 4.0d+00 * u(m,i-1,j,k)  &
     &                        + 6.0d+00 * u(m,i,j,k)  &
     &                        - 4.0d+00 * u(m,i+1,j,k)  &
     &                        +           u(m,i+2,j,k) )
               end do
            end do

            IF (south.eq.-1) then
             do m = 1, 5
               rsd(m,nx-2,j,k) = rsd(m,nx-2,j,k)  &
     &           - dssp * (             u(m,nx-4,j,k)  &
     &                      - 4.0d+00 * u(m,nx-3,j,k)  &
     &                      + 6.0d+00 * u(m,nx-2,j,k)  &
     &                      - 4.0d+00 * u(m,nx-1,j,k)  )
               rsd(m,nx-1,j,k) = rsd(m,nx-1,j,k)  &
     &           - dssp * (             u(m,nx-3,j,k)  &
     &                      - 4.0d+00 * u(m,nx-2,j,k)  &
     &                      + 5.0d+00 * u(m,nx-1,j,k) )
             end do
            END IF

         end do
      end do 

!---------------------------------------------------------------------
!   eta-direction flux differences
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   iex = flag : iex = 0  north/south communication
!---------------------------------------------------------------------
      iex   = 1

!---------------------------------------------------------------------
!   communicate and receive/send two rows of data
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_exch)
      call exchange_3(u,iex)
      if (timeron) call timer_stop(t_exch)

      L1 = 0
      if (west.eq.-1) L1 = 1
      L2 = ny + 1
      if (east.eq.-1) L2 = ny

      jst1 = 1
      jend1 = ny
      if (west.eq.-1) jst1 = 4
      if (east.eq.-1) jend1 = ny - 3

      do k = 2, nz - 1
         do j = L1, L2
            do i = ist, iend
               flux(1,i,j,k) = u(3,i,j,k)
               u31 = u(3,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)  &
     &                         + u(3,i,j,k) * u(3,i,j,k)  &
     &                         + u(4,i,j,k) * u(4,i,j,k) )  &
     &                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u31 
               flux(3,i,j,k) = u(3,i,j,k) * u31 + c2 * (u(5,i,j,k)-q)
               flux(4,i,j,k) = u(4,i,j,k) * u31
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u31
            end do
         end do

         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)  &
     &                   - ty2 * ( flux(m,i,j+1,k) - flux(m,i,j-1,k) )
               end do
            end do
         end do

         do j = jst, L2
            do i = ist, iend
               tmp = 1.0d+00 / u(1,i,j,k)

               u21j = tmp * u(2,i,j,k)
               u31j = tmp * u(3,i,j,k)
               u41j = tmp * u(4,i,j,k)
               u51j = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i,j-1,k)
               u21jm1 = tmp * u(2,i,j-1,k)
               u31jm1 = tmp * u(3,i,j-1,k)
               u41jm1 = tmp * u(4,i,j-1,k)
               u51jm1 = tmp * u(5,i,j-1,k)

               flux(2,i,j,k) = ty3 * ( u21j - u21jm1 )
               flux(3,i,j,k) = (4.0d+00/3.0d+00) * ty3 * (u31j-u31jm1)
               flux(4,i,j,k) = ty3 * ( u41j - u41jm1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * ty3 * ( ( u21j  **2 + u31j  **2 + u41j  **2 )  &
     &                      - ( u21jm1**2 + u31jm1**2 + u41jm1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * ty3 * ( u31j**2 - u31jm1**2 )  &
     &              + c1 * c5 * ty3 * ( u51j - u51jm1 )
            end do
         end do

         do j = jst, jend
            do i = ist, iend

               rsd(1,i,j,k) = rsd(1,i,j,k)  &
     &              + dy1 * ty1 * (            u(1,i,j-1,k)  &
     &                             - 2.0d+00 * u(1,i,j,k)  &
     &                             +           u(1,i,j+1,k) )

               rsd(2,i,j,k) = rsd(2,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(2,i,j+1,k) - flux(2,i,j,k) )  &
     &              + dy2 * ty1 * (            u(2,i,j-1,k)  &
     &                             - 2.0d+00 * u(2,i,j,k)  &
     &                             +           u(2,i,j+1,k) )

               rsd(3,i,j,k) = rsd(3,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(3,i,j+1,k) - flux(3,i,j,k) )  &
     &              + dy3 * ty1 * (            u(3,i,j-1,k)  &
     &                             - 2.0d+00 * u(3,i,j,k)  &
     &                             +           u(3,i,j+1,k) )

               rsd(4,i,j,k) = rsd(4,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(4,i,j+1,k) - flux(4,i,j,k) )  &
     &              + dy4 * ty1 * (            u(4,i,j-1,k)  &
     &                             - 2.0d+00 * u(4,i,j,k)  &
     &                             +           u(4,i,j+1,k) )

               rsd(5,i,j,k) = rsd(5,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(5,i,j+1,k) - flux(5,i,j,k) )  &
     &              + dy5 * ty1 * (            u(5,i,j-1,k)  &
     &                             - 2.0d+00 * u(5,i,j,k)  &
     &                             +           u(5,i,j+1,k) )

            end do
         end do

!---------------------------------------------------------------------
!   fourth-order dissipation
!---------------------------------------------------------------------
         IF (west.eq.-1) then
            do i = ist, iend
             do m = 1, 5
               rsd(m,i,2,k) = rsd(m,i,2,k)  &
     &           - dssp * ( + 5.0d+00 * u(m,i,2,k)  &
     &                      - 4.0d+00 * u(m,i,3,k)  &
     &                      +           u(m,i,4,k) )
               rsd(m,i,3,k) = rsd(m,i,3,k)  &
     &           - dssp * ( - 4.0d+00 * u(m,i,2,k)  &
     &                      + 6.0d+00 * u(m,i,3,k)  &
     &                      - 4.0d+00 * u(m,i,4,k)  &
     &                      +           u(m,i,5,k) )
             end do
            end do
         END IF

         do j = jst1, jend1
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)  &
     &              - dssp * (            u(m,i,j-2,k)  &
     &                        - 4.0d+00 * u(m,i,j-1,k)  &
     &                        + 6.0d+00 * u(m,i,j,k)  &
     &                        - 4.0d+00 * u(m,i,j+1,k)  &
     &                        +           u(m,i,j+2,k) )
               end do
            end do
         end do

         IF (east.eq.-1) then
            do i = ist, iend
             do m = 1, 5
               rsd(m,i,ny-2,k) = rsd(m,i,ny-2,k)  &
     &           - dssp * (             u(m,i,ny-4,k)  &
     &                      - 4.0d+00 * u(m,i,ny-3,k)  &
     &                      + 6.0d+00 * u(m,i,ny-2,k)  &
     &                      - 4.0d+00 * u(m,i,ny-1,k)  )
               rsd(m,i,ny-1,k) = rsd(m,i,ny-1,k)  &
     &           - dssp * (             u(m,i,ny-3,k)  &
     &                      - 4.0d+00 * u(m,i,ny-2,k)  &
     &                      + 5.0d+00 * u(m,i,ny-1,k) )
             end do
            end do
         END IF

      end do

!---------------------------------------------------------------------
!   zeta-direction flux differences
!---------------------------------------------------------------------
      do k = 1, nz
         do j = jst, jend
            do i = ist, iend
               flux(1,i,j,k) = u(4,i,j,k)
               u41 = u(4,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)  &
     &                         + u(3,i,j,k) * u(3,i,j,k)  &
     &                         + u(4,i,j,k) * u(4,i,j,k) )  &
     &                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u41 
               flux(3,i,j,k) = u(3,i,j,k) * u41 
               flux(4,i,j,k) = u(4,i,j,k) * u41 + c2 * (u(5,i,j,k)-q)
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u41
            end do
         end do
      end do

      do k = 2, nz - 1
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)  &
     &                - tz2 * ( flux(m,i,j,k+1) - flux(m,i,j,k-1) )
               end do
            end do
         end do
      end do

      do k = 2, nz
         do j = jst, jend
            do i = ist, iend
               tmp = 1.0d+00 / u(1,i,j,k)

               u21k = tmp * u(2,i,j,k)
               u31k = tmp * u(3,i,j,k)
               u41k = tmp * u(4,i,j,k)
               u51k = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i,j,k-1)

               u21km1 = tmp * u(2,i,j,k-1)
               u31km1 = tmp * u(3,i,j,k-1)
               u41km1 = tmp * u(4,i,j,k-1)
               u51km1 = tmp * u(5,i,j,k-1)

               flux(2,i,j,k) = tz3 * ( u21k - u21km1 )
               flux(3,i,j,k) = tz3 * ( u31k - u31km1 )
               flux(4,i,j,k) = (4.0d+00/3.0d+00) * tz3 * (u41k-u41km1)
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * tz3 * ( ( u21k  **2 + u31k  **2 + u41k  **2 )  &
     &                      - ( u21km1**2 + u31km1**2 + u41km1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * tz3 * ( u41k**2 - u41km1**2 )  &
     &              + c1 * c5 * tz3 * ( u51k - u51km1 )
            end do
         end do
      end do

      do k = 2, nz - 1
         do j = jst, jend
            do i = ist, iend
               rsd(1,i,j,k) = rsd(1,i,j,k)  &
     &              + dz1 * tz1 * (            u(1,i,j,k-1)  &
     &                             - 2.0d+00 * u(1,i,j,k)  &
     &                             +           u(1,i,j,k+1) )
               rsd(2,i,j,k) = rsd(2,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(2,i,j,k+1) - flux(2,i,j,k) )  &
     &              + dz2 * tz1 * (            u(2,i,j,k-1)  &
     &                             - 2.0d+00 * u(2,i,j,k)  &
     &                             +           u(2,i,j,k+1) )
               rsd(3,i,j,k) = rsd(3,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(3,i,j,k+1) - flux(3,i,j,k) )  &
     &              + dz3 * tz1 * (            u(3,i,j,k-1)  &
     &                             - 2.0d+00 * u(3,i,j,k)  &
     &                             +           u(3,i,j,k+1) )
               rsd(4,i,j,k) = rsd(4,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(4,i,j,k+1) - flux(4,i,j,k) )  &
     &              + dz4 * tz1 * (            u(4,i,j,k-1)  &
     &                             - 2.0d+00 * u(4,i,j,k)  &
     &                             +           u(4,i,j,k+1) )
               rsd(5,i,j,k) = rsd(5,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(5,i,j,k+1) - flux(5,i,j,k) )  &
     &              + dz5 * tz1 * (            u(5,i,j,k-1)  &
     &                             - 2.0d+00 * u(5,i,j,k)  &
     &                             +           u(5,i,j,k+1) )
            end do
         end do
      end do

!---------------------------------------------------------------------
!   fourth-order dissipation
!---------------------------------------------------------------------
      do j = jst, jend
         do i = ist, iend
            do m = 1, 5
               rsd(m,i,j,2) = rsd(m,i,j,2)  &
     &           - dssp * ( + 5.0d+00 * u(m,i,j,2)  &
     &                      - 4.0d+00 * u(m,i,j,3)  &
     &                      +           u(m,i,j,4) )
               rsd(m,i,j,3) = rsd(m,i,j,3)  &
     &           - dssp * ( - 4.0d+00 * u(m,i,j,2)  &
     &                      + 6.0d+00 * u(m,i,j,3)  &
     &                      - 4.0d+00 * u(m,i,j,4)  &
     &                      +           u(m,i,j,5) )
            end do
         end do
      end do

      do k = 4, nz - 3
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)  &
     &              - dssp * (            u(m,i,j,k-2)  &
     &                        - 4.0d+00 * u(m,i,j,k-1)  &
     &                        + 6.0d+00 * u(m,i,j,k)  &
     &                        - 4.0d+00 * u(m,i,j,k+1)  &
     &                        +           u(m,i,j,k+2) )
               end do
            end do
         end do
      end do

      do j = jst, jend
         do i = ist, iend
            do m = 1, 5
               rsd(m,i,j,nz-2) = rsd(m,i,j,nz-2)  &
     &           - dssp * (             u(m,i,j,nz-4)  &
     &                      - 4.0d+00 * u(m,i,j,nz-3)  &
     &                      + 6.0d+00 * u(m,i,j,nz-2)  &
     &                      - 4.0d+00 * u(m,i,j,nz-1)  )
               rsd(m,i,j,nz-1) = rsd(m,i,j,nz-1)  &
     &           - dssp * (             u(m,i,j,nz-3)  &
     &                      - 4.0d+00 * u(m,i,j,nz-2)  &
     &                      + 5.0d+00 * u(m,i,j,nz-1) )
            end do
         end do
      end do

      if (timeron) call timer_stop(t_rhs)

      return
      end
