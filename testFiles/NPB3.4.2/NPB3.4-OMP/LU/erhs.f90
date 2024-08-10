!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine erhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   compute the right hand side based on exact solution
!
!---------------------------------------------------------------------

      use lu_data
      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision  xi, eta, zeta
      double precision  q
      double precision  u21, u31, u41
      double precision  tmp
      double precision  u21i, u31i, u41i, u51i
      double precision  u21j, u31j, u41j, u51j
      double precision  u21k, u31k, u41k, u51k
      double precision  u21im1, u31im1, u41im1, u51im1
      double precision  u21jm1, u31jm1, u41jm1, u51jm1
      double precision  u21km1, u31km1, u41km1, u51km1


!$omp parallel default(shared) private(i,j,k,m,xi,eta,zeta,tmp,q,  &
!$omp&   u51im1,u41im1,u31im1,u21im1,u51i,u41i,u31i,u21i,u21,  &
!$omp&   u51jm1,u41jm1,u31jm1,u21jm1,u51j,u41j,u31j,u21j,u31,  &
!$omp&   u51km1,u41km1,u31km1,u21km1,u51k,u41k,u31k,u21k,u41)
!$omp do schedule(static) collapse(2)
      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
               do m = 1, 5
                  frct( m, i, j, k ) = 0.0d+00
               end do
            end do
         end do
      end do
!$omp end do nowait

!$omp do schedule(static) collapse(2)
      do k = 1, nz
         do j = 1, ny
            zeta = ( dble(k-1) ) / ( nz - 1 )
            eta = ( dble(j-1) ) / ( ny0 - 1 )
            do i = 1, nx
               xi = ( dble(i-1) ) / ( nx0 - 1 )
               do m = 1, 5
                  rsd(m,i,j,k) =  ce(m,1)  &
     &                 + (ce(m,2)  &
     &                 + (ce(m,5)  &
     &                 + (ce(m,8)  &
     &                 +  ce(m,11) * xi) * xi) * xi) * xi  &
     &                 + (ce(m,3)  &
     &                 + (ce(m,6)  &
     &                 + (ce(m,9)  &
     &                 +  ce(m,12) * eta) * eta) * eta) * eta  &
     &                 + (ce(m,4)  &
     &                 + (ce(m,7)  &
     &                 + (ce(m,10)  &
     &                 +  ce(m,13) * zeta) * zeta) * zeta) * zeta
               end do
            end do
         end do
      end do
!$omp end do

!---------------------------------------------------------------------
!   xi-direction flux differences
!---------------------------------------------------------------------

!$omp do schedule(static) collapse(2)
      do k = 2, nz - 1
         do j = jst, jend
            do i = 1, nx
               flux(1,i) = rsd(2,i,j,k)
               u21 = rsd(2,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)  &
     &                         + rsd(3,i,j,k) * rsd(3,i,j,k)  &
     &                         + rsd(4,i,j,k) * rsd(4,i,j,k) )  &
     &                      / rsd(1,i,j,k)
               flux(2,i) = rsd(2,i,j,k) * u21 + c2 *  &
     &                         ( rsd(5,i,j,k) - q )
               flux(3,i) = rsd(3,i,j,k) * u21
               flux(4,i) = rsd(4,i,j,k) * u21
               flux(5,i) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u21
            end do

            do i = ist, iend
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)  &
     &                   - tx2 * ( flux(m,i+1) - flux(m,i-1) )
               end do
            end do
            do i = ist, nx
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21i = tmp * rsd(2,i,j,k)
               u31i = tmp * rsd(3,i,j,k)
               u41i = tmp * rsd(4,i,j,k)
               u51i = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i-1,j,k)

               u21im1 = tmp * rsd(2,i-1,j,k)
               u31im1 = tmp * rsd(3,i-1,j,k)
               u41im1 = tmp * rsd(4,i-1,j,k)
               u51im1 = tmp * rsd(5,i-1,j,k)

               flux(2,i) = (4.0d+00/3.0d+00) * tx3 *  &
     &                        ( u21i - u21im1 )
               flux(3,i) = tx3 * ( u31i - u31im1 )
               flux(4,i) = tx3 * ( u41i - u41im1 )
               flux(5,i) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * tx3 * ( ( u21i  **2 + u31i  **2 + u41i  **2 )  &
     &                      - ( u21im1**2 + u31im1**2 + u41im1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * tx3 * ( u21i**2 - u21im1**2 )  &
     &              + c1 * c5 * tx3 * ( u51i - u51im1 )
            end do

            do i = ist, iend
               frct(1,i,j,k) = frct(1,i,j,k)  &
     &              + dx1 * tx1 * (            rsd(1,i-1,j,k)  &
     &                             - 2.0d+00 * rsd(1,i,j,k)  &
     &                             +           rsd(1,i+1,j,k) )
               frct(2,i,j,k) = frct(2,i,j,k)  &
     &           + tx3 * c3 * c4 * ( flux(2,i+1) - flux(2,i) )  &
     &              + dx2 * tx1 * (            rsd(2,i-1,j,k)  &
     &                             - 2.0d+00 * rsd(2,i,j,k)  &
     &                             +           rsd(2,i+1,j,k) )
               frct(3,i,j,k) = frct(3,i,j,k)  &
     &           + tx3 * c3 * c4 * ( flux(3,i+1) - flux(3,i) )  &
     &              + dx3 * tx1 * (            rsd(3,i-1,j,k)  &
     &                             - 2.0d+00 * rsd(3,i,j,k)  &
     &                             +           rsd(3,i+1,j,k) )
               frct(4,i,j,k) = frct(4,i,j,k)  &
     &            + tx3 * c3 * c4 * ( flux(4,i+1) - flux(4,i) )  &
     &              + dx4 * tx1 * (            rsd(4,i-1,j,k)  &
     &                             - 2.0d+00 * rsd(4,i,j,k)  &
     &                             +           rsd(4,i+1,j,k) )
               frct(5,i,j,k) = frct(5,i,j,k)  &
     &           + tx3 * c3 * c4 * ( flux(5,i+1) - flux(5,i) )  &
     &              + dx5 * tx1 * (            rsd(5,i-1,j,k)  &
     &                             - 2.0d+00 * rsd(5,i,j,k)  &
     &                             +           rsd(5,i+1,j,k) )
            end do

!---------------------------------------------------------------------
!   Fourth-order dissipation
!---------------------------------------------------------------------
            do m = 1, 5
               frct(m,2,j,k) = frct(m,2,j,k)  &
     &           - dssp * ( + 5.0d+00 * rsd(m,2,j,k)  &
     &                       - 4.0d+00 * rsd(m,3,j,k)  &
     &                       +           rsd(m,4,j,k) )
               frct(m,3,j,k) = frct(m,3,j,k)  &
     &           - dssp * ( - 4.0d+00 * rsd(m,2,j,k)  &
     &                       + 6.0d+00 * rsd(m,3,j,k)  &
     &                       - 4.0d+00 * rsd(m,4,j,k)  &
     &                       +           rsd(m,5,j,k) )
            end do

            do i = 4, nx - 3
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)  &
     &              - dssp * (            rsd(m,i-2,j,k)  &
     &                         - 4.0d+00 * rsd(m,i-1,j,k)  &
     &                         + 6.0d+00 * rsd(m,i,j,k)  &
     &                         - 4.0d+00 * rsd(m,i+1,j,k)  &
     &                         +           rsd(m,i+2,j,k) )
               end do
            end do

            do m = 1, 5
               frct(m,nx-2,j,k) = frct(m,nx-2,j,k)  &
     &           - dssp * (             rsd(m,nx-4,j,k)  &
     &                       - 4.0d+00 * rsd(m,nx-3,j,k)  &
     &                       + 6.0d+00 * rsd(m,nx-2,j,k)  &
     &                       - 4.0d+00 * rsd(m,nx-1,j,k)  )
               frct(m,nx-1,j,k) = frct(m,nx-1,j,k)  &
     &           - dssp * (             rsd(m,nx-3,j,k)  &
     &                       - 4.0d+00 * rsd(m,nx-2,j,k)  &
     &                       + 5.0d+00 * rsd(m,nx-1,j,k) )
            end do

         end do
      end do
!$omp end do

!---------------------------------------------------------------------
!   eta-direction flux differences
!---------------------------------------------------------------------

!$omp do schedule(static) collapse(2)
      do k = 2, nz - 1
         do i = ist, iend
            do j = 1, ny
               flux(1,j) = rsd(3,i,j,k)
               u31 = rsd(3,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)  &
     &                         + rsd(3,i,j,k) * rsd(3,i,j,k)  &
     &                         + rsd(4,i,j,k) * rsd(4,i,j,k) )  &
     &                      / rsd(1,i,j,k)
               flux(2,j) = rsd(2,i,j,k) * u31 
               flux(3,j) = rsd(3,i,j,k) * u31 + c2 *  &
     &                       ( rsd(5,i,j,k) - q )
               flux(4,j) = rsd(4,i,j,k) * u31
               flux(5,j) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u31
            end do

            do j = jst, jend
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)  &
     &                 - ty2 * ( flux(m,j+1) - flux(m,j-1) )
               end do
            end do

            do j = jst, ny
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21j = tmp * rsd(2,i,j,k)
               u31j = tmp * rsd(3,i,j,k)
               u41j = tmp * rsd(4,i,j,k)
               u51j = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i,j-1,k)

               u21jm1 = tmp * rsd(2,i,j-1,k)
               u31jm1 = tmp * rsd(3,i,j-1,k)
               u41jm1 = tmp * rsd(4,i,j-1,k)
               u51jm1 = tmp * rsd(5,i,j-1,k)

               flux(2,j) = ty3 * ( u21j - u21jm1 )
               flux(3,j) = (4.0d+00/3.0d+00) * ty3 *  &
     &                       ( u31j - u31jm1 )
               flux(4,j) = ty3 * ( u41j - u41jm1 )
               flux(5,j) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * ty3 * ( ( u21j  **2 + u31j  **2 + u41j  **2 )  &
     &                      - ( u21jm1**2 + u31jm1**2 + u41jm1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * ty3 * ( u31j**2 - u31jm1**2 )  &
     &              + c1 * c5 * ty3 * ( u51j - u51jm1 )
            end do

            do j = jst, jend
               frct(1,i,j,k) = frct(1,i,j,k)  &
     &              + dy1 * ty1 * (            rsd(1,i,j-1,k)  &
     &                             - 2.0d+00 * rsd(1,i,j,k)  &
     &                             +           rsd(1,i,j+1,k) )
               frct(2,i,j,k) = frct(2,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(2,j+1) - flux(2,j) )  &
     &              + dy2 * ty1 * (            rsd(2,i,j-1,k)  &
     &                             - 2.0d+00 * rsd(2,i,j,k)  &
     &                             +           rsd(2,i,j+1,k) )
               frct(3,i,j,k) = frct(3,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(3,j+1) - flux(3,j) )  &
     &              + dy3 * ty1 * (            rsd(3,i,j-1,k)  &
     &                             - 2.0d+00 * rsd(3,i,j,k)  &
     &                             +           rsd(3,i,j+1,k) )
               frct(4,i,j,k) = frct(4,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(4,j+1) - flux(4,j) )  &
     &              + dy4 * ty1 * (            rsd(4,i,j-1,k)  &
     &                             - 2.0d+00 * rsd(4,i,j,k)  &
     &                             +           rsd(4,i,j+1,k) )
               frct(5,i,j,k) = frct(5,i,j,k)  &
     &          + ty3 * c3 * c4 * ( flux(5,j+1) - flux(5,j) )  &
     &              + dy5 * ty1 * (            rsd(5,i,j-1,k)  &
     &                             - 2.0d+00 * rsd(5,i,j,k)  &
     &                             +           rsd(5,i,j+1,k) )
            end do

!---------------------------------------------------------------------
!   fourth-order dissipation
!---------------------------------------------------------------------
            do m = 1, 5
               frct(m,i,2,k) = frct(m,i,2,k)  &
     &           - dssp * ( + 5.0d+00 * rsd(m,i,2,k)  &
     &                       - 4.0d+00 * rsd(m,i,3,k)  &
     &                       +           rsd(m,i,4,k) )
               frct(m,i,3,k) = frct(m,i,3,k)  &
     &           - dssp * ( - 4.0d+00 * rsd(m,i,2,k)  &
     &                       + 6.0d+00 * rsd(m,i,3,k)  &
     &                       - 4.0d+00 * rsd(m,i,4,k)  &
     &                       +           rsd(m,i,5,k) )
            end do

            do j = 4, ny - 3
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)  &
     &              - dssp * (            rsd(m,i,j-2,k)  &
     &                        - 4.0d+00 * rsd(m,i,j-1,k)  &
     &                        + 6.0d+00 * rsd(m,i,j,k)  &
     &                        - 4.0d+00 * rsd(m,i,j+1,k)  &
     &                        +           rsd(m,i,j+2,k) )
               end do
            end do

            do m = 1, 5
               frct(m,i,ny-2,k) = frct(m,i,ny-2,k)  &
     &           - dssp * (             rsd(m,i,ny-4,k)  &
     &                       - 4.0d+00 * rsd(m,i,ny-3,k)  &
     &                       + 6.0d+00 * rsd(m,i,ny-2,k)  &
     &                       - 4.0d+00 * rsd(m,i,ny-1,k)  )
               frct(m,i,ny-1,k) = frct(m,i,ny-1,k)  &
     &           - dssp * (             rsd(m,i,ny-3,k)  &
     &                       - 4.0d+00 * rsd(m,i,ny-2,k)  &
     &                       + 5.0d+00 * rsd(m,i,ny-1,k)  )
            end do

         end do
      end do
!$omp end do

!---------------------------------------------------------------------
!   zeta-direction flux differences
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do j = jst, jend
         do i = ist, iend
            do k = 1, nz
               flux(1,k) = rsd(4,i,j,k)
               u41 = rsd(4,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)  &
     &                         + rsd(3,i,j,k) * rsd(3,i,j,k)  &
     &                         + rsd(4,i,j,k) * rsd(4,i,j,k) )  &
     &                      / rsd(1,i,j,k)
               flux(2,k) = rsd(2,i,j,k) * u41 
               flux(3,k) = rsd(3,i,j,k) * u41 
               flux(4,k) = rsd(4,i,j,k) * u41 + c2 *  &
     &                         ( rsd(5,i,j,k) - q )
               flux(5,k) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u41
            end do

            do k = 2, nz - 1
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)  &
     &                  - tz2 * ( flux(m,k+1) - flux(m,k-1) )
               end do
            end do

            do k = 2, nz
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21k = tmp * rsd(2,i,j,k)
               u31k = tmp * rsd(3,i,j,k)
               u41k = tmp * rsd(4,i,j,k)
               u51k = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i,j,k-1)

               u21km1 = tmp * rsd(2,i,j,k-1)
               u31km1 = tmp * rsd(3,i,j,k-1)
               u41km1 = tmp * rsd(4,i,j,k-1)
               u51km1 = tmp * rsd(5,i,j,k-1)

               flux(2,k) = tz3 * ( u21k - u21km1 )
               flux(3,k) = tz3 * ( u31k - u31km1 )
               flux(4,k) = (4.0d+00/3.0d+00) * tz3 * ( u41k  &
     &                       - u41km1 )
               flux(5,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )  &
     &              * tz3 * ( ( u21k  **2 + u31k  **2 + u41k  **2 )  &
     &                      - ( u21km1**2 + u31km1**2 + u41km1**2 ) )  &
     &              + (1.0d+00/6.0d+00)  &
     &              * tz3 * ( u41k**2 - u41km1**2 )  &
     &              + c1 * c5 * tz3 * ( u51k - u51km1 )
            end do

            do k = 2, nz - 1
               frct(1,i,j,k) = frct(1,i,j,k)  &
     &              + dz1 * tz1 * (            rsd(1,i,j,k+1)  &
     &                             - 2.0d+00 * rsd(1,i,j,k)  &
     &                             +           rsd(1,i,j,k-1) )
               frct(2,i,j,k) = frct(2,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(2,k+1) - flux(2,k) )  &
     &              + dz2 * tz1 * (            rsd(2,i,j,k+1)  &
     &                             - 2.0d+00 * rsd(2,i,j,k)  &
     &                             +           rsd(2,i,j,k-1) )
               frct(3,i,j,k) = frct(3,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(3,k+1) - flux(3,k) )  &
     &              + dz3 * tz1 * (            rsd(3,i,j,k+1)  &
     &                             - 2.0d+00 * rsd(3,i,j,k)  &
     &                             +           rsd(3,i,j,k-1) )
               frct(4,i,j,k) = frct(4,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(4,k+1) - flux(4,k) )  &
     &              + dz4 * tz1 * (            rsd(4,i,j,k+1)  &
     &                             - 2.0d+00 * rsd(4,i,j,k)  &
     &                             +           rsd(4,i,j,k-1) )
               frct(5,i,j,k) = frct(5,i,j,k)  &
     &          + tz3 * c3 * c4 * ( flux(5,k+1) - flux(5,k) )  &
     &              + dz5 * tz1 * (            rsd(5,i,j,k+1)  &
     &                             - 2.0d+00 * rsd(5,i,j,k)  &
     &                             +           rsd(5,i,j,k-1) )
            end do

!---------------------------------------------------------------------
!   fourth-order dissipation
!---------------------------------------------------------------------
            do m = 1, 5
               frct(m,i,j,2) = frct(m,i,j,2)  &
     &           - dssp * ( + 5.0d+00 * rsd(m,i,j,2)  &
     &                       - 4.0d+00 * rsd(m,i,j,3)  &
     &                       +           rsd(m,i,j,4) )
               frct(m,i,j,3) = frct(m,i,j,3)  &
     &           - dssp * (- 4.0d+00 * rsd(m,i,j,2)  &
     &                      + 6.0d+00 * rsd(m,i,j,3)  &
     &                      - 4.0d+00 * rsd(m,i,j,4)  &
     &                      +           rsd(m,i,j,5) )
            end do

            do k = 4, nz - 3
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)  &
     &              - dssp * (           rsd(m,i,j,k-2)  &
     &                        - 4.0d+00 * rsd(m,i,j,k-1)  &
     &                        + 6.0d+00 * rsd(m,i,j,k)  &
     &                        - 4.0d+00 * rsd(m,i,j,k+1)  &
     &                        +           rsd(m,i,j,k+2) )
               end do
            end do

            do m = 1, 5
               frct(m,i,j,nz-2) = frct(m,i,j,nz-2)  &
     &           - dssp * (            rsd(m,i,j,nz-4)  &
     &                      - 4.0d+00 * rsd(m,i,j,nz-3)  &
     &                      + 6.0d+00 * rsd(m,i,j,nz-2)  &
     &                      - 4.0d+00 * rsd(m,i,j,nz-1)  )
               frct(m,i,j,nz-1) = frct(m,i,j,nz-1)  &
     &           - dssp * (             rsd(m,i,j,nz-3)  &
     &                       - 4.0d+00 * rsd(m,i,j,nz-2)  &
     &                       + 5.0d+00 * rsd(m,i,j,nz-1)  )
            end do
         end do
      end do
!$omp end do nowait
!$omp end parallel

      return
      end
