!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine  initialize

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     This subroutine initializes the field variable u using 
!     tri-linear transfinite interpolation of the boundary values     
!---------------------------------------------------------------------

      use bt_data
      implicit none
      
      integer i, j, k, m, ix, iy, iz
      double precision  xi, eta, zeta, Pface(5,3,2), Pxi, Peta,   &
     &     Pzeta, temp(5)


!$omp parallel default(shared)  &
!$omp& private(i,j,k,m,zeta,eta,xi,ix,iy,iz,Pface,Pxi,Peta,Pzeta,temp)
!---------------------------------------------------------------------
!  Later (in compute_rhs) we compute 1/u for every element. A few of 
!  the corner elements are not used, but it convenient (and faster) 
!  to compute the whole thing with a simple loop. Make sure those 
!  values are nonzero by initializing the whole thing here. 
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            do i = 0, grid_points(1)-1
               do m = 1, 5
                  u(m,i,j,k) = 1.0
               end do
            end do
         end do
      end do
!$omp end do nowait
!---------------------------------------------------------------------


!---------------------------------------------------------------------
!     first store the "interpolated" values everywhere on the grid    
!---------------------------------------------------------------------

!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            zeta = dble(k) * dnzm1
            eta = dble(j) * dnym1
            do i = 0, grid_points(1)-1
               xi = dble(i) * dnxm1
                  
               do ix = 1, 2
                  call exact_solution(dble(ix-1), eta, zeta,   &
     &                    Pface(1,1,ix))
               enddo

               do iy = 1, 2
                  call exact_solution(xi, dble(iy-1) , zeta,   &
     &                    Pface(1,2,iy))
               enddo

               do iz = 1, 2
                  call exact_solution(xi, eta, dble(iz-1),     &
     &                    Pface(1,3,iz))
               enddo

               do m = 1, 5
                  Pxi   = xi   * Pface(m,1,2) +   &
     &                    (1.0d0-xi)   * Pface(m,1,1)
                  Peta  = eta  * Pface(m,2,2) +   &
     &                    (1.0d0-eta)  * Pface(m,2,1)
                  Pzeta = zeta * Pface(m,3,2) +   &
     &                    (1.0d0-zeta) * Pface(m,3,1)
                     
                  u(m,i,j,k) = Pxi + Peta + Pzeta -   &
     &                    Pxi*Peta - Pxi*Pzeta - Peta*Pzeta +   &
     &                    Pxi*Peta*Pzeta

               enddo
            enddo
         enddo
      enddo
!$omp end do nowait

!---------------------------------------------------------------------
!     now store the exact values on the boundaries        
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     west face                                                  
!---------------------------------------------------------------------
      i = 0
      xi = 0.0d0
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            zeta = dble(k) * dnzm1
            eta = dble(j) * dnym1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait

!---------------------------------------------------------------------
!     east face                                                      
!---------------------------------------------------------------------

      i = grid_points(1)-1
      xi = 1.0d0
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            zeta = dble(k) * dnzm1
            eta = dble(j) * dnym1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do

!---------------------------------------------------------------------
!     south face                                                 
!---------------------------------------------------------------------
      j = 0
      eta = 0.0d0
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do i = 0, grid_points(1)-1
            zeta = dble(k) * dnzm1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait


!---------------------------------------------------------------------
!     north face                                    
!---------------------------------------------------------------------
      j = grid_points(2)-1
      eta = 1.0d0
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do i = 0, grid_points(1)-1
            zeta = dble(k) * dnzm1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do

!---------------------------------------------------------------------
!     bottom face                                       
!---------------------------------------------------------------------
      k = 0
      zeta = 0.0d0
!$omp do schedule(static) collapse(2)
      do j = 0, grid_points(2)-1
         do i =0, grid_points(1)-1
            eta = dble(j) * dnym1
            xi = dble(i) *dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait

!---------------------------------------------------------------------
!     top face     
!---------------------------------------------------------------------
      k = grid_points(3)-1
      zeta = 1.0d0
!$omp do schedule(static) collapse(2)
      do j = 0, grid_points(2)-1
         do i =0, grid_points(1)-1
            eta = dble(j) * dnym1
            xi = dble(i) * dnxm1
            call exact_solution(xi, eta, zeta, temp)
            do m = 1, 5
               u(m,i,j,k) = temp(m)
            enddo
         enddo
      enddo
!$omp end do nowait
!$omp end parallel

      return
      end


