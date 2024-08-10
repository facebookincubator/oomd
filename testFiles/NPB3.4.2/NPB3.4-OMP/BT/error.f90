!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine error_norm(rms)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     this function computes the norm of the difference between the
!     computed solution and the exact solution
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i, j, k, m, d
      double precision xi, eta, zeta, u_exact(5), rms(5), add

      do m = 1, 5
         rms(m) = 0.0d0
      enddo

!$omp parallel do schedule(static) collapse(2) default(shared)  &
!$omp& private(i,j,k,m,zeta,eta,xi,add,u_exact) reduction(+: rms)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            zeta = dble(k) * dnzm1
            eta = dble(j) * dnym1
            do i = 0, grid_points(1)-1
               xi = dble(i) * dnxm1
               call exact_solution(xi, eta, zeta, u_exact)

               do m = 1, 5
                  add = u(m,i,j,k)-u_exact(m)
                  rms(m) = rms(m) + add*add
               enddo
            enddo
         enddo
      enddo
!$omp end parallel do

      do m = 1, 5
         do d = 1, 3
            rms(m) = rms(m) / dble(grid_points(d)-2)
         enddo
         rms(m) = dsqrt(rms(m))
      enddo

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine rhs_norm(rms)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i, j, k, d, m
      double precision rms(5), add

      do m = 1, 5
         rms(m) = 0.0d0
      enddo

!$omp parallel do schedule(static) collapse(2)  &
!$omp&  default(shared) private(i,j,k,m,add) reduction(+: rms)
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 1, grid_points(1)-2
               do m = 1, 5
                  add = rhs(m,i,j,k)
                  rms(m) = rms(m) + add*add
               enddo 
            enddo 
         enddo 
      enddo 
!$omp end parallel do

      do m = 1, 5
         do d = 1, 3
            rms(m) = rms(m) / dble(grid_points(d)-2)
         enddo 
         rms(m) = dsqrt(rms(m))
      enddo 

      return
      end

