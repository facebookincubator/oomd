
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setbv

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   set the boundary values of dependent variables
!---------------------------------------------------------------------

      use lu_data
      implicit none

!---------------------------------------------------------------------
!   local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision temp1(5), temp2(5)

!---------------------------------------------------------------------
!   set the dependent variable values along the top and bottom faces
!---------------------------------------------------------------------
!$omp parallel default(shared) private(i,j,k,m,temp1,temp2)  &
!$omp& shared(nx,ny,nz)
!$omp do schedule(static) collapse(2)
      do j = 1, ny
         do i = 1, nx
            call exact( i, j, 1, temp1 )
            call exact( i, j, nz, temp2 )
            do m = 1, 5
               u( m, i, j, 1 ) = temp1(m)
               u( m, i, j, nz ) = temp2(m)
            end do
         end do
      end do
!$omp end do

!---------------------------------------------------------------------
!   set the dependent variable values along north and south faces
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 1, nz
         do i = 1, nx
            call exact( i, 1, k, temp1 )
            call exact( i, ny, k, temp2 )
            do m = 1, 5
               u( m, i, 1, k ) = temp1(m)
               u( m, i, ny, k ) = temp2(m)
            end do
         end do
      end do
!$omp end do

!---------------------------------------------------------------------
!   set the dependent variable values along east and west faces
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 1, nz
         do j = 1, ny
            call exact( 1, j, k, temp1 )
            call exact( nx, j, k, temp2 )
            do m = 1, 5
               u( m, 1, j, k ) = temp1(m)
               u( m, nx, j, k ) = temp2(m)
            end do
         end do
      end do
!$omp end do nowait
!$omp end parallel

      return
      end
