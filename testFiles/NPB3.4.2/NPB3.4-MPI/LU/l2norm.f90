
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine l2norm ( ldx, ldy, ldz,  &
     &                    nx0, ny0, nz0,  &
     &                    ist, iend,  &
     &                    jst, jend,  &
     &                    v, sum )
!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   to compute the l2-norm of vector v.
!---------------------------------------------------------------------

      use timing
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer ldx, ldy, ldz
      integer nx0, ny0, nz0
      integer ist, iend
      integer jst, jend
      double precision  v(5,-1:ldx+2,-1:ldy+2,*), sum(5)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      double precision  dummy(5)

      integer IERROR


      do m = 1, 5
         dummy(m) = 0.0d+00
      end do

      do k = 2, nz0-1
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  dummy(m) = dummy(m) + v(m,i,j,k) * v(m,i,j,k)
               end do
            end do
         end do
      end do

!---------------------------------------------------------------------
!   compute the global sum of individual contributions to dot product.
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_rcomm)
      call MPI_ALLREDUCE( dummy,  &
     &                    sum,  &
     &                    5,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_solve,  &
     &                    IERROR )
      if (timeron) call timer_stop(t_rcomm)

      do m = 1, 5
         sum(m) = sqrt ( sum(m) / ( dble(nx0-2)*(ny0-2)*(nz0-2) ) )
      end do

      return
      end
