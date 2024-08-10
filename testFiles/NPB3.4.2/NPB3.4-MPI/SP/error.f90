
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine error_norm(rms)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function computes the norm of the difference between the
! computed solution and the exact solution
!---------------------------------------------------------------------

       use sp_data
       use mpinpb

       implicit none

       integer c, i, j, k, m, ii, jj, kk, d, error
       double precision xi, eta, zeta, u_exact(5), rms(5), rms_work(5),  &
     &                  add

       do   m = 1, 5 
          rms_work(m) = 0.0d0
       end do

       do   c = 1, ncells
          kk = 0
          do   k = cell_low(3,c), cell_high(3,c)
             zeta = dble(k) * dnzm1
             jj = 0
             do   j = cell_low(2,c), cell_high(2,c)
                eta = dble(j) * dnym1
                ii = 0
                do   i = cell_low(1,c), cell_high(1,c)
                   xi = dble(i) * dnxm1
                   call exact_solution(xi, eta, zeta, u_exact)

                   do   m = 1, 5
                      add = u(ii,jj,kk,m,c)-u_exact(m)
                      rms_work(m) = rms_work(m) + add*add
                   end do
                   ii = ii + 1
                end do
                jj = jj + 1
             end do
             kk = kk + 1
          end do
       end do

       call mpi_allreduce(rms_work, rms, 5, dp_type,  &
     &                 MPI_SUM, comm_setup, error)

       do    m = 1, 5
          do    d = 1, 3
             rms(m) = rms(m) / dble(grid_points(d)-2)
          end do
          rms(m) = dsqrt(rms(m))
       end do

       return
       end



       subroutine rhs_norm(rms)

       use sp_data
       use mpinpb

       implicit none

       integer c, i, j, k, d, m, error
       double precision rms(5), rms_work(5), add

       do    m = 1, 5
          rms_work(m) = 0.0d0
       end do

       do   c = 1, ncells
          do   k = start(3,c), cell_size(3,c)-end(3,c)-1
             do   j = start(2,c), cell_size(2,c)-end(2,c)-1
                do   i = start(1,c), cell_size(1,c)-end(1,c)-1
                   do   m = 1, 5
                      add = rhs(i,j,k,m,c)
                      rms_work(m) = rms_work(m) + add*add
                   end do
                end do
             end do
          end do
       end do



       call mpi_allreduce(rms_work, rms, 5, dp_type,  &
     &                 MPI_SUM, comm_setup, error)

       do   m = 1, 5
          do   d = 1, 3
             rms(m) = rms(m) / dble(grid_points(d)-2)
          end do
          rms(m) = dsqrt(rms(m))
       end do

       return
       end


