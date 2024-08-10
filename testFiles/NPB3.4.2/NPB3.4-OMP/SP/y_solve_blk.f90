
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine y_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function performs the solution of the approximate factorization
! step in the y-direction for all five matrix components
! simultaneously. The Thomas algorithm is employed to solve the
! systems for the y-lines. Boundary conditions are non-periodic
!---------------------------------------------------------------------

       use sp_data
       use work_lhs

       implicit none

       integer i, j, k, j1, j2, ii, ib, im
       double precision ru1, fac1, fac2


!---------------------------------------------------------------------
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_ysolve)
!$omp parallel default(shared) private(i,j,k,j1,j2,ii,ib,im,  &
!$omp&    ru1,fac1,fac2)

       call lhsinit(ny2+1)

!$omp do collapse(2)
       do  k = 1, nz2
       do  ii = 1, nx2, bsize
          im = min(bsize, nx2 - ii + 1)

          do  j = 0, grid_points(2)-1
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                rhsx(ib,1,j) = rhs(1,i,j,k)
                rhsx(ib,2,j) = rhs(2,i,j,k)
                rhsx(ib,3,j) = rhs(3,i,j,k)
                rhsx(ib,4,j) = rhs(4,i,j,k)
                rhsx(ib,5,j) = rhs(5,i,j,k)
             end do
          end do

!---------------------------------------------------------------------
! Computes the left hand side for the three y-factors   
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      first fill the lhs for the u-eigenvalue         
!---------------------------------------------------------------------

          do  j = 0, grid_points(2)-1
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                ru1 = c3c4*rho_i(i,j,k)
                cv(ib,j) = vs(i,j,k)
                rhov(ib,j) = dmax1( dy3 + con43 * ru1,  &
     &                           dy5 + c1c5*ru1,  &
     &                           dymax + ru1,  &
     &                           dy1)
             end do
          end do
 
          do  j = 1, grid_points(2)-2
             do  ib = 1, bsize
                lhs(ib,1,j) =  0.0d0
                lhs(ib,2,j) = -dtty2 * cv(ib,j-1) - dtty1 * rhov(ib,j-1)
                lhs(ib,3,j) =  1.0 + c2dtty1 * rhov(ib,j)
                lhs(ib,4,j) =  dtty2 * cv(ib,j+1) - dtty1 * rhov(ib,j+1)
                lhs(ib,5,j) =  0.0d0
             end do
          end do

!---------------------------------------------------------------------
!      add fourth order dissipation                             
!---------------------------------------------------------------------

          do  ib = 1, bsize
             j = 1
             lhs(ib,3,j) = lhs(ib,3,j) + comz5
             lhs(ib,4,j) = lhs(ib,4,j) - comz4
             lhs(ib,5,j) = lhs(ib,5,j) + comz1
       
             j = 2
             lhs(ib,2,j) = lhs(ib,2,j) - comz4
             lhs(ib,3,j) = lhs(ib,3,j) + comz6
             lhs(ib,4,j) = lhs(ib,4,j) - comz4
             lhs(ib,5,j) = lhs(ib,5,j) + comz1
          end do

          do   j=3, grid_points(2)-4
             do  ib = 1, bsize
                lhs(ib,1,j) = lhs(ib,1,j) + comz1
                lhs(ib,2,j) = lhs(ib,2,j) - comz4
                lhs(ib,3,j) = lhs(ib,3,j) + comz6
                lhs(ib,4,j) = lhs(ib,4,j) - comz4
                lhs(ib,5,j) = lhs(ib,5,j) + comz1
             end do
          end do

          do  ib = 1, bsize
             j = grid_points(2)-3
             lhs(ib,1,j) = lhs(ib,1,j) + comz1
             lhs(ib,2,j) = lhs(ib,2,j) - comz4
             lhs(ib,3,j) = lhs(ib,3,j) + comz6
             lhs(ib,4,j) = lhs(ib,4,j) - comz4

             j = grid_points(2)-2
             lhs(ib,1,j) = lhs(ib,1,j) + comz1
             lhs(ib,2,j) = lhs(ib,2,j) - comz4
             lhs(ib,3,j) = lhs(ib,3,j) + comz5
          end do

!---------------------------------------------------------------------
!      subsequently, do the other two factors                    
!---------------------------------------------------------------------
          do    j = 1, grid_points(2)-2
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                lhsp(ib,1,j) = lhs(ib,1,j)
                lhsp(ib,2,j) = lhs(ib,2,j) -  &
     &                            dtty2 * speed(i,j-1,k)
                lhsp(ib,3,j) = lhs(ib,3,j)
                lhsp(ib,4,j) = lhs(ib,4,j) +  &
     &                            dtty2 * speed(i,j+1,k)
                lhsp(ib,5,j) = lhs(ib,5,j)
                lhsm(ib,1,j) = lhs(ib,1,j)
                lhsm(ib,2,j) = lhs(ib,2,j) +  &
     &                            dtty2 * speed(i,j-1,k)
                lhsm(ib,3,j) = lhs(ib,3,j)
                lhsm(ib,4,j) = lhs(ib,4,j) -  &
     &                            dtty2 * speed(i,j+1,k)
                lhsm(ib,5,j) = lhs(ib,5,j)
             end do
          end do


!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------

          do    j = 0, grid_points(2)-3
             j1 = j  + 1
             j2 = j  + 2
             do  ib = 1, bsize
                fac1      = 1.d0/lhs(ib,3,j)
                lhs(ib,4,j)  = fac1*lhs(ib,4,j)
                lhs(ib,5,j)  = fac1*lhs(ib,5,j)
                rhsx(ib,1,j) = fac1*rhsx(ib,1,j)
                rhsx(ib,2,j) = fac1*rhsx(ib,2,j)
                rhsx(ib,3,j) = fac1*rhsx(ib,3,j)
                lhs(ib,3,j1) = lhs(ib,3,j1) -  &
     &                         lhs(ib,2,j1)*lhs(ib,4,j)
                lhs(ib,4,j1) = lhs(ib,4,j1) -  &
     &                         lhs(ib,2,j1)*lhs(ib,5,j)
                rhsx(ib,1,j1) = rhsx(ib,1,j1) -  &
     &                         lhs(ib,2,j1)*rhsx(ib,1,j)
                rhsx(ib,2,j1) = rhsx(ib,2,j1) -  &
     &                         lhs(ib,2,j1)*rhsx(ib,2,j)
                rhsx(ib,3,j1) = rhsx(ib,3,j1) -  &
     &                         lhs(ib,2,j1)*rhsx(ib,3,j)
                lhs(ib,2,j2) = lhs(ib,2,j2) -  &
     &                         lhs(ib,1,j2)*lhs(ib,4,j)
                lhs(ib,3,j2) = lhs(ib,3,j2) -  &
     &                         lhs(ib,1,j2)*lhs(ib,5,j)
                rhsx(ib,1,j2) = rhsx(ib,1,j2) -  &
     &                         lhs(ib,1,j2)*rhsx(ib,1,j)
                rhsx(ib,2,j2) = rhsx(ib,2,j2) -  &
     &                         lhs(ib,1,j2)*rhsx(ib,2,j)
                rhsx(ib,3,j2) = rhsx(ib,3,j2) -  &
     &                         lhs(ib,1,j2)*rhsx(ib,3,j)
             end do
          end do

!---------------------------------------------------------------------
!      The last two rows in this grid block are a bit different, 
!      since they do not have two more rows available for the
!      elimination of off-diagonal entries
!---------------------------------------------------------------------

          j  = grid_points(2)-2
          j1 = grid_points(2)-1
          do  ib = 1, bsize
             fac1      = 1.d0/lhs(ib,3,j)
             lhs(ib,4,j)  = fac1*lhs(ib,4,j)
             lhs(ib,5,j)  = fac1*lhs(ib,5,j)
             rhsx(ib,1,j) = fac1*rhsx(ib,1,j)
             rhsx(ib,2,j) = fac1*rhsx(ib,2,j)
             rhsx(ib,3,j) = fac1*rhsx(ib,3,j)
             lhs(ib,3,j1) = lhs(ib,3,j1) -  &
     &                      lhs(ib,2,j1)*lhs(ib,4,j)
             lhs(ib,4,j1) = lhs(ib,4,j1) -  &
     &                      lhs(ib,2,j1)*lhs(ib,5,j)
             rhsx(ib,1,j1) = rhsx(ib,1,j1) -  &
     &                      lhs(ib,2,j1)*rhsx(ib,1,j)
             rhsx(ib,2,j1) = rhsx(ib,2,j1) -  &
     &                      lhs(ib,2,j1)*rhsx(ib,2,j)
             rhsx(ib,3,j1) = rhsx(ib,3,j1) -  &
     &                      lhs(ib,2,j1)*rhsx(ib,3,j)
!---------------------------------------------------------------------
!            scale the last row immediately 
!---------------------------------------------------------------------
             fac2      = 1.d0/lhs(ib,3,j1)
             rhsx(ib,1,j1) = fac2*rhsx(ib,1,j1)
             rhsx(ib,2,j1) = fac2*rhsx(ib,2,j1)
             rhsx(ib,3,j1) = fac2*rhsx(ib,3,j1)
          end do

!---------------------------------------------------------------------
!      do the u+c and the u-c factors                 
!---------------------------------------------------------------------
          do    j = 0, grid_points(2)-3
             j1 = j  + 1
             j2 = j  + 2
             do  ib = 1, bsize
                fac1       = 1.d0/lhsp(ib,3,j)
                lhsp(ib,4,j)  = fac1*lhsp(ib,4,j)
                lhsp(ib,5,j)  = fac1*lhsp(ib,5,j)
                rhsx(ib,4,j)  = fac1*rhsx(ib,4,j)
                lhsp(ib,3,j1) = lhsp(ib,3,j1) -  &
     &                       lhsp(ib,2,j1)*lhsp(ib,4,j)
                lhsp(ib,4,j1) = lhsp(ib,4,j1) -  &
     &                       lhsp(ib,2,j1)*lhsp(ib,5,j)
                rhsx(ib,4,j1) = rhsx(ib,4,j1) -  &
     &                       lhsp(ib,2,j1)*rhsx(ib,4,j)
                lhsp(ib,2,j2) = lhsp(ib,2,j2) -  &
     &                       lhsp(ib,1,j2)*lhsp(ib,4,j)
                lhsp(ib,3,j2) = lhsp(ib,3,j2) -  &
     &                       lhsp(ib,1,j2)*lhsp(ib,5,j)
                rhsx(ib,4,j2) = rhsx(ib,4,j2) -  &
     &                       lhsp(ib,1,j2)*rhsx(ib,4,j)
                fac1       = 1.d0/lhsm(ib,3,j)
                lhsm(ib,4,j)  = fac1*lhsm(ib,4,j)
                lhsm(ib,5,j)  = fac1*lhsm(ib,5,j)
                rhsx(ib,5,j)  = fac1*rhsx(ib,5,j)
                lhsm(ib,3,j1) = lhsm(ib,3,j1) -  &
     &                       lhsm(ib,2,j1)*lhsm(ib,4,j)
                lhsm(ib,4,j1) = lhsm(ib,4,j1) -  &
     &                       lhsm(ib,2,j1)*lhsm(ib,5,j)
                rhsx(ib,5,j1) = rhsx(ib,5,j1) -  &
     &                       lhsm(ib,2,j1)*rhsx(ib,5,j)
                lhsm(ib,2,j2) = lhsm(ib,2,j2) -  &
     &                       lhsm(ib,1,j2)*lhsm(ib,4,j)
                lhsm(ib,3,j2) = lhsm(ib,3,j2) -  &
     &                       lhsm(ib,1,j2)*lhsm(ib,5,j)
                rhsx(ib,5,j2) = rhsx(ib,5,j2) -  &
     &                       lhsm(ib,1,j2)*rhsx(ib,5,j)
             end do
          end do

!---------------------------------------------------------------------
!         And again the last two rows separately
!---------------------------------------------------------------------
          j  = grid_points(2)-2
          j1 = grid_points(2)-1
          do  ib = 1, bsize
             fac1       = 1.d0/lhsp(ib,3,j)
             lhsp(ib,4,j)  = fac1*lhsp(ib,4,j)
             lhsp(ib,5,j)  = fac1*lhsp(ib,5,j)
             rhsx(ib,4,j)  = fac1*rhsx(ib,4,j)
             lhsp(ib,3,j1) = lhsp(ib,3,j1) -  &
     &                    lhsp(ib,2,j1)*lhsp(ib,4,j)
             lhsp(ib,4,j1) = lhsp(ib,4,j1) -  &
     &                    lhsp(ib,2,j1)*lhsp(ib,5,j)
             rhsx(ib,4,j1)   = rhsx(ib,4,j1) -  &
     &                    lhsp(ib,2,j1)*rhsx(ib,4,j)
             fac1       = 1.d0/lhsm(ib,3,j)
             lhsm(ib,4,j)  = fac1*lhsm(ib,4,j)
             lhsm(ib,5,j)  = fac1*lhsm(ib,5,j)
             rhsx(ib,5,j)  = fac1*rhsx(ib,5,j)
             lhsm(ib,3,j1) = lhsm(ib,3,j1) -  &
     &                    lhsm(ib,2,j1)*lhsm(ib,4,j)
             lhsm(ib,4,j1) = lhsm(ib,4,j1) -  &
     &                    lhsm(ib,2,j1)*lhsm(ib,5,j)
             rhsx(ib,5,j1)   = rhsx(ib,5,j1) -  &
     &                    lhsm(ib,2,j1)*rhsx(ib,5,j)
!---------------------------------------------------------------------
!               Scale the last row immediately 
!---------------------------------------------------------------------
             rhsx(ib,4,j1)   = rhsx(ib,4,j1)/lhsp(ib,3,j1)
             rhsx(ib,5,j1)   = rhsx(ib,5,j1)/lhsm(ib,3,j1)
          end do


!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------

          j  = grid_points(2)-2
          j1 = grid_points(2)-1
          do  ib = 1, bsize
             rhsx(ib,1,j) = rhsx(ib,1,j) -  &
     &                           lhs(ib,4,j)*rhsx(ib,1,j1)
             rhsx(ib,2,j) = rhsx(ib,2,j) -  &
     &                           lhs(ib,4,j)*rhsx(ib,2,j1)
             rhsx(ib,3,j) = rhsx(ib,3,j) -  &
     &                           lhs(ib,4,j)*rhsx(ib,3,j1)

             rhsx(ib,4,j) = rhsx(ib,4,j) -  &
     &                           lhsp(ib,4,j)*rhsx(ib,4,j1)
             rhsx(ib,5,j) = rhsx(ib,5,j) -  &
     &                           lhsm(ib,4,j)*rhsx(ib,5,j1)
          end do

!---------------------------------------------------------------------
!      The first three factors
!---------------------------------------------------------------------
          do   j = grid_points(2)-3, 0, -1
             j1 = j  + 1
             j2 = j  + 2
             do  ib = 1, bsize
                rhsx(ib,1,j) = rhsx(ib,1,j) -  &
     &                          lhs(ib,4,j)*rhsx(ib,1,j1) -  &
     &                          lhs(ib,5,j)*rhsx(ib,1,j2)
                rhsx(ib,2,j) = rhsx(ib,2,j) -  &
     &                          lhs(ib,4,j)*rhsx(ib,2,j1) -  &
     &                          lhs(ib,5,j)*rhsx(ib,2,j2)
                rhsx(ib,3,j) = rhsx(ib,3,j) -  &
     &                          lhs(ib,4,j)*rhsx(ib,3,j1) -  &
     &                          lhs(ib,5,j)*rhsx(ib,3,j2)

!---------------------------------------------------------------------
!      And the remaining two
!---------------------------------------------------------------------
                rhsx(ib,4,j) = rhsx(ib,4,j) -  &
     &                          lhsp(ib,4,j)*rhsx(ib,4,j1) -  &
     &                          lhsp(ib,5,j)*rhsx(ib,4,j2)
                rhsx(ib,5,j) = rhsx(ib,5,j) -  &
     &                          lhsm(ib,4,j)*rhsx(ib,5,j1) -  &
     &                          lhsm(ib,5,j)*rhsx(ib,5,j2)
             end do
          end do

          do  j = 0, grid_points(2)-1
             do  ib = 1, im
                i = ib + ii - 1
                rhs(1,i,j,k) = rhsx(ib,1,j)
                rhs(2,i,j,k) = rhsx(ib,2,j)
                rhs(3,i,j,k) = rhsx(ib,3,j)
                rhs(4,i,j,k) = rhsx(ib,4,j)
                rhs(5,i,j,k) = rhsx(ib,5,j)
             end do
          end do

       end do
       end do
!$omp end do nowait
!$omp end parallel
       if (timeron) call timer_stop(t_ysolve)


       call pinvr

       return
       end


