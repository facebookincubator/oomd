
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine z_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function performs the solution of the approximate factorization
! step in the z-direction for all five matrix components
! simultaneously. The Thomas algorithm is employed to solve the
! systems for the z-lines. Boundary conditions are non-periodic
!---------------------------------------------------------------------

       use sp_data
       use work_lhs

       implicit none

       integer i, j, k, k1, k2, ii, ib, im
       double precision ru1, fac1, fac2


!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! Prepare for z-solve, array redistribution   
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_zsolve)
!$omp parallel default(shared) private(i,j,k,k1,k2,ii,ib,im,  &
!$omp&    ru1,fac1,fac2)

       call lhsinit(nz2+1)

!$omp do collapse(2)
       do   j = 1, ny2
       do  ii = 1, nx2, bsize
          im = min(bsize, nx2 - ii + 1)

          do  k = 0, grid_points(3)-1
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                rhsx(ib,1,k) = rhs(1,i,j,k)
                rhsx(ib,2,k) = rhs(2,i,j,k)
                rhsx(ib,3,k) = rhs(3,i,j,k)
                rhsx(ib,4,k) = rhs(4,i,j,k)
                rhsx(ib,5,k) = rhs(5,i,j,k)
             end do
          end do

!---------------------------------------------------------------------
! Computes the left hand side for the three z-factors   
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! first fill the lhs for the u-eigenvalue                          
!---------------------------------------------------------------------

          do   k = 0, nz2 + 1
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                ru1 = c3c4*rho_i(i,j,k)
                cv(ib,k) = ws(i,j,k)
                rhov(ib,k) = dmax1(dz4 + con43 * ru1,  &
     &                          dz5 + c1c5 * ru1,  &
     &                          dzmax + ru1,  &
     &                          dz1)
             end do
          end do

          do   k =  1, nz2
             do  ib = 1, bsize
                lhs(ib,1,k) =  0.0d0
                lhs(ib,2,k) = -dttz2 * cv(ib,k-1) - dttz1 * rhov(ib,k-1)
                lhs(ib,3,k) =  1.0 + c2dttz1 * rhov(ib,k)
                lhs(ib,4,k) =  dttz2 * cv(ib,k+1) - dttz1 * rhov(ib,k+1)
                lhs(ib,5,k) =  0.0d0
             end do
          end do

!---------------------------------------------------------------------
!      add fourth order dissipation                                  
!---------------------------------------------------------------------

          do  ib = 1, bsize
             k = 1
             lhs(ib,3,k) = lhs(ib,3,k) + comz5
             lhs(ib,4,k) = lhs(ib,4,k) - comz4
             lhs(ib,5,k) = lhs(ib,5,k) + comz1

             k = 2
             lhs(ib,2,k) = lhs(ib,2,k) - comz4
             lhs(ib,3,k) = lhs(ib,3,k) + comz6
             lhs(ib,4,k) = lhs(ib,4,k) - comz4
             lhs(ib,5,k) = lhs(ib,5,k) + comz1
          end do

          do    k = 3, nz2-2
             do  ib = 1, bsize
                lhs(ib,1,k) = lhs(ib,1,k) + comz1
                lhs(ib,2,k) = lhs(ib,2,k) - comz4
                lhs(ib,3,k) = lhs(ib,3,k) + comz6
                lhs(ib,4,k) = lhs(ib,4,k) - comz4
                lhs(ib,5,k) = lhs(ib,5,k) + comz1
             end do
          end do

          do  ib = 1, bsize
             k = nz2-1
             lhs(ib,1,k) = lhs(ib,1,k) + comz1
             lhs(ib,2,k) = lhs(ib,2,k) - comz4
             lhs(ib,3,k) = lhs(ib,3,k) + comz6
             lhs(ib,4,k) = lhs(ib,4,k) - comz4

             k = nz2
             lhs(ib,1,k) = lhs(ib,1,k) + comz1
             lhs(ib,2,k) = lhs(ib,2,k) - comz4
             lhs(ib,3,k) = lhs(ib,3,k) + comz5
          end do


!---------------------------------------------------------------------
!      subsequently, fill the other factors (u+c), (u-c) 
!---------------------------------------------------------------------
          do    k = 1, nz2
             do  ib = 1, bsize
                i = min(ib,im) + ii - 1
                lhsp(ib,1,k) = lhs(ib,1,k)
                lhsp(ib,2,k) = lhs(ib,2,k) -  &
     &                            dttz2 * speed(i,j,k-1)
                lhsp(ib,3,k) = lhs(ib,3,k)
                lhsp(ib,4,k) = lhs(ib,4,k) +  &
     &                            dttz2 * speed(i,j,k+1)
                lhsp(ib,5,k) = lhs(ib,5,k)
                lhsm(ib,1,k) = lhs(ib,1,k)
                lhsm(ib,2,k) = lhs(ib,2,k) +  &
     &                            dttz2 * speed(i,j,k-1)
                lhsm(ib,3,k) = lhs(ib,3,k)
                lhsm(ib,4,k) = lhs(ib,4,k) -  &
     &                            dttz2 * speed(i,j,k+1)
                lhsm(ib,5,k) = lhs(ib,5,k)
             end do
          end do


!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------

          do    k = 0, grid_points(3)-3
             k1 = k  + 1
             k2 = k  + 2
             do  ib = 1, bsize
                fac1      = 1.d0/lhs(ib,3,k)
                lhs(ib,4,k)  = fac1*lhs(ib,4,k)
                lhs(ib,5,k)  = fac1*lhs(ib,5,k)
                rhsx(ib,1,k) = fac1*rhsx(ib,1,k)
                rhsx(ib,2,k) = fac1*rhsx(ib,2,k)
                rhsx(ib,3,k) = fac1*rhsx(ib,3,k)
                lhs(ib,3,k1) = lhs(ib,3,k1) -  &
     &                         lhs(ib,2,k1)*lhs(ib,4,k)
                lhs(ib,4,k1) = lhs(ib,4,k1) -  &
     &                         lhs(ib,2,k1)*lhs(ib,5,k)
                rhsx(ib,1,k1) = rhsx(ib,1,k1) -  &
     &                         lhs(ib,2,k1)*rhsx(ib,1,k)
                rhsx(ib,2,k1) = rhsx(ib,2,k1) -  &
     &                         lhs(ib,2,k1)*rhsx(ib,2,k)
                rhsx(ib,3,k1) = rhsx(ib,3,k1) -  &
     &                         lhs(ib,2,k1)*rhsx(ib,3,k)
                lhs(ib,2,k2) = lhs(ib,2,k2) -  &
     &                         lhs(ib,1,k2)*lhs(ib,4,k)
                lhs(ib,3,k2) = lhs(ib,3,k2) -  &
     &                         lhs(ib,1,k2)*lhs(ib,5,k)
                rhsx(ib,1,k2) = rhsx(ib,1,k2) -  &
     &                         lhs(ib,1,k2)*rhsx(ib,1,k)
                rhsx(ib,2,k2) = rhsx(ib,2,k2) -  &
     &                         lhs(ib,1,k2)*rhsx(ib,2,k)
                rhsx(ib,3,k2) = rhsx(ib,3,k2) -  &
     &                         lhs(ib,1,k2)*rhsx(ib,3,k)
             end do
          end do

!---------------------------------------------------------------------
!      The last two rows in this grid block are a bit different, 
!      since they do not have two more rows available for the
!      elimination of off-diagonal entries
!---------------------------------------------------------------------
          k  = grid_points(3)-2
          k1 = grid_points(3)-1
          do  ib = 1, bsize
             fac1      = 1.d0/lhs(ib,3,k)
             lhs(ib,4,k)  = fac1*lhs(ib,4,k)
             lhs(ib,5,k)  = fac1*lhs(ib,5,k)
             rhsx(ib,1,k) = fac1*rhsx(ib,1,k)
             rhsx(ib,2,k) = fac1*rhsx(ib,2,k)
             rhsx(ib,3,k) = fac1*rhsx(ib,3,k)
             lhs(ib,3,k1) = lhs(ib,3,k1) -  &
     &                      lhs(ib,2,k1)*lhs(ib,4,k)
             lhs(ib,4,k1) = lhs(ib,4,k1) -  &
     &                      lhs(ib,2,k1)*lhs(ib,5,k)
             rhsx(ib,1,k1) = rhsx(ib,1,k1) -  &
     &                      lhs(ib,2,k1)*rhsx(ib,1,k)
             rhsx(ib,2,k1) = rhsx(ib,2,k1) -  &
     &                      lhs(ib,2,k1)*rhsx(ib,2,k)
             rhsx(ib,3,k1) = rhsx(ib,3,k1) -  &
     &                      lhs(ib,2,k1)*rhsx(ib,3,k)
!---------------------------------------------------------------------
!               scale the last row immediately
!---------------------------------------------------------------------
             fac2      = 1.d0/lhs(ib,3,k1)
             rhsx(ib,1,k1) = fac2*rhsx(ib,1,k1)
             rhsx(ib,2,k1) = fac2*rhsx(ib,2,k1)
             rhsx(ib,3,k1) = fac2*rhsx(ib,3,k1)
          end do

!---------------------------------------------------------------------
!      do the u+c and the u-c factors               
!---------------------------------------------------------------------
          do    k = 0, grid_points(3)-3
             k1 = k  + 1
             k2 = k  + 2
             do  ib = 1, bsize
                fac1       = 1.d0/lhsp(ib,3,k)
                lhsp(ib,4,k)  = fac1*lhsp(ib,4,k)
                lhsp(ib,5,k)  = fac1*lhsp(ib,5,k)
                rhsx(ib,4,k)  = fac1*rhsx(ib,4,k)
                lhsp(ib,3,k1) = lhsp(ib,3,k1) -  &
     &                       lhsp(ib,2,k1)*lhsp(ib,4,k)
                lhsp(ib,4,k1) = lhsp(ib,4,k1) -  &
     &                       lhsp(ib,2,k1)*lhsp(ib,5,k)
                rhsx(ib,4,k1) = rhsx(ib,4,k1) -  &
     &                       lhsp(ib,2,k1)*rhsx(ib,4,k)
                lhsp(ib,2,k2) = lhsp(ib,2,k2) -  &
     &                       lhsp(ib,1,k2)*lhsp(ib,4,k)
                lhsp(ib,3,k2) = lhsp(ib,3,k2) -  &
     &                       lhsp(ib,1,k2)*lhsp(ib,5,k)
                rhsx(ib,4,k2) = rhsx(ib,4,k2) -  &
     &                       lhsp(ib,1,k2)*rhsx(ib,4,k)
                fac1       = 1.d0/lhsm(ib,3,k)
                lhsm(ib,4,k)  = fac1*lhsm(ib,4,k)
                lhsm(ib,5,k)  = fac1*lhsm(ib,5,k)
                rhsx(ib,5,k)  = fac1*rhsx(ib,5,k)
                lhsm(ib,3,k1) = lhsm(ib,3,k1) -  &
     &                       lhsm(ib,2,k1)*lhsm(ib,4,k)
                lhsm(ib,4,k1) = lhsm(ib,4,k1) -  &
     &                       lhsm(ib,2,k1)*lhsm(ib,5,k)
                rhsx(ib,5,k1) = rhsx(ib,5,k1) -  &
     &                       lhsm(ib,2,k1)*rhsx(ib,5,k)
                lhsm(ib,2,k2) = lhsm(ib,2,k2) -  &
     &                       lhsm(ib,1,k2)*lhsm(ib,4,k)
                lhsm(ib,3,k2) = lhsm(ib,3,k2) -  &
     &                       lhsm(ib,1,k2)*lhsm(ib,5,k)
                rhsx(ib,5,k2) = rhsx(ib,5,k2) -  &
     &                       lhsm(ib,1,k2)*rhsx(ib,5,k)
             end do
          end do

!---------------------------------------------------------------------
!         And again the last two rows separately
!---------------------------------------------------------------------
          k  = grid_points(3)-2
          k1 = grid_points(3)-1
          do  ib = 1, bsize
             fac1       = 1.d0/lhsp(ib,3,k)
             lhsp(ib,4,k)  = fac1*lhsp(ib,4,k)
             lhsp(ib,5,k)  = fac1*lhsp(ib,5,k)
             rhsx(ib,4,k)  = fac1*rhsx(ib,4,k)
             lhsp(ib,3,k1) = lhsp(ib,3,k1) -  &
     &                    lhsp(ib,2,k1)*lhsp(ib,4,k)
             lhsp(ib,4,k1) = lhsp(ib,4,k1) -  &
     &                    lhsp(ib,2,k1)*lhsp(ib,5,k)
             rhsx(ib,4,k1) = rhsx(ib,4,k1) -  &
     &                    lhsp(ib,2,k1)*rhsx(ib,4,k)
             fac1       = 1.d0/lhsm(ib,3,k)
             lhsm(ib,4,k)  = fac1*lhsm(ib,4,k)
             lhsm(ib,5,k)  = fac1*lhsm(ib,5,k)
             rhsx(ib,5,k)  = fac1*rhsx(ib,5,k)
             lhsm(ib,3,k1) = lhsm(ib,3,k1) -  &
     &                    lhsm(ib,2,k1)*lhsm(ib,4,k)
             lhsm(ib,4,k1) = lhsm(ib,4,k1) -  &
     &                    lhsm(ib,2,k1)*lhsm(ib,5,k)
             rhsx(ib,5,k1) = rhsx(ib,5,k1) -  &
     &                    lhsm(ib,2,k1)*rhsx(ib,5,k)
!---------------------------------------------------------------------
!               Scale the last row immediately (some of this is overkill
!               if this is the last cell)
!---------------------------------------------------------------------
             rhsx(ib,4,k1) = rhsx(ib,4,k1)/lhsp(ib,3,k1)
             rhsx(ib,5,k1) = rhsx(ib,5,k1)/lhsm(ib,3,k1)
          end do


!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------

          k  = grid_points(3)-2
          k1 = grid_points(3)-1
          do  ib = 1, bsize
             rhsx(ib,1,k) = rhsx(ib,1,k) -  &
     &                             lhs(ib,4,k)*rhsx(ib,1,k1)
             rhsx(ib,2,k) = rhsx(ib,2,k) -  &
     &                             lhs(ib,4,k)*rhsx(ib,2,k1)
             rhsx(ib,3,k) = rhsx(ib,3,k) -  &
     &                             lhs(ib,4,k)*rhsx(ib,3,k1)

             rhsx(ib,4,k) = rhsx(ib,4,k) -  &
     &                             lhsp(ib,4,k)*rhsx(ib,4,k1)
             rhsx(ib,5,k) = rhsx(ib,5,k) -  &
     &                             lhsm(ib,4,k)*rhsx(ib,5,k1)
          end do

!---------------------------------------------------------------------
!      Whether or not this is the last processor, we always have
!      to complete the back-substitution 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      The first three factors
!---------------------------------------------------------------------
          do   k = grid_points(3)-3, 0, -1
             k1 = k  + 1
             k2 = k  + 2
             do  ib = 1, bsize
                rhsx(ib,1,k) = rhsx(ib,1,k) -  &
     &                          lhs(ib,4,k)*rhsx(ib,1,k1) -  &
     &                          lhs(ib,5,k)*rhsx(ib,1,k2)
                rhsx(ib,2,k) = rhsx(ib,2,k) -  &
     &                          lhs(ib,4,k)*rhsx(ib,2,k1) -  &
     &                          lhs(ib,5,k)*rhsx(ib,2,k2)
                rhsx(ib,3,k) = rhsx(ib,3,k) -  &
     &                          lhs(ib,4,k)*rhsx(ib,3,k1) -  &
     &                          lhs(ib,5,k)*rhsx(ib,3,k2)

!---------------------------------------------------------------------
!      And the remaining two
!---------------------------------------------------------------------
                rhsx(ib,4,k) = rhsx(ib,4,k) -  &
     &                          lhsp(ib,4,k)*rhsx(ib,4,k1) -  &
     &                          lhsp(ib,5,k)*rhsx(ib,4,k2)
                rhsx(ib,5,k) = rhsx(ib,5,k) -  &
     &                          lhsm(ib,4,k)*rhsx(ib,5,k1) -  &
     &                          lhsm(ib,5,k)*rhsx(ib,5,k2)
             end do
          end do

          do  k = 0, grid_points(3)-1
             do  ib = 1, im
                i = ib + ii - 1
                rhs(1,i,j,k) = rhsx(ib,1,k)
                rhs(2,i,j,k) = rhsx(ib,2,k)
                rhs(3,i,j,k) = rhsx(ib,3,k)
                rhs(4,i,j,k) = rhsx(ib,4,k)
                rhs(5,i,j,k) = rhsx(ib,5,k)
             end do
          end do

       end do
       end do
!$omp end do nowait
!$omp end parallel
       if (timeron) call timer_stop(t_zsolve)

       call tzetar

       return
       end



