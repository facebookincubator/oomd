
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

       integer i, j, k, k1, k2, m
       double precision ru1, fac1, fac2


!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! Prepare for z-solve, array redistribution   
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_zsolve)
!$omp parallel do default(shared) private(i,j,k,k1,k2,m,  &
!$omp&    ru1,fac1,fac2) collapse(2)
       do   j = 1, ny2
          do   i = 1, nx2

            call lhsinit(nz2+1, lhs, lhsp, lhsm)

!---------------------------------------------------------------------
! Computes the left hand side for the three z-factors   
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! first fill the lhs for the u-eigenvalue                          
!---------------------------------------------------------------------

             do   k = 0, nz2 + 1
                ru1 = c3c4*rho_i(i,j,k)
                cv(k) = ws(i,j,k)
                rhov(k) = dmax1(dz4 + con43 * ru1,  &
     &                          dz5 + c1c5 * ru1,  &
     &                          dzmax + ru1,  &
     &                          dz1)
             end do

             do   k =  1, nz2
                lhs(1,k) =  0.0d0
                lhs(2,k) = -dttz2 * cv(k-1) - dttz1 * rhov(k-1)
                lhs(3,k) =  1.0 + c2dttz1 * rhov(k)
                lhs(4,k) =  dttz2 * cv(k+1) - dttz1 * rhov(k+1)
                lhs(5,k) =  0.0d0
             end do

!---------------------------------------------------------------------
!      add fourth order dissipation                                  
!---------------------------------------------------------------------

             k = 1
             lhs(3,k) = lhs(3,k) + comz5
             lhs(4,k) = lhs(4,k) - comz4
             lhs(5,k) = lhs(5,k) + comz1

             k = 2
             lhs(2,k) = lhs(2,k) - comz4
             lhs(3,k) = lhs(3,k) + comz6
             lhs(4,k) = lhs(4,k) - comz4
             lhs(5,k) = lhs(5,k) + comz1

             do    k = 3, nz2-2
                lhs(1,k) = lhs(1,k) + comz1
                lhs(2,k) = lhs(2,k) - comz4
                lhs(3,k) = lhs(3,k) + comz6
                lhs(4,k) = lhs(4,k) - comz4
                lhs(5,k) = lhs(5,k) + comz1
             end do

             k = nz2-1
             lhs(1,k) = lhs(1,k) + comz1
             lhs(2,k) = lhs(2,k) - comz4
             lhs(3,k) = lhs(3,k) + comz6
             lhs(4,k) = lhs(4,k) - comz4

             k = nz2
             lhs(1,k) = lhs(1,k) + comz1
             lhs(2,k) = lhs(2,k) - comz4
             lhs(3,k) = lhs(3,k) + comz5


!---------------------------------------------------------------------
!      subsequently, fill the other factors (u+c), (u-c) 
!---------------------------------------------------------------------
             do    k = 1, nz2
                lhsp(1,k) = lhs(1,k)
                lhsp(2,k) = lhs(2,k) -  &
     &                            dttz2 * speed(i,j,k-1)
                lhsp(3,k) = lhs(3,k)
                lhsp(4,k) = lhs(4,k) +  &
     &                            dttz2 * speed(i,j,k+1)
                lhsp(5,k) = lhs(5,k)
                lhsm(1,k) = lhs(1,k)
                lhsm(2,k) = lhs(2,k) +  &
     &                            dttz2 * speed(i,j,k-1)
                lhsm(3,k) = lhs(3,k)
                lhsm(4,k) = lhs(4,k) -  &
     &                            dttz2 * speed(i,j,k+1)
                lhsm(5,k) = lhs(5,k)
             end do


!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------

             do    k = 0, grid_points(3)-3
                k1 = k  + 1
                k2 = k  + 2
                fac1      = 1.d0/lhs(3,k)
                lhs(4,k)  = fac1*lhs(4,k)
                lhs(5,k)  = fac1*lhs(5,k)
                do    m = 1, 3
                   rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                end do
                lhs(3,k1) = lhs(3,k1) -  &
     &                         lhs(2,k1)*lhs(4,k)
                lhs(4,k1) = lhs(4,k1) -  &
     &                         lhs(2,k1)*lhs(5,k)
                do    m = 1, 3
                   rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                         lhs(2,k1)*rhs(m,i,j,k)
                end do
                lhs(2,k2) = lhs(2,k2) -  &
     &                         lhs(1,k2)*lhs(4,k)
                lhs(3,k2) = lhs(3,k2) -  &
     &                         lhs(1,k2)*lhs(5,k)
                do    m = 1, 3
                   rhs(m,i,j,k2) = rhs(m,i,j,k2) -  &
     &                         lhs(1,k2)*rhs(m,i,j,k)
                end do
             end do

!---------------------------------------------------------------------
!      The last two rows in this grid block are a bit different, 
!      since they do not have two more rows available for the
!      elimination of off-diagonal entries
!---------------------------------------------------------------------
             k  = grid_points(3)-2
             k1 = grid_points(3)-1
             fac1      = 1.d0/lhs(3,k)
             lhs(4,k)  = fac1*lhs(4,k)
             lhs(5,k)  = fac1*lhs(5,k)
             do    m = 1, 3
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             end do
             lhs(3,k1) = lhs(3,k1) -  &
     &                      lhs(2,k1)*lhs(4,k)
             lhs(4,k1) = lhs(4,k1) -  &
     &                      lhs(2,k1)*lhs(5,k)
             do    m = 1, 3
                rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                      lhs(2,k1)*rhs(m,i,j,k)
             end do
!---------------------------------------------------------------------
!               scale the last row immediately
!---------------------------------------------------------------------
             fac2      = 1.d0/lhs(3,k1)
             do    m = 1, 3
                rhs(m,i,j,k1) = fac2*rhs(m,i,j,k1)
             end do

!---------------------------------------------------------------------
!      do the u+c and the u-c factors               
!---------------------------------------------------------------------
             do    k = 0, grid_points(3)-3
                k1 = k  + 1
                k2 = k  + 2
                m = 4
                fac1       = 1.d0/lhsp(3,k)
                lhsp(4,k)  = fac1*lhsp(4,k)
                lhsp(5,k)  = fac1*lhsp(5,k)
                rhs(m,i,j,k)  = fac1*rhs(m,i,j,k)
                lhsp(3,k1) = lhsp(3,k1) -  &
     &                       lhsp(2,k1)*lhsp(4,k)
                lhsp(4,k1) = lhsp(4,k1) -  &
     &                       lhsp(2,k1)*lhsp(5,k)
                rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                       lhsp(2,k1)*rhs(m,i,j,k)
                lhsp(2,k2) = lhsp(2,k2) -  &
     &                       lhsp(1,k2)*lhsp(4,k)
                lhsp(3,k2) = lhsp(3,k2) -  &
     &                       lhsp(1,k2)*lhsp(5,k)
                rhs(m,i,j,k2) = rhs(m,i,j,k2) -  &
     &                       lhsp(1,k2)*rhs(m,i,j,k)
                m = 5
                fac1       = 1.d0/lhsm(3,k)
                lhsm(4,k)  = fac1*lhsm(4,k)
                lhsm(5,k)  = fac1*lhsm(5,k)
                rhs(m,i,j,k)  = fac1*rhs(m,i,j,k)
                lhsm(3,k1) = lhsm(3,k1) -  &
     &                       lhsm(2,k1)*lhsm(4,k)
                lhsm(4,k1) = lhsm(4,k1) -  &
     &                       lhsm(2,k1)*lhsm(5,k)
                rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                       lhsm(2,k1)*rhs(m,i,j,k)
                lhsm(2,k2) = lhsm(2,k2) -  &
     &                       lhsm(1,k2)*lhsm(4,k)
                lhsm(3,k2) = lhsm(3,k2) -  &
     &                       lhsm(1,k2)*lhsm(5,k)
                rhs(m,i,j,k2) = rhs(m,i,j,k2) -  &
     &                       lhsm(1,k2)*rhs(m,i,j,k)
             end do

!---------------------------------------------------------------------
!         And again the last two rows separately
!---------------------------------------------------------------------
             k  = grid_points(3)-2
             k1 = grid_points(3)-1
             m = 4
             fac1       = 1.d0/lhsp(3,k)
             lhsp(4,k)  = fac1*lhsp(4,k)
             lhsp(5,k)  = fac1*lhsp(5,k)
             rhs(m,i,j,k)  = fac1*rhs(m,i,j,k)
             lhsp(3,k1) = lhsp(3,k1) -  &
     &                    lhsp(2,k1)*lhsp(4,k)
             lhsp(4,k1) = lhsp(4,k1) -  &
     &                    lhsp(2,k1)*lhsp(5,k)
             rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                    lhsp(2,k1)*rhs(m,i,j,k)
             m = 5
             fac1       = 1.d0/lhsm(3,k)
             lhsm(4,k)  = fac1*lhsm(4,k)
             lhsm(5,k)  = fac1*lhsm(5,k)
             rhs(m,i,j,k)  = fac1*rhs(m,i,j,k)
             lhsm(3,k1) = lhsm(3,k1) -  &
     &                    lhsm(2,k1)*lhsm(4,k)
             lhsm(4,k1) = lhsm(4,k1) -  &
     &                    lhsm(2,k1)*lhsm(5,k)
             rhs(m,i,j,k1) = rhs(m,i,j,k1) -  &
     &                    lhsm(2,k1)*rhs(m,i,j,k)
!---------------------------------------------------------------------
!               Scale the last row immediately (some of this is overkill
!               if this is the last cell)
!---------------------------------------------------------------------
             rhs(4,i,j,k1) = rhs(4,i,j,k1)/lhsp(3,k1)
             rhs(5,i,j,k1) = rhs(5,i,j,k1)/lhsm(3,k1)


!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------

             k  = grid_points(3)-2
             k1 = grid_points(3)-1
             do   m = 1, 3
                rhs(m,i,j,k) = rhs(m,i,j,k) -  &
     &                             lhs(4,k)*rhs(m,i,j,k1)
             end do

             rhs(4,i,j,k) = rhs(4,i,j,k) -  &
     &                             lhsp(4,k)*rhs(4,i,j,k1)
             rhs(5,i,j,k) = rhs(5,i,j,k) -  &
     &                             lhsm(4,k)*rhs(5,i,j,k1)

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
                do   m = 1, 3
                   rhs(m,i,j,k) = rhs(m,i,j,k) -  &
     &                          lhs(4,k)*rhs(m,i,j,k1) -  &
     &                          lhs(5,k)*rhs(m,i,j,k2)
                end do

!---------------------------------------------------------------------
!      And the remaining two
!---------------------------------------------------------------------
                rhs(4,i,j,k) = rhs(4,i,j,k) -  &
     &                          lhsp(4,k)*rhs(4,i,j,k1) -  &
     &                          lhsp(5,k)*rhs(4,i,j,k2)
                rhs(5,i,j,k) = rhs(5,i,j,k) -  &
     &                          lhsm(4,k)*rhs(5,i,j,k1) -  &
     &                          lhsm(5,k)*rhs(5,i,j,k2)
             end do

          end do
       end do
       if (timeron) call timer_stop(t_zsolve)

       call tzetar

       return
       end



