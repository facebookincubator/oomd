
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

       integer i, j, k, j1, j2, m
       double precision ru1, fac1, fac2


!---------------------------------------------------------------------
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_ysolve)
!$omp parallel do default(shared) private(i,j,k,j1,j2,m,  &
!$omp&    ru1,fac1,fac2) collapse(2)
       do  k = 1, nz2
          do  i = 1, grid_points(1)-2

            call lhsinit(ny2+1, lhs, lhsp, lhsm)

!---------------------------------------------------------------------
! Computes the left hand side for the three y-factors   
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      first fill the lhs for the u-eigenvalue         
!---------------------------------------------------------------------

             do  j = 0, grid_points(2)-1
                ru1 = c3c4*rho_i(i,j,k)
                cv(j) = vs(i,j,k)
                rhov(j) = dmax1( dy3 + con43 * ru1,  &
     &                           dy5 + c1c5*ru1,  &
     &                           dymax + ru1,  &
     &                           dy1)
             end do
            
             do  j = 1, grid_points(2)-2
                lhs(1,j) =  0.0d0
                lhs(2,j) = -dtty2 * cv(j-1) - dtty1 * rhov(j-1)
                lhs(3,j) =  1.0 + c2dtty1 * rhov(j)
                lhs(4,j) =  dtty2 * cv(j+1) - dtty1 * rhov(j+1)
                lhs(5,j) =  0.0d0
             end do

!---------------------------------------------------------------------
!      add fourth order dissipation                             
!---------------------------------------------------------------------

             j = 1
             lhs(3,j) = lhs(3,j) + comz5
             lhs(4,j) = lhs(4,j) - comz4
             lhs(5,j) = lhs(5,j) + comz1
       
             lhs(2,j+1) = lhs(2,j+1) - comz4
             lhs(3,j+1) = lhs(3,j+1) + comz6
             lhs(4,j+1) = lhs(4,j+1) - comz4
             lhs(5,j+1) = lhs(5,j+1) + comz1

             do   j=3, grid_points(2)-4
                lhs(1,j) = lhs(1,j) + comz1
                lhs(2,j) = lhs(2,j) - comz4
                lhs(3,j) = lhs(3,j) + comz6
                lhs(4,j) = lhs(4,j) - comz4
                lhs(5,j) = lhs(5,j) + comz1
             end do

             j = grid_points(2)-3
             lhs(1,j) = lhs(1,j) + comz1
             lhs(2,j) = lhs(2,j) - comz4
             lhs(3,j) = lhs(3,j) + comz6
             lhs(4,j) = lhs(4,j) - comz4

             lhs(1,j+1) = lhs(1,j+1) + comz1
             lhs(2,j+1) = lhs(2,j+1) - comz4
             lhs(3,j+1) = lhs(3,j+1) + comz5

!---------------------------------------------------------------------
!      subsequently, do the other two factors                    
!---------------------------------------------------------------------
             do    j = 1, grid_points(2)-2
                lhsp(1,j) = lhs(1,j)
                lhsp(2,j) = lhs(2,j) -  &
     &                            dtty2 * speed(i,j-1,k)
                lhsp(3,j) = lhs(3,j)
                lhsp(4,j) = lhs(4,j) +  &
     &                            dtty2 * speed(i,j+1,k)
                lhsp(5,j) = lhs(5,j)
                lhsm(1,j) = lhs(1,j)
                lhsm(2,j) = lhs(2,j) +  &
     &                            dtty2 * speed(i,j-1,k)
                lhsm(3,j) = lhs(3,j)
                lhsm(4,j) = lhs(4,j) -  &
     &                            dtty2 * speed(i,j+1,k)
                lhsm(5,j) = lhs(5,j)
             end do


!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------

             do    j = 0, grid_points(2)-3
                j1 = j  + 1
                j2 = j  + 2
                fac1      = 1.d0/lhs(3,j)
                lhs(4,j)  = fac1*lhs(4,j)
                lhs(5,j)  = fac1*lhs(5,j)
                do    m = 1, 3
                   rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                end do
                lhs(3,j1) = lhs(3,j1) -  &
     &                         lhs(2,j1)*lhs(4,j)
                lhs(4,j1) = lhs(4,j1) -  &
     &                         lhs(2,j1)*lhs(5,j)
                do    m = 1, 3
                   rhs(m,i,j1,k) = rhs(m,i,j1,k) -  &
     &                         lhs(2,j1)*rhs(m,i,j,k)
                end do
                lhs(2,j2) = lhs(2,j2) -  &
     &                         lhs(1,j2)*lhs(4,j)
                lhs(3,j2) = lhs(3,j2) -  &
     &                         lhs(1,j2)*lhs(5,j)
                do    m = 1, 3
                   rhs(m,i,j2,k) = rhs(m,i,j2,k) -  &
     &                         lhs(1,j2)*rhs(m,i,j,k)
                end do
             end do

!---------------------------------------------------------------------
!      The last two rows in this grid block are a bit different, 
!      since they do not have two more rows available for the
!      elimination of off-diagonal entries
!---------------------------------------------------------------------

             j  = grid_points(2)-2
             j1 = grid_points(2)-1
             fac1      = 1.d0/lhs(3,j)
             lhs(4,j)  = fac1*lhs(4,j)
             lhs(5,j)  = fac1*lhs(5,j)
             do    m = 1, 3
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             end do
             lhs(3,j1) = lhs(3,j1) -  &
     &                      lhs(2,j1)*lhs(4,j)
             lhs(4,j1) = lhs(4,j1) -  &
     &                      lhs(2,j1)*lhs(5,j)
             do    m = 1, 3
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -  &
     &                      lhs(2,j1)*rhs(m,i,j,k)
             end do
!---------------------------------------------------------------------
!            scale the last row immediately 
!---------------------------------------------------------------------
             fac2      = 1.d0/lhs(3,j1)
             do    m = 1, 3
                rhs(m,i,j1,k) = fac2*rhs(m,i,j1,k)
             end do

!---------------------------------------------------------------------
!      do the u+c and the u-c factors                 
!---------------------------------------------------------------------
             do    j = 0, grid_points(2)-3
                j1 = j  + 1
                j2 = j  + 2
                m = 4
                fac1       = 1.d0/lhsp(3,j)
                lhsp(4,j)  = fac1*lhsp(4,j)
                lhsp(5,j)  = fac1*lhsp(5,j)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsp(3,j1) = lhsp(3,j1) -  &
     &                       lhsp(2,j1)*lhsp(4,j)
                lhsp(4,j1) = lhsp(4,j1) -  &
     &                       lhsp(2,j1)*lhsp(5,j)
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -  &
     &                       lhsp(2,j1)*rhs(m,i,j,k)
                lhsp(2,j2) = lhsp(2,j2) -  &
     &                       lhsp(1,j2)*lhsp(4,j)
                lhsp(3,j2) = lhsp(3,j2) -  &
     &                       lhsp(1,j2)*lhsp(5,j)
                rhs(m,i,j2,k) = rhs(m,i,j2,k) -  &
     &                       lhsp(1,j2)*rhs(m,i,j,k)
                m = 5
                fac1       = 1.d0/lhsm(3,j)
                lhsm(4,j)  = fac1*lhsm(4,j)
                lhsm(5,j)  = fac1*lhsm(5,j)
                rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
                lhsm(3,j1) = lhsm(3,j1) -  &
     &                       lhsm(2,j1)*lhsm(4,j)
                lhsm(4,j1) = lhsm(4,j1) -  &
     &                       lhsm(2,j1)*lhsm(5,j)
                rhs(m,i,j1,k) = rhs(m,i,j1,k) -  &
     &                       lhsm(2,j1)*rhs(m,i,j,k)
                lhsm(2,j2) = lhsm(2,j2) -  &
     &                       lhsm(1,j2)*lhsm(4,j)
                lhsm(3,j2) = lhsm(3,j2) -  &
     &                       lhsm(1,j2)*lhsm(5,j)
                rhs(m,i,j2,k) = rhs(m,i,j2,k) -  &
     &                       lhsm(1,j2)*rhs(m,i,j,k)
             end do

!---------------------------------------------------------------------
!         And again the last two rows separately
!---------------------------------------------------------------------
             j  = grid_points(2)-2
             j1 = grid_points(2)-1
             m = 4
             fac1       = 1.d0/lhsp(3,j)
             lhsp(4,j)  = fac1*lhsp(4,j)
             lhsp(5,j)  = fac1*lhsp(5,j)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsp(3,j1) = lhsp(3,j1) -  &
     &                    lhsp(2,j1)*lhsp(4,j)
             lhsp(4,j1) = lhsp(4,j1) -  &
     &                    lhsp(2,j1)*lhsp(5,j)
             rhs(m,i,j1,k)   = rhs(m,i,j1,k) -  &
     &                    lhsp(2,j1)*rhs(m,i,j,k)
             m = 5
             fac1       = 1.d0/lhsm(3,j)
             lhsm(4,j)  = fac1*lhsm(4,j)
             lhsm(5,j)  = fac1*lhsm(5,j)
             rhs(m,i,j,k) = fac1*rhs(m,i,j,k)
             lhsm(3,j1) = lhsm(3,j1) -  &
     &                    lhsm(2,j1)*lhsm(4,j)
             lhsm(4,j1) = lhsm(4,j1) -  &
     &                    lhsm(2,j1)*lhsm(5,j)
             rhs(m,i,j1,k)   = rhs(m,i,j1,k) -  &
     &                    lhsm(2,j1)*rhs(m,i,j,k)
!---------------------------------------------------------------------
!               Scale the last row immediately 
!---------------------------------------------------------------------
             rhs(4,i,j1,k)   = rhs(4,i,j1,k)/lhsp(3,j1)
             rhs(5,i,j1,k)   = rhs(5,i,j1,k)/lhsm(3,j1)


!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------

             j  = grid_points(2)-2
             j1 = grid_points(2)-1
             do   m = 1, 3
                rhs(m,i,j,k) = rhs(m,i,j,k) -  &
     &                           lhs(4,j)*rhs(m,i,j1,k)
             end do

             rhs(4,i,j,k) = rhs(4,i,j,k) -  &
     &                           lhsp(4,j)*rhs(4,i,j1,k)
             rhs(5,i,j,k) = rhs(5,i,j,k) -  &
     &                           lhsm(4,j)*rhs(5,i,j1,k)

!---------------------------------------------------------------------
!      The first three factors
!---------------------------------------------------------------------
             do   j = grid_points(2)-3, 0, -1
                j1 = j  + 1
                j2 = j  + 2
                do   m = 1, 3
                   rhs(m,i,j,k) = rhs(m,i,j,k) -  &
     &                          lhs(4,j)*rhs(m,i,j1,k) -  &
     &                          lhs(5,j)*rhs(m,i,j2,k)
                end do

!---------------------------------------------------------------------
!      And the remaining two
!---------------------------------------------------------------------
                rhs(4,i,j,k) = rhs(4,i,j,k) -  &
     &                          lhsp(4,j)*rhs(4,i,j1,k) -  &
     &                          lhsp(5,j)*rhs(4,i,j2,k)
                rhs(5,i,j,k) = rhs(5,i,j,k) -  &
     &                          lhsm(4,j)*rhs(5,i,j1,k) -  &
     &                          lhsm(5,j)*rhs(5,i,j2,k)
             end do

          end do
       end do
       if (timeron) call timer_stop(t_ysolve)


       call pinvr

       return
       end


