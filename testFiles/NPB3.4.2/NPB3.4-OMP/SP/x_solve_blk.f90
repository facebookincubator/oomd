
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine x_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function performs the solution of the approximate factorization
! step in the x-direction for all five matrix components
! simultaneously. The Thomas algorithm is employed to solve the
! systems for the x-lines. Boundary conditions are non-periodic
!---------------------------------------------------------------------

       use sp_data
       use work_lhs

       implicit none

       integer i, j, k, i1, i2, jj, jb, jm
       double precision  ru1, fac1, fac2


!---------------------------------------------------------------------
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_xsolve)
!$omp parallel default(shared) private(i,j,k,i1,i2,jj,jb,jm,  &
!$omp&    ru1,fac1,fac2)

       call lhsinit(nx2+1)

!$omp do collapse(2)
       do  k = 1, nz2
       do  jj = 1, ny2, bsize
          jm = min(bsize, ny2 - jj + 1)

!---------------------------------------------------------------------
! To improve cache utilization, copy a slab of rhs to temp array  
!---------------------------------------------------------------------
          do  i = 0, grid_points(1)-1
             do  jb = 1, bsize
                j = min(jb,jm) + jj - 1
                rhsx(jb,1,i) = rhs(1,i,j,k)
                rhsx(jb,2,i) = rhs(2,i,j,k)
                rhsx(jb,3,i) = rhs(3,i,j,k)
                rhsx(jb,4,i) = rhs(4,i,j,k)
                rhsx(jb,5,i) = rhs(5,i,j,k)
             end do
          end do

!---------------------------------------------------------------------
! Computes the left hand side for the three x-factors  
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      first fill the lhs for the u-eigenvalue                   
!---------------------------------------------------------------------
          do  i = 0, grid_points(1)-1
             do  jb = 1, bsize
                j = min(jb,jm) + jj - 1
                ru1 = c3c4*rho_i(i,j,k)
                cv(jb,i) = us(i,j,k)
                rhov(jb,i) = dmax1(dx2+con43*ru1,  &
     &                          dx5+c1c5*ru1,  &
     &                          dxmax+ru1,  &
     &                          dx1)
             end do
          end do

          do  i = 1, nx2
             do  jb = 1, bsize
                lhs(jb,1,i) =  0.0d0
                lhs(jb,2,i) = -dttx2 * cv(jb,i-1) - dttx1 * rhov(jb,i-1)
                lhs(jb,3,i) =  1.0d0 + c2dttx1 * rhov(jb,i)
                lhs(jb,4,i) =  dttx2 * cv(jb,i+1) - dttx1 * rhov(jb,i+1)
                lhs(jb,5,i) =  0.0d0
             end do
          end do

!---------------------------------------------------------------------
!      add fourth order dissipation                             
!---------------------------------------------------------------------

          do  jb = 1, bsize
             i = 1
             lhs(jb,3,i) = lhs(jb,3,i) + comz5
             lhs(jb,4,i) = lhs(jb,4,i) - comz4
             lhs(jb,5,i) = lhs(jb,5,i) + comz1
  
             i = 2
             lhs(jb,2,i) = lhs(jb,2,i) - comz4
             lhs(jb,3,i) = lhs(jb,3,i) + comz6
             lhs(jb,4,i) = lhs(jb,4,i) - comz4
             lhs(jb,5,i) = lhs(jb,5,i) + comz1
          end do

          do   i=3, grid_points(1)-4
             do  jb = 1, bsize
                lhs(jb,1,i) = lhs(jb,1,i) + comz1
                lhs(jb,2,i) = lhs(jb,2,i) - comz4
                lhs(jb,3,i) = lhs(jb,3,i) + comz6
                lhs(jb,4,i) = lhs(jb,4,i) - comz4
                lhs(jb,5,i) = lhs(jb,5,i) + comz1
             end do
          end do

          do  jb = 1, bsize
             i = grid_points(1)-3
             lhs(jb,1,i) = lhs(jb,1,i) + comz1
             lhs(jb,2,i) = lhs(jb,2,i) - comz4
             lhs(jb,3,i) = lhs(jb,3,i) + comz6
             lhs(jb,4,i) = lhs(jb,4,i) - comz4

             i = grid_points(1)-2
             lhs(jb,1,i) = lhs(jb,1,i) + comz1
             lhs(jb,2,i) = lhs(jb,2,i) - comz4
             lhs(jb,3,i) = lhs(jb,3,i) + comz5
          end do

!---------------------------------------------------------------------
!      subsequently, fill the other factors (u+c), (u-c) by adding to 
!      the first  
!---------------------------------------------------------------------
          do   i = 1, nx2
             do  jb = 1, bsize
                j = min(jb,jm) + jj - 1
                lhsp(jb,1,i) = lhs(jb,1,i)
                lhsp(jb,2,i) = lhs(jb,2,i) -  &
     &                            dttx2 * speed(i-1,j,k)
                lhsp(jb,3,i) = lhs(jb,3,i)
                lhsp(jb,4,i) = lhs(jb,4,i) +  &
     &                            dttx2 * speed(i+1,j,k)
                lhsp(jb,5,i) = lhs(jb,5,i)
                lhsm(jb,1,i) = lhs(jb,1,i)
                lhsm(jb,2,i) = lhs(jb,2,i) +  &
     &                            dttx2 * speed(i-1,j,k)
                lhsm(jb,3,i) = lhs(jb,3,i)
                lhsm(jb,4,i) = lhs(jb,4,i) -  &
     &                            dttx2 * speed(i+1,j,k)
                lhsm(jb,5,i) = lhs(jb,5,i)
             end do
          end do

!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      perform the Thomas algorithm; first, FORWARD ELIMINATION     
!---------------------------------------------------------------------

          do    i = 0, grid_points(1)-3
             i1 = i  + 1
             i2 = i  + 2
             do  jb = 1, bsize
                fac1      = 1.d0/lhs(jb,3,i)
                lhs(jb,4,i)  = fac1*lhs(jb,4,i)
                lhs(jb,5,i)  = fac1*lhs(jb,5,i)
                rhsx(jb,1,i) = fac1*rhsx(jb,1,i)
                rhsx(jb,2,i) = fac1*rhsx(jb,2,i)
                rhsx(jb,3,i) = fac1*rhsx(jb,3,i)
                lhs(jb,3,i1) = lhs(jb,3,i1) -  &
     &                         lhs(jb,2,i1)*lhs(jb,4,i)
                lhs(jb,4,i1) = lhs(jb,4,i1) -  &
     &                         lhs(jb,2,i1)*lhs(jb,5,i)
                rhsx(jb,1,i1) = rhsx(jb,1,i1) -  &
     &                         lhs(jb,2,i1)*rhsx(jb,1,i)
                rhsx(jb,2,i1) = rhsx(jb,2,i1) -  &
     &                         lhs(jb,2,i1)*rhsx(jb,2,i)
                rhsx(jb,3,i1) = rhsx(jb,3,i1) -  &
     &                         lhs(jb,2,i1)*rhsx(jb,3,i)
                lhs(jb,2,i2) = lhs(jb,2,i2) -  &
     &                         lhs(jb,1,i2)*lhs(jb,4,i)
                lhs(jb,3,i2) = lhs(jb,3,i2) -  &
     &                         lhs(jb,1,i2)*lhs(jb,5,i)
                rhsx(jb,1,i2) = rhsx(jb,1,i2) -  &
     &                         lhs(jb,1,i2)*rhsx(jb,1,i)
                rhsx(jb,2,i2) = rhsx(jb,2,i2) -  &
     &                         lhs(jb,1,i2)*rhsx(jb,2,i)
                rhsx(jb,3,i2) = rhsx(jb,3,i2) -  &
     &                         lhs(jb,1,i2)*rhsx(jb,3,i)
             end do
          end do

!---------------------------------------------------------------------
!      The last two rows in this grid block are a bit different, 
!      since they do not have two more rows available for the
!      elimination of off-diagonal entries
!---------------------------------------------------------------------

          i  = grid_points(1)-2
          i1 = grid_points(1)-1
          do  jb = 1, bsize
             fac1      = 1.d0/lhs(jb,3,i)
             lhs(jb,4,i)  = fac1*lhs(jb,4,i)
             lhs(jb,5,i)  = fac1*lhs(jb,5,i)
             rhsx(jb,1,i) = fac1*rhsx(jb,1,i)
             rhsx(jb,2,i) = fac1*rhsx(jb,2,i)
             rhsx(jb,3,i) = fac1*rhsx(jb,3,i)
             lhs(jb,3,i1) = lhs(jb,3,i1) -  &
     &                      lhs(jb,2,i1)*lhs(jb,4,i)
             lhs(jb,4,i1) = lhs(jb,4,i1) -  &
     &                      lhs(jb,2,i1)*lhs(jb,5,i)
             rhsx(jb,1,i1) = rhsx(jb,1,i1) -  &
     &                      lhs(jb,2,i1)*rhsx(jb,1,i)
             rhsx(jb,2,i1) = rhsx(jb,2,i1) -  &
     &                      lhs(jb,2,i1)*rhsx(jb,2,i)
             rhsx(jb,3,i1) = rhsx(jb,3,i1) -  &
     &                      lhs(jb,2,i1)*rhsx(jb,3,i)
!---------------------------------------------------------------------
!            scale the last row immediately 
!---------------------------------------------------------------------
             fac2             = 1.d0/lhs(jb,3,i1)
             rhsx(jb,1,i1) = fac2*rhsx(jb,1,i1)
             rhsx(jb,2,i1) = fac2*rhsx(jb,2,i1)
             rhsx(jb,3,i1) = fac2*rhsx(jb,3,i1)
          end do

!---------------------------------------------------------------------
!      do the u+c and the u-c factors                 
!---------------------------------------------------------------------

          do    i = 0, grid_points(1)-3
             i1 = i  + 1
             i2 = i  + 2
             do  jb = 1, bsize
                fac1       = 1.d0/lhsp(jb,3,i)
                lhsp(jb,4,i)  = fac1*lhsp(jb,4,i)
                lhsp(jb,5,i)  = fac1*lhsp(jb,5,i)
                rhsx(jb,4,i)  = fac1*rhsx(jb,4,i)
                lhsp(jb,3,i1) = lhsp(jb,3,i1) -  &
     &                        lhsp(jb,2,i1)*lhsp(jb,4,i)
                lhsp(jb,4,i1) = lhsp(jb,4,i1) -  &
     &                        lhsp(jb,2,i1)*lhsp(jb,5,i)
                rhsx(jb,4,i1) = rhsx(jb,4,i1) -  &
     &                        lhsp(jb,2,i1)*rhsx(jb,4,i)
                lhsp(jb,2,i2) = lhsp(jb,2,i2) -  &
     &                        lhsp(jb,1,i2)*lhsp(jb,4,i)
                lhsp(jb,3,i2) = lhsp(jb,3,i2) -  &
     &                        lhsp(jb,1,i2)*lhsp(jb,5,i)
                rhsx(jb,4,i2) = rhsx(jb,4,i2) -  &
     &                        lhsp(jb,1,i2)*rhsx(jb,4,i)
                fac1       = 1.d0/lhsm(jb,3,i)
                lhsm(jb,4,i)  = fac1*lhsm(jb,4,i)
                lhsm(jb,5,i)  = fac1*lhsm(jb,5,i)
                rhsx(jb,5,i)  = fac1*rhsx(jb,5,i)
                lhsm(jb,3,i1) = lhsm(jb,3,i1) -  &
     &                        lhsm(jb,2,i1)*lhsm(jb,4,i)
                lhsm(jb,4,i1) = lhsm(jb,4,i1) -  &
     &                        lhsm(jb,2,i1)*lhsm(jb,5,i)
                rhsx(jb,5,i1) = rhsx(jb,5,i1) -  &
     &                        lhsm(jb,2,i1)*rhsx(jb,5,i)
                lhsm(jb,2,i2) = lhsm(jb,2,i2) -  &
     &                        lhsm(jb,1,i2)*lhsm(jb,4,i)
                lhsm(jb,3,i2) = lhsm(jb,3,i2) -  &
     &                        lhsm(jb,1,i2)*lhsm(jb,5,i)
                rhsx(jb,5,i2) = rhsx(jb,5,i2) -  &
     &                        lhsm(jb,1,i2)*rhsx(jb,5,i)
             end do
          end do

!---------------------------------------------------------------------
!         And again the last two rows separately
!---------------------------------------------------------------------
          i  = grid_points(1)-2
          i1 = grid_points(1)-1
          do  jb = 1, bsize
             fac1       = 1.d0/lhsp(jb,3,i)
             lhsp(jb,4,i)  = fac1*lhsp(jb,4,i)
             lhsp(jb,5,i)  = fac1*lhsp(jb,5,i)
             rhsx(jb,4,i)  = fac1*rhsx(jb,4,i)
             lhsp(jb,3,i1) = lhsp(jb,3,i1) -  &
     &                      lhsp(jb,2,i1)*lhsp(jb,4,i)
             lhsp(jb,4,i1) = lhsp(jb,4,i1) -  &
     &                      lhsp(jb,2,i1)*lhsp(jb,5,i)
             rhsx(jb,4,i1) = rhsx(jb,4,i1) -  &
     &                      lhsp(jb,2,i1)*rhsx(jb,4,i)
             fac1       = 1.d0/lhsm(jb,3,i)
             lhsm(jb,4,i)  = fac1*lhsm(jb,4,i)
             lhsm(jb,5,i)  = fac1*lhsm(jb,5,i)
             rhsx(jb,5,i)  = fac1*rhsx(jb,5,i)
             lhsm(jb,3,i1) = lhsm(jb,3,i1) -  &
     &                      lhsm(jb,2,i1)*lhsm(jb,4,i)
             lhsm(jb,4,i1) = lhsm(jb,4,i1) -  &
     &                      lhsm(jb,2,i1)*lhsm(jb,5,i)
             rhsx(jb,5,i1) = rhsx(jb,5,i1) -  &
     &                      lhsm(jb,2,i1)*rhsx(jb,5,i)
!---------------------------------------------------------------------
!               Scale the last row immediately
!---------------------------------------------------------------------
             rhsx(jb,4,i1) = rhsx(jb,4,i1)/lhsp(jb,3,i1)
             rhsx(jb,5,i1) = rhsx(jb,5,i1)/lhsm(jb,3,i1)
          end do


!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------


          i  = grid_points(1)-2
          i1 = grid_points(1)-1
          do  jb = 1, bsize
             rhsx(jb,1,i) = rhsx(jb,1,i) -  &
     &                             lhs(jb,4,i)*rhsx(jb,1,i1)
             rhsx(jb,2,i) = rhsx(jb,2,i) -  &
     &                             lhs(jb,4,i)*rhsx(jb,2,i1)
             rhsx(jb,3,i) = rhsx(jb,3,i) -  &
     &                             lhs(jb,4,i)*rhsx(jb,3,i1)

             rhsx(jb,4,i) = rhsx(jb,4,i) -  &
     &                          lhsp(jb,4,i)*rhsx(jb,4,i1)
             rhsx(jb,5,i) = rhsx(jb,5,i) -  &
     &                          lhsm(jb,4,i)*rhsx(jb,5,i1)
          end do

!---------------------------------------------------------------------
!      The first three factors
!---------------------------------------------------------------------
          do    i = grid_points(1)-3, 0, -1
             i1 = i  + 1
             i2 = i  + 2
             do  jb = 1, bsize
                rhsx(jb,1,i) = rhsx(jb,1,i) -  &
     &                          lhs(jb,4,i)*rhsx(jb,1,i1) -  &
     &                          lhs(jb,5,i)*rhsx(jb,1,i2)
                rhsx(jb,2,i) = rhsx(jb,2,i) -  &
     &                          lhs(jb,4,i)*rhsx(jb,2,i1) -  &
     &                          lhs(jb,5,i)*rhsx(jb,2,i2)
                rhsx(jb,3,i) = rhsx(jb,3,i) -  &
     &                          lhs(jb,4,i)*rhsx(jb,3,i1) -  &
     &                          lhs(jb,5,i)*rhsx(jb,3,i2)

!---------------------------------------------------------------------
!      And the remaining two
!---------------------------------------------------------------------
                rhsx(jb,4,i) = rhsx(jb,4,i) -  &
     &                          lhsp(jb,4,i)*rhsx(jb,4,i1) -  &
     &                          lhsp(jb,5,i)*rhsx(jb,4,i2)
                rhsx(jb,5,i) = rhsx(jb,5,i) -  &
     &                          lhsm(jb,4,i)*rhsx(jb,5,i1) -  &
     &                          lhsm(jb,5,i)*rhsx(jb,5,i2)
             end do
          end do

          do  jb = 1, jm
             j = jb + jj - 1
             do  i = 0, grid_points(1)-1
                rhs(1,i,j,k) = rhsx(jb,1,i)
                rhs(2,i,j,k) = rhsx(jb,2,i)
                rhs(3,i,j,k) = rhsx(jb,3,i)
                rhs(4,i,j,k) = rhsx(jb,4,i)
                rhs(5,i,j,k) = rhsx(jb,5,i)
             end do
          end do

       end do
       end do
!$omp end do nowait
!$omp end parallel
       if (timeron) call timer_stop(t_xsolve)

!---------------------------------------------------------------------
!      Do the block-diagonal inversion          
!---------------------------------------------------------------------
       call ninvr

       return
       end


