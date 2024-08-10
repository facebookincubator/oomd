
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine lhsz(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! This function computes the left hand side for the three z-factors   
!---------------------------------------------------------------------

       use sp_data
       implicit none

       double precision ru1
       integer i, j, k, c

!---------------------------------------------------------------------
!      treat only cell c                                         
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! first fill the lhs for the u-eigenvalue                          
!---------------------------------------------------------------------
       do   j = start(2,c), cell_size(2,c)-end(2,c)-1
          do   i = start(1,c), cell_size(1,c)-end(1,c)-1

             do   k = start(3,c)-1, cell_size(3,c)-end(3,c)
                ru1 = c3c4*rho_i(i,j,k,c)
                cv(k) = ws(i,j,k,c)
                rhos(k) = dmax1(dz4 + con43 * ru1,  &
     &                          dz5 + c1c5 * ru1,  &
     &                          dzmax + ru1,  &
     &                          dz1)
             end do

             do   k =  start(3,c), cell_size(3,c)-end(3,c)-1
                lhs(i,j,k,1,c) =  0.0d0
                lhs(i,j,k,2,c) = -dttz2 * cv(k-1) - dttz1 * rhos(k-1)
                lhs(i,j,k,3,c) =  1.0 + c2dttz1 * rhos(k)
                lhs(i,j,k,4,c) =  dttz2 * cv(k+1) - dttz1 * rhos(k+1)
                lhs(i,j,k,5,c) =  0.0d0
             end do
          end do
       end do

!---------------------------------------------------------------------
!      add fourth order dissipation                                  
!---------------------------------------------------------------------
       if (start(3,c) .gt. 0) then
          k = 1
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz5
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4
                lhs(i,j,k,5,c) = lhs(i,j,k,5,c) + comz1

                lhs(i,j,k+1,2,c) = lhs(i,j,k+1,2,c) - comz4
                lhs(i,j,k+1,3,c) = lhs(i,j,k+1,3,c) + comz6
                lhs(i,j,k+1,4,c) = lhs(i,j,k+1,4,c) - comz4
                lhs(i,j,k+1,5,c) = lhs(i,j,k+1,5,c) + comz1
             end do
          end do
       endif

       do    k = 3*start(3,c), cell_size(3,c)-3*end(3,c)-1
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,1,c) = lhs(i,j,k,1,c) + comz1
                lhs(i,j,k,2,c) = lhs(i,j,k,2,c) - comz4
                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz6
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4
                lhs(i,j,k,5,c) = lhs(i,j,k,5,c) + comz1
             end do
          end do
       end do

       if (end(3,c) .gt. 0) then
          k = cell_size(3,c)-3 
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,1,c) = lhs(i,j,k,1,c) + comz1
                lhs(i,j,k,2,c) = lhs(i,j,k,2,c) - comz4
                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz6
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4

                lhs(i,j,k+1,1,c) = lhs(i,j,k+1,1,c) + comz1
                lhs(i,j,k+1,2,c) = lhs(i,j,k+1,2,c) - comz4
                lhs(i,j,k+1,3,c) = lhs(i,j,k+1,3,c) + comz5
             end do
          end do
       endif


!---------------------------------------------------------------------
!      subsequently, fill the other factors (u+c), (u-c) 
!---------------------------------------------------------------------
       do    k = start(3,c), cell_size(3,c)-end(3,c)-1
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,1+5,c)  = lhs(i,j,k,1,c)
                lhs(i,j,k,2+5,c)  = lhs(i,j,k,2,c) -  &
     &                            dttz2 * speed(i,j,k-1,c)
                lhs(i,j,k,3+5,c)  = lhs(i,j,k,3,c)
                lhs(i,j,k,4+5,c)  = lhs(i,j,k,4,c) +  &
     &                            dttz2 * speed(i,j,k+1,c)
                lhs(i,j,k,5+5,c) = lhs(i,j,k,5,c)
                lhs(i,j,k,1+10,c) = lhs(i,j,k,1,c)
                lhs(i,j,k,2+10,c) = lhs(i,j,k,2,c) +  &
     &                            dttz2 * speed(i,j,k-1,c)
                lhs(i,j,k,3+10,c) = lhs(i,j,k,3,c)
                lhs(i,j,k,4+10,c) = lhs(i,j,k,4,c) -  &
     &                            dttz2 * speed(i,j,k+1,c)
                lhs(i,j,k,5+10,c) = lhs(i,j,k,5,c)
             end do
          end do
       end do

       return
       end


