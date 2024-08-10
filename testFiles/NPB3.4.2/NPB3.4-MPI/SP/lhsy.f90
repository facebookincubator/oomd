
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine lhsy(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! This function computes the left hand side for the three y-factors   
!---------------------------------------------------------------------

       use sp_data
       implicit none

       double precision ru1
       integer          i, j, k, c

!---------------------------------------------------------------------
!      treat only cell c
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!      first fill the lhs for the u-eigenvalue         
!---------------------------------------------------------------------
       do  k = start(3,c), cell_size(3,c)-end(3,c)-1
          do  i = start(1,c), cell_size(1,c)-end(1,c)-1

             do  j = start(2,c)-1, cell_size(2,c)-end(2,c)
                ru1 = c3c4*rho_i(i,j,k,c)
                cv(j) = vs(i,j,k,c)
                rhoq(j) = dmax1( dy3 + con43 * ru1,  &
     &                           dy5 + c1c5*ru1,  &
     &                           dymax + ru1,  &
     &                           dy1)
             end do
            
             do  j = start(2,c), cell_size(2,c)-end(2,c)-1
                lhs(i,j,k,1,c) =  0.0d0
                lhs(i,j,k,2,c) = -dtty2 * cv(j-1) - dtty1 * rhoq(j-1)
                lhs(i,j,k,3,c) =  1.0 + c2dtty1 * rhoq(j)
                lhs(i,j,k,4,c) =  dtty2 * cv(j+1) - dtty1 * rhoq(j+1)
                lhs(i,j,k,5,c) =  0.0d0
             end do
          end do
       end do

!---------------------------------------------------------------------
!      add fourth order dissipation                             
!---------------------------------------------------------------------
       if (start(2,c) .gt. 0) then
          j = 1
          do   k = start(3,c), cell_size(3,c)-end(3,c)-1
             do   i = start(1,c), cell_size(1,c)-end(1,c)-1

                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz5
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4
                lhs(i,j,k,5,c) = lhs(i,j,k,5,c) + comz1
       
                lhs(i,j+1,k,2,c) = lhs(i,j+1,k,2,c) - comz4
                lhs(i,j+1,k,3,c) = lhs(i,j+1,k,3,c) + comz6
                lhs(i,j+1,k,4,c) = lhs(i,j+1,k,4,c) - comz4
                lhs(i,j+1,k,5,c) = lhs(i,j+1,k,5,c) + comz1
             end do
          end do
       endif

       do   k = start(3,c), cell_size(3,c)-end(3,c)-1
          do   j=3*start(2,c), cell_size(2,c)-3*end(2,c)-1
             do   i = start(1,c), cell_size(1,c)-end(1,c)-1

                lhs(i,j,k,1,c) = lhs(i,j,k,1,c) + comz1
                lhs(i,j,k,2,c) = lhs(i,j,k,2,c) - comz4
                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz6
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4
                lhs(i,j,k,5,c) = lhs(i,j,k,5,c) + comz1
             end do
          end do
       end do

       if (end(2,c) .gt. 0) then
          j = cell_size(2,c)-3
          do   k = start(3,c), cell_size(3,c)-end(3,c)-1
             do   i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,1,c) = lhs(i,j,k,1,c) + comz1
                lhs(i,j,k,2,c) = lhs(i,j,k,2,c) - comz4
                lhs(i,j,k,3,c) = lhs(i,j,k,3,c) + comz6
                lhs(i,j,k,4,c) = lhs(i,j,k,4,c) - comz4

                lhs(i,j+1,k,1,c) = lhs(i,j+1,k,1,c) + comz1
                lhs(i,j+1,k,2,c) = lhs(i,j+1,k,2,c) - comz4
                lhs(i,j+1,k,3,c) = lhs(i,j+1,k,3,c) + comz5
             end do
          end do
       endif

!---------------------------------------------------------------------
!      subsequently, do the other two factors                    
!---------------------------------------------------------------------
       do    k = start(3,c), cell_size(3,c)-end(3,c)-1
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                lhs(i,j,k,1+5,c)  = lhs(i,j,k,1,c)
                lhs(i,j,k,2+5,c)  = lhs(i,j,k,2,c) -  &
     &                            dtty2 * speed(i,j-1,k,c)
                lhs(i,j,k,3+5,c)  = lhs(i,j,k,3,c)
                lhs(i,j,k,4+5,c)  = lhs(i,j,k,4,c) +  &
     &                            dtty2 * speed(i,j+1,k,c)
                lhs(i,j,k,5+5,c) = lhs(i,j,k,5,c)
                lhs(i,j,k,1+10,c) = lhs(i,j,k,1,c)
                lhs(i,j,k,2+10,c) = lhs(i,j,k,2,c) +  &
     &                            dtty2 * speed(i,j-1,k,c)
                lhs(i,j,k,3+10,c) = lhs(i,j,k,3,c)
                lhs(i,j,k,4+10,c) = lhs(i,j,k,4,c) -  &
     &                            dtty2 * speed(i,j+1,k,c)
                lhs(i,j,k,5+10,c) = lhs(i,j,k,5,c)
             end do
          end do
       end do

       return
       end



