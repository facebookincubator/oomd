
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine  tzetar(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   block-diagonal matrix-vector multiplication                       
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer i, j, k, c
       double precision  t1, t2, t3, ac, xvel, yvel, zvel, r1, r2, r3,  &
     &                   r4, r5, btuz, acinv, ac2u, uzik1

!---------------------------------------------------------------------
!      treat only one cell                                             
!---------------------------------------------------------------------
       do    k = start(3,c), cell_size(3,c)-end(3,c)-1
          do    j = start(2,c), cell_size(2,c)-end(2,c)-1
             do    i = start(1,c), cell_size(1,c)-end(1,c)-1

                xvel = us(i,j,k,c)
                yvel = vs(i,j,k,c)
                zvel = ws(i,j,k,c)
                ac   = speed(i,j,k,c)
                acinv = ainv(i,j,k,c)

                ac2u = ac*ac

                r1 = rhs(i,j,k,1,c)
                r2 = rhs(i,j,k,2,c)
                r3 = rhs(i,j,k,3,c)
                r4 = rhs(i,j,k,4,c)
                r5 = rhs(i,j,k,5,c)      

                uzik1 = u(i,j,k,1,c)
                btuz  = bt * uzik1

                t1 = btuz*acinv * (r4 + r5)
                t2 = r3 + t1
                t3 = btuz * (r4 - r5)

                rhs(i,j,k,1,c) = t2
                rhs(i,j,k,2,c) = -uzik1*r2 + xvel*t2
                rhs(i,j,k,3,c) =  uzik1*r1 + yvel*t2
                rhs(i,j,k,4,c) =  zvel*t2  + t3
                rhs(i,j,k,5,c) =  uzik1*(-xvel*r2 + yvel*r1) +  &
     &                    qs(i,j,k,c)*t2 + c2iv*ac2u*t1 + zvel*t3

             end do
          end do
       end do

       return
       end
