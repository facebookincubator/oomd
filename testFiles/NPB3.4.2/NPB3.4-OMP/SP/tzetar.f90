
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine  tzetar

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   block-diagonal matrix-vector multiplication                       
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer i, j, k
       double precision  t1, t2, t3, ac, xvel, yvel, zvel, r1, r2, r3,  &
     &                   r4, r5, btuz, ac2u, uzik1


       if (timeron) call timer_start(t_tzetar)
!$omp parallel do default(shared)  &
!$omp& private(i,j,k,t1,t2,t3,ac,xvel,yvel,zvel,r1,r2,r3,  &
!$omp&              r4,r5,btuz,ac2u,uzik1)  &
!$omp&  collapse(2)
       do    k = 1, nz2
          do    j = 1, ny2
             do    i = 1, nx2

                xvel = us(i,j,k)
                yvel = vs(i,j,k)
                zvel = ws(i,j,k)
                ac   = speed(i,j,k)

                ac2u = ac*ac

                r1 = rhs(1,i,j,k)
                r2 = rhs(2,i,j,k)
                r3 = rhs(3,i,j,k)
                r4 = rhs(4,i,j,k)
                r5 = rhs(5,i,j,k)      

                uzik1 = u(1,i,j,k)
                btuz  = bt * uzik1

                t1 = btuz/ac * (r4 + r5)
                t2 = r3 + t1
                t3 = btuz * (r4 - r5)

                rhs(1,i,j,k) = t2
                rhs(2,i,j,k) = -uzik1*r2 + xvel*t2
                rhs(3,i,j,k) =  uzik1*r1 + yvel*t2
                rhs(4,i,j,k) =  zvel*t2  + t3
                rhs(5,i,j,k) =  uzik1*(-xvel*r2 + yvel*r1) +  &
     &                    qs(i,j,k)*t2 + c2iv*ac2u*t1 + zvel*t3

             end do
          end do
       end do
       if (timeron) call timer_stop(t_tzetar)

       return
       end
