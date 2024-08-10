
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine pinvr

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   block-diagonal matrix-vector multiplication                       
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer i, j, k
       double precision r1, r2, r3, r4, r5, t1, t2

       if (timeron) call timer_start(t_pinvr)
!$omp parallel do default(shared) private(i,j,k,r1,r2,r3,r4,r5,t1,t2)  &
!$omp&  collapse(2)
       do   k = 1, nz2
          do   j = 1, ny2
             do   i = 1, nx2

                r1 = rhs(1,i,j,k)
                r2 = rhs(2,i,j,k)
                r3 = rhs(3,i,j,k)
                r4 = rhs(4,i,j,k)
                r5 = rhs(5,i,j,k)

                t1 = bt * r1
                t2 = 0.5d0 * ( r4 + r5 )

                rhs(1,i,j,k) =  bt * ( r4 - r5 )
                rhs(2,i,j,k) = -r3
                rhs(3,i,j,k) =  r2
                rhs(4,i,j,k) = -t1 + t2
                rhs(5,i,j,k) =  t1 + t2
             end do
          end do
       end do
       if (timeron) call timer_stop(t_pinvr)

       return
       end



