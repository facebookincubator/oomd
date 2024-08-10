!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Performs line solves in Z direction by first factoring
!     the block-tridiagonal matrix into an upper triangular matrix, 
!     and then performing back substitution to solve for the unknow
!     vectors of each line.  
!     
!     Make sure we treat elements zero to cell_size in the direction
!     of the sweep.
!---------------------------------------------------------------------

      use bt_data
      use work_lhs

      implicit none

      integer i, j, k, m, ksize
      integer ii, ib,kk,km,kp,kb
      double precision tmp1, tmp2, tmp3

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      if (timeron) call timer_start(t_zsolve)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     This function computes the left hand side for the three z-factors   
!---------------------------------------------------------------------

      ksize = grid_points(3)-1

!---------------------------------------------------------------------
!     Compute the indices for storing the block-diagonal matrix;
!     determine c (labeled f) and s jacobians
!---------------------------------------------------------------------
!$omp parallel default(shared) shared(ksize)  &
!$omp& private(i,j,k,m,ii,ib,kk,km,kp,kb,tmp1,tmp2,tmp3)

      call lhsinit(ksize)

!$omp do collapse(2)
      do j = 1, grid_points(2)-2
         do ii = 1, grid_points(1)-2, bsize

            if (timeron) call timer_start(t_rdis1)
            do k=0,ksize
            do ib = 1, bsize
               i = min(ii+ib-1, grid_points(1)-2)
               rhsx(ib,1,k) = rhs(1,i,j,k)
               rhsx(ib,2,k) = rhs(2,i,j,k)
               rhsx(ib,3,k) = rhs(3,i,j,k)
               rhsx(ib,4,k) = rhs(4,i,j,k)
               rhsx(ib,5,k) = rhs(5,i,j,k)
            end do
            end do
            if (timeron) call timer_stop(t_rdis1)

            call lhsinit(0)

            kb = 0
            do kk = 1, ksize-1
            kb = mod(kb + 1, 3)
            km = min(2*kk - 3, 1)     ! -1 or 1
            kp = mod(kb + km, 3) - 1

            do k = kk+km, kk+1
            kp = kp + 1
            do ib = 1, bsize
               i = min(ii+ib-1, grid_points(1)-2)

               tmp1 = 1.0d+00 / u(1,i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               fjac(ib,1,1,kp) = 0.0d+00
               fjac(ib,1,2,kp) = 0.0d+00
               fjac(ib,1,3,kp) = 0.0d+00
               fjac(ib,1,4,kp) = 1.0d+00
               fjac(ib,1,5,kp) = 0.0d+00

               fjac(ib,2,1,kp) = - ( u(2,i,j,k)*u(4,i,j,k) )   &
     &              * tmp2 
               fjac(ib,2,2,kp) = u(4,i,j,k) * tmp1
               fjac(ib,2,3,kp) = 0.0d+00
               fjac(ib,2,4,kp) = u(2,i,j,k) * tmp1
               fjac(ib,2,5,kp) = 0.0d+00

               fjac(ib,3,1,kp) = - ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2 
               fjac(ib,3,2,kp) = 0.0d+00
               fjac(ib,3,3,kp) = u(4,i,j,k) * tmp1
               fjac(ib,3,4,kp) = u(3,i,j,k) * tmp1
               fjac(ib,3,5,kp) = 0.0d+00

               fjac(ib,4,1,kp) = - (u(4,i,j,k)*u(4,i,j,k) * tmp2 )   &
     &              + c2 * qs(i,j,k)
               fjac(ib,4,2,kp) = - c2 *  u(2,i,j,k) * tmp1 
               fjac(ib,4,3,kp) = - c2 *  u(3,i,j,k) * tmp1
               fjac(ib,4,4,kp) = ( 2.0d+00 - c2 )  &
     &              *  u(4,i,j,k) * tmp1 
               fjac(ib,4,5,kp) = c2

               fjac(ib,5,1,kp) = ( c2 * 2.0d0 * square(i,j,k)   &
     &              - c1 * u(5,i,j,k) )  &
     &              * u(4,i,j,k) * tmp2
               fjac(ib,5,2,kp) = - c2 * ( u(2,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2 
               fjac(ib,5,3,kp) = - c2 * ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2
               fjac(ib,5,4,kp) = c1 * ( u(5,i,j,k) * tmp1 )  &
     &              - c2  &
     &              * ( qs(i,j,k)  &
     &              + u(4,i,j,k)*u(4,i,j,k) * tmp2 )
               fjac(ib,5,5,kp) = c1 * u(4,i,j,k) * tmp1

               njac(ib,1,1,kp) = 0.0d+00
               njac(ib,1,2,kp) = 0.0d+00
               njac(ib,1,3,kp) = 0.0d+00
               njac(ib,1,4,kp) = 0.0d+00
               njac(ib,1,5,kp) = 0.0d+00

               njac(ib,2,1,kp) = - c3c4 * tmp2 * u(2,i,j,k)
               njac(ib,2,2,kp) =   c3c4 * tmp1
               njac(ib,2,3,kp) =   0.0d+00
               njac(ib,2,4,kp) =   0.0d+00
               njac(ib,2,5,kp) =   0.0d+00

               njac(ib,3,1,kp) = - c3c4 * tmp2 * u(3,i,j,k)
               njac(ib,3,2,kp) =   0.0d+00
               njac(ib,3,3,kp) =   c3c4 * tmp1
               njac(ib,3,4,kp) =   0.0d+00
               njac(ib,3,5,kp) =   0.0d+00

               njac(ib,4,1,kp) = - con43 * c3c4 * tmp2 * u(4,i,j,k)
               njac(ib,4,2,kp) =   0.0d+00
               njac(ib,4,3,kp) =   0.0d+00
               njac(ib,4,4,kp) =   con43 * c3 * c4 * tmp1
               njac(ib,4,5,kp) =   0.0d+00

               njac(ib,5,1,kp) = - (  c3c4  &
     &              - c1345 ) * tmp3 * (u(2,i,j,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(3,i,j,k)**2)  &
     &              - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (u(4,i,j,k)**2)  &
     &              - c1345 * tmp2 * u(5,i,j,k)

               njac(ib,5,2,kp) = (  c3c4 - c1345 ) * tmp2 * u(2,i,j,k)
               njac(ib,5,3,kp) = (  c3c4 - c1345 ) * tmp2 * u(3,i,j,k)
               njac(ib,5,4,kp) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * u(4,i,j,k)
               njac(ib,5,5,kp) = ( c1345 )* tmp1

            enddo
            enddo

!---------------------------------------------------------------------
!     now jacobians set, so form left hand side in z direction
!---------------------------------------------------------------------
            km = mod(kb + 2, 3)
            k = kk
!dir$ vector always
            do ib = 1, bsize

               tmp1 = dt * tz1
               tmp2 = dt * tz2

               lhsa(ib,1,1,1) = - tmp2 * fjac(ib,1,1,km)  &
     &              - tmp1 * njac(ib,1,1,km)  &
     &              - tmp1 * dz1 
               lhsa(ib,1,2,1) = - tmp2 * fjac(ib,1,2,km)  &
     &              - tmp1 * njac(ib,1,2,km)
               lhsa(ib,1,3,1) = - tmp2 * fjac(ib,1,3,km)  &
     &              - tmp1 * njac(ib,1,3,km)
               lhsa(ib,1,4,1) = - tmp2 * fjac(ib,1,4,km)  &
     &              - tmp1 * njac(ib,1,4,km)
               lhsa(ib,1,5,1) = - tmp2 * fjac(ib,1,5,km)  &
     &              - tmp1 * njac(ib,1,5,km)

               lhsa(ib,2,1,1) = - tmp2 * fjac(ib,2,1,km)  &
     &              - tmp1 * njac(ib,2,1,km)
               lhsa(ib,2,2,1) = - tmp2 * fjac(ib,2,2,km)  &
     &              - tmp1 * njac(ib,2,2,km)  &
     &              - tmp1 * dz2
               lhsa(ib,2,3,1) = - tmp2 * fjac(ib,2,3,km)  &
     &              - tmp1 * njac(ib,2,3,km)
               lhsa(ib,2,4,1) = - tmp2 * fjac(ib,2,4,km)  &
     &              - tmp1 * njac(ib,2,4,km)
               lhsa(ib,2,5,1) = - tmp2 * fjac(ib,2,5,km)  &
     &              - tmp1 * njac(ib,2,5,km)

               lhsa(ib,3,1,1) = - tmp2 * fjac(ib,3,1,km)  &
     &              - tmp1 * njac(ib,3,1,km)
               lhsa(ib,3,2,1) = - tmp2 * fjac(ib,3,2,km)  &
     &              - tmp1 * njac(ib,3,2,km)
               lhsa(ib,3,3,1) = - tmp2 * fjac(ib,3,3,km)  &
     &              - tmp1 * njac(ib,3,3,km)  &
     &              - tmp1 * dz3 
               lhsa(ib,3,4,1) = - tmp2 * fjac(ib,3,4,km)  &
     &              - tmp1 * njac(ib,3,4,km)
               lhsa(ib,3,5,1) = - tmp2 * fjac(ib,3,5,km)  &
     &              - tmp1 * njac(ib,3,5,km)

               lhsa(ib,4,1,1) = - tmp2 * fjac(ib,4,1,km)  &
     &              - tmp1 * njac(ib,4,1,km)
               lhsa(ib,4,2,1) = - tmp2 * fjac(ib,4,2,km)  &
     &              - tmp1 * njac(ib,4,2,km)
               lhsa(ib,4,3,1) = - tmp2 * fjac(ib,4,3,km)  &
     &              - tmp1 * njac(ib,4,3,km)
               lhsa(ib,4,4,1) = - tmp2 * fjac(ib,4,4,km)  &
     &              - tmp1 * njac(ib,4,4,km)  &
     &              - tmp1 * dz4
               lhsa(ib,4,5,1) = - tmp2 * fjac(ib,4,5,km)  &
     &              - tmp1 * njac(ib,4,5,km)

               lhsa(ib,5,1,1) = - tmp2 * fjac(ib,5,1,km)  &
     &              - tmp1 * njac(ib,5,1,km)
               lhsa(ib,5,2,1) = - tmp2 * fjac(ib,5,2,km)  &
     &              - tmp1 * njac(ib,5,2,km)
               lhsa(ib,5,3,1) = - tmp2 * fjac(ib,5,3,km)  &
     &              - tmp1 * njac(ib,5,3,km)
               lhsa(ib,5,4,1) = - tmp2 * fjac(ib,5,4,km)  &
     &              - tmp1 * njac(ib,5,4,km)
               lhsa(ib,5,5,1) = - tmp2 * fjac(ib,5,5,km)  &
     &              - tmp1 * njac(ib,5,5,km)  &
     &              - tmp1 * dz5

               lhsb(ib,1,1,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,1,1,kb)  &
     &              + tmp1 * 2.0d+00 * dz1
               lhsb(ib,1,2,1) = tmp1 * 2.0d+00 * njac(ib,1,2,kb)
               lhsb(ib,1,3,1) = tmp1 * 2.0d+00 * njac(ib,1,3,kb)
               lhsb(ib,1,4,1) = tmp1 * 2.0d+00 * njac(ib,1,4,kb)
               lhsb(ib,1,5,1) = tmp1 * 2.0d+00 * njac(ib,1,5,kb)

               lhsb(ib,2,1,1) = tmp1 * 2.0d+00 * njac(ib,2,1,kb)
               lhsb(ib,2,2,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,2,2,kb)  &
     &              + tmp1 * 2.0d+00 * dz2
               lhsb(ib,2,3,1) = tmp1 * 2.0d+00 * njac(ib,2,3,kb)
               lhsb(ib,2,4,1) = tmp1 * 2.0d+00 * njac(ib,2,4,kb)
               lhsb(ib,2,5,1) = tmp1 * 2.0d+00 * njac(ib,2,5,kb)

               lhsb(ib,3,1,1) = tmp1 * 2.0d+00 * njac(ib,3,1,kb)
               lhsb(ib,3,2,1) = tmp1 * 2.0d+00 * njac(ib,3,2,kb)
               lhsb(ib,3,3,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,3,3,kb)  &
     &              + tmp1 * 2.0d+00 * dz3
               lhsb(ib,3,4,1) = tmp1 * 2.0d+00 * njac(ib,3,4,kb)
               lhsb(ib,3,5,1) = tmp1 * 2.0d+00 * njac(ib,3,5,kb)

               lhsb(ib,4,1,1) = tmp1 * 2.0d+00 * njac(ib,4,1,kb)
               lhsb(ib,4,2,1) = tmp1 * 2.0d+00 * njac(ib,4,2,kb)
               lhsb(ib,4,3,1) = tmp1 * 2.0d+00 * njac(ib,4,3,kb)
               lhsb(ib,4,4,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,4,4,kb)  &
     &              + tmp1 * 2.0d+00 * dz4
               lhsb(ib,4,5,1) = tmp1 * 2.0d+00 * njac(ib,4,5,kb)

               lhsb(ib,5,1,1) = tmp1 * 2.0d+00 * njac(ib,5,1,kb)
               lhsb(ib,5,2,1) = tmp1 * 2.0d+00 * njac(ib,5,2,kb)
               lhsb(ib,5,3,1) = tmp1 * 2.0d+00 * njac(ib,5,3,kb)
               lhsb(ib,5,4,1) = tmp1 * 2.0d+00 * njac(ib,5,4,kb)
               lhsb(ib,5,5,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,5,5,kb)   &
     &              + tmp1 * 2.0d+00 * dz5

               lhsc(ib,1,1,k) =  tmp2 * fjac(ib,1,1,kp)  &
     &              - tmp1 * njac(ib,1,1,kp)  &
     &              - tmp1 * dz1
               lhsc(ib,1,2,k) =  tmp2 * fjac(ib,1,2,kp)  &
     &              - tmp1 * njac(ib,1,2,kp)
               lhsc(ib,1,3,k) =  tmp2 * fjac(ib,1,3,kp)  &
     &              - tmp1 * njac(ib,1,3,kp)
               lhsc(ib,1,4,k) =  tmp2 * fjac(ib,1,4,kp)  &
     &              - tmp1 * njac(ib,1,4,kp)
               lhsc(ib,1,5,k) =  tmp2 * fjac(ib,1,5,kp)  &
     &              - tmp1 * njac(ib,1,5,kp)

               lhsc(ib,2,1,k) =  tmp2 * fjac(ib,2,1,kp)  &
     &              - tmp1 * njac(ib,2,1,kp)
               lhsc(ib,2,2,k) =  tmp2 * fjac(ib,2,2,kp)  &
     &              - tmp1 * njac(ib,2,2,kp)  &
     &              - tmp1 * dz2
               lhsc(ib,2,3,k) =  tmp2 * fjac(ib,2,3,kp)  &
     &              - tmp1 * njac(ib,2,3,kp)
               lhsc(ib,2,4,k) =  tmp2 * fjac(ib,2,4,kp)  &
     &              - tmp1 * njac(ib,2,4,kp)
               lhsc(ib,2,5,k) =  tmp2 * fjac(ib,2,5,kp)  &
     &              - tmp1 * njac(ib,2,5,kp)

               lhsc(ib,3,1,k) =  tmp2 * fjac(ib,3,1,kp)  &
     &              - tmp1 * njac(ib,3,1,kp)
               lhsc(ib,3,2,k) =  tmp2 * fjac(ib,3,2,kp)  &
     &              - tmp1 * njac(ib,3,2,kp)
               lhsc(ib,3,3,k) =  tmp2 * fjac(ib,3,3,kp)  &
     &              - tmp1 * njac(ib,3,3,kp)  &
     &              - tmp1 * dz3
               lhsc(ib,3,4,k) =  tmp2 * fjac(ib,3,4,kp)  &
     &              - tmp1 * njac(ib,3,4,kp)
               lhsc(ib,3,5,k) =  tmp2 * fjac(ib,3,5,kp)  &
     &              - tmp1 * njac(ib,3,5,kp)

               lhsc(ib,4,1,k) =  tmp2 * fjac(ib,4,1,kp)  &
     &              - tmp1 * njac(ib,4,1,kp)
               lhsc(ib,4,2,k) =  tmp2 * fjac(ib,4,2,kp)  &
     &              - tmp1 * njac(ib,4,2,kp)
               lhsc(ib,4,3,k) =  tmp2 * fjac(ib,4,3,kp)  &
     &              - tmp1 * njac(ib,4,3,kp)
               lhsc(ib,4,4,k) =  tmp2 * fjac(ib,4,4,kp)  &
     &              - tmp1 * njac(ib,4,4,kp)  &
     &              - tmp1 * dz4
               lhsc(ib,4,5,k) =  tmp2 * fjac(ib,4,5,kp)  &
     &              - tmp1 * njac(ib,4,5,kp)

               lhsc(ib,5,1,k) =  tmp2 * fjac(ib,5,1,kp)  &
     &              - tmp1 * njac(ib,5,1,kp)
               lhsc(ib,5,2,k) =  tmp2 * fjac(ib,5,2,kp)  &
     &              - tmp1 * njac(ib,5,2,kp)
               lhsc(ib,5,3,k) =  tmp2 * fjac(ib,5,3,kp)  &
     &              - tmp1 * njac(ib,5,3,kp)
               lhsc(ib,5,4,k) =  tmp2 * fjac(ib,5,4,kp)  &
     &              - tmp1 * njac(ib,5,4,kp)
               lhsc(ib,5,5,k) =  tmp2 * fjac(ib,5,5,kp)  &
     &              - tmp1 * njac(ib,5,5,kp)  &
     &              - tmp1 * dz5

            enddo

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     performs guaussian elimination on this cell.
!     
!     assumes that unpacking routines for non-first cells 
!     preload C' and rhs' from previous cell.
!     
!     assumed send happens outside this routine, but that
!     c'(KMAX) and rhs'(KMAX) will be sent to next cell.
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     outer most do loops - sweeping in i direction
!---------------------------------------------------------------------

            if (timeron) call timer_start(t_solsub)
!---------------------------------------------------------------------
!     multiply c(i,j,0) by b_inverse and copy back to c
!     multiply rhs(0) by b_inverse(0) and copy to rhs
!---------------------------------------------------------------------
            if (kk .eq. 1) then
            call binvcrhs( lhsb(1,1,1,0),  &
     &                        lhsc(1,1,1,0),  &
     &                        rhsx(1,1,0) )
            endif


!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     subtract A*lhs_vector(k-1) from lhs_vector(k)
!     
!     rhs(k) = rhs(k) - A*rhs(k-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,1,1),  &
     &                         rhsx(1,1,k-1),rhsx(1,1,k))

!---------------------------------------------------------------------
!     B(k) = B(k) - C(k-1)*A(k)
!     call matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,1,1),  &
     &                         lhsc(1,1,1,k-1),  &
     &                         lhsb(1,1,1,1))

!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to c
!     multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,1,1),  &
     &                        lhsc(1,1,1,k),  &
     &                        rhsx(1,1,k) )


!---------------------------------------------------------------------
!     Now finish up special cases for last cell
!---------------------------------------------------------------------

            if (kk .eq. ksize-1) then
!---------------------------------------------------------------------
!     rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
!---------------------------------------------------------------------
            call matvec_sub(lhsa(1,1,1,2),  &
     &                         rhsx(1,1,ksize-1),rhsx(1,1,ksize))

!---------------------------------------------------------------------
!     B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
!     call matmul_sub(aa,i,j,ksize,c,
!     $              cc,i,j,ksize-1,c,bb,i,j,ksize)
!---------------------------------------------------------------------
            call matmul_sub(lhsa(1,1,1,2),  &
     &                         lhsc(1,1,1,ksize-1),  &
     &                         lhsb(1,1,1,2))

!---------------------------------------------------------------------
!     multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
!---------------------------------------------------------------------
            call binvrhs( lhsb(1,1,1,2),  &
     &                       rhsx(1,1,ksize) )
            endif

            if (timeron) call timer_stop(t_solsub)

            enddo

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(ksize)=rhs(ksize)
!     else assume U(ksize) is loaded in un pack backsub_info
!     so just use it
!     after call u(kstart) will be sent to next cell
!---------------------------------------------------------------------

            do k=ksize-1,0,-1
!dir$ vector always
            do ib = 1, bsize
!dir$ unroll
               do m=1,BLOCK_SIZE
                  rhsx(ib,m,k) = rhsx(ib,m,k)   &
     &                 - lhsc(ib,m,1,k)*rhsx(ib,1,k+1)  &
     &                 - lhsc(ib,m,2,k)*rhsx(ib,2,k+1)  &
     &                 - lhsc(ib,m,3,k)*rhsx(ib,3,k+1)  &
     &                 - lhsc(ib,m,4,k)*rhsx(ib,4,k+1)  &
     &                 - lhsc(ib,m,5,k)*rhsx(ib,5,k+1)
               enddo
            enddo
            enddo

            if (timeron) call timer_start(t_rdis1)
            do ib = 1, bsize
               i = ii+ib-1
               if (i < grid_points(1)-1) then
               do k=0,ksize
                  rhs(1,i,j,k) = rhsx(ib,1,k)
                  rhs(2,i,j,k) = rhsx(ib,2,k)
                  rhs(3,i,j,k) = rhsx(ib,3,k)
                  rhs(4,i,j,k) = rhsx(ib,4,k)
                  rhs(5,i,j,k) = rhsx(ib,5,k)
               end do
               endif
            end do
            if (timeron) call timer_stop(t_rdis1)

         enddo
      enddo
!$omp end parallel
      if (timeron) call timer_stop(t_zsolve)

      return
      end
