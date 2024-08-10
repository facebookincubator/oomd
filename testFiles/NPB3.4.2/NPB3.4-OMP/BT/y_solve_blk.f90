!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Performs line solves in Y direction by first factoring
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

      integer i, j, k, m, jsize
      integer ii,ib,jj,jm,jp,jb
      double precision tmp1, tmp2, tmp3

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      if (timeron) call timer_start(t_ysolve)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     This function computes the left hand side for the three y-factors   
!---------------------------------------------------------------------

      jsize = grid_points(2)-1

!---------------------------------------------------------------------
!     Compute the indices for storing the tri-diagonal matrix;
!     determine a (labeled f) and n jacobians for cell c
!---------------------------------------------------------------------
!$omp parallel default(shared) shared(jsize)  &
!$omp& private(i,j,k,m,ii,ib,jj,jm,jp,jb,tmp1,tmp2,tmp3)

      call lhsinit(jsize)

!$omp do collapse(2)
      do k = 1, grid_points(3)-2
         do ii = 1, grid_points(1)-2, bsize

            if (timeron) call timer_start(t_rdis1)
            do j=0,jsize
            do ib = 1, bsize
               i = min(ii+ib-1, grid_points(1)-2)
               rhsx(ib,1,j) = rhs(1,i,j,k)
               rhsx(ib,2,j) = rhs(2,i,j,k)
               rhsx(ib,3,j) = rhs(3,i,j,k)
               rhsx(ib,4,j) = rhs(4,i,j,k)
               rhsx(ib,5,j) = rhs(5,i,j,k)
            end do
            end do
            if (timeron) call timer_stop(t_rdis1)

            call lhsinit(0)

            jb = 0
            do jj = 1, jsize-1
            jb = mod(jb + 1, 3)
            jm = min(2*jj - 3, 1)     ! -1 or 1
            jp = mod(jb + jm, 3) - 1

            do j = jj+jm, jj+1
            jp = jp + 1
            do ib = 1, bsize
               i = min(ii+ib-1, grid_points(1)-2)

               tmp1 = rho_i(i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               fjac(ib,1,1,jp) = 0.0d+00
               fjac(ib,1,2,jp) = 0.0d+00
               fjac(ib,1,3,jp) = 1.0d+00
               fjac(ib,1,4,jp) = 0.0d+00
               fjac(ib,1,5,jp) = 0.0d+00

               fjac(ib,2,1,jp) = - ( u(2,i,j,k)*u(3,i,j,k) )  &
     &              * tmp2
               fjac(ib,2,2,jp) = u(3,i,j,k) * tmp1
               fjac(ib,2,3,jp) = u(2,i,j,k) * tmp1
               fjac(ib,2,4,jp) = 0.0d+00
               fjac(ib,2,5,jp) = 0.0d+00

               fjac(ib,3,1,jp) = - ( u(3,i,j,k)*u(3,i,j,k)*tmp2)  &
     &              + c2 * qs(i,j,k)
               fjac(ib,3,2,jp) = - c2 *  u(2,i,j,k) * tmp1
               fjac(ib,3,3,jp) = ( 2.0d+00 - c2 )  &
     &              *  u(3,i,j,k) * tmp1 
               fjac(ib,3,4,jp) = - c2 * u(4,i,j,k) * tmp1 
               fjac(ib,3,5,jp) = c2

               fjac(ib,4,1,jp) = - ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2
               fjac(ib,4,2,jp) = 0.0d+00
               fjac(ib,4,3,jp) = u(4,i,j,k) * tmp1
               fjac(ib,4,4,jp) = u(3,i,j,k) * tmp1
               fjac(ib,4,5,jp) = 0.0d+00

               fjac(ib,5,1,jp) = ( c2 * 2.0d0 * square(i,j,k)  &
     &              - c1 * u(5,i,j,k) )  &
     &              * u(3,i,j,k) * tmp2
               fjac(ib,5,2,jp) = - c2 * u(2,i,j,k)*u(3,i,j,k)   &
     &              * tmp2
               fjac(ib,5,3,jp) = c1 * u(5,i,j,k) * tmp1   &
     &              - c2   &
     &              * ( qs(i,j,k)  &
     &              + u(3,i,j,k)*u(3,i,j,k) * tmp2 )
               fjac(ib,5,4,jp) = - c2 * ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2
               fjac(ib,5,5,jp) = c1 * u(3,i,j,k) * tmp1 

               njac(ib,1,1,jp) = 0.0d+00
               njac(ib,1,2,jp) = 0.0d+00
               njac(ib,1,3,jp) = 0.0d+00
               njac(ib,1,4,jp) = 0.0d+00
               njac(ib,1,5,jp) = 0.0d+00

               njac(ib,2,1,jp) = - c3c4 * tmp2 * u(2,i,j,k)
               njac(ib,2,2,jp) =   c3c4 * tmp1
               njac(ib,2,3,jp) =   0.0d+00
               njac(ib,2,4,jp) =   0.0d+00
               njac(ib,2,5,jp) =   0.0d+00

               njac(ib,3,1,jp) = - con43 * c3c4 * tmp2 * u(3,i,j,k)
               njac(ib,3,2,jp) =   0.0d+00
               njac(ib,3,3,jp) =   con43 * c3c4 * tmp1
               njac(ib,3,4,jp) =   0.0d+00
               njac(ib,3,5,jp) =   0.0d+00

               njac(ib,4,1,jp) = - c3c4 * tmp2 * u(4,i,j,k)
               njac(ib,4,2,jp) =   0.0d+00
               njac(ib,4,3,jp) =   0.0d+00
               njac(ib,4,4,jp) =   c3c4 * tmp1
               njac(ib,4,5,jp) =   0.0d+00

               njac(ib,5,1,jp) = - (  c3c4  &
     &              - c1345 ) * tmp3 * (u(2,i,j,k)**2)  &
     &              - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (u(3,i,j,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(4,i,j,k)**2)  &
     &              - c1345 * tmp2 * u(5,i,j,k)

               njac(ib,5,2,jp) = (  c3c4 - c1345 ) * tmp2 * u(2,i,j,k)
               njac(ib,5,3,jp) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * u(3,i,j,k)
               njac(ib,5,4,jp) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k)
               njac(ib,5,5,jp) = ( c1345 ) * tmp1

            enddo
            enddo

!---------------------------------------------------------------------
!     now joacobians set, so form left hand side in y direction
!---------------------------------------------------------------------
            jm = mod(jb + 2, 3)
            j = jj
!dir$ vector always
            do ib = 1, bsize

               tmp1 = dt * ty1
               tmp2 = dt * ty2

               lhsa(ib,1,1,1) = - tmp2 * fjac(ib,1,1,jm)  &
     &              - tmp1 * njac(ib,1,1,jm)  &
     &              - tmp1 * dy1 
               lhsa(ib,1,2,1) = - tmp2 * fjac(ib,1,2,jm)  &
     &              - tmp1 * njac(ib,1,2,jm)
               lhsa(ib,1,3,1) = - tmp2 * fjac(ib,1,3,jm)  &
     &              - tmp1 * njac(ib,1,3,jm)
               lhsa(ib,1,4,1) = - tmp2 * fjac(ib,1,4,jm)  &
     &              - tmp1 * njac(ib,1,4,jm)
               lhsa(ib,1,5,1) = - tmp2 * fjac(ib,1,5,jm)  &
     &              - tmp1 * njac(ib,1,5,jm)

               lhsa(ib,2,1,1) = - tmp2 * fjac(ib,2,1,jm)  &
     &              - tmp1 * njac(ib,2,1,jm)
               lhsa(ib,2,2,1) = - tmp2 * fjac(ib,2,2,jm)  &
     &              - tmp1 * njac(ib,2,2,jm)  &
     &              - tmp1 * dy2
               lhsa(ib,2,3,1) = - tmp2 * fjac(ib,2,3,jm)  &
     &              - tmp1 * njac(ib,2,3,jm)
               lhsa(ib,2,4,1) = - tmp2 * fjac(ib,2,4,jm)  &
     &              - tmp1 * njac(ib,2,4,jm)
               lhsa(ib,2,5,1) = - tmp2 * fjac(ib,2,5,jm)  &
     &              - tmp1 * njac(ib,2,5,jm)

               lhsa(ib,3,1,1) = - tmp2 * fjac(ib,3,1,jm)  &
     &              - tmp1 * njac(ib,3,1,jm)
               lhsa(ib,3,2,1) = - tmp2 * fjac(ib,3,2,jm)  &
     &              - tmp1 * njac(ib,3,2,jm)
               lhsa(ib,3,3,1) = - tmp2 * fjac(ib,3,3,jm)  &
     &              - tmp1 * njac(ib,3,3,jm)  &
     &              - tmp1 * dy3 
               lhsa(ib,3,4,1) = - tmp2 * fjac(ib,3,4,jm)  &
     &              - tmp1 * njac(ib,3,4,jm)
               lhsa(ib,3,5,1) = - tmp2 * fjac(ib,3,5,jm)  &
     &              - tmp1 * njac(ib,3,5,jm)

               lhsa(ib,4,1,1) = - tmp2 * fjac(ib,4,1,jm)  &
     &              - tmp1 * njac(ib,4,1,jm)
               lhsa(ib,4,2,1) = - tmp2 * fjac(ib,4,2,jm)  &
     &              - tmp1 * njac(ib,4,2,jm)
               lhsa(ib,4,3,1) = - tmp2 * fjac(ib,4,3,jm)  &
     &              - tmp1 * njac(ib,4,3,jm)
               lhsa(ib,4,4,1) = - tmp2 * fjac(ib,4,4,jm)  &
     &              - tmp1 * njac(ib,4,4,jm)  &
     &              - tmp1 * dy4
               lhsa(ib,4,5,1) = - tmp2 * fjac(ib,4,5,jm)  &
     &              - tmp1 * njac(ib,4,5,jm)

               lhsa(ib,5,1,1) = - tmp2 * fjac(ib,5,1,jm)  &
     &              - tmp1 * njac(ib,5,1,jm)
               lhsa(ib,5,2,1) = - tmp2 * fjac(ib,5,2,jm)  &
     &              - tmp1 * njac(ib,5,2,jm)
               lhsa(ib,5,3,1) = - tmp2 * fjac(ib,5,3,jm)  &
     &              - tmp1 * njac(ib,5,3,jm)
               lhsa(ib,5,4,1) = - tmp2 * fjac(ib,5,4,jm)  &
     &              - tmp1 * njac(ib,5,4,jm)
               lhsa(ib,5,5,1) = - tmp2 * fjac(ib,5,5,jm)  &
     &              - tmp1 * njac(ib,5,5,jm)  &
     &              - tmp1 * dy5

               lhsb(ib,1,1,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,1,1,jb)  &
     &              + tmp1 * 2.0d+00 * dy1
               lhsb(ib,1,2,1) = tmp1 * 2.0d+00 * njac(ib,1,2,jb)
               lhsb(ib,1,3,1) = tmp1 * 2.0d+00 * njac(ib,1,3,jb)
               lhsb(ib,1,4,1) = tmp1 * 2.0d+00 * njac(ib,1,4,jb)
               lhsb(ib,1,5,1) = tmp1 * 2.0d+00 * njac(ib,1,5,jb)

               lhsb(ib,2,1,1) = tmp1 * 2.0d+00 * njac(ib,2,1,jb)
               lhsb(ib,2,2,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,2,2,jb)  &
     &              + tmp1 * 2.0d+00 * dy2
               lhsb(ib,2,3,1) = tmp1 * 2.0d+00 * njac(ib,2,3,jb)
               lhsb(ib,2,4,1) = tmp1 * 2.0d+00 * njac(ib,2,4,jb)
               lhsb(ib,2,5,1) = tmp1 * 2.0d+00 * njac(ib,2,5,jb)

               lhsb(ib,3,1,1) = tmp1 * 2.0d+00 * njac(ib,3,1,jb)
               lhsb(ib,3,2,1) = tmp1 * 2.0d+00 * njac(ib,3,2,jb)
               lhsb(ib,3,3,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,3,3,jb)  &
     &              + tmp1 * 2.0d+00 * dy3
               lhsb(ib,3,4,1) = tmp1 * 2.0d+00 * njac(ib,3,4,jb)
               lhsb(ib,3,5,1) = tmp1 * 2.0d+00 * njac(ib,3,5,jb)

               lhsb(ib,4,1,1) = tmp1 * 2.0d+00 * njac(ib,4,1,jb)
               lhsb(ib,4,2,1) = tmp1 * 2.0d+00 * njac(ib,4,2,jb)
               lhsb(ib,4,3,1) = tmp1 * 2.0d+00 * njac(ib,4,3,jb)
               lhsb(ib,4,4,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,4,4,jb)  &
     &              + tmp1 * 2.0d+00 * dy4
               lhsb(ib,4,5,1) = tmp1 * 2.0d+00 * njac(ib,4,5,jb)

               lhsb(ib,5,1,1) = tmp1 * 2.0d+00 * njac(ib,5,1,jb)
               lhsb(ib,5,2,1) = tmp1 * 2.0d+00 * njac(ib,5,2,jb)
               lhsb(ib,5,3,1) = tmp1 * 2.0d+00 * njac(ib,5,3,jb)
               lhsb(ib,5,4,1) = tmp1 * 2.0d+00 * njac(ib,5,4,jb)
               lhsb(ib,5,5,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(ib,5,5,jb)   &
     &              + tmp1 * 2.0d+00 * dy5

               lhsc(ib,1,1,j) =  tmp2 * fjac(ib,1,1,jp)  &
     &              - tmp1 * njac(ib,1,1,jp)  &
     &              - tmp1 * dy1
               lhsc(ib,1,2,j) =  tmp2 * fjac(ib,1,2,jp)  &
     &              - tmp1 * njac(ib,1,2,jp)
               lhsc(ib,1,3,j) =  tmp2 * fjac(ib,1,3,jp)  &
     &              - tmp1 * njac(ib,1,3,jp)
               lhsc(ib,1,4,j) =  tmp2 * fjac(ib,1,4,jp)  &
     &              - tmp1 * njac(ib,1,4,jp)
               lhsc(ib,1,5,j) =  tmp2 * fjac(ib,1,5,jp)  &
     &              - tmp1 * njac(ib,1,5,jp)

               lhsc(ib,2,1,j) =  tmp2 * fjac(ib,2,1,jp)  &
     &              - tmp1 * njac(ib,2,1,jp)
               lhsc(ib,2,2,j) =  tmp2 * fjac(ib,2,2,jp)  &
     &              - tmp1 * njac(ib,2,2,jp)  &
     &              - tmp1 * dy2
               lhsc(ib,2,3,j) =  tmp2 * fjac(ib,2,3,jp)  &
     &              - tmp1 * njac(ib,2,3,jp)
               lhsc(ib,2,4,j) =  tmp2 * fjac(ib,2,4,jp)  &
     &              - tmp1 * njac(ib,2,4,jp)
               lhsc(ib,2,5,j) =  tmp2 * fjac(ib,2,5,jp)  &
     &              - tmp1 * njac(ib,2,5,jp)

               lhsc(ib,3,1,j) =  tmp2 * fjac(ib,3,1,jp)  &
     &              - tmp1 * njac(ib,3,1,jp)
               lhsc(ib,3,2,j) =  tmp2 * fjac(ib,3,2,jp)  &
     &              - tmp1 * njac(ib,3,2,jp)
               lhsc(ib,3,3,j) =  tmp2 * fjac(ib,3,3,jp)  &
     &              - tmp1 * njac(ib,3,3,jp)  &
     &              - tmp1 * dy3
               lhsc(ib,3,4,j) =  tmp2 * fjac(ib,3,4,jp)  &
     &              - tmp1 * njac(ib,3,4,jp)
               lhsc(ib,3,5,j) =  tmp2 * fjac(ib,3,5,jp)  &
     &              - tmp1 * njac(ib,3,5,jp)

               lhsc(ib,4,1,j) =  tmp2 * fjac(ib,4,1,jp)  &
     &              - tmp1 * njac(ib,4,1,jp)
               lhsc(ib,4,2,j) =  tmp2 * fjac(ib,4,2,jp)  &
     &              - tmp1 * njac(ib,4,2,jp)
               lhsc(ib,4,3,j) =  tmp2 * fjac(ib,4,3,jp)  &
     &              - tmp1 * njac(ib,4,3,jp)
               lhsc(ib,4,4,j) =  tmp2 * fjac(ib,4,4,jp)  &
     &              - tmp1 * njac(ib,4,4,jp)  &
     &              - tmp1 * dy4
               lhsc(ib,4,5,j) =  tmp2 * fjac(ib,4,5,jp)  &
     &              - tmp1 * njac(ib,4,5,jp)

               lhsc(ib,5,1,j) =  tmp2 * fjac(ib,5,1,jp)  &
     &              - tmp1 * njac(ib,5,1,jp)
               lhsc(ib,5,2,j) =  tmp2 * fjac(ib,5,2,jp)  &
     &              - tmp1 * njac(ib,5,2,jp)
               lhsc(ib,5,3,j) =  tmp2 * fjac(ib,5,3,jp)  &
     &              - tmp1 * njac(ib,5,3,jp)
               lhsc(ib,5,4,j) =  tmp2 * fjac(ib,5,4,jp)  &
     &              - tmp1 * njac(ib,5,4,jp)
               lhsc(ib,5,5,j) =  tmp2 * fjac(ib,5,5,jp)  &
     &              - tmp1 * njac(ib,5,5,jp)  &
     &              - tmp1 * dy5

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
!     c'(JMAX) and rhs'(JMAX) will be sent to next cell
!---------------------------------------------------------------------

            if (timeron) call timer_start(t_solsub)
!---------------------------------------------------------------------
!     multiply c(i,0,k) by b_inverse and copy back to c
!     multiply rhs(0) by b_inverse(0) and copy to rhs
!---------------------------------------------------------------------
            if (jj .eq. 1) then
            call binvcrhs( lhsb(1,1,1,0),  &
     &                        lhsc(1,1,1,0),  &
     &                        rhsx(1,1,0) )
            endif

!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     subtract A*lhs_vector(j-1) from lhs_vector(j)
!     
!     rhs(j) = rhs(j) - A*rhs(j-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,1,1),  &
     &                         rhsx(1,1,j-1),rhsx(1,1,j))

!---------------------------------------------------------------------
!     B(j) = B(j) - C(j-1)*A(j)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,1,1),  &
     &                         lhsc(1,1,1,j-1),  &
     &                         lhsb(1,1,1,1))

!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to c
!     multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,1,1),  &
     &                        lhsc(1,1,1,j),  &
     &                        rhsx(1,1,j) )


            if (jj .eq. jsize-1) then
!---------------------------------------------------------------------
!     rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
!---------------------------------------------------------------------
            call matvec_sub(lhsa(1,1,1,2),  &
     &                         rhsx(1,1,jsize-1),rhsx(1,1,jsize))

!---------------------------------------------------------------------
!     B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
!     call matmul_sub(aa,i,jsize,k,c,
!     $              cc,i,jsize-1,k,c,bb,i,jsize,k)
!---------------------------------------------------------------------
            call matmul_sub(lhsa(1,1,1,2),  &
     &                         lhsc(1,1,1,jsize-1),  &
     &                         lhsb(1,1,1,2))

!---------------------------------------------------------------------
!     multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
!---------------------------------------------------------------------
            call binvrhs( lhsb(1,1,1,2),  &
     &                       rhsx(1,1,jsize) )
            endif

            if (timeron) call timer_stop(t_solsub)

            enddo

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(jsize)=rhs(jsize)
!     else assume U(jsize) is loaded in un pack backsub_info
!     so just use it
!     after call u(jstart) will be sent to next cell
!---------------------------------------------------------------------
      
            do j=jsize-1,0,-1
!dir$ vector always
            do ib=1,bsize
!dir$ unroll
               do m=1,BLOCK_SIZE
                  rhsx(ib,m,j) = rhsx(ib,m,j)   &
     &                 - lhsc(ib,m,1,j)*rhsx(ib,1,j+1)  &
     &                 - lhsc(ib,m,2,j)*rhsx(ib,2,j+1)  &
     &                 - lhsc(ib,m,3,j)*rhsx(ib,3,j+1)  &
     &                 - lhsc(ib,m,4,j)*rhsx(ib,4,j+1)  &
     &                 - lhsc(ib,m,5,j)*rhsx(ib,5,j+1)
               enddo
            enddo
            enddo

            if (timeron) call timer_start(t_rdis1)
            do ib = 1, bsize
               i = ii+ib-1
               if (i .lt. grid_points(1)-1) then
               do j=0,jsize
                  rhs(1,i,j,k) = rhsx(ib,1,j)
                  rhs(2,i,j,k) = rhsx(ib,2,j)
                  rhs(3,i,j,k) = rhsx(ib,3,j)
                  rhs(4,i,j,k) = rhsx(ib,4,j)
                  rhs(5,i,j,k) = rhsx(ib,5,j)
               end do
               endif
            end do
            if (timeron) call timer_stop(t_rdis1)

         enddo
      enddo
!$omp end parallel
      if (timeron) call timer_stop(t_ysolve)

      return
      end


