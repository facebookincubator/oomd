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

      integer i, j, k, m, n, ksize
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
!$omp parallel do default(shared) shared(ksize) collapse(2)  &
!$omp& private(i,j,k,m,n,tmp1,tmp2,tmp3)
      do j = 1, grid_points(2)-2
         do i = 1, grid_points(1)-2
            do k = 0, ksize

               tmp1 = 1.0d+00 / u(1,i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               fjac(1,1,k) = 0.0d+00
               fjac(1,2,k) = 0.0d+00
               fjac(1,3,k) = 0.0d+00
               fjac(1,4,k) = 1.0d+00
               fjac(1,5,k) = 0.0d+00

               fjac(2,1,k) = - ( u(2,i,j,k)*u(4,i,j,k) )   &
     &              * tmp2 
               fjac(2,2,k) = u(4,i,j,k) * tmp1
               fjac(2,3,k) = 0.0d+00
               fjac(2,4,k) = u(2,i,j,k) * tmp1
               fjac(2,5,k) = 0.0d+00

               fjac(3,1,k) = - ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2 
               fjac(3,2,k) = 0.0d+00
               fjac(3,3,k) = u(4,i,j,k) * tmp1
               fjac(3,4,k) = u(3,i,j,k) * tmp1
               fjac(3,5,k) = 0.0d+00

               fjac(4,1,k) = - (u(4,i,j,k)*u(4,i,j,k) * tmp2 )   &
     &              + c2 * qs(i,j,k)
               fjac(4,2,k) = - c2 *  u(2,i,j,k) * tmp1 
               fjac(4,3,k) = - c2 *  u(3,i,j,k) * tmp1
               fjac(4,4,k) = ( 2.0d+00 - c2 )  &
     &              *  u(4,i,j,k) * tmp1 
               fjac(4,5,k) = c2

               fjac(5,1,k) = ( c2 * 2.0d0 * square(i,j,k)   &
     &              - c1 * u(5,i,j,k) )  &
     &              * u(4,i,j,k) * tmp2
               fjac(5,2,k) = - c2 * ( u(2,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2 
               fjac(5,3,k) = - c2 * ( u(3,i,j,k)*u(4,i,j,k) )  &
     &              * tmp2
               fjac(5,4,k) = c1 * ( u(5,i,j,k) * tmp1 )  &
     &              - c2  &
     &              * ( qs(i,j,k)  &
     &              + u(4,i,j,k)*u(4,i,j,k) * tmp2 )
               fjac(5,5,k) = c1 * u(4,i,j,k) * tmp1

               njac(1,1,k) = 0.0d+00
               njac(1,2,k) = 0.0d+00
               njac(1,3,k) = 0.0d+00
               njac(1,4,k) = 0.0d+00
               njac(1,5,k) = 0.0d+00

               njac(2,1,k) = - c3c4 * tmp2 * u(2,i,j,k)
               njac(2,2,k) =   c3c4 * tmp1
               njac(2,3,k) =   0.0d+00
               njac(2,4,k) =   0.0d+00
               njac(2,5,k) =   0.0d+00

               njac(3,1,k) = - c3c4 * tmp2 * u(3,i,j,k)
               njac(3,2,k) =   0.0d+00
               njac(3,3,k) =   c3c4 * tmp1
               njac(3,4,k) =   0.0d+00
               njac(3,5,k) =   0.0d+00

               njac(4,1,k) = - con43 * c3c4 * tmp2 * u(4,i,j,k)
               njac(4,2,k) =   0.0d+00
               njac(4,3,k) =   0.0d+00
               njac(4,4,k) =   con43 * c3 * c4 * tmp1
               njac(4,5,k) =   0.0d+00

               njac(5,1,k) = - (  c3c4  &
     &              - c1345 ) * tmp3 * (u(2,i,j,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(3,i,j,k)**2)  &
     &              - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (u(4,i,j,k)**2)  &
     &              - c1345 * tmp2 * u(5,i,j,k)

               njac(5,2,k) = (  c3c4 - c1345 ) * tmp2 * u(2,i,j,k)
               njac(5,3,k) = (  c3c4 - c1345 ) * tmp2 * u(3,i,j,k)
               njac(5,4,k) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * u(4,i,j,k)
               njac(5,5,k) = ( c1345 )* tmp1

            enddo

!---------------------------------------------------------------------
!     now jacobians set, so form left hand side in z direction
!---------------------------------------------------------------------
            call lhsinit(lhs, ksize)
            do k = 1, ksize-1

               tmp1 = dt * tz1
               tmp2 = dt * tz2

               lhs(1,1,aa,k) = - tmp2 * fjac(1,1,k-1)  &
     &              - tmp1 * njac(1,1,k-1)  &
     &              - tmp1 * dz1 
               lhs(1,2,aa,k) = - tmp2 * fjac(1,2,k-1)  &
     &              - tmp1 * njac(1,2,k-1)
               lhs(1,3,aa,k) = - tmp2 * fjac(1,3,k-1)  &
     &              - tmp1 * njac(1,3,k-1)
               lhs(1,4,aa,k) = - tmp2 * fjac(1,4,k-1)  &
     &              - tmp1 * njac(1,4,k-1)
               lhs(1,5,aa,k) = - tmp2 * fjac(1,5,k-1)  &
     &              - tmp1 * njac(1,5,k-1)

               lhs(2,1,aa,k) = - tmp2 * fjac(2,1,k-1)  &
     &              - tmp1 * njac(2,1,k-1)
               lhs(2,2,aa,k) = - tmp2 * fjac(2,2,k-1)  &
     &              - tmp1 * njac(2,2,k-1)  &
     &              - tmp1 * dz2
               lhs(2,3,aa,k) = - tmp2 * fjac(2,3,k-1)  &
     &              - tmp1 * njac(2,3,k-1)
               lhs(2,4,aa,k) = - tmp2 * fjac(2,4,k-1)  &
     &              - tmp1 * njac(2,4,k-1)
               lhs(2,5,aa,k) = - tmp2 * fjac(2,5,k-1)  &
     &              - tmp1 * njac(2,5,k-1)

               lhs(3,1,aa,k) = - tmp2 * fjac(3,1,k-1)  &
     &              - tmp1 * njac(3,1,k-1)
               lhs(3,2,aa,k) = - tmp2 * fjac(3,2,k-1)  &
     &              - tmp1 * njac(3,2,k-1)
               lhs(3,3,aa,k) = - tmp2 * fjac(3,3,k-1)  &
     &              - tmp1 * njac(3,3,k-1)  &
     &              - tmp1 * dz3 
               lhs(3,4,aa,k) = - tmp2 * fjac(3,4,k-1)  &
     &              - tmp1 * njac(3,4,k-1)
               lhs(3,5,aa,k) = - tmp2 * fjac(3,5,k-1)  &
     &              - tmp1 * njac(3,5,k-1)

               lhs(4,1,aa,k) = - tmp2 * fjac(4,1,k-1)  &
     &              - tmp1 * njac(4,1,k-1)
               lhs(4,2,aa,k) = - tmp2 * fjac(4,2,k-1)  &
     &              - tmp1 * njac(4,2,k-1)
               lhs(4,3,aa,k) = - tmp2 * fjac(4,3,k-1)  &
     &              - tmp1 * njac(4,3,k-1)
               lhs(4,4,aa,k) = - tmp2 * fjac(4,4,k-1)  &
     &              - tmp1 * njac(4,4,k-1)  &
     &              - tmp1 * dz4
               lhs(4,5,aa,k) = - tmp2 * fjac(4,5,k-1)  &
     &              - tmp1 * njac(4,5,k-1)

               lhs(5,1,aa,k) = - tmp2 * fjac(5,1,k-1)  &
     &              - tmp1 * njac(5,1,k-1)
               lhs(5,2,aa,k) = - tmp2 * fjac(5,2,k-1)  &
     &              - tmp1 * njac(5,2,k-1)
               lhs(5,3,aa,k) = - tmp2 * fjac(5,3,k-1)  &
     &              - tmp1 * njac(5,3,k-1)
               lhs(5,4,aa,k) = - tmp2 * fjac(5,4,k-1)  &
     &              - tmp1 * njac(5,4,k-1)
               lhs(5,5,aa,k) = - tmp2 * fjac(5,5,k-1)  &
     &              - tmp1 * njac(5,5,k-1)  &
     &              - tmp1 * dz5

               lhs(1,1,bb,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(1,1,k)  &
     &              + tmp1 * 2.0d+00 * dz1
               lhs(1,2,bb,k) = tmp1 * 2.0d+00 * njac(1,2,k)
               lhs(1,3,bb,k) = tmp1 * 2.0d+00 * njac(1,3,k)
               lhs(1,4,bb,k) = tmp1 * 2.0d+00 * njac(1,4,k)
               lhs(1,5,bb,k) = tmp1 * 2.0d+00 * njac(1,5,k)

               lhs(2,1,bb,k) = tmp1 * 2.0d+00 * njac(2,1,k)
               lhs(2,2,bb,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(2,2,k)  &
     &              + tmp1 * 2.0d+00 * dz2
               lhs(2,3,bb,k) = tmp1 * 2.0d+00 * njac(2,3,k)
               lhs(2,4,bb,k) = tmp1 * 2.0d+00 * njac(2,4,k)
               lhs(2,5,bb,k) = tmp1 * 2.0d+00 * njac(2,5,k)

               lhs(3,1,bb,k) = tmp1 * 2.0d+00 * njac(3,1,k)
               lhs(3,2,bb,k) = tmp1 * 2.0d+00 * njac(3,2,k)
               lhs(3,3,bb,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(3,3,k)  &
     &              + tmp1 * 2.0d+00 * dz3
               lhs(3,4,bb,k) = tmp1 * 2.0d+00 * njac(3,4,k)
               lhs(3,5,bb,k) = tmp1 * 2.0d+00 * njac(3,5,k)

               lhs(4,1,bb,k) = tmp1 * 2.0d+00 * njac(4,1,k)
               lhs(4,2,bb,k) = tmp1 * 2.0d+00 * njac(4,2,k)
               lhs(4,3,bb,k) = tmp1 * 2.0d+00 * njac(4,3,k)
               lhs(4,4,bb,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(4,4,k)  &
     &              + tmp1 * 2.0d+00 * dz4
               lhs(4,5,bb,k) = tmp1 * 2.0d+00 * njac(4,5,k)

               lhs(5,1,bb,k) = tmp1 * 2.0d+00 * njac(5,1,k)
               lhs(5,2,bb,k) = tmp1 * 2.0d+00 * njac(5,2,k)
               lhs(5,3,bb,k) = tmp1 * 2.0d+00 * njac(5,3,k)
               lhs(5,4,bb,k) = tmp1 * 2.0d+00 * njac(5,4,k)
               lhs(5,5,bb,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(5,5,k)   &
     &              + tmp1 * 2.0d+00 * dz5

               lhs(1,1,cc,k) =  tmp2 * fjac(1,1,k+1)  &
     &              - tmp1 * njac(1,1,k+1)  &
     &              - tmp1 * dz1
               lhs(1,2,cc,k) =  tmp2 * fjac(1,2,k+1)  &
     &              - tmp1 * njac(1,2,k+1)
               lhs(1,3,cc,k) =  tmp2 * fjac(1,3,k+1)  &
     &              - tmp1 * njac(1,3,k+1)
               lhs(1,4,cc,k) =  tmp2 * fjac(1,4,k+1)  &
     &              - tmp1 * njac(1,4,k+1)
               lhs(1,5,cc,k) =  tmp2 * fjac(1,5,k+1)  &
     &              - tmp1 * njac(1,5,k+1)

               lhs(2,1,cc,k) =  tmp2 * fjac(2,1,k+1)  &
     &              - tmp1 * njac(2,1,k+1)
               lhs(2,2,cc,k) =  tmp2 * fjac(2,2,k+1)  &
     &              - tmp1 * njac(2,2,k+1)  &
     &              - tmp1 * dz2
               lhs(2,3,cc,k) =  tmp2 * fjac(2,3,k+1)  &
     &              - tmp1 * njac(2,3,k+1)
               lhs(2,4,cc,k) =  tmp2 * fjac(2,4,k+1)  &
     &              - tmp1 * njac(2,4,k+1)
               lhs(2,5,cc,k) =  tmp2 * fjac(2,5,k+1)  &
     &              - tmp1 * njac(2,5,k+1)

               lhs(3,1,cc,k) =  tmp2 * fjac(3,1,k+1)  &
     &              - tmp1 * njac(3,1,k+1)
               lhs(3,2,cc,k) =  tmp2 * fjac(3,2,k+1)  &
     &              - tmp1 * njac(3,2,k+1)
               lhs(3,3,cc,k) =  tmp2 * fjac(3,3,k+1)  &
     &              - tmp1 * njac(3,3,k+1)  &
     &              - tmp1 * dz3
               lhs(3,4,cc,k) =  tmp2 * fjac(3,4,k+1)  &
     &              - tmp1 * njac(3,4,k+1)
               lhs(3,5,cc,k) =  tmp2 * fjac(3,5,k+1)  &
     &              - tmp1 * njac(3,5,k+1)

               lhs(4,1,cc,k) =  tmp2 * fjac(4,1,k+1)  &
     &              - tmp1 * njac(4,1,k+1)
               lhs(4,2,cc,k) =  tmp2 * fjac(4,2,k+1)  &
     &              - tmp1 * njac(4,2,k+1)
               lhs(4,3,cc,k) =  tmp2 * fjac(4,3,k+1)  &
     &              - tmp1 * njac(4,3,k+1)
               lhs(4,4,cc,k) =  tmp2 * fjac(4,4,k+1)  &
     &              - tmp1 * njac(4,4,k+1)  &
     &              - tmp1 * dz4
               lhs(4,5,cc,k) =  tmp2 * fjac(4,5,k+1)  &
     &              - tmp1 * njac(4,5,k+1)

               lhs(5,1,cc,k) =  tmp2 * fjac(5,1,k+1)  &
     &              - tmp1 * njac(5,1,k+1)
               lhs(5,2,cc,k) =  tmp2 * fjac(5,2,k+1)  &
     &              - tmp1 * njac(5,2,k+1)
               lhs(5,3,cc,k) =  tmp2 * fjac(5,3,k+1)  &
     &              - tmp1 * njac(5,3,k+1)
               lhs(5,4,cc,k) =  tmp2 * fjac(5,4,k+1)  &
     &              - tmp1 * njac(5,4,k+1)
               lhs(5,5,cc,k) =  tmp2 * fjac(5,5,k+1)  &
     &              - tmp1 * njac(5,5,k+1)  &
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
            call binvcrhs( lhs(1,1,bb,0),  &
     &                        lhs(1,1,cc,0),  &
     &                        rhs(1,i,j,0) )


!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------
            do k=1,ksize-1

!---------------------------------------------------------------------
!     subtract A*lhs_vector(k-1) from lhs_vector(k)
!     
!     rhs(k) = rhs(k) - A*rhs(k-1)
!---------------------------------------------------------------------
               call matvec_sub(lhs(1,1,aa,k),  &
     &                         rhs(1,i,j,k-1),rhs(1,i,j,k))

!---------------------------------------------------------------------
!     B(k) = B(k) - C(k-1)*A(k)
!     call matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k)
!---------------------------------------------------------------------
               call matmul_sub(lhs(1,1,aa,k),  &
     &                         lhs(1,1,cc,k-1),  &
     &                         lhs(1,1,bb,k))

!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to c
!     multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhs(1,1,bb,k),  &
     &                        lhs(1,1,cc,k),  &
     &                        rhs(1,i,j,k) )

            enddo

!---------------------------------------------------------------------
!     Now finish up special cases for last cell
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
!---------------------------------------------------------------------
            call matvec_sub(lhs(1,1,aa,ksize),  &
     &                         rhs(1,i,j,ksize-1),rhs(1,i,j,ksize))

!---------------------------------------------------------------------
!     B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
!     call matmul_sub(aa,i,j,ksize,c,
!     $              cc,i,j,ksize-1,c,bb,i,j,ksize)
!---------------------------------------------------------------------
            call matmul_sub(lhs(1,1,aa,ksize),  &
     &                         lhs(1,1,cc,ksize-1),  &
     &                         lhs(1,1,bb,ksize))

!---------------------------------------------------------------------
!     multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
!---------------------------------------------------------------------
            call binvrhs( lhs(1,1,bb,ksize),  &
     &                       rhs(1,i,j,ksize) )
            if (timeron) call timer_stop(t_solsub)


!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(ksize)=rhs(ksize)
!     else assume U(ksize) is loaded in un pack backsub_info
!     so just use it
!     after call u(kstart) will be sent to next cell
!---------------------------------------------------------------------

            do k=ksize-1,0,-1
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k) = rhs(m,i,j,k)   &
     &                    - lhs(m,n,cc,k)*rhs(n,i,j,k+1)
                  enddo
               enddo
            enddo

         enddo
      enddo
      if (timeron) call timer_stop(t_zsolve)

      return
      end
