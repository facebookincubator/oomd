
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine x_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     
!     Performs line solves in X direction by first factoring
!     the block-tridiagonal matrix into an upper triangular matrix, 
!     and then performing back substitution to solve for the unknow
!     vectors of each line.  
!     
!     Make sure we treat elements zero to cell_size in the direction
!     of the sweep.
!     
!---------------------------------------------------------------------

      use bt_data
      use work_lhs

      implicit none

      integer i,j,k,m,isize
      integer ii,im,ip,ib,jj,jb
      double precision tmp1, tmp2, tmp3

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      if (timeron) call timer_start(t_xsolve)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     This function computes the left hand side in the xi-direction
!---------------------------------------------------------------------

      isize = grid_points(1)-1

!---------------------------------------------------------------------
!     determine a (labeled f) and n jacobians
!---------------------------------------------------------------------
!$omp parallel default(shared) shared(isize)  &
!$omp& private(i,j,k,m,ii,im,ip,ib,jj,jb,tmp1,tmp2,tmp3)

      call lhsinit(isize)

!$omp do collapse(2)
      do k = 1, grid_points(3)-2
         do jj = 1, grid_points(2)-2, bsize

            if (timeron) call timer_start(t_rdis1)
            do i=0,isize
            do jb = 1, bsize
               j = min(jj+jb-1, grid_points(2)-2)
               rhsx(jb,1,i) = rhs(1,i,j,k)
               rhsx(jb,2,i) = rhs(2,i,j,k)
               rhsx(jb,3,i) = rhs(3,i,j,k)
               rhsx(jb,4,i) = rhs(4,i,j,k)
               rhsx(jb,5,i) = rhs(5,i,j,k)
            end do
            end do
            if (timeron) call timer_stop(t_rdis1)

            call lhsinit(0)

            ib = 0
            do ii = 1, isize-1
            ib = mod(ib + 1, 3)
            im = min(2*ii - 3, 1)     ! -1 or 1
            ip = mod(ib + im, 3) - 1

            do i = ii+im, ii+1
            ip = ip + 1
            do jb = 1, bsize
               j = min(jj+jb-1, grid_points(2)-2)

               tmp1 = rho_i(i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2
!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------
               fjac(jb,1,1,ip) = 0.0d+00
               fjac(jb,1,2,ip) = 1.0d+00
               fjac(jb,1,3,ip) = 0.0d+00
               fjac(jb,1,4,ip) = 0.0d+00
               fjac(jb,1,5,ip) = 0.0d+00

               fjac(jb,2,1,ip) = -(u(2,i,j,k) * tmp2 *   &
     &              u(2,i,j,k))  &
     &              + c2 * qs(i,j,k)
               fjac(jb,2,2,ip) = ( 2.0d+00 - c2 )  &
     &              * ( u(2,i,j,k) / u(1,i,j,k) )
               fjac(jb,2,3,ip) = - c2 * ( u(3,i,j,k) * tmp1 )
               fjac(jb,2,4,ip) = - c2 * ( u(4,i,j,k) * tmp1 )
               fjac(jb,2,5,ip) = c2

               fjac(jb,3,1,ip) = - ( u(2,i,j,k)*u(3,i,j,k) ) * tmp2
               fjac(jb,3,2,ip) = u(3,i,j,k) * tmp1
               fjac(jb,3,3,ip) = u(2,i,j,k) * tmp1
               fjac(jb,3,4,ip) = 0.0d+00
               fjac(jb,3,5,ip) = 0.0d+00

               fjac(jb,4,1,ip) = - ( u(2,i,j,k)*u(4,i,j,k) ) * tmp2
               fjac(jb,4,2,ip) = u(4,i,j,k) * tmp1
               fjac(jb,4,3,ip) = 0.0d+00
               fjac(jb,4,4,ip) = u(2,i,j,k) * tmp1
               fjac(jb,4,5,ip) = 0.0d+00

               fjac(jb,5,1,ip) = ( c2 * 2.0d0 * square(i,j,k)  &
     &              - c1 * u(5,i,j,k) )  &
     &              * ( u(2,i,j,k) * tmp2 )
               fjac(jb,5,2,ip) = c1 *  u(5,i,j,k) * tmp1   &
     &              - c2  &
     &              * ( u(2,i,j,k)*u(2,i,j,k) * tmp2  &
     &              + qs(i,j,k) )
               fjac(jb,5,3,ip) = - c2 * ( u(3,i,j,k)*u(2,i,j,k) )  &
     &              * tmp2
               fjac(jb,5,4,ip) = - c2 * ( u(4,i,j,k)*u(2,i,j,k) )  &
     &              * tmp2
               fjac(jb,5,5,ip) = c1 * ( u(2,i,j,k) * tmp1 )

               njac(jb,1,1,ip) = 0.0d+00
               njac(jb,1,2,ip) = 0.0d+00
               njac(jb,1,3,ip) = 0.0d+00
               njac(jb,1,4,ip) = 0.0d+00
               njac(jb,1,5,ip) = 0.0d+00

               njac(jb,2,1,ip) = - con43 * c3c4 * tmp2 * u(2,i,j,k)
               njac(jb,2,2,ip) =   con43 * c3c4 * tmp1
               njac(jb,2,3,ip) =   0.0d+00
               njac(jb,2,4,ip) =   0.0d+00
               njac(jb,2,5,ip) =   0.0d+00

               njac(jb,3,1,ip) = - c3c4 * tmp2 * u(3,i,j,k)
               njac(jb,3,2,ip) =   0.0d+00
               njac(jb,3,3,ip) =   c3c4 * tmp1
               njac(jb,3,4,ip) =   0.0d+00
               njac(jb,3,5,ip) =   0.0d+00

               njac(jb,4,1,ip) = - c3c4 * tmp2 * u(4,i,j,k)
               njac(jb,4,2,ip) =   0.0d+00 
               njac(jb,4,3,ip) =   0.0d+00
               njac(jb,4,4,ip) =   c3c4 * tmp1
               njac(jb,4,5,ip) =   0.0d+00

               njac(jb,5,1,ip) = - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (u(2,i,j,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(3,i,j,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(4,i,j,k)**2)  &
     &              - c1345 * tmp2 * u(5,i,j,k)

               njac(jb,5,2,ip) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * u(2,i,j,k)
               njac(jb,5,3,ip) = ( c3c4 - c1345 ) * tmp2 * u(3,i,j,k)
               njac(jb,5,4,ip) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k)
               njac(jb,5,5,ip) = ( c1345 ) * tmp1

            enddo
            enddo

!---------------------------------------------------------------------
!     now jacobians set, so form left hand side in x direction
!---------------------------------------------------------------------
            im = mod(ib + 2, 3)
            i = ii
!dir$ vector always
            do jb = 1, bsize

               tmp1 = dt * tx1
               tmp2 = dt * tx2

               lhsa(jb,1,1,1) = - tmp2 * fjac(jb,1,1,im)  &
     &              - tmp1 * njac(jb,1,1,im)  &
     &              - tmp1 * dx1 
               lhsa(jb,1,2,1) = - tmp2 * fjac(jb,1,2,im)  &
     &              - tmp1 * njac(jb,1,2,im)
               lhsa(jb,1,3,1) = - tmp2 * fjac(jb,1,3,im)  &
     &              - tmp1 * njac(jb,1,3,im)
               lhsa(jb,1,4,1) = - tmp2 * fjac(jb,1,4,im)  &
     &              - tmp1 * njac(jb,1,4,im)
               lhsa(jb,1,5,1) = - tmp2 * fjac(jb,1,5,im)  &
     &              - tmp1 * njac(jb,1,5,im)

               lhsa(jb,2,1,1) = - tmp2 * fjac(jb,2,1,im)  &
     &              - tmp1 * njac(jb,2,1,im)
               lhsa(jb,2,2,1) = - tmp2 * fjac(jb,2,2,im)  &
     &              - tmp1 * njac(jb,2,2,im)  &
     &              - tmp1 * dx2
               lhsa(jb,2,3,1) = - tmp2 * fjac(jb,2,3,im)  &
     &              - tmp1 * njac(jb,2,3,im)
               lhsa(jb,2,4,1) = - tmp2 * fjac(jb,2,4,im)  &
     &              - tmp1 * njac(jb,2,4,im)
               lhsa(jb,2,5,1) = - tmp2 * fjac(jb,2,5,im)  &
     &              - tmp1 * njac(jb,2,5,im)

               lhsa(jb,3,1,1) = - tmp2 * fjac(jb,3,1,im)  &
     &              - tmp1 * njac(jb,3,1,im)
               lhsa(jb,3,2,1) = - tmp2 * fjac(jb,3,2,im)  &
     &              - tmp1 * njac(jb,3,2,im)
               lhsa(jb,3,3,1) = - tmp2 * fjac(jb,3,3,im)  &
     &              - tmp1 * njac(jb,3,3,im)  &
     &              - tmp1 * dx3 
               lhsa(jb,3,4,1) = - tmp2 * fjac(jb,3,4,im)  &
     &              - tmp1 * njac(jb,3,4,im)
               lhsa(jb,3,5,1) = - tmp2 * fjac(jb,3,5,im)  &
     &              - tmp1 * njac(jb,3,5,im)

               lhsa(jb,4,1,1) = - tmp2 * fjac(jb,4,1,im)  &
     &              - tmp1 * njac(jb,4,1,im)
               lhsa(jb,4,2,1) = - tmp2 * fjac(jb,4,2,im)  &
     &              - tmp1 * njac(jb,4,2,im)
               lhsa(jb,4,3,1) = - tmp2 * fjac(jb,4,3,im)  &
     &              - tmp1 * njac(jb,4,3,im)
               lhsa(jb,4,4,1) = - tmp2 * fjac(jb,4,4,im)  &
     &              - tmp1 * njac(jb,4,4,im)  &
     &              - tmp1 * dx4
               lhsa(jb,4,5,1) = - tmp2 * fjac(jb,4,5,im)  &
     &              - tmp1 * njac(jb,4,5,im)

               lhsa(jb,5,1,1) = - tmp2 * fjac(jb,5,1,im)  &
     &              - tmp1 * njac(jb,5,1,im)
               lhsa(jb,5,2,1) = - tmp2 * fjac(jb,5,2,im)  &
     &              - tmp1 * njac(jb,5,2,im)
               lhsa(jb,5,3,1) = - tmp2 * fjac(jb,5,3,im)  &
     &              - tmp1 * njac(jb,5,3,im)
               lhsa(jb,5,4,1) = - tmp2 * fjac(jb,5,4,im)  &
     &              - tmp1 * njac(jb,5,4,im)
               lhsa(jb,5,5,1) = - tmp2 * fjac(jb,5,5,im)  &
     &              - tmp1 * njac(jb,5,5,im)  &
     &              - tmp1 * dx5

               lhsb(jb,1,1,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(jb,1,1,ib)  &
     &              + tmp1 * 2.0d+00 * dx1
               lhsb(jb,1,2,1) = tmp1 * 2.0d+00 * njac(jb,1,2,ib)
               lhsb(jb,1,3,1) = tmp1 * 2.0d+00 * njac(jb,1,3,ib)
               lhsb(jb,1,4,1) = tmp1 * 2.0d+00 * njac(jb,1,4,ib)
               lhsb(jb,1,5,1) = tmp1 * 2.0d+00 * njac(jb,1,5,ib)

               lhsb(jb,2,1,1) = tmp1 * 2.0d+00 * njac(jb,2,1,ib)
               lhsb(jb,2,2,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(jb,2,2,ib)  &
     &              + tmp1 * 2.0d+00 * dx2
               lhsb(jb,2,3,1) = tmp1 * 2.0d+00 * njac(jb,2,3,ib)
               lhsb(jb,2,4,1) = tmp1 * 2.0d+00 * njac(jb,2,4,ib)
               lhsb(jb,2,5,1) = tmp1 * 2.0d+00 * njac(jb,2,5,ib)

               lhsb(jb,3,1,1) = tmp1 * 2.0d+00 * njac(jb,3,1,ib)
               lhsb(jb,3,2,1) = tmp1 * 2.0d+00 * njac(jb,3,2,ib)
               lhsb(jb,3,3,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(jb,3,3,ib)  &
     &              + tmp1 * 2.0d+00 * dx3
               lhsb(jb,3,4,1) = tmp1 * 2.0d+00 * njac(jb,3,4,ib)
               lhsb(jb,3,5,1) = tmp1 * 2.0d+00 * njac(jb,3,5,ib)

               lhsb(jb,4,1,1) = tmp1 * 2.0d+00 * njac(jb,4,1,ib)
               lhsb(jb,4,2,1) = tmp1 * 2.0d+00 * njac(jb,4,2,ib)
               lhsb(jb,4,3,1) = tmp1 * 2.0d+00 * njac(jb,4,3,ib)
               lhsb(jb,4,4,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(jb,4,4,ib)  &
     &              + tmp1 * 2.0d+00 * dx4
               lhsb(jb,4,5,1) = tmp1 * 2.0d+00 * njac(jb,4,5,ib)

               lhsb(jb,5,1,1) = tmp1 * 2.0d+00 * njac(jb,5,1,ib)
               lhsb(jb,5,2,1) = tmp1 * 2.0d+00 * njac(jb,5,2,ib)
               lhsb(jb,5,3,1) = tmp1 * 2.0d+00 * njac(jb,5,3,ib)
               lhsb(jb,5,4,1) = tmp1 * 2.0d+00 * njac(jb,5,4,ib)
               lhsb(jb,5,5,1) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(jb,5,5,ib)  &
     &              + tmp1 * 2.0d+00 * dx5

               lhsc(jb,1,1,i) =  tmp2 * fjac(jb,1,1,ip)  &
     &              - tmp1 * njac(jb,1,1,ip)  &
     &              - tmp1 * dx1
               lhsc(jb,1,2,i) =  tmp2 * fjac(jb,1,2,ip)  &
     &              - tmp1 * njac(jb,1,2,ip)
               lhsc(jb,1,3,i) =  tmp2 * fjac(jb,1,3,ip)  &
     &              - tmp1 * njac(jb,1,3,ip)
               lhsc(jb,1,4,i) =  tmp2 * fjac(jb,1,4,ip)  &
     &              - tmp1 * njac(jb,1,4,ip)
               lhsc(jb,1,5,i) =  tmp2 * fjac(jb,1,5,ip)  &
     &              - tmp1 * njac(jb,1,5,ip)

               lhsc(jb,2,1,i) =  tmp2 * fjac(jb,2,1,ip)  &
     &              - tmp1 * njac(jb,2,1,ip)
               lhsc(jb,2,2,i) =  tmp2 * fjac(jb,2,2,ip)  &
     &              - tmp1 * njac(jb,2,2,ip)  &
     &              - tmp1 * dx2
               lhsc(jb,2,3,i) =  tmp2 * fjac(jb,2,3,ip)  &
     &              - tmp1 * njac(jb,2,3,ip)
               lhsc(jb,2,4,i) =  tmp2 * fjac(jb,2,4,ip)  &
     &              - tmp1 * njac(jb,2,4,ip)
               lhsc(jb,2,5,i) =  tmp2 * fjac(jb,2,5,ip)  &
     &              - tmp1 * njac(jb,2,5,ip)

               lhsc(jb,3,1,i) =  tmp2 * fjac(jb,3,1,ip)  &
     &              - tmp1 * njac(jb,3,1,ip)
               lhsc(jb,3,2,i) =  tmp2 * fjac(jb,3,2,ip)  &
     &              - tmp1 * njac(jb,3,2,ip)
               lhsc(jb,3,3,i) =  tmp2 * fjac(jb,3,3,ip)  &
     &              - tmp1 * njac(jb,3,3,ip)  &
     &              - tmp1 * dx3
               lhsc(jb,3,4,i) =  tmp2 * fjac(jb,3,4,ip)  &
     &              - tmp1 * njac(jb,3,4,ip)
               lhsc(jb,3,5,i) =  tmp2 * fjac(jb,3,5,ip)  &
     &              - tmp1 * njac(jb,3,5,ip)

               lhsc(jb,4,1,i) =  tmp2 * fjac(jb,4,1,ip)  &
     &              - tmp1 * njac(jb,4,1,ip)
               lhsc(jb,4,2,i) =  tmp2 * fjac(jb,4,2,ip)  &
     &              - tmp1 * njac(jb,4,2,ip)
               lhsc(jb,4,3,i) =  tmp2 * fjac(jb,4,3,ip)  &
     &              - tmp1 * njac(jb,4,3,ip)
               lhsc(jb,4,4,i) =  tmp2 * fjac(jb,4,4,ip)  &
     &              - tmp1 * njac(jb,4,4,ip)  &
     &              - tmp1 * dx4
               lhsc(jb,4,5,i) =  tmp2 * fjac(jb,4,5,ip)  &
     &              - tmp1 * njac(jb,4,5,ip)

               lhsc(jb,5,1,i) =  tmp2 * fjac(jb,5,1,ip)  &
     &              - tmp1 * njac(jb,5,1,ip)
               lhsc(jb,5,2,i) =  tmp2 * fjac(jb,5,2,ip)  &
     &              - tmp1 * njac(jb,5,2,ip)
               lhsc(jb,5,3,i) =  tmp2 * fjac(jb,5,3,ip)  &
     &              - tmp1 * njac(jb,5,3,ip)
               lhsc(jb,5,4,i) =  tmp2 * fjac(jb,5,4,ip)  &
     &              - tmp1 * njac(jb,5,4,ip)
               lhsc(jb,5,5,i) =  tmp2 * fjac(jb,5,5,ip)  &
     &              - tmp1 * njac(jb,5,5,ip)  &
     &              - tmp1 * dx5

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
!     c'(IMAX) and rhs'(IMAX) will be sent to next cell
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     outer most do loops - sweeping in i direction
!---------------------------------------------------------------------

            if (timeron) call timer_start(t_solsub)
!---------------------------------------------------------------------
!     multiply c(0,j,k) by b_inverse and copy back to c
!     multiply rhs(0) by b_inverse(0) and copy to rhs
!---------------------------------------------------------------------
            if (ii .eq. 1) then
               call binvcrhs( lhsb(1,1,1,0),  &
     &                        lhsc(1,1,1,0),  &
     &                        rhsx(1,1,0) )
            endif

!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     rhs(i) = rhs(i) - A*rhs(i-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,1,1),  &
     &                         rhsx(1,1,i-1),rhsx(1,1,i))

!---------------------------------------------------------------------
!     B(i) = B(i) - C(i-1)*A(i)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,1,1),  &
     &                         lhsc(1,1,1,i-1),  &
     &                         lhsb(1,1,1,1))


!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to c
!     multiply rhs(1,j,k) by b_inverse(1,j,k) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,1,1),  &
     &                        lhsc(1,1,1,i),  &
     &                        rhsx(1,1,i) )


            if (ii .eq. isize-1) then
!---------------------------------------------------------------------
!     rhs(isize) = rhs(isize) - A*rhs(isize-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,1,2),  &
     &                         rhsx(1,1,isize-1),rhsx(1,1,isize))

!---------------------------------------------------------------------
!     B(isize) = B(isize) - C(isize-1)*A(isize)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,1,2),  &
     &                         lhsc(1,1,1,isize-1),  &
     &                         lhsb(1,1,1,2))

!---------------------------------------------------------------------
!     multiply rhs() by b_inverse() and copy to rhs
!---------------------------------------------------------------------
               call binvrhs( lhsb(1,1,1,2),  &
     &                       rhsx(1,1,isize) )
            endif
            if (timeron) call timer_stop(t_solsub)

            enddo

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(isize)=rhs(isize)
!     else assume U(isize) is loaded in un pack backsub_info
!     so just use it
!     after call u(istart) will be sent to next cell
!---------------------------------------------------------------------

            do i=isize-1,0,-1
!dir$ vector always
            do jb=1,bsize
!dir$ unroll
               do m=1,BLOCK_SIZE
                  rhsx(jb,m,i) = rhsx(jb,m,i)   &
     &                 - lhsc(jb,m,1,i)*rhsx(jb,1,i+1)  &
     &                 - lhsc(jb,m,2,i)*rhsx(jb,2,i+1)  &
     &                 - lhsc(jb,m,3,i)*rhsx(jb,3,i+1)  &
     &                 - lhsc(jb,m,4,i)*rhsx(jb,4,i+1)  &
     &                 - lhsc(jb,m,5,i)*rhsx(jb,5,i+1)
               enddo
            enddo
            enddo

            if (timeron) call timer_start(t_rdis1)
            do jb = 1, bsize
               j = jj+jb-1
               if (j .lt. grid_points(2)-1) then
               do i=0,isize
                  rhs(1,i,j,k) = rhsx(jb,1,i)
                  rhs(2,i,j,k) = rhsx(jb,2,i)
                  rhs(3,i,j,k) = rhsx(jb,3,i)
                  rhs(4,i,j,k) = rhsx(jb,4,i)
                  rhs(5,i,j,k) = rhsx(jb,5,i)
               end do
               endif
            end do
            if (timeron) call timer_stop(t_rdis1)

         enddo
      enddo
!$omp end parallel
      if (timeron) call timer_stop(t_xsolve)

      return
      end



