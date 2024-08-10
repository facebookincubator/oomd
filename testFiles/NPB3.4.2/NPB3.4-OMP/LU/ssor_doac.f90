!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine ssor(niter)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   to perform pseudo-time stepping SSOR iterations
!   for five nonlinear pde's.
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer niter

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m, n
      integer istep
      double precision  tmp, tmp2
      double precision  delunm(5)

      external timer_read
      double precision timer_read

 
!---------------------------------------------------------------------
!   begin pseudo-time stepping iterations
!---------------------------------------------------------------------
      tmp = 1.0d+00 / ( omega * ( 2.0d+00 - omega ) ) 

      do i = 1, t_last
         call timer_clear(i)
      end do

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
      call rhs
 
!---------------------------------------------------------------------
!   compute the L2 norms of newton iteration residuals
!---------------------------------------------------------------------
      call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,  &
     &             ist, iend, jst, jend,  &
     &             rsd, rsdnm )

 
      do i = 1, t_last
         call timer_clear(i)
      end do
      call timer_start(1)
 
!---------------------------------------------------------------------
!   the timestep loop
!---------------------------------------------------------------------
      do istep = 1, niter

         if (mod ( istep, 20) .eq. 0 .or.  &
     &         istep .eq. itmax .or.  &
     &         istep .eq. 1) then
            if (niter .gt. 1) write( *, 200) istep
 200        format(' Time step ', i4)
         endif
 
!---------------------------------------------------------------------
!   perform SSOR iteration
!---------------------------------------------------------------------
!$omp parallel default(shared) private(i,j,k,m,tmp2)  &
!$omp&  shared(ist,iend,jst,jend,nx,ny,nz,nx0,ny0,omega)
!$omp master
         if (timeron) call timer_start(t_rhs)
!$omp end master
         tmp2 = dt
!$omp do schedule(static) collapse(2)
         do k = 2, nz - 1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     rsd(m,i,j,k) = tmp2 * rsd(m,i,j,k)
                  end do
               end do
            end do
         end do
!$omp end do
!$omp master
         if (timeron) call timer_stop(t_rhs)

         if (timeron) call timer_start(t_blts)
!$omp end master
!$omp do schedule(static,1) ordered(2)
         do k = 2, nz -1 
            do j = jst, jend

!$omp ordered depend(sink: k-1,j) depend(sink: k,j-1)
!---------------------------------------------------------------------
!   form the lower triangular part of the jacobian matrix
!---------------------------------------------------------------------
               call jacld(j, k)
 
!---------------------------------------------------------------------
!   perform the lower triangular solution
!---------------------------------------------------------------------
               call blts( isiz1, isiz2, isiz3,  &
     &                    nx, ny, nz,  &
     &                    omega,  &
     &                    rsd,  &
     &                    a, b, c, d,  &
     &                    ist, iend, j, k )
!$omp ordered depend(source)

            end do
         end do
!$omp end do
!$omp master
         if (timeron) call timer_stop(t_blts)

         if (timeron) call timer_start(t_buts)
!$omp end master
!$omp do schedule(static,1) ordered(2)
         do k = nz - 1, 2, -1
            do j = jend, jst, -1

!$omp ordered depend(sink: k+1,j) depend(sink: k,j+1)
!---------------------------------------------------------------------
!   form the strictly upper triangular part of the jacobian matrix
!---------------------------------------------------------------------
               call jacu(j, k)

!---------------------------------------------------------------------
!   perform the upper triangular solution
!---------------------------------------------------------------------
               call buts( isiz1, isiz2, isiz3,  &
     &                    nx, ny, nz,  &
     &                    omega,  &
     &                    rsd,  &
     &                    d, a, b, c,  &
     &                    ist, iend, j, k )
!$omp ordered depend(source)

            end do
         end do
!$omp end do
!$omp master
         if (timeron) call timer_stop(t_buts)
!$omp end master

!---------------------------------------------------------------------
!   update the variables
!---------------------------------------------------------------------

!$omp master
         if (timeron) call timer_start(t_add)
!$omp end master
         tmp2 = tmp
!$omp do schedule(static) collapse(2)
         do k = 2, nz-1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     u( m, i, j, k ) = u( m, i, j, k )  &
     &                    + tmp2 * rsd( m, i, j, k )
                  end do
               end do
            end do
         end do
!$omp end do nowait
!$omp master
         if (timeron) call timer_stop(t_add)
!$omp end master
!$omp end parallel
 
!---------------------------------------------------------------------
!   compute the max-norms of newton iteration corrections
!---------------------------------------------------------------------
         if ( mod ( istep, inorm ) .eq. 0 ) then
            if (timeron) call timer_start(t_l2norm)
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,  &
     &                   ist, iend, jst, jend,  &
     &                   rsd, delunm )
            if (timeron) call timer_stop(t_l2norm)
!            if ( ipr .eq. 1 ) then
!                write (*,1006) ( delunm(m), m = 1, 5 )
!            else if ( ipr .eq. 2 ) then
!                write (*,'(i5,f15.6)') istep,delunm(5)
!            end if
         end if
 
!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
         call rhs
 
!---------------------------------------------------------------------
!   compute the max-norms of newton iteration residuals
!---------------------------------------------------------------------
         if ( ( mod ( istep, inorm ) .eq. 0 ) .or.  &
     &        ( istep .eq. itmax ) ) then
            if (timeron) call timer_start(t_l2norm)
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,  &
     &                   ist, iend, jst, jend,  &
     &                   rsd, rsdnm )
            if (timeron) call timer_stop(t_l2norm)
!            if ( ipr .eq. 1 ) then
!                write (*,1007) ( rsdnm(m), m = 1, 5 )
!            end if
         end if

!---------------------------------------------------------------------
!   check the newton-iteration residuals against the tolerance levels
!---------------------------------------------------------------------
         if ( ( rsdnm(1) .lt. tolrsd(1) ) .and.  &
     &        ( rsdnm(2) .lt. tolrsd(2) ) .and.  &
     &        ( rsdnm(3) .lt. tolrsd(3) ) .and.  &
     &        ( rsdnm(4) .lt. tolrsd(4) ) .and.  &
     &        ( rsdnm(5) .lt. tolrsd(5) ) ) then
!            if (ipr .eq. 1 ) then
               write (*,1004) istep
!            end if
            go to 900
         end if
 
      end do
  900 continue
 
      call timer_stop(1)
      maxtime= timer_read(1)
 


      return
      
 1001 format (1x/5x,'pseudo-time SSOR iteration no.=',i4/)
 1004 format (1x/1x,'convergence was achieved after ',i4,  &
     &   ' pseudo-time steps' )
 1006 format (1x/1x,'RMS-norm of SSOR-iteration correction ',  &
     & 'for first pde  = ',1pe12.5/,  &
     & 1x,'RMS-norm of SSOR-iteration correction ',  &
     & 'for second pde = ',1pe12.5/,  &
     & 1x,'RMS-norm of SSOR-iteration correction ',  &
     & 'for third pde  = ',1pe12.5/,  &
     & 1x,'RMS-norm of SSOR-iteration correction ',  &
     & 'for fourth pde = ',1pe12.5/,  &
     & 1x,'RMS-norm of SSOR-iteration correction ',  &
     & 'for fifth pde  = ',1pe12.5)
 1007 format (1x/1x,'RMS-norm of steady-state residual for ',  &
     & 'first pde  = ',1pe12.5/,  &
     & 1x,'RMS-norm of steady-state residual for ',  &
     & 'second pde = ',1pe12.5/,  &
     & 1x,'RMS-norm of steady-state residual for ',  &
     & 'third pde  = ',1pe12.5/,  &
     & 1x,'RMS-norm of steady-state residual for ',  &
     & 'fourth pde = ',1pe12.5/,  &
     & 1x,'RMS-norm of steady-state residual for ',  &
     & 'fifth pde  = ',1pe12.5)
 
      end
