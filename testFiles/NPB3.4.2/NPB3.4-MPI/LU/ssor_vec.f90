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
      use mpinpb
      use timing

      implicit none
      integer  niter


!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      integer istep
      double precision  tmp
      double precision  delunm(5), tv(5,isiz1,isiz2)

      external timer_read
      double precision wtime, timer_read

      integer IERROR

 
!---------------------------------------------------------------------
!   begin pseudo-time stepping iterations
!---------------------------------------------------------------------
      tmp = 1.0d+00 / ( omega * ( 2.0d+00 - omega ) ) 

!---------------------------------------------------------------------
!   initialize a,b,c,d to zero (guarantees that page tables have been
!   formed, if applicable on given architecture, before timestepping).
!---------------------------------------------------------------------
      do j=1,isiz2
         do i=1,isiz1
            do m=1,5
               do k=1,5
                  a(k,m,i,j) = 0.d0
                  b(k,m,i,j) = 0.d0
                  c(k,m,i,j) = 0.d0
                  d(k,m,i,j) = 0.d0
               enddo
            enddo
         enddo
      enddo

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

      call MPI_BARRIER( comm_solve, IERROR )
 
      call timer_clear(1)
      call timer_start(1)

!---------------------------------------------------------------------
!   the timestep loop
!---------------------------------------------------------------------
      do istep = 1, niter

         if (id .eq. 0) then
            if (mod ( istep, 20) .eq. 0 .or.  &
     &            istep .eq. itmax .or.  &
     &            istep .eq. 1) then
               if (niter .gt. 1) write( *, 200) istep
 200           format(' Time step ', i4)
            endif
         endif
 
!---------------------------------------------------------------------
!   perform SSOR iteration
!---------------------------------------------------------------------
         do k = 2, nz - 1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     rsd(m,i,j,k) = dt * rsd(m,i,j,k)
                  end do
               end do
            end do
         end do
 
         do k = 2, nz -1 
!---------------------------------------------------------------------
!   form the lower triangular part of the jacobian matrix
!---------------------------------------------------------------------
            call jacld(k)
 
!---------------------------------------------------------------------
!   perform the lower triangular solution
!---------------------------------------------------------------------
            call blts( isiz1, isiz2, isiz3,  &
     &                 nx, ny, nz, k,  &
     &                 omega,  &
     &                 rsd,  &
     &                 a, b, c, d,  &
     &                 ist, iend, jst, jend,  &
     &                 nx0, ny0, ipt, jpt)
         end do
 
         do k = nz - 1, 2, -1
!---------------------------------------------------------------------
!   form the strictly upper triangular part of the jacobian matrix
!---------------------------------------------------------------------
            call jacu(k)

!---------------------------------------------------------------------
!   perform the upper triangular solution
!---------------------------------------------------------------------
            call buts( isiz1, isiz2, isiz3,  &
     &                 nx, ny, nz, k,  &
     &                 omega,  &
     &                 rsd, tv,  &
     &                 d, a, b, c,  &
     &                 ist, iend, jst, jend,  &
     &                 nx0, ny0, ipt, jpt)
         end do
 
!---------------------------------------------------------------------
!   update the variables
!---------------------------------------------------------------------
 
         do k = 2, nz-1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     u( m, i, j, k ) = u( m, i, j, k )  &
     &                    + tmp * rsd( m, i, j, k )
                  end do
               end do
            end do
         end do
 
!---------------------------------------------------------------------
!   compute the max-norms of newton iteration corrections
!---------------------------------------------------------------------
         if ( mod ( istep, inorm ) .eq. 0 ) then
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,  &
     &                   ist, iend, jst, jend,  &
     &                   rsd, delunm )
!            if ( ipr .eq. 1 .and. id .eq. 0 ) then
!                write (*,1006) ( delunm(m), m = 1, 5 )
!            else if ( ipr .eq. 2 .and. id .eq. 0 ) then
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
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,  &
     &                   ist, iend, jst, jend,  &
     &                   rsd, rsdnm )
!            if ( ipr .eq. 1.and.id.eq.0 ) then
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
            if (id.eq.0) then
               write (*,1004) istep
            end if
            go to 900
         end if
 
      end do
  900 continue
 
      call timer_stop(1)
      wtime = timer_read(1)
 

      call MPI_ALLREDUCE( wtime,  &
     &                    maxtime,  &
     &                    1,  &
     &                    MPI_DOUBLE_PRECISION,  &
     &                    MPI_MAX,  &
     &                    comm_solve,  &
     &                    IERROR )
 


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
