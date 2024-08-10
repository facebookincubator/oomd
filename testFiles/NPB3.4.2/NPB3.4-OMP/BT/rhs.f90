!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine compute_rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i, j, k, m
      double precision rho_inv, uijk, up1, um1, vijk, vp1, vm1,  &
     &     wijk, wp1, wm1


      if (timeron) call timer_start(t_rhs)

!$omp parallel default(shared) private(i,j,k,m,rho_inv,uijk,up1,um1,  &
!$omp&   vijk,vp1,vm1,wijk,wp1,wm1)

!---------------------------------------------------------------------
!     compute the reciprocal of density, and the kinetic energy, 
!     and the speed of sound.
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            do i = 0, grid_points(1)-1
               rho_inv = 1.0d0/u(1,i,j,k)
               rho_i(i,j,k) = rho_inv
               us(i,j,k) = u(2,i,j,k) * rho_inv
               vs(i,j,k) = u(3,i,j,k) * rho_inv
               ws(i,j,k) = u(4,i,j,k) * rho_inv
               square(i,j,k)     = 0.5d0* (  &
     &                 u(2,i,j,k)*u(2,i,j,k) +   &
     &                 u(3,i,j,k)*u(3,i,j,k) +  &
     &                 u(4,i,j,k)*u(4,i,j,k) ) * rho_inv
               qs(i,j,k) = square(i,j,k) * rho_inv
            enddo
         enddo
      enddo
!$omp end do nowait

!---------------------------------------------------------------------
! copy the exact forcing term to the right hand side;  because 
! this forcing term is known, we can store it on the whole grid
! including the boundary                   
!---------------------------------------------------------------------

!$omp do schedule(static) collapse(2)
      do k = 0, grid_points(3)-1
         do j = 0, grid_points(2)-1
            do i = 0, grid_points(1)-1
               do m = 1, 5
                  rhs(m,i,j,k) = forcing(m,i,j,k)
               enddo
            enddo
         enddo
      enddo
!$omp end do


!$omp master
      if (timeron) call timer_start(t_rhsx)
!$omp end master
!---------------------------------------------------------------------
!     compute xi-direction fluxes 
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 1, grid_points(1)-2
               uijk = us(i,j,k)
               up1  = us(i+1,j,k)
               um1  = us(i-1,j,k)

               rhs(1,i,j,k) = rhs(1,i,j,k) + dx1tx1 *   &
     &                 (u(1,i+1,j,k) - 2.0d0*u(1,i,j,k) +   &
     &                 u(1,i-1,j,k)) -  &
     &                 tx2 * (u(2,i+1,j,k) - u(2,i-1,j,k))

               rhs(2,i,j,k) = rhs(2,i,j,k) + dx2tx1 *   &
     &                 (u(2,i+1,j,k) - 2.0d0*u(2,i,j,k) +   &
     &                 u(2,i-1,j,k)) +  &
     &                 xxcon2*con43 * (up1 - 2.0d0*uijk + um1) -  &
     &                 tx2 * (u(2,i+1,j,k)*up1 -   &
     &                 u(2,i-1,j,k)*um1 +  &
     &                 (u(5,i+1,j,k)- square(i+1,j,k)-  &
     &                 u(5,i-1,j,k)+ square(i-1,j,k))*  &
     &                 c2)

               rhs(3,i,j,k) = rhs(3,i,j,k) + dx3tx1 *   &
     &                 (u(3,i+1,j,k) - 2.0d0*u(3,i,j,k) +  &
     &                 u(3,i-1,j,k)) +  &
     &                 xxcon2 * (vs(i+1,j,k) - 2.0d0*vs(i,j,k) +  &
     &                 vs(i-1,j,k)) -  &
     &                 tx2 * (u(3,i+1,j,k)*up1 -   &
     &                 u(3,i-1,j,k)*um1)

               rhs(4,i,j,k) = rhs(4,i,j,k) + dx4tx1 *   &
     &                 (u(4,i+1,j,k) - 2.0d0*u(4,i,j,k) +  &
     &                 u(4,i-1,j,k)) +  &
     &                 xxcon2 * (ws(i+1,j,k) - 2.0d0*ws(i,j,k) +  &
     &                 ws(i-1,j,k)) -  &
     &                 tx2 * (u(4,i+1,j,k)*up1 -   &
     &                 u(4,i-1,j,k)*um1)

               rhs(5,i,j,k) = rhs(5,i,j,k) + dx5tx1 *   &
     &                 (u(5,i+1,j,k) - 2.0d0*u(5,i,j,k) +  &
     &                 u(5,i-1,j,k)) +  &
     &                 xxcon3 * (qs(i+1,j,k) - 2.0d0*qs(i,j,k) +  &
     &                 qs(i-1,j,k)) +  &
     &                 xxcon4 * (up1*up1 -       2.0d0*uijk*uijk +   &
     &                 um1*um1) +  &
     &                 xxcon5 * (u(5,i+1,j,k)*rho_i(i+1,j,k) -   &
     &                 2.0d0*u(5,i,j,k)*rho_i(i,j,k) +  &
     &                 u(5,i-1,j,k)*rho_i(i-1,j,k)) -  &
     &                 tx2 * ( (c1*u(5,i+1,j,k) -   &
     &                 c2*square(i+1,j,k))*up1 -  &
     &                 (c1*u(5,i-1,j,k) -   &
     &                 c2*square(i-1,j,k))*um1 )
            enddo

!---------------------------------------------------------------------
!     add fourth order xi-direction dissipation               
!---------------------------------------------------------------------
            i = 1
            do m = 1, 5
               rhs(m,i,j,k) = rhs(m,i,j,k)- dssp *   &
     &                    ( 5.0d0*u(m,i,j,k) - 4.0d0*u(m,i+1,j,k) +  &
     &                    u(m,i+2,j,k))
            enddo

            i = 2
            do m = 1, 5
               rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (-4.0d0*u(m,i-1,j,k) + 6.0d0*u(m,i,j,k) -  &
     &                    4.0d0*u(m,i+1,j,k) + u(m,i+2,j,k))
            enddo

            do i = 3,grid_points(1)-4
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (  u(m,i-2,j,k) - 4.0d0*u(m,i-1,j,k) +   &
     &                    6.0*u(m,i,j,k) - 4.0d0*u(m,i+1,j,k) +   &
     &                    u(m,i+2,j,k) )
               enddo
            enddo
         
            i = grid_points(1)-3
            do m = 1, 5
               rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i-2,j,k) - 4.0d0*u(m,i-1,j,k) +   &
     &                    6.0d0*u(m,i,j,k) - 4.0d0*u(m,i+1,j,k) )
            enddo

            i = grid_points(1)-2
            do m = 1, 5
               rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i-2,j,k) - 4.d0*u(m,i-1,j,k) +  &
     &                    5.d0*u(m,i,j,k) )
            enddo
         enddo
      enddo
!$omp end do nowait
!$omp master
      if (timeron) call timer_stop(t_rhsx)

      if (timeron) call timer_start(t_rhsy)
!$omp end master
!---------------------------------------------------------------------
!     compute eta-direction fluxes 
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 1, grid_points(1)-2
               vijk = vs(i,j,k)
               vp1  = vs(i,j+1,k)
               vm1  = vs(i,j-1,k)
               rhs(1,i,j,k) = rhs(1,i,j,k) + dy1ty1 *   &
     &                 (u(1,i,j+1,k) - 2.0d0*u(1,i,j,k) +   &
     &                 u(1,i,j-1,k)) -  &
     &                 ty2 * (u(3,i,j+1,k) - u(3,i,j-1,k))
               rhs(2,i,j,k) = rhs(2,i,j,k) + dy2ty1 *   &
     &                 (u(2,i,j+1,k) - 2.0d0*u(2,i,j,k) +   &
     &                 u(2,i,j-1,k)) +  &
     &                 yycon2 * (us(i,j+1,k) - 2.0d0*us(i,j,k) +   &
     &                 us(i,j-1,k)) -  &
     &                 ty2 * (u(2,i,j+1,k)*vp1 -   &
     &                 u(2,i,j-1,k)*vm1)
               rhs(3,i,j,k) = rhs(3,i,j,k) + dy3ty1 *   &
     &                 (u(3,i,j+1,k) - 2.0d0*u(3,i,j,k) +   &
     &                 u(3,i,j-1,k)) +  &
     &                 yycon2*con43 * (vp1 - 2.0d0*vijk + vm1) -  &
     &                 ty2 * (u(3,i,j+1,k)*vp1 -   &
     &                 u(3,i,j-1,k)*vm1 +  &
     &                 (u(5,i,j+1,k) - square(i,j+1,k) -   &
     &                 u(5,i,j-1,k) + square(i,j-1,k))  &
     &                 *c2)
               rhs(4,i,j,k) = rhs(4,i,j,k) + dy4ty1 *   &
     &                 (u(4,i,j+1,k) - 2.0d0*u(4,i,j,k) +   &
     &                 u(4,i,j-1,k)) +  &
     &                 yycon2 * (ws(i,j+1,k) - 2.0d0*ws(i,j,k) +   &
     &                 ws(i,j-1,k)) -  &
     &                 ty2 * (u(4,i,j+1,k)*vp1 -   &
     &                 u(4,i,j-1,k)*vm1)
               rhs(5,i,j,k) = rhs(5,i,j,k) + dy5ty1 *   &
     &                 (u(5,i,j+1,k) - 2.0d0*u(5,i,j,k) +   &
     &                 u(5,i,j-1,k)) +  &
     &                 yycon3 * (qs(i,j+1,k) - 2.0d0*qs(i,j,k) +   &
     &                 qs(i,j-1,k)) +  &
     &                 yycon4 * (vp1*vp1       - 2.0d0*vijk*vijk +   &
     &                 vm1*vm1) +  &
     &                 yycon5 * (u(5,i,j+1,k)*rho_i(i,j+1,k) -   &
     &                 2.0d0*u(5,i,j,k)*rho_i(i,j,k) +  &
     &                 u(5,i,j-1,k)*rho_i(i,j-1,k)) -  &
     &                 ty2 * ((c1*u(5,i,j+1,k) -   &
     &                 c2*square(i,j+1,k)) * vp1 -  &
     &                 (c1*u(5,i,j-1,k) -   &
     &                 c2*square(i,j-1,k)) * vm1)
            enddo

!---------------------------------------------------------------------
!     add fourth order eta-direction dissipation         
!---------------------------------------------------------------------
            if (j .eq. 1) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k)- dssp *   &
     &                    ( 5.0d0*u(m,i,j,k) - 4.0d0*u(m,i,j+1,k) +  &
     &                    u(m,i,j+2,k))
               enddo
               enddo

            else if (j .eq. 2) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (-4.0d0*u(m,i,j-1,k) + 6.0d0*u(m,i,j,k) -  &
     &                    4.0d0*u(m,i,j+1,k) + u(m,i,j+2,k))
               enddo
               enddo
         
            else if (j .eq. grid_points(2)-3) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i,j-2,k) - 4.0d0*u(m,i,j-1,k) +   &
     &                    6.0d0*u(m,i,j,k) - 4.0d0*u(m,i,j+1,k) )
               enddo
               enddo

            else if (j .eq. grid_points(2)-2) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i,j-2,k) - 4.d0*u(m,i,j-1,k) +  &
     &                    5.d0*u(m,i,j,k) )
               enddo
               enddo

            else
               do i = 1,grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (  u(m,i,j-2,k) - 4.0d0*u(m,i,j-1,k) +   &
     &                    6.0*u(m,i,j,k) - 4.0d0*u(m,i,j+1,k) +   &
     &                    u(m,i,j+2,k) )
               enddo
               enddo
            endif
         enddo
      enddo
!$omp end do nowait
!$omp master
      if (timeron) call timer_stop(t_rhsy)

      if (timeron) call timer_start(t_rhsz)
!$omp end master
!---------------------------------------------------------------------
!     compute zeta-direction fluxes 
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 1, grid_points(1)-2
               wijk = ws(i,j,k)
               wp1  = ws(i,j,k+1)
               wm1  = ws(i,j,k-1)

               rhs(1,i,j,k) = rhs(1,i,j,k) + dz1tz1 *   &
     &                 (u(1,i,j,k+1) - 2.0d0*u(1,i,j,k) +   &
     &                 u(1,i,j,k-1)) -  &
     &                 tz2 * (u(4,i,j,k+1) - u(4,i,j,k-1))
               rhs(2,i,j,k) = rhs(2,i,j,k) + dz2tz1 *   &
     &                 (u(2,i,j,k+1) - 2.0d0*u(2,i,j,k) +   &
     &                 u(2,i,j,k-1)) +  &
     &                 zzcon2 * (us(i,j,k+1) - 2.0d0*us(i,j,k) +   &
     &                 us(i,j,k-1)) -  &
     &                 tz2 * (u(2,i,j,k+1)*wp1 -   &
     &                 u(2,i,j,k-1)*wm1)
               rhs(3,i,j,k) = rhs(3,i,j,k) + dz3tz1 *   &
     &                 (u(3,i,j,k+1) - 2.0d0*u(3,i,j,k) +   &
     &                 u(3,i,j,k-1)) +  &
     &                 zzcon2 * (vs(i,j,k+1) - 2.0d0*vs(i,j,k) +   &
     &                 vs(i,j,k-1)) -  &
     &                 tz2 * (u(3,i,j,k+1)*wp1 -   &
     &                 u(3,i,j,k-1)*wm1)
               rhs(4,i,j,k) = rhs(4,i,j,k) + dz4tz1 *   &
     &                 (u(4,i,j,k+1) - 2.0d0*u(4,i,j,k) +   &
     &                 u(4,i,j,k-1)) +  &
     &                 zzcon2*con43 * (wp1 - 2.0d0*wijk + wm1) -  &
     &                 tz2 * (u(4,i,j,k+1)*wp1 -   &
     &                 u(4,i,j,k-1)*wm1 +  &
     &                 (u(5,i,j,k+1) - square(i,j,k+1) -   &
     &                 u(5,i,j,k-1) + square(i,j,k-1))  &
     &                 *c2)
               rhs(5,i,j,k) = rhs(5,i,j,k) + dz5tz1 *   &
     &                 (u(5,i,j,k+1) - 2.0d0*u(5,i,j,k) +   &
     &                 u(5,i,j,k-1)) +  &
     &                 zzcon3 * (qs(i,j,k+1) - 2.0d0*qs(i,j,k) +   &
     &                 qs(i,j,k-1)) +  &
     &                 zzcon4 * (wp1*wp1 - 2.0d0*wijk*wijk +   &
     &                 wm1*wm1) +  &
     &                 zzcon5 * (u(5,i,j,k+1)*rho_i(i,j,k+1) -   &
     &                 2.0d0*u(5,i,j,k)*rho_i(i,j,k) +  &
     &                 u(5,i,j,k-1)*rho_i(i,j,k-1)) -  &
     &                 tz2 * ( (c1*u(5,i,j,k+1) -   &
     &                 c2*square(i,j,k+1))*wp1 -  &
     &                 (c1*u(5,i,j,k-1) -   &
     &                 c2*square(i,j,k-1))*wm1)
            enddo

!---------------------------------------------------------------------
!     add fourth order zeta-direction dissipation                
!---------------------------------------------------------------------
            if (k.eq.1) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k)- dssp *   &
     &                    ( 5.0d0*u(m,i,j,k) - 4.0d0*u(m,i,j,k+1) +  &
     &                    u(m,i,j,k+2))
               enddo
               enddo

            else if (k.eq.2) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (-4.0d0*u(m,i,j,k-1) + 6.0d0*u(m,i,j,k) -  &
     &                    4.0d0*u(m,i,j,k+1) + u(m,i,j,k+2))
               enddo
               enddo

            else if (k.eq.grid_points(3)-3) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i,j,k-2) - 4.0d0*u(m,i,j,k-1) +   &
     &                    6.0d0*u(m,i,j,k) - 4.0d0*u(m,i,j,k+1) )
               enddo
               enddo

            else if (k.eq.grid_points(3)-2) then
               do i = 1, grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *  &
     &                    ( u(m,i,j,k-2) - 4.d0*u(m,i,j,k-1) +  &
     &                    5.d0*u(m,i,j,k) )
               enddo
               enddo

            else
               do i = 1,grid_points(1)-2
               do m = 1, 5
                  rhs(m,i,j,k) = rhs(m,i,j,k) - dssp *   &
     &                    (  u(m,i,j,k-2) - 4.0d0*u(m,i,j,k-1) +   &
     &                    6.0*u(m,i,j,k) - 4.0d0*u(m,i,j,k+1) +   &
     &                    u(m,i,j,k+2) )
               enddo
               enddo
            endif
         enddo
      enddo
!$omp end do nowait
!$omp master
      if (timeron) call timer_stop(t_rhsz)
!$omp end master

!$omp do schedule(static) collapse(2)
      do k = 1, grid_points(3)-2
         do j = 1, grid_points(2)-2
            do i = 1, grid_points(1)-2
               rhs(1,i,j,k) = rhs(1,i,j,k) * dt
               rhs(2,i,j,k) = rhs(2,i,j,k) * dt
               rhs(3,i,j,k) = rhs(3,i,j,k) * dt
               rhs(4,i,j,k) = rhs(4,i,j,k) * dt
               rhs(5,i,j,k) = rhs(5,i,j,k) * dt
            enddo
         enddo
      enddo
!$omp end do nowait

!$omp end parallel

      if (timeron) call timer_stop(t_rhs)

      return
      end




