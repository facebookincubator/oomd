!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine compute_rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer c, i, j, k, m
      double precision rho_inv, uijk, up1, um1, vijk, vp1, vm1,  &
     &     wijk, wp1, wm1


      if (timeron) call timer_start(t_rhs)
!---------------------------------------------------------------------
!     loop over all cells owned by this node                           
!---------------------------------------------------------------------
      do c = 1, ncells

!---------------------------------------------------------------------
!     compute the reciprocal of density, and the kinetic energy, 
!     and the speed of sound.
!---------------------------------------------------------------------
         do k = -1, cell_size(3,c)
            do j = -1, cell_size(2,c)
               do i = -1, cell_size(1,c)
                  rho_inv = 1.0d0/u(1,i,j,k,c)
                  rho_i(i,j,k,c) = rho_inv
                  us(i,j,k,c) = u(2,i,j,k,c) * rho_inv
                  vs(i,j,k,c) = u(3,i,j,k,c) * rho_inv
                  ws(i,j,k,c) = u(4,i,j,k,c) * rho_inv
                  square(i,j,k,c)     = 0.5d0* (  &
     &                 u(2,i,j,k,c)*u(2,i,j,k,c) +  &
     &                 u(3,i,j,k,c)*u(3,i,j,k,c) +  &
     &                 u(4,i,j,k,c)*u(4,i,j,k,c) ) * rho_inv
                  qs(i,j,k,c) = square(i,j,k,c) * rho_inv
               enddo
            enddo
         enddo

!---------------------------------------------------------------------
! copy the exact forcing term to the right hand side;  because 
! this forcing term is known, we can store it on the whole of every 
! cell,  including the boundary                   
!---------------------------------------------------------------------

         do k = 0, cell_size(3,c)-1
            do j = 0, cell_size(2,c)-1
               do i = 0, cell_size(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = forcing(m,i,j,k,c)
                  enddo
               enddo
            enddo
         enddo


!---------------------------------------------------------------------
!     compute xi-direction fluxes 
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  uijk = us(i,j,k,c)
                  up1  = us(i+1,j,k,c)
                  um1  = us(i-1,j,k,c)

                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dx1tx1 *  &
     &                 (u(1,i+1,j,k,c) - 2.0d0*u(1,i,j,k,c) +  &
     &                 u(1,i-1,j,k,c)) -  &
     &                 tx2 * (u(2,i+1,j,k,c) - u(2,i-1,j,k,c))

                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dx2tx1 *  &
     &                 (u(2,i+1,j,k,c) - 2.0d0*u(2,i,j,k,c) +  &
     &                 u(2,i-1,j,k,c)) +  &
     &                 xxcon2*con43 * (up1 - 2.0d0*uijk + um1) -  &
     &                 tx2 * (u(2,i+1,j,k,c)*up1 -  &
     &                 u(2,i-1,j,k,c)*um1 +  &
     &                 (u(5,i+1,j,k,c)- square(i+1,j,k,c)-  &
     &                 u(5,i-1,j,k,c)+ square(i-1,j,k,c))*  &
     &                 c2)

                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dx3tx1 *  &
     &                 (u(3,i+1,j,k,c) - 2.0d0*u(3,i,j,k,c) +  &
     &                 u(3,i-1,j,k,c)) +  &
     &                 xxcon2 * (vs(i+1,j,k,c) - 2.0d0*vs(i,j,k,c) +  &
     &                 vs(i-1,j,k,c)) -  &
     &                 tx2 * (u(3,i+1,j,k,c)*up1 -  &
     &                 u(3,i-1,j,k,c)*um1)

                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dx4tx1 *  &
     &                 (u(4,i+1,j,k,c) - 2.0d0*u(4,i,j,k,c) +  &
     &                 u(4,i-1,j,k,c)) +  &
     &                 xxcon2 * (ws(i+1,j,k,c) - 2.0d0*ws(i,j,k,c) +  &
     &                 ws(i-1,j,k,c)) -  &
     &                 tx2 * (u(4,i+1,j,k,c)*up1 -  &
     &                 u(4,i-1,j,k,c)*um1)

                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dx5tx1 *  &
     &                 (u(5,i+1,j,k,c) - 2.0d0*u(5,i,j,k,c) +  &
     &                 u(5,i-1,j,k,c)) +  &
     &                 xxcon3 * (qs(i+1,j,k,c) - 2.0d0*qs(i,j,k,c) +  &
     &                 qs(i-1,j,k,c)) +  &
     &                 xxcon4 * (up1*up1 -       2.0d0*uijk*uijk +  &
     &                 um1*um1) +  &
     &                 xxcon5 * (u(5,i+1,j,k,c)*rho_i(i+1,j,k,c) -  &
     &                 2.0d0*u(5,i,j,k,c)*rho_i(i,j,k,c) +  &
     &                 u(5,i-1,j,k,c)*rho_i(i-1,j,k,c)) -  &
     &                 tx2 * ( (c1*u(5,i+1,j,k,c) -  &
     &                 c2*square(i+1,j,k,c))*up1 -  &
     &                 (c1*u(5,i-1,j,k,c) -  &
     &                 c2*square(i-1,j,k,c))*um1 )
               enddo
            enddo
         enddo

!---------------------------------------------------------------------
!     add fourth order xi-direction dissipation               
!---------------------------------------------------------------------
         if (start(1,c) .gt. 0) then
            do k = start(3,c), cell_size(3,c)-end(3,c)-1
               do j = start(2,c), cell_size(2,c)-end(2,c)-1
                  i = 1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp *  &
     &                    ( 5.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i+1,j,k,c) +  &
     &                    u(m,i+2,j,k,c))
                  enddo

                  i = 2
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*u(m,i-1,j,k,c) + 6.0d0*u(m,i,j,k,c) -  &
     &                    4.0d0*u(m,i+1,j,k,c) + u(m,i+2,j,k,c))
                  enddo
               enddo
            enddo
         endif

         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = 3*start(1,c),cell_size(1,c)-3*end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (  u(m,i-2,j,k,c) - 4.0d0*u(m,i-1,j,k,c) +  &
     &                    6.0*u(m,i,j,k,c) - 4.0d0*u(m,i+1,j,k,c) +  &
     &                    u(m,i+2,j,k,c) )
                  enddo
               enddo
            enddo
         enddo
         

         if (end(1,c) .gt. 0) then
            do k = start(3,c), cell_size(3,c)-end(3,c)-1
               do j = start(2,c), cell_size(2,c)-end(2,c)-1
                  i = cell_size(1,c)-3
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i-2,j,k,c) - 4.0d0*u(m,i-1,j,k,c) +  &
     &                    6.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i+1,j,k,c) )
                  enddo

                  i = cell_size(1,c)-2
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i-2,j,k,c) - 4.d0*u(m,i-1,j,k,c) +  &
     &                    5.d0*u(m,i,j,k,c) )
                  enddo
               enddo
            enddo
         endif

!---------------------------------------------------------------------
!     compute eta-direction fluxes 
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  vijk = vs(i,j,k,c)
                  vp1  = vs(i,j+1,k,c)
                  vm1  = vs(i,j-1,k,c)
                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dy1ty1 *  &
     &                 (u(1,i,j+1,k,c) - 2.0d0*u(1,i,j,k,c) +  &
     &                 u(1,i,j-1,k,c)) -  &
     &                 ty2 * (u(3,i,j+1,k,c) - u(3,i,j-1,k,c))
                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dy2ty1 *  &
     &                 (u(2,i,j+1,k,c) - 2.0d0*u(2,i,j,k,c) +  &
     &                 u(2,i,j-1,k,c)) +  &
     &                 yycon2 * (us(i,j+1,k,c) - 2.0d0*us(i,j,k,c) +  &
     &                 us(i,j-1,k,c)) -  &
     &                 ty2 * (u(2,i,j+1,k,c)*vp1 -  &
     &                 u(2,i,j-1,k,c)*vm1)
                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dy3ty1 *  &
     &                 (u(3,i,j+1,k,c) - 2.0d0*u(3,i,j,k,c) +  &
     &                 u(3,i,j-1,k,c)) +  &
     &                 yycon2*con43 * (vp1 - 2.0d0*vijk + vm1) -  &
     &                 ty2 * (u(3,i,j+1,k,c)*vp1 -  &
     &                 u(3,i,j-1,k,c)*vm1 +  &
     &                 (u(5,i,j+1,k,c) - square(i,j+1,k,c) -  &
     &                 u(5,i,j-1,k,c) + square(i,j-1,k,c))  &
     &                 *c2)
                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dy4ty1 *  &
     &                 (u(4,i,j+1,k,c) - 2.0d0*u(4,i,j,k,c) +  &
     &                 u(4,i,j-1,k,c)) +  &
     &                 yycon2 * (ws(i,j+1,k,c) - 2.0d0*ws(i,j,k,c) +  &
     &                 ws(i,j-1,k,c)) -  &
     &                 ty2 * (u(4,i,j+1,k,c)*vp1 -  &
     &                 u(4,i,j-1,k,c)*vm1)
                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dy5ty1 *  &
     &                 (u(5,i,j+1,k,c) - 2.0d0*u(5,i,j,k,c) +  &
     &                 u(5,i,j-1,k,c)) +  &
     &                 yycon3 * (qs(i,j+1,k,c) - 2.0d0*qs(i,j,k,c) +  &
     &                 qs(i,j-1,k,c)) +  &
     &                 yycon4 * (vp1*vp1       - 2.0d0*vijk*vijk +  &
     &                 vm1*vm1) +  &
     &                 yycon5 * (u(5,i,j+1,k,c)*rho_i(i,j+1,k,c) -  &
     &                 2.0d0*u(5,i,j,k,c)*rho_i(i,j,k,c) +  &
     &                 u(5,i,j-1,k,c)*rho_i(i,j-1,k,c)) -  &
     &                 ty2 * ((c1*u(5,i,j+1,k,c) -  &
     &                 c2*square(i,j+1,k,c)) * vp1 -  &
     &                 (c1*u(5,i,j-1,k,c) -  &
     &                 c2*square(i,j-1,k,c)) * vm1)
               enddo
            enddo
         enddo

!---------------------------------------------------------------------
!     add fourth order eta-direction dissipation         
!---------------------------------------------------------------------
         if (start(2,c) .gt. 0) then
            do k = start(3,c), cell_size(3,c)-end(3,c)-1
               j = 1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp *  &
     &                    ( 5.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i,j+1,k,c) +  &
     &                    u(m,i,j+2,k,c))
                  enddo
               enddo

               j = 2
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*u(m,i,j-1,k,c) + 6.0d0*u(m,i,j,k,c) -  &
     &                    4.0d0*u(m,i,j+1,k,c) + u(m,i,j+2,k,c))
                  enddo
               enddo
            enddo
         endif

         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = 3*start(2,c), cell_size(2,c)-3*end(2,c)-1
               do i = start(1,c),cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (  u(m,i,j-2,k,c) - 4.0d0*u(m,i,j-1,k,c) +  &
     &                    6.0*u(m,i,j,k,c) - 4.0d0*u(m,i,j+1,k,c) +  &
     &                    u(m,i,j+2,k,c) )
                  enddo
               enddo
            enddo
         enddo
         
         if (end(2,c) .gt. 0) then
            do k = start(3,c), cell_size(3,c)-end(3,c)-1
               j = cell_size(2,c)-3
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i,j-2,k,c) - 4.0d0*u(m,i,j-1,k,c) +  &
     &                    6.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i,j+1,k,c) )
                  enddo
               enddo

               j = cell_size(2,c)-2
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i,j-2,k,c) - 4.d0*u(m,i,j-1,k,c) +  &
     &                    5.d0*u(m,i,j,k,c) )
                  enddo
               enddo
            enddo
         endif

!---------------------------------------------------------------------
!     compute zeta-direction fluxes 
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  wijk = ws(i,j,k,c)
                  wp1  = ws(i,j,k+1,c)
                  wm1  = ws(i,j,k-1,c)

                  rhs(1,i,j,k,c) = rhs(1,i,j,k,c) + dz1tz1 *  &
     &                 (u(1,i,j,k+1,c) - 2.0d0*u(1,i,j,k,c) +  &
     &                 u(1,i,j,k-1,c)) -  &
     &                 tz2 * (u(4,i,j,k+1,c) - u(4,i,j,k-1,c))
                  rhs(2,i,j,k,c) = rhs(2,i,j,k,c) + dz2tz1 *  &
     &                 (u(2,i,j,k+1,c) - 2.0d0*u(2,i,j,k,c) +  &
     &                 u(2,i,j,k-1,c)) +  &
     &                 zzcon2 * (us(i,j,k+1,c) - 2.0d0*us(i,j,k,c) +  &
     &                 us(i,j,k-1,c)) -  &
     &                 tz2 * (u(2,i,j,k+1,c)*wp1 -  &
     &                 u(2,i,j,k-1,c)*wm1)
                  rhs(3,i,j,k,c) = rhs(3,i,j,k,c) + dz3tz1 *  &
     &                 (u(3,i,j,k+1,c) - 2.0d0*u(3,i,j,k,c) +  &
     &                 u(3,i,j,k-1,c)) +  &
     &                 zzcon2 * (vs(i,j,k+1,c) - 2.0d0*vs(i,j,k,c) +  &
     &                 vs(i,j,k-1,c)) -  &
     &                 tz2 * (u(3,i,j,k+1,c)*wp1 -  &
     &                 u(3,i,j,k-1,c)*wm1)
                  rhs(4,i,j,k,c) = rhs(4,i,j,k,c) + dz4tz1 *  &
     &                 (u(4,i,j,k+1,c) - 2.0d0*u(4,i,j,k,c) +  &
     &                 u(4,i,j,k-1,c)) +  &
     &                 zzcon2*con43 * (wp1 - 2.0d0*wijk + wm1) -  &
     &                 tz2 * (u(4,i,j,k+1,c)*wp1 -  &
     &                 u(4,i,j,k-1,c)*wm1 +  &
     &                 (u(5,i,j,k+1,c) - square(i,j,k+1,c) -  &
     &                 u(5,i,j,k-1,c) + square(i,j,k-1,c))  &
     &                 *c2)
                  rhs(5,i,j,k,c) = rhs(5,i,j,k,c) + dz5tz1 *  &
     &                 (u(5,i,j,k+1,c) - 2.0d0*u(5,i,j,k,c) +  &
     &                 u(5,i,j,k-1,c)) +  &
     &                 zzcon3 * (qs(i,j,k+1,c) - 2.0d0*qs(i,j,k,c) +  &
     &                 qs(i,j,k-1,c)) +  &
     &                 zzcon4 * (wp1*wp1 - 2.0d0*wijk*wijk +  &
     &                 wm1*wm1) +  &
     &                 zzcon5 * (u(5,i,j,k+1,c)*rho_i(i,j,k+1,c) -  &
     &                 2.0d0*u(5,i,j,k,c)*rho_i(i,j,k,c) +  &
     &                 u(5,i,j,k-1,c)*rho_i(i,j,k-1,c)) -  &
     &                 tz2 * ( (c1*u(5,i,j,k+1,c) -  &
     &                 c2*square(i,j,k+1,c))*wp1 -  &
     &                 (c1*u(5,i,j,k-1,c) -  &
     &                 c2*square(i,j,k-1,c))*wm1)
               enddo
            enddo
         enddo

!---------------------------------------------------------------------
!     add fourth order zeta-direction dissipation                
!---------------------------------------------------------------------
         if (start(3,c) .gt. 0) then
            k = 1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)- dssp *  &
     &                    ( 5.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i,j,k+1,c) +  &
     &                    u(m,i,j,k+2,c))
                  enddo
               enddo
            enddo

            k = 2
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*u(m,i,j,k-1,c) + 6.0d0*u(m,i,j,k,c) -  &
     &                    4.0d0*u(m,i,j,k+1,c) + u(m,i,j,k+2,c))
                  enddo
               enddo
            enddo
         endif

         do k = 3*start(3,c), cell_size(3,c)-3*end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c),cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    (  u(m,i,j,k-2,c) - 4.0d0*u(m,i,j,k-1,c) +  &
     &                    6.0*u(m,i,j,k,c) - 4.0d0*u(m,i,j,k+1,c) +  &
     &                    u(m,i,j,k+2,c) )
                  enddo
               enddo
            enddo
         enddo
         
         if (end(3,c) .gt. 0) then
            k = cell_size(3,c)-3
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i,j,k-2,c) - 4.0d0*u(m,i,j,k-1,c) +  &
     &                    6.0d0*u(m,i,j,k,c) - 4.0d0*u(m,i,j,k+1,c) )
                  enddo
               enddo
            enddo

            k = cell_size(3,c)-2
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) - dssp *  &
     &                    ( u(m,i,j,k-2,c) - 4.d0*u(m,i,j,k-1,c) +  &
     &                    5.d0*u(m,i,j,k,c) )
                  enddo
               enddo
            enddo
         endif

         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c) * dt
                  enddo
               enddo
            enddo
         enddo

      enddo
     
      if (timeron) call timer_stop(t_rhs)
     
      return
      end




