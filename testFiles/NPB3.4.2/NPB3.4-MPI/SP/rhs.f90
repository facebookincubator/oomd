!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine compute_rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer c, i, j, k, m
       double precision aux, rho_inv, uijk, up1, um1, vijk, vp1, vm1,  &
     &                  wijk, wp1, wm1


       if (timeron) call timer_start(t_rhs)
!---------------------------------------------------------------------
! loop over all cells owned by this node                           
!---------------------------------------------------------------------
       do    c = 1, ncells

!---------------------------------------------------------------------
!         compute the reciprocal of density, and the kinetic energy, 
!         and the speed of sound. 
!---------------------------------------------------------------------

          do    k = -1, cell_size(3,c)
             do    j = -1, cell_size(2,c)
                do    i = -1, cell_size(1,c)
                   rho_inv = 1.0d0/u(i,j,k,1,c)
                   rho_i(i,j,k,c) = rho_inv
                   us(i,j,k,c) = u(i,j,k,2,c) * rho_inv
                   vs(i,j,k,c) = u(i,j,k,3,c) * rho_inv
                   ws(i,j,k,c) = u(i,j,k,4,c) * rho_inv
                   square(i,j,k,c)     = 0.5d0* (  &
     &                        u(i,j,k,2,c)*u(i,j,k,2,c) +  &
     &                        u(i,j,k,3,c)*u(i,j,k,3,c) +  &
     &                        u(i,j,k,4,c)*u(i,j,k,4,c) ) * rho_inv
                   qs(i,j,k,c) = square(i,j,k,c) * rho_inv
!---------------------------------------------------------------------
!                  (don't need speed and ainx until the lhs computation)
!---------------------------------------------------------------------
                   aux = c1c2*rho_inv* (u(i,j,k,5,c) - square(i,j,k,c))
                   aux = dsqrt(aux)
                   speed(i,j,k,c) = aux
                   ainv(i,j,k,c)  = 1.0d0/aux
                end do
             end do
          end do

!---------------------------------------------------------------------
! copy the exact forcing term to the right hand side;  because 
! this forcing term is known, we can store it on the whole of every 
! cell,  including the boundary                   
!---------------------------------------------------------------------

          do   m = 1, 5
             do   k = 0, cell_size(3,c)-1
                do   j = 0, cell_size(2,c)-1
                   do   i = 0, cell_size(1,c)-1
                      rhs(i,j,k,m,c) = forcing(i,j,k,m,c)
                   end do
                end do
             end do
          end do


!---------------------------------------------------------------------
!         compute xi-direction fluxes 
!---------------------------------------------------------------------
          do    k = start(3,c), cell_size(3,c)-end(3,c)-1
             do    j = start(2,c), cell_size(2,c)-end(2,c)-1
                do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                   uijk = us(i,j,k,c)
                   up1  = us(i+1,j,k,c)
                   um1  = us(i-1,j,k,c)

                   rhs(i,j,k,1,c) = rhs(i,j,k,1,c) + dx1tx1 *  &
     &                    (u(i+1,j,k,1,c) - 2.0d0*u(i,j,k,1,c) +  &
     &                     u(i-1,j,k,1,c)) -  &
     &                    tx2 * (u(i+1,j,k,2,c) - u(i-1,j,k,2,c))

                   rhs(i,j,k,2,c) = rhs(i,j,k,2,c) + dx2tx1 *  &
     &                    (u(i+1,j,k,2,c) - 2.0d0*u(i,j,k,2,c) +  &
     &                     u(i-1,j,k,2,c)) +  &
     &                    xxcon2*con43 * (up1 - 2.0d0*uijk + um1) -  &
     &                    tx2 * (u(i+1,j,k,2,c)*up1 -  &
     &                           u(i-1,j,k,2,c)*um1 +  &
     &                           (u(i+1,j,k,5,c)- square(i+1,j,k,c)-  &
     &                            u(i-1,j,k,5,c)+ square(i-1,j,k,c))*  &
     &                            c2)

                   rhs(i,j,k,3,c) = rhs(i,j,k,3,c) + dx3tx1 *  &
     &                    (u(i+1,j,k,3,c) - 2.0d0*u(i,j,k,3,c) +  &
     &                     u(i-1,j,k,3,c)) +  &
     &                    xxcon2 * (vs(i+1,j,k,c) - 2.0d0*vs(i,j,k,c) +  &
     &                              vs(i-1,j,k,c)) -  &
     &                    tx2 * (u(i+1,j,k,3,c)*up1 -  &
     &                           u(i-1,j,k,3,c)*um1)

                   rhs(i,j,k,4,c) = rhs(i,j,k,4,c) + dx4tx1 *  &
     &                    (u(i+1,j,k,4,c) - 2.0d0*u(i,j,k,4,c) +  &
     &                     u(i-1,j,k,4,c)) +  &
     &                    xxcon2 * (ws(i+1,j,k,c) - 2.0d0*ws(i,j,k,c) +  &
     &                              ws(i-1,j,k,c)) -  &
     &                    tx2 * (u(i+1,j,k,4,c)*up1 -  &
     &                           u(i-1,j,k,4,c)*um1)

                   rhs(i,j,k,5,c) = rhs(i,j,k,5,c) + dx5tx1 *  &
     &                    (u(i+1,j,k,5,c) - 2.0d0*u(i,j,k,5,c) +  &
     &                     u(i-1,j,k,5,c)) +  &
     &                    xxcon3 * (qs(i+1,j,k,c) - 2.0d0*qs(i,j,k,c) +  &
     &                              qs(i-1,j,k,c)) +  &
     &                    xxcon4 * (up1*up1 -       2.0d0*uijk*uijk +  &
     &                              um1*um1) +  &
     &                    xxcon5 * (u(i+1,j,k,5,c)*rho_i(i+1,j,k,c) -  &
     &                              2.0d0*u(i,j,k,5,c)*rho_i(i,j,k,c) +  &
     &                              u(i-1,j,k,5,c)*rho_i(i-1,j,k,c)) -  &
     &                    tx2 * ( (c1*u(i+1,j,k,5,c) -  &
     &                             c2*square(i+1,j,k,c))*up1 -  &
     &                            (c1*u(i-1,j,k,5,c) -  &
     &                             c2*square(i-1,j,k,c))*um1 )
                end do
             end do
          end do

!---------------------------------------------------------------------
!         add fourth order xi-direction dissipation               
!---------------------------------------------------------------------
          if (start(1,c) .gt. 0) then
             i = 1
             do    m = 1, 5
                do    k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do    j = start(2,c), cell_size(2,c)-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c)- dssp *  &
     &                    ( 5.0d0*u(i,j,k,m,c) - 4.0d0*u(i+1,j,k,m,c) +  &
     &                            u(i+2,j,k,m,c))
                   end do
                end do
             end do

             i = 2
             do    m = 1, 5
                do    k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do    j = start(2,c), cell_size(2,c)-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (-4.0d0*u(i-1,j,k,m,c) + 6.0d0*u(i,j,k,m,c) -  &
     &                      4.0d0*u(i+1,j,k,m,c) + u(i+2,j,k,m,c))
                   end do
                end do
             end do
          endif

          do     m = 1, 5
             do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do  i = 3*start(1,c),cell_size(1,c)-3*end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (  u(i-2,j,k,m,c) - 4.0d0*u(i-1,j,k,m,c) +  &
     &                     6.0*u(i,j,k,m,c) - 4.0d0*u(i+1,j,k,m,c) +  &
     &                         u(i+2,j,k,m,c) )
                   end do
                end do
             end do
          end do
 

          if (end(1,c) .gt. 0) then
             i = cell_size(1,c)-3
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i-2,j,k,m,c) - 4.0d0*u(i-1,j,k,m,c) +  &
     &                      6.0d0*u(i,j,k,m,c) - 4.0d0*u(i+1,j,k,m,c) )
                   end do
                end do
             end do

             i = cell_size(1,c)-2
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i-2,j,k,m,c) - 4.d0*u(i-1,j,k,m,c) +  &
     &                      5.d0*u(i,j,k,m,c) )
                   end do
                end do
             end do
          endif

!---------------------------------------------------------------------
!         compute eta-direction fluxes 
!---------------------------------------------------------------------
          do     k = start(3,c), cell_size(3,c)-end(3,c)-1
             do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                   vijk = vs(i,j,k,c)
                   vp1  = vs(i,j+1,k,c)
                   vm1  = vs(i,j-1,k,c)
                   rhs(i,j,k,1,c) = rhs(i,j,k,1,c) + dy1ty1 *  &
     &                   (u(i,j+1,k,1,c) - 2.0d0*u(i,j,k,1,c) +  &
     &                    u(i,j-1,k,1,c)) -  &
     &                   ty2 * (u(i,j+1,k,3,c) - u(i,j-1,k,3,c))
                   rhs(i,j,k,2,c) = rhs(i,j,k,2,c) + dy2ty1 *  &
     &                   (u(i,j+1,k,2,c) - 2.0d0*u(i,j,k,2,c) +  &
     &                    u(i,j-1,k,2,c)) +  &
     &                   yycon2 * (us(i,j+1,k,c) - 2.0d0*us(i,j,k,c) +  &
     &                             us(i,j-1,k,c)) -  &
     &                   ty2 * (u(i,j+1,k,2,c)*vp1 -  &
     &                          u(i,j-1,k,2,c)*vm1)
                   rhs(i,j,k,3,c) = rhs(i,j,k,3,c) + dy3ty1 *  &
     &                   (u(i,j+1,k,3,c) - 2.0d0*u(i,j,k,3,c) +  &
     &                    u(i,j-1,k,3,c)) +  &
     &                   yycon2*con43 * (vp1 - 2.0d0*vijk + vm1) -  &
     &                   ty2 * (u(i,j+1,k,3,c)*vp1 -  &
     &                          u(i,j-1,k,3,c)*vm1 +  &
     &                          (u(i,j+1,k,5,c) - square(i,j+1,k,c) -  &
     &                           u(i,j-1,k,5,c) + square(i,j-1,k,c))  &
     &                          *c2)
                   rhs(i,j,k,4,c) = rhs(i,j,k,4,c) + dy4ty1 *  &
     &                   (u(i,j+1,k,4,c) - 2.0d0*u(i,j,k,4,c) +  &
     &                    u(i,j-1,k,4,c)) +  &
     &                   yycon2 * (ws(i,j+1,k,c) - 2.0d0*ws(i,j,k,c) +  &
     &                             ws(i,j-1,k,c)) -  &
     &                   ty2 * (u(i,j+1,k,4,c)*vp1 -  &
     &                          u(i,j-1,k,4,c)*vm1)
                   rhs(i,j,k,5,c) = rhs(i,j,k,5,c) + dy5ty1 *  &
     &                   (u(i,j+1,k,5,c) - 2.0d0*u(i,j,k,5,c) +  &
     &                    u(i,j-1,k,5,c)) +  &
     &                   yycon3 * (qs(i,j+1,k,c) - 2.0d0*qs(i,j,k,c) +  &
     &                             qs(i,j-1,k,c)) +  &
     &                   yycon4 * (vp1*vp1       - 2.0d0*vijk*vijk +  &
     &                             vm1*vm1) +  &
     &                   yycon5 * (u(i,j+1,k,5,c)*rho_i(i,j+1,k,c) -  &
     &                             2.0d0*u(i,j,k,5,c)*rho_i(i,j,k,c) +  &
     &                             u(i,j-1,k,5,c)*rho_i(i,j-1,k,c)) -  &
     &                   ty2 * ((c1*u(i,j+1,k,5,c) -  &
     &                           c2*square(i,j+1,k,c)) * vp1 -  &
     &                          (c1*u(i,j-1,k,5,c) -  &
     &                           c2*square(i,j-1,k,c)) * vm1)
                end do
             end do
          end do

!---------------------------------------------------------------------
!         add fourth order eta-direction dissipation         
!---------------------------------------------------------------------
          if (start(2,c) .gt. 0) then
             j = 1
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c)- dssp *  &
     &                    ( 5.0d0*u(i,j,k,m,c) - 4.0d0*u(i,j+1,k,m,c) +  &
     &                            u(i,j+2,k,m,c))
                   end do
                end do
             end do

             j = 2
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (-4.0d0*u(i,j-1,k,m,c) + 6.0d0*u(i,j,k,m,c) -  &
     &                      4.0d0*u(i,j+1,k,m,c) + u(i,j+2,k,m,c))
                   end do
                end do
             end do
          endif

          do     m = 1, 5
             do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                do    j = 3*start(2,c), cell_size(2,c)-3*end(2,c)-1
                   do  i = start(1,c),cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (  u(i,j-2,k,m,c) - 4.0d0*u(i,j-1,k,m,c) +  &
     &                     6.0*u(i,j,k,m,c) - 4.0d0*u(i,j+1,k,m,c) +  &
     &                         u(i,j+2,k,m,c) )
                   end do
                end do
             end do
          end do
 
          if (end(2,c) .gt. 0) then
             j = cell_size(2,c)-3
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i,j-2,k,m,c) - 4.0d0*u(i,j-1,k,m,c) +  &
     &                      6.0d0*u(i,j,k,m,c) - 4.0d0*u(i,j+1,k,m,c) )
                   end do
                end do
             end do

             j = cell_size(2,c)-2
             do     m = 1, 5
                do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i,j-2,k,m,c) - 4.d0*u(i,j-1,k,m,c) +  &
     &                      5.d0*u(i,j,k,m,c) )
                   end do
                end do
             end do
          endif


!---------------------------------------------------------------------
!         compute zeta-direction fluxes 
!---------------------------------------------------------------------
          do    k = start(3,c), cell_size(3,c)-end(3,c)-1
             do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                   wijk = ws(i,j,k,c)
                   wp1  = ws(i,j,k+1,c)
                   wm1  = ws(i,j,k-1,c)

                   rhs(i,j,k,1,c) = rhs(i,j,k,1,c) + dz1tz1 *  &
     &                   (u(i,j,k+1,1,c) - 2.0d0*u(i,j,k,1,c) +  &
     &                    u(i,j,k-1,1,c)) -  &
     &                   tz2 * (u(i,j,k+1,4,c) - u(i,j,k-1,4,c))
                   rhs(i,j,k,2,c) = rhs(i,j,k,2,c) + dz2tz1 *  &
     &                   (u(i,j,k+1,2,c) - 2.0d0*u(i,j,k,2,c) +  &
     &                    u(i,j,k-1,2,c)) +  &
     &                   zzcon2 * (us(i,j,k+1,c) - 2.0d0*us(i,j,k,c) +  &
     &                             us(i,j,k-1,c)) -  &
     &                   tz2 * (u(i,j,k+1,2,c)*wp1 -  &
     &                          u(i,j,k-1,2,c)*wm1)
                   rhs(i,j,k,3,c) = rhs(i,j,k,3,c) + dz3tz1 *  &
     &                   (u(i,j,k+1,3,c) - 2.0d0*u(i,j,k,3,c) +  &
     &                    u(i,j,k-1,3,c)) +  &
     &                   zzcon2 * (vs(i,j,k+1,c) - 2.0d0*vs(i,j,k,c) +  &
     &                             vs(i,j,k-1,c)) -  &
     &                   tz2 * (u(i,j,k+1,3,c)*wp1 -  &
     &                          u(i,j,k-1,3,c)*wm1)
                   rhs(i,j,k,4,c) = rhs(i,j,k,4,c) + dz4tz1 *  &
     &                   (u(i,j,k+1,4,c) - 2.0d0*u(i,j,k,4,c) +  &
     &                    u(i,j,k-1,4,c)) +  &
     &                   zzcon2*con43 * (wp1 - 2.0d0*wijk + wm1) -  &
     &                   tz2 * (u(i,j,k+1,4,c)*wp1 -  &
     &                          u(i,j,k-1,4,c)*wm1 +  &
     &                          (u(i,j,k+1,5,c) - square(i,j,k+1,c) -  &
     &                           u(i,j,k-1,5,c) + square(i,j,k-1,c))  &
     &                          *c2)
                   rhs(i,j,k,5,c) = rhs(i,j,k,5,c) + dz5tz1 *  &
     &                   (u(i,j,k+1,5,c) - 2.0d0*u(i,j,k,5,c) +  &
     &                    u(i,j,k-1,5,c)) +  &
     &                   zzcon3 * (qs(i,j,k+1,c) - 2.0d0*qs(i,j,k,c) +  &
     &                             qs(i,j,k-1,c)) +  &
     &                   zzcon4 * (wp1*wp1 - 2.0d0*wijk*wijk +  &
     &                             wm1*wm1) +  &
     &                   zzcon5 * (u(i,j,k+1,5,c)*rho_i(i,j,k+1,c) -  &
     &                             2.0d0*u(i,j,k,5,c)*rho_i(i,j,k,c) +  &
     &                             u(i,j,k-1,5,c)*rho_i(i,j,k-1,c)) -  &
     &                   tz2 * ( (c1*u(i,j,k+1,5,c) -  &
     &                            c2*square(i,j,k+1,c))*wp1 -  &
     &                           (c1*u(i,j,k-1,5,c) -  &
     &                            c2*square(i,j,k-1,c))*wm1)
                end do
             end do
          end do

!---------------------------------------------------------------------
!         add fourth order zeta-direction dissipation                
!---------------------------------------------------------------------
          if (start(3,c) .gt. 0) then
             k = 1
             do     m = 1, 5
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c)- dssp *  &
     &                    ( 5.0d0*u(i,j,k,m,c) - 4.0d0*u(i,j,k+1,m,c) +  &
     &                            u(i,j,k+2,m,c))
                   end do
                end do
             end do

             k = 2
             do     m = 1, 5
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (-4.0d0*u(i,j,k-1,m,c) + 6.0d0*u(i,j,k,m,c) -  &
     &                      4.0d0*u(i,j,k+1,m,c) + u(i,j,k+2,m,c))
                   end do
                end do
             end do
          endif

          do     m = 1, 5
             do     k = 3*start(3,c), cell_size(3,c)-3*end(3,c)-1
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do     i = start(1,c),cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    (  u(i,j,k-2,m,c) - 4.0d0*u(i,j,k-1,m,c) +  &
     &                     6.0*u(i,j,k,m,c) - 4.0d0*u(i,j,k+1,m,c) +  &
     &                         u(i,j,k+2,m,c) )
                   end do
                end do
             end do
          end do
 
          if (end(3,c) .gt. 0) then
             k = cell_size(3,c)-3
             do     m = 1, 5
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i,j,k-2,m,c) - 4.0d0*u(i,j,k-1,m,c) +  &
     &                      6.0d0*u(i,j,k,m,c) - 4.0d0*u(i,j,k+1,m,c) )
                   end do
                end do
             end do

             k = cell_size(3,c)-2
             do     m = 1, 5
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do     i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - dssp *  &
     &                    ( u(i,j,k-2,m,c) - 4.d0*u(i,j,k-1,m,c) +  &
     &                      5.d0*u(i,j,k,m,c) )
                   end do
                end do
             end do
          endif

          do     m = 1, 5
             do     k = start(3,c), cell_size(3,c)-end(3,c)-1
                do     j = start(2,c), cell_size(2,c)-end(2,c)-1
                   do    i = start(1,c), cell_size(1,c)-end(1,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) * dt
                   end do
                end do
             end do
          end do

       end do
    
       if (timeron) call timer_stop(t_rhs)

       return
       end




