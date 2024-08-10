
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine exact_rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     compute the right hand side based on exact solution
!---------------------------------------------------------------------

      use bt_data
      implicit none

      double precision dtemp(5), xi, eta, zeta, dtpp
      integer          c, m, i, j, k, ip1, im1, jp1,  &
     &     jm1, km1, kp1


!---------------------------------------------------------------------
!     loop over all cells owned by this node                   
!---------------------------------------------------------------------
      do c = 1, ncells

!---------------------------------------------------------------------
!     initialize                                  
!---------------------------------------------------------------------
         do k= 0, cell_size(3,c)-1
            do j = 0, cell_size(2,c)-1
               do i = 0, cell_size(1,c)-1
                  do m = 1, 5
                     forcing(m,i,j,k,c) = 0.0d0
                  enddo
               enddo
            enddo
         enddo

!---------------------------------------------------------------------
!     xi-direction flux differences                      
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            zeta = dble(k+cell_low(3,c)) * dnzm1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               eta = dble(j+cell_low(2,c)) * dnym1

               do i=-2*(1-start(1,c)), cell_size(1,c)+1-2*end(1,c)
                  xi = dble(i+cell_low(1,c)) * dnxm1

                  call exact_solution(xi, eta, zeta, dtemp)
                  do m = 1, 5
                     ue(i,m) = dtemp(m)
                  enddo

                  dtpp = 1.0d0 / dtemp(1)

                  do m = 2, 5
                     buf(i,m) = dtpp * dtemp(m)
                  enddo

                  cuf(i)   = buf(i,2) * buf(i,2)
                  buf(i,1) = cuf(i) + buf(i,3) * buf(i,3) +  &
     &                 buf(i,4) * buf(i,4) 
                  q(i) = 0.5d0*(buf(i,2)*ue(i,2) + buf(i,3)*ue(i,3) +  &
     &                 buf(i,4)*ue(i,4))

               enddo
               
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  im1 = i-1
                  ip1 = i+1

                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -  &
     &                 tx2*( ue(ip1,2)-ue(im1,2) )+  &
     &                 dx1tx1*(ue(ip1,1)-2.0d0*ue(i,1)+ue(im1,1))

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - tx2 * (  &
     &                 (ue(ip1,2)*buf(ip1,2)+c2*(ue(ip1,5)-q(ip1)))-  &
     &                 (ue(im1,2)*buf(im1,2)+c2*(ue(im1,5)-q(im1))))+  &
     &                 xxcon1*(buf(ip1,2)-2.0d0*buf(i,2)+buf(im1,2))+  &
     &                 dx2tx1*( ue(ip1,2)-2.0d0* ue(i,2)+ue(im1,2))

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - tx2 * (  &
     &                 ue(ip1,3)*buf(ip1,2)-ue(im1,3)*buf(im1,2))+  &
     &                 xxcon2*(buf(ip1,3)-2.0d0*buf(i,3)+buf(im1,3))+  &
     &                 dx3tx1*( ue(ip1,3)-2.0d0*ue(i,3) +ue(im1,3))
                  
                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - tx2*(  &
     &                 ue(ip1,4)*buf(ip1,2)-ue(im1,4)*buf(im1,2))+  &
     &                 xxcon2*(buf(ip1,4)-2.0d0*buf(i,4)+buf(im1,4))+  &
     &                 dx4tx1*( ue(ip1,4)-2.0d0* ue(i,4)+ ue(im1,4))

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - tx2*(  &
     &                 buf(ip1,2)*(c1*ue(ip1,5)-c2*q(ip1))-  &
     &                 buf(im1,2)*(c1*ue(im1,5)-c2*q(im1)))+  &
     &                 0.5d0*xxcon3*(buf(ip1,1)-2.0d0*buf(i,1)+  &
     &                 buf(im1,1))+  &
     &                 xxcon4*(cuf(ip1)-2.0d0*cuf(i)+cuf(im1))+  &
     &                 xxcon5*(buf(ip1,5)-2.0d0*buf(i,5)+buf(im1,5))+  &
     &                 dx5tx1*( ue(ip1,5)-2.0d0* ue(i,5)+ ue(im1,5))
               enddo

!---------------------------------------------------------------------
!     Fourth-order dissipation                         
!---------------------------------------------------------------------
               if (start(1,c) .gt. 0) then
                  do m = 1, 5
                     i = 1
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (5.0d0*ue(i,m) - 4.0d0*ue(i+1,m) +ue(i+2,m))
                     i = 2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*ue(i-1,m) + 6.0d0*ue(i,m) -  &
     &                    4.0d0*ue(i+1,m) +       ue(i+2,m))
                  enddo
               endif

               do i = start(1,c)*3, cell_size(1,c)-3*end(1,c)-1
                  do m = 1, 5
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*  &
     &                    (ue(i-2,m) - 4.0d0*ue(i-1,m) +  &
     &                    6.0d0*ue(i,m) - 4.0d0*ue(i+1,m) + ue(i+2,m))
                  enddo
               enddo

               if (end(1,c) .gt. 0) then
                  do m = 1, 5
                     i = cell_size(1,c)-3
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(i-2,m) - 4.0d0*ue(i-1,m) +  &
     &                    6.0d0*ue(i,m) - 4.0d0*ue(i+1,m))
                     i = cell_size(1,c)-2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(i-2,m) - 4.0d0*ue(i-1,m) + 5.0d0*ue(i,m))
                  enddo
               endif

            enddo
         enddo

!---------------------------------------------------------------------
!     eta-direction flux differences             
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1          
            zeta = dble(k+cell_low(3,c)) * dnzm1
            do i=start(1,c), cell_size(1,c)-end(1,c)-1
               xi = dble(i+cell_low(1,c)) * dnxm1

               do j=-2*(1-start(2,c)), cell_size(2,c)+1-2*end(2,c)
                  eta = dble(j+cell_low(2,c)) * dnym1

                  call exact_solution(xi, eta, zeta, dtemp)
                  do m = 1, 5 
                     ue(j,m) = dtemp(m)
                  enddo
                  
                  dtpp = 1.0d0/dtemp(1)

                  do m = 2, 5
                     buf(j,m) = dtpp * dtemp(m)
                  enddo

                  cuf(j)   = buf(j,3) * buf(j,3)
                  buf(j,1) = cuf(j) + buf(j,2) * buf(j,2) +  &
     &                 buf(j,4) * buf(j,4)
                  q(j) = 0.5d0*(buf(j,2)*ue(j,2) + buf(j,3)*ue(j,3) +  &
     &                 buf(j,4)*ue(j,4))
               enddo

               do j = start(2,c), cell_size(2,c)-end(2,c)-1
                  jm1 = j-1
                  jp1 = j+1
                  
                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -  &
     &                 ty2*( ue(jp1,3)-ue(jm1,3) )+  &
     &                 dy1ty1*(ue(jp1,1)-2.0d0*ue(j,1)+ue(jm1,1))

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - ty2*(  &
     &                 ue(jp1,2)*buf(jp1,3)-ue(jm1,2)*buf(jm1,3))+  &
     &                 yycon2*(buf(jp1,2)-2.0d0*buf(j,2)+buf(jm1,2))+  &
     &                 dy2ty1*( ue(jp1,2)-2.0* ue(j,2)+ ue(jm1,2))

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - ty2*(  &
     &                 (ue(jp1,3)*buf(jp1,3)+c2*(ue(jp1,5)-q(jp1)))-  &
     &                 (ue(jm1,3)*buf(jm1,3)+c2*(ue(jm1,5)-q(jm1))))+  &
     &                 yycon1*(buf(jp1,3)-2.0d0*buf(j,3)+buf(jm1,3))+  &
     &                 dy3ty1*( ue(jp1,3)-2.0d0*ue(j,3) +ue(jm1,3))

                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - ty2*(  &
     &                 ue(jp1,4)*buf(jp1,3)-ue(jm1,4)*buf(jm1,3))+  &
     &                 yycon2*(buf(jp1,4)-2.0d0*buf(j,4)+buf(jm1,4))+  &
     &                 dy4ty1*( ue(jp1,4)-2.0d0*ue(j,4)+ ue(jm1,4))

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - ty2*(  &
     &                 buf(jp1,3)*(c1*ue(jp1,5)-c2*q(jp1))-  &
     &                 buf(jm1,3)*(c1*ue(jm1,5)-c2*q(jm1)))+  &
     &                 0.5d0*yycon3*(buf(jp1,1)-2.0d0*buf(j,1)+  &
     &                 buf(jm1,1))+  &
     &                 yycon4*(cuf(jp1)-2.0d0*cuf(j)+cuf(jm1))+  &
     &                 yycon5*(buf(jp1,5)-2.0d0*buf(j,5)+buf(jm1,5))+  &
     &                 dy5ty1*(ue(jp1,5)-2.0d0*ue(j,5)+ue(jm1,5))
               enddo

!---------------------------------------------------------------------
!     Fourth-order dissipation                      
!---------------------------------------------------------------------
               if (start(2,c) .gt. 0) then
                  do m = 1, 5
                     j = 1
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (5.0d0*ue(j,m) - 4.0d0*ue(j+1,m) +ue(j+2,m))
                     j = 2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*ue(j-1,m) + 6.0d0*ue(j,m) -  &
     &                    4.0d0*ue(j+1,m) +       ue(j+2,m))
                  enddo
               endif

               do j = start(2,c)*3, cell_size(2,c)-3*end(2,c)-1
                  do m = 1, 5
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*  &
     &                    (ue(j-2,m) - 4.0d0*ue(j-1,m) +  &
     &                    6.0d0*ue(j,m) - 4.0d0*ue(j+1,m) + ue(j+2,m))
                  enddo
               enddo

               if (end(2,c) .gt. 0) then
                  do m = 1, 5
                     j = cell_size(2,c)-3
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(j-2,m) - 4.0d0*ue(j-1,m) +  &
     &                    6.0d0*ue(j,m) - 4.0d0*ue(j+1,m))
                     j = cell_size(2,c)-2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(j-2,m) - 4.0d0*ue(j-1,m) + 5.0d0*ue(j,m))

                  enddo
               endif

            enddo
         enddo

!---------------------------------------------------------------------
!     zeta-direction flux differences                      
!---------------------------------------------------------------------
         do j=start(2,c), cell_size(2,c)-end(2,c)-1
            eta = dble(j+cell_low(2,c)) * dnym1
            do i = start(1,c), cell_size(1,c)-end(1,c)-1
               xi = dble(i+cell_low(1,c)) * dnxm1

               do k=-2*(1-start(3,c)), cell_size(3,c)+1-2*end(3,c)
                  zeta = dble(k+cell_low(3,c)) * dnzm1

                  call exact_solution(xi, eta, zeta, dtemp)
                  do m = 1, 5
                     ue(k,m) = dtemp(m)
                  enddo

                  dtpp = 1.0d0/dtemp(1)

                  do m = 2, 5
                     buf(k,m) = dtpp * dtemp(m)
                  enddo

                  cuf(k)   = buf(k,4) * buf(k,4)
                  buf(k,1) = cuf(k) + buf(k,2) * buf(k,2) +  &
     &                 buf(k,3) * buf(k,3)
                  q(k) = 0.5d0*(buf(k,2)*ue(k,2) + buf(k,3)*ue(k,3) +  &
     &                 buf(k,4)*ue(k,4))
               enddo

               do k=start(3,c), cell_size(3,c)-end(3,c)-1
                  km1 = k-1
                  kp1 = k+1
                  
                  forcing(1,i,j,k,c) = forcing(1,i,j,k,c) -  &
     &                 tz2*( ue(kp1,4)-ue(km1,4) )+  &
     &                 dz1tz1*(ue(kp1,1)-2.0d0*ue(k,1)+ue(km1,1))

                  forcing(2,i,j,k,c) = forcing(2,i,j,k,c) - tz2 * (  &
     &                 ue(kp1,2)*buf(kp1,4)-ue(km1,2)*buf(km1,4))+  &
     &                 zzcon2*(buf(kp1,2)-2.0d0*buf(k,2)+buf(km1,2))+  &
     &                 dz2tz1*( ue(kp1,2)-2.0d0* ue(k,2)+ ue(km1,2))

                  forcing(3,i,j,k,c) = forcing(3,i,j,k,c) - tz2 * (  &
     &                 ue(kp1,3)*buf(kp1,4)-ue(km1,3)*buf(km1,4))+  &
     &                 zzcon2*(buf(kp1,3)-2.0d0*buf(k,3)+buf(km1,3))+  &
     &                 dz3tz1*(ue(kp1,3)-2.0d0*ue(k,3)+ue(km1,3))

                  forcing(4,i,j,k,c) = forcing(4,i,j,k,c) - tz2 * (  &
     &                 (ue(kp1,4)*buf(kp1,4)+c2*(ue(kp1,5)-q(kp1)))-  &
     &                 (ue(km1,4)*buf(km1,4)+c2*(ue(km1,5)-q(km1))))+  &
     &                 zzcon1*(buf(kp1,4)-2.0d0*buf(k,4)+buf(km1,4))+  &
     &                 dz4tz1*( ue(kp1,4)-2.0d0*ue(k,4) +ue(km1,4))

                  forcing(5,i,j,k,c) = forcing(5,i,j,k,c) - tz2 * (  &
     &                 buf(kp1,4)*(c1*ue(kp1,5)-c2*q(kp1))-  &
     &                 buf(km1,4)*(c1*ue(km1,5)-c2*q(km1)))+  &
     &                 0.5d0*zzcon3*(buf(kp1,1)-2.0d0*buf(k,1)  &
     &                 +buf(km1,1))+  &
     &                 zzcon4*(cuf(kp1)-2.0d0*cuf(k)+cuf(km1))+  &
     &                 zzcon5*(buf(kp1,5)-2.0d0*buf(k,5)+buf(km1,5))+  &
     &                 dz5tz1*( ue(kp1,5)-2.0d0*ue(k,5)+ ue(km1,5))
               enddo

!---------------------------------------------------------------------
!     Fourth-order dissipation                        
!---------------------------------------------------------------------
               if (start(3,c) .gt. 0) then
                  do m = 1, 5
                     k = 1
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (5.0d0*ue(k,m) - 4.0d0*ue(k+1,m) +ue(k+2,m))
                     k = 2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (-4.0d0*ue(k-1,m) + 6.0d0*ue(k,m) -  &
     &                    4.0d0*ue(k+1,m) +       ue(k+2,m))
                  enddo
               endif

               do k = start(3,c)*3, cell_size(3,c)-3*end(3,c)-1
                  do m = 1, 5
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp*  &
     &                    (ue(k-2,m) - 4.0d0*ue(k-1,m) +  &
     &                    6.0d0*ue(k,m) - 4.0d0*ue(k+1,m) + ue(k+2,m))
                  enddo
               enddo

               if (end(3,c) .gt. 0) then
                  do m = 1, 5
                     k = cell_size(3,c)-3
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(k-2,m) - 4.0d0*ue(k-1,m) +  &
     &                    6.0d0*ue(k,m) - 4.0d0*ue(k+1,m))
                     k = cell_size(3,c)-2
                     forcing(m,i,j,k,c) = forcing(m,i,j,k,c) - dssp *  &
     &                    (ue(k-2,m) - 4.0d0*ue(k-1,m) + 5.0d0*ue(k,m))
                  enddo
               endif

            enddo
         enddo

!---------------------------------------------------------------------
!     now change the sign of the forcing function, 
!---------------------------------------------------------------------
         do k = start(3,c), cell_size(3,c)-end(3,c)-1
            do j = start(2,c), cell_size(2,c)-end(2,c)-1
               do i = start(1,c), cell_size(1,c)-end(1,c)-1
                  do m = 1, 5
                     forcing(m,i,j,k,c) = -1.d0 * forcing(m,i,j,k,c)
                  enddo
               enddo
            enddo
         enddo

      enddo

      return
      end
