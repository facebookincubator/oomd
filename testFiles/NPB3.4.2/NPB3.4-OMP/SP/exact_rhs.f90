
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine exact_rhs

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! compute the right hand side based on exact solution
!---------------------------------------------------------------------

       use sp_data
       implicit none

       double precision dtemp(5), xi, eta, zeta, dtpp
       integer          m, i, j, k, ip1, im1, jp1,  &
     &                  jm1, km1, kp1

!$omp parallel default(shared)  &
!$omp& private(i,j,k,m,zeta,eta,xi,dtpp,im1,ip1,  &
!$omp&         jm1,jp1,km1,kp1,dtemp)
!---------------------------------------------------------------------
!      initialize                                  
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
       do   k= 0, grid_points(3)-1
          do   j = 0, grid_points(2)-1
             do   i = 0, grid_points(1)-1
                do   m = 1, 5
                   forcing(m,i,j,k) = 0.0d0
                end do
             end do
          end do
       end do

!---------------------------------------------------------------------
!      xi-direction flux differences                      
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
          zeta = dble(k) * dnzm1
             eta = dble(j) * dnym1

             do  i=0, grid_points(1)-1
                xi = dble(i) * dnxm1

                call exact_solution(xi, eta, zeta, dtemp)
                do  m = 1, 5
                   ue(i,m) = dtemp(m)
                end do

                dtpp = 1.0d0 / dtemp(1)

                do  m = 2, 5
                   buf(i,m) = dtpp * dtemp(m)
                end do

                cuf(i)   = buf(i,2) * buf(i,2)
                buf(i,1) = cuf(i) + buf(i,3) * buf(i,3) +  &
     &                     buf(i,4) * buf(i,4) 
                q(i) = 0.5d0*(buf(i,2)*ue(i,2) + buf(i,3)*ue(i,3) +  &
     &                        buf(i,4)*ue(i,4))

             end do
 
             do  i = 1, grid_points(1)-2
                im1 = i-1
                ip1 = i+1

                forcing(1,i,j,k) = forcing(1,i,j,k) -  &
     &                 tx2*( ue(ip1,2)-ue(im1,2) )+  &
     &                 dx1tx1*(ue(ip1,1)-2.0d0*ue(i,1)+ue(im1,1))

                forcing(2,i,j,k) = forcing(2,i,j,k) - tx2 * (  &
     &                (ue(ip1,2)*buf(ip1,2)+c2*(ue(ip1,5)-q(ip1)))-  &
     &                (ue(im1,2)*buf(im1,2)+c2*(ue(im1,5)-q(im1))))+  &
     &                 xxcon1*(buf(ip1,2)-2.0d0*buf(i,2)+buf(im1,2))+  &
     &                 dx2tx1*( ue(ip1,2)-2.0d0* ue(i,2)+ue(im1,2))

                forcing(3,i,j,k) = forcing(3,i,j,k) - tx2 * (  &
     &                 ue(ip1,3)*buf(ip1,2)-ue(im1,3)*buf(im1,2))+  &
     &                 xxcon2*(buf(ip1,3)-2.0d0*buf(i,3)+buf(im1,3))+  &
     &                 dx3tx1*( ue(ip1,3)-2.0d0*ue(i,3) +ue(im1,3))
                  
                forcing(4,i,j,k) = forcing(4,i,j,k) - tx2*(  &
     &                 ue(ip1,4)*buf(ip1,2)-ue(im1,4)*buf(im1,2))+  &
     &                 xxcon2*(buf(ip1,4)-2.0d0*buf(i,4)+buf(im1,4))+  &
     &                 dx4tx1*( ue(ip1,4)-2.0d0* ue(i,4)+ ue(im1,4))

                forcing(5,i,j,k) = forcing(5,i,j,k) - tx2*(  &
     &                 buf(ip1,2)*(c1*ue(ip1,5)-c2*q(ip1))-  &
     &                 buf(im1,2)*(c1*ue(im1,5)-c2*q(im1)))+  &
     &                 0.5d0*xxcon3*(buf(ip1,1)-2.0d0*buf(i,1)+  &
     &                               buf(im1,1))+  &
     &                 xxcon4*(cuf(ip1)-2.0d0*cuf(i)+cuf(im1))+  &
     &                 xxcon5*(buf(ip1,5)-2.0d0*buf(i,5)+buf(im1,5))+  &
     &                 dx5tx1*( ue(ip1,5)-2.0d0* ue(i,5)+ ue(im1,5))
             end do

!---------------------------------------------------------------------
!            Fourth-order dissipation                         
!---------------------------------------------------------------------
             do   m = 1, 5
                i = 1
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                    (5.0d0*ue(i,m) - 4.0d0*ue(i+1,m) +ue(i+2,m))
                i = 2
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (-4.0d0*ue(i-1,m) + 6.0d0*ue(i,m) -  &
     &                     4.0d0*ue(i+1,m) +       ue(i+2,m))
             end do

             do   m = 1, 5
                do  i = 3, grid_points(1)-4
                   forcing(m,i,j,k) = forcing(m,i,j,k) - dssp*  &
     &                   (ue(i-2,m) - 4.0d0*ue(i-1,m) +  &
     &                    6.0d0*ue(i,m) - 4.0d0*ue(i+1,m) + ue(i+2,m))
                end do
             end do

             do   m = 1, 5
                i = grid_points(1)-3
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(i-2,m) - 4.0d0*ue(i-1,m) +  &
     &                    6.0d0*ue(i,m) - 4.0d0*ue(i+1,m))
                i = grid_points(1)-2
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(i-2,m) - 4.0d0*ue(i-1,m) + 5.0d0*ue(i,m))
             end do

          end do
       end do
!$omp end do

!---------------------------------------------------------------------
!  eta-direction flux differences             
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
       do   k = 1, grid_points(3)-2          
          do   i=1, grid_points(1)-2
          zeta = dble(k) * dnzm1
             xi = dble(i) * dnxm1

             do  j=0, grid_points(2)-1
                eta = dble(j) * dnym1

                call exact_solution(xi, eta, zeta, dtemp)
                do   m = 1, 5 
                   ue(j,m) = dtemp(m)
                end do
                dtpp = 1.0d0/dtemp(1)

                do  m = 2, 5
                   buf(j,m) = dtpp * dtemp(m)
                end do

                cuf(j)   = buf(j,3) * buf(j,3)
                buf(j,1) = cuf(j) + buf(j,2) * buf(j,2) +  &
     &                     buf(j,4) * buf(j,4)
                q(j) = 0.5d0*(buf(j,2)*ue(j,2) + buf(j,3)*ue(j,3) +  &
     &                        buf(j,4)*ue(j,4))
             end do

             do  j = 1, grid_points(2)-2
                jm1 = j-1
                jp1 = j+1
                  
                forcing(1,i,j,k) = forcing(1,i,j,k) -  &
     &                ty2*( ue(jp1,3)-ue(jm1,3) )+  &
     &                dy1ty1*(ue(jp1,1)-2.0d0*ue(j,1)+ue(jm1,1))

                forcing(2,i,j,k) = forcing(2,i,j,k) - ty2*(  &
     &                ue(jp1,2)*buf(jp1,3)-ue(jm1,2)*buf(jm1,3))+  &
     &                yycon2*(buf(jp1,2)-2.0d0*buf(j,2)+buf(jm1,2))+  &
     &                dy2ty1*( ue(jp1,2)-2.0* ue(j,2)+ ue(jm1,2))

                forcing(3,i,j,k) = forcing(3,i,j,k) - ty2*(  &
     &                (ue(jp1,3)*buf(jp1,3)+c2*(ue(jp1,5)-q(jp1)))-  &
     &                (ue(jm1,3)*buf(jm1,3)+c2*(ue(jm1,5)-q(jm1))))+  &
     &                yycon1*(buf(jp1,3)-2.0d0*buf(j,3)+buf(jm1,3))+  &
     &                dy3ty1*( ue(jp1,3)-2.0d0*ue(j,3) +ue(jm1,3))

                forcing(4,i,j,k) = forcing(4,i,j,k) - ty2*(  &
     &                ue(jp1,4)*buf(jp1,3)-ue(jm1,4)*buf(jm1,3))+  &
     &                yycon2*(buf(jp1,4)-2.0d0*buf(j,4)+buf(jm1,4))+  &
     &                dy4ty1*( ue(jp1,4)-2.0d0*ue(j,4)+ ue(jm1,4))

                forcing(5,i,j,k) = forcing(5,i,j,k) - ty2*(  &
     &                buf(jp1,3)*(c1*ue(jp1,5)-c2*q(jp1))-  &
     &                buf(jm1,3)*(c1*ue(jm1,5)-c2*q(jm1)))+  &
     &                0.5d0*yycon3*(buf(jp1,1)-2.0d0*buf(j,1)+  &
     &                              buf(jm1,1))+  &
     &                yycon4*(cuf(jp1)-2.0d0*cuf(j)+cuf(jm1))+  &
     &                yycon5*(buf(jp1,5)-2.0d0*buf(j,5)+buf(jm1,5))+  &
     &                dy5ty1*(ue(jp1,5)-2.0d0*ue(j,5)+ue(jm1,5))
             end do

!---------------------------------------------------------------------
!            Fourth-order dissipation                      
!---------------------------------------------------------------------
             do   m = 1, 5
                j = 1
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                    (5.0d0*ue(j,m) - 4.0d0*ue(j+1,m) +ue(j+2,m))
                j = 2
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (-4.0d0*ue(j-1,m) + 6.0d0*ue(j,m) -  &
     &                     4.0d0*ue(j+1,m) +       ue(j+2,m))
             end do

             do   m = 1, 5
                do  j = 3, grid_points(2)-4
                   forcing(m,i,j,k) = forcing(m,i,j,k) - dssp*  &
     &                   (ue(j-2,m) - 4.0d0*ue(j-1,m) +  &
     &                    6.0d0*ue(j,m) - 4.0d0*ue(j+1,m) + ue(j+2,m))
                end do
             end do

             do   m = 1, 5
                j = grid_points(2)-3
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(j-2,m) - 4.0d0*ue(j-1,m) +  &
     &                    6.0d0*ue(j,m) - 4.0d0*ue(j+1,m))
                j = grid_points(2)-2
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(j-2,m) - 4.0d0*ue(j-1,m) + 5.0d0*ue(j,m))

             end do

          end do
       end do

!---------------------------------------------------------------------
!      zeta-direction flux differences                      
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
       do  j=1, grid_points(2)-2
          do   i = 1, grid_points(1)-2
          eta = dble(j) * dnym1
             xi = dble(i) * dnxm1

             do k=0, grid_points(3)-1
                zeta = dble(k) * dnzm1

                call exact_solution(xi, eta, zeta, dtemp)
                do   m = 1, 5
                   ue(k,m) = dtemp(m)
                end do

                dtpp = 1.0d0/dtemp(1)

                do   m = 2, 5
                   buf(k,m) = dtpp * dtemp(m)
                end do

                cuf(k)   = buf(k,4) * buf(k,4)
                buf(k,1) = cuf(k) + buf(k,2) * buf(k,2) +  &
     &                     buf(k,3) * buf(k,3)
                q(k) = 0.5d0*(buf(k,2)*ue(k,2) + buf(k,3)*ue(k,3) +  &
     &                        buf(k,4)*ue(k,4))
             end do

             do    k=1, grid_points(3)-2
                km1 = k-1
                kp1 = k+1

                forcing(1,i,j,k) = forcing(1,i,j,k) -  &
     &                 tz2*( ue(kp1,4)-ue(km1,4) )+  &
     &                 dz1tz1*(ue(kp1,1)-2.0d0*ue(k,1)+ue(km1,1))

                forcing(2,i,j,k) = forcing(2,i,j,k) - tz2 * (  &
     &                 ue(kp1,2)*buf(kp1,4)-ue(km1,2)*buf(km1,4))+  &
     &                 zzcon2*(buf(kp1,2)-2.0d0*buf(k,2)+buf(km1,2))+  &
     &                 dz2tz1*( ue(kp1,2)-2.0d0* ue(k,2)+ ue(km1,2))

                forcing(3,i,j,k) = forcing(3,i,j,k) - tz2 * (  &
     &                 ue(kp1,3)*buf(kp1,4)-ue(km1,3)*buf(km1,4))+  &
     &                 zzcon2*(buf(kp1,3)-2.0d0*buf(k,3)+buf(km1,3))+  &
     &                 dz3tz1*(ue(kp1,3)-2.0d0*ue(k,3)+ue(km1,3))

                forcing(4,i,j,k) = forcing(4,i,j,k) - tz2 * (  &
     &                (ue(kp1,4)*buf(kp1,4)+c2*(ue(kp1,5)-q(kp1)))-  &
     &                (ue(km1,4)*buf(km1,4)+c2*(ue(km1,5)-q(km1))))+  &
     &                zzcon1*(buf(kp1,4)-2.0d0*buf(k,4)+buf(km1,4))+  &
     &                dz4tz1*( ue(kp1,4)-2.0d0*ue(k,4) +ue(km1,4))

                forcing(5,i,j,k) = forcing(5,i,j,k) - tz2 * (  &
     &                 buf(kp1,4)*(c1*ue(kp1,5)-c2*q(kp1))-  &
     &                 buf(km1,4)*(c1*ue(km1,5)-c2*q(km1)))+  &
     &                 0.5d0*zzcon3*(buf(kp1,1)-2.0d0*buf(k,1)  &
     &                              +buf(km1,1))+  &
     &                 zzcon4*(cuf(kp1)-2.0d0*cuf(k)+cuf(km1))+  &
     &                 zzcon5*(buf(kp1,5)-2.0d0*buf(k,5)+buf(km1,5))+  &
     &                 dz5tz1*( ue(kp1,5)-2.0d0*ue(k,5)+ ue(km1,5))
             end do

!---------------------------------------------------------------------
!            Fourth-order dissipation
!---------------------------------------------------------------------
             do   m = 1, 5
                k = 1
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                    (5.0d0*ue(k,m) - 4.0d0*ue(k+1,m) +ue(k+2,m))
                k = 2
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (-4.0d0*ue(k-1,m) + 6.0d0*ue(k,m) -  &
     &                     4.0d0*ue(k+1,m) +       ue(k+2,m))
             end do

             do   m = 1, 5
                do  k = 3, grid_points(3)-4
                   forcing(m,i,j,k) = forcing(m,i,j,k) - dssp*  &
     &                   (ue(k-2,m) - 4.0d0*ue(k-1,m) +  &
     &                    6.0d0*ue(k,m) - 4.0d0*ue(k+1,m) + ue(k+2,m))
                end do
             end do

             do    m = 1, 5
                k = grid_points(3)-3
                forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(k-2,m) - 4.0d0*ue(k-1,m) +  &
     &                    6.0d0*ue(k,m) - 4.0d0*ue(k+1,m))
                   k = grid_points(3)-2
                   forcing(m,i,j,k) = forcing(m,i,j,k) - dssp *  &
     &                   (ue(k-2,m) - 4.0d0*ue(k-1,m) + 5.0d0*ue(k,m))
                end do

          end do
       end do

!---------------------------------------------------------------------
! now change the sign of the forcing function, 
!---------------------------------------------------------------------
!$omp do schedule(static) collapse(2)
       do   k = 1, grid_points(3)-2
          do   j = 1, grid_points(2)-2
             do   i = 1, grid_points(1)-2
                do   m = 1, 5
                   forcing(m,i,j,k) = -1.d0 * forcing(m,i,j,k)
                end do
             end do
          end do
       end do
!$omp end do nowait
!$omp end parallel

       return
       end





