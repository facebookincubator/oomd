
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine  initialize

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! This subroutine initializes the field variable u using 
! tri-linear transfinite interpolation of the boundary values     
!---------------------------------------------------------------------

       use sp_data
       implicit none
  
       integer c, i, j, k, m, ii, jj, kk, ix, iy, iz
       double precision  xi, eta, zeta, Pface(5,3,2), Pxi, Peta,  &
     &                   Pzeta, temp(5)


!---------------------------------------------------------------------
!  Later (in compute_rhs) we compute 1/u for every element. A few of 
!  the corner elements are not used, but it convenient (and faster) 
!  to compute the whole thing with a simple loop. Make sure those 
!  values are nonzero by initializing the whole thing here. 
!---------------------------------------------------------------------
      do c = 1, ncells
         do kk = -1, IMAX
            do jj = -1, IMAX
               do ii = -1, IMAX
                  u(ii, jj, kk, 1, c) = 1.0
                  u(ii, jj, kk, 2, c) = 0.0
                  u(ii, jj, kk, 3, c) = 0.0
                  u(ii, jj, kk, 4, c) = 0.0
                  u(ii, jj, kk, 5, c) = 1.0
               end do
            end do
         end do
      end do

!---------------------------------------------------------------------
! first store the "interpolated" values everywhere on the grid    
!---------------------------------------------------------------------
       do  c=1, ncells
          kk = 0
          do  k = cell_low(3,c), cell_high(3,c)
             zeta = dble(k) * dnzm1
             jj = 0
             do  j = cell_low(2,c), cell_high(2,c)
                eta = dble(j) * dnym1
                ii = 0
                do   i = cell_low(1,c), cell_high(1,c)
                   xi = dble(i) * dnxm1
                  
                   do ix = 1, 2
                      call exact_solution(dble(ix-1), eta, zeta,  &
     &                                    Pface(1,1,ix))
                   end do

                   do    iy = 1, 2
                      call exact_solution(xi, dble(iy-1) , zeta,  &
     &                                    Pface(1,2,iy))
                   end do

                   do    iz = 1, 2
                      call exact_solution(xi, eta, dble(iz-1),   &
     &                                    Pface(1,3,iz))
                   end do

                   do   m = 1, 5
                      Pxi   = xi   * Pface(m,1,2) +  &
     &                        (1.0d0-xi)   * Pface(m,1,1)
                      Peta  = eta  * Pface(m,2,2) +  &
     &                        (1.0d0-eta)  * Pface(m,2,1)
                      Pzeta = zeta * Pface(m,3,2) +  &
     &                        (1.0d0-zeta) * Pface(m,3,1)
 
                      u(ii,jj,kk,m,c) = Pxi + Peta + Pzeta -  &
     &                          Pxi*Peta - Pxi*Pzeta - Peta*Pzeta +  &
     &                          Pxi*Peta*Pzeta

                   end do
                   ii = ii + 1
                end do
                jj = jj + 1
             end do
             kk = kk+1
          end do
       end do

!---------------------------------------------------------------------
! now store the exact values on the boundaries        
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! west face                                                  
!---------------------------------------------------------------------
       c = slice(1,1)
       ii = 0
       xi = 0.0d0
       kk = 0
       do  k = cell_low(3,c), cell_high(3,c)
          zeta = dble(k) * dnzm1
          jj = 0
          do   j = cell_low(2,c), cell_high(2,c)
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             jj = jj + 1
          end do
          kk = kk + 1
       end do

!---------------------------------------------------------------------
! east face                                                      
!---------------------------------------------------------------------
       c  = slice(1,ncells)
       ii = cell_size(1,c)-1
       xi = 1.0d0
       kk = 0
       do   k = cell_low(3,c), cell_high(3,c)
          zeta = dble(k) * dnzm1
          jj = 0
          do   j = cell_low(2,c), cell_high(2,c)
             eta = dble(j) * dnym1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             jj = jj + 1
          end do
          kk = kk + 1
       end do

!---------------------------------------------------------------------
! south face                                                 
!---------------------------------------------------------------------
       c = slice(2,1)
       jj = 0
       eta = 0.0d0
       kk = 0
       do  k = cell_low(3,c), cell_high(3,c)
          zeta = dble(k) * dnzm1
          ii = 0
          do   i = cell_low(1,c), cell_high(1,c)
             xi = dble(i) * dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             ii = ii + 1
          end do
          kk = kk + 1
       end do


!---------------------------------------------------------------------
! north face                                    
!---------------------------------------------------------------------
       c = slice(2,ncells)
       jj = cell_size(2,c)-1
       eta = 1.0d0
       kk = 0
       do   k = cell_low(3,c), cell_high(3,c)
          zeta = dble(k) * dnzm1
          ii = 0
          do   i = cell_low(1,c), cell_high(1,c)
             xi = dble(i) * dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             ii = ii + 1
          end do
          kk = kk + 1
       end do

!---------------------------------------------------------------------
! bottom face                                       
!---------------------------------------------------------------------
       c = slice(3,1)
       kk = 0
       zeta = 0.0d0
       jj = 0
       do   j = cell_low(2,c), cell_high(2,c)
          eta = dble(j) * dnym1
          ii = 0
          do   i =cell_low(1,c), cell_high(1,c)
             xi = dble(i) *dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             ii = ii + 1
          end do
          jj = jj + 1
       end do

!---------------------------------------------------------------------
! top face     
!---------------------------------------------------------------------
       c = slice(3,ncells)
       kk = cell_size(3,c)-1
       zeta = 1.0d0
       jj = 0
       do   j = cell_low(2,c), cell_high(2,c)
          eta = dble(j) * dnym1
          ii = 0
          do   i =cell_low(1,c), cell_high(1,c)
             xi = dble(i) * dnxm1
             call exact_solution(xi, eta, zeta, temp)
             do   m = 1, 5
                u(ii,jj,kk,m,c) = temp(m)
             end do
             ii = ii + 1
          end do
          jj = jj + 1
       end do

       return
       end


       subroutine lhsinit

       use sp_data
       implicit none
       
       integer i, j, k, d, c, n

!---------------------------------------------------------------------
! loop over all cells                                       
!---------------------------------------------------------------------
       do  c = 1, ncells

!---------------------------------------------------------------------
!         first, initialize the start and end arrays
!---------------------------------------------------------------------
          do  d = 1, 3
             if (cell_coord(d,c) .eq. 1) then
                start(d,c) = 1
             else 
                start(d,c) = 0
             endif
             if (cell_coord(d,c) .eq. ncells) then
                end(d,c) = 1
             else
                end(d,c) = 0
             endif
          end do

!---------------------------------------------------------------------
!     zap the whole left hand side for starters
!---------------------------------------------------------------------
          do  n = 1, 15
             do  k = 0, cell_size(3,c)-1
                do  j = 0, cell_size(2,c)-1
                   do  i = 0, cell_size(1,c)-1
                      lhs(i,j,k,n,c) = 0.0d0
                   end do
                end do
             end do
          end do

!---------------------------------------------------------------------
! next, set all diagonal values to 1. This is overkill, but convenient
!---------------------------------------------------------------------
          do   n = 1, 3
             do   k = 0, cell_size(3,c)-1
                do   j = 0, cell_size(2,c)-1
                   do   i = 0, cell_size(1,c)-1
                      lhs(i,j,k,5*n-2,c) = 1.0d0
                   end do
                end do
             end do
          end do

       end do

      return
      end



