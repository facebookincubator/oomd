!-----------------------------------------------------------------
      subroutine create_initial_grid        
!------------------------------------------------------------------
    
      use ua_data
      implicit none

      integer i

      nelt=1
      ntot=nelt*lx1*lx1*lx1 
      tree(1)=1
      mt_to_id(1)=1
      do i=1,7,2
        xc(i,1)=0.d0
        xc(i+1,1)=1.d0
      end do

      do i=1,2
        yc(i,1)=0.d0
        yc(2+i,1)=1.d0
        yc(4+i,1)=0.d0
        yc(6+i,1)=1.d0
      end do
     
      do i=1,4
        zc(i,1)=0.d0
        zc(4+i,1)=1.d0
      end do
  
      do i=1,6
        cbc(i,1)=0
      end do

      return

      end

!-----------------------------------------------------------------
      subroutine coef
!-----------------------------------------------------------------
!
!     generate 
!
!            - collocation points
!            - weights
!            - derivative matrices 
!            - projection matrices
!            - interpolation matrices 
!
!     associated with the 
!
!            - gauss-legendre lobatto mesh (suffix m1)
!
!----------------------------------------------------------------

      use ua_data
      implicit none

      integer i,j,k

!.....for gauss-legendre lobatto mesh (suffix m1)
!.....generate collocation points and weights 

      zgm1(1)=-1.d0
      zgm1(2)=-0.6546536707079771d0
      zgm1(3)=0.d0
      zgm1(4)= 0.6546536707079771d0
      zgm1(5)=1.d0
      wxm1(1)=0.1d0
      wxm1(2)=49.d0/90.d0
      wxm1(3)=32.d0/45.d0
      wxm1(4)=wxm1(2)
      wxm1(5)=0.1d0 

      do k=1,lx1
        do j=1,lx1
          do i=1,lx1
            w3m1(i,j,k)=wxm1(i)*wxm1(j)*wxm1(k)
          end do
        end do
      end do

!.....generate derivative matrices

      dxm1(1,1)=-5.0d0
      dxm1(2,1)=-1.240990253030982d0
      dxm1(3,1)= 0.375d0
      dxm1(4,1)=-0.2590097469690172d0
      dxm1(5,1)= 0.5d0
      dxm1(1,2)= 6.756502488724238d0
      dxm1(2,2)= 0.d0
      dxm1(3,2)=-1.336584577695453d0
      dxm1(4,2)= 0.7637626158259734d0
      dxm1(5,2)=-1.410164177942427d0
      dxm1(1,3)=-2.666666666666667d0
      dxm1(2,3)= 1.745743121887939d0
      dxm1(3,3)= 0.d0
      dxm1(4,3)=-dxm1(2,3)
      dxm1(5,3)=-dxm1(1,3)
      do j=4,lx1
        do i=1,lx1
          dxm1(i,j)=-dxm1(lx1+1-i,lx1+1-j)
        end do
      end do
      do j=1,lx1
        do i=1,lx1
          dxtm1(i,j)=dxm1(j,i)
        end do
      end do

!.....generate projection (mapping) matrices

      qbnew(1,1,1)=-0.1772843218615690d0
      qbnew(2,1,1)=9.375d-02
      qbnew(3,1,1)=-3.700139242414530d-02
      qbnew(1,2,1)= 0.7152146412463197d0
      qbnew(2,2,1)=-0.2285757930375471d0
      qbnew(3,2,1)= 8.333333333333333d-02
      qbnew(1,3,1)= 0.4398680650316104d0
      qbnew(2,3,1)= 0.2083333333333333d0
      qbnew(3,3,1)=-5.891568407922938d-02
      qbnew(1,4,1)= 8.333333333333333d-02
      qbnew(2,4,1)= 0.3561799597042137d0
      qbnew(3,4,1)=-4.854797457965334d-02
      qbnew(1,5,1)= 0.d0
      qbnew(2,5,1)=7.03125d-02
      qbnew(3,5,1)=0.d0
      
      do j=1,lx1
        do i=1,3
          qbnew(i,j,2)=qbnew(4-i,lx1+1-j,1)
        end do
      end do 

!.....generate interpolation matrices for mesh refinement

      ixtmc1(1,1)=1.d0
      ixtmc1(2,1)=0.d0
      ixtmc1(3,1)=0.d0
      ixtmc1(4,1)=0.d0
      ixtmc1(5,1)=0.d0 
      ixtmc1(1,2)= 0.3385078435248143d0
      ixtmc1(2,2)= 0.7898516348912331d0
      ixtmc1(3,2)=-0.1884018684471238d0
      ixtmc1(4,2)= 9.202967302175333d-02
      ixtmc1(5,2)=-3.198728299067715d-02
      ixtmc1(1,3)=-0.1171875d0
      ixtmc1(2,3)= 0.8840317166357952d0
      ixtmc1(3,3)= 0.3125d0    
      ixtmc1(4,3)=-0.118406716635795d0 
      ixtmc1(5,3)= 0.0390625d0   
      ixtmc1(1,4)=-7.065070066767144d-02
      ixtmc1(2,4)= 0.2829703269782467d0 
      ixtmc1(3,4)= 0.902687582732838d0
      ixtmc1(4,4)=-0.1648516348912333d0 
      ixtmc1(5,4)= 4.984442584781999d-02
      ixtmc1(1,5)=0.d0
      ixtmc1(2,5)=0.d0
      ixtmc1(3,5)=1.d0 
      ixtmc1(4,5)=0.d0
      ixtmc1(5,5)=0.d0  
      do j=1,lx1
        do i=1,lx1
          ixmc1(i,j)=ixtmc1(j,i)
        end do
      end do

      do j=1,lx1
        do i=1,lx1
          ixtmc2(i,j)=ixtmc1(lx1+1-i,lx1+1-j)
        end do
      end do

      do j=1,lx1
        do i=1,lx1
          ixmc2(i,j)=ixtmc2(j,i)
        end do
      end do

!.....solution interpolation matrix for mesh coarsening

      map2(1)=-0.1179652785083428d0
      map2(2)= 0.5505046330389332d0
      map2(3)= 0.7024534364259963d0
      map2(4)=-0.1972224518285866d0
      map2(5)= 6.222966087199998d-02

      do i=1,lx1
        map4(i)=map2(lx1+1-i)
      end do

      return
      end

!-------------------------------------------------------------------
      subroutine geom1
!-------------------------------------------------------------------
!
!     routine to generate elemental geometry information on mesh m1,
!     (gauss-legendre lobatto mesh).
!
!         xrm1_s   -   dx/dr, dy/dr, dz/dr
!         rxm1_s   -   dr/dx, dr/dy, dr/dz
!         g1m1_s  geometric factors used in preconditioner computation
!         g4m1_s  g5m1_s  g6m1_s :
!         geometric factors used in lapacian opertor
!         jacm1    -   jacobian
!         bm1      -   mass matrix
!         xfrac    -   will be used in prepwork for calculating collocation
!                      coordinates
!         idel     -   collocation points index on element boundaries 
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision temp,temp1,temp2,dtemp
      integer isize,i,j,k,ntemp,iel
 
      do i=1,lx1
        xfrac(i)=zgm1(i)*0.5d0 + 0.5d0
      end do

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ISIZE,TEMP,TEMP1,TEMP2,  &
!$OMP&  K,J,I,dtemp)
      do isize=1,refine_max
        temp=2.d0**(-isize-1)
        dtemp=1.d0/temp
        temp1=temp**3
        temp2=temp**2
        do k=1,lx1
          do j=1,lx1
            do i=1,lx1
              xrm1_s(i,j,k,isize)=dtemp
              jacm1_s(i,j,k,isize)=temp1
              rxm1_s(i,j,k,isize)=temp2
              g1m1_s(i,j,k,isize)=w3m1(i,j,k)*temp
              bm1_s(i,j,k,isize)=w3m1(i,j,k)*temp1
              g4m1_s(i,j,k,isize)=g1m1_s(i,j,k,isize)/wxm1(i)
              g5m1_s(i,j,k,isize)=g1m1_s(i,j,k,isize)/wxm1(j)
              g6m1_s(i,j,k,isize)=g1m1_s(i,j,k,isize)/wxm1(k)
            end do
          end do
        end do
      end do
!$OMP END PARALLEL DO

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ntemp,i,j,iel)
      do iel = 1, lelt
        ntemp=lx1*lx1*lx1*(iel-1)
        do j = 1, lx1
          do i = 1, lx1
            idel(i,j,1,iel)=ntemp+(i-1)*lx1 + (j-1)*lx1*lx1+lx1
            idel(i,j,2,iel)=ntemp+(i-1)*lx1 + (j-1)*lx1*lx1+1
            idel(i,j,3,iel)=ntemp+(i-1)*1 + (j-1)*lx1*lx1+lx1*(lx1-1)+1
            idel(i,j,4,iel)=ntemp+(i-1)*1 + (j-1)*lx1*lx1+1
            idel(i,j,5,iel)=ntemp+(i-1)*1 + (j-1)*lx1+lx1*lx1*(lx1-1)+1
            idel(i,j,6,iel)=ntemp+(i-1)*1 + (j-1)*lx1+1
          end do
        end do
      end do
!$OMP END PARALLEL DO

      return
      end

!------------------------------------------------------------------
      subroutine setdef
!------------------------------------------------------------------
!     compute the discrete laplacian operators
!------------------------------------------------------------------

      use ua_data
      implicit none

      integer i,j,ip
 
      call r_init(wdtdr(1,1),lx1*lx1,0.d0)

      do i=1,lx1
        do j=1,lx1
          do ip=1,lx1
            wdtdr(i,j) = wdtdr(i,j) + wxm1(ip)*dxm1(ip,i)*dxm1(ip,j)
          end do
        end do
      end do

      return 
      end


!------------------------------------------------------------------
      subroutine prepwork
!------------------------------------------------------------------
!     mesh information preparations: calculate refinement levels of
!     each element, mask matrix for domain boundary and element 
!     boundaries
!------------------------------------------------------------------

      use ua_data
      implicit none

      integer i, j, iel, iface, cb
      double precision rdlog2

      ntot = nelt*nxyz
      rdlog2 = 1.d0/dlog(2.d0)

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(I,J,IEL,IFACE,CB)

!.....calculate the refinement levels of each element

!$OMP DO 
      do iel = 1, nelt
        size_e(iel)=-dlog(xc(2,iel)-xc(1,iel))*rdlog2+1.d-8
      end do
!$OMP END DO nowait

!.....mask matrix for element boundary

!$OMP DO
      do iel = 1, nelt
        call r_init(tmult(1,1,1,iel),nxyz,1.d0)   
        do iface=1,nsides
          call facev(tmult(1,1,1,iel),iface,0.0d0)
        end do
      end do
!$OMP END DO nowait

!.....masks for domain boundary at mortar 

!$OMP DO
      do iel=1,nmor
        tmmor(iel)=1.d0
      end do
!$OMP END DO

!$OMP DO
      do iel = 1, nelt
        do iface = 1,nsides
          cb=cbc(iface,iel)
          if(cb.eq.0) then
            do j=2,lx1-1
              do i=2,lx1-1
               tmmor(idmo(i,j,1,1,iface,iel))=0.d0
              end do
            end do

            j=1
            do i = 1, lx1-1
               tmmor(idmo(i,j,1,1,iface,iel))=0.d0
            end do

            if(idmo(lx1,1,1,1,iface,iel).eq.0)then
              tmmor(idmo(lx1,1,1,2,iface,iel))=0.d0
            else
              tmmor(idmo(lx1,1,1,1,iface,iel))=0.d0
              do i=1,lx1
                tmmor(idmo(i,j,1,2,iface,iel))=0.d0
              end do
            end if

            i=lx1
            if(idmo(lx1,2,1,2,iface,iel).eq.0)then
              do j=2,lx1-1
                tmmor(idmo(i,j,1,1,iface,iel))=0.d0
              end do
              tmmor(idmo(lx1,lx1,2,2,iface,iel))=0.d0
            else
              do j=2,lx1
                tmmor(idmo(i,j,1,2,iface,iel))=0.d0
              end do
              do j=1,lx1
                tmmor(idmo(i,j,2,2,iface,iel))=0.d0
              end do
            end if
            
            j=lx1
            tmmor(idmo(1,lx1,2,1,iface,iel))=0.d0
            if(idmo(2,lx1,2,1,iface,iel).eq.0)then
              do i=2,lx1-1
                tmmor(idmo(i,j,1,1,iface,iel))=0.d0
              end do
            else
              do i=2,lx1
                tmmor(idmo(i,j,2,1,iface,iel))=0.d0
              end do
              do i=1,lx1-1
                tmmor(idmo(i,j,2,2,iface,iel))=0.d0
              end do
            end if

            i=1
            do j=2,lx1-1
             tmmor(idmo(i,j,1,1,iface,iel))=0.d0
            end do
            if(idmo(1,lx1,1,1,iface,iel).ne.0)then
              tmmor(idmo(i,lx1,1,1,iface,iel))=0.d0
              do j=1,lx1-1
               tmmor(idmo(i,j,2,1,iface,iel))=0.d0
              end do
            end if

          endif
        end do
       end do
!$OMP END DO nowait
            
!$OMP END PARALLEL
      return
      end 
    
