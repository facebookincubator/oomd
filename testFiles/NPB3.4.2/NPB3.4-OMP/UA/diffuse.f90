!---------------------------------------------------------------------
      subroutine diffusion(ifmortar)      
!---------------------------------------------------------------------
!     advance the diffusion term using CG iterations
!---------------------------------------------------------------------

      use ua_data
      implicit none

      double precision  rho_aux, rho1, rho2, beta, cona
      logical ifmortar
      integer iter,ie, im,iside,i,j,k

      if (timeron) call timer_start(t_diffusion)
!.....set up diagonal preconditioner
      if (ifmortar) then
        call setuppc
        call setpcmo
      end if

!.....arrays t and umor are accumlators of (am pm) in the CG algorithm
!     (see the specification)

      call r_init_omp(t,ntot,0.d0)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,nmor
        umor(i)=0.d0
      end do
!$OMP END PARALLEL DO

!.....calculate initial am (see specification) in CG algorithm

!.....trhs and rmor are combined to generate r0 in CG algorithm.
!     pdiff and pmorx are combined to generate q0 in the CG algorithm.
!     rho1 is  (qm,rm) in the CG algorithm.

      rho1 = 0.d0
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(im,ie,i,j,k) REDUCTION(+:rho1)
!$OMP DO
       do ie=1,nelt
         do k=1,lx1
           do j=1,lx1
             do i=1,lx1
               pdiff(i,j,k,ie) = dpcelm(i,j,k,ie)*trhs(i,j,k,ie)
               rho1            = rho1 + trhs(i,j,k,ie)*pdiff(i,j,k,ie)*  &
     &                                          tmult(i,j,k,ie)
             end do
           end do
         end do
       end do
!$OMP END DO nowait

!$OMP DO
      do im = 1, nmor
        pmorx(im) = dpcmor(im)*rmor(im)
        rho1      = rho1 + rmor(im)*pmorx(im)
      end do
!$OMP END DO nowait
!$OMP END PARALLEL

!.................................................................
!     commence conjugate gradient iteration
!.................................................................

      do iter=1, nmxh
        if(iter.gt.1) then 
          rho_aux = 0.d0
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(im,ie,i,j,k) REDUCTION(+:rho_aux)
!$OMP DO
!.........pdiffp and ppmor are combined to generate q_m+1 in the specification
!         rho_aux is (q_m+1,r_m+1)
          do ie = 1, nelt
            do k=1,lx1
              do j=1,lx1
                do i=1,lx1
                  pdiffp(i,j,k,ie) = dpcelm(i,j,k,ie)*trhs(i,j,k,ie)
                  rho_aux =rho_aux+trhs(i,j,k,ie)*pdiffp(i,j,k,ie)*  &
     &                                            tmult(i,j,k,ie)
                end do
              end do
            end do
          end do
!$OMP END DO nowait
!$OMP DO
          do im = 1, nmor
            ppmor(im) = dpcmor(im)*rmor(im)
            rho_aux = rho_aux + rmor(im)*ppmor(im)
          end do
!$OMP END DO nowait
!$OMP END PARALLEL

!.........compute bm (beta) in the specification
          rho2 = rho1
          rho1 = rho_aux
          beta = rho1/rho2
!.........update p_m+1 in the specification
          call adds1m1(pdiff, pdiffp, beta,ntot)
          call adds1m1(pmorx, ppmor,  beta, nmor)  
        end if
 
!.......compute matrix vector product: (theta pm) in the specification

        if (timeron) call timer_start(t_transf)
        call transf(pmorx,pdiff) 
        if (timeron) call timer_stop(t_transf)

!.......compute pdiffp which is (A theta pm) in the specification
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ie) 
        do ie=1, nelt
          call laplacian(pdiffp(1,1,1,ie),pdiff(1,1,1,ie),size_e(ie))
        end do
!$OMP END PARALLEL DO

!.......compute ppmor which will be used to compute (thetaT A theta pm) 
!       in the specification
        if (timeron) call timer_start(t_transfb)
        call transfb(ppmor,pdiffp) 
        if (timeron) call timer_stop(t_transfb)
 
!.......apply boundary condition
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ie,iside)
        do ie=1,nelt
          do iside=1,nsides
            if(cbc(iside,ie).eq.0)then
              call facev(pdiffp(1,1,1,ie),iside,0.d0)
            end if
          end do
        end do
!$OMP END PARALLEL DO

!.......compute cona which is (pm,theta T A theta pm)
        cona = 0.d0
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(im,ie,i,j,k) REDUCTION(+:cona)
!$OMP DO
        do ie = 1, nelt
          do k=1,lx1
            do j=1,lx1
              do i=1,lx1
                cona = cona +  &
     &          pdiff(i,j,k,ie)*pdiffp(i,j,k,ie)*tmult(i,j,k,ie)
              end do 
             end do 
          end do 
        end do 
!$OMP END DO nowait
!$OMP DO
        do im = 1, nmor
          ppmor(im) = ppmor(im)*tmmor(im)
          cona = cona + pmorx(im)*ppmor(im)
        end do
!$OMP END DO nowait
!$OMP END PARALLEL

!.......compute am
        cona = rho1/cona
!.......compute (am pm)
        call adds2m1(t,    pdiff,   cona, ntot)
        call adds2m1(umor, pmorx,   cona, nmor) 
!.......compute r_m+1
        call adds2m1(trhs, pdiffp, -cona, ntot)
        call adds2m1(rmor, ppmor,  -cona, nmor) 
 
      end do

      if (timeron) call timer_start(t_transf)
      call transf(umor,t)  
      if (timeron) call timer_stop(t_transf)
      if (timeron) call timer_stop(t_diffusion)

      return
      end


!------------------------------------------------------------------
      subroutine laplacian(r,u,sizei)
!------------------------------------------------------------------
!     compute  r = visc*[A]x +[B]x on a given element.
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision r(lx1,lx1,lx1), u(lx1,lx1,lx1), rdtime
      integer i,j,k, ix,iz, sizei

      double precision tm1(lx1,lx1,lx1),tm2(lx1,lx1,lx1)                     

      rdtime = 1.d0/dtime

      call r_init(tm1,nxyz,0.d0)
      do iz=1,lx1                     
        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              tm1(i,j,iz) = tm1(i,j,iz)+wdtdr(i,k)*u(k,j,iz)
            end do
          end do
        end do                           
      end do
              
      call r_init(tm2,nxyz,0.d0)                                                   
      do iz=1,lx1                                            
        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              tm2(i,j,iz) = tm2(i,j,iz)+u(i,k,iz)*wdtdr(k,j)
            end do
          end do
        end do
      end do
                                                            
      call r_init(r,nxyz,0.d0)   
      do k = 1, lx1
        do iz=1, lx1    
          do j = 1, lx1
            do i = 1, lx1
              r(i,j,iz) = r(i,j,iz)+u(i,j,k)*wdtdr(k,iz)
            end do
          end do
        end do
      end do

!.....collocate with remaining weights and sum to complete factorization.                   
                                                      
!      do ix=1,nxyz                                            
!         r(ix,1,1)=visc*(tm1(ix,1,1)*g4m1_s(ix,1,1,sizei)+
!     &                   tm2(ix,1,1)*g5m1_s(ix,1,1,sizei)+
!     &                     r(ix,1,1)*g6m1_s(ix,1,1,sizei))+
!     &               bm1_s(ix,1,1,sizei)*rdtime*u(ix,1,1)             
!      end do
      do k=1,lx1
        do j=1,lx1
          do i=1,lx1
            r(i,j,k)=visc*(tm1(i,j,k)*g4m1_s(i,j,k,sizei)+  &
     &                   tm2(i,j,k)*g5m1_s(i,j,k,sizei)+  &
     &                    r(i,j,k)*g6m1_s(i,j,k,sizei))+  &
     &               bm1_s(i,j,k,sizei)*rdtime*u(i,j,k)             
          end do
        end do
      end do

      return                                                  
      end                                                    


 
