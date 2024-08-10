!---------------------------------------------------------------
      subroutine move
!---------------------------------------------------------------
!     move element to proper location in morton space filling curve
!---------------------------------------------------------------

      use ua_data
      implicit none

      integer i,iside,jface,iel,ntemp,ii1,ii2,n1,n2,cb

      n2=2*6*nelt
      n1=n2*2

      call nr_init_omp(sje_new,n1,0)
      call nr_init_omp(ijel_new,n2,0)

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(iel,i,iside,jface,cb,ntemp,  &
!$OMP& ii1,ii2) 
!$OMP DO
      do iel=1,nelt
        i=mt_to_id(iel)
        treenew(iel)=tree(i)
        call copy(xc_new(1,iel),xc(1,i),8)
        call copy(yc_new(1,iel),yc(1,i),8)
        call copy(zc_new(1,iel),zc(1,i),8)

        do iside=1,nsides
          jface = jjface(iside)
          cb=cbc(iside,i)
          xc_new(iside,iel)=xc(iside,i)
          yc_new(iside,iel)=yc(iside,i)
          zc_new(iside,iel)=zc(iside,i)
          cbc_new(iside,iel)=cb

          if(cb.eq.2)then
            ntemp=sje(1,1,iside,i)
            ijel_new(1,iside,iel)=1
            ijel_new(2,iside,iel)=1
            sje_new(1,1,iside,iel)=id_to_mt(ntemp)

          else if(cb.eq.1) then
            ntemp=sje(1,1,iside,i)
            ijel_new(1,iside,iel)=ijel(1,iside,i)
            ijel_new(2,iside,iel)=ijel(2,iside,i)
            sje_new(1,1,iside,iel)=id_to_mt(ntemp)
         
          else if(cb.eq.3) then
            do ii2=1,2
              do ii1=1,2
                ntemp=sje(ii1,ii2,iside,i)
                ijel_new(1,iside,iel)=1
                ijel_new(2,iside,iel)=1
                sje_new(ii1,ii2,iside,iel)=id_to_mt(ntemp)
              end do
            end do

          else if(cb.eq.0)then
            sje_new(1,1,iside,iel)=0
            sje_new(1,2,iside,iel)=0
            sje_new(2,1,iside,iel)=0
            sje_new(2,2,iside,iel)=0       
          end if 

        end do

        call copy(ta2(1,1,1,iel),ta1(1,1,1,i),nxyz)

      end do
!$OMP ENDDO

!$OMP DO
      do iel=1,nelt
        call copy(xc(1,iel),xc_new(1,iel),8)
        call copy(yc(1,iel),yc_new(1,iel),8)
        call copy(zc(1,iel),zc_new(1,iel),8)
        call copy(ta1(1,1,1,iel),ta2(1,1,1,iel),nxyz)
        call ncopy(sje(1,1,1,iel),sje_new(1,1,1,iel),4*6)
        call ncopy(ijel(1,1,iel),ijel_new(1,1,iel),2*6)
        call ncopy(cbc(1,iel),cbc_new(1,iel),6)
        mt_to_id(iel)=iel
        id_to_mt(iel)=iel
        tree(iel)=treenew(iel)
      end do
!$OMP ENDDO 
!$OMP END PARALLEL

      return
      end 
