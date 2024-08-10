!---------------------------------------------------------
      subroutine convect(ifmortar)  
!---------------------------------------------------------
!     Advance the convection term using 4th order RK
!     1.ta1 is solution from last time step 
!     2.the heat source is considered part of d/dx
!     3.trhs is right hand side for the diffusion equation
!     4.tmor is solution on mortar points, which will be used
!       as the initial guess when advancing the diffusion term 
!---------------------------------------------------------

      use ua_data
      implicit none

      double precision alpha2, tempa(lx1,lx1,lx1),  &
     &       rdtime, pidivalpha, sixth,  &
     &       dtx1, dtx2, dtx3, src, rk1(lx1,lx1,lx1), rk2(lx1,lx1,lx1),  &
     &       rk3(lx1,lx1,lx1), rk4(lx1,lx1,lx1), temp(lx1,lx1,lx1),  &
     &       subtime(3), xx0(3), yy0(3), zz0(3), dtime2, r2, sum,  &
     &       xloc(lx1), yloc(lx1), zloc(lx1)
      integer k,iel,i,j,iside,isize, substep, ip
      logical ifmortar
      parameter (sixth=1.d0/6.d0)

      if (timeron) call timer_start(t_convect)
      pidivalpha = dacos(-1.d0)/alpha
      alpha2     = alpha*alpha
      dtime2     = dtime/2.d0 
      rdtime     = 1.d0/dtime
      subtime(1) = time
      subtime(2) = time+dtime2
      subtime(3) = time+dtime
      do substep = 1, 3
        xx0(substep) = x00+velx*subtime(substep)
        yy0(substep) = y00+vely*subtime(substep)
        zz0(substep) = z00+velz*subtime(substep)
      end do

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(rk4,rk3,rk2,temp,rk1,dtx3,  &
!$OMP& dtx2,dtx1,iside,ip,sum,src,r2,i,j,k,isize,iel,tempa,  &
!$OMP& xloc,yloc,zloc)

      do iel = 1, nelt
        isize=size_e(iel)
!.......xloc(i) is the location of i'th collocation in x direction in an element.
!       yloc(i) is the location of j'th collocation in y direction in an element.
!       zloc(i) is the location of k'th collocation in z direction in an element.
        do i = 1, lx1
          xloc(i) = xfrac(i)*(xc(2,iel)-xc(1,iel))+xc(1,iel)
        end do
        do j = 1, lx1
          yloc(j) = xfrac(j)*(yc(4,iel)-yc(1,iel))+yc(1,iel)
        end do
        do k = 1, lx1
          zloc(k) = xfrac(k)*(zc(5,iel)-zc(1,iel))+zc(1,iel)
        end do

        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              r2 = (xloc(i)-xx0(1))**2+(yloc(j)-yy0(1))**2+  &
     &             (zloc(k)-zz0(1))**2
              if (r2.le.alpha2) then
                src = dcos(dsqrt(r2)*pidivalpha)+1.d0
              else
                src = 0.d0
              endif
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(i,ip) * ta1(ip,j,k,iel)
              end do
              dtx1 = -velx*sum*xrm1_s(i,j,k,isize)
              sum  = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(j,ip) * ta1(i,ip,k,iel)
              end do
              dtx2=-vely*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(k,ip) * ta1(i,j,ip,iel)
              end do
              dtx3=-velz*sum*xrm1_s(i,j,k,isize)

              rk1(i,j,k)= dtx1 + dtx2 + dtx3 + src
              temp(i,j,k)=ta1(i,j,k,iel)+dtime2*rk1(i,j,k)

            end do
          end do
        end do        

        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              r2 = (xloc(i)-xx0(2))**2 + (yloc(j)-yy0(2))**2 +  &
     &             (zloc(k)-zz0(2))**2
              if (r2.le.alpha2) then
                src = dcos(dsqrt(r2)*pidivalpha)+1.d0
              else
                src = 0.d0
              endif
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(i,ip) * temp(ip,j,k)
              end do
              dtx1 =-velx*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(j,ip) * temp(i,ip,k)
              end do
              dtx2 =-vely*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(k,ip) * temp(i,j,ip)
              end do
              dtx3 =-velz*sum*xrm1_s(i,j,k,isize)

              rk2(i,j,k)= dtx1 + dtx2 + dtx3 + src
              tempa(i,j,k)=ta1(i,j,k,iel)+dtime2*rk2(i,j,k)
            end do
          end do
        end do        

        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              r2 = (xloc(i)-xx0(2))**2 + (yloc(j)-yy0(2))**2 +  &
     &             (zloc(k)-zz0(2))**2
              if (r2.le.alpha2) then
                src = dcos(dsqrt(r2)*pidivalpha)+1.d0
              else
                src = 0.d0
              endif
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(i,ip) * tempa(ip,j,k)
              end do
              dtx1 =-velx*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(j,ip) * tempa(i,ip,k)
              end do
              dtx2 =-vely*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(k,ip) * tempa(i,j,ip)
              end do
              dtx3 =-velz*sum*xrm1_s(i,j,k,isize)

              rk3(i,j,k)= dtx1 + dtx2 + dtx3 + src
              temp(i,j,k)=ta1(i,j,k,iel)+dtime*rk3(i,j,k)
            end do
          end do
        end do        

        do k = 1, lx1
          do j = 1, lx1
            do i = 1, lx1
              r2 = (xloc(i)-xx0(3))**2 + (yloc(j)-yy0(3))**2 +  &
     &             (zloc(k)-zz0(3))**2
              if (r2.le.alpha2) then
                src = dcos(dsqrt(r2)*pidivalpha)+1.d0
              else
                src = 0.d0
              endif
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(i,ip) * temp(ip,j,k)
              end do
              dtx1 =-velx*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(j,ip) * temp(i,ip,k)
              end do
              dtx2 =-vely*sum*xrm1_s(i,j,k,isize)
              sum = 0.d0
              do ip = 1, lx1
                sum = sum + dxm1(k,ip) * temp(i,j,ip)
              end do
              dtx3 =-velz*sum*xrm1_s(i,j,k,isize)

              rk4(i,j,k)= dtx1 + dtx2 + dtx3 + src
              tempa(i,j,k)=sixth*(rk1(i,j,k)+2.d0*  &
     &                   rk2(i,j,k)+2.d0*rk3(i,j,k)+rk4(i,j,k))
            end do
          end do
        end do        

!.......apply boundary condition
        do iside=1,nsides
          if(cbc(iside,iel).eq.0)then
            call facev(tempa,iside,0.0d0)
          end if
        end do
          
        do k=1,lx1
          do j=1,lx1
            do i=1,lx1
              trhs(i,j,k,iel)=bm1_s(i,j,k,isize)*(ta1(i,j,k,iel)*rdtime+  &
     &                        tempa(i,j,k))
              ta1(i,j,k,iel)=ta1(i,j,k,iel)+tempa(i,j,k)*dtime
            end do
          end do
        end do

      end do 
!$OMP END PARALLEL DO

!.....get mortar for intial guess for CG

      if (timeron) call timer_start(t_transfb_c)
      if(ifmortar)then
        call transfb_c_2(ta1)
      else
        call transfb_c(ta1)
      end if
      if (timeron) call timer_stop(t_transfb_c)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,nmor
       tmort(i)=tmort(i)/mormult(i)
      end do
!$OMP END PARALLEL DO
      if (timeron) call timer_stop(t_convect)

      return
      end

