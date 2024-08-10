!------------------------------------------------------------------
      subroutine init_locks
!------------------------------------------------------------------
!     This version uses ATOMIC for atomic updates, 
!     but locks are still used in get_emo (mason.f).
!------------------------------------------------------------------

      use ua_data
      implicit none

      integer i

!.....initialize locks in parallel
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
!$    do i=1,8*lelt
!$      call omp_init_lock(tlock(i))
!$    end do

      return
      end


!------------------------------------------------------------------
      subroutine transf(tmor,tx)
!------------------------------------------------------------------
!     Map values from mortar(tmor) to element(tx)
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tmor(*),tx(*), tmp(lx1,lx1,2)
      integer ig1,ig2,ig3,ig4,ie,iface,il1,il2,il3,il4,  &
     &        nnje,ije1,ije2,col,i,j,ig,il


!.....zero out tx on element boundaries
      call col2(tx,tmult,ntot)     

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(il,j,ig,i,col,ije2,ije1,ig4,  &
!$OMP& ig3,ig2,ig1,nnje,il4,il3,il2,il1,iface,ie,tmp)
      do ie=1,nelt
        do iface=1,nsides

!.........get the collocation point index of the four local corners on the
!         face iface of element ie
          il1=idel(1,1,iface,ie)
          il2=idel(lx1,1,iface,ie)
          il3=idel(1,lx1,iface,ie)
          il4=idel(lx1,lx1,iface,ie)

!.........get the mortar indices of the four local corners
          ig1= idmo(1,  1  ,1,1,iface,ie)
          ig2= idmo(lx1,1  ,1,2,iface,ie)
          ig3= idmo(1,  lx1,2,1,iface,ie)
          ig4= idmo(lx1,lx1,2,2,iface,ie)
  
!.........copy the value from tmor to tx for these four local corners
          tx(il1) = tmor(ig1)
          tx(il2) = tmor(ig2)
          tx(il3) = tmor(ig3)
          tx(il4) = tmor(ig4)
 
!.........nnje=1 for conforming faces, nnje=2 for nonconforming faces
          if(cbc(iface,ie).eq.3) then
            nnje=2
          else
            nnje=1 
          end if

!.........for nonconforming faces
          if(nnje.eq.2) then

!...........nonconforming faces have four pieces of mortar, first map them to
!           two intermediate mortars, stored in tmp
            call r_init(tmp,lx1*lx1*2,0.d0)
   
            do ije1=1,nnje
              do ije2=1,nnje
                do col=1,lx1

!.................in each row col, when coloumn i=1 or lx1, the value
!                 in tmor is copied to tmp
                  i = v_end(ije2)
                  ig=idmo(i,col,ije1,ije2,iface,ie)
                  tmp(i,col,ije1)=tmor(ig)

!.................in each row col, value in the interior three collocation
!                 points is computed by apply mapping matrix qbnew to tmor
                  do i=2,lx1-1
                    il= idel(i,col,iface,ie)
                    do j=1,lx1
                      ig=idmo(j,col,ije1,ije2,iface,ie)
                      tmp(i,col,ije1) = tmp(i,col,ije1) +  &
     &                qbnew(i-1,j,ije2)*tmor(ig)
                    end do
                  end do

                end do
              end do
            end do
      
!...........mapping from two pieces of intermediate mortar tmp to element 
!           face tx

            do ije1=1, nnje

!.............the first column, col=1, is an edge of face iface.
!             the value on the three interior collocation points, tx, is 
!             computed by applying mapping matrices qbnew to tmp.
!             the mapping result is divided by 2, because there will be 
!             duplicated contribution from another face sharing this edge.
              col=1
              do i=2,lx1-1
                il= idel(col,i,iface,ie)
                do j=1,lx1
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)*  &
     &                       tmp(col,j,ije1)*0.5d0
                end do 
              end do 

!.............for column 2 ~ lx-1 
              do col=2,lx1-1

!...............when i=1 or lx1, the collocation points are also on an edge of
!               the face, so the mapping result also needs to be divided by 2
                i = v_end(ije1)
                il= idel(col,i,iface,ie)
                tx(il)=tx(il)+tmp(col,i,ije1)*0.5d0

!...............compute the value at interior collocation points in 
!               columns 2 ~ lx1
                do i=2,lx1-1
                  il= idel(col,i,iface,ie)
                  do j=1,lx1
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)* tmp(col,j,ije1)
                  end do 
                end do
              end do

!.............same as col=1
              col=lx1
              do  i=2,lx1-1
                il= idel(col,i,iface,ie)
                do j=1,lx1
                  tx(il) = tx(il) + qbnew(i-1,j,ije1)*  &
     &                     tmp(col,j,ije1)*0.5d0
                end do 
              end do
            end do

!.........for conforming faces
          else

!.........face interior
            do col=2,lx1-1
              do i=2,lx1-1  
                il= idel(i,col,iface,ie)
                ig= idmo(i,col,1,1,iface,ie)
                tx(il)=tmor(ig)
              end do
            end do

        
!...........edges of conforming faces

!...........if local edge 1 is a nonconforming edge
            if(idmo(lx1,1,1,1,iface,ie).ne.0)then
              do i=2,lx1-1               
                il= idel(i,1,iface,ie)
                do ije1=1,2
                  do j=1,lx1
                    ig=idmo(j,1,1,ije1,iface,ie)
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)*tmor(ig)*0.5d0
                  end do
                end do
              end do

!...........if local edge 1 is a conforming edge
            else
              do i=2,lx1-1
                il= idel(i,1,iface,ie)
                ig= idmo(i,1,1,1,iface,ie)
                tx(il)=tmor(ig)
              end do
            end if 

!...........if local edge 2 is a nonconforming edge
            if(idmo(lx1,2,1,2,iface,ie).ne.0)then
              do i=2,lx1-1               
                il= idel(lx1,i,iface,ie)
                do ije1=1,2
                  do j=1,lx1
                    ig=idmo(lx1,j,ije1,2,iface,ie)
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)*tmor(ig)*0.5d0
                  end do
                end do
              end do

!...........if local edge 2 is a conforming edge
            else
              do i=2,lx1-1
                il= idel(lx1,i,iface,ie)
                ig= idmo(lx1,i,1,1,iface,ie)
                tx(il)=tmor(ig)
              end do
            end if 

!...........if local edge 3 is a nonconforming edge
            if(idmo(2,lx1,2,1,iface,ie).ne.0)then
              do  i=2,lx1-1               
                il= idel(i,lx1,iface,ie)
                do ije1=1,2
                  do j=1,lx1
                    ig=idmo(j,lx1,2,ije1,iface,ie)
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)*tmor(ig)*0.5d0
                  end do
                end do
              end do

!...........if local edge 3 is a conforming edge
            else
              do i=2,lx1-1
                il= idel(i,lx1,iface,ie)
                ig= idmo(i,lx1,1,1,iface,ie)
                tx(il)=tmor(ig)
              end do
            end if 

!...........if local edge 4 is a nonconforming edge
            if(idmo(1,lx1,1,1,iface,ie).ne.0)then
              do i=2,lx1-1               
                il= idel(1,i,iface,ie)
                do ije1=1,2
                  do j=1,lx1
                    ig=idmo(1,j,ije1,1,iface,ie)
                    tx(il) = tx(il) + qbnew(i-1,j,ije1)*tmor(ig)*0.5d0
                  end do
                end do
              end do
!...........if local edge 4 is a conforming edge
            else
              do i=2,lx1-1
                il= idel(1,i,iface,ie)
                ig= idmo(1,i,1,1,iface,ie)
                tx(il)=tmor(ig)
              end do
            end if 
          end if
          
        end do
      end do
!$OMP END PARALLEL DO

      return
      end


!------------------------------------------------------------------
      subroutine transfb(tmor,tx)
!------------------------------------------------------------------
!     Map from element(tx) to mortar(tmor).
!     tmor sums contributions from all elements.
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision third
      parameter (third=1.d0/3.d0)
      integer shift

      double precision tmp,tmp1,tx(*),tmor(*),temp(lx1,lx1,2),  &
     &                 top(lx1,2)
      integer il1,il2,il3,il4,ig1,ig2,ig3,ig4,ie,iface,nnje,  &
     &        ije1,ije2,col,i,j,ije,ig,il

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(il,j,ig,i,col,ije2,ije1,ig4,  &
!$OMP& ig3,ig2,ig1,nnje,il4,il3,il2,il1,iface,ie,ije,  &
!$OMP& tmp,shift,temp,top,tmp1)

!$OMP DO
      do ie=1,nmor
        tmor(ie)=0.d0
      end do
!$OMP END DO

!$OMP DO
      do ie=1,nelt
        do iface=1,nsides
!.........nnje=1 for conforming faces, nnje=2 for nonconforming faces
          if(cbc(iface,ie).eq.3) then
            nnje=2
          else
            nnje=1 
          end if

!.........get collocation point index of four local corners on the face
          il1 = idel(1,  1,  iface,ie)
          il2 = idel(lx1,1,  iface,ie)
          il3 = idel(1,  lx1,iface,ie)
          il4 = idel(lx1,lx1,iface,ie)

!.........get the mortar indices of the four local corners
          ig1 = idmo(1,  1,  1,1,iface,ie)
          ig2 = idmo(lx1,1,  1,2,iface,ie)
          ig3 = idmo(1,  lx1,2,1,iface,ie )
          ig4 = idmo(lx1,lx1,2,2,iface,ie)

!.........sum the values from tx to tmor for these four local corners
!         only 1/3 of the value is summed, since there will be two duplicated
!         contributions from the other two faces sharing this vertex 
!$OMP ATOMIC
          tmor(ig1) = tmor(ig1)+tx(il1)*third
!$OMP ATOMIC
          tmor(ig2) = tmor(ig2)+tx(il2)*third
!$OMP ATOMIC
          tmor(ig3) = tmor(ig3)+tx(il3)*third
!$OMP ATOMIC
          tmor(ig4) = tmor(ig4)+tx(il4)*third

!.........for nonconforming faces
          if(nnje.eq.2) then       
            call r_init(temp,lx1*lx1*2,0.d0)

!...........nonconforming faces have four pieces of mortar, first map tx to
!           two intermediate mortars stored in temp

            do ije2 = 1, nnje
              shift = ije2-1
              do col=1,lx1
!...............For mortar points on face edge (top and bottom), copy the 
!               value from tx to temp
                il=idel(col,v_end(ije2),iface,ie)
                temp(col,v_end(ije2),ije2)=tx(il)

!...............For mortar points on face edge (top and bottom), calculate 
!               the interior points' contribution to them, i.e. top()
                j = v_end(ije2)
                tmp=0.d0
                do i=2,lx1-1 
                  il=idel(col,i,iface,ie)
                  tmp = tmp + qbnew(i-1,j,ije2)*tx(il)
                end do

                top(col,ije2)=tmp

!...............Use mapping matrices qbnew to map the value from tx to temp 
!               for mortar points not on the top bottom face edge.
                do j=2-shift,lx1-shift
                  tmp=0.d0
                  do i=2,lx1-1 
                    il=idel(col,i,iface,ie)
                    tmp = tmp + qbnew(i-1,j,ije2)*tx(il)
                  end do
                  temp(col,j,ije2) = tmp + temp(col,j,ije2)
                end do
              end do
            end do

!...........mapping from temp to tmor

            do ije1=1, nnje
              shift = ije1-1
              do ije2=1,nnje

!...............for each column of collocation points on a piece of mortar
                do col=2-shift,lx1-shift

!.................For the end point, which is on an edge (local edge 2,4), 
!                 the contribution is halved since there will be duplicated 
!                 contribution from another face sharing this edge.

                  ig=idmo(v_end(ije2),col,ije1,ije2,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+temp(v_end(ije2),col,ije1)*0.5d0

!.................In each row of collocation points on a piece of mortar, 
!                 sum the contributions from interior collocation points 
!                 (i=2,lx1-1)

                  do  j=1,lx1
                    tmp=0.d0
                    do i=2,lx1-1
                      tmp = tmp + qbnew(i-1,j,ije2) * temp(i,col,ije1)
                    end do
                    ig=idmo(j,col,ije1,ije2,iface,ie)
!$OMP ATOMIC
                    tmor(ig)=tmor(ig)+tmp
                  end do
                end do

!...............For tmor on local edge 1 and 3, tmp is the contribution from
!               an edge, so it is halved because of duplicated contribution
!               from another face sharing this edge. tmp1 is contribution 
!               from face interior. 

                col = v_end(ije1)
                ig=idmo(v_end(ije2),col,ije1,ije2,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+top(v_end(ije2),ije1)*0.5d0
                do  j=1,lx1
                  tmp=0.d0
                  tmp1=0.d0
                  do i=2,lx1-1
                    tmp  = tmp  + qbnew(i-1,j,ije2) * temp(i,col,ije1)
                    tmp1 = tmp1 + qbnew(i-1,j,ije2) * top(i,ije1)
                  end do
                  ig=idmo(j,col,ije1,ije2,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+tmp*0.5d0+tmp1 
                end do
              end do
            end do

!.........for conforming faces
          else

!.........face interior
            do col=2,lx1-1
              do j=2,lx1-1
                il=idel(j,col,iface,ie)
                ig=idmo(j,col,1,1,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+tx(il)
              end do
            end do

!...........edges of conforming faces

!...........if local edge 1 is a nonconforming edge
            if(idmo(lx1,1,1,1,iface,ie).ne.0)then
              do ije=1,2
                do j=1,lx1
                  tmp=0.d0
                  do i=2,lx1-1
                    il=idel(i,1,iface,ie)
                    tmp= tmp + qbnew(i-1,j,ije)*tx(il)
                  end do
                  ig=idmo(j,1,1,ije,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+tmp*0.5d0
                end do
              end do

!...........if local edge 1 is a conforming edge
            else
              do j=2,lx1-1
                il=idel(j,1,iface,ie)
                ig=idmo(j,1,1,1,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+tx(il)*0.5d0
              end do
            end if 

!...........if local edge 2 is a nonconforming edge
            if(idmo(lx1,2,1,2,iface,ie).ne.0)then
              do ije=1,2
                do j=1,lx1
                  tmp=0.d0
                  do i=2,lx1-1
                    il=idel(lx1,i,iface,ie)
                    tmp = tmp + qbnew(i-1,j,ije)*tx(il)
                  end do
                  ig=idmo(lx1,j,ije,2,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+tmp*0.5d0
                end do
              end do

!...........if local edge 2 is a conforming edge
            else
              do j=2,lx1-1
                il=idel(lx1,j,iface,ie)
                ig=idmo(lx1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+tx(il)*0.5d0
              end do
            end if 

!...........if local edge 3 is a nonconforming edge
            if(idmo(2,lx1,2,1,iface,ie).ne.0)then
              do ije=1,2
                do j=1,lx1
                  tmp=0.d0
                  do i=2,lx1-1
                    il=idel(i,lx1,iface,ie)
                    tmp = tmp + qbnew(i-1,j,ije)*tx(il)
                  end do
                  ig=idmo(j,lx1,2,ije,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+tmp*0.5d0
                end do
              end do

!...........if local edge 3 is a conforming edge
            else
              do j=2,lx1-1
                il=idel(j,lx1,iface,ie)
                ig=idmo(j,lx1,1,1,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+tx(il)*0.5d0
              end do
            end if 

!...........if local edge 4 is a nonconforming edge
            if(idmo(1,lx1,1,1,iface,ie).ne.0)then
              do ije=1,2
                do j=1,lx1
                  tmp=0.d0
                  do i=2,lx1-1
                    il=idel(1,i,iface,ie)
                    tmp = tmp + qbnew(i-1,j,ije)*tx(il)
                  end do
                  ig=idmo(1,j,ije,1,iface,ie)
!$OMP ATOMIC
                  tmor(ig)=tmor(ig)+tmp*0.5d0
                end do
              end do

!...........if local edge 4 is a conforming edge
            else
              do j=2,lx1-1
                il=idel(1,j,iface,ie)
                ig=idmo(1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmor(ig)=tmor(ig)+tx(il)*0.5d0
              end do
            end if 
          end if
        end do
      end do
!$OMP END DO NOWAIT
!$OMP END PARALLEL 

      return
      end


!--------------------------------------------------------------
      subroutine transfb_cor_e(n,tmor,tx)
!--------------------------------------------------------------
!     This subroutine performs the edge to mortar mapping and
!     calculates the mapping result on the mortar point at a vertex
!     under situation 1,2, or 3.
!     n refers to the configuration of three edges sharing a vertex, 
!     n = 1: only one edge is nonconforming
!     n = 2: two edges are nonconforming 
!     n = 3: three edges are nonconforming 
!-------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tmor,tx(lx1,lx1,lx1),tmp
      integer i,n

      tmor=tx(1,1,1)

      do i=2,lx1-1
        tmor= tmor + qbnew(i-1,1,1)*tx(i,1,1)
      end do

      if(n.gt.1)then
        do i=2,lx1-1
          tmor= tmor + qbnew(i-1,1,1)*tx(1,i,1)
        end do
      end if

      if(n.eq.3)then
        do i=2,lx1-1
          tmor= tmor + qbnew(i-1,1,1)*tx(1,1,i)
        end do
      end if

      return
      end

!--------------------------------------------------------------
      subroutine transfb_cor_f(n,tmor,tx)
!--------------------------------------------------------------
!     This subroutine performs the mapping from face to mortar.
!     Output tmor is the mapping result on a mortar vertex
!     of situations of three edges and three faces sharing a vertex:
!     n=4: only one face is nonconforming 
!     n=5: one face and one edge are nonconforming
!     n=6: two faces are nonconforming 
!     n=7: three faces are nonconforming 
!--------------------------------------------------------------

      use ua_data
      implicit none

      double precision tx(lx1,lx1,lx1),tmor,temp(lx1)
      integer col,i,n

      call r_init(temp,lx1,0.d0)

      do col=1,lx1
        temp(col)=tx(col,1,1)
        do i=2,lx1-1
          temp(col) = temp(col) + qbnew(i-1,1,1)*tx(col,i,1)
        end do
      end do
      tmor=temp(1)

      do i=2,lx1-1
        tmor = tmor + qbnew(i-1,1,1) *temp(i)
      end do

      if(n.eq.5)then
        do i=2,lx1-1
          tmor = tmor + qbnew(i-1,1,1) *tx(1,1,i)
        end do
      end if
 
      if(n.ge.6)then
        call r_init(temp,lx1,0.d0)
        do col=1,lx1
          do i=2,lx1-1
            temp(col) = temp(col) + qbnew(i-1,1,1)*tx(col,1,i)
          end do
        end do
        tmor=tmor+temp(1)
        do i=2,lx1-1
          tmor = tmor +qbnew(i-1,1,1) *temp(i)
        end do
      end if
        
      if(n.eq.7)then
        call r_init(temp,lx1,0.d0)
        do col=2,lx1-1
          do i=2,lx1-1
            temp(col) = temp(col) + qbnew(i-1,1,1)*tx(1,col,i)
          end do
        end do
        do i=2,lx1-1
          tmor = tmor + qbnew(i-1,1,1) *temp(i)
        end do
      end if

      return
      end


!-------------------------------------------------------------------------
      subroutine transf_nc(tmor,tx)
!------------------------------------------------------------------------
!     Perform mortar to element mapping on a nonconforming face. 
!     This subroutin is used when all entries in tmor are zero except
!     one tmor(i,j)=1. So this routine is simplified. Only one piece of 
!     mortar  (tmor only has two indices) and one piece of intermediate 
!     mortar (tmp) are involved.
!------------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tmor(lx1,lx1), tx(lx1,lx1), tmp(lx1,lx1)
      integer col,i,j

      call r_init(tmp,lx1*lx1,0.d0)
      do col=1,lx1
        i = 1
        tmp(i,col)=tmor(i,col)                           
        do i=2,lx1-1
          do j=1,lx1
            tmp(i,col) = tmp(i,col) + qbnew(i-1,j,1)*tmor(j,col)
          end do
        end do
      end do

      do col=1,lx1
        i = 1
        tx(col,i)   = tx(col,i)   + tmp(col,i)
        do i=2,lx1-1
          do j=1,lx1
            tx(col,i) = tx(col,i) + qbnew(i-1,j,1)*tmp(col,j)
          end do
        end do
      end do

      return                                                  
      end                                                     

!------------------------------------------------------------------------
      subroutine transfb_nc0(tmor,tx)
!------------------------------------------------------------------------
!     Performs mapping from element to mortar when the nonconforming 
!     edges are shared by two conforming faces of an element.
!------------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tmor(lx1,lx1),tx(lx1,lx1,lx1)
      integer i,j

      call r_init(tmor,lx1*lx1,0.d0)
      do j=1,lx1
        do i=2,lx1-1
          tmor(j,1)= tmor(j,1) + qbnew(i-1,j  ,1)*tx(i,1,1)
        end do
      end do

      return
      end 

!------------------------------------------------------------------------
      subroutine transfb_nc2(tmor,tx)
!------------------------------------------------------------------------
!     Maps values from element to mortar when the nonconforming edges are
!     shared by two nonconforming faces of an element.
!     Although each face shall have four pieces of mortar, only value in
!     one piece (location (1,1)) is used in the calling routine so only
!     the value in the first mortar is calculated in this subroutine.
!------------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tx(lx1,lx1),tmor(lx1,lx1),bottom(lx1),  &
     &                 temp(lx1,lx1)
      integer col,j,i

      call r_init(tmor,lx1*lx1,0.d0)
      call r_init(temp,lx1*lx1,0.d0)
      tmor(1,1)=tx(1,1)

!.....mapping from tx to intermediate mortar temp + bottom
      do col=1,lx1
        temp(col,1)=tx(col,1)
        j=1
        bottom(col)= 0.d0
        do i=2,lx1-1 
          bottom(col) = bottom(col) + qbnew(i-1,j,1)*tx(col,i)
        end do

        do j=2,lx1
          do i=2,lx1-1 
            temp(col,j) = temp(col,j) + qbnew(i-1,j,1)*tx(col,i)
          end do
        end do
      end do

!.....from intermediate mortar to mortar

!.....On the nonconforming edge, temp is divided by 2 as there will be
!     a duplicate contribution from another face sharing this edge
      col=1
      do j=1,lx1
        do i=2,lx1-1
          tmor(j,col)=tmor(j,col)+ qbnew(i-1,j,1) * bottom(i) +  &
     &                             qbnew(i-1,j,1) * temp(i,col) * 0.5d0 
        end do
      end do

      do col=2,lx1
        tmor(1,col)=tmor(1,col)+temp(1,col)
        do j=1,lx1
          do i=2,lx1-1
            tmor(j,col) = tmor(j,col) + qbnew(i-1,j,1) *temp(i,col)
          end do
        end do
      end do

      return
      end 


!------------------------------------------------------------------------
      subroutine transfb_nc1(tmor,tx)
!------------------------------------------------------------------------
!     Maps values from element to mortar when the nonconforming edges are
!     shared by a nonconforming face and a conforming face of an element
!------------------------------------------------------------------------

      use ua_data
      implicit none

      double precision tx(lx1,lx1),tmor(lx1,lx1),bottom(lx1),  &
     &                 temp(lx1,lx1)
      integer col,j,i

      call r_init(tmor,lx1*lx1,0.d0)
      call r_init(temp,lx1*lx1,0.d0)

      tmor(1,1)=tx(1,1)
!.....Contribution from the nonconforming faces
!     Since the calling subroutine is only interested in the value on the
!     mortar (location (1,1)), only this piece of mortar is calculated.

      do col=1,lx1
        temp(col,1)=tx(col,1)
        j = 1
        bottom(col)= 0.d0
        do i=2,lx1-1 
          bottom(col)=bottom(col) + qbnew(i-1,j,1)*tx(col,i)
        end do

        do j=2,lx1
          do i=2,lx1-1 
            temp(col,j) = temp(col,j) + qbnew(i-1,j,1)*tx(col,i)
          end do

        end do
      end do

      col=1
      tmor(1,col)=tmor(1,col)+bottom(1)
      do j=1,lx1
        do i=2,lx1-1

!.........temp is not divided by 2 here. It includes the contribution
!         from the other conforming face.

          tmor(j,col)=tmor(j,col) + qbnew(i-1,j,1) *bottom(i) +  &
     &                              qbnew(i-1,j,1) *temp(i,col) 
        end do
      end do

      do col=2,lx1
        tmor(1,col)=tmor(1,col)+temp(1,col)
        do j=1,lx1
          do i=2,lx1-1
            tmor(j,col) = tmor(j,col) + qbnew(i-1,j,1) *temp(i,col)
          end do
        end do
      end do

      return
      end


!-------------------------------------------------------------------
      subroutine transfb_c(tx)
!-------------------------------------------------------------------
!     Prepare initial guess for cg. All values from conforming 
!     boundary are copied and summed on tmor.
!-------------------------------------------------------------------

      use ua_data
      implicit none

      double precision third
      parameter (third = 1.d0/3.d0)
      double precision tx(*)
      integer il1,il2,il3,il4,ig1,ig2,ig3,ig4,ie,iface,col,j,ig,il

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IE,IFACE,IL1,IL2,  &
!$OMP& IL3,IL4,IG1,IG2,IG3,IG4,COL,J,IG,IL) 

!$OMP DO
      do j=1,nmor
        tmort(j)=0.d0
      end do
!$OMP END DO

!$OMP DO
      do ie=1,nelt
        do iface=1,nsides
          if(cbc(iface,ie).ne.3)then
            il1 = idel(1,1,iface,ie)
            il2 = idel(lx1,1,iface,ie)
            il3 = idel(1,lx1,iface,ie)
            il4 = idel(lx1,lx1,iface,ie)
            ig1 = idmo(1,  1,  1,1,iface,ie)
            ig2 = idmo(lx1,1,  1,2,iface,ie)
            ig3 = idmo(1,  lx1,2,1,iface,ie)
            ig4 = idmo(lx1,lx1,2,2,iface,ie)

!$OMP ATOMIC
            tmort(ig1) = tmort(ig1)+tx(il1)*third
!$OMP ATOMIC
            tmort(ig2) = tmort(ig2)+tx(il2)*third
!$OMP ATOMIC
            tmort(ig3) = tmort(ig3)+tx(il3)*third
!$OMP ATOMIC
            tmort(ig4) = tmort(ig4)+tx(il4)*third

            do  col=2,lx1-1
              do j=2,lx1-1
                il=idel(j,col,iface,ie)
                ig=idmo(j,col,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)
              end do
            end do

            if(idmo(lx1,1,1,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(j,1,iface,ie)
                ig=idmo(j,1,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
              end do
            end if

            if(idmo(lx1,2,1,2,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(lx1,j,iface,ie)
                ig=idmo(lx1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
              end do
            end if

            if(idmo(2,lx1,2,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(j,lx1,iface,ie)
                ig=idmo(j,lx1,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
              end do
            end if

            if(idmo(1,lx1,1,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(1,j,iface,ie)
                ig=idmo(1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
              end do
            end if
          end if!
        end do
      end do
!$OMP END DO NOWAIT
!$OMP END PARALLEL
      return
      end

!-------------------------------------------------------------------
      subroutine transfb_c_2(tx)
!-------------------------------------------------------------------
!     Prepare initial guess for CG. All values from conforming 
!     boundary are copied and summed in tmort. 
!     mormult is multiplicity, which is used to average tmort.
!-------------------------------------------------------------------

      use ua_data
      implicit none

      double precision third
      parameter (third = 1.d0/3.d0)
      double precision tx(*)
      integer il1,il2,il3,il4,ig1,ig2,ig3,ig4,ie,iface,col,j,ig,il

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(IE,IFACE,IL1,IL2,  &
!$OMP& IL3,IL4,IG1,IG2,IG3,IG4,COL,J,IG,IL)

!$OMP DO     
      do j=1,nmor
        tmort(j)=0.d0
      end do
!$OMP END DO nowait
!$OMP DO
      do j=1,nmor
        mormult(j)=0.d0
      end do
!$OMP END DO

!$OMP DO 
      do ie=1,nelt
        do iface=1,nsides
          
          if(cbc(iface,ie).ne.3)then
            il1 = idel(1,  1,  iface,ie)
            il2 = idel(lx1,1,  iface,ie)
            il3 = idel(1,  lx1,iface,ie)
            il4 = idel(lx1,lx1,iface,ie)
            ig1 = idmo(1,  1,  1,1,iface,ie)
            ig2 = idmo(lx1,1,  1,2,iface,ie)
            ig3 = idmo(1,  lx1,2,1,iface,ie)
            ig4 = idmo(lx1,lx1,2,2,iface,ie)

!$OMP ATOMIC
            tmort(ig1) = tmort(ig1)+tx(il1)*third
!$OMP ATOMIC
            tmort(ig2) = tmort(ig2)+tx(il2)*third
!$OMP ATOMIC
            tmort(ig3) = tmort(ig3)+tx(il3)*third
!$OMP ATOMIC
            tmort(ig4) = tmort(ig4)+tx(il4)*third
!$OMP ATOMIC
            mormult(ig1) = mormult(ig1)+third
!$OMP ATOMIC
            mormult(ig2) = mormult(ig2)+third
!$OMP ATOMIC
            mormult(ig3) = mormult(ig3)+third
!$OMP ATOMIC
            mormult(ig4) = mormult(ig4)+third

            do  col=2,lx1-1
              do j=2,lx1-1
                il=idel(j,col,iface,ie)
                ig=idmo(j,col,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)
!$OMP ATOMIC
                mormult(ig)=mormult(ig)+1.d0
              end do
            end do

            if(idmo(lx1,1,1,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(j,1,iface,ie)
                ig=idmo(j,1,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
!$OMP ATOMIC
                mormult(ig)=mormult(ig)+0.5d0
              end do
            end if

            if(idmo(lx1,2,1,2,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(lx1,j,iface,ie)
                ig=idmo(lx1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
!$OMP ATOMIC
                mormult(ig)=mormult(ig)+0.5d0
              end do
            end if

            if(idmo(2,lx1,2,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(j,lx1,iface,ie)
                ig=idmo(j,lx1,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
!$OMP ATOMIC
                mormult(ig)=mormult(ig)+0.5d0
               end do
            end if

            if(idmo(1,lx1,1,1,iface,ie).eq.0)then
              do j=2,lx1-1
                il=idel(1,j,iface,ie)
                ig=idmo(1,j,1,1,iface,ie)
!$OMP ATOMIC
                tmort(ig)=tmort(ig)+tx(il)*0.5d0
!$OMP ATOMIC
                mormult(ig)=mormult(ig)+0.5d0
              end do
            end if
          end if!nnje=1
        end do
      end do
!$OMP END DO NOWAIT
!$OMP END PARALLEL

      return
      end

