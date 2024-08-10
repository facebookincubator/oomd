!-----------------------------------------------------------
      subroutine adaptation (ifmortar,step)
!-----------------------------------------------------------
!     For 3-D mesh adaptation (refinement+ coarsening)
!-----------------------------------------------------------

      use ua_data
      implicit none
      
      logical if_coarsen,if_refine,ifmortar,ifrepeat
      integer iel,miel,irefine,icoarsen,neltold,step

      if (timeron) call timer_start(t_adaptation)
      ifmortar=.false.
!.....compute heat source center(x0,y0,z0)
      x0=x00+velx*time
      y0=y00+vely*time
      z0=z00+velz*time

!.....Search elements to be refined. Check with restrictions. Perform
!     refinement repeatedly until all desired refinements are done.

!.....ich(iel)=0 no grid change on element iel
!.....ich(iel)=2 iel is marked to be coarsened
!.....ich(iel)=4 iel is marked to be refined

!.....irefine records how many elements got refined
      irefine=0

!.....check whether elements need to be refined because they have overlap
!     with the  heat source
4     call find_refine(if_refine)

      if(if_refine) then
        ifrepeat=.true.
2       if(ifrepeat) then
!.........Check with restriction, unmark elements that cannot be refined.
!         Elements preventing desired refinement will be marked to be refined.
          call check_refine(ifrepeat) 
          go to 2
        end if
!.......perform refinement
        call do_refine(ifmortar,irefine)
        goto 4
      endif

!.....Search for elements to be coarsened. Check with restrictions,
!     Perform coarsening repeatedly until all possible coarsening
!     is done.

!.....icoarsen records how many elements got coarsened 
      icoarsen=0

!.....skip(iel)=.true. indicates an element no longer exists (because it
!     got merged)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel)
       do iel=1,nelt
        skip(iel)=.false.
      end do
!$OMP END PARALLEL DO

      neltold=nelt

!.....Check whether elements need to be coarsened because they don't have
!     overlap with the heat source. Only elements that don't have a larger 
!     size neighbor can be marked to be coarsened

5     call find_coarsen(if_coarsen,neltold)

      if(if_coarsen) then
!.......Perform coarsening, however subject to restriction. Only possible 
!       coarsening will be performed. if_coarsen=.true. indicates that
!       actual coarsening happened
        call do_coarsen(if_coarsen,icoarsen,neltold)
        if(if_coarsen) then
!.........ifmortar=.true. indicates the grid changed, i.e. the mortar points 
!         indices need to be regenerated on the new grid.
          ifmortar=.true.
          go to 5
        end if 
      end if

      write(*,1000) step, irefine, icoarsen, nelt
 1000 format('Step ',i4, ': elements refined, merged, total: ',  &
     &       i7, 1X , i7, 1X, i7)

!.....mt_to_id(miel) takes as argument the morton index  and returns the actual 
!                    element index
!.....id_to_mt(iel)  takes as argument the actual element index and returns the 
!                    morton index
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel,iel)
      do miel=1,nelt
        iel=mt_to_id(miel)
        id_to_mt(iel)=miel
      end do 
!$OMP END PARALLEL DO

!.....Reorder the elements in the order of the morton curve. After the move 
!     subroutine the element indices are  the same as the morton indices
      call move

!.....if the grid changed, regenerate mortar indices and update variables
!     associated to grid.
      if (ifmortar) then
        call mortar
        call prepwork
      endif
      if (timeron) call timer_stop(t_adaptation)

      return
      end 


!-----------------------------------------------------------
      subroutine do_coarsen(if_coarsen,icoarsen,neltold)
!---------------------------------------------------------------
!     Coarsening procedure: 
!     1) check with restrictions
!     2) perform coarsening
!---------------------------------------------------------------

      use ua_data
      implicit none

      logical if_coarsen, icheck,test,test1,test2,test3
      integer iel, ntp(8), ntempmin, ic, parent, mielnew, miel,  &
     &        icoarsen, i, index, num_coarsen, ntemp, ii, ntemp1,  &
     &        neltold
      
      if_coarsen=.false.

!.....If an element has been merged, it will be skipped afterwards
!     skip(iel)=.true. for elements that will be skipped.
!     ifcoa_id(iel)=.true. indicates that element iel will be coarsened
!     ifcoa(miel)=.true. refers to element miel(mortar index) will be
!                        coarsened

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(iel)
!$OMP DO 
      do iel=1,nelt
        mt_to_id_old(iel)=mt_to_id(iel)
        mt_to_id(iel)=0
      end do
!$OMP END DO nowait
!$OMP DO 
      do iel=1,neltold 
        ifcoa_id(iel)=.false.
      end do
!$OMP END DO nowait
!$OMP END PARALLEL

!.....Check whether the potential coarsening will make neighbor, 
!     and neighbor's neighbor....break grid restriction

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel,iel,ic,  &
!$OMP& ntp,parent,test,test1,i,test2,test3)  &
!$OMP& SHARED(if_coarsen)
      do miel=1,nelt
        ifcoa(miel)=.false.
        front(miel)=0
        iel=mt_to_id_old(miel)
!.......if an element is marked to be coarsened
        if(ich(iel).eq.2) then

!.........If the current  element is the "first" child (front-left-
!         bottom) of its parent (tree(iel) mod 8 equals 0), then 
!         find all its neighbors. Check whether they are from the same 
!         parent.

          ic=tree(iel)
          if(.not.btest(ic,0).and..not.btest(ic,1).and.  &
     &       .not.btest(ic,2)) then
            ntp(1)=iel
            ntp(2)=sje(1,1,1,iel)
            ntp(3)=sje(1,1,3,iel)
            ntp(4)=sje(1,1,1,ntp(3))
            ntp(5)=sje(1,1,5,iel)
            ntp(6)=sje(1,1,1,ntp(5))
            ntp(7)=sje(1,1,3,ntp(5))
            ntp(8)=sje(1,1,1,ntp(7))
 
            parent=ishft(tree(iel),-3)
            test=.false.

            test1=.true.
            do i=1,8
              if(ishft(tree(ntp(i)),-3).ne.parent)test1=.false.
            end do

!...........check whether all child elements are marked to be coarsened
            if(test1)then
              test2=.true.
              do i=1,8
                if(ich(ntp(i)).ne.2)test2=.false.
              end do

!.............check whether all child elements can be coarsened or not.
              if(test2)then
                test3=.true.
                do i=1,8
                  if(.not.icheck(ntp(i),i))test3=.false.
                end do
                if(test3)test=.true.
              end if
            end if
!...........if the eight child elements are eligible to be coarsened
!           mark the first children ifcoa(miel)=.true.
!           mark them all ifcoa_id()=.true.
!           front(miel) will be used to calculate (potentially in parallel) 
!                       how many elements with seuqnece numbers less than
!                       miel will be coarsened.
!           skip()      marks that an element will no longer exist after merge.

            if(test)then

              ifcoa(miel)=.true.
              do i=1,8
                ifcoa_id(ntp(i))=.true.
              end do
              front(miel)=1
              do i=1,7
                 skip(ntp(i+1))=.true.
              end do
              if(.not.if_coarsen) if_coarsen=.true.
            end if
          end if 
        end if 
      end do 
!$OMP END PARALLEL DO

!.....compute front(iel), how many elements will be coarsened before iel
!     (including iel)
      call parallel_add(front)

!.....num_coarsen is the total number of elements that will be coarsened
      num_coarsen=front(nelt)

!.....action(i) records the morton index of the i'th element (if it is an
!     element's front-left-bottom-child) to be coarsened.

!.....create array mt_to_id to convert actual element index to morton index
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel,iel,mielnew)
      do miel=1,nelt
        iel=mt_to_id_old(miel)
        if(.not.skip(iel))then
          if(ifcoa(miel))then
            action(front(miel))=miel
            mielnew=miel-(front(miel)-1)*7
          else 
            mielnew=miel-front(miel)*7
          end if
          mt_to_id(mielnew)=iel
        end if
      end do
!$OMP END PARALLEL DO

!.....perform the coarsening procedure (potentially in parallel)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(index,miel,iel,ntp)
      do index=1,num_coarsen
        miel=action(index)
        iel=mt_to_id_old(miel)
!.......find eight child elements to be coarsened
        ntp(1)=iel
        ntp(2)=sje(1,1,1,iel)
        ntp(3)=sje(1,1,3,iel)
        ntp(4)=sje(1,1,1,ntp(3))
        ntp(5)=sje(1,1,5,iel)
        ntp(6)=sje(1,1,1,ntp(5))
        ntp(7)=sje(1,1,3,ntp(5))
        ntp(8)=sje(1,1,1,ntp(7))
!.......merge them to be the parent
        call merging(ntp)
      end do
!$OMP END PARALLEL DO
      nelt=nelt-num_coarsen*7
      icoarsen=icoarsen+num_coarsen*8

      return
      end

!-------------------------------------------------------
      subroutine do_refine(ifmortar,irefine)
!-------------------------------------------------------
!     Refinement procedure
!--------------------------------------------------------

      use ua_data
      implicit none

      logical ifmortar
      double precision xctemp(8), yctemp(8), zctemp(8), xleft, xright,  &
     &       yleft, yright, zleft, zright, ta1temp(lx1,lx1,lx1),  &
     &       xhalf, yhalf, zhalf
      integer iel, i, ii, jj, j, jface,  &
     &        ntemp, ndir, facedir, k, le(4), ne(4), mielnew,  &
     &        miel, irefine,ntemp1, num_refine, index, treetemp,  &
     &        sjetemp(2,2,6), n1, n2, nelttemp,  &
     &        cb, cbctemp(6)

!.....initialize

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel)
      do miel=1,nelt
        mt_to_id_old(miel)=mt_to_id(miel)
        mt_to_id(miel)=0
        action(miel)=0
        if(ich(mt_to_id_old(miel)).ne.4)then
          front(miel)=0
        else
          front(miel)=1
        end if
      end do
!$OMP END PARALLEL DO

!.....front(iel) records how many elements with sequence numbers less than
!     or equal to iel will be refined
      call parallel_add(front)

!.....num_refine is the total number of elements that will be refined
      num_refine=front(nelt)

!.....action(i) records the morton index of the  i'th element to be refined
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel,iel)
      do miel=1,nelt
        iel=mt_to_id_old(miel)
        if(ich(iel).eq.4)then
          action(front(miel))=miel
        end if
      end do
!$OMP END PARALLEL DO

!.....Compute array mt_to_id to convert the element index to morton index.
!     ref_front_id(iel) records how many elements with index less than
!     iel (actual element index, not morton index), will be refined.
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(miel,iel,ntemp,mielnew)
      do miel=1,nelt
        iel=mt_to_id_old(miel)
        if(ich(iel).eq.4)then
          ntemp=(front(miel)-1)*7
          mielnew=miel+ntemp
        else
          ntemp=front(miel)*7
          mielnew=miel+ntemp
        end if

        mt_to_id(mielnew)=iel
        ref_front_id(iel)=nelt+ntemp
      end do
!$OMP END PARALLEL DO


!.....Perform refinement (potentially in parallel): 
!       - Cut an element into eight children.
!       - Assign them element index  as iel, nelt+1,...., nelt+7.
!       - Update neighboring information.

      nelttemp=nelt

      if (num_refine .gt. 0) then
        ifmortar=.true.
      endif
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(index,miel,mielnew,iel,nelt,  &
!$OMP& treetemp,xctemp,yctemp,zctemp,cbctemp,sjetemp,ta1temp,  &
!$OMP& ii,jj,ntemp,xleft,xright,xhalf,yleft,yright,yhalf,zleft,zright,  &
!$OMP& zhalf,ndir,facedir,jface,cb,le,ne,n1,n2,i,j,k)
      do index=1, num_refine  
!.......miel is old morton index and mielnew is new morton index after refinement.
        miel=action(index)
        mielnew=miel+(front(miel)-1)*7
        iel=mt_to_id_old(miel) 
        nelt=nelttemp+(front(miel)-1)*7 
!.......save iel's information in a temporary array
        treetemp=tree(iel)
        do i=1,8
          xctemp(i)=xc(i,iel)
          yctemp(i)=yc(i,iel)
          zctemp(i)=zc(i,iel)
        end do
        do i=1,6
          cbctemp(i)=cbc(i,iel)
          do jj=1,2
            do ii=1,2
              sjetemp(ii,jj,i)=sje(ii,jj,i,iel)
            end do
          end do
        end do
        call copy(ta1temp,ta1(1,1,1,iel),nxyz)


!.......zero out iel here
        tree(iel)=0
        call nr_init(cbc(1,iel),6,0)
        call nr_init(sje(1,1,1,iel),24,0)
        call nr_init(ijel(1,1,iel),12,0)
        call r_init(ta1(1,1,1,iel),nxyz,0.d0)


!.......initialize new child elements:iel and nelt+1~nelt+7
        do j=1,7 
          mt_to_id(mielnew+j)=nelt+j
          tree(nelt+j)=0
          call nr_init(cbc(1,nelt+j),6,0)
          call nr_init(sje(1,1,1,nelt+j),24,0)
          call nr_init(ijel(1,1,nelt+j),12,0)
          call r_init(ta1(1,1,1,nelt+j),nxyz,0.d0)
        end do
          
!.......update the tree()
        ntemp=ishft(treetemp,3)
        tree(iel)=ntemp
        do i=1,7
          tree(nelt+i)=ntemp+mod(i,8)
        end do   
!.......update the children's vertices' coordinates
        xhalf=xctemp(1)+(xctemp(2)-xctemp(1))/2.d0
        xleft=xctemp(1)
        xright=xctemp(2)
        yhalf=yctemp(1)+(yctemp(3)-yctemp(1))/2.d0
        yleft=yctemp(1)
        yright=yctemp(3)
        zhalf=zctemp(1)+(zctemp(5)-zctemp(1))/2.d0
        zleft=zctemp(1)
        zright=zctemp(5)
       
        do j=1,7,2
          do i=1,7,2
            xc(i,nelt+j)     = xhalf
            xc(i+1,nelt+j)   = xright 
          end do
        end do

        do j=2,6,2
          do i=1,7,2
            xc(i,nelt+j)   = xleft
            xc(i+1,nelt+j) = xhalf
          end do
        end do
         
        do i=1,7,2
          xc(i,iel)=xleft
          xc(i+1,iel)=xhalf
        end do

        do i=1,2
          yc(i,nelt+1)=yleft
          yc(i,nelt+4)=yleft
          yc(i,nelt+5)=yleft
          yc(i+4,nelt+1)=yleft
          yc(i+4,nelt+4)=yleft
          yc(i+4,nelt+5)=yleft
        enddo
        do i=3,4
          yc(i,nelt+1)=yhalf
          yc(i,nelt+4)=yhalf
          yc(i,nelt+5)=yhalf
          yc(i+4,nelt+1)=yhalf
          yc(i+4,nelt+4)=yhalf
          yc(i+4,nelt+5)=yhalf
        end do
        do j=2,3
          do i=1,2
            yc(i,nelt+j)=yhalf
            yc(i,nelt+j+4)=yhalf
            yc(i+4,nelt+j)=yhalf
            yc(i+4,nelt+j+4)=yhalf
          end do
          do i=3,4
            yc(i,nelt+j)=yright
            yc(i,nelt+j+4)=yright
            yc(i+4,nelt+j)=yright
            yc(i+4,nelt+j+4)=yright
          end do
        end do
          
        do i=1,2
          yc(i,iel)=yleft
          yc(i+4,iel)=yleft
        end do
        do i=3,4
          yc(i,iel)=yhalf
          yc(i+4,iel)=yhalf
        end do

        do j=1,3
          do i=1,4
            zc(i,nelt+j)=zleft
            zc(i+4,nelt+j)=zhalf
          end do
        end do
        do j=4,7
          do i=1,4
            zc(i,nelt+j)=zhalf
            zc(i+4,nelt+j)=zright
          end do
        end do
        do i=1,4
          zc(i,iel)=zleft
          zc(i+4,iel)=zhalf
        end do

!.......update the children's neighbor information

!.......ndir refers to the x,y,z directions, respectively.
!       facedir refers to the orientation of the face in each direction, 
!       e.g. ndir=1, facedir=0 refers to face 1,
!       and ndir =1, facedir=1 refers to face 2.

        do ndir = 1, 3
          do facedir = 0, 1
            i=2*ndir-1+facedir
            jface=jjface(i)
            cb=cbctemp(i)

!...........find the new element indices of the four children on each
!           face of the parent element
            do k = 1, 4
              le(k) = le_arr(k,facedir,ndir)+nelt
              ne(k) = le_arr(k,1-facedir,ndir)+nelt
            end do
            if(facedir.eq.0)then
              le(1)=iel
            else
              ne(1)=iel
            end if
!...........update neighbor information of the four child elements on each 
!           face of the parent element
            do k=1,4
              cbc(i,le(k))=2
              sje(1,1,i,le(k))=ne(k)
              ijel(1,i,le(k))=1
              ijel(2,i,le(k))=1
            end do

!...........if the face type of the parent element is type 2
            if(cb.eq.2) then
              ntemp=sjetemp(1,1,i)

!.............if the neighbor ntemp is not marked to be refined
              if(ich(ntemp).ne.4)then
                cbc(jface,ntemp)=3
                ijel(1,jface,ntemp)=1
                ijel(2,jface,ntemp)=1
  
                do k=1,4
                  cbc(i,ne(k))=1
                  sje(1,1,i,ne(k))=ntemp
                  if(k.eq.1) then
                    ijel(1,i,ne(k))=1
                    ijel(2,i,ne(k))=1
                    sje(1,1,jface,ntemp)=ne(k)
                  elseif(k.eq.2) then
                    ijel(1,i,ne(k))=1
                    ijel(2,i,ne(k))=2
                    sje(1,2,jface,ntemp)=ne(k)
                  elseif(k.eq.3) then
                    ijel(1,i,ne(k))=2
                    ijel(2,i,ne(k))=1
                    sje(2,1,jface,ntemp)=ne(k)
                  elseif(k.eq.4) then
                    ijel(1,i,ne(k))=2
                    ijel(2,i,ne(k))=2
                    sje(2,2,jface,ntemp)=ne(k)
                  end if
                end do

!.............if the neighbor ntemp is also marked to be refined
              else
                n1=ref_front_id(ntemp)
                 
                do k=1,4
                  cbc(i,ne(k))=2
                  n2=n1+le_arr(k,facedir,ndir)
                  if(n2.eq.n1+8)n2=ntemp
                  sje(1,1,i,ne(k))=n2
                  ijel(1,i,ne(k))=1
                end do

              endif
!...........if the face type of the parent element is type 3
            elseif(cb.eq.3) then
              do k=1,4
                cbc(i,ne(k))=2
                if(k.eq.1) then
                  ntemp=sjetemp(1,1,i)
                elseif (k.eq.2) then
                  ntemp=sjetemp(1,2,i)
                elseif(k.eq.3) then
                  ntemp=sjetemp(2,1,i)
                elseif(k.eq.4) then
                  ntemp=sjetemp(2,2,i)
                end if
                ijel(1,i,ne(k))=1
                ijel(2,i,ne(k))=1
                sje(1,1,i,ne(k))=ntemp
                cbc(jface,ntemp)=2
                sje(1,1,jface,ntemp)=ne(k)
                ijel(1,jface,ntemp)=1
                ijel(2,jface,ntemp)=1
              end do

!...........if the face type of the parent element is type 0
            elseif(cb.eq.0) then
              do k=1,4
                cbc(i,ne(k))=cb
              end do
            end if

          end do 
        end do 

!.......map solution from parent element to children
        call remap(ta1(1,1,1,iel),ta1(1,1,1,ref_front_id(iel)+1),  &
     &             ta1temp(1,1,1))
      end do
!$OMP ENDPARALLEL DO

      nelt=nelttemp+num_refine*7
      irefine=irefine+num_refine
      ntot=nelt*lx1*lx1*lx1
      return
      end

!-----------------------------------------------------------
       logical function ifcor(n1,n2,i,iface)
!-----------------------------------------------------------
!      returns whether element n1's face i and element n2's 
!      jjface(iface) have intersections, i.e. whether n1 and 
!      n2 are neighbored by an edge.
!-----------------------------------------------------------

       use ua_data
       implicit none

       integer n1,n2,i,iface
       logical ifsame

       ifcor=.false.

       if(ifsame(n1,e1v1(iface,i),n2,e2v1(iface,i)).or.  &
     &    ifsame(n1,e1v2(iface,i),n2,e2v2(iface,i))) then
          ifcor=.true.
       end if

       return
       end

!-----------------------------------------------------------
      logical function icheck(ie,n)
!-----------------------------------------------------------
!     Check whether element ie's three faces (sharing vertex n)
!     are nonconforming. This will prevent it from being coarsened.
!     Also check ie's neighbors on those three faces, whether ie's
!     neighbors by only an edge have a size smaller than ie's,
!     which also prevents ie from being coarsened.
!-----------------------------------------------------------

      use ua_data
      implicit none

      integer ie, n, iside, ntemp1, ntemp2, ntemp3, n1, n2, n3,  &
     &cb2_1,cb3_1,cb1_2,cb3_2,cb1_3,cb2_3

      icheck=.true.
      cb2_1=0
      cb3_1=0
      cb1_2=0
      cb3_2=0
      cb1_3=0
      cb2_3=0

      n1=f_c(1,n)
      n2=f_c(2,n)
      n3=f_c(3,n)
      if((cbc(n1,ie).eq.3) .or. (cbc(n2,ie).eq.3) .or.  &
     &   (cbc(n3,ie).eq.3)) then
         icheck=.false.
      else
        ntemp1=sje(1,1,n1,ie)
        ntemp2=sje(1,1,n2,ie)
        ntemp3=sje(1,1,n3,ie)
        if(ntemp1.ne.0)then
           cb2_1=cbc(n2,ntemp1)
           cb3_1=cbc(n3,ntemp1)
        end if
        if(ntemp2.ne.0)then
           cb3_2=cbc(n3,ntemp2)
           cb1_2=cbc(n1,ntemp2)
        end if
        if(ntemp3.ne.0)then
           cb1_3=cbc(n1,ntemp3)
           cb2_3=cbc(n2,ntemp3)
        end if
        if((cbc(n1,ie).eq.2.and.(cb2_1.eq.3.or.  &
     &                               cb3_1.eq.3)).or.  &
     &     (cbc(n2,ie).eq.2.and.(cb3_2.eq.3.or.  &
     &                               cb1_2.eq.3)).or.  &
     &     (cbc(n3,ie).eq.2.and.(cb1_3.eq.3.or.  &
     &                              cb2_3.eq.3)))then
          icheck=.false.
        end if
      end if

      return
      end 

!-----------------------------------------------------------
      subroutine find_coarsen(if_coarsen,neltold)
!-----------------------------------------------------------
!     Search elements to be coarsened. Check with restrictions.
!     This subroutine only checks the element itself, not its
!     neighbors.
!-----------------------------------------------------------

      use ua_data
      implicit none

      logical if_coarsen, iftemp, iftouch
      integer iel,i,neltold

      if_coarsen=.false.

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,i,iftemp)  &
!$OMP& SHARED(if_coarsen)
      do iel=1,neltold
        if(.not.skip(iel))then
          ich(iel)=0
          if(.not.iftouch(iel)) then
            iftemp=.false.
            do i=1,nsides
!.............if iel has a larger size than its face neighbors, it
!             can not be coarsened
              if(cbc(i,iel).eq.3) then
                iftemp=.true.
              endif
            enddo
            if(.not.iftemp) then
              if(.not.if_coarsen) if_coarsen=.true.
              ich(iel)=2
            end if
          end if
        endif
      enddo
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------
      subroutine find_refine(if_refine)
!-----------------------------------------------------------
!     search elements to be refined based on whether they
!     have overlap with the heat source
!-----------------------------------------------------------

      use ua_data
      implicit none

      logical if_refine, iftouch
      integer iel

      if_refine=.false.

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel)  &
!$OMP& SHARED(if_refine)
      do iel=1,nelt
        ich(iel)=0
        if(iftouch(iel)) then
          if((xc(2,iel)-xc(1,iel)).gt.dlmin) then
            if(.not.if_refine) if_refine=.true.
            ich(iel)=4
          end if
        end if
      enddo
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      subroutine check_refine(ifrepeat)
!-----------------------------------------------------------------
!     Check whether the potential refinement will violate the
!     restriction. If so, mark the neighbor and unmark the
!     original element, and set ifrepeat true. i.e. this procedure
!     needs to be repeated until no further check is needed
!-----------------------------------------------------------------

      use ua_data
      implicit none
 
      logical ifrepeat,ifcor
      integer iel,iface,ntemp,nntemp,i,jface

      ifrepeat=.false.

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,i,jface,ntemp,  &
!$OMP& iface,nntemp) SHARED(ifrepeat)
      do iel=1,nelt
!.......if iel is marked to be refined
        if(ich(iel).eq.4) then
!.........check its six faces
          do i=1,nsides
            jface=jjface(i)
            ntemp=sje(1,1,i,iel)
!...........if one face neighbor is larger in size than iel
            if(cbc(i,iel).eq.1) then
!.............unmark iel
              ich(iel)=0
!.............the large size neighbor ntemp is marked to be refined
              if(ich(ntemp).ne.4) then
                if(.not.ifrepeat) ifrepeat=.true.
                ich(ntemp)=4
              end if
!.............check iel's neighbor, neighbored by an edge on face i, which
!             must be a face neighbor of ntemp
              do iface=1,nsides
                if(iface.ne.i.and.iface.ne.jface) then
!................if edge neighbors are larger than iel, mark them to be refined
                  if(cbc(iface,ntemp).eq.2) then
                    nntemp=sje(1,1,iface,ntemp)
!..................ifcor is to make sure the edge neighbor exist
                    if(ich(nntemp).ne.4.and.  &
     &                 ifcor(iel,nntemp,i,iface))then
                      ich(nntemp)=4
                    end if
                  end if
                end if
              end do
!...........if face neighbor are of the same size of iel, check edge neighbors
            elseif(cbc(i,iel).eq.2)then
              do iface=1,nsides
                if(iface.ne.i.and.iface.ne.jface) then
                  if(cbc(iface,ntemp).eq.1)then
                    nntemp=sje(1,1,iface,ntemp)
                    ich(nntemp)=4
                    ich(iel)=0
                    if(.not.ifrepeat) ifrepeat=.true.
                  end if
                end if
              end do
            end if
          enddo
        end if
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      logical function iftouch(iel)
!-----------------------------------------------------------------
!     check whether element iel has overlap with the heat source
!-----------------------------------------------------------------

      use ua_data
      implicit none

      double precision dis, dis1, dis2, dis3, alpha2
      integer iel

      alpha2 = alpha*alpha

      if     (x0 .lt. xc(1,iel)) then
        dis1 = xc(1,iel) - x0
      elseif (x0 .gt. xc(2,iel)) then
        dis1 = x0 - xc(2,iel)
      else
        dis1 = 0.d0
      endif

      if     (y0 .lt. yc(1,iel)) then
        dis2 = yc(1,iel) - y0
      elseif (y0 .gt. yc(3,iel)) then
        dis2 = y0 - yc(3,iel)
      else
        dis2 = 0.d0
      endif

      if     (z0 .lt. zc(1,iel)) then
        dis3 = zc(1,iel) - z0
      elseif (z0 .gt. zc(5,iel)) then
        dis3 = z0 - zc(5,iel)
      else
       dis3 = 0.d0
      endif

      dis = dis1**2+dis2**2+dis3**2

      if (dis .lt. alpha2) then
       iftouch=.true.
      else
       iftouch=.false.
      end if

      return
      end


!-----------------------------------------------------------------
      subroutine remap (y,y1,x) 
!-----------------------------------------------------------------
!     After a refinement, map the solution  from the parent (x) to
!     the eight children. y is the solution on the first child
!     (front-bottom-left) and y1 is the solution on the next 7 
!     children.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      double precision x(lx1,lx1,lx1),y(lx1,lx1,lx1),y1(lx1,lx1,lx1,7),  &
     &       yone(lx1,lx1,lx1,2), ytwo(lx1,lx1,lx1,4)
      integer i, iz, ii, jj, kk

      call r_init(y,lx1*lx1*lx1,0.d0)
      call r_init(y1,lx1*lx1*lx1*7,0.d0)
      call r_init(yone,lx1*lx1*lx1*2,0.d0)
      call r_init(ytwo,lx1*lx1*lx1*4,0.d0)

      do  i=1,lx1
        do kk = 1, lx1
          do jj = 1, lx1
            do ii = 1, lx1
              yone(ii,jj,i,1) = yone(ii,jj,i,1) +ixmc1(ii,kk)*x(kk,jj,i)
              yone(ii,jj,i,2) = yone(ii,jj,i,2) +ixmc2(ii,kk)*x(kk,jj,i)
            end do
          end do
        end do

        do kk = 1, lx1
          do jj = 1, lx1
            do ii = 1, lx1
              ytwo(ii,i,jj,1) = ytwo(ii,i,jj,1) +  &
     &                          yone(ii,kk,i,1)*ixtmc1(kk,jj)
              ytwo(ii,i,jj,2) = ytwo(ii,i,jj,2) +  &
     &                          yone(ii,kk,i,1)*ixtmc2(kk,jj)
              ytwo(ii,i,jj,3) = ytwo(ii,i,jj,3) +  &
     &                          yone(ii,kk,i,2)*ixtmc1(kk,jj)
              ytwo(ii,i,jj,4) = ytwo(ii,i,jj,4) +  &
     &                          yone(ii,kk,i,2)*ixtmc2(kk,jj)
            end do
          end do
        end do
      end do

      do  iz=1,lx1
        do kk = 1, lx1
          do jj = 1, lx1
            do ii = 1, lx1
              y(ii,iz,jj) = y(ii,iz,jj) +  &
     &                        ytwo(ii,kk,iz,1)*ixtmc1(kk,jj)
              y1(ii,iz,jj,1) = y1(ii,iz,jj,1) +  &
     &                        ytwo(ii,kk,iz,3)*ixtmc1(kk,jj)
              y1(ii,iz,jj,2) = y1(ii,iz,jj,2) +  &
     &                        ytwo(ii,kk,iz,2)*ixtmc1(kk,jj)
              y1(ii,iz,jj,3) = y1(ii,iz,jj,3) +  &
     &                        ytwo(ii,kk,iz,4)*ixtmc1(kk,jj)
              y1(ii,iz,jj,4) = y1(ii,iz,jj,4) +  &
     &                        ytwo(ii,kk,iz,1)*ixtmc2(kk,jj)
              y1(ii,iz,jj,5) = y1(ii,iz,jj,5) +  &
     &                        ytwo(ii,kk,iz,3)*ixtmc2(kk,jj)
              y1(ii,iz,jj,6) = y1(ii,iz,jj,6) +  &
     &                        ytwo(ii,kk,iz,2)*ixtmc2(kk,jj)
              y1(ii,iz,jj,7) = y1(ii,iz,jj,7) +  &
     &                        ytwo(ii,kk,iz,4)*ixtmc2(kk,jj)            
            end do
          end do
        end do
      end do

      return
      end


!=======================================================================
      subroutine merging(iela)
!-----------------------------------------------------------------------
!     This subroutine is to merge the eight child elements and map 
!     the solution from eight children to the  merged element. 
!     iela array records the eight elements to be merged.
!-----------------------------------------------------------------------

      use ua_data
      implicit none

      double precision x1,x2,y1,y2,z1,z2
      integer ielnew,i,ntemp,jface,ii,cb,ntempa(4),iela(8),ielold,  &
     &        ntema(4)

      ielnew=iela(1)

      tree(ielnew)=ishft(tree(ielnew),-3)   

!.....element vertices 
      x1=xc(1,iela(1))
      x2=xc(2,iela(2))
      y1=yc(1,iela(1))
      y2=yc(3,iela(3))
      z1=zc(1,iela(1))
      z2=zc(5,iela(5))

      do i=1,7,2
        xc(i,ielnew)=x1
      end do
      do i=2,8,2
        xc(i,ielnew)=x2
      end do
      do i=1,2
        yc(i,ielnew)=y1
        yc(i+4,ielnew)=y1
      end do
      do i=3,4
        yc(i,ielnew)=y2
        yc(i+4,ielnew)=y2
      end do
      do i=1,4
        zc(i,ielnew)=z1
      end do
      do i=5,8
        zc(i,ielnew)=z2
      end do

!.....update neighboring information
      do i=1,nsides
        jface=jjface(i)
        ielold=iela(children(1,i))
        do ii=1,4
          ntempa(ii)=iela(children(ii,i))
        end do

        cb=cbc(i,ielold)
       
        if (cb.eq.2) then
!.........if the neighbor elements also will be coarsened
          if(ifcoa_id(sje(1,1,i,ielold)))then
            if (i.eq.2 .or. i.eq. 4 .or. i.eq.6) then
              ntemp=sje(1,1,i,sje(1,1,i,ntempa(1)))
            else
              ntemp=sje(1,1,i,ntempa(1))
            end if 
            sje(1,1,i,ielnew)=ntemp
            ijel(1,i,ielnew)=1
            ijel(2,i,ielnew)=1
            cbc(i,ielnew)=2

!.........if the neighbor elements will not be coarsened
          else
            do ii=1,4
              ntema(ii)=sje(1,1,i,ntempa(ii)) 
              cbc(jface,ntema(ii))=1
              sje(1,1,jface,ntema(ii))=ielnew
              ijel(1,jface,ntema(ii))=iijj(1,ii)
              ijel(2,jface,ntema(ii))=iijj(2,ii)
              sje(iijj(1,ii),iijj(2,ii),i,ielnew)=ntema(ii)
              ijel(1,i,ielnew)=1
              ijel(2,i,ielnew)=1
            end do
            cbc(i,ielnew)=3
          end if       

        else if(cb.eq.1)then

          ntemp=sje(1,1,i,ielold)
          cbc(jface,ntemp)=2
          ijel(1,jface,ntemp)=1
          ijel(2,jface,ntemp)=1
          sje(1,1,jface,ntemp)=ielnew
          sje(1,2,jface,ntemp)=0
          sje(2,1,jface,ntemp)=0
          sje(2,2,jface,ntemp)=0
           
          cbc(i,ielnew)=2
          ijel(1,i,ielnew)=1
          ijel(2,i,ielnew)=1
          sje(1,1,i,ielnew)=ntemp
         
        else if(cb.eq.0)then
          cbc(i,ielnew)=0
          sje(1,1,i,ielnew)=0
          sje(1,2,i,ielnew)=0
          sje(2,1,i,ielnew)=0
          sje(2,2,i,ielnew)=0
        endif

      end do

!.....map solution from children to the merged element
      call remap2(iela, ielnew)
      
      return
      end

!-----------------------------------------------------------------
      subroutine remap2(iela, ielnew)
!-----------------------------------------------------------------
!     Map the solution from the children to the parent.
!     iela array records the eight elements to be merged.
!     ielnew is the element index of the merged element.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      integer iela(8), ielnew

      double precision temp1(lx1,lx1,lx1),  &
     &       temp2(lx1,lx1,lx1),temp3(lx1,lx1,lx1),temp4(lx1,lx1,lx1),  &
     &       temp5(lx1,lx1,lx1),temp6(lx1,lx1,lx1)

      call remapx(ta1(1,1,1,iela(1)),ta1(1,1,1,iela(2)),temp1)
      call remapx(ta1(1,1,1,iela(3)),ta1(1,1,1,iela(4)),temp2)
      call remapx(ta1(1,1,1,iela(5)),ta1(1,1,1,iela(6)),temp3)
      call remapx(ta1(1,1,1,iela(7)),ta1(1,1,1,iela(8)),temp4)
      call remapy(temp1,temp2,temp5)
      call remapy(temp3,temp4,temp6)
      call remapz(temp5,temp6,ta1(1,1,1,ielnew))

      return
      end       

!-----------------------------------------------------------------
      subroutine remapz(x1,x2,y)
!-----------------------------------------------------------------
!     z direction mapping after the merge.
!     Map solution from x1 & x2 to y.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      double precision x1(lx1,lx1,lx1),x2(lx1,lx1,lx1),y(lx1,lx1,lx1)
      integer ix, iy, ip

      do iy=1,lx1
        do ix=1,lx1
          y(ix,iy,1)=x1(ix,iy,1)

          y(ix,iy,2)=0.d0
          do ip=1,lx1
            y(ix,iy,2)=y(ix,iy,2)+map2(ip)*x1(ix,iy,ip)
          end do

          y(ix,iy,3)=x1(ix,iy,lx1)

          y(ix,iy,4)=0.d0
          do ip=1,lx1
            y(ix,iy,4)=y(ix,iy,4)+map4(ip)*x2(ix,iy,ip)
          end do

          y(ix,iy,lx1)=x2(ix,iy,lx1)
        end do
      end do

      return
      end      

!-----------------------------------------------------------------
      subroutine remapy(x1,x2,y)
!-----------------------------------------------------------------
!     y direction mapping after the merge.
!     Map solution from x1 & x2 to y.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      double precision x1(lx1,lx1,lx1),x2(lx1,lx1,lx1),y(lx1,lx1,lx1)
      integer ix, iz, ip

      do iz=1,lx1
        do ix=1,lx1
          y(ix,1,iz)=x1(ix,1,iz)

          y(ix,2,iz)=0.d0
          do ip=1,lx1
            y(ix,2,iz)=y(ix,2,iz)+map2(ip)*x1(ix,ip,iz)
          end do

          y(ix,3,iz)=x1(ix,lx1,iz)

          y(ix,4,iz)=0.d0
          do ip=1,lx1
            y(ix,4,iz)=y(ix,4,iz)+map4(ip)*x2(ix,ip,iz)
          end do

          y(ix,lx1,iz)=x2(ix,lx1,iz)
        end do
      end do

      return
      end      

!-----------------------------------------------------------------
      subroutine remapx(x1,x2,y)
!-----------------------------------------------------------------
!     x direction mapping after the merge.
!     Map solution from x1 & x2 to y.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      double precision x1(lx1,lx1,lx1),x2(lx1,lx1,lx1),y(lx1,lx1,lx1)
      integer iy, iz, ip

      do iz=1,lx1
        do iy=1,lx1
          y(1,iy,iz)=x1(1,iy,iz)

          y(2,iy,iz)=0.d0
          do ip=1,lx1
            y(2,iy,iz)=y(2,iy,iz)+map2(ip)*x1(ip,iy,iz)
          end do

          y(3,iy,iz)=x1(lx1,iy,iz)

          y(4,iy,iz)=0.d0
          do ip=1,lx1
            y(4,iy,iz)=y(4,iy,iz)+map4(ip)*x2(ip,iy,iz)
          end do

          y(lx1,iy,iz)=x2(lx1,iy,iz)
        end do
      end do

      return
      end      
       
