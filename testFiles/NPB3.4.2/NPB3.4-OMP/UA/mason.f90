!-----------------------------------------------------------------
      subroutine mortar
!-----------------------------------------------------------------
!     generate mortar point index number 
!-----------------------------------------------------------------

      use ua_data
      implicit none

      integer count, iel, jface, ntemp, i, ii, jj, ntemp1,  &
     &        iii, jjj, face2, ne, ie, edge_g, ie2,  &
     &        mor_v(3), cb, cb1, cb2, cb3, cb4, cb5, cb6,  &
     &        space, sumcb, ij1, ij2, n1, n2, n3, n4, n5

      n1=lx1*lx1*6*4*nelt
      n2=8*nelt
      n3=2*64*nelt
      n4=12*nelt
      n5=2*12*nelt

      call nr_init_omp(idmo,n1,0)
      call nr_init_omp(nemo,n2,0)
      call nr_init_omp(vassign,n2,0)
      call nr_init_omp(emo,n3,0)
      call  l_init_omp(if_1_edge,n4,.false.)
      call nr_init_omp(diagn,n5,0)
!.....Mortar points indices are generated in two steps: first generate 
!     them for all element vertices (corner points), then for conforming 
!     edge and conforming face interiors. Each time a new mortar index 
!     is generated for a mortar point, it is broadcast to all elements 
!     sharing this mortar point. 

!.....VERTICES
      count=0

!.....assign mortar point indices to element vertices
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,sumcb,ij1,ij2,  &
!$OMP& cb,cb1,cb2,ntemp,ntemp1)

      do iel=1,nelt

!.......first calculate how many new mortar indices will be generated for 
!       each element.

!.......For each element, at least one vertex (vertex 8) will be new mortar
!       point. All possible new mortar points will be on face 2,4 or 6. By
!       checking the type of these three faces, we are able to tell
!       how many new mortar vertex points will be generated in each element.

        cb=cbc(6,iel)
        cb1=cbc(4,iel)
        cb2=cbc(2,iel)

!.......For different combinations of the type of these three faces,
!       we group them into 27 configurations.
!       For different face types we assign the following integers:
!              1 for type 2 or 3
!              2 for type 0
!              5 for type 1
!       By summing these integers for faces 2,4 and 6, sumcb will have 
!       10 different numbers indicating 10 different combinations. 

        sumcb=0
        if(cb.eq.2.or.cb.eq.3)then
          sumcb=sumcb+1
        elseif(cb.eq.0)then
          sumcb=sumcb+2
        elseif(cb.eq.1)then
          sumcb=sumcb+5
        end if
        if(cb1.eq.2.or.cb1.eq.3)then
          sumcb=sumcb+1
        elseif(cb1.eq.0)then
          sumcb=sumcb+2
        elseif(cb1.eq.1)then
          sumcb=sumcb+5
        end if
        if(cb2.eq.2.or.cb2.eq.3)then
          sumcb=sumcb+1
        elseif(cb2.eq.0)then
          sumcb=sumcb+2
        elseif(cb2.eq.1)then
          sumcb=sumcb+5
        end if

!.......compute newc(iel)
!       newc(iel) records how many new mortar indices will be generated
!                 for element iel
!       vassign(i,iel) records the element vertex of the i'th new mortar 
!                 vertex point for element iel. e.g. vassign(2,iel)=8 means
!                 the 2nd new mortar vertex point generated on element
!                 iel is iel's 8th vertex.
 
        if(sumcb.eq.3)then
!.......the three face types for face 2,4, and 6 are 2 2 2
          newc(iel)=1
          vassign(1,iel)=8
          
        elseif(sumcb.eq.4)then
!.......the three face types for face 2,4 and 6 are 2 2 0 (not 
!       necessarily in this order)
          newc(iel)=2
          if(cb.eq.0)then
            vassign(1,iel)=4
          elseif(cb1.eq.0)then
            vassign(1,iel)=6
          elseif(cb2.eq.0)then
            vassign(1,iel)=7
          end if
          vassign(2,iel)=8

        elseif(sumcb.eq.7)then
!.......the three face types for face 2,4 and 6 are 2 2 1 (not 
!       necessarily in this order)
          if(cb.eq.1)then
            ij1=ijel(1,6,iel)
            ij2=ijel(2,6,iel)
            if(ij1.eq.1.and.ij2.eq.1)then
              newc(iel)=2
              vassign(1,iel)=4
              vassign(2,iel)=8
            elseif(ij1.eq.1.and.ij2.eq.2)then
              ntemp=sje(1,1,6,iel)
              if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=4
                vassign(2,iel)=8
              end if
            elseif(ij1.eq.2.and.ij2.eq.1)then
              ntemp=sje(1,1,6,iel)
              if(cbc(3,ntemp).eq.3.and.sje(1,1,3,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=4
                vassign(2,iel)=8
              endif
            else
              newc(iel)=1
              vassign(1,iel)=8
            end if
          elseif(cb1.eq.1)then
            ij1=ijel(1,4,iel)
            ij2=ijel(2,4,iel)
            if(ij1.eq.1.and.ij2.eq.1)then
              newc(iel)=2
              vassign(1,iel)=6
              vassign(2,iel)=8
            elseif(ij1.eq.1.and.ij2.eq.2)then
              ntemp=sje(1,1,4,iel)
              if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=6
                vassign(2,iel)=8
              endif
            elseif(ij1.eq.2.and.ij2.eq.1)then
              ntemp=sje(1,1,4,iel)
              if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=6
                vassign(2,iel)=8
              endif
            else
              newc(iel)=1
              vassign(1,iel)=8
            end if

          elseif(cb2.eq.1)then
            ij1=ijel(1,2,iel)
            ij2=ijel(2,2,iel)
            if(ij1.eq.1.and.ij2.eq.1)then
              newc(iel)=2
              vassign(1,iel)=7
              vassign(2,iel)=8
            elseif(ij1.eq.1.and.ij2.eq.2)then
              ntemp=sje(1,1,2,iel)
              if(cbc(3,ntemp).eq.3.and.sje(1,1,3,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=7
                vassign(2,iel)=8
              end if

            elseif(ij1.eq.2.and.ij2.eq.1)then
              ntemp=sje(1,1,2,iel)
              if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                newc(iel)=1
                vassign(1,iel)=8
              else
                newc(iel)=2
                vassign(1,iel)=7
                vassign(2,iel)=8
              end if
            else
              newc(iel)=1
              vassign(1,iel)=8
            end if
          end if

        elseif(sumcb.eq.5)then
!.......the three face types for face 2,4 and 6 are 2/3 0 0 (not 
!       necessarily in this order)
          newc(iel)=4
          if(cb.eq.2.or.cb.eq.3)then
            vassign(1,iel)=5
            vassign(2,iel)=6
            vassign(3,iel)=7
            vassign(4,iel)=8
          elseif(cb1.eq.2.or.cb1.eq.3)then
            vassign(1,iel)=3
            vassign(2,iel)=4
            vassign(3,iel)=7
            vassign(4,iel)=8
          elseif(cb2.eq.2.or.cb2.eq.3)then
            vassign(1,iel)=2
            vassign(2,iel)=4
            vassign(3,iel)=6
            vassign(4,iel)=8
          end if

        elseif(sumcb.eq.8)then
!.......the three face types for face 2,4 and 6 are 2 0 1 (not 
!       necessarily in this order)

!.........if face 2 of type 1
          if(cb.eq.1)then
            if(cb1.eq.2.or.cb1.eq.3)then
              ij1=ijel(1,6,iel)
              if(ij1.eq.1)then
                newc(iel)=4
                vassign(1,iel)=3
                vassign(2,iel)=4
                vassign(3,iel)=7
                vassign(4,iel)=8
              else 
                ntemp=sje(1,1,6,iel)
                if(cbc(3,ntemp).eq.3.and.sje(1,1,3,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=7
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if

            elseif(cb2.eq.2.or.cb2.eq.3)then
              if(ijel(2,6,iel).eq.1)then
                newc(iel)=4
                vassign(1,iel)=2
                vassign(2,iel)=4
                vassign(3,iel)=6
                vassign(4,iel)=8
              else
                ntemp=sje(1,1,6,iel)
                if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=6
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=6
                  vassign(3,iel)=8
                end if
              end if
            end if

!.........if face 4 of type 1
          elseif(cb1.eq.1)then
            if(cb.eq.2.or.cb.eq.3)then
              ij1=ijel(1,4,iel)
              ij2=ijel(2,4,iel)

              if(ij1.eq.1.and.ij2.eq.1)then
                ntemp=sje(1,1,4,iel)
                if(cbc(2,ntemp).eq.3.and.sje(1,1,2,ntemp).lt.iel)then
                  newc(iel)=3
                  vassign(1,iel)=6
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                else
                  newc(iel)=4
                  vassign(1,iel)=5
                  vassign(2,iel)=6
                  vassign(3,iel)=7
                  vassign(4,iel)=8
                end if
              elseif(ij1.eq.1.and.ij2.eq.2)then
                ntemp=sje(1,1,4,iel)
                if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                  newc(iel)=3
                  vassign(1,iel)=5
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                else
                  newc(iel)=4
                  vassign(1,iel)=5
                  vassign(2,iel)=6
                  vassign(3,iel)=7
                  vassign(4,iel)=8
                end if
              elseif(ij1.eq.2.and.ij2.eq.1)then
                ntemp=sje(1,1,4,iel)
                if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=7
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=6
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              elseif(ij1.eq.2.and.ij2.eq.2)then
                ntemp=sje(1,1,4,iel)
                if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=7
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=5
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if
            else 
              if(ijel(2,4,iel).eq.1)then
                newc(iel)=4
                vassign(1,iel)=2
                vassign(2,iel)=4
                vassign(3,iel)=6
                vassign(4,iel)=8
              else
                ntemp=sje(1,1,4,iel)
                if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=4
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=6
                  vassign(3,iel)=8
                end if
              end if
            endif
!.........if face 6 of type 1
          elseif(cb2.eq.1)then
            if(cb.eq.2.or.cb.eq.3)then
              if(ijel(1,2,iel).eq.1)then
                newc(iel)=4
                vassign(1,iel)=5
                vassign(2,iel)=6
                vassign(3,iel)=7
                vassign(4,iel)=8
              else
                ntemp=sje(1,1,2,iel)
                if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=6
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=6
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if
            else 
              if(ijel(2,2,iel).eq.1)then
                newc(iel)=4
                vassign(1,iel)=3
                vassign(2,iel)=4
                vassign(3,iel)=7
                vassign(4,iel)=8
              else
                ntemp=sje(1,1,2,iel)
                if(cbc(3,ntemp).eq.3.and.sje(1,1,3,ntemp).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=4
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if
            end if
          end if

        elseif(sumcb.eq.11)then
!.......the three face type for face 2,4 and 6 are 2 1 1(not 
!       necessarily in this order)
          if(cb.eq.2.or.cb.eq.3)then
            if(ijel(1,4,iel).eq.1)then
              ntemp=sje(1,1,4,iel)
              if(cbc(2,ntemp).eq.3.and.sje(1,1,2,ntemp).lt.iel)then
                newc(iel)=3
                vassign(1,iel)=6
                vassign(2,iel)=7
                vassign(3,iel)=8
              else
                newc(iel)=4
                vassign(1,iel)=5
                vassign(2,iel)=6
                vassign(3,iel)=7
                vassign(4,iel)=8
              end if

!...........if ijel(1,4,iel)=2
            else
              ntemp=sje(1,1,2,iel)
              if(cbc(5,ntemp).eq.3.and.sje(1,1,5,ntemp).lt.iel)then
                ntemp1=sje(1,1,4,iel)
                if(cbc(5,ntemp1).eq.3.and.  &
     &             sje(1,1,5,ntemp1).lt.iel)then
                  newc(iel)=1
                  vassign(1,iel)=8
                else
                  newc(iel)=2
                  vassign(1,iel)=6
                  vassign(2,iel)=8
                end if
              else
                ntemp1=sje(1,1,4,iel)
                if(cbc(5,ntemp1).eq.3.and.  &
     &             sje(1,1,5,ntemp1).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=7
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=6
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if
            end if
          elseif(cb1.eq.2.or.cb1.eq.3)then
            if(ijel(2,2,iel).eq.1)then
              ntemp=sje(1,1,2,iel)
              if(cbc(6,ntemp).eq.3.and.sje(1,1,6,ntemp).lt.iel)then
                newc(iel)=3
                vassign(1,iel)=4
                vassign(2,iel)=7
                vassign(3,iel)=8
              else
                newc(iel)=4
                vassign(1,iel)=3
                vassign(2,iel)=4
                vassign(3,iel)=7
                vassign(4,iel)=8
              end if
!...........if ijel(2,2,iel)=2
            else
              ntemp=sje(1,1,2,iel)
              if(cbc(3,ntemp).eq.3.and.sje(1,1,3,ntemp).lt.iel)then
                ntemp1=sje(1,1,6,iel)
                if(cbc(3,ntemp1).eq.3.and.  &
     &            sje(1,1,3,ntemp1).lt.iel)then
                  newc(iel)=1
                  vassign(1,iel)=8
                else
                  newc(iel)=2
                  vassign(1,iel)=4
                  vassign(2,iel)=8
                end if
              else
                ntemp1=sje(1,1,6,iel)
                if(cbc(3,ntemp1).eq.3.and.  &
     &            sje(1,1,3,ntemp1).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=7
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=7
                  vassign(3,iel)=8
                end if
              end if
            end if
          elseif(cb2.eq.2.or.cb2.eq.3)then
            if(ijel(2,6,iel).eq.1)then
              ntemp=sje(1,1,4,iel)
              if(cbc(6,ntemp).eq.3.and.sje(1,1,6,ntemp).lt.iel)then
                newc(iel)=3
                vassign(1,iel)=4
                vassign(2,iel)=6
                vassign(3,iel)=8
              else
                newc(iel)=4
                vassign(1,iel)=2
                vassign(2,iel)=4
                vassign(3,iel)=6
                vassign(4,iel)=8
              end if
!...........if ijel(2,6,iel)=2
            else
              ntemp=sje(1,1,4,iel)
              if(cbc(1,ntemp).eq.3.and.sje(1,1,1,ntemp).lt.iel)then
                ntemp1=sje(1,1,6,iel)
                if(cbc(1,ntemp1).eq.3.and.  &
     &            sje(1,1,1,ntemp1).lt.iel)then
                  newc(iel)=1
                  vassign(1,iel)=8
                else
                  newc(iel)=2
                  vassign(1,iel)=4
                  vassign(2,iel)=8
                end if
              else
                ntemp1=sje(1,1,6,iel)
                if(cbc(1,ntemp1).eq.3.and.  &
     &              sje(1,1,1,ntemp1).lt.iel)then
                  newc(iel)=2
                  vassign(1,iel)=6
                  vassign(2,iel)=8
                else
                  newc(iel)=3
                  vassign(1,iel)=4
                  vassign(2,iel)=6
                  vassign(3,iel)=8
                end if
              end if
            end if

          end if
          
        elseif(sumcb.eq.6)then
!.......the three face type for face 2,4 and 6 are 0 0 0(not 
!       necessarily in this order)
          newc(iel)=8
          vassign(1,iel)=1
          vassign(2,iel)=2
          vassign(3,iel)=3
          vassign(4,iel)=4
          vassign(5,iel)=5
          vassign(6,iel)=6
          vassign(7,iel)=7
          vassign(8,iel)=8

        elseif(sumcb.eq.9)then
!.......the three face type for face 2,4 and 6 are 0 0 1(not 
!       necessarily in this order)
          newc(iel)=7
          vassign(1,iel)=2
          vassign(2,iel)=3
          vassign(3,iel)=4
          vassign(4,iel)=5
          vassign(5,iel)=6
          vassign(6,iel)=7
          vassign(7,iel)=8

        elseif(sumcb.eq.12)then
!.......the three face type for face 2,4 and 6 are 0 1 1(not 
!       necessarily in this order)
          if(cb.eq.0)then
            ntemp=sje(1,1,2,iel)
            if(cbc(4,ntemp).eq.3.and.sje(1,1,4,ntemp).lt.iel)then
              newc(iel)=6
              vassign(1,iel)=2
              vassign(2,iel)=3
              vassign(3,iel)=4
              vassign(4,iel)=6
              vassign(5,iel)=7
              vassign(6,iel)=8
            else
              newc(iel)=7
              vassign(1,iel)=2
              vassign(2,iel)=3
              vassign(3,iel)=4
              vassign(4,iel)=5
              vassign(5,iel)=6
              vassign(6,iel)=7
              vassign(7,iel)=8
            end if
          elseif(cb1.eq.0)then
            newc(iel)=7
            vassign(1,iel)=2
            vassign(2,iel)=3
            vassign(3,iel)=4
            vassign(4,iel)=5
            vassign(5,iel)=6
            vassign(6,iel)=7
            vassign(7,iel)=8
          elseif(cb2.eq.0)then
            ntemp=sje(1,1,4,iel)
            if(cbc(6,ntemp).eq.3.and.sje(1,1,6,ntemp).lt.iel)then
              newc(iel)=6
              vassign(1,iel)=3
              vassign(2,iel)=4
              vassign(3,iel)=5
              vassign(4,iel)=6
              vassign(5,iel)=7
              vassign(6,iel)=8
            else
              newc(iel)=7
              vassign(1,iel)=2
              vassign(2,iel)=3
              vassign(3,iel)=4
              vassign(4,iel)=5
              vassign(5,iel)=6
              vassign(6,iel)=7
              vassign(7,iel)=8
            end if
          end if
        
        elseif(sumcb.eq.15)then
!.......the three face type for face 2,4 and 6 are 1 1 1(not 
!       necessarily in this order)
          ntemp=sje(1,1,4,iel)
          ntemp1=sje(1,1,2,iel)
          if(cbc(6,ntemp).eq.3.and.sje(1,1,6,ntemp).lt.iel)then
            if(cbc(2,ntemp).eq.3.and.sje(1,1,2,ntemp).lt.iel)then
              if(cbc(6,ntemp1).eq.3.and.sje(1,1,6,ntemp1).lt.iel)then
                newc(iel)=4
                vassign(1,iel)=4
                vassign(2,iel)=6
                vassign(3,iel)=7
                vassign(4,iel)=8
              else
                newc(iel)=5
                vassign(1,iel)=3
                vassign(2,iel)=4
                vassign(3,iel)=6
                vassign(4,iel)=7
                vassign(5,iel)=8
              end if
            else
              if(cbc(6,ntemp1).eq.3.and.sje(1,1,6,ntemp1).lt.iel)then
                newc(iel)=5
                vassign(1,iel)=4
                vassign(2,iel)=5
                vassign(3,iel)=6
                vassign(4,iel)=7
                vassign(5,iel)=8
              else
                newc(iel)=6
                vassign(1,iel)=3
                vassign(2,iel)=4
                vassign(3,iel)=5
                vassign(4,iel)=6
                vassign(5,iel)=7
                vassign(6,iel)=8
              end if
            end if
          else
            if(cbc(2,ntemp).eq.3.and.sje(1,1,2,ntemp).lt.iel)then
              if(cbc(6,ntemp1).eq.3.and.sje(1,1,6,ntemp1).lt.iel)then
                newc(iel)=5
                vassign(1,iel)=2
                vassign(2,iel)=4
                vassign(3,iel)=6
                vassign(4,iel)=7
                vassign(5,iel)=8
              else
                newc(iel)=6
                vassign(1,iel)=2
                vassign(2,iel)=3
                vassign(3,iel)=4
                vassign(4,iel)=6
                vassign(5,iel)=7
                vassign(6,iel)=8
              end if
            else
              if(cbc(6,ntemp1).eq.3.and.sje(1,1,6,ntemp1).lt.iel)then
                newc(iel)=6
                vassign(1,iel)=2
                vassign(2,iel)=4
                vassign(3,iel)=5
                vassign(4,iel)=6
                vassign(5,iel)=7
                vassign(6,iel)=8

              else
                newc(iel)=7
                vassign(1,iel)=2 
                vassign(2,iel)=3 
                vassign(3,iel)=4 
                vassign(4,iel)=5
                vassign(5,iel)=6
                vassign(6,iel)=7
                vassign(7,iel)=8
              end if
            end if
          end if
        end if
      end do
!$OMP END PARALLEL DO
!.....end computing how many new mortar vertex points will be generated
!     on each element.

!.....Compute (potentially in parallel) front(iel), which records how many 
!     new mortar point indices are to be generated from element 1 to iel.
!     front(iel)=newc(1)+newc(2)+...+newc(iel)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel)
      do iel=1,nelt
        front(iel)=newc(iel)
      end do
!$OMP END PARALLEL DO

      call parallel_add(front)

!.....On each element, generate new mortar point indices and assign them
!     to all elements sharing this mortar point. Note, if a mortar point 
!     is shared by several elements, the mortar point index of it will only
!     be generated on the element with the lowest element index. 

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,i,count)
      do iel=1,nelt

!.......compute the starting vertex mortar point index in element iel
        front(iel)=front(iel)-newc(iel)

        do i=1,newc(iel)
!.........count is the new mortar index number, which will be assigned
!         to a vertex of iel and broadcast to all other elements sharing
!         this vertex point.
          count=front(iel)+i
          call mortar_vertex(vassign(i,iel),iel,count) 
        end do
      end do
!$OMP END PARALLEL DO

!.....nvertex records how many mortar indices are for element vertices.
!     It is used in the computation of the preconditioner.
      count=front(nelt)+newc(nelt)
      nvertex=count

!.....CONFORMING EDGE AND FACE INTERIOR

!.....find out how many new mortar point indices will be assigned to all
!.....conforming edges and all conforming face interiors on each element

      n1=12*nelt
      n2=6*nelt

!.....eassign(i,iel)=.true.   indicates that the i'th edge on iel will 
!                             generate new mortar points. 
!     ncon_edge(i,iel)=.true. indicates that the i'th edge on iel is 
!                             nonconforming
      call l_init_omp(ncon_edge,n1,.false.)
      call l_init_omp(eassign,n1,.false.)
!.....fassign(i,iel)=.true. indicates that the i'th face of iel will 
!                           generate new mortar points
      call l_init_omp(fassign,n2,.false.)

!.....newe records how many new edges are to be assigned
!     diagn(1,n,iel) records the element index of neighbor element of iel,
!                    that shares edge n of iel
!     diagn(2,n,iel) records the neighbor element diagn(1,n,iel) shares which
!                    part of edge n of iel. diagn(2,n,iel)=1 refers to left
!                    or bottom half of the edge n, diagn(2,n,iel)=2 refers
!                    to the right or top part of edge n.
!     if_1_edge(n,iel)=.true. indicates that the size of iel is smaller than 
!                    that of its neighbor connected, neighbored by edge n only

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,cb1,cb2,cb3,cb4,cb5  &
!$OMP& ,cb6,ntemp)

      do iel=1,nelt
        newc(iel)=0
        newe(iel)=0
        newi(iel)=0
        cb1=cbc(1,iel)
        cb2=cbc(2,iel)
        cb3=cbc(3,iel)
        cb4=cbc(4,iel)
        cb5=cbc(5,iel)
        cb6=cbc(6,iel)

!.......on face 6

        if(cb6.eq.0)then
          if(cb4.eq.0.or.cb4.eq.1)then
!...........if face 6 is of type 0 and face 4 is of type 0 or type 1, the edge
!           shared by face 4 and 6 (edge 11) will generate new mortar point
!           indices.
            newe(iel)=newe(iel)+1
            eassign(11,iel)=.true.
          end if
          if(cb1.ne.3)then
!...........if face 1 is of type 3, the edge shared by face 6 and 1 (edge 1)
!           will generate new mortar points indices.
            newe(iel)=newe(iel)+1
            eassign(1,iel)=.true.
          end if
          if(cb3.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(9,iel)=.true.
          end if
          if(cb2.eq.0.or.cb2.eq.1)then
            newe(iel)=newe(iel)+1
            eassign(5,iel)=.true.
          end if
        elseif(cb6.eq.1)then
          if(cb4.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(11,iel)=.true.
          elseif(cb4.eq.1)then

!...........If face 6 and face 4 both are of type 1, ntemp is the neighbor
!           element on face 4.
            ntemp=sje(1,1,4,iel)

!...........if ntemp's face 6 is not noncoforming or the neighbor element
!           of ntemp on face 6 has an element index larger than iel, the 
!           edge shared by face 6 and 4 (edge 11) will generate new mortar
!           point indices.
            if(cbc(6,ntemp).ne.3.or.sje(1,1,6,ntemp).gt.iel)then

              newe(iel)=newe(iel)+1
              eassign(11,iel)=.true.
!.............if the face 6 of ntemp is of type 2
              if(cbc(6,ntemp).eq.2)then
!...............The neighbor element of iel, neighbored by edge 11, is 
!               sje(1,1,6,ntemp) (the neighbor element of ntemp on ntemp's
!               face 6).
                diagn(1,11,iel)=sje(1,1,6,ntemp)
!...............The neighbor element of iel, neighbored by edge 11 shares
!               the ijel(2,6,iel) part of edge 11 of iel
                diagn(2,11,iel)=ijel(2,6,iel)
!...............edge 10 of element sje(1,1,6,ntemp) (the neighbor element of 
!               ntemp on ntemp's face 6) is a nonconforming edge
                ncon_edge(10,sje(1,1,6,ntemp))=.true.
!...............if_1_edge(n,iel)=.true. indicates that iel is of a smaller
!               size than its neighbor element, neighbored by edge n of iel only.
                if_1_edge(11,iel)=.true.
              endif
              if(cbc(6,ntemp).eq.3.and.  &
     &          sje(1,1,6,ntemp).gt.iel)then
                diagn(1,11,iel)=sje(2,ijel(2,6,iel),6,ntemp)
              endif
            end if
          endif

          if(cb1.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(1,iel)=.true.
          elseif(cb1.eq.1)then
            ntemp=sje(1,1,1,iel)
            if(cbc(6,ntemp).ne.3.or.sje(1,1,6,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(1,iel)=.true.
              if(cbc(6,ntemp).eq.2)then
                diagn(1,1,iel)=sje(1,1,6,ntemp)
                diagn(2,1,iel)=ijel(1,6,iel)
                ncon_edge(7,sje(1,1,6,ntemp))=.true.
                if_1_edge(1,iel)=.true.
              endif
              if(cbc(6,ntemp).eq.3.and.  &
     &          sje(1,1,6,ntemp).gt.iel)then
                diagn(1,1,iel)=sje(ijel(1,6,iel),1,6,ntemp)
              endif
            end if
          elseif(cb1.eq.2)then
            if(ijel(2,6,iel).eq.2)then
              ntemp=sje(1,1,1,iel)
              if(cbc(6,ntemp).eq.1)then
                newe(iel)=newe(iel)+1
                eassign(1,iel)=.true.
!.............if cbc(6,ntemp)=2
              else
                if(sje(1,1,6,ntemp).gt.iel)then
                  newe(iel)=newe(iel)+1
                  eassign(1,iel)=.true.
                  diagn(1,1,iel)=sje(1,1,6,ntemp)
                end if
              end if
            else
              newe(iel)=newe(iel)+1
              eassign(1,iel)=.true.
            end if
          end if

          if(cb3.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(9,iel)=.true.
          elseif(cb3.eq.1)then
            ntemp=sje(1,1,3,iel)
            if(cbc(6,ntemp).ne.3.or.sje(1,1,6,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(9,iel)=.true.
              if(cbc(6,ntemp).eq.2)then
                diagn(1,9,iel)=sje(1,1,6,ntemp)
                diagn(2,9,iel)=ijel(2,6,iel)
                ncon_edge(12,sje(1,1,6,ntemp))=.true.
                if_1_edge(9,iel)=.true.
              endif
              if(cbc(6,ntemp).eq.3.and.  &
     &           sje(1,1,6,ntemp).gt.iel)then
                diagn(1,9,iel)=sje(2,ijel(2,6,iel),6,ntemp)
              endif
            end if
          elseif(cb3.eq.2)then
            if(ijel(1,6,iel).eq.2)then
              ntemp=sje(1,1,3,iel)
              if(cbc(6,ntemp).eq.1)then
                newe(iel)=newe(iel)+1
                eassign(9,iel)=.true.
!.............if cbc(6,ntemp)=2
              else
                if(sje(1,1,6,ntemp).gt.iel)then
                  newe(iel)=newe(iel)+1
                  eassign(9,iel)=.true.
                  diagn(1,9,iel)=sje(1,1,6,ntemp)
                end if
              end if
            else
              newe(iel)=newe(iel)+1
              eassign(9,iel)=.true.
            end if
          end if

          if(cb2.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(5,iel)=.true.
          elseif(cb2.eq.1)then
            ntemp=sje(1,1,2,iel)
            if(cbc(6,ntemp).ne.3.or.sje(1,1,6,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(5,iel)=.true.
              if(cbc(6,ntemp).eq.2)then
                diagn(1,5,iel)=sje(1,1,6,ntemp)
                diagn(2,5,iel)=ijel(1,6,iel)
                ncon_edge(3,sje(1,1,6,ntemp))=.true.
                if_1_edge(5,iel)=.true.
              endif
              if(cbc(6,ntemp).eq.3.and.  &
     &          sje(1,1,6,ntemp).gt.iel)then
                diagn(1,9,iel)=sje(2,ijel(2,6,iel),6,ntemp)
              endif
            endif
          end if
        end if

!.......one face 4
        if(cb4.eq.0)then
          if(cb1.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(4,iel)=.true.
          endif
          if(cb5.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(12,iel)=.true.
          endif
          if(cb2.eq.0.or.cb2.eq.1)then
            newe(iel)=newe(iel)+1
            eassign(8,iel)=.true.
          end if 
           
        elseif(cb4.eq.1)then
          if(cb1.eq.2)then
            if(ijel(2,4,iel).eq.1)then
              newe(iel)=newe(iel)+1
              eassign(4,iel)=.true.
            else
              ntemp=sje(1,1,4,iel)
              if(cbc(1,ntemp).ne.3.or.sje(1,1,1,ntemp).gt.iel)then
                newe(iel)=newe(iel)+1
                eassign(4,iel)=.true.
                if(cbc(1,ntemp).eq.3.and.  &
     &            sje(1,1,1,ntemp).gt.iel)then
                  diagn(1,4,iel)=sje(ijel(1,4,iel),2,1,ntemp) 
                endif
              endif
            end if
          elseif(cb1.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(4,iel)=.true.
          elseif(cb1.eq.1)then
            ntemp=sje(1,1,4,iel)
            if(cbc(1,ntemp).ne.3.or.sje(1,1,1,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(4,iel)=.true.
              if(cbc(1,ntemp).eq.2)then
                diagn(1,4,iel)=sje(1,1,1,ntemp)
                diagn(2,4,iel)=ijel(1,4,iel)
                ncon_edge(6,sje(1,1,1,ntemp))=.true.
                if_1_edge(4,iel)=.true.
              endif
              if(cbc(1,ntemp).eq.3.and.  &
     &          sje(1,1,1,ntemp).gt.iel)then
                diagn(1,4,iel)=sje(ijel(1,4,iel),2,1,ntemp)
              endif
            end if
          end if
          if(cb5.eq.2)then
            if(ijel(1,4,iel).eq.1)then
              newe(iel)=newe(iel)+1
              eassign(12,iel)=.true.
            else
              ntemp=sje(1,1,4,iel)
              if(cbc(5,ntemp).ne.3.or.sje(1,1,5,ntemp).gt.iel)then
                newe(iel)=newe(iel)+1
                eassign(12,iel)=.true.
                if(cbc(5,ntemp).eq.3.and.  &
     &            sje(1,1,5,ntemp).gt.iel)then
                  diagn(1,12,iel)=sje(2,ijel(2,4,iel),5,ntemp)
                endif
              endif
            end if
          elseif(cb5.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(12,iel)=.true.
          elseif(cb5.eq.1)then
            ntemp=sje(1,1,4,iel)
            if(cbc(5,ntemp).ne.3.or.sje(1,1,5,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(12,iel)=.true.
              if(cbc(5,ntemp).eq.2)then
                diagn(1,12,iel)=sje(1,1,5,ntemp)
                diagn(2,12,iel)=ijel(2,4,iel)
                ncon_edge(9,sje(1,1,5,ntemp))=.true.
                if_1_edge(12,iel)=.true.
              endif
              if(cbc(5,ntemp).eq.3.and.  &
     &          sje(1,1,5,ntemp).gt.iel)then
                diagn(1,12,iel)=sje(2,ijel(2,4,iel),5,ntemp)
              endif
            end if
          end if
          if(cb2.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(8,iel)=.true.
          elseif(cb2.eq.1)then
            ntemp=sje(1,1,4,iel)
            if(cbc(2,ntemp).ne.3.or.sje(1,1,2,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(8,iel)=.true.
              if(cbc(2,ntemp).eq.2)then
                diagn(1,8,iel)=sje(1,1,2,ntemp)
                diagn(2,8,iel)=ijel(1,4,iel)
                ncon_edge(2,sje(1,1,2,ntemp))=.true.
                if_1_edge(8,iel)=.true.
              endif
              if(cbc(2,ntemp).eq.3.and.  &
     &          sje(1,1,2,ntemp).gt.iel)then
                diagn(1,8,iel)=sje(ijel(1,4,iel),2,3,ntemp)
              endif
            endif
          end if
        end if

!.......on face 2
        if(cb2.eq.0)then
          if(cb3.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(6,iel)=.true.
          endif
          if(cb5.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(7,iel)=.true.
          endif
        elseif(cb2.eq.1)then
          if(cb3.eq.2)then
            if(ijel(2,2,iel).eq.1)then
              newe(iel)=newe(iel)+1
              eassign(6,iel)=.true.
            else
              ntemp=sje(1,1,2,iel)
              if(cbc(3,ntemp).ne.3.or.  &
     &          sje(1,1,3,ntemp).gt.iel)then
                newe(iel)=newe(iel)+1
                eassign(6,iel)=.true.
                if(cbc(3,ntemp).eq.3.and.  &
     &            sje(1,1,3,ntemp).gt.iel)then
                  diagn(1,6,iel)=sje(ijel(1,2,iel),2,3,ntemp)
                endif
              endif
            endif
          elseif(cb3.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(6,iel)=.true.
          elseif(cb3.eq.1)then
            ntemp=sje(1,1,2,iel)
            if(cbc(3,ntemp).ne.3.or.sje(1,1,3,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(6,iel)=.true.
              if(cbc(3,ntemp).eq.2)then
                diagn(1,6,iel)=sje(1,1,3,ntemp)
                diagn(2,6,iel)=ijel(1,2,iel)
                ncon_edge(4,sje(1,1,3,ntemp))=.true.
                if_1_edge(6,iel)=.true.
              endif
              if(cbc(3,ntemp).eq.3.and.  &
     &          sje(1,1,3,ntemp).gt.iel)then
                diagn(1,6,iel)=sje(ijel(1,4,iel),2,3,ntemp)
              endif
            endif
          endif
          if(cb5.eq.2)then
            if(ijel(1,2,iel).eq.1)then
              newe(iel)=newe(iel)+1
              eassign(7,iel)=.true.
            else
              ntemp=sje(1,1,2,iel)
              if(cbc(5,ntemp).ne.3.or.sje(1,1,5,ntemp).gt.iel)then
                newe(iel)=newe(iel)+1
                eassign(7,iel)=.true.
                if(cbc(5,ntemp).eq.3.and.  &
     &            sje(1,1,5,ntemp).gt.iel)then
                  diagn(1,7,iel)=sje(ijel(2,2,iel),2,5,ntemp)
                endif
              endif
            endif
          elseif(cb5.eq.0)then
            newe(iel)=newe(iel)+1
            eassign(7,iel)=.true.
          elseif(cb5.eq.1)then
            ntemp=sje(1,1,2,iel)
            if(cbc(5,ntemp).ne.3.or.sje(1,1,5,ntemp).gt.iel)then
              newe(iel)=newe(iel)+1
              eassign(7,iel)=.true.
              if(cbc(5,ntemp).eq.2)then
                diagn(1,7,iel)=sje(1,1,5,ntemp)
                diagn(2,7,iel)=ijel(2,2,iel)
                ncon_edge(1,sje(1,1,5,ntemp))=.true.
                if_1_edge(7,iel)=.true.
              endif
              if(cbc(5,ntemp).eq.3.and.  &
     &          sje(1,1,5,ntemp).gt.iel)then
                diagn(1,7,iel)=sje(2,ijel(2,4,iel),5,ntemp)
              endif
            endif
          endif
        end if

!.......on face 1
        if(cb1.eq.1)then
          newe(iel)=newe(iel)+2
          eassign(2,iel)=.true.
          if(cb3.eq.1)then
            ntemp=sje(1,1,1,iel)
            if(cbc(3,ntemp).eq.2)then
              diagn(1,2,iel)=sje(1,1,3,ntemp)
              diagn(2,2,iel)=ijel(1,1,iel)
              ncon_edge(8,sje(1,1,3,ntemp))=.true.
              if_1_edge(2,iel)=.true.
            elseif(cbc(3,ntemp).eq.3)then
              diagn(1,2,iel)=sje(ijel(1,1,iel),1,3,ntemp)
            endif
          elseif(cb3.eq.2)then
            ntemp=sje(1,1,3,iel)
            if(ijel(2,1,iel).eq.2)then
              if(cbc(1,ntemp).eq.2)then
                diagn(1,2,iel)=sje(1,1,1,ntemp)
              end if
            endif
          end if

          eassign(3,iel)=.true.
          if(cb5.eq.1)then
            ntemp=sje(1,1,1,iel)
            if(cbc(5,ntemp).eq.2)then
              diagn(1,3,iel)=sje(1,1,5,ntemp)
              diagn(2,3,iel)=ijel(2,1,iel)
              ncon_edge(5,sje(1,1,5,ntemp))=.true.
              if_1_edge(3,iel)=.true.
            elseif(cbc(5,ntemp).eq.3)then
              diagn(1,3,iel)=sje(ijel(2,1,iel),1,5,ntemp)
            endif
          elseif(cb5.eq.2)then
            ntemp=sje(1,1,5,iel)
            if(ijel(1,1,iel).eq.2)then
              if(cbc(1,ntemp).eq.2)then
                diagn(1,3,iel)=sje(1,1,1,ntemp)
              end if
            endif
            
          end if
        elseif(cb1.eq.2)then
          if(cb3.eq.2)then
            ntemp=sje(1,1,1,iel)
            if(cbc(3,ntemp).ne.3)then
              newe(iel)=newe(iel)+1
              eassign(2,iel)=.true.
              if(cbc(3,ntemp).eq.2)then
                diagn(1,2,iel)=sje(1,1,3,ntemp)
              endif 
            endif
          elseif(cb3.eq.0.or.cb3.eq.1)then
            newe(iel)=newe(iel)+1
            eassign(2,iel)=.true.
            if(cb3.eq.1)then
              ntemp=sje(1,1,1,iel)
              if(cbc(3,ntemp).eq.2)then
                diagn(1,2,iel)=sje(1,1,3,ntemp)
              endif
            endif
          end if
          if(cb5.eq.2)then
            ntemp=sje(1,1,1,iel)
            if(cbc(5,ntemp).ne.3)then
              newe(iel)=newe(iel)+1
              eassign(3,iel)=.true.
              if(cbc(5,ntemp).eq.2)then
                diagn(1,3,iel)=sje(1,1,5,ntemp)
              endif
            endif
          elseif(cb5.eq.0.or.cb5.eq.1)then
            newe(iel)=newe(iel)+1
            eassign(3,iel)=.true.
            if(cb5.eq.1)then
              ntemp=sje(1,1,1,iel)
              if(cbc(5,ntemp).eq.2)then
                diagn(1,3,iel)=sje(1,1,5,ntemp)
              endif
            endif
          end if
        elseif(cb1.eq.0)then
          if(cb3.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(2,iel)=.true.
          endif
          if(cb5.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(3,iel)=.true.
          endif
        endif

!.......on face 3
        if(cb3.eq.1)then
          newe(iel)=newe(iel)+1
          eassign(10,iel)=.true.
          if(cb5.eq.1)then
            ntemp=sje(1,1,3,iel)
            if(cbc(5,ntemp).eq.2)then
              diagn(1,10,iel)=sje(1,1,5,ntemp)
              diagn(2,10,iel)=ijel(2,3,iel)
              ncon_edge(11,sje(1,1,5,ntemp))=.true.
              if_1_edge(10,iel)=.true.
            endif
          endif
          if(ijel(1,3,iel).eq.2)then
            ntemp=sje(1,1,3,iel)
            if(cbc(5,ntemp).eq.3)then
              diagn(1,10,iel)=sje(1,ijel(2,3,iel),5,ntemp)
            endif
          endif
        elseif(cb3.eq.2)then
          if(cb5.eq.2)then
            ntemp=sje(1,1,3,iel)
            if(cbc(5,ntemp).ne.3)then
              newe(iel)=newe(iel)+1
              eassign(10,iel)=.true.
              if(cbc(5,ntemp).eq.2)then
                diagn(1,10,iel)=sje(1,1,5,ntemp)
              endif
            endif
          elseif(cb5.eq.0.or.cb5.eq.1)then
            newe(iel)=newe(iel)+1
            eassign(10,iel)=.true.
            if(cb5.eq.1)then
              ntemp=sje(1,1,3,iel)
              if(cbc(5,ntemp).eq.2)then
                diagn(1,10,iel)=sje(1,1,5,ntemp)
              endif 
            endif
          end if
        elseif(cb3.eq.0)then
          if(cb5.ne.3)then
            newe(iel)=newe(iel)+1
            eassign(10,iel)=.true.
          endif
        endif

!       CONFORMING FACE INTERIOR

!.......find how many new mortar point indices will be assigned
!       to face interiors on all faces on each element

!.......newi record how many new face interior points will be assigned

!.......on face 6
        if(cb6.eq.1.or.cb6.eq.0)then
          newi(iel)=newi(iel)+9
          fassign(6,iel)=.true.
        end if
!.......on face 4
        if(cb4.eq.1.or.cb4.eq.0)then
          newi(iel)=newi(iel)+9
          fassign(4,iel)=.true.
        end if
!.......on face 2
        if(cb2.eq.1.or.cb2.eq.0)then
          newi(iel)=newi(iel)+9
          fassign(2,iel)=.true.
        end if
!.......on face 1
        if(cb1.ne.3)then
          newi(iel)=newi(iel)+9
          fassign(1,iel)=.true.
        end if
!.......on face 3
        if(cb3.ne.3)then
          newi(iel)=newi(iel)+9
          fassign(3,iel)=.true.
        endif
!.......on face 5
        if(cb5.ne.3)then
          newi(iel)=newi(iel)+9
          fassign(5,iel)=.true.
        endif

!.......newc is the total number of new mortar point indices
!       to be assigned to each element.
        newc(iel)=newe(iel)*3+newi(iel)
      end do
!$OMP END PARALLEL DO

!.....Compute (potentially in parallel) front(iel), which records how 
!     many new mortar point indices are to be assigned (to conforming 
!     edges and conforming face interiors) from element 1 to iel.
!     front(iel)=newc(1)+newc(2)+...+newc(iel)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel)
      do iel=1,nelt
        front(iel)=newc(iel)
      end do
!$OMP END PARALLEL DO

      call parallel_add(front)

!.....nmor is the total number or mortar points
      nmor=nvertex+front(nelt)

!.....Generate (potentially in parallel) new mortar point indices on 
!     each conforming element face. On each face, first visit all 
!     conforming edges, and then the face interior.

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(iel,count,i,cb1,ne,  &
!$OMP& space,ie,edge_g,face2,ie2,ntemp,ii,jj,jface,cb,mor_v)
      do iel=1,nelt
        front(iel)=front(iel)-newc(iel)
        count=nvertex+front(iel)
        do i=1,6
          cb1=cbc(i,iel)
          if (i.le.2) then
            ne=4
            space=1
          elseif (i.le.4)then
            ne=3
            space=2

!.........i loops over faces. Only 4 faces need to be examed for edge visit.
!         On face 1, edge 1,2,3 and 4 will be visited. On face 2, edge 5,6,7
!         and 8 will be visited. On face 3, edge 9 and 10 will be visited and
!         on face 4, edge 11 and 12 will be visited. The 12 edges can be 
!         covered by four faces, there is no need to visit edges on face
!         5 and 6.  So ne is set to be 0. 
!         However, i still needs to loop over 5 and 6, since the interiors
!         of face 5 and 6 still need to be visited.

          else
            ne=0
            space=1
          end if

          do ie=1,ne,space
            edge_g=edgenumber(ie,i)
            if(eassign(edge_g,iel))then
!.............generate the new mortar points index, mor_v
              call mor_assign(mor_v,count)
!.............assign mor_v to local edge ie of face i on element iel
              call mor_edge(ie,i,iel,mor_v)

!.............Since this edge is shared by another face of element 
!             iel, assign mor_v to the corresponding edge on the other 
!             face also.

!.............find the other face
              face2=f_e_ef(ie,i)
!.............find the local edge index of this edge on the other face
              ie2=localedgenumber(face2,edge_g)
!.............asssign mor_v  to local edge ie2 of face face2 on element iel
              call mor_edge(ie2,face2,iel,mor_v)

!.............There are some neighbor elements also sharing this edge. Assign
!             mor_v to neighbor element, neighbored by face i.
              if (cbc(i,iel).eq.2)then
                ntemp=sje(1,1,i,iel)
                call mor_edge(ie,jjface(i),ntemp,mor_v)
                call mor_edge(op(ie2),face2,ntemp,mor_v)
              end if

!.............assign mor_v  to neighbor element neighbored by face face2
              if (cbc(face2,iel).eq.2)then
                ntemp=sje(1,1,face2,iel)
                call mor_edge(ie2,jjface(face2),ntemp,mor_v)
                call mor_edge(op(ie),i,ntemp,mor_v)
              end if

!.............assign mor_v to neighbor element sharing this edge

!.............if the neighbor is of the same size of iel
              if(.not.if_1_edge(edgenumber(ie,i),iel))then
                if(diagn(1,edgenumber(ie,i),iel).ne.0)then
                  ntemp=diagn(1,edgenumber(ie,i),iel)
                  call mor_edge(op(ie2),jjface(face2),ntemp,mor_v)
                  call mor_edge(op(ie),jjface(i),ntemp,mor_v)
                endif

!.............if the neighbor has a size larger than iel's
              else
                if(diagn(1,edgenumber(ie,i),iel).ne.0)then
                  ntemp=diagn(1,edgenumber(ie,i),iel)
                  call mor_ne(mor_v,diagn(2,edgenumber(ie,i),iel),  &
     &            ie,i,ie2,face2,iel,ntemp)
                end if
              endif
 
            endif
          end do 

          if(fassign(i,iel))then
!...........generate new mortar points index in face interior. 
!           if face i is of type 2 or iel doesn't have a neighbor element,
!           assign new mortar point indices to interior mortar points
!           of face i of iel.
            cb=cbc(i,iel)
            if (cb.eq.1.or.cb.eq.0) then
              do jj =2,lx1-1
                do ii=2,lx1-1
                  count=count+1
                  idmo(ii,jj,1,1,i,iel)=count
                end do
              end do

!...........if face i is of type 2, assign new mortar point indices
!           to iel as well as to the neighboring element on face i
            elseif (cb.eq.2) then
              if (idmo(2,2,1,1,i,iel).eq.0) then
                ntemp=sje(1,1,i,iel)
                jface = jjface(i)
                do jj =2,lx1-1
                  do ii=2,lx1-1
                    count=count+1
                    idmo(ii,jj,1,1,i,iel)=count
                    idmo(ii,jj,1,1,jface,ntemp)=count
                  end do
                end do
              end if 
            end if
          end if
        end do
      end do 
!$OMP END  PARALLEL DO

 
!.....for edges on nonconforming faces, copy the mortar points indices
!     from neighbors.
!$OMP PARALLEL DO DEFAULT(SHARED)  &
!$OMP& PRIVATE(iel,i,cb,jface,iii,jjj,ntemp,ii,jj)
      do iel=1,nelt
        do i=1,6
          cb=cbc(i,iel)
          if (cb.eq.3) then
!...........edges 
            call edgecopy_s(i,iel)
          end if 

!.........face interior 

          jface = jjface(i)
          if (cb.eq.3) then
            do iii=1,2
              do jjj=1,2
                ntemp=sje(iii,jjj,i,iel) 
                do jj =1,lx1
                  do ii=1,lx1
                    idmo(ii,jj,iii,jjj,i,iel)=  &
     &                         idmo(ii,jj,1,1,jface,ntemp)
                  end do
                end do
                idmo(1,1,iii,jjj,i,iel)=idmo(1,1,1,1,jface,ntemp)
                idmo(lx1,1,iii,jjj,i,iel)=idmo(lx1,1,1,2,jface,ntemp)
                idmo(1,lx1,iii,jjj,i,iel)=idmo(1,lx1,2,1,jface,ntemp)
                idmo(lx1,lx1,iii,jjj,i,iel)=  &
     &                         idmo(lx1,lx1,2,2,jface,ntemp)
              end do
            end do
          end if
        end do
      end do
!$OMP END PARALLEL DO
      return
      end
       
!-----------------------------------------------------------------
       subroutine get_emo(ie,n,ng)
!-----------------------------------------------------------------
!      This subroutine fills array emo.
!      emo  records all elements sharing the same mortar point 
!                 (only applies to element vertices) .
!      emo(1,i,n) gives the element ID of the i'th element sharing
!                 mortar point n. (emo(1,i,n)=ie), ie is element
!                 index.
!      emo(2,i,n) gives the vertex index of mortar point n on this
!                 element (emo(2,i,n)=ng), ng is the vertex index.
!      nemo(n) records the total number of elements sharing mortar 
!                 point n.
!-----------------------------------------------------------------
 
       use ua_data
       implicit none

       integer ie, n, ntemp, i,ng

       do i=1,nemo(n)
         if (emo(1,i,n).eq.ie) return
       end do

!$     call omp_set_lock(tlock(n))
       ntemp=nemo(n)+1
       nemo(n)=ntemp
       emo(1,ntemp,n)=ie
       emo(2,ntemp,n)=ng
!$     call omp_unset_lock(tlock(n))

       return
       end 

!-----------------------------------------------------------------
      logical function ifsame(ntemp,j,iel,i)
!-----------------------------------------------------------------
!     Check whether the i's vertex of element iel is at the same
!     location as j's vertex of element ntemp.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      integer iel, i, ntemp, j

      ifsame=.false.
      if (ntemp.eq.0 .or. iel.eq.0) return
      if (xc(i,iel).eq.xc(j,ntemp).and.  &
     &    yc(i,iel).eq.yc(j,ntemp).and.  &
     &    zc(i,iel).eq.zc(j,ntemp)) then
        ifsame=.true.
      end if

      return
      end

!-----------------------------------------------------------------
      subroutine mor_assign(mor_v,count)
!-----------------------------------------------------------------
!     Assign three consecutive numbers for mor_v, which will
!     be assigned to the three interior points of an edge as the 
!     mortar point indices.
!-----------------------------------------------------------------
      
      implicit none
      integer mor_v(3),count,i
   
      do i=1,3 
        count=count+1
        mor_v(i)=count
      end do

      return
      end  
     
!-----------------------------------------------------------------
      subroutine mor_edge(ie,face,iel,mor_v)
!-----------------------------------------------------------------
!     Copy the mortar points index from mor_v to local 
!     edge ie of the face'th face on element iel.
!     The edge is conforming.
!-----------------------------------------------------------------

      use ua_data
      implicit none

      integer ie,i,iel,mor_v(3),j,nn,face

      if (ie.eq.1) then
        j=1
        do nn=2,lx1-1
          idmo(nn,j,1,1,face,iel)=mor_v(nn-1)
        end do
      elseif (ie.eq.2) then 
        i=lx1
        do nn=2,lx1-1
          idmo(i,nn,1,1,face,iel)=mor_v(nn-1)
        end do
      elseif (ie.eq.3) then 
        j=lx1
        do nn=2,lx1-1
          idmo(nn,j,1,1,face,iel)=mor_v(nn-1)
        end do
      elseif (ie.eq.4) then 
        i=1
        do nn=2,lx1-1
          idmo(i,nn,1,1,face,iel)=mor_v(nn-1)
        end do
      end if

      return
      end 

!------------------------------------------------------------
      subroutine edgecopy_s(face,iel)
!------------------------------------------------------------
!     Copy mortar points index on edges from neighbor elements 
!     to an element face of the 3rd type.
!------------------------------------------------------------

       use ua_data
       implicit none

       integer face, iel, ntemp1, ntemp2, ntemp3, ntemp4,  &
     &         edge_g, edge_l, face2, mor_s_v(4,2), i

!......find four neighbors on this face (3rd type)
       ntemp1=sje(1,1,face,iel)
       ntemp2=sje(1,2,face,iel)
       ntemp3=sje(2,1,face,iel)
       ntemp4=sje(2,2,face,iel)

!......local edge 1

!......mor_s_v is the array of mortar indices to  be copied.
       call nrzero(mor_s_v,4*2)
       do i=2,lx1-1
          mor_s_v(i-1,1)=idmo(i,1,1,1,jjface(face),ntemp1)
       end do
       mor_s_v(lx1-1,1)=idmo(lx1,1,1,2,jjface(face),ntemp1)
       do i=1,lx1-1
          mor_s_v(i,2)=idmo(i,1,1,1,jjface(face),ntemp2)
       end do

!......copy mor_s_v to local edge 1 on this face
       call mor_s_e(1,face,iel,mor_s_v)

!......copy mor_s_v to the corresponding edge on the other face sharing
!      local edge 1
       face2=f_e_ef(1,face)
       edge_g=edgenumber(1,face)
       edge_l=localedgenumber(face2,edge_g)
       call mor_s_e(edge_l,face2,iel,mor_s_v)

!......local edge 2
       do i=2,lx1-1
          mor_s_v(i-1,1)=idmo(lx1,i,1,1,jjface(face),ntemp2)
       end do
       mor_s_v(lx1-1,1)=idmo(lx1,lx1,2,2,jjface(face),ntemp2)

       mor_s_v(1,2)=idmo(lx1,1,1,2,jjface(face),ntemp4)
       do i=2,lx1-1
          mor_s_v(i,2)=idmo(lx1,i,1,1,jjface(face),ntemp4)
       end do

       call mor_s_e(2,face,iel,mor_s_v)
       face2=f_e_ef(2,face)
       edge_g=edgenumber(2,face)
       edge_l=localedgenumber(face2,edge_g)
       call mor_s_e(edge_l,face2,iel,mor_s_v)

!......local edge 3
       do i=2,lx1-1
          mor_s_v(i-1,1)=idmo(i,lx1,1,1,jjface(face),ntemp3)
       end do
       mor_s_v(lx1-1,1)=idmo(lx1,lx1,2,2,jjface(face),ntemp3)

       mor_s_v(1,2)=idmo(1,lx1,2,1,jjface(face),ntemp4)
       do i=2,lx1-1
          mor_s_v(i,2)=idmo(i,lx1,1,1,jjface(face),ntemp4)
       end do

       call mor_s_e(3,face,iel,mor_s_v)
       face2=f_e_ef(3,face)
       edge_g=edgenumber(3,face)
       edge_l=localedgenumber(face2,edge_g)
       call mor_s_e(edge_l,face2,iel,mor_s_v)

!......local edge 4
       do i=2,lx1-1
          mor_s_v(i-1,1)=idmo(1,i,1,1,jjface(face),ntemp1)
       end do
       mor_s_v(lx1-1,1)=idmo(1,lx1,2,1,jjface(face),ntemp1)

       do i=1,lx1-1
          mor_s_v(i,2)=idmo(1,i,1,1,jjface(face),ntemp3)
       end do

       call mor_s_e(4,face,iel,mor_s_v)
       face2=f_e_ef(4,face)
       edge_g=edgenumber(4,face)
       edge_l=localedgenumber(face2,edge_g)
       call mor_s_e(edge_l,face2,iel,mor_s_v)

       return
       end

!------------------------------------------------------------
       subroutine mor_s_e(n,face,iel,mor_s_v)
!------------------------------------------------------------
!      Copy mortar points index from mor_s_v to local edge n
!      on face "face" of element iel. The edge is nonconforming. 
!------------------------------------------------------------

       use ua_data
       implicit none

       integer n,face,iel,mor_s_v(4,2), i

       if (n.eq.1) then
         do i=2,lx1
           idmo(i,1,1,1,face,iel)=mor_s_v(i-1,1)
         end do
         do i=1,lx1-1
           idmo(i,1,1,2,face,iel)=mor_s_v(i,2)
         end do
       else if (n.eq.2) then
         do i=2,lx1
          idmo(lx1,i,1,2,face,iel)=mor_s_v(i-1,1)
         end do
         do i=1,lx1-1
          idmo(lx1,i,2,2,face,iel)=mor_s_v(i,2)
         end do
       else if (n.eq.3) then
         do i=2,lx1
           idmo(i,lx1,2,1,face,iel)=mor_s_v(i-1,1)
         end do
         do i=1,lx1-1
           idmo(i,lx1,2,2,face,iel)=mor_s_v(i,2)
         end do
       else if (n.eq.4) then
         do i=2,lx1
           idmo(1,i,1,1,face,iel)=mor_s_v(i-1,1)
         end do
         do i=1,lx1-1
           idmo(1,i,2,1,face,iel)=mor_s_v(i,2)
         end do
       end if
       return
       end

!------------------------------------------------------------
       subroutine mor_s_e_nn(n,face,iel,mor_s_v,nn)
!------------------------------------------------------------
!      Copy mortar point indices from mor_s_v to local edge n
!      on face "face" of element iel. nn is the edge mortar index,
!      which indicates that mor_s_v  corresponds to left/bottom or 
!      right/top part of the edge.
!------------------------------------------------------------

       use ua_data
       implicit none

       integer n,face,iel,mor_s_v(4), i,nn

       if (n.eq.1) then
         if(nn.eq.1)then
            do i=2,lx1
              idmo(i,1,1,1,face,iel)=mor_s_v(i-1)
            end do
         else
           do i=1,lx1-1
             idmo(i,1,1,2,face,iel)=mor_s_v(i)
           end do
         endif
       else if (n.eq.2) then
         if(nn.eq.1)then
           do i=2,lx1
            idmo(lx1,i,1,2,face,iel)=mor_s_v(i-1)
           end do
         else
           do i=1,lx1-1
            idmo(lx1,i,2,2,face,iel)=mor_s_v(i)
           end do
         endif
       else if (n.eq.3) then
         if(nn.eq.1)then
           do i=2,lx1
             idmo(i,lx1,2,1,face,iel)=mor_s_v(i-1)
           end do
         else
           do i=1,lx1-1
            idmo(i,lx1,2,2,face,iel)=mor_s_v(i)
           end do
         endif
       else if (n.eq.4) then
         if(nn.eq.1)then
           do i=2,lx1
            idmo(1,i,1,1,face,iel)=mor_s_v(i-1)
           end do
         else
           do i=1,lx1-1
            idmo(1,i,2,1,face,iel)=mor_s_v(i)
           end do
         endif
       end if
       return
       end


!---------------------------------------------------------------
      subroutine mortar_vertex(i,iel,count)
!---------------------------------------------------------------
!     Assign mortar point index "count" to iel's i'th vertex
!     and also to all elements sharing this vertex.
!---------------------------------------------------------------

      use ua_data
      implicit none

      integer i,iel,count,ntempx(8),ifntempx(8),lc_a(3),nnb(3),  &
     &        face_a(3),itemp,ntemp,ii, jj,j(3),  &
     &        iintempx(3),l,nbe, lc, temp
      logical ifsame,if_temp

      do l= 1,8
        ntempx(l)=0
        ifntempx(l)=0
      end do

!.....face_a records the three faces sharing this vertex on iel.
!     lc_a gives the local corner number of this vertex on each 
!     face in face_a.

      do l=1,3
        face_a(l)=f_c(l,i)
        lc_a(l)=local_corner(i,face_a(l))
      end do

!.....each vertex is shared by at most 8 elements. 
!     ntempx(j) gives the element index of a POSSIBLE element with its 
!               j'th  vertex is iel's i'th vertex
!     ifntempx(i)=ntempx(i) means  ntempx(i) exists 
!     ifntempx(i)=0 means ntempx(i) does not exist.

      ntempx(9-i)=iel
      ifntempx(9-i)=iel

!.....first find all elements sharing this vertex, ifntempx

!.....find the three possible neighbors of iel, neighbored by faces 
!     listed in array face_a

      do itemp= 1, 3

!.......j(itemp) is the local corner number of this vertex on the 
!       neighbor element on the corresponding face.
        j(itemp)=c_f(lc_a(itemp),jjface(face_a(itemp)))

!.......iitempx(itemp) records the vertex index of i on the
!       neighbor element, neighborned by face_a(itemp)
        iintempx(itemp)=cal_intempx(lc_a(itemp),face_a(itemp))

!.......ntemp refers the neighbor element 
        ntemp=0

!.......if the face is nonconforming, find out in which piece of the 
!       mortar the vertex is located
        ii=cal_iijj(1,lc_a(itemp))
        jj=cal_iijj(2,lc_a(itemp))
        ntemp=sje(ii,jj,face_a(itemp),iel)

!.......if the face is conforming
        if(ntemp.eq.0)then
          ntemp=sje(1,1,face_a(itemp),iel)
!.........find the possible neighbor        
          ntempx(iintempx(itemp))=ntemp
!.........check whether this possible neighbor is a real neighbor or not
          if(ntemp.ne.0)then
            if(ifsame(ntemp,j(itemp),iel,i))then
              ifntempx(iintempx(itemp))=ntemp
            end if
          end if

!.......if the face is nonconforming
        else
          if(ntemp.ne.0)then
            if(ifsame(ntemp,j(itemp),iel,i))then
              ifntempx(iintempx(itemp))=ntemp
              ntempx(iintempx(itemp))=ntemp
            end if
          end if
        end if 
      end do 

!.....find the possible three neighbors, neighbored by an edge only
      do l=1,3

!.....find first existing neighbor of any of the faces in array face_a
        if_temp=.false.
        if(l.eq.1)then
          if_temp=.true.
        elseif(l.eq.2)then
          if(ifntempx(iintempx(l-1)).eq.0)then
            if_temp=.true.
          end if
        elseif(l.eq.3)then
          if(ifntempx(iintempx(l-1)).eq.0  &
     &       .and.ifntempx(iintempx(l-2)).eq.0) then
            if_temp=.true.
          end if
        end if
           
        if(if_temp)then
          if (ifntempx(iintempx(l)).ne.0) then
            nbe=ifntempx(iintempx(l))
!...........if 1st neighor exists, check the neighbor's two neighbors in
!           the other two directions. 
!           e.g. if l=1, check directions 2 and 3,i.e. itemp=2,3,1
!           if l=2, itemp=3,1,-2
!           if l=3, itemp=1,2,1
!
            do itemp=face_l1(l),face_l2(l),face_ld(l)
!.............lc is the local corner number of this vertex on face face_a(itemp)
!             on the neighbor element of iel, neighbored by a face face_a(l)
              lc=local_corner(j(l),face_a(itemp))
!.............temp is the vertex index of this vertex on the neighbor element
!             neighbored by an edge
              temp=cal_intempx(lc,face_a(itemp))
              ii=cal_iijj(1,lc)
              jj=cal_iijj(2,lc)
              ntemp=sje(ii,jj,face_a(itemp),nbe)

!.............if the face face_a(itemp) is conforming
              if(ntemp.eq.0)then
                ntemp=sje(1,1,face_a(itemp),nbe)
                if(ntemp.ne.0)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &            nbe,j(l)))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
!...................nnb(itemp) records the neighbor element neighbored by an
!                   edge only
                    nnb(itemp)=ntemp
                  end if
                end if

!.............if the face face_a(itemp) is nonconforming
              else
                if(ntemp.ne.0)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &              nbe,j(l)))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
                    nnb(itemp)=ntemp
                  end if
                end if
              end if
            end do

!...........check the last neighbor element, neighbored by an edge

!...........ifntempx(iintempx(l)) has been visited in the above, now 
!           check another neighbor element(nbe) neighbored by a face 

!...........if the neighbor element is neighbored by face 
!           face_a(face_l1(l)) exists
            if(ifntempx(iintempx(face_l1(l))).ne.0)then
              nbe=ifntempx(iintempx(face_l1(l)))
!.............itemp is the last direction other than l and face_l1(l)
              itemp=face_l2(l)
              lc=local_corner(j(face_l1(l)),face_a(itemp))
              temp=cal_intempx(lc,face_a(itemp))
              ii=cal_iijj(1,lc)
              jj=cal_iijj(2,lc)

!.............ntemp records the last neighbor element neighbored by an edge
!             with element iel
              ntemp=sje(ii,jj,face_a(itemp),nbe)
!.............if conforming
              if(ntemp.eq.0)then
                ntemp=sje(1,1,face_a(itemp),nbe)
                if(ntemp.ne.0)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &              nbe,j(face_l1(l))))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
                    nnb(l)=ntemp
                  end if
                end if
!.............if nonconforming
              else
                if(ntemp.ne.0)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &            nbe,j(face_l1(l))))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
                    nnb(l)=ntemp
                  end if
                end if
              end if

!...........if the neighbor element neighbored by face face_a(face_l2(l)) 
!           does not exist
            elseif(ifntempx(iintempx(face_l2(l))).ne.0)then
              nbe=ifntempx(iintempx(face_l2(l)))
              itemp=face_l1(l)
              lc=local_corner(j(face_l2(l)),face_a(itemp))
              temp=cal_intempx(lc,face_a(itemp))
              ii=cal_iijj(1,lc)
              jj=cal_iijj(2,lc)
              ntemp=sje(ii,jj,face_a(itemp),nbe)
              if(ntemp.eq.0)then
                ntemp=sje(1,1,face_a(itemp),nbe)
                if(ntemp.ne.0)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &            nbe,j(face_l2(l))))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
                    nnb(l)=ntemp
                  end if
                end if
              else
                if(ntemp.ne.0.)then
                  if(ifsame(ntemp,c_f(lc,jjface(face_a(itemp))),  &
     &            nbe,j(face_l2(l))))then
                    ntempx(temp)=ntemp
                    ifntempx(temp)=ntemp
                    nnb(l)=ntemp
                  end if
                end if
              end if
            endif
          endif
        end if
      end do

!.....check the neighbor element, neighbored by a vertex only

!.....nnb are the three possible neighbor elements neighbored by an edge

      nnb(1)=ifntempx(cal_nnb(1,i))
      nnb(2)=ifntempx(cal_nnb(2,i))
      nnb(3)=ifntempx(cal_nnb(3,i))
      ntemp=0

!.....the neighbor element neighbored by a vertex must be a neighbor of
!     a valid(nonzero) nnb(i), neighbored by a face 

      if(nnb(1).ne.0)then
        lc=oplc(local_corner(i,face_a(3)))
        ii=cal_iijj(1,lc)
        jj=cal_iijj(2,lc)
!.......ntemp records the neighbor of iel, neighbored by vertex i 
        ntemp=sje(ii,jj,face_a(3),nnb(1))
!.......temp is the vertex index of i on ntemp
        temp=cal_intempx(lc,face_a(3))
        if(ntemp.eq.0)then
          ntemp=sje(1,1,face_a(3),nnb(1))
          if(ntemp.ne.0)then
            if(ifsame(ntemp,c_f(lc,jjface(face_a(3))),  &
     &         iel,i))then
              ntempx(temp)=ntemp
              ifntempx(temp)=ntemp
            end if
          end if
        else
          if(ntemp.ne.0)then
            if(ifsame(ntemp,c_f(lc,jjface(face_a(3))),  &
     &         iel,i))then
              ntempx(temp)=ntemp
              ifntempx(temp)=ntemp
            end if
          end if
        end if
      elseif(nnb(2).ne.0)then
        lc=oplc(local_corner(i,face_a(1)))
        ii=cal_iijj(1,lc)
        jj=cal_iijj(2,lc)
        ntemp=sje(ii,jj,face_a(1),nnb(2))
        temp=cal_intempx(lc,face_a(1))
        if(ntemp.eq.0)then
          ntemp=sje(1,1,face_a(1),nnb(2))
          if(ntemp.ne.0)then
            if(ifsame(ntemp,  &
     &        c_f(lc,jjface(face_a(1))),iel,i))then
              ntempx(temp)=ntemp
              ifntempx(temp)=ntemp
            end if
          end if
        else
          if(ntemp.ne.0)then
            if(ifsame(ntemp,  &
     &      c_f(lc,jjface(face_a(1))),iel,i))then
              ntempx(temp)=ntemp
              ifntempx(temp)=ntemp
            end if
          end if
        end if
      elseif(nnb(3).ne.0)then
        lc=oplc(local_corner(i,face_a(2)))
        ii=cal_iijj(1,lc)
        jj=cal_iijj(2,lc)
        ntemp=sje(ii,jj,face_a(2),nnb(3))
        temp=cal_intempx(lc, face_a(2))
        if(ntemp.eq.0)then
          ntemp=sje(1,1,face_a(2),nnb(3))
          if(ntemp.ne.0)then
            if(ifsame(ntemp,  &
     &         c_f(lc,jjface(face_a(2))),iel,i))then
              ifntempx(temp)=ntemp
              ntempx(temp)=ntemp
            end if
          end if
        else
          if(ntemp.ne.0)then
            if(ifsame(ntemp,  &
     &        c_f(lc,jjface(face_a(2))),iel,i))then
              ifntempx(temp)=ntemp
              ntempx(temp)=ntemp
            end if
          end if
        end if
      end if

!.....ifntempx records all elements sharing this vertex, assign count
!     to all these elements.

      if (ifntempx(1).ne.0) then
        idmo(lx1,lx1,2,2,1,ntempx(1))=count
        idmo(lx1,lx1,2,2,3,ntempx(1))=count
        idmo(lx1,lx1,2,2,5,ntempx(1))=count
        call get_emo(ntempx(1),count,8)
      end if

      if (ifntempx(2).ne.0) then
        idmo(lx1,lx1,2,2,2,ntempx(2))=count
        idmo(1,lx1,2,1,3,ntempx(2))=count
        idmo(1,lx1,2,1,5,ntempx(2))=count
        call get_emo(ntempx(2),count,7)
      end if

      if (ifntempx(3).ne.0) then
        idmo(1,lx1,2,1,1,ntempx(3))=count
        idmo(lx1,lx1,2,2,4,ntempx(3))=count
        idmo(lx1,1,1,2,5,ntempx(3))=count
        call get_emo(ntempx(3),count,6)
      end if
      if (ifntempx(4).ne.0) then
        idmo(1,lx1,2,1,2,ntempx(4))=count
        idmo(1,lx1,2,1,4,ntempx(4))=count
        idmo(1,1,1,1,5,ntempx(4))=count
        call get_emo(ntempx(4),count,5)
      end if

      if (ifntempx(5).ne.0) then
        idmo(lx1,1,1,2,1,ntempx(5))=count
        idmo(lx1,1,1,2,3,ntempx(5))=count
        idmo(lx1,lx1,2,2,6,ntempx(5))=count
        call get_emo(ntempx(5),count,4)
      end if


      if (ifntempx(6).ne.0) then
        idmo(lx1,1,1,2,2,ntempx(6))=count
        idmo(1,1,1,1,3,ntempx(6))=count
        idmo(1,lx1,2,1,6,ntempx(6))=count
        call get_emo(ntempx(6),count,3)
      end if

      if (ifntempx(7).ne.0) then
        idmo(1,1,1,1,1,ntempx(7))=count
        idmo(lx1,1,1,2,4,ntempx(7))=count
        idmo(lx1,1,1,2,6,ntempx(7))=count
        call get_emo(ntempx(7),count,2)
      end if

      if (ifntempx(8).ne.0) then
        idmo(1,1,1,1,2,ntempx(8))=count
        idmo(1,1,1,1,4,ntempx(8))=count
        idmo(1,1,1,1,6,ntempx(8))=count
        call get_emo(ntempx(8),count,1)
      end if

      return
      end

     
!---------------------------------------------------------------
      subroutine mor_ne(mor_v,nn,edge,face,edge2,face2,ntemp,iel)
!---------------------------------------------------------------
!     Copy the mortar points index  (mor_v + vertex mortar point) from
!     edge'th local edge on face'th face on element ntemp to iel.
!     ntemp is iel's neighbor, neighbored by this edge only. 
!     This subroutine is for the situation that iel is of larger
!     size than ntemp.  
!     face, face2 are face indices
!     edge and edge2 are local edge numbers of this edge on face and face2
!     nn is edge motar index, which indicate whether this edge
!     corresponds to the left/bottom or right/top part of the edge
!     on iel.
!---------------------------------------------------------------

      use ua_data
      implicit none

      integer mor_v(3),nn,edge,face,edge2,face2,ntemp,iel, i,  &
     &mor_s_v(4)

!.....get mor_s_v which is the mor_v + vertex mortar
      if (edge.eq.3) then
        if(nn.eq.1)then
          do i=2,lx1-1
            mor_s_v(i-1)=mor_v(i-1)
          end do
          mor_s_v(4)=idmo(lx1,lx1,2,2,face,ntemp)
        else
          mor_s_v(1)=idmo(1,lx1,2,1,face,ntemp)
          do i=2,lx1-1
            mor_s_v(i)=mor_v(i-1)
          end do
        endif
      
      elseif (edge.eq.4) then
        if(nn.eq.1)then
          do i=2,lx1-1
            mor_s_v(i-1)=mor_v(i-1)
          end do
          mor_s_v(4)=idmo(1,lx1,2,1,face,ntemp)
        else
          mor_s_v(1)=idmo(1,1,1,1,face,ntemp)
          do i=2,lx1-1
            mor_s_v(i)=mor_v(i-1)
          end do
        endif

      elseif (edge.eq.1) then
        if(nn.eq.1)then
          do i=2,lx1-1
            mor_s_v(i-1)=mor_v(i-1)
          end do
          mor_s_v(4)=idmo(lx1,1,1,2,face,ntemp)
        else
          mor_s_v(1)=idmo(1,1,1,1,face,ntemp)
          do i=2,lx1-1
            mor_s_v(i)=mor_v(i-1)
          end do
         endif

      else if (edge.eq.2) then
        if(nn.eq.1)then
          do i=2,lx1-1
             mor_s_v(i-1)=mor_v(i-1)
          end do
          mor_s_v(4)=idmo(lx1,lx1,2,2,face,ntemp)
        else
          mor_s_v(1)=idmo(lx1,1,1,2,face,ntemp)
          do i=2,lx1-1
             mor_s_v(i)=mor_v(i-1)
          end do
        endif
      end if

!.....copy mor_s_v to iel's local edge(op(edge)), on face jjface(face)
      call mor_s_e_nn(op(edge),jjface(face),iel,mor_s_v,nn)
!.....copy mor_s_v to iel's local edge(op(edge2)), on face jjface(face2)
!     since this edge is shared by two faces on iel
      call mor_s_e_nn(op(edge2),jjface(face2),iel,mor_s_v,nn)

      return
      end

