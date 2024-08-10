
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine matvec_sub(ablock,avec,bvec)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     subtracts bvec=bvec - ablock*avec
!---------------------------------------------------------------------

      implicit none
      include 'blk_par.h'

      double precision ablock,avec,bvec
      dimension ablock(blkdim,5,5),avec(blkdim,5),bvec(blkdim,5)

      integer i

!---------------------------------------------------------------------
!            rhs(i,ic,jc,kc) = rhs(i,ic,jc,kc) 
!     $           - lhs(i,1,ablock,ia)*
!---------------------------------------------------------------------
!dir$ vector always
      do i = 1, bsize
         bvec(i,1) = bvec(i,1) - ablock(i,1,1)*avec(i,1)  &
     &                     - ablock(i,1,2)*avec(i,2)  &
     &                     - ablock(i,1,3)*avec(i,3)  &
     &                     - ablock(i,1,4)*avec(i,4)  &
     &                     - ablock(i,1,5)*avec(i,5)
         bvec(i,2) = bvec(i,2) - ablock(i,2,1)*avec(i,1)  &
     &                     - ablock(i,2,2)*avec(i,2)  &
     &                     - ablock(i,2,3)*avec(i,3)  &
     &                     - ablock(i,2,4)*avec(i,4)  &
     &                     - ablock(i,2,5)*avec(i,5)
         bvec(i,3) = bvec(i,3) - ablock(i,3,1)*avec(i,1)  &
     &                     - ablock(i,3,2)*avec(i,2)  &
     &                     - ablock(i,3,3)*avec(i,3)  &
     &                     - ablock(i,3,4)*avec(i,4)  &
     &                     - ablock(i,3,5)*avec(i,5)
         bvec(i,4) = bvec(i,4) - ablock(i,4,1)*avec(i,1)  &
     &                     - ablock(i,4,2)*avec(i,2)  &
     &                     - ablock(i,4,3)*avec(i,3)  &
     &                     - ablock(i,4,4)*avec(i,4)  &
     &                     - ablock(i,4,5)*avec(i,5)
         bvec(i,5) = bvec(i,5) - ablock(i,5,1)*avec(i,1)  &
     &                     - ablock(i,5,2)*avec(i,2)  &
     &                     - ablock(i,5,3)*avec(i,3)  &
     &                     - ablock(i,5,4)*avec(i,4)  &
     &                     - ablock(i,5,5)*avec(i,5)
       end do


      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine matmul_sub(ablock, bblock, cblock)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     subtracts a(i,j,k) X b(i,j,k) from c(i,j,k)
!---------------------------------------------------------------------

      implicit none
      include 'blk_par.h'

      double precision ablock, bblock, cblock
      dimension ablock(blkdim,5,5), bblock(blkdim,5,5),   &
     &          cblock(blkdim,5,5)

      integer i

!dir$ vector always
      do i = 1, bsize
         cblock(i,1,1) = cblock(i,1,1) - ablock(i,1,1)*bblock(i,1,1)  &
     &                             - ablock(i,1,2)*bblock(i,2,1)  &
     &                             - ablock(i,1,3)*bblock(i,3,1)  &
     &                             - ablock(i,1,4)*bblock(i,4,1)  &
     &                             - ablock(i,1,5)*bblock(i,5,1)
         cblock(i,2,1) = cblock(i,2,1) - ablock(i,2,1)*bblock(i,1,1)  &
     &                             - ablock(i,2,2)*bblock(i,2,1)  &
     &                             - ablock(i,2,3)*bblock(i,3,1)  &
     &                             - ablock(i,2,4)*bblock(i,4,1)  &
     &                             - ablock(i,2,5)*bblock(i,5,1)
         cblock(i,3,1) = cblock(i,3,1) - ablock(i,3,1)*bblock(i,1,1)  &
     &                             - ablock(i,3,2)*bblock(i,2,1)  &
     &                             - ablock(i,3,3)*bblock(i,3,1)  &
     &                             - ablock(i,3,4)*bblock(i,4,1)  &
     &                             - ablock(i,3,5)*bblock(i,5,1)
         cblock(i,4,1) = cblock(i,4,1) - ablock(i,4,1)*bblock(i,1,1)  &
     &                             - ablock(i,4,2)*bblock(i,2,1)  &
     &                             - ablock(i,4,3)*bblock(i,3,1)  &
     &                             - ablock(i,4,4)*bblock(i,4,1)  &
     &                             - ablock(i,4,5)*bblock(i,5,1)
         cblock(i,5,1) = cblock(i,5,1) - ablock(i,5,1)*bblock(i,1,1)  &
     &                             - ablock(i,5,2)*bblock(i,2,1)  &
     &                             - ablock(i,5,3)*bblock(i,3,1)  &
     &                             - ablock(i,5,4)*bblock(i,4,1)  &
     &                             - ablock(i,5,5)*bblock(i,5,1)
         cblock(i,1,2) = cblock(i,1,2) - ablock(i,1,1)*bblock(i,1,2)  &
     &                             - ablock(i,1,2)*bblock(i,2,2)  &
     &                             - ablock(i,1,3)*bblock(i,3,2)  &
     &                             - ablock(i,1,4)*bblock(i,4,2)  &
     &                             - ablock(i,1,5)*bblock(i,5,2)
         cblock(i,2,2) = cblock(i,2,2) - ablock(i,2,1)*bblock(i,1,2)  &
     &                             - ablock(i,2,2)*bblock(i,2,2)  &
     &                             - ablock(i,2,3)*bblock(i,3,2)  &
     &                             - ablock(i,2,4)*bblock(i,4,2)  &
     &                             - ablock(i,2,5)*bblock(i,5,2)
         cblock(i,3,2) = cblock(i,3,2) - ablock(i,3,1)*bblock(i,1,2)  &
     &                             - ablock(i,3,2)*bblock(i,2,2)  &
     &                             - ablock(i,3,3)*bblock(i,3,2)  &
     &                             - ablock(i,3,4)*bblock(i,4,2)  &
     &                             - ablock(i,3,5)*bblock(i,5,2)
         cblock(i,4,2) = cblock(i,4,2) - ablock(i,4,1)*bblock(i,1,2)  &
     &                             - ablock(i,4,2)*bblock(i,2,2)  &
     &                             - ablock(i,4,3)*bblock(i,3,2)  &
     &                             - ablock(i,4,4)*bblock(i,4,2)  &
     &                             - ablock(i,4,5)*bblock(i,5,2)
         cblock(i,5,2) = cblock(i,5,2) - ablock(i,5,1)*bblock(i,1,2)  &
     &                             - ablock(i,5,2)*bblock(i,2,2)  &
     &                             - ablock(i,5,3)*bblock(i,3,2)  &
     &                             - ablock(i,5,4)*bblock(i,4,2)  &
     &                             - ablock(i,5,5)*bblock(i,5,2)
         cblock(i,1,3) = cblock(i,1,3) - ablock(i,1,1)*bblock(i,1,3)  &
     &                             - ablock(i,1,2)*bblock(i,2,3)  &
     &                             - ablock(i,1,3)*bblock(i,3,3)  &
     &                             - ablock(i,1,4)*bblock(i,4,3)  &
     &                             - ablock(i,1,5)*bblock(i,5,3)
         cblock(i,2,3) = cblock(i,2,3) - ablock(i,2,1)*bblock(i,1,3)  &
     &                             - ablock(i,2,2)*bblock(i,2,3)  &
     &                             - ablock(i,2,3)*bblock(i,3,3)  &
     &                             - ablock(i,2,4)*bblock(i,4,3)  &
     &                             - ablock(i,2,5)*bblock(i,5,3)
         cblock(i,3,3) = cblock(i,3,3) - ablock(i,3,1)*bblock(i,1,3)  &
     &                             - ablock(i,3,2)*bblock(i,2,3)  &
     &                             - ablock(i,3,3)*bblock(i,3,3)  &
     &                             - ablock(i,3,4)*bblock(i,4,3)  &
     &                             - ablock(i,3,5)*bblock(i,5,3)
         cblock(i,4,3) = cblock(i,4,3) - ablock(i,4,1)*bblock(i,1,3)  &
     &                             - ablock(i,4,2)*bblock(i,2,3)  &
     &                             - ablock(i,4,3)*bblock(i,3,3)  &
     &                             - ablock(i,4,4)*bblock(i,4,3)  &
     &                             - ablock(i,4,5)*bblock(i,5,3)
         cblock(i,5,3) = cblock(i,5,3) - ablock(i,5,1)*bblock(i,1,3)  &
     &                             - ablock(i,5,2)*bblock(i,2,3)  &
     &                             - ablock(i,5,3)*bblock(i,3,3)  &
     &                             - ablock(i,5,4)*bblock(i,4,3)  &
     &                             - ablock(i,5,5)*bblock(i,5,3)
         cblock(i,1,4) = cblock(i,1,4) - ablock(i,1,1)*bblock(i,1,4)  &
     &                             - ablock(i,1,2)*bblock(i,2,4)  &
     &                             - ablock(i,1,3)*bblock(i,3,4)  &
     &                             - ablock(i,1,4)*bblock(i,4,4)  &
     &                             - ablock(i,1,5)*bblock(i,5,4)
         cblock(i,2,4) = cblock(i,2,4) - ablock(i,2,1)*bblock(i,1,4)  &
     &                             - ablock(i,2,2)*bblock(i,2,4)  &
     &                             - ablock(i,2,3)*bblock(i,3,4)  &
     &                             - ablock(i,2,4)*bblock(i,4,4)  &
     &                             - ablock(i,2,5)*bblock(i,5,4)
         cblock(i,3,4) = cblock(i,3,4) - ablock(i,3,1)*bblock(i,1,4)  &
     &                             - ablock(i,3,2)*bblock(i,2,4)  &
     &                             - ablock(i,3,3)*bblock(i,3,4)  &
     &                             - ablock(i,3,4)*bblock(i,4,4)  &
     &                             - ablock(i,3,5)*bblock(i,5,4)
         cblock(i,4,4) = cblock(i,4,4) - ablock(i,4,1)*bblock(i,1,4)  &
     &                             - ablock(i,4,2)*bblock(i,2,4)  &
     &                             - ablock(i,4,3)*bblock(i,3,4)  &
     &                             - ablock(i,4,4)*bblock(i,4,4)  &
     &                             - ablock(i,4,5)*bblock(i,5,4)
         cblock(i,5,4) = cblock(i,5,4) - ablock(i,5,1)*bblock(i,1,4)  &
     &                             - ablock(i,5,2)*bblock(i,2,4)  &
     &                             - ablock(i,5,3)*bblock(i,3,4)  &
     &                             - ablock(i,5,4)*bblock(i,4,4)  &
     &                             - ablock(i,5,5)*bblock(i,5,4)
         cblock(i,1,5) = cblock(i,1,5) - ablock(i,1,1)*bblock(i,1,5)  &
     &                             - ablock(i,1,2)*bblock(i,2,5)  &
     &                             - ablock(i,1,3)*bblock(i,3,5)  &
     &                             - ablock(i,1,4)*bblock(i,4,5)  &
     &                             - ablock(i,1,5)*bblock(i,5,5)
         cblock(i,2,5) = cblock(i,2,5) - ablock(i,2,1)*bblock(i,1,5)  &
     &                             - ablock(i,2,2)*bblock(i,2,5)  &
     &                             - ablock(i,2,3)*bblock(i,3,5)  &
     &                             - ablock(i,2,4)*bblock(i,4,5)  &
     &                             - ablock(i,2,5)*bblock(i,5,5)
         cblock(i,3,5) = cblock(i,3,5) - ablock(i,3,1)*bblock(i,1,5)  &
     &                             - ablock(i,3,2)*bblock(i,2,5)  &
     &                             - ablock(i,3,3)*bblock(i,3,5)  &
     &                             - ablock(i,3,4)*bblock(i,4,5)  &
     &                             - ablock(i,3,5)*bblock(i,5,5)
         cblock(i,4,5) = cblock(i,4,5) - ablock(i,4,1)*bblock(i,1,5)  &
     &                             - ablock(i,4,2)*bblock(i,2,5)  &
     &                             - ablock(i,4,3)*bblock(i,3,5)  &
     &                             - ablock(i,4,4)*bblock(i,4,5)  &
     &                             - ablock(i,4,5)*bblock(i,5,5)
         cblock(i,5,5) = cblock(i,5,5) - ablock(i,5,1)*bblock(i,1,5)  &
     &                             - ablock(i,5,2)*bblock(i,2,5)  &
     &                             - ablock(i,5,3)*bblock(i,3,5)  &
     &                             - ablock(i,5,4)*bblock(i,4,5)  &
     &                             - ablock(i,5,5)*bblock(i,5,5)
      end do
              
      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine binvcrhs( lhs,c,r )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------

      implicit none
      include 'blk_par.h'

      double precision pivot, coeff, lhs
      dimension lhs(blkdim,5,5)
      double precision c(blkdim,5,5), r(blkdim,5)

      integer i

!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------

!dir$ vector always
      do i = 1, bsize
      pivot = 1.00d0/lhs(i,1,1)
      lhs(i,1,2) = lhs(i,1,2)*pivot
      lhs(i,1,3) = lhs(i,1,3)*pivot
      lhs(i,1,4) = lhs(i,1,4)*pivot
      lhs(i,1,5) = lhs(i,1,5)*pivot
      c(i,1,1) = c(i,1,1)*pivot
      c(i,1,2) = c(i,1,2)*pivot
      c(i,1,3) = c(i,1,3)*pivot
      c(i,1,4) = c(i,1,4)*pivot
      c(i,1,5) = c(i,1,5)*pivot
      r(i,1)   = r(i,1)  *pivot

      coeff = lhs(i,2,1)
      lhs(i,2,2)= lhs(i,2,2) - coeff*lhs(i,1,2)
      lhs(i,2,3)= lhs(i,2,3) - coeff*lhs(i,1,3)
      lhs(i,2,4)= lhs(i,2,4) - coeff*lhs(i,1,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,1,5)
      c(i,2,1) = c(i,2,1) - coeff*c(i,1,1)
      c(i,2,2) = c(i,2,2) - coeff*c(i,1,2)
      c(i,2,3) = c(i,2,3) - coeff*c(i,1,3)
      c(i,2,4) = c(i,2,4) - coeff*c(i,1,4)
      c(i,2,5) = c(i,2,5) - coeff*c(i,1,5)
      r(i,2)   = r(i,2)   - coeff*r(i,1)

      coeff = lhs(i,3,1)
      lhs(i,3,2)= lhs(i,3,2) - coeff*lhs(i,1,2)
      lhs(i,3,3)= lhs(i,3,3) - coeff*lhs(i,1,3)
      lhs(i,3,4)= lhs(i,3,4) - coeff*lhs(i,1,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,1,5)
      c(i,3,1) = c(i,3,1) - coeff*c(i,1,1)
      c(i,3,2) = c(i,3,2) - coeff*c(i,1,2)
      c(i,3,3) = c(i,3,3) - coeff*c(i,1,3)
      c(i,3,4) = c(i,3,4) - coeff*c(i,1,4)
      c(i,3,5) = c(i,3,5) - coeff*c(i,1,5)
      r(i,3)   = r(i,3)   - coeff*r(i,1)

      coeff = lhs(i,4,1)
      lhs(i,4,2)= lhs(i,4,2) - coeff*lhs(i,1,2)
      lhs(i,4,3)= lhs(i,4,3) - coeff*lhs(i,1,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,1,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,1,5)
      c(i,4,1) = c(i,4,1) - coeff*c(i,1,1)
      c(i,4,2) = c(i,4,2) - coeff*c(i,1,2)
      c(i,4,3) = c(i,4,3) - coeff*c(i,1,3)
      c(i,4,4) = c(i,4,4) - coeff*c(i,1,4)
      c(i,4,5) = c(i,4,5) - coeff*c(i,1,5)
      r(i,4)   = r(i,4)   - coeff*r(i,1)

      coeff = lhs(i,5,1)
      lhs(i,5,2)= lhs(i,5,2) - coeff*lhs(i,1,2)
      lhs(i,5,3)= lhs(i,5,3) - coeff*lhs(i,1,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,1,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,1,5)
      c(i,5,1) = c(i,5,1) - coeff*c(i,1,1)
      c(i,5,2) = c(i,5,2) - coeff*c(i,1,2)
      c(i,5,3) = c(i,5,3) - coeff*c(i,1,3)
      c(i,5,4) = c(i,5,4) - coeff*c(i,1,4)
      c(i,5,5) = c(i,5,5) - coeff*c(i,1,5)
      r(i,5)   = r(i,5)   - coeff*r(i,1)


      pivot = 1.00d0/lhs(i,2,2)
      lhs(i,2,3) = lhs(i,2,3)*pivot
      lhs(i,2,4) = lhs(i,2,4)*pivot
      lhs(i,2,5) = lhs(i,2,5)*pivot
      c(i,2,1) = c(i,2,1)*pivot
      c(i,2,2) = c(i,2,2)*pivot
      c(i,2,3) = c(i,2,3)*pivot
      c(i,2,4) = c(i,2,4)*pivot
      c(i,2,5) = c(i,2,5)*pivot
      r(i,2)   = r(i,2)  *pivot

      coeff = lhs(i,1,2)
      lhs(i,1,3)= lhs(i,1,3) - coeff*lhs(i,2,3)
      lhs(i,1,4)= lhs(i,1,4) - coeff*lhs(i,2,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,2,5)
      c(i,1,1) = c(i,1,1) - coeff*c(i,2,1)
      c(i,1,2) = c(i,1,2) - coeff*c(i,2,2)
      c(i,1,3) = c(i,1,3) - coeff*c(i,2,3)
      c(i,1,4) = c(i,1,4) - coeff*c(i,2,4)
      c(i,1,5) = c(i,1,5) - coeff*c(i,2,5)
      r(i,1)   = r(i,1)   - coeff*r(i,2)

      coeff = lhs(i,3,2)
      lhs(i,3,3)= lhs(i,3,3) - coeff*lhs(i,2,3)
      lhs(i,3,4)= lhs(i,3,4) - coeff*lhs(i,2,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,2,5)
      c(i,3,1) = c(i,3,1) - coeff*c(i,2,1)
      c(i,3,2) = c(i,3,2) - coeff*c(i,2,2)
      c(i,3,3) = c(i,3,3) - coeff*c(i,2,3)
      c(i,3,4) = c(i,3,4) - coeff*c(i,2,4)
      c(i,3,5) = c(i,3,5) - coeff*c(i,2,5)
      r(i,3)   = r(i,3)   - coeff*r(i,2)

      coeff = lhs(i,4,2)
      lhs(i,4,3)= lhs(i,4,3) - coeff*lhs(i,2,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,2,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,2,5)
      c(i,4,1) = c(i,4,1) - coeff*c(i,2,1)
      c(i,4,2) = c(i,4,2) - coeff*c(i,2,2)
      c(i,4,3) = c(i,4,3) - coeff*c(i,2,3)
      c(i,4,4) = c(i,4,4) - coeff*c(i,2,4)
      c(i,4,5) = c(i,4,5) - coeff*c(i,2,5)
      r(i,4)   = r(i,4)   - coeff*r(i,2)

      coeff = lhs(i,5,2)
      lhs(i,5,3)= lhs(i,5,3) - coeff*lhs(i,2,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,2,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,2,5)
      c(i,5,1) = c(i,5,1) - coeff*c(i,2,1)
      c(i,5,2) = c(i,5,2) - coeff*c(i,2,2)
      c(i,5,3) = c(i,5,3) - coeff*c(i,2,3)
      c(i,5,4) = c(i,5,4) - coeff*c(i,2,4)
      c(i,5,5) = c(i,5,5) - coeff*c(i,2,5)
      r(i,5)   = r(i,5)   - coeff*r(i,2)


      pivot = 1.00d0/lhs(i,3,3)
      lhs(i,3,4) = lhs(i,3,4)*pivot
      lhs(i,3,5) = lhs(i,3,5)*pivot
      c(i,3,1) = c(i,3,1)*pivot
      c(i,3,2) = c(i,3,2)*pivot
      c(i,3,3) = c(i,3,3)*pivot
      c(i,3,4) = c(i,3,4)*pivot
      c(i,3,5) = c(i,3,5)*pivot
      r(i,3)   = r(i,3)  *pivot

      coeff = lhs(i,1,3)
      lhs(i,1,4)= lhs(i,1,4) - coeff*lhs(i,3,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,3,5)
      c(i,1,1) = c(i,1,1) - coeff*c(i,3,1)
      c(i,1,2) = c(i,1,2) - coeff*c(i,3,2)
      c(i,1,3) = c(i,1,3) - coeff*c(i,3,3)
      c(i,1,4) = c(i,1,4) - coeff*c(i,3,4)
      c(i,1,5) = c(i,1,5) - coeff*c(i,3,5)
      r(i,1)   = r(i,1)   - coeff*r(i,3)

      coeff = lhs(i,2,3)
      lhs(i,2,4)= lhs(i,2,4) - coeff*lhs(i,3,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,3,5)
      c(i,2,1) = c(i,2,1) - coeff*c(i,3,1)
      c(i,2,2) = c(i,2,2) - coeff*c(i,3,2)
      c(i,2,3) = c(i,2,3) - coeff*c(i,3,3)
      c(i,2,4) = c(i,2,4) - coeff*c(i,3,4)
      c(i,2,5) = c(i,2,5) - coeff*c(i,3,5)
      r(i,2)   = r(i,2)   - coeff*r(i,3)

      coeff = lhs(i,4,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,3,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,3,5)
      c(i,4,1) = c(i,4,1) - coeff*c(i,3,1)
      c(i,4,2) = c(i,4,2) - coeff*c(i,3,2)
      c(i,4,3) = c(i,4,3) - coeff*c(i,3,3)
      c(i,4,4) = c(i,4,4) - coeff*c(i,3,4)
      c(i,4,5) = c(i,4,5) - coeff*c(i,3,5)
      r(i,4)   = r(i,4)   - coeff*r(i,3)

      coeff = lhs(i,5,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,3,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,3,5)
      c(i,5,1) = c(i,5,1) - coeff*c(i,3,1)
      c(i,5,2) = c(i,5,2) - coeff*c(i,3,2)
      c(i,5,3) = c(i,5,3) - coeff*c(i,3,3)
      c(i,5,4) = c(i,5,4) - coeff*c(i,3,4)
      c(i,5,5) = c(i,5,5) - coeff*c(i,3,5)
      r(i,5)   = r(i,5)   - coeff*r(i,3)


      pivot = 1.00d0/lhs(i,4,4)
      lhs(i,4,5) = lhs(i,4,5)*pivot
      c(i,4,1) = c(i,4,1)*pivot
      c(i,4,2) = c(i,4,2)*pivot
      c(i,4,3) = c(i,4,3)*pivot
      c(i,4,4) = c(i,4,4)*pivot
      c(i,4,5) = c(i,4,5)*pivot
      r(i,4)   = r(i,4)  *pivot

      coeff = lhs(i,1,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,4,5)
      c(i,1,1) = c(i,1,1) - coeff*c(i,4,1)
      c(i,1,2) = c(i,1,2) - coeff*c(i,4,2)
      c(i,1,3) = c(i,1,3) - coeff*c(i,4,3)
      c(i,1,4) = c(i,1,4) - coeff*c(i,4,4)
      c(i,1,5) = c(i,1,5) - coeff*c(i,4,5)
      r(i,1)   = r(i,1)   - coeff*r(i,4)

      coeff = lhs(i,2,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,4,5)
      c(i,2,1) = c(i,2,1) - coeff*c(i,4,1)
      c(i,2,2) = c(i,2,2) - coeff*c(i,4,2)
      c(i,2,3) = c(i,2,3) - coeff*c(i,4,3)
      c(i,2,4) = c(i,2,4) - coeff*c(i,4,4)
      c(i,2,5) = c(i,2,5) - coeff*c(i,4,5)
      r(i,2)   = r(i,2)   - coeff*r(i,4)

      coeff = lhs(i,3,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,4,5)
      c(i,3,1) = c(i,3,1) - coeff*c(i,4,1)
      c(i,3,2) = c(i,3,2) - coeff*c(i,4,2)
      c(i,3,3) = c(i,3,3) - coeff*c(i,4,3)
      c(i,3,4) = c(i,3,4) - coeff*c(i,4,4)
      c(i,3,5) = c(i,3,5) - coeff*c(i,4,5)
      r(i,3)   = r(i,3)   - coeff*r(i,4)

      coeff = lhs(i,5,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,4,5)
      c(i,5,1) = c(i,5,1) - coeff*c(i,4,1)
      c(i,5,2) = c(i,5,2) - coeff*c(i,4,2)
      c(i,5,3) = c(i,5,3) - coeff*c(i,4,3)
      c(i,5,4) = c(i,5,4) - coeff*c(i,4,4)
      c(i,5,5) = c(i,5,5) - coeff*c(i,4,5)
      r(i,5)   = r(i,5)   - coeff*r(i,4)


      pivot = 1.00d0/lhs(i,5,5)
      c(i,5,1) = c(i,5,1)*pivot
      c(i,5,2) = c(i,5,2)*pivot
      c(i,5,3) = c(i,5,3)*pivot
      c(i,5,4) = c(i,5,4)*pivot
      c(i,5,5) = c(i,5,5)*pivot
      r(i,5)   = r(i,5)  *pivot

      coeff = lhs(i,1,5)
      c(i,1,1) = c(i,1,1) - coeff*c(i,5,1)
      c(i,1,2) = c(i,1,2) - coeff*c(i,5,2)
      c(i,1,3) = c(i,1,3) - coeff*c(i,5,3)
      c(i,1,4) = c(i,1,4) - coeff*c(i,5,4)
      c(i,1,5) = c(i,1,5) - coeff*c(i,5,5)
      r(i,1)   = r(i,1)   - coeff*r(i,5)

      coeff = lhs(i,2,5)
      c(i,2,1) = c(i,2,1) - coeff*c(i,5,1)
      c(i,2,2) = c(i,2,2) - coeff*c(i,5,2)
      c(i,2,3) = c(i,2,3) - coeff*c(i,5,3)
      c(i,2,4) = c(i,2,4) - coeff*c(i,5,4)
      c(i,2,5) = c(i,2,5) - coeff*c(i,5,5)
      r(i,2)   = r(i,2)   - coeff*r(i,5)

      coeff = lhs(i,3,5)
      c(i,3,1) = c(i,3,1) - coeff*c(i,5,1)
      c(i,3,2) = c(i,3,2) - coeff*c(i,5,2)
      c(i,3,3) = c(i,3,3) - coeff*c(i,5,3)
      c(i,3,4) = c(i,3,4) - coeff*c(i,5,4)
      c(i,3,5) = c(i,3,5) - coeff*c(i,5,5)
      r(i,3)   = r(i,3)   - coeff*r(i,5)

      coeff = lhs(i,4,5)
      c(i,4,1) = c(i,4,1) - coeff*c(i,5,1)
      c(i,4,2) = c(i,4,2) - coeff*c(i,5,2)
      c(i,4,3) = c(i,4,3) - coeff*c(i,5,3)
      c(i,4,4) = c(i,4,4) - coeff*c(i,5,4)
      c(i,4,5) = c(i,4,5) - coeff*c(i,5,5)
      r(i,4)   = r(i,4)   - coeff*r(i,5)
      end do


      return
      end



!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine binvrhs( lhs,r )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------

      implicit none
      include 'blk_par.h'

      double precision pivot, coeff, lhs
      dimension lhs(blkdim,5,5)
      double precision r(blkdim,5)

      integer i

!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------

!dir$ vector always
      do i = 1, bsize

      pivot = 1.00d0/lhs(i,1,1)
      lhs(i,1,2) = lhs(i,1,2)*pivot
      lhs(i,1,3) = lhs(i,1,3)*pivot
      lhs(i,1,4) = lhs(i,1,4)*pivot
      lhs(i,1,5) = lhs(i,1,5)*pivot
      r(i,1)   = r(i,1)  *pivot

      coeff = lhs(i,2,1)
      lhs(i,2,2)= lhs(i,2,2) - coeff*lhs(i,1,2)
      lhs(i,2,3)= lhs(i,2,3) - coeff*lhs(i,1,3)
      lhs(i,2,4)= lhs(i,2,4) - coeff*lhs(i,1,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,1,5)
      r(i,2)   = r(i,2)   - coeff*r(i,1)

      coeff = lhs(i,3,1)
      lhs(i,3,2)= lhs(i,3,2) - coeff*lhs(i,1,2)
      lhs(i,3,3)= lhs(i,3,3) - coeff*lhs(i,1,3)
      lhs(i,3,4)= lhs(i,3,4) - coeff*lhs(i,1,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,1,5)
      r(i,3)   = r(i,3)   - coeff*r(i,1)

      coeff = lhs(i,4,1)
      lhs(i,4,2)= lhs(i,4,2) - coeff*lhs(i,1,2)
      lhs(i,4,3)= lhs(i,4,3) - coeff*lhs(i,1,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,1,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,1,5)
      r(i,4)   = r(i,4)   - coeff*r(i,1)

      coeff = lhs(i,5,1)
      lhs(i,5,2)= lhs(i,5,2) - coeff*lhs(i,1,2)
      lhs(i,5,3)= lhs(i,5,3) - coeff*lhs(i,1,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,1,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,1,5)
      r(i,5)   = r(i,5)   - coeff*r(i,1)


      pivot = 1.00d0/lhs(i,2,2)
      lhs(i,2,3) = lhs(i,2,3)*pivot
      lhs(i,2,4) = lhs(i,2,4)*pivot
      lhs(i,2,5) = lhs(i,2,5)*pivot
      r(i,2)   = r(i,2)  *pivot

      coeff = lhs(i,1,2)
      lhs(i,1,3)= lhs(i,1,3) - coeff*lhs(i,2,3)
      lhs(i,1,4)= lhs(i,1,4) - coeff*lhs(i,2,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,2,5)
      r(i,1)   = r(i,1)   - coeff*r(i,2)

      coeff = lhs(i,3,2)
      lhs(i,3,3)= lhs(i,3,3) - coeff*lhs(i,2,3)
      lhs(i,3,4)= lhs(i,3,4) - coeff*lhs(i,2,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,2,5)
      r(i,3)   = r(i,3)   - coeff*r(i,2)

      coeff = lhs(i,4,2)
      lhs(i,4,3)= lhs(i,4,3) - coeff*lhs(i,2,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,2,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,2,5)
      r(i,4)   = r(i,4)   - coeff*r(i,2)

      coeff = lhs(i,5,2)
      lhs(i,5,3)= lhs(i,5,3) - coeff*lhs(i,2,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,2,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,2,5)
      r(i,5)   = r(i,5)   - coeff*r(i,2)


      pivot = 1.00d0/lhs(i,3,3)
      lhs(i,3,4) = lhs(i,3,4)*pivot
      lhs(i,3,5) = lhs(i,3,5)*pivot
      r(i,3)   = r(i,3)  *pivot

      coeff = lhs(i,1,3)
      lhs(i,1,4)= lhs(i,1,4) - coeff*lhs(i,3,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,3,5)
      r(i,1)   = r(i,1)   - coeff*r(i,3)

      coeff = lhs(i,2,3)
      lhs(i,2,4)= lhs(i,2,4) - coeff*lhs(i,3,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,3,5)
      r(i,2)   = r(i,2)   - coeff*r(i,3)

      coeff = lhs(i,4,3)
      lhs(i,4,4)= lhs(i,4,4) - coeff*lhs(i,3,4)
      lhs(i,4,5)= lhs(i,4,5) - coeff*lhs(i,3,5)
      r(i,4)   = r(i,4)   - coeff*r(i,3)

      coeff = lhs(i,5,3)
      lhs(i,5,4)= lhs(i,5,4) - coeff*lhs(i,3,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,3,5)
      r(i,5)   = r(i,5)   - coeff*r(i,3)


      pivot = 1.00d0/lhs(i,4,4)
      lhs(i,4,5) = lhs(i,4,5)*pivot
      r(i,4)   = r(i,4)  *pivot

      coeff = lhs(i,1,4)
      lhs(i,1,5)= lhs(i,1,5) - coeff*lhs(i,4,5)
      r(i,1)   = r(i,1)   - coeff*r(i,4)

      coeff = lhs(i,2,4)
      lhs(i,2,5)= lhs(i,2,5) - coeff*lhs(i,4,5)
      r(i,2)   = r(i,2)   - coeff*r(i,4)

      coeff = lhs(i,3,4)
      lhs(i,3,5)= lhs(i,3,5) - coeff*lhs(i,4,5)
      r(i,3)   = r(i,3)   - coeff*r(i,4)

      coeff = lhs(i,5,4)
      lhs(i,5,5)= lhs(i,5,5) - coeff*lhs(i,4,5)
      r(i,5)   = r(i,5)   - coeff*r(i,4)


      pivot = 1.00d0/lhs(i,5,5)
      r(i,5)   = r(i,5)  *pivot

      coeff = lhs(i,1,5)
      r(i,1)   = r(i,1)   - coeff*r(i,5)

      coeff = lhs(i,2,5)
      r(i,2)   = r(i,2)   - coeff*r(i,5)

      coeff = lhs(i,3,5)
      r(i,3)   = r(i,3)   - coeff*r(i,5)

      coeff = lhs(i,4,5)
      r(i,4)   = r(i,4)   - coeff*r(i,5)
      end do


      return
      end



