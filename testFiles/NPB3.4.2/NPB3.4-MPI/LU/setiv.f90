
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      subroutine setiv

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   set the initial values of independent variables based on tri-linear
!   interpolation of boundary values in the computational space.
!
!---------------------------------------------------------------------

      use lu_data
      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k, m
      integer iglob, jglob
      double precision  xi, eta, zeta
      double precision  pxi, peta, pzeta
      double precision  ue_1jk(5),ue_nx0jk(5),ue_i1k(5),  &
     &        ue_iny0k(5),ue_ij1(5),ue_ijnz(5)


      do k = 2, nz - 1
         zeta = ( dble (k-1) ) / (nz-1)
         do j = 1, ny
          jglob = jpt + j
          IF (jglob.ne.1.and.jglob.ne.ny0) then
            eta = ( dble (jglob-1) ) / (ny0-1)
            do i = 1, nx
              iglob = ipt + i
              IF (iglob.ne.1.and.iglob.ne.nx0) then
               xi = ( dble (iglob-1) ) / (nx0-1)
               call exact (1,jglob,k,ue_1jk)
               call exact (nx0,jglob,k,ue_nx0jk)
               call exact (iglob,1,k,ue_i1k)
               call exact (iglob,ny0,k,ue_iny0k)
               call exact (iglob,jglob,1,ue_ij1)
               call exact (iglob,jglob,nz,ue_ijnz)
               do m = 1, 5
                  pxi =   ( 1.0d+00 - xi ) * ue_1jk(m)  &
     &                              + xi   * ue_nx0jk(m)
                  peta =  ( 1.0d+00 - eta ) * ue_i1k(m)  &
     &                              + eta   * ue_iny0k(m)
                  pzeta = ( 1.0d+00 - zeta ) * ue_ij1(m)  &
     &                              + zeta   * ue_ijnz(m)

                  u( m, i, j, k ) = pxi + peta + pzeta  &
     &                 - pxi * peta - peta * pzeta - pzeta * pxi  &
     &                 + pxi * peta * pzeta

               end do
              END IF
            end do
          END IF
         end do
      end do

      return
      end
