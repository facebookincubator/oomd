
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine pintgr

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k
      integer ibeg, ifin, ifin1
      integer jbeg, jfin, jfin1
      integer iglob, iglob1, iglob2
      integer jglob, jglob1, jglob2
      integer ind1, ind2
      double precision  frc1, frc2, frc3
      double precision  dummy

      integer IERROR


!---------------------------------------------------------------------
!   set up the sub-domains for integeration in each processor
!---------------------------------------------------------------------
      ibeg = nx + 1
      ifin = 0
      iglob1 = ipt + 1
      iglob2 = ipt + nx
      if (iglob1.ge.ii1.and.iglob2.lt.ii2+nx) ibeg = 1
      if (iglob1.gt.ii1-nx.and.iglob2.le.ii2) ifin = nx
      if (ii1.ge.iglob1.and.ii1.le.iglob2) ibeg = ii1 - ipt
      if (ii2.ge.iglob1.and.ii2.le.iglob2) ifin = ii2 - ipt
      jbeg = ny + 1
      jfin = 0
      jglob1 = jpt + 1
      jglob2 = jpt + ny
      if (jglob1.ge.ji1.and.jglob2.lt.ji2+ny) jbeg = 1
      if (jglob1.gt.ji1-ny.and.jglob2.le.ji2) jfin = ny
      if (ji1.ge.jglob1.and.ji1.le.jglob2) jbeg = ji1 - jpt
      if (ji2.ge.jglob1.and.ji2.le.jglob2) jfin = ji2 - jpt
      ifin1 = ifin
      jfin1 = jfin
      if (ipt + ifin1.eq.ii2) ifin1 = ifin -1
      if (jpt + jfin1.eq.ji2) jfin1 = jfin -1

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
      do k = 0,isiz3+1
        do i = 0,isiz2+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do

      do j = jbeg,jfin
         jglob = jpt + j
         do i = ibeg,ifin
            iglob = ipt + i

            k = ki1

            phi1(i,j) = c2*(  u(5,i,j,k)  &
     &           - 0.50d+00 * (  u(2,i,j,k) ** 2  &
     &                         + u(3,i,j,k) ** 2  &
     &                         + u(4,i,j,k) ** 2 )  &
     &                        / u(1,i,j,k) )

            k = ki2

            phi2(i,j) = c2*(  u(5,i,j,k)  &
     &           - 0.50d+00 * (  u(2,i,j,k) ** 2  &
     &                         + u(3,i,j,k) ** 2  &
     &                         + u(4,i,j,k) ** 2 )  &
     &                        / u(1,i,j,k) )
         end do
      end do

!---------------------------------------------------------------------
!  communicate in i and j directions
!---------------------------------------------------------------------
      call exchange_4(phi1,phi2,ibeg,ifin1,jbeg,jfin1)

      frc1 = 0.0d+00

      do j = jbeg,jfin1
         do i = ibeg, ifin1
            frc1 = frc1 + (  phi1(i,j)  &
     &                     + phi1(i+1,j)  &
     &                     + phi1(i,j+1)  &
     &                     + phi1(i+1,j+1)  &
     &                     + phi2(i,j)  &
     &                     + phi2(i+1,j)  &
     &                     + phi2(i,j+1)  &
     &                     + phi2(i+1,j+1) )
         end do
      end do

!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc1
!---------------------------------------------------------------------
      dummy = frc1
      call MPI_ALLREDUCE( dummy,  &
     &                    frc1,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_solve,  &
     &                    IERROR )

      frc1 = dxi * deta * frc1

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
      do k = 0,isiz3+1
        do i = 0,isiz2+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do
      jglob = jpt + jbeg
      ind1 = 0
      if (jglob.eq.ji1) then
        ind1 = 1
        do k = ki1, ki2
           do i = ibeg, ifin
              iglob = ipt + i
              phi1(i,k) = c2*(  u(5,i,jbeg,k)  &
     &             - 0.50d+00 * (  u(2,i,jbeg,k) ** 2  &
     &                           + u(3,i,jbeg,k) ** 2  &
     &                           + u(4,i,jbeg,k) ** 2 )  &
     &                          / u(1,i,jbeg,k) )
           end do
        end do
      end if

      jglob = jpt + jfin
      ind2 = 0
      if (jglob.eq.ji2) then
        ind2 = 1
        do k = ki1, ki2
           do i = ibeg, ifin
              iglob = ipt + i
              phi2(i,k) = c2*(  u(5,i,jfin,k)  &
     &             - 0.50d+00 * (  u(2,i,jfin,k) ** 2  &
     &                           + u(3,i,jfin,k) ** 2  &
     &                           + u(4,i,jfin,k) ** 2 )  &
     &                          / u(1,i,jfin,k) )
           end do
        end do
      end if

!---------------------------------------------------------------------
!  communicate in i direction
!---------------------------------------------------------------------
      if (ind1.eq.1) then
        call exchange_5(phi1,ibeg,ifin1)
      end if
      if (ind2.eq.1) then
        call exchange_5 (phi2,ibeg,ifin1)
      end if

      frc2 = 0.0d+00
      do k = ki1, ki2-1
         do i = ibeg, ifin1
            frc2 = frc2 + (  phi1(i,k)  &
     &                     + phi1(i+1,k)  &
     &                     + phi1(i,k+1)  &
     &                     + phi1(i+1,k+1)  &
     &                     + phi2(i,k)  &
     &                     + phi2(i+1,k)  &
     &                     + phi2(i,k+1)  &
     &                     + phi2(i+1,k+1) )
         end do
      end do

!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc2
!---------------------------------------------------------------------
      dummy = frc2
      call MPI_ALLREDUCE( dummy,  &
     &                    frc2,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_solve,  &
     &                    IERROR )

      frc2 = dxi * dzeta * frc2

!---------------------------------------------------------------------
!   initialize
!---------------------------------------------------------------------
      do k = 0,isiz3+1
        do i = 0,isiz2+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do
      iglob = ipt + ibeg
      ind1 = 0
      if (iglob.eq.ii1) then
        ind1 = 1
        do k = ki1, ki2
           do j = jbeg, jfin
              jglob = jpt + j
              phi1(j,k) = c2*(  u(5,ibeg,j,k)  &
     &             - 0.50d+00 * (  u(2,ibeg,j,k) ** 2  &
     &                           + u(3,ibeg,j,k) ** 2  &
     &                           + u(4,ibeg,j,k) ** 2 )  &
     &                          / u(1,ibeg,j,k) )
           end do
        end do
      end if

      iglob = ipt + ifin
      ind2 = 0
      if (iglob.eq.ii2) then
        ind2 = 1
        do k = ki1, ki2
           do j = jbeg, jfin
              jglob = jpt + j
              phi2(j,k) = c2*(  u(5,ifin,j,k)  &
     &             - 0.50d+00 * (  u(2,ifin,j,k) ** 2  &
     &                           + u(3,ifin,j,k) ** 2  &
     &                           + u(4,ifin,j,k) ** 2 )  &
     &                          / u(1,ifin,j,k) )
           end do
        end do
      end if

!---------------------------------------------------------------------
!  communicate in j direction
!---------------------------------------------------------------------
      if (ind1.eq.1) then
        call exchange_6(phi1,jbeg,jfin1)
      end if
      if (ind2.eq.1) then
        call exchange_6(phi2,jbeg,jfin1)
      end if

      frc3 = 0.0d+00

      do k = ki1, ki2-1
         do j = jbeg, jfin1
            frc3 = frc3 + (  phi1(j,k)  &
     &                     + phi1(j+1,k)  &
     &                     + phi1(j,k+1)  &
     &                     + phi1(j+1,k+1)  &
     &                     + phi2(j,k)  &
     &                     + phi2(j+1,k)  &
     &                     + phi2(j,k+1)  &
     &                     + phi2(j+1,k+1) )
         end do
      end do

!---------------------------------------------------------------------
!  compute the global sum of individual contributions to frc3
!---------------------------------------------------------------------
      dummy = frc3
      call MPI_ALLREDUCE( dummy,  &
     &                    frc3,  &
     &                    1,  &
     &                    dp_type,  &
     &                    MPI_SUM,  &
     &                    comm_solve,  &
     &                    IERROR )

      frc3 = deta * dzeta * frc3
      frc = 0.25d+00 * ( frc1 + frc2 + frc3 )
!      if (id.eq.0) write (*,1001) frc

      return

 1001 format (//5x,'surface integral = ',1pe12.5//)

      end
