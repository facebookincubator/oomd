
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setup_btio

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      character(128) newfilenm
      integer m

      if (node .lt. 10000) then
          write (newfilenm, 996) filenm,node
      else
          print *, 'error generating file names (> 10000 nodes)'
          stop
      endif

996   format (a,'.',i4.4)

      open (unit=99, file=newfilenm, form='unformatted',  &
     &      status='unknown')

      do m = 1, 5
         xce_sub(m) = 0.d0
      end do

      idump_sub = 0

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine output_timestep

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer ix, iio, jio, kio, cio, aio

      do cio=1,ncells
          write(99)  &
     &         ((((u(aio,ix, jio,kio,cio),aio=1,5),  &
     &             ix=0, cell_size(1,cio)-1),  &
     &             jio=0, cell_size(2,cio)-1),  &
     &             kio=0, cell_size(3,cio)-1)
      enddo

      idump_sub = idump_sub + 1
      if (rd_interval .gt. 0) then
         if (idump_sub .ge. rd_interval) then

            rewind(99)
            call acc_sub_norms(idump+1)

            rewind(99)
            idump_sub = 0
         endif
      endif

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine acc_sub_norms(idump_cur)

      use bt_data
      use mpinpb

      implicit none

      integer idump_cur

      integer ix, jio, kio, cio, ii, m, ichunk
      double precision xce_single(5)

      ichunk = idump_cur - idump_sub + 1
      do ii=0, idump_sub-1
        do cio=1,ncells
          read(99)  &
     &         ((((u(m,ix, jio,kio,cio),m=1,5),  &
     &             ix=0, cell_size(1,cio)-1),  &
     &             jio=0, cell_size(2,cio)-1),  &
     &             kio=0, cell_size(3,cio)-1)
        enddo

        if (node .eq. root) print *, 'Reading data set ', ii+ichunk

        call error_norm(xce_single)
        do m = 1, 5
           xce_sub(m) = xce_sub(m) + xce_single(m)
        end do
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine btio_cleanup

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      implicit none

      close(unit=99)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine accumulate_norms(xce_acc)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      double precision xce_acc(5)

      character(128) newfilenm
      integer m

      if (rd_interval .gt. 0) goto 20

      if (node .lt. 10000) then
          write (newfilenm, 996) filenm,node
      else
          print *, 'error generating file names (> 10000 nodes)'
          stop
      endif

996   format (a,'.',i4.4)

      open (unit=99, file=newfilenm,  &
     &      form='unformatted', action='read')

!     clear the last time step

      call clear_timestep

!     read back the time steps and accumulate norms

      call acc_sub_norms(idump)

      close(unit=99)

 20   continue
      do m = 1, 5
         xce_acc(m) = xce_sub(m) / dble(idump)
      end do

      return
      end
