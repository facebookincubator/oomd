
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setup_btio

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer m, ierr

      iseek=0

      if (node .eq. root) then
          call MPI_File_delete(filenm, MPI_INFO_NULL, ierr)
      endif

      call MPI_Barrier(comm_solve, ierr)

      call MPI_File_open(comm_solve,  &
     &          filenm,  &
     &          MPI_MODE_RDWR + MPI_MODE_CREATE,  &
     &          MPI_INFO_NULL,  &
     &          fp,  &
     &          ierr)

      call MPI_File_set_view(fp,  &
     &          iseek, MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION,  &
     &          'native', MPI_INFO_NULL, ierr)

      if (ierr .ne. MPI_SUCCESS) then
          print *, 'Error opening file'
          stop
      endif

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
      use mpinpb

      implicit none

      integer count, jio, kio, cio, aio
      integer ierr
      integer mstatus(MPI_STATUS_SIZE)

      do cio=1,ncells
          do kio=0, cell_size(3,cio)-1
              do jio=0, cell_size(2,cio)-1
                  iseek=(cell_low(3,cio)+kio) +  &
     &                   PROBLEM_SIZE*idump_sub
                  iseek=(cell_low(2,cio)+jio) +  &
     &                   PROBLEM_SIZE*iseek
                  iseek=5*(cell_low(1,cio) +  &
     &                   PROBLEM_SIZE*iseek)

                  count=5*cell_size(1,cio)

                  call MPI_File_write_at(fp, iseek,  &
     &                  u(1,0,jio,kio,cio),  &
     &                  count, MPI_DOUBLE_PRECISION,  &
     &                  mstatus, ierr)

                  if (ierr .ne. MPI_SUCCESS) then
                      print *, 'Error writing to file'
                      stop
                  endif
              enddo
          enddo
      enddo

      idump_sub = idump_sub + 1
      if (rd_interval .gt. 0) then
         if (idump_sub .ge. rd_interval) then

            call acc_sub_norms(idump+1)

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

      integer count, jio, kio, cio, ii, m, ichunk
      integer ierr
      integer mstatus(MPI_STATUS_SIZE)
      double precision xce_single(5)

      ichunk = idump_cur - idump_sub + 1
      do ii=0, idump_sub-1
        do cio=1,ncells
          do kio=0, cell_size(3,cio)-1
              do jio=0, cell_size(2,cio)-1
                  iseek=(cell_low(3,cio)+kio) +  &
     &                   PROBLEM_SIZE*ii
                  iseek=(cell_low(2,cio)+jio) +  &
     &                   PROBLEM_SIZE*iseek
                  iseek=5*(cell_low(1,cio) +  &
     &                   PROBLEM_SIZE*iseek)

                  count=5*cell_size(1,cio)

                  call MPI_File_read_at(fp, iseek,  &
     &                  u(1,0,jio,kio,cio),  &
     &                  count, MPI_DOUBLE_PRECISION,  &
     &                  mstatus, ierr)

                  if (ierr .ne. MPI_SUCCESS) then
                      print *, 'Error reading back file'
                      call MPI_File_close(fp, ierr)
                      stop
                  endif
              enddo
          enddo
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

      use bt_data
      use mpinpb

      implicit none

      integer ierr

      call MPI_File_close(fp, ierr)

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
      integer m, ierr

      if (rd_interval .gt. 0) goto 20

      call MPI_File_open(comm_solve,  &
     &          filenm,  &
     &          MPI_MODE_RDONLY,  &
     &          MPI_INFO_NULL,  &
     &          fp,  &
     &          ierr)

      iseek = 0
      call MPI_File_set_view(fp,  &
     &          iseek, MPI_DOUBLE_PRECISION, MPI_DOUBLE_PRECISION,  &
     &          'native', MPI_INFO_NULL, ierr)

!     clear the last time step

      call clear_timestep

!     read back the time steps and accumulate norms

      call acc_sub_norms(idump)

      call MPI_File_close(fp, ierr)

 20   continue
      do m = 1, 5
         xce_acc(m) = xce_sub(m) / dble(idump)
      end do

      return
      end

