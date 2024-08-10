
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine setup_btio

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer ierr
      integer mstatus(MPI_STATUS_SIZE)
      integer sizes(4), starts(4), subsizes(4)
      integer cell_btype(maxcells), cell_ftype(maxcells)
      integer cell_blength(maxcells)
      integer info
      character*20 cb_nodes, cb_size
      integer c, m
      integer cell_disp(maxcells)

       call mpi_bcast(collbuf_nodes, 1, MPI_INTEGER,  &
     &                root, comm_setup, ierr)

       call mpi_bcast(collbuf_size, 1, MPI_INTEGER,  &
     &                root, comm_setup, ierr)

       if (collbuf_nodes .eq. 0) then
          info = MPI_INFO_NULL
       else
          write (cb_nodes,*) collbuf_nodes
          write (cb_size,*) collbuf_size
          call MPI_Info_create(info, ierr)
          call MPI_Info_set(info, 'cb_nodes', cb_nodes, ierr)
          call MPI_Info_set(info, 'cb_buffer_size', cb_size, ierr)
          call MPI_Info_set(info, 'collective_buffering', 'true', ierr)
       endif

       call MPI_Type_contiguous(5, MPI_DOUBLE_PRECISION,  &
     &                          element, ierr)
       call MPI_Type_commit(element, ierr)
       call MPI_Type_extent(element, eltext, ierr)

       do  c = 1, ncells
!
! Outer array dimensions ar same for every cell
!
           sizes(1) = IMAX+4
           sizes(2) = JMAX+4
           sizes(3) = KMAX+4
!
! 4th dimension is cell number, total of maxcells cells
!
           sizes(4) = maxcells
!
! Internal dimensions of cells can differ slightly between cells
!
           subsizes(1) = cell_size(1, c)
           subsizes(2) = cell_size(2, c)
           subsizes(3) = cell_size(3, c)
!
! Cell is 4th dimension, 1 cell per cell type to handle varying 
! cell sub-array sizes
!
           subsizes(4) = 1

!
! type constructors use 0-based start addresses
!
           starts(1) = 2 
           starts(2) = 2
           starts(3) = 2
           starts(4) = c-1

! 
! Create buftype for a cell
!
           call MPI_Type_create_subarray(4, sizes, subsizes,  &
     &          starts, MPI_ORDER_FORTRAN, element,  &
     &          cell_btype(c), ierr)
!
! block length and displacement for joining cells - 
! 1 cell buftype per block, cell buftypes have own displacment
! generated from cell number (4th array dimension)
!
           cell_blength(c) = 1
           cell_disp(c) = 0

       enddo
!
! Create combined buftype for all cells
!
       call MPI_Type_struct(ncells, cell_blength, cell_disp,  &
     &            cell_btype, combined_btype, ierr)
       call MPI_Type_commit(combined_btype, ierr)

       do  c = 1, ncells
!
! Entire array size
!
           sizes(1) = PROBLEM_SIZE
           sizes(2) = PROBLEM_SIZE
           sizes(3) = PROBLEM_SIZE

!
! Size of c'th cell
!
           subsizes(1) = cell_size(1, c)
           subsizes(2) = cell_size(2, c)
           subsizes(3) = cell_size(3, c)

!
! Starting point in full array of c'th cell
!
           starts(1) = cell_low(1,c)
           starts(2) = cell_low(2,c)
           starts(3) = cell_low(3,c)

           call MPI_Type_create_subarray(3, sizes, subsizes,  &
     &          starts, MPI_ORDER_FORTRAN,  &
     &          element, cell_ftype(c), ierr)
           cell_blength(c) = 1
           cell_disp(c) = 0
       enddo

       call MPI_Type_struct(ncells, cell_blength, cell_disp,  &
     &            cell_ftype, combined_ftype, ierr)
       call MPI_Type_commit(combined_ftype, ierr)

       iseek=0
       if (node .eq. root) then
          call MPI_File_delete(filenm, MPI_INFO_NULL, ierr)
       endif


      call MPI_Barrier(comm_solve, ierr)

       call MPI_File_open(comm_solve,  &
     &          filenm,  &
     &          MPI_MODE_RDWR+MPI_MODE_CREATE,  &
     &          MPI_INFO_NULL, fp, ierr)

       if (ierr .ne. MPI_SUCCESS) then
                print *, 'Error opening file'
                stop
       endif

        call MPI_File_set_view(fp, iseek, element,  &
     &          combined_ftype, 'native', info, ierr)

       if (ierr .ne. MPI_SUCCESS) then
                print *, 'Error setting file view'
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

      integer mstatus(MPI_STATUS_SIZE)
      integer ierr

      call MPI_File_write_at_all(fp, iseek, u,  &
     &                           1, combined_btype, mstatus, ierr)
      if (ierr .ne. MPI_SUCCESS) then
          print *, 'Error writing to file'
          stop
      endif

      call MPI_Type_size(combined_btype, iosize, ierr)
      iseek = iseek + iosize/eltext

      idump_sub = idump_sub + 1
      if (rd_interval .gt. 0) then
         if (idump_sub .ge. rd_interval) then

            iseek = 0
            call acc_sub_norms(idump+1)

            iseek = 0
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

      integer ii, m, ichunk
      integer ierr
      integer mstatus(MPI_STATUS_SIZE)
      double precision xce_single(5)

      ichunk = idump_cur - idump_sub + 1
      do ii=0, idump_sub-1

        call MPI_File_read_at_all(fp, iseek, u,  &
     &                           1, combined_btype, mstatus, ierr)
        if (ierr .ne. MPI_SUCCESS) then
           print *, 'Error reading back file'
           call MPI_File_close(fp, ierr)
           stop
        endif

        call MPI_Type_size(combined_btype, iosize, ierr)
        iseek = iseek + iosize/eltext

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
      call MPI_File_set_view(fp, iseek, element, combined_ftype,  &
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

