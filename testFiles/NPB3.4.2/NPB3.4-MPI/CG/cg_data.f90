!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  cg_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module cg_data


!---------------------------------------------------------------------
!  Class specific parameters are defined in the npbparams.h file,
!  which is written by the sys/setparams.c program.
!---------------------------------------------------------------------

      include 'npbparams.h'


      ! main_int_mem
      integer, allocatable :: colidx(:), rowstr(:),  &
     &                        iv(:), arow(:), acol(:)

      ! main_flt_mem
      double precision, allocatable ::  &
     &                        v(:), aelt(:), a(:),  &
     &                        x(:),  &
     &                        z(:),  &
     &                        p(:),  &
     &                        q(:),  &
     &                        r(:),  &
     &                        w(:)

      ! urando
      double precision   amult, tran


      ! process grid
      integer num_procs, num_proc_rows, num_proc_cols

      ! number of nonzeros after partition
      integer nz

      ! partit_size
      integer naa, nzz,  &
     &        npcols, nprows,  &
     &        proc_col, proc_row,  &
     &        firstrow,  &
     &        lastrow,  &
     &        firstcol,  &
     &        lastcol,  &
     &        exch_proc,  &
     &        exch_recv_length,  &
     &        send_start,  &
     &        send_len

      ! work arrays for reduction
      integer l2npcols
      integer, allocatable ::  &
     &        reduce_exch_proc(:),  &
     &        reduce_send_starts(:),  &
     &        reduce_send_lengths(:),  &
     &        reduce_recv_starts(:),  &
     &        reduce_recv_lengths(:)


      end module cg_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  timing module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module timing

      integer t_total, t_conjg, t_rcomm, t_ncomm, t_last
      parameter (t_total=1, t_conjg=2, t_rcomm=3, t_ncomm=4, t_last=4)
      logical timeron

      end module timing


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use cg_data
      use mpinpb

      implicit none

      integer(8) naz
      integer ios, ierr


!---------------------------------------------------------------------
! set up dimension parameters after partition
!---------------------------------------------------------------------

      naz = na			! to avoid integer overflow
      naz = naz*(nonzer+1)/num_procs*(nonzer+1)+nonzer  &
     &     + naz*(nonzer+2+num_procs/256)/num_proc_cols
      nz = naz
      if (nz .ne. naz) then
         write(*,*) 'Error: integer overflow', nz, naz
         call MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER, ierr)
      endif

      naa = na / num_proc_rows
      nzz = nz

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      allocate (  &
     &          colidx(nz),  &
     &          rowstr(na+1),  &
     &          iv(2*na+1),  &
     &          arow(nz),  &
     &          acol(nz),  &
     &          stat = ios)

      if (ios .eq. 0) allocate (  &
     &          v(na+1), aelt(nz), a(nz),  &
     &          x(naa+2),  &
     &          z(naa+2),  &
     &          p(naa+2),  &
     &          q(naa+2),  &
     &          r(naa+2),  &
     &          w(naa+2),  &
     &          stat = ios)

      if (ios .eq. 0) allocate (  &
     &          reduce_exch_proc(num_proc_cols),  &
     &          reduce_send_starts(num_proc_cols),  &
     &          reduce_send_lengths(num_proc_cols),  &
     &          reduce_recv_starts(num_proc_cols),  &
     &          reduce_recv_lengths(num_proc_cols),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         call MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER, ierr)
         stop
      endif

      return
      end

