!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  bt_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------
 
      module bt_data

!---------------------------------------------------------------------
! The following include file is generated automatically by the
! "setparams" utility. It defines 
!      maxcells:      the square root of the maximum number of processors
!      problem_size:  12, 64, 102, 162 (for class S, A, B, C)
!      dt_default:    default time step for this problem size if no
!                     config file
!      niter_default: default number of iterations for this problem size
!---------------------------------------------------------------------

      include 'npbparams.h'

      integer           aa, bb, cc, BLOCK_SIZE
      parameter (aa=1, bb=2, cc=3, BLOCK_SIZE=5)

      integer           ncells, grid_points(3)
      double precision  elapsed_time

      double precision  tx1, tx2, tx3, ty1, ty2, ty3, tz1, tz2, tz3,  &
     &                  dx1, dx2, dx3, dx4, dx5, dy1, dy2, dy3, dy4,  &
     &                  dy5, dz1, dz2, dz3, dz4, dz5, dssp, dt,  &
     &                  ce(5,13), dxmax, dymax, dzmax, xxcon1, xxcon2,  &
     &                  xxcon3, xxcon4, xxcon5, dx1tx1, dx2tx1, dx3tx1,  &
     &                  dx4tx1, dx5tx1, yycon1, yycon2, yycon3, yycon4,  &
     &                  yycon5, dy1ty1, dy2ty1, dy3ty1, dy4ty1, dy5ty1,  &
     &                  zzcon1, zzcon2, zzcon3, zzcon4, zzcon5, dz1tz1,  &
     &                  dz2tz1, dz3tz1, dz4tz1, dz5tz1, dnxm1, dnym1,  &
     &                  dnzm1, c1c2, c1c5, c3c4, c1345, conz1, c1, c2,  &
     &                  c3, c4, c5, c4dssp, c5dssp, dtdssp, dttx1, bt,  &
     &                  dttx2, dtty1, dtty2, dttz1, dttz2, c2dttx1,  &
     &                  c2dtty1, c2dttz1, comz1, comz4, comz5, comz6,  &
     &                  c3c4tx3, c3c4ty3, c3c4tz3, c2iv, con43, con16

      integer           EAST, WEST, NORTH, SOUTH,  &
     &                  BOTTOM, TOP

      parameter (EAST=2000, WEST=3000,      NORTH=4000, SOUTH=5000,  &
     &           BOTTOM=6000, TOP=7000)

      integer maxcells, IMAX, JMAX, KMAX, MAX_CELL_DIM, BUF_SIZE

      integer predecessor(3), successor(3), grid_size(3)
      integer, allocatable ::  &
     &        cell_coord (:,:), cell_low (:,:),  &
     &        cell_high  (:,:), cell_size(:,:),  &
     &        start      (:,:), end      (:,:),  &
     &        slice      (:,:)

      double precision, allocatable ::  &
     &        us      (    :,:,:,:),  &
     &        vs      (    :,:,:,:),  &
     &        ws      (    :,:,:,:),  &
     &        qs      (    :,:,:,:),  &
     &        rho_i   (    :,:,:,:),  &
     &        square  (    :,:,:,:),  &
     &        forcing (  :,:,:,:,:),  &
     &        u       (  :,:,:,:,:),  &
     &        rhs     (  :,:,:,:,:),  &
     &        lhsc    (:,:,:,:,:,:),  &
     &        backsub_info(:,:,:,:),  &
     &        in_buffer(:), out_buffer(:)

      double precision, allocatable ::  &
     &        cv  (:), rhon(:),  &
     &        rhos(:), rhoq(:),  &
     &        cuf (:), q   (:),  &
     &        ue(:,:), buf (:,:)

      double precision, allocatable ::  &
     &        fjac(:, :, :),  &
     &        njac(:, :, :),  &
     &        lhsa(:, :, :),  &
     &        lhsb(:, :, :)

      integer west_size, east_size, bottom_size, top_size,  &
     &        north_size, south_size, start_send_west,  &
     &        start_send_east, start_send_south, start_send_north,  &
     &        start_send_bottom, start_send_top, start_recv_west,  &
     &        start_recv_east, start_recv_south, start_recv_north,  &
     &        start_recv_bottom, start_recv_top

      double precision tmp_block(5,5), b_inverse(5,5), tmp_vec(5)

!---------------------------------------------------------------------
!     These are used by btio
!---------------------------------------------------------------------
      integer collbuf_nodes, collbuf_size, iosize, eltext,  &
     &        combined_btype, fp, idump, record_length, element,  &
     &        combined_ftype, idump_sub, rd_interval
      double precision sum(niter_default), xce_sub(5)
      integer(kind=8) :: iseek


!---------------------------------------------------------------------
!     Timer constants
!---------------------------------------------------------------------
      integer t_total, t_io, t_rhs, t_xsolve, t_ysolve, t_zsolve,  &
     &        t_bpack, t_exch, t_xcomm, t_ycomm, t_zcomm,  &
     &        t_comp, t_comm, t_enorm, t_iov, t_last
      parameter (t_total=1, t_io=2, t_rhs=3, t_xsolve=4, t_ysolve=5,  &
     &        t_zsolve=6, t_bpack=7, t_exch=8, t_xcomm=9,  &
     &        t_ycomm=10, t_zcomm=11, t_comp=12, t_comm=13,  &
     &        t_enorm=12, t_iov=13, t_last=13)
      logical timeron

      end module bt_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer ios, ierr

      MAX_CELL_DIM = (problem_size/maxcells)+1

      IMAX = MAX_CELL_DIM
      JMAX = MAX_CELL_DIM
      KMAX = MAX_CELL_DIM

      BUF_SIZE = MAX_CELL_DIM*MAX_CELL_DIM*(maxcells-1)*60+1

      allocate (  &
     &         cell_coord (3,maxcells), cell_low (3,maxcells),  &
     &         cell_high  (3,maxcells), cell_size(3,maxcells),  &
     &         start      (3,maxcells), end      (3,maxcells),  &
     &         slice      (3,maxcells),  &
     &         stat = ios)

      if (ios .eq. 0) allocate (  &
     &   forcing (5,   0:IMAX-1, 0:JMAX-1, 0:KMAX-1, maxcells),  &
     &   u       (5,  -2:IMAX+1,-2:JMAX+1,-2:KMAX+1, maxcells),  &
     &   rhs     (5,  -1:IMAX-1,-1:JMAX-1,-1:KMAX-1, maxcells),  &
     &   lhsc    (5,5,-1:IMAX-1,-1:JMAX-1,-1:KMAX-1, maxcells),  &
     &   backsub_info (5, 0:MAX_CELL_DIM, 0:MAX_CELL_DIM, maxcells),  &
     &   in_buffer(BUF_SIZE), out_buffer(BUF_SIZE),  &
     &         stat = ios)

      if (ios .eq. 0) allocate (  &
     &         cv  (-2:MAX_CELL_DIM+1),  rhon(-2:MAX_CELL_DIM+1),  &
     &         rhos(-2:MAX_CELL_DIM+1),  rhoq(-2:MAX_CELL_DIM+1),  &
     &         cuf (-2:MAX_CELL_DIM+1),     q(-2:MAX_CELL_DIM+1),  &
     &         ue  (-2:MAX_CELL_DIM+1,5), buf(-2:MAX_CELL_DIM+1,5),  &
     &         stat = ios)

      if (ios .eq. 0) allocate (  &
     &         fjac(5, 5, -2:MAX_CELL_DIM+1),  &
     &         njac(5, 5, -2:MAX_CELL_DIM+1),  &
     &         lhsa(5, 5, -1:MAX_CELL_DIM),  &
     &         lhsb(5, 5, -1:MAX_CELL_DIM),  &
     &         stat = ios)

      if (ios .eq. 0) allocate (  &
     &         us    (-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         vs    (-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         ws    (-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         qs    (-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         rho_i (-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         square(-1:IMAX, -1:JMAX, -1:KMAX, maxcells),  &
     &         stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         call MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER, ierr)
         stop
      endif

      return
      end

