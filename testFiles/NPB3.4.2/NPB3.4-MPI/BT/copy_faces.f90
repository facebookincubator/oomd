!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine copy_faces

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     
! This function copies the face values of a variable defined on a set 
! of cells to the overlap locations of the adjacent sets of cells. 
! Because a set of cells interfaces in each direction with exactly one 
! other set, we only need to fill six different buffers. We could try to 
! overlap communication with computation, by computing
! some internal values while communicating boundary values, but this
! adds so much overhead that it's not clearly useful. 
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer i, j, k, c, m, requests(0:11), p0, p1,  &
     &     p2, p3, p4, p5, b_size(0:5), ss(0:5),  &
     &     sr(0:5), error, statuses(MPI_STATUS_SIZE, 0:11)

!---------------------------------------------------------------------
!     exit immediately if there are no faces to be copied           
!---------------------------------------------------------------------
      if (no_nodes .eq. 1) then
         call compute_rhs
         return
      endif

      ss(0) = start_send_east
      ss(1) = start_send_west
      ss(2) = start_send_north
      ss(3) = start_send_south
      ss(4) = start_send_top
      ss(5) = start_send_bottom

      sr(0) = start_recv_east
      sr(1) = start_recv_west
      sr(2) = start_recv_north
      sr(3) = start_recv_south
      sr(4) = start_recv_top
      sr(5) = start_recv_bottom

      b_size(0) = east_size   
      b_size(1) = west_size   
      b_size(2) = north_size  
      b_size(3) = south_size  
      b_size(4) = top_size    
      b_size(5) = bottom_size 

!---------------------------------------------------------------------
!     because the difference stencil for the diagonalized scheme is 
!     orthogonal, we do not have to perform the staged copying of faces, 
!     but can send all face information simultaneously to the neighboring 
!     cells in all directions          
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_bpack)
      p0 = 0
      p1 = 0
      p2 = 0
      p3 = 0
      p4 = 0
      p5 = 0

      do  c = 1, ncells

!---------------------------------------------------------------------
!     fill the buffer to be sent to eastern neighbors (i-dir)
!---------------------------------------------------------------------
         if (cell_coord(1,c) .ne. ncells) then
            do   k = 0, cell_size(3,c)-1
               do   j = 0, cell_size(2,c)-1
                  do   i = cell_size(1,c)-2, cell_size(1,c)-1
                     do   m = 1, 5
                        out_buffer(ss(0)+p0) = u(m,i,j,k,c)
                        p0 = p0 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     fill the buffer to be sent to western neighbors 
!---------------------------------------------------------------------
         if (cell_coord(1,c) .ne. 1) then
            do   k = 0, cell_size(3,c)-1
               do   j = 0, cell_size(2,c)-1
                  do   i = 0, 1
                     do   m = 1, 5
                        out_buffer(ss(1)+p1) = u(m,i,j,k,c)
                        p1 = p1 + 1
                     end do
                  end do
               end do
            end do

         endif

!---------------------------------------------------------------------
!     fill the buffer to be sent to northern neighbors (j_dir)
!---------------------------------------------------------------------
         if (cell_coord(2,c) .ne. ncells) then
            do   k = 0, cell_size(3,c)-1
               do   j = cell_size(2,c)-2, cell_size(2,c)-1
                  do   i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        out_buffer(ss(2)+p2) = u(m,i,j,k,c)
                        p2 = p2 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     fill the buffer to be sent to southern neighbors 
!---------------------------------------------------------------------
         if (cell_coord(2,c).ne. 1) then
            do   k = 0, cell_size(3,c)-1
               do   j = 0, 1
                  do   i = 0, cell_size(1,c)-1   
                     do   m = 1, 5
                        out_buffer(ss(3)+p3) = u(m,i,j,k,c)
                        p3 = p3 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     fill the buffer to be sent to top neighbors (k-dir)
!---------------------------------------------------------------------
         if (cell_coord(3,c) .ne. ncells) then
            do   k = cell_size(3,c)-2, cell_size(3,c)-1
               do   j = 0, cell_size(2,c)-1
                  do   i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        out_buffer(ss(4)+p4) = u(m,i,j,k,c)
                        p4 = p4 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     fill the buffer to be sent to bottom neighbors
!---------------------------------------------------------------------
         if (cell_coord(3,c).ne. 1) then
            do    k=0, 1
               do   j = 0, cell_size(2,c)-1
                  do   i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        out_buffer(ss(5)+p5) = u(m,i,j,k,c)
                        p5 = p5 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     cell loop
!---------------------------------------------------------------------
      end do
      if (timeron) call timer_stop(t_bpack)

      if (timeron) call timer_start(t_exch)
      call mpi_irecv(in_buffer(sr(0)), b_size(0),  &
     &     dp_type, successor(1), WEST,  &
     &     comm_rhs, requests(0), error)
      call mpi_irecv(in_buffer(sr(1)), b_size(1),  &
     &     dp_type, predecessor(1), EAST,  &
     &     comm_rhs, requests(1), error)
      call mpi_irecv(in_buffer(sr(2)), b_size(2),  &
     &     dp_type, successor(2), SOUTH,  &
     &     comm_rhs, requests(2), error)
      call mpi_irecv(in_buffer(sr(3)), b_size(3),  &
     &     dp_type, predecessor(2), NORTH,  &
     &     comm_rhs, requests(3), error)
      call mpi_irecv(in_buffer(sr(4)), b_size(4),  &
     &     dp_type, successor(3), BOTTOM,  &
     &     comm_rhs, requests(4), error)
      call mpi_irecv(in_buffer(sr(5)), b_size(5),  &
     &     dp_type, predecessor(3), TOP,   &
     &     comm_rhs, requests(5), error)

      call mpi_isend(out_buffer(ss(0)), b_size(0),  &
     &     dp_type, successor(1),   EAST,  &
     &     comm_rhs, requests(6), error)
      call mpi_isend(out_buffer(ss(1)), b_size(1),  &
     &     dp_type, predecessor(1), WEST,  &
     &     comm_rhs, requests(7), error)
      call mpi_isend(out_buffer(ss(2)), b_size(2),  &
     &     dp_type,successor(2),   NORTH,  &
     &     comm_rhs, requests(8), error)
      call mpi_isend(out_buffer(ss(3)), b_size(3),  &
     &     dp_type,predecessor(2), SOUTH,  &
     &     comm_rhs, requests(9), error)
      call mpi_isend(out_buffer(ss(4)), b_size(4),  &
     &     dp_type,successor(3),   TOP,  &
     &     comm_rhs,   requests(10), error)
      call mpi_isend(out_buffer(ss(5)), b_size(5),  &
     &     dp_type,predecessor(3), BOTTOM,  &
     &     comm_rhs,requests(11), error)


      call mpi_waitall(12, requests, statuses, error)
      if (timeron) call timer_stop(t_exch)

!---------------------------------------------------------------------
!     unpack the data that has just been received;             
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_bpack)
      p0 = 0
      p1 = 0
      p2 = 0
      p3 = 0
      p4 = 0
      p5 = 0

      do   c = 1, ncells

         if (cell_coord(1,c) .ne. 1) then
            do   k = 0, cell_size(3,c)-1
               do   j = 0, cell_size(2,c)-1
                  do   i = -2, -1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(1)+p0)
                        p0 = p0 + 1
                     end do
                  end do
               end do
            end do
         endif

         if (cell_coord(1,c) .ne. ncells) then
            do  k = 0, cell_size(3,c)-1
               do  j = 0, cell_size(2,c)-1
                  do  i = cell_size(1,c), cell_size(1,c)+1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(0)+p1)
                        p1 = p1 + 1
                     end do
                  end do
               end do
            end do
         end if
            
         if (cell_coord(2,c) .ne. 1) then
            do  k = 0, cell_size(3,c)-1
               do   j = -2, -1
                  do  i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(3)+p2)
                        p2 = p2 + 1
                     end do
                  end do
               end do
            end do

         endif
            
         if (cell_coord(2,c) .ne. ncells) then
            do  k = 0, cell_size(3,c)-1
               do   j = cell_size(2,c), cell_size(2,c)+1
                  do  i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(2)+p3)
                        p3 = p3 + 1
                     end do
                  end do
               end do
            end do
         endif

         if (cell_coord(3,c) .ne. 1) then
            do  k = -2, -1
               do  j = 0, cell_size(2,c)-1
                  do  i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(5)+p4)
                        p4 = p4 + 1
                     end do
                  end do
               end do
            end do
         endif

         if (cell_coord(3,c) .ne. ncells) then
            do  k = cell_size(3,c), cell_size(3,c)+1
               do  j = 0, cell_size(2,c)-1
                  do  i = 0, cell_size(1,c)-1
                     do   m = 1, 5
                        u(m,i,j,k,c) = in_buffer(sr(4)+p5)
                        p5 = p5 + 1
                     end do
                  end do
               end do
            end do
         endif

!---------------------------------------------------------------------
!     cells loop
!---------------------------------------------------------------------
      end do
      if (timeron) call timer_stop(t_bpack)

!---------------------------------------------------------------------
!     do the rest of the rhs that uses the copied face values          
!---------------------------------------------------------------------
      call compute_rhs

      return
      end
