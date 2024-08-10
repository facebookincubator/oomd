
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine compute_buffer_size(dim)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

       use sp_data
       implicit none

       integer  c, dim, face_size

       if (ncells .eq. 1) return

!---------------------------------------------------------------------
!      compute the actual sizes of the buffers; note that there is 
!      always one cell face that doesn't need buffer space, because it 
!      is at the boundary of the grid
!---------------------------------------------------------------------

       west_size = 0
       east_size = 0

       do   c = 1, ncells
          face_size = cell_size(2,c) * cell_size(3,c) * dim * 2
          if (cell_coord(1,c).ne.1) west_size = west_size + face_size
          if (cell_coord(1,c).ne.ncells) east_size = east_size +  &
     &                                                 face_size 
       end do

       north_size = 0
       south_size = 0
       do   c = 1, ncells
          face_size = cell_size(1,c)*cell_size(3,c) * dim * 2
          if (cell_coord(2,c).ne.1) south_size = south_size + face_size
          if (cell_coord(2,c).ne.ncells) north_size = north_size +  &
     &                                                  face_size 
       end do

       top_size = 0
       bottom_size = 0
       do   c = 1, ncells
          face_size = cell_size(1,c) * cell_size(2,c) * dim * 2
          if (cell_coord(3,c).ne.1) bottom_size = bottom_size +  &
     &                                            face_size
          if (cell_coord(3,c).ne.ncells) top_size = top_size +  &
     &                                                face_size     
       end do

       start_send_west   = 1
       start_send_east   = start_send_west   + west_size
       start_send_south  = start_send_east   + east_size
       start_send_north  = start_send_south  + south_size
       start_send_bottom = start_send_north  + north_size
       start_send_top    = start_send_bottom + bottom_size
       start_recv_west   = 1
       start_recv_east   = start_recv_west   + west_size
       start_recv_south  = start_recv_east   + east_size
       start_recv_north  = start_recv_south  + south_size
       start_recv_bottom = start_recv_north  + north_size
       start_recv_top    = start_recv_bottom + bottom_size

       return
       end

