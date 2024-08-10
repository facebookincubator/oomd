!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine clear_timestep

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer cio, kio, jio, ix

      do cio=1,ncells
          do kio=0, cell_size(3,cio)-1
              do jio=0, cell_size(2,cio)-1
                  do ix=0,cell_size(1,cio)-1
                            u(1,ix, jio,kio,cio) = 0
                            u(2,ix, jio,kio,cio) = 0
                            u(3,ix, jio,kio,cio) = 0
                            u(4,ix, jio,kio,cio) = 0
                            u(5,ix, jio,kio,cio) = 0
                  enddo
              enddo
          enddo
      enddo

      return
      end

