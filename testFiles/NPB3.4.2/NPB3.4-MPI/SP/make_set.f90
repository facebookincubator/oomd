
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine make_set

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! This function allocates space for a set of cells and fills the set     
! such that communication between cells on different nodes is only
! nearest neighbor                                                   
!---------------------------------------------------------------------

       use sp_data
       use mpinpb

       implicit none

       integer p, i, j, c, dir, size, excess, ierr,ierrcode

!---------------------------------------------------------------------
!     compute square root; add small number to allow for roundoff
!     (note: this is computed in setup_mpi.f also, but prefer to do
!     it twice because of some include file problems).
!---------------------------------------------------------------------
      ncells = dint(dsqrt(dble(no_nodes) + 0.00001d0))

!---------------------------------------------------------------------
!      this makes coding easier
!---------------------------------------------------------------------
       p = ncells
   
!---------------------------------------------------------------------
!      determine the location of the cell at the bottom of the 3D 
!      array of cells
!---------------------------------------------------------------------
       cell_coord(1,1) = mod(node,p) 
       cell_coord(2,1) = node/p 
       cell_coord(3,1) = 0

!---------------------------------------------------------------------
!      set the cell_coords for cells in the rest of the z-layers; 
!      this comes down to a simple linear numbering in the z-direct-
!      ion, and to the doubly-cyclic numbering in the other dirs     
!---------------------------------------------------------------------
       do    c=2, p
          cell_coord(1,c) = mod(cell_coord(1,c-1)+1,p) 
          cell_coord(2,c) = mod(cell_coord(2,c-1)-1+p,p) 
          cell_coord(3,c) = c-1
       end do

!---------------------------------------------------------------------
!      offset all the coordinates by 1 to adjust for Fortran arrays
!---------------------------------------------------------------------
       do    dir = 1, 3
          do    c = 1, p
             cell_coord(dir,c) = cell_coord(dir,c) + 1
          end do
       end do
   
!---------------------------------------------------------------------
!      slice(dir,n) contains the sequence number of the cell that is in
!      coordinate plane n in the dir direction
!---------------------------------------------------------------------
       do   dir = 1, 3
          do   c = 1, p
             slice(dir,cell_coord(dir,c)) = c
          end do
       end do


!---------------------------------------------------------------------
!      fill the predecessor and successor entries, using the indices 
!      of the bottom cells (they are the same at each level of k 
!      anyway) acting as if full periodicity pertains; note that p is
!      added to those arguments to the mod functions that might
!      otherwise return wrong values when using the modulo function
!---------------------------------------------------------------------
       i = cell_coord(1,1)-1
       j = cell_coord(2,1)-1

       predecessor(1) = mod(i-1+p,p) + p*j
       predecessor(2) = i + p*mod(j-1+p,p)
       predecessor(3) = mod(i+1,p) + p*mod(j-1+p,p)
       successor(1)   = mod(i+1,p) + p*j
       successor(2)   = i + p*mod(j+1,p)
       successor(3)   = mod(i-1+p,p) + p*mod(j+1,p)

!---------------------------------------------------------------------
! now compute the sizes of the cells                                    
!---------------------------------------------------------------------
       do    dir= 1, 3
!---------------------------------------------------------------------
!         set cell_coord range for each direction                            
!---------------------------------------------------------------------
          size   = grid_points(dir)/p
          excess = mod(grid_points(dir),p)
          do    c=1, ncells
             if (cell_coord(dir,c) .le. excess) then
                cell_size(dir,c) = size+1
                cell_low(dir,c) = (cell_coord(dir,c)-1)*(size+1)
                cell_high(dir,c) = cell_low(dir,c)+size
             else 
                cell_size(dir,c) = size
                cell_low(dir,c)  = excess*(size+1)+  &
     &                   (cell_coord(dir,c)-excess-1)*size
                cell_high(dir,c) = cell_low(dir,c)+size-1
             endif
             if (cell_size(dir, c) .le. 2) then
                write(*,50)
 50             format(' Error: Cell size too small. Min size is 3')
                ierrcode = 1
                call MPI_Abort(mpi_comm_world,ierrcode,ierr)
                stop
             endif
          end do
       end do

       return
       end

