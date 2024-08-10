!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Performs line solves in Y direction by first factoring
!     the block-tridiagonal matrix into an upper triangular matrix, 
!     and then performing back substitution to solve for the unknow
!     vectors of each line.  
!     
!     Make sure we treat elements zero to cell_size in the direction
!     of the sweep.
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer  &
     &     c, jstart, stage,  &
     &     first, last, recv_id, error, r_status(MPI_STATUS_SIZE),  &
     &     isize,jsize,ksize,send_id

      jstart = 0

      if (timeron) call timer_start(t_ysolve)
!---------------------------------------------------------------------
!     in our terminology stage is the number of the cell in the y-direct
!     i.e. stage = 1 means the start of the line stage=ncells means end
!---------------------------------------------------------------------
      do stage = 1,ncells
         c = slice(2,stage)
         isize = cell_size(1,c) - 1
         jsize = cell_size(2,c) - 1
         ksize = cell_size(3,c) - 1

!---------------------------------------------------------------------
!     set last-cell flag
!---------------------------------------------------------------------
         if (stage .eq. ncells) then
            last = 1
         else
            last = 0
         endif

         if (stage .eq. 1) then
!---------------------------------------------------------------------
!     This is the first cell, so solve without receiving data
!---------------------------------------------------------------------
            first = 1
!            call lhsy(c)
            call y_solve_cell(first,last,c)
         else
!---------------------------------------------------------------------
!     Not the first cell of this line, so receive info from
!     processor working on preceeding cell
!---------------------------------------------------------------------
            first = 0
            if (timeron) call timer_start(t_ycomm)
            call y_receive_solve_info(recv_id,c)
!---------------------------------------------------------------------
!     overlap computations and communications
!---------------------------------------------------------------------
!            call lhsy(c)
!---------------------------------------------------------------------
!     wait for completion
!---------------------------------------------------------------------
            call mpi_wait(send_id,r_status,error)
            call mpi_wait(recv_id,r_status,error)
            if (timeron) call timer_stop(t_ycomm)
!---------------------------------------------------------------------
!     install C'(jstart+1) and rhs'(jstart+1) to be used in this cell
!---------------------------------------------------------------------
            call y_unpack_solve_info(c)
            call y_solve_cell(first,last,c)
         endif

         if (last .eq. 0) call y_send_solve_info(send_id,c)
      enddo

!---------------------------------------------------------------------
!     now perform backsubstitution in reverse direction
!---------------------------------------------------------------------
      do stage = ncells, 1, -1
         c = slice(2,stage)
         first = 0
         last = 0
         if (stage .eq. 1) first = 1
         if (stage .eq. ncells) then
            last = 1
!---------------------------------------------------------------------
!     last cell, so perform back substitute without waiting
!---------------------------------------------------------------------
            call y_backsubstitute(first, last,c)
         else
            if (timeron) call timer_start(t_ycomm)
            call y_receive_backsub_info(recv_id,c)
            call mpi_wait(send_id,r_status,error)
            call mpi_wait(recv_id,r_status,error)
            if (timeron) call timer_stop(t_ycomm)
            call y_unpack_backsub_info(c)
            call y_backsubstitute(first,last,c)
         endif
         if (first .eq. 0) call y_send_backsub_info(send_id,c)
      enddo

      if (timeron) call timer_stop(t_ysolve)

      return
      end
      
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      subroutine y_unpack_solve_info(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     unpack C'(-1) and rhs'(-1) for
!     all i and k
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i,k,m,n,ptr,c,jstart 

      jstart = 0
      ptr = 0
      do k=0,KMAX-1
         do i=0,IMAX-1
            do m=1,BLOCK_SIZE
               do n=1,BLOCK_SIZE
                  lhsc(m,n,i,jstart-1,k,c) = out_buffer(ptr+n)
               enddo
               ptr = ptr+BLOCK_SIZE
            enddo
            do n=1,BLOCK_SIZE
               rhs(n,i,jstart-1,k,c) = out_buffer(ptr+n)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      subroutine y_send_solve_info(send_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     pack up and send C'(jend) and rhs'(jend) for
!     all i and k
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer i,k,m,n,jsize,ptr,c,ip,kp
      integer error,send_id,buffer_size 

      jsize = cell_size(2,c)-1
      ip = cell_coord(1,c) - 1
      kp = cell_coord(3,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*  &
     &     (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE)

!---------------------------------------------------------------------
!     pack up buffer
!---------------------------------------------------------------------
      ptr = 0
      do k=0,KMAX-1
         do i=0,IMAX-1
            do m=1,BLOCK_SIZE
               do n=1,BLOCK_SIZE
                  in_buffer(ptr+n) = lhsc(m,n,i,jsize,k,c)
               enddo
               ptr = ptr+BLOCK_SIZE
            enddo
            do n=1,BLOCK_SIZE
               in_buffer(ptr+n) = rhs(n,i,jsize,k,c)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

!---------------------------------------------------------------------
!     send buffer 
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_ycomm)
      call mpi_isend(in_buffer, buffer_size,  &
     &     dp_type, successor(2),  &
     &     SOUTH+ip+kp*NCELLS, comm_solve,  &
     &     send_id,error)
      if (timeron) call timer_stop(t_ycomm)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_send_backsub_info(send_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     pack up and send U(jstart) for all i and k
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer i,k,n,ptr,c,jstart,ip,kp
      integer error,send_id,buffer_size

!---------------------------------------------------------------------
!     Send element 0 to previous processor
!---------------------------------------------------------------------
      jstart = 0
      ip = cell_coord(1,c)-1
      kp = cell_coord(3,c)-1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE
      ptr = 0
      do k=0,KMAX-1
         do i=0,IMAX-1
            do n=1,BLOCK_SIZE
               in_buffer(ptr+n) = rhs(n,i,jstart,k,c)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo
      if (timeron) call timer_start(t_ycomm)
      call mpi_isend(in_buffer, buffer_size,  &
     &     dp_type, predecessor(2),  &
     &     NORTH+ip+kp*NCELLS, comm_solve,  &
     &     send_id,error)
      if (timeron) call timer_stop(t_ycomm)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_unpack_backsub_info(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     unpack U(jsize) for all i and k
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i,k,n,ptr,c 

      ptr = 0
      do k=0,KMAX-1
         do i=0,IMAX-1
            do n=1,BLOCK_SIZE
               backsub_info(n,i,k,c) = out_buffer(ptr+n)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_receive_backsub_info(recv_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     post mpi receives
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer error,recv_id,ip,kp,c,buffer_size
      ip = cell_coord(1,c) - 1
      kp = cell_coord(3,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE
      call mpi_irecv(out_buffer, buffer_size,  &
     &     dp_type, successor(2),  &
     &     NORTH+ip+kp*NCELLS, comm_solve,  &
     &     recv_id, error)
      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_receive_solve_info(recv_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     post mpi receives 
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer ip,kp,recv_id,error,c,buffer_size
      ip = cell_coord(1,c) - 1
      kp = cell_coord(3,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*  &
     &     (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE)
      call mpi_irecv(out_buffer, buffer_size,  &
     &     dp_type, predecessor(2),  &
     &     SOUTH+ip+kp*NCELLS,  comm_solve,  &
     &     recv_id, error)

      return
      end
      
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_backsubstitute(first, last, c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(jsize)=rhs(jsize)
!     else assume U(jsize) is loaded in un pack backsub_info
!     so just use it
!     after call u(jstart) will be sent to next cell
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer first, last, c, i, k
      integer m,n,j,jsize,isize,ksize,jstart
      
      jstart = 0
      isize = cell_size(1,c)-end(1,c)-1      
      jsize = cell_size(2,c)-1
      ksize = cell_size(3,c)-end(3,c)-1
      if (last .eq. 0) then
         do k=start(3,c),ksize
            do i=start(1,c),isize
!---------------------------------------------------------------------
!     U(jsize) uses info from previous cell if not last cell
!---------------------------------------------------------------------
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,jsize,k,c) = rhs(m,i,jsize,k,c)  &
     &                    - lhsc(m,n,i,jsize,k,c)*  &
     &                    backsub_info(n,i,k,c)
                  enddo
               enddo
            enddo
         enddo
      endif
      do k=start(3,c),ksize
         do j=jsize-1,jstart,-1
            do i=start(1,c),isize
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)  &
     &                    - lhsc(m,n,i,j,k,c)*rhs(n,i,j+1,k,c)
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine y_solve_cell(first,last,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     performs guaussian elimination on this cell.
!     
!     assumes that unpacking routines for non-first cells 
!     preload C' and rhs' from previous cell.
!     
!     assumed send happens outside this routine, but that
!     c'(JMAX) and rhs'(JMAX) will be sent to next cell
!---------------------------------------------------------------------

      use bt_data
      implicit none

      double precision tmp1, tmp2, tmp3
      integer first,last,c
      integer i,j,k,m,n,isize,ksize,jsize,jstart

      jstart = 0
      isize = cell_size(1,c)-end(1,c)-1
      jsize = cell_size(2,c)-1
      ksize = cell_size(3,c)-end(3,c)-1

!---------------------------------------------------------------------
!     zero the left hand side for starters
!     set diagonal values to 1. This is overkill, but convenient
!---------------------------------------------------------------------
      do i = 0, isize
         do m = 1, 5
            do n = 1, 5
               lhsa(m,n,i,0) = 0.0d0
               lhsb(m,n,i,0) = 0.0d0
               lhsa(m,n,i,jsize) = 0.0d0
               lhsb(m,n,i,jsize) = 0.0d0
            enddo
            lhsb(m,m,i,0) = 1.0d0
            lhsb(m,m,i,jsize) = 1.0d0
         enddo
      enddo

      do k=start(3,c),ksize 

!---------------------------------------------------------------------
!     This function computes the left hand side for the three y-factors 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Compute the indices for storing the tri-diagonal matrix;
!     determine a (labeled f) and n jacobians for cell !
!---------------------------------------------------------------------

         do j = start(2,c)-1, cell_size(2,c)-end(2,c)
            do i=start(1,c),isize

               tmp1 = 1.0d0 / u(1,i,j,k,c)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               fjac(1,1,i,j) = 0.0d+00
               fjac(1,2,i,j) = 0.0d+00
               fjac(1,3,i,j) = 1.0d+00
               fjac(1,4,i,j) = 0.0d+00
               fjac(1,5,i,j) = 0.0d+00

               fjac(2,1,i,j) = - ( u(2,i,j,k,c)*u(3,i,j,k,c) )  &
     &              * tmp2
               fjac(2,2,i,j) = u(3,i,j,k,c) * tmp1
               fjac(2,3,i,j) = u(2,i,j,k,c) * tmp1
               fjac(2,4,i,j) = 0.0d+00
               fjac(2,5,i,j) = 0.0d+00

               fjac(3,1,i,j) = - ( u(3,i,j,k,c)*u(3,i,j,k,c)*tmp2)  &
     &              + c2 * qs(i,j,k,c)
               fjac(3,2,i,j) = - c2 *  u(2,i,j,k,c) * tmp1
               fjac(3,3,i,j) = ( 2.0d+00 - c2 )  &
     &              *  u(3,i,j,k,c) * tmp1 
               fjac(3,4,i,j) = - c2 * u(4,i,j,k,c) * tmp1 
               fjac(3,5,i,j) = c2

               fjac(4,1,i,j) = - ( u(3,i,j,k,c)*u(4,i,j,k,c) )  &
     &              * tmp2
               fjac(4,2,i,j) = 0.0d+00
               fjac(4,3,i,j) = u(4,i,j,k,c) * tmp1
               fjac(4,4,i,j) = u(3,i,j,k,c) * tmp1
               fjac(4,5,i,j) = 0.0d+00

               fjac(5,1,i,j) = ( c2 * 2.0d0 * qs(i,j,k,c)  &
     &              - c1 * u(5,i,j,k,c) * tmp1 )  &
     &              * u(3,i,j,k,c) * tmp1 
               fjac(5,2,i,j) = - c2 * u(2,i,j,k,c)*u(3,i,j,k,c)  &
     &              * tmp2
               fjac(5,3,i,j) = c1 * u(5,i,j,k,c) * tmp1  &
     &              - c2 * ( qs(i,j,k,c)  &
     &              + u(3,i,j,k,c)*u(3,i,j,k,c) * tmp2 )
               fjac(5,4,i,j) = - c2 * ( u(3,i,j,k,c)*u(4,i,j,k,c) )  &
     &              * tmp2
               fjac(5,5,i,j) = c1 * u(3,i,j,k,c) * tmp1 

               njac(1,1,i,j) = 0.0d+00
               njac(1,2,i,j) = 0.0d+00
               njac(1,3,i,j) = 0.0d+00
               njac(1,4,i,j) = 0.0d+00
               njac(1,5,i,j) = 0.0d+00

               njac(2,1,i,j) = - c3c4 * tmp2 * u(2,i,j,k,c)
               njac(2,2,i,j) =   c3c4 * tmp1
               njac(2,3,i,j) =   0.0d+00
               njac(2,4,i,j) =   0.0d+00
               njac(2,5,i,j) =   0.0d+00

               njac(3,1,i,j) = - con43 * c3c4 * tmp2 * u(3,i,j,k,c)
               njac(3,2,i,j) =   0.0d+00
               njac(3,3,i,j) =   con43 * c3c4 * tmp1
               njac(3,4,i,j) =   0.0d+00
               njac(3,5,i,j) =   0.0d+00

               njac(4,1,i,j) = - c3c4 * tmp2 * u(4,i,j,k,c)
               njac(4,2,i,j) =   0.0d+00
               njac(4,3,i,j) =   0.0d+00
               njac(4,4,i,j) =   c3c4 * tmp1
               njac(4,5,i,j) =   0.0d+00

               njac(5,1,i,j) = - (  c3c4  &
     &              - c1345 ) * tmp3 * (u(2,i,j,k,c)**2)  &
     &              - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (u(3,i,j,k,c)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (u(4,i,j,k,c)**2)  &
     &              - c1345 * tmp2 * u(5,i,j,k,c)

               njac(5,2,i,j) = (  c3c4 - c1345 ) * tmp2 * u(2,i,j,k,c)
               njac(5,3,i,j) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * u(3,i,j,k,c)
               njac(5,4,i,j) = ( c3c4 - c1345 ) * tmp2 * u(4,i,j,k,c)
               njac(5,5,i,j) = ( c1345 ) * tmp1

            enddo
         enddo

!---------------------------------------------------------------------
!     now joacobians set, so form left hand side in y direction
!---------------------------------------------------------------------
         do j = start(2,c), jsize-end(2,c)
            do i=start(1,c),isize

               tmp1 = dt * ty1
               tmp2 = dt * ty2

               lhsa(1,1,i,j) = - tmp2 * fjac(1,1,i,j-1)  &
     &              - tmp1 * njac(1,1,i,j-1)  &
     &              - tmp1 * dy1 
               lhsa(1,2,i,j) = - tmp2 * fjac(1,2,i,j-1)  &
     &              - tmp1 * njac(1,2,i,j-1)
               lhsa(1,3,i,j) = - tmp2 * fjac(1,3,i,j-1)  &
     &              - tmp1 * njac(1,3,i,j-1)
               lhsa(1,4,i,j) = - tmp2 * fjac(1,4,i,j-1)  &
     &              - tmp1 * njac(1,4,i,j-1)
               lhsa(1,5,i,j) = - tmp2 * fjac(1,5,i,j-1)  &
     &              - tmp1 * njac(1,5,i,j-1)

               lhsa(2,1,i,j) = - tmp2 * fjac(2,1,i,j-1)  &
     &              - tmp1 * njac(2,1,i,j-1)
               lhsa(2,2,i,j) = - tmp2 * fjac(2,2,i,j-1)  &
     &              - tmp1 * njac(2,2,i,j-1)  &
     &              - tmp1 * dy2
               lhsa(2,3,i,j) = - tmp2 * fjac(2,3,i,j-1)  &
     &              - tmp1 * njac(2,3,i,j-1)
               lhsa(2,4,i,j) = - tmp2 * fjac(2,4,i,j-1)  &
     &              - tmp1 * njac(2,4,i,j-1)
               lhsa(2,5,i,j) = - tmp2 * fjac(2,5,i,j-1)  &
     &              - tmp1 * njac(2,5,i,j-1)

               lhsa(3,1,i,j) = - tmp2 * fjac(3,1,i,j-1)  &
     &              - tmp1 * njac(3,1,i,j-1)
               lhsa(3,2,i,j) = - tmp2 * fjac(3,2,i,j-1)  &
     &              - tmp1 * njac(3,2,i,j-1)
               lhsa(3,3,i,j) = - tmp2 * fjac(3,3,i,j-1)  &
     &              - tmp1 * njac(3,3,i,j-1)  &
     &              - tmp1 * dy3 
               lhsa(3,4,i,j) = - tmp2 * fjac(3,4,i,j-1)  &
     &              - tmp1 * njac(3,4,i,j-1)
               lhsa(3,5,i,j) = - tmp2 * fjac(3,5,i,j-1)  &
     &              - tmp1 * njac(3,5,i,j-1)

               lhsa(4,1,i,j) = - tmp2 * fjac(4,1,i,j-1)  &
     &              - tmp1 * njac(4,1,i,j-1)
               lhsa(4,2,i,j) = - tmp2 * fjac(4,2,i,j-1)  &
     &              - tmp1 * njac(4,2,i,j-1)
               lhsa(4,3,i,j) = - tmp2 * fjac(4,3,i,j-1)  &
     &              - tmp1 * njac(4,3,i,j-1)
               lhsa(4,4,i,j) = - tmp2 * fjac(4,4,i,j-1)  &
     &              - tmp1 * njac(4,4,i,j-1)  &
     &              - tmp1 * dy4
               lhsa(4,5,i,j) = - tmp2 * fjac(4,5,i,j-1)  &
     &              - tmp1 * njac(4,5,i,j-1)

               lhsa(5,1,i,j) = - tmp2 * fjac(5,1,i,j-1)  &
     &              - tmp1 * njac(5,1,i,j-1)
               lhsa(5,2,i,j) = - tmp2 * fjac(5,2,i,j-1)  &
     &              - tmp1 * njac(5,2,i,j-1)
               lhsa(5,3,i,j) = - tmp2 * fjac(5,3,i,j-1)  &
     &              - tmp1 * njac(5,3,i,j-1)
               lhsa(5,4,i,j) = - tmp2 * fjac(5,4,i,j-1)  &
     &              - tmp1 * njac(5,4,i,j-1)
               lhsa(5,5,i,j) = - tmp2 * fjac(5,5,i,j-1)  &
     &              - tmp1 * njac(5,5,i,j-1)  &
     &              - tmp1 * dy5

               lhsb(1,1,i,j) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(1,1,i,j)  &
     &              + tmp1 * 2.0d+00 * dy1
               lhsb(1,2,i,j) = tmp1 * 2.0d+00 * njac(1,2,i,j)
               lhsb(1,3,i,j) = tmp1 * 2.0d+00 * njac(1,3,i,j)
               lhsb(1,4,i,j) = tmp1 * 2.0d+00 * njac(1,4,i,j)
               lhsb(1,5,i,j) = tmp1 * 2.0d+00 * njac(1,5,i,j)

               lhsb(2,1,i,j) = tmp1 * 2.0d+00 * njac(2,1,i,j)
               lhsb(2,2,i,j) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(2,2,i,j)  &
     &              + tmp1 * 2.0d+00 * dy2
               lhsb(2,3,i,j) = tmp1 * 2.0d+00 * njac(2,3,i,j)
               lhsb(2,4,i,j) = tmp1 * 2.0d+00 * njac(2,4,i,j)
               lhsb(2,5,i,j) = tmp1 * 2.0d+00 * njac(2,5,i,j)

               lhsb(3,1,i,j) = tmp1 * 2.0d+00 * njac(3,1,i,j)
               lhsb(3,2,i,j) = tmp1 * 2.0d+00 * njac(3,2,i,j)
               lhsb(3,3,i,j) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(3,3,i,j)  &
     &              + tmp1 * 2.0d+00 * dy3
               lhsb(3,4,i,j) = tmp1 * 2.0d+00 * njac(3,4,i,j)
               lhsb(3,5,i,j) = tmp1 * 2.0d+00 * njac(3,5,i,j)

               lhsb(4,1,i,j) = tmp1 * 2.0d+00 * njac(4,1,i,j)
               lhsb(4,2,i,j) = tmp1 * 2.0d+00 * njac(4,2,i,j)
               lhsb(4,3,i,j) = tmp1 * 2.0d+00 * njac(4,3,i,j)
               lhsb(4,4,i,j) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(4,4,i,j)  &
     &              + tmp1 * 2.0d+00 * dy4
               lhsb(4,5,i,j) = tmp1 * 2.0d+00 * njac(4,5,i,j)

               lhsb(5,1,i,j) = tmp1 * 2.0d+00 * njac(5,1,i,j)
               lhsb(5,2,i,j) = tmp1 * 2.0d+00 * njac(5,2,i,j)
               lhsb(5,3,i,j) = tmp1 * 2.0d+00 * njac(5,3,i,j)
               lhsb(5,4,i,j) = tmp1 * 2.0d+00 * njac(5,4,i,j)
               lhsb(5,5,i,j) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(5,5,i,j)  &
     &              + tmp1 * 2.0d+00 * dy5

               lhsc(1,1,i,j,k,c) =  tmp2 * fjac(1,1,i,j+1)  &
     &              - tmp1 * njac(1,1,i,j+1)  &
     &              - tmp1 * dy1
               lhsc(1,2,i,j,k,c) =  tmp2 * fjac(1,2,i,j+1)  &
     &              - tmp1 * njac(1,2,i,j+1)
               lhsc(1,3,i,j,k,c) =  tmp2 * fjac(1,3,i,j+1)  &
     &              - tmp1 * njac(1,3,i,j+1)
               lhsc(1,4,i,j,k,c) =  tmp2 * fjac(1,4,i,j+1)  &
     &              - tmp1 * njac(1,4,i,j+1)
               lhsc(1,5,i,j,k,c) =  tmp2 * fjac(1,5,i,j+1)  &
     &              - tmp1 * njac(1,5,i,j+1)

               lhsc(2,1,i,j,k,c) =  tmp2 * fjac(2,1,i,j+1)  &
     &              - tmp1 * njac(2,1,i,j+1)
               lhsc(2,2,i,j,k,c) =  tmp2 * fjac(2,2,i,j+1)  &
     &              - tmp1 * njac(2,2,i,j+1)  &
     &              - tmp1 * dy2
               lhsc(2,3,i,j,k,c) =  tmp2 * fjac(2,3,i,j+1)  &
     &              - tmp1 * njac(2,3,i,j+1)
               lhsc(2,4,i,j,k,c) =  tmp2 * fjac(2,4,i,j+1)  &
     &              - tmp1 * njac(2,4,i,j+1)
               lhsc(2,5,i,j,k,c) =  tmp2 * fjac(2,5,i,j+1)  &
     &              - tmp1 * njac(2,5,i,j+1)

               lhsc(3,1,i,j,k,c) =  tmp2 * fjac(3,1,i,j+1)  &
     &              - tmp1 * njac(3,1,i,j+1)
               lhsc(3,2,i,j,k,c) =  tmp2 * fjac(3,2,i,j+1)  &
     &              - tmp1 * njac(3,2,i,j+1)
               lhsc(3,3,i,j,k,c) =  tmp2 * fjac(3,3,i,j+1)  &
     &              - tmp1 * njac(3,3,i,j+1)  &
     &              - tmp1 * dy3
               lhsc(3,4,i,j,k,c) =  tmp2 * fjac(3,4,i,j+1)  &
     &              - tmp1 * njac(3,4,i,j+1)
               lhsc(3,5,i,j,k,c) =  tmp2 * fjac(3,5,i,j+1)  &
     &              - tmp1 * njac(3,5,i,j+1)

               lhsc(4,1,i,j,k,c) =  tmp2 * fjac(4,1,i,j+1)  &
     &              - tmp1 * njac(4,1,i,j+1)
               lhsc(4,2,i,j,k,c) =  tmp2 * fjac(4,2,i,j+1)  &
     &              - tmp1 * njac(4,2,i,j+1)
               lhsc(4,3,i,j,k,c) =  tmp2 * fjac(4,3,i,j+1)  &
     &              - tmp1 * njac(4,3,i,j+1)
               lhsc(4,4,i,j,k,c) =  tmp2 * fjac(4,4,i,j+1)  &
     &              - tmp1 * njac(4,4,i,j+1)  &
     &              - tmp1 * dy4
               lhsc(4,5,i,j,k,c) =  tmp2 * fjac(4,5,i,j+1)  &
     &              - tmp1 * njac(4,5,i,j+1)

               lhsc(5,1,i,j,k,c) =  tmp2 * fjac(5,1,i,j+1)  &
     &              - tmp1 * njac(5,1,i,j+1)
               lhsc(5,2,i,j,k,c) =  tmp2 * fjac(5,2,i,j+1)  &
     &              - tmp1 * njac(5,2,i,j+1)
               lhsc(5,3,i,j,k,c) =  tmp2 * fjac(5,3,i,j+1)  &
     &              - tmp1 * njac(5,3,i,j+1)
               lhsc(5,4,i,j,k,c) =  tmp2 * fjac(5,4,i,j+1)  &
     &              - tmp1 * njac(5,4,i,j+1)
               lhsc(5,5,i,j,k,c) =  tmp2 * fjac(5,5,i,j+1)  &
     &              - tmp1 * njac(5,5,i,j+1)  &
     &              - tmp1 * dy5

            enddo
         enddo


!---------------------------------------------------------------------
!     outer most do loops - sweeping in i direction
!---------------------------------------------------------------------
         if (first .eq. 1) then 

!---------------------------------------------------------------------
!     multiply c(i,jstart,k) by b_inverse and copy back to !
!     multiply rhs(jstart) by b_inverse(jstart) and copy to rhs
!---------------------------------------------------------------------
!dir$ ivdep
            do i=start(1,c),isize
               call binvcrhs( lhsb(1,1,i,jstart),  &
     &                        lhsc(1,1,i,jstart,k,c),  &
     &                        rhs(1,i,jstart,k,c) )
            enddo

         endif

!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------
         do j=jstart+first,jsize-last
!dir$ ivdep
            do i=start(1,c),isize

!---------------------------------------------------------------------
!     subtract A*lhs_vector(j-1) from lhs_vector(j)
!     
!     rhs(j) = rhs(j) - A*rhs(j-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,i,j),  &
     &                         rhs(1,i,j-1,k,c),rhs(1,i,j,k,c))

!---------------------------------------------------------------------
!     B(j) = B(j) - C(j-1)*A(j)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,i,j),  &
     &                         lhsc(1,1,i,j-1,k,c),  &
     &                         lhsb(1,1,i,j))

!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to !
!     multiply rhs(i,1,k) by b_inverse(i,1,k) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,i,j),  &
     &                        lhsc(1,1,i,j,k,c),  &
     &                        rhs(1,i,j,k,c) )

            enddo
         enddo

!---------------------------------------------------------------------
!     Now finish up special cases for last cell
!---------------------------------------------------------------------
         if (last .eq. 1) then

!dir$ ivdep
            do i=start(1,c),isize
!---------------------------------------------------------------------
!     rhs(jsize) = rhs(jsize) - A*rhs(jsize-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,i,jsize),  &
     &                         rhs(1,i,jsize-1,k,c),rhs(1,i,jsize,k,c))

!---------------------------------------------------------------------
!     B(jsize) = B(jsize) - C(jsize-1)*A(jsize)
!     call matmul_sub(aa,i,jsize,k,c,
!     $              cc,i,jsize-1,k,c,bb,i,jsize,k,c)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,i,jsize),  &
     &                         lhsc(1,1,i,jsize-1,k,c),  &
     &                         lhsb(1,1,i,jsize))

!---------------------------------------------------------------------
!     multiply rhs(jsize) by b_inverse(jsize) and copy to rhs
!---------------------------------------------------------------------
               call binvrhs( lhsb(1,1,i,jsize),  &
     &                       rhs(1,i,jsize,k,c) )
            enddo

         endif
      enddo


      return
      end
      


