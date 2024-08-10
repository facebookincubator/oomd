!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Performs line solves in Z direction by first factoring
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

      integer c, kstart, stage,  &
     &     first, last, recv_id, error, r_status(MPI_STATUS_SIZE),  &
     &     isize,jsize,ksize,send_id

      kstart = 0

      if (timeron) call timer_start(t_zsolve)
!---------------------------------------------------------------------
!     in our terminology stage is the number of the cell in the y-direction
!     i.e. stage = 1 means the start of the line stage=ncells means end
!---------------------------------------------------------------------
      do stage = 1,ncells
         c = slice(3,stage)
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
!            call lhsz(c)
            call z_solve_cell(first,last,c)
         else
!---------------------------------------------------------------------
!     Not the first cell of this line, so receive info from
!     processor working on preceeding cell
!---------------------------------------------------------------------
            first = 0
            if (timeron) call timer_start(t_zcomm)
            call z_receive_solve_info(recv_id,c)
!---------------------------------------------------------------------
!     overlap computations and communications
!---------------------------------------------------------------------
!            call lhsz(c)
!---------------------------------------------------------------------
!     wait for completion
!---------------------------------------------------------------------
            call mpi_wait(send_id,r_status,error)
            call mpi_wait(recv_id,r_status,error)
            if (timeron) call timer_stop(t_zcomm)
!---------------------------------------------------------------------
!     install C'(kstart+1) and rhs'(kstart+1) to be used in this cell
!---------------------------------------------------------------------
            call z_unpack_solve_info(c)
            call z_solve_cell(first,last,c)
         endif

         if (last .eq. 0) call z_send_solve_info(send_id,c)
      enddo

!---------------------------------------------------------------------
!     now perform backsubstitution in reverse direction
!---------------------------------------------------------------------
      do stage = ncells, 1, -1
         c = slice(3,stage)
         first = 0
         last = 0
         if (stage .eq. 1) first = 1
         if (stage .eq. ncells) then
            last = 1
!---------------------------------------------------------------------
!     last cell, so perform back substitute without waiting
!---------------------------------------------------------------------
            call z_backsubstitute(first, last,c)
         else
            if (timeron) call timer_start(t_zcomm)
            call z_receive_backsub_info(recv_id,c)
            call mpi_wait(send_id,r_status,error)
            call mpi_wait(recv_id,r_status,error)
            if (timeron) call timer_stop(t_zcomm)
            call z_unpack_backsub_info(c)
            call z_backsubstitute(first,last,c)
         endif
         if (first .eq. 0) call z_send_backsub_info(send_id,c)
      enddo

      if (timeron) call timer_stop(t_zsolve)

      return
      end
      
!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      subroutine z_unpack_solve_info(c)
!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     unpack C'(-1) and rhs'(-1) for
!     all i and j
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i,j,m,n,ptr,c,kstart 

      kstart = 0
      ptr = 0
      do j=0,JMAX-1
         do i=0,IMAX-1
            do m=1,BLOCK_SIZE
               do n=1,BLOCK_SIZE
                  lhsc(m,n,i,j,kstart-1,c) = out_buffer(ptr+n)
               enddo
               ptr = ptr+BLOCK_SIZE
            enddo
            do n=1,BLOCK_SIZE
               rhs(n,i,j,kstart-1,c) = out_buffer(ptr+n)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      
      subroutine z_send_solve_info(send_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     pack up and send C'(kend) and rhs'(kend) for
!     all i and j
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer i,j,m,n,ksize,ptr,c,ip,jp
      integer error,send_id,buffer_size

      ksize = cell_size(3,c)-1
      ip = cell_coord(1,c) - 1
      jp = cell_coord(2,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*  &
     &     (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE)

!---------------------------------------------------------------------
!     pack up buffer
!---------------------------------------------------------------------
      ptr = 0
      do j=0,JMAX-1
         do i=0,IMAX-1
            do m=1,BLOCK_SIZE
               do n=1,BLOCK_SIZE
                  in_buffer(ptr+n) = lhsc(m,n,i,j,ksize,c)
               enddo
               ptr = ptr+BLOCK_SIZE
            enddo
            do n=1,BLOCK_SIZE
               in_buffer(ptr+n) = rhs(n,i,j,ksize,c)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

!---------------------------------------------------------------------
!     send buffer 
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_zcomm)
      call mpi_isend(in_buffer, buffer_size,  &
     &     dp_type, successor(3),  &
     &     BOTTOM+ip+jp*NCELLS, comm_solve,  &
     &     send_id,error)
      if (timeron) call timer_stop(t_zcomm)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_send_backsub_info(send_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     pack up and send U(jstart) for all i and j
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer i,j,n,ptr,c,kstart,ip,jp
      integer error,send_id,buffer_size

!---------------------------------------------------------------------
!     Send element 0 to previous processor
!---------------------------------------------------------------------
      kstart = 0
      ip = cell_coord(1,c)-1
      jp = cell_coord(2,c)-1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE
      ptr = 0
      do j=0,JMAX-1
         do i=0,IMAX-1
            do n=1,BLOCK_SIZE
               in_buffer(ptr+n) = rhs(n,i,j,kstart,c)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

      if (timeron) call timer_start(t_zcomm)
      call mpi_isend(in_buffer, buffer_size,  &
     &     dp_type, predecessor(3),  &
     &     TOP+ip+jp*NCELLS, comm_solve,  &
     &     send_id,error)
      if (timeron) call timer_stop(t_zcomm)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_unpack_backsub_info(c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     unpack U(ksize) for all i and j
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer i,j,n,ptr,c

      ptr = 0
      do j=0,JMAX-1
         do i=0,IMAX-1
            do n=1,BLOCK_SIZE
               backsub_info(n,i,j,c) = out_buffer(ptr+n)
            enddo
            ptr = ptr+BLOCK_SIZE
         enddo
      enddo

      return
      end


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_receive_backsub_info(recv_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     post mpi receives
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer error,recv_id,ip,jp,c,buffer_size

      ip = cell_coord(1,c) - 1
      jp = cell_coord(2,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*BLOCK_SIZE
      call mpi_irecv(out_buffer, buffer_size,  &
     &     dp_type, successor(3),  &
     &     TOP+ip+jp*NCELLS, comm_solve,  &
     &     recv_id, error)

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_receive_solve_info(recv_id,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     post mpi receives 
!---------------------------------------------------------------------

      use bt_data
      use mpinpb

      implicit none

      integer ip,jp,recv_id,error,c,buffer_size

      ip = cell_coord(1,c) - 1
      jp = cell_coord(2,c) - 1
      buffer_size=MAX_CELL_DIM*MAX_CELL_DIM*  &
     &     (BLOCK_SIZE*BLOCK_SIZE + BLOCK_SIZE)
      call mpi_irecv(out_buffer, buffer_size,  &
     &     dp_type, predecessor(3),  &
     &     BOTTOM+ip+jp*NCELLS, comm_solve,  &
     &     recv_id, error)

      return
      end
      
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_backsubstitute(first, last, c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     back solve: if last cell, then generate U(ksize)=rhs(ksize)
!     else assume U(ksize) is loaded in un pack backsub_info
!     so just use it
!     after call u(kstart) will be sent to next cell
!---------------------------------------------------------------------

      use bt_data
      implicit none

      integer first, last, c, i, k
      integer m,n,j,jsize,isize,ksize,kstart
      
      kstart = 0
      isize = cell_size(1,c)-end(1,c)-1      
      jsize = cell_size(2,c)-end(2,c)-1
      ksize = cell_size(3,c)-1
      if (last .eq. 0) then
         do j=start(2,c),jsize
            do i=start(1,c),isize
!---------------------------------------------------------------------
!     U(jsize) uses info from previous cell if not last cell
!---------------------------------------------------------------------
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,ksize,c) = rhs(m,i,j,ksize,c)  &
     &                    - lhsc(m,n,i,j,ksize,c)*  &
     &                    backsub_info(n,i,j,c)
                  enddo
               enddo
            enddo
         enddo
      endif
      do k=ksize-1,kstart,-1
         do j=start(2,c),jsize
            do i=start(1,c),isize
               do m=1,BLOCK_SIZE
                  do n=1,BLOCK_SIZE
                     rhs(m,i,j,k,c) = rhs(m,i,j,k,c)  &
     &                    - lhsc(m,n,i,j,k,c)*rhs(n,i,j,k+1,c)
                  enddo
               enddo
            enddo
         enddo
      enddo

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine z_solve_cell(first,last,c)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     performs guaussian elimination on this cell.
!     
!     assumes that unpacking routines for non-first cells 
!     preload C' and rhs' from previous cell.
!     
!     assumed send happens outside this routine, but that
!     c'(KMAX) and rhs'(KMAX) will be sent to next cell.
!---------------------------------------------------------------------

      use bt_data
      implicit none

      double precision tmp1, tmp2, tmp3
      integer first,last,c
      integer i,j,k,isize,ksize,jsize,kstart
      double precision utmp(6,-2:KMAX+1)

      kstart = 0
      isize = cell_size(1,c)-end(1,c)-1
      jsize = cell_size(2,c)-end(2,c)-1
      ksize = cell_size(3,c)-1

      call lhsabinit(lhsa, lhsb, ksize)

      do j=start(2,c),jsize 
         do i=start(1,c),isize

!---------------------------------------------------------------------
!     This function computes the left hand side for the three z-factors   
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!     Compute the indices for storing the block-diagonal matrix;
!     determine c (labeled f) and s jacobians for cell c
!---------------------------------------------------------------------
            do k = start(3,c)-1, cell_size(3,c)-end(3,c)
               utmp(1,k) = 1.0d0 / u(1,i,j,k,c)
               utmp(2,k) = u(2,i,j,k,c)
               utmp(3,k) = u(3,i,j,k,c)
               utmp(4,k) = u(4,i,j,k,c)
               utmp(5,k) = u(5,i,j,k,c)
               utmp(6,k) = qs(i,j,k,c)
            end do

            do k = start(3,c)-1, cell_size(3,c)-end(3,c)

               tmp1 = utmp(1,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               fjac(1,1,k) = 0.0d+00
               fjac(1,2,k) = 0.0d+00
               fjac(1,3,k) = 0.0d+00
               fjac(1,4,k) = 1.0d+00
               fjac(1,5,k) = 0.0d+00

               fjac(2,1,k) = - ( utmp(2,k)*utmp(4,k) )  &
     &              * tmp2 
               fjac(2,2,k) = utmp(4,k) * tmp1
               fjac(2,3,k) = 0.0d+00
               fjac(2,4,k) = utmp(2,k) * tmp1
               fjac(2,5,k) = 0.0d+00

               fjac(3,1,k) = - ( utmp(3,k)*utmp(4,k) )  &
     &              * tmp2 
               fjac(3,2,k) = 0.0d+00
               fjac(3,3,k) = utmp(4,k) * tmp1
               fjac(3,4,k) = utmp(3,k) * tmp1
               fjac(3,5,k) = 0.0d+00

               fjac(4,1,k) = - (utmp(4,k)*utmp(4,k) * tmp2 )  &
     &              + c2 * utmp(6,k)
               fjac(4,2,k) = - c2 *  utmp(2,k) * tmp1 
               fjac(4,3,k) = - c2 *  utmp(3,k) * tmp1
               fjac(4,4,k) = ( 2.0d+00 - c2 )  &
     &              *  utmp(4,k) * tmp1 
               fjac(4,5,k) = c2

               fjac(5,1,k) = ( c2 * 2.0d0 * utmp(6,k)  &
     &              - c1 * ( utmp(5,k) * tmp1 ) )  &
     &              * ( utmp(4,k) * tmp1 )
               fjac(5,2,k) = - c2 * ( utmp(2,k)*utmp(4,k) )  &
     &              * tmp2 
               fjac(5,3,k) = - c2 * ( utmp(3,k)*utmp(4,k) )  &
     &              * tmp2
               fjac(5,4,k) = c1 * ( utmp(5,k) * tmp1 )  &
     &              - c2 * ( utmp(6,k)  &
     &              + utmp(4,k)*utmp(4,k) * tmp2 )
               fjac(5,5,k) = c1 * utmp(4,k) * tmp1

               njac(1,1,k) = 0.0d+00
               njac(1,2,k) = 0.0d+00
               njac(1,3,k) = 0.0d+00
               njac(1,4,k) = 0.0d+00
               njac(1,5,k) = 0.0d+00

               njac(2,1,k) = - c3c4 * tmp2 * utmp(2,k)
               njac(2,2,k) =   c3c4 * tmp1
               njac(2,3,k) =   0.0d+00
               njac(2,4,k) =   0.0d+00
               njac(2,5,k) =   0.0d+00

               njac(3,1,k) = - c3c4 * tmp2 * utmp(3,k)
               njac(3,2,k) =   0.0d+00
               njac(3,3,k) =   c3c4 * tmp1
               njac(3,4,k) =   0.0d+00
               njac(3,5,k) =   0.0d+00

               njac(4,1,k) = - con43 * c3c4 * tmp2 * utmp(4,k)
               njac(4,2,k) =   0.0d+00
               njac(4,3,k) =   0.0d+00
               njac(4,4,k) =   con43 * c3 * c4 * tmp1
               njac(4,5,k) =   0.0d+00

               njac(5,1,k) = - (  c3c4  &
     &              - c1345 ) * tmp3 * (utmp(2,k)**2)  &
     &              - ( c3c4 - c1345 ) * tmp3 * (utmp(3,k)**2)  &
     &              - ( con43 * c3c4  &
     &              - c1345 ) * tmp3 * (utmp(4,k)**2)  &
     &              - c1345 * tmp2 * utmp(5,k)

               njac(5,2,k) = (  c3c4 - c1345 ) * tmp2 * utmp(2,k)
               njac(5,3,k) = (  c3c4 - c1345 ) * tmp2 * utmp(3,k)
               njac(5,4,k) = ( con43 * c3c4  &
     &              - c1345 ) * tmp2 * utmp(4,k)
               njac(5,5,k) = ( c1345 )* tmp1


            enddo

!---------------------------------------------------------------------
!     now joacobians set, so form left hand side in z direction
!---------------------------------------------------------------------
            do k = start(3,c), ksize-end(3,c)

               tmp1 = dt * tz1
               tmp2 = dt * tz2

               lhsa(1,1,k) = - tmp2 * fjac(1,1,k-1)  &
     &              - tmp1 * njac(1,1,k-1)  &
     &              - tmp1 * dz1 
               lhsa(1,2,k) = - tmp2 * fjac(1,2,k-1)  &
     &              - tmp1 * njac(1,2,k-1)
               lhsa(1,3,k) = - tmp2 * fjac(1,3,k-1)  &
     &              - tmp1 * njac(1,3,k-1)
               lhsa(1,4,k) = - tmp2 * fjac(1,4,k-1)  &
     &              - tmp1 * njac(1,4,k-1)
               lhsa(1,5,k) = - tmp2 * fjac(1,5,k-1)  &
     &              - tmp1 * njac(1,5,k-1)

               lhsa(2,1,k) = - tmp2 * fjac(2,1,k-1)  &
     &              - tmp1 * njac(2,1,k-1)
               lhsa(2,2,k) = - tmp2 * fjac(2,2,k-1)  &
     &              - tmp1 * njac(2,2,k-1)  &
     &              - tmp1 * dz2
               lhsa(2,3,k) = - tmp2 * fjac(2,3,k-1)  &
     &              - tmp1 * njac(2,3,k-1)
               lhsa(2,4,k) = - tmp2 * fjac(2,4,k-1)  &
     &              - tmp1 * njac(2,4,k-1)
               lhsa(2,5,k) = - tmp2 * fjac(2,5,k-1)  &
     &              - tmp1 * njac(2,5,k-1)

               lhsa(3,1,k) = - tmp2 * fjac(3,1,k-1)  &
     &              - tmp1 * njac(3,1,k-1)
               lhsa(3,2,k) = - tmp2 * fjac(3,2,k-1)  &
     &              - tmp1 * njac(3,2,k-1)
               lhsa(3,3,k) = - tmp2 * fjac(3,3,k-1)  &
     &              - tmp1 * njac(3,3,k-1)  &
     &              - tmp1 * dz3 
               lhsa(3,4,k) = - tmp2 * fjac(3,4,k-1)  &
     &              - tmp1 * njac(3,4,k-1)
               lhsa(3,5,k) = - tmp2 * fjac(3,5,k-1)  &
     &              - tmp1 * njac(3,5,k-1)

               lhsa(4,1,k) = - tmp2 * fjac(4,1,k-1)  &
     &              - tmp1 * njac(4,1,k-1)
               lhsa(4,2,k) = - tmp2 * fjac(4,2,k-1)  &
     &              - tmp1 * njac(4,2,k-1)
               lhsa(4,3,k) = - tmp2 * fjac(4,3,k-1)  &
     &              - tmp1 * njac(4,3,k-1)
               lhsa(4,4,k) = - tmp2 * fjac(4,4,k-1)  &
     &              - tmp1 * njac(4,4,k-1)  &
     &              - tmp1 * dz4
               lhsa(4,5,k) = - tmp2 * fjac(4,5,k-1)  &
     &              - tmp1 * njac(4,5,k-1)

               lhsa(5,1,k) = - tmp2 * fjac(5,1,k-1)  &
     &              - tmp1 * njac(5,1,k-1)
               lhsa(5,2,k) = - tmp2 * fjac(5,2,k-1)  &
     &              - tmp1 * njac(5,2,k-1)
               lhsa(5,3,k) = - tmp2 * fjac(5,3,k-1)  &
     &              - tmp1 * njac(5,3,k-1)
               lhsa(5,4,k) = - tmp2 * fjac(5,4,k-1)  &
     &              - tmp1 * njac(5,4,k-1)
               lhsa(5,5,k) = - tmp2 * fjac(5,5,k-1)  &
     &              - tmp1 * njac(5,5,k-1)  &
     &              - tmp1 * dz5

               lhsb(1,1,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(1,1,k)  &
     &              + tmp1 * 2.0d+00 * dz1
               lhsb(1,2,k) = tmp1 * 2.0d+00 * njac(1,2,k)
               lhsb(1,3,k) = tmp1 * 2.0d+00 * njac(1,3,k)
               lhsb(1,4,k) = tmp1 * 2.0d+00 * njac(1,4,k)
               lhsb(1,5,k) = tmp1 * 2.0d+00 * njac(1,5,k)

               lhsb(2,1,k) = tmp1 * 2.0d+00 * njac(2,1,k)
               lhsb(2,2,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(2,2,k)  &
     &              + tmp1 * 2.0d+00 * dz2
               lhsb(2,3,k) = tmp1 * 2.0d+00 * njac(2,3,k)
               lhsb(2,4,k) = tmp1 * 2.0d+00 * njac(2,4,k)
               lhsb(2,5,k) = tmp1 * 2.0d+00 * njac(2,5,k)

               lhsb(3,1,k) = tmp1 * 2.0d+00 * njac(3,1,k)
               lhsb(3,2,k) = tmp1 * 2.0d+00 * njac(3,2,k)
               lhsb(3,3,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(3,3,k)  &
     &              + tmp1 * 2.0d+00 * dz3
               lhsb(3,4,k) = tmp1 * 2.0d+00 * njac(3,4,k)
               lhsb(3,5,k) = tmp1 * 2.0d+00 * njac(3,5,k)

               lhsb(4,1,k) = tmp1 * 2.0d+00 * njac(4,1,k)
               lhsb(4,2,k) = tmp1 * 2.0d+00 * njac(4,2,k)
               lhsb(4,3,k) = tmp1 * 2.0d+00 * njac(4,3,k)
               lhsb(4,4,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(4,4,k)  &
     &              + tmp1 * 2.0d+00 * dz4
               lhsb(4,5,k) = tmp1 * 2.0d+00 * njac(4,5,k)

               lhsb(5,1,k) = tmp1 * 2.0d+00 * njac(5,1,k)
               lhsb(5,2,k) = tmp1 * 2.0d+00 * njac(5,2,k)
               lhsb(5,3,k) = tmp1 * 2.0d+00 * njac(5,3,k)
               lhsb(5,4,k) = tmp1 * 2.0d+00 * njac(5,4,k)
               lhsb(5,5,k) = 1.0d+00  &
     &              + tmp1 * 2.0d+00 * njac(5,5,k)  &
     &              + tmp1 * 2.0d+00 * dz5

               lhsc(1,1,i,j,k,c) =  tmp2 * fjac(1,1,k+1)  &
     &              - tmp1 * njac(1,1,k+1)  &
     &              - tmp1 * dz1
               lhsc(1,2,i,j,k,c) =  tmp2 * fjac(1,2,k+1)  &
     &              - tmp1 * njac(1,2,k+1)
               lhsc(1,3,i,j,k,c) =  tmp2 * fjac(1,3,k+1)  &
     &              - tmp1 * njac(1,3,k+1)
               lhsc(1,4,i,j,k,c) =  tmp2 * fjac(1,4,k+1)  &
     &              - tmp1 * njac(1,4,k+1)
               lhsc(1,5,i,j,k,c) =  tmp2 * fjac(1,5,k+1)  &
     &              - tmp1 * njac(1,5,k+1)

               lhsc(2,1,i,j,k,c) =  tmp2 * fjac(2,1,k+1)  &
     &              - tmp1 * njac(2,1,k+1)
               lhsc(2,2,i,j,k,c) =  tmp2 * fjac(2,2,k+1)  &
     &              - tmp1 * njac(2,2,k+1)  &
     &              - tmp1 * dz2
               lhsc(2,3,i,j,k,c) =  tmp2 * fjac(2,3,k+1)  &
     &              - tmp1 * njac(2,3,k+1)
               lhsc(2,4,i,j,k,c) =  tmp2 * fjac(2,4,k+1)  &
     &              - tmp1 * njac(2,4,k+1)
               lhsc(2,5,i,j,k,c) =  tmp2 * fjac(2,5,k+1)  &
     &              - tmp1 * njac(2,5,k+1)

               lhsc(3,1,i,j,k,c) =  tmp2 * fjac(3,1,k+1)  &
     &              - tmp1 * njac(3,1,k+1)
               lhsc(3,2,i,j,k,c) =  tmp2 * fjac(3,2,k+1)  &
     &              - tmp1 * njac(3,2,k+1)
               lhsc(3,3,i,j,k,c) =  tmp2 * fjac(3,3,k+1)  &
     &              - tmp1 * njac(3,3,k+1)  &
     &              - tmp1 * dz3
               lhsc(3,4,i,j,k,c) =  tmp2 * fjac(3,4,k+1)  &
     &              - tmp1 * njac(3,4,k+1)
               lhsc(3,5,i,j,k,c) =  tmp2 * fjac(3,5,k+1)  &
     &              - tmp1 * njac(3,5,k+1)

               lhsc(4,1,i,j,k,c) =  tmp2 * fjac(4,1,k+1)  &
     &              - tmp1 * njac(4,1,k+1)
               lhsc(4,2,i,j,k,c) =  tmp2 * fjac(4,2,k+1)  &
     &              - tmp1 * njac(4,2,k+1)
               lhsc(4,3,i,j,k,c) =  tmp2 * fjac(4,3,k+1)  &
     &              - tmp1 * njac(4,3,k+1)
               lhsc(4,4,i,j,k,c) =  tmp2 * fjac(4,4,k+1)  &
     &              - tmp1 * njac(4,4,k+1)  &
     &              - tmp1 * dz4
               lhsc(4,5,i,j,k,c) =  tmp2 * fjac(4,5,k+1)  &
     &              - tmp1 * njac(4,5,k+1)

               lhsc(5,1,i,j,k,c) =  tmp2 * fjac(5,1,k+1)  &
     &              - tmp1 * njac(5,1,k+1)
               lhsc(5,2,i,j,k,c) =  tmp2 * fjac(5,2,k+1)  &
     &              - tmp1 * njac(5,2,k+1)
               lhsc(5,3,i,j,k,c) =  tmp2 * fjac(5,3,k+1)  &
     &              - tmp1 * njac(5,3,k+1)
               lhsc(5,4,i,j,k,c) =  tmp2 * fjac(5,4,k+1)  &
     &              - tmp1 * njac(5,4,k+1)
               lhsc(5,5,i,j,k,c) =  tmp2 * fjac(5,5,k+1)  &
     &              - tmp1 * njac(5,5,k+1)  &
     &              - tmp1 * dz5

            enddo


!---------------------------------------------------------------------
!     outer most do loops - sweeping in i direction
!---------------------------------------------------------------------
            if (first .eq. 1) then 

!---------------------------------------------------------------------
!     multiply c(i,j,kstart) by b_inverse and copy back to c
!     multiply rhs(kstart) by b_inverse(kstart) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,kstart),  &
     &                        lhsc(1,1,i,j,kstart,c),  &
     &                        rhs(1,i,j,kstart,c) )

            endif

!---------------------------------------------------------------------
!     begin inner most do loop
!     do all the elements of the cell unless last 
!---------------------------------------------------------------------
            do k=kstart+first,ksize-last

!---------------------------------------------------------------------
!     subtract A*lhs_vector(k-1) from lhs_vector(k)
!     
!     rhs(k) = rhs(k) - A*rhs(k-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,k),  &
     &                         rhs(1,i,j,k-1,c),rhs(1,i,j,k,c))

!---------------------------------------------------------------------
!     B(k) = B(k) - C(k-1)*A(k)
!     call matmul_sub(aa,i,j,k,c,cc,i,j,k-1,c,bb,i,j,k,c)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,k),  &
     &                         lhsc(1,1,i,j,k-1,c),  &
     &                         lhsb(1,1,k))

!---------------------------------------------------------------------
!     multiply c(i,j,k) by b_inverse and copy back to c
!     multiply rhs(i,j,1) by b_inverse(i,j,1) and copy to rhs
!---------------------------------------------------------------------
               call binvcrhs( lhsb(1,1,k),  &
     &                        lhsc(1,1,i,j,k,c),  &
     &                        rhs(1,i,j,k,c) )

            enddo

!---------------------------------------------------------------------
!     Now finish up special cases for last cell
!---------------------------------------------------------------------
            if (last .eq. 1) then

!---------------------------------------------------------------------
!     rhs(ksize) = rhs(ksize) - A*rhs(ksize-1)
!---------------------------------------------------------------------
               call matvec_sub(lhsa(1,1,ksize),  &
     &                         rhs(1,i,j,ksize-1,c),rhs(1,i,j,ksize,c))

!---------------------------------------------------------------------
!     B(ksize) = B(ksize) - C(ksize-1)*A(ksize)
!     call matmul_sub(aa,i,j,ksize,c,
!     $              cc,i,j,ksize-1,c,bb,i,j,ksize,c)
!---------------------------------------------------------------------
               call matmul_sub(lhsa(1,1,ksize),  &
     &                         lhsc(1,1,i,j,ksize-1,c),  &
     &                         lhsb(1,1,ksize))

!---------------------------------------------------------------------
!     multiply rhs(ksize) by b_inverse(ksize) and copy to rhs
!---------------------------------------------------------------------
               call binvrhs( lhsb(1,1,ksize),  &
     &                       rhs(1,i,j,ksize,c) )

            endif
         enddo
      enddo


      return
      end
      





