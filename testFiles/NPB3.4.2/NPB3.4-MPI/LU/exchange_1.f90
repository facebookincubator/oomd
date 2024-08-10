
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine exchange_1( g,k,iex )

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer k, iex
      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j

      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



      if( iex .eq. 0 ) then

          if( north .ne. -1 ) then
              call MPI_RECV( buf1(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       north,  &
     &                       from_n,  &
     &                       comm_solve,  &
     &                       status,  &
     &                       IERROR )
              do j=jst,jend
                  g(1,0,j,k) = buf1(1,j)
                  g(2,0,j,k) = buf1(2,j)
                  g(3,0,j,k) = buf1(3,j)
                  g(4,0,j,k) = buf1(4,j)
                  g(5,0,j,k) = buf1(5,j)
              enddo
          endif

          if( west .ne. -1 ) then
              call MPI_RECV( buf1(1,ist),  &
     &                       5*(iend-ist+1),  &
     &                       dp_type,  &
     &                       west,  &
     &                       from_w,  &
     &                       comm_solve,  &
     &                       status,  &
     &                       IERROR )
              do i=ist,iend
                  g(1,i,0,k) = buf1(1,i)
                  g(2,i,0,k) = buf1(2,i)
                  g(3,i,0,k) = buf1(3,i)
                  g(4,i,0,k) = buf1(4,i)
                  g(5,i,0,k) = buf1(5,i)
              enddo
          endif

      else if( iex .eq. 1 ) then

          if( south .ne. -1 ) then
              call MPI_RECV( buf1(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       south,  &
     &                       from_s,  &
     &                       comm_solve,  &
     &                       status,  &
     &                       IERROR )
              do j=jst,jend
                  g(1,nx+1,j,k) = buf1(1,j)
                  g(2,nx+1,j,k) = buf1(2,j)
                  g(3,nx+1,j,k) = buf1(3,j)
                  g(4,nx+1,j,k) = buf1(4,j)
                  g(5,nx+1,j,k) = buf1(5,j)
              enddo
          endif

          if( east .ne. -1 ) then
              call MPI_RECV( buf1(1,ist),  &
     &                       5*(iend-ist+1),  &
     &                       dp_type,  &
     &                       east,  &
     &                       from_e,  &
     &                       comm_solve,  &
     &                       status,  &
     &                       IERROR )
              do i=ist,iend
                  g(1,i,ny+1,k) = buf1(1,i)
                  g(2,i,ny+1,k) = buf1(2,i)
                  g(3,i,ny+1,k) = buf1(3,i)
                  g(4,i,ny+1,k) = buf1(4,i)
                  g(5,i,ny+1,k) = buf1(5,i)
              enddo
          endif

      else if( iex .eq. 2 ) then

          if( south .ne. -1 ) then
              do j=jst,jend
                  buf(1,j) = g(1,nx,j,k) 
                  buf(2,j) = g(2,nx,j,k) 
                  buf(3,j) = g(3,nx,j,k) 
                  buf(4,j) = g(4,nx,j,k) 
                  buf(5,j) = g(5,nx,j,k) 
              enddo
              call MPI_SEND( buf(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       south,  &
     &                       from_n,  &
     &                       comm_solve,  &
     &                       IERROR )
          endif

          if( east .ne. -1 ) then
              do i=ist,iend
                  buf(1,i) = g(1,i,ny,k)
                  buf(2,i) = g(2,i,ny,k)
                  buf(3,i) = g(3,i,ny,k)
                  buf(4,i) = g(4,i,ny,k)
                  buf(5,i) = g(5,i,ny,k)
              enddo
              call MPI_SEND( buf(1,ist),  &
     &                       5*(iend-ist+1),  &
     &                       dp_type,  &
     &                       east,  &
     &                       from_w,  &
     &                       comm_solve,  &
     &                       IERROR )
          endif

      else

          if( north .ne. -1 ) then
              do j=jst,jend
                  buf(1,j) = g(1,1,j,k)
                  buf(2,j) = g(2,1,j,k)
                  buf(3,j) = g(3,1,j,k)
                  buf(4,j) = g(4,1,j,k)
                  buf(5,j) = g(5,1,j,k)
              enddo
              call MPI_SEND( buf(1,jst),  &
     &                       5*(jend-jst+1),  &
     &                       dp_type,  &
     &                       north,  &
     &                       from_s,  &
     &                       comm_solve,  &
     &                       IERROR )
          endif

          if( west .ne. -1 ) then
              do i=ist,iend
                  buf(1,i) = g(1,i,1,k)
                  buf(2,i) = g(2,i,1,k)
                  buf(3,i) = g(3,i,1,k)
                  buf(4,i) = g(4,i,1,k)
                  buf(5,i) = g(5,i,1,k)
              enddo
              call MPI_SEND( buf(1,ist),  &
     &                       5*(iend-ist+1),  &
     &                       dp_type,  &
     &                       west,  &
     &                       from_e,  &
     &                       comm_solve,  &
     &                       IERROR )
          endif

      endif

      end



