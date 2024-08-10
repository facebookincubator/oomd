
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine exchange_3(g,iex)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   compute the right hand side based on exact solution
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  input parameters
!---------------------------------------------------------------------
      integer iex
      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer i, j, k
      integer ipos1, ipos2

      integer mid
      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



      if (iex.eq.0) then
!---------------------------------------------------------------------
!   communicate in the south and north directions
!---------------------------------------------------------------------
      if (north.ne.-1) then
          call MPI_IRECV( buf1,  &
     &                    10*ny*nz,  &
     &                    dp_type,  &
     &                    north,  &
     &                    from_n,  &
     &                    comm_solve,  &
     &                    mid,  &
     &                    IERROR )
      end if

!---------------------------------------------------------------------
!   send south
!---------------------------------------------------------------------
      if (south.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,nx-1,j,k) 
              buf(2,ipos1) = g(2,nx-1,j,k) 
              buf(3,ipos1) = g(3,nx-1,j,k) 
              buf(4,ipos1) = g(4,nx-1,j,k) 
              buf(5,ipos1) = g(5,nx-1,j,k) 
              buf(1,ipos2) = g(1,nx,j,k)
              buf(2,ipos2) = g(2,nx,j,k)
              buf(3,ipos2) = g(3,nx,j,k)
              buf(4,ipos2) = g(4,nx,j,k)
              buf(5,ipos2) = g(5,nx,j,k)
            end do
          end do

          call MPI_SEND( buf,  &
     &                   10*ny*nz,  &
     &                   dp_type,  &
     &                   south,  &
     &                   from_n,  &
     &                   comm_solve,  &
     &                   IERROR )
        end if

!---------------------------------------------------------------------
!   receive from north
!---------------------------------------------------------------------
        if (north.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,-1,j,k) = buf1(1,ipos1)
              g(2,-1,j,k) = buf1(2,ipos1)
              g(3,-1,j,k) = buf1(3,ipos1)
              g(4,-1,j,k) = buf1(4,ipos1)
              g(5,-1,j,k) = buf1(5,ipos1)
              g(1,0,j,k) = buf1(1,ipos2)
              g(2,0,j,k) = buf1(2,ipos2)
              g(3,0,j,k) = buf1(3,ipos2)
              g(4,0,j,k) = buf1(4,ipos2)
              g(5,0,j,k) = buf1(5,ipos2)
            end do
          end do

        end if

      if (south.ne.-1) then
          call MPI_IRECV( buf1,  &
     &                    10*ny*nz,  &
     &                    dp_type,  &
     &                    south,  &
     &                    from_s,  &
     &                    comm_solve,  &
     &                    mid,  &
     &                    IERROR )
      end if

!---------------------------------------------------------------------
!   send north
!---------------------------------------------------------------------
        if (north.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,2,j,k)
              buf(2,ipos1) = g(2,2,j,k)
              buf(3,ipos1) = g(3,2,j,k)
              buf(4,ipos1) = g(4,2,j,k)
              buf(5,ipos1) = g(5,2,j,k)
              buf(1,ipos2) = g(1,1,j,k)
              buf(2,ipos2) = g(2,1,j,k)
              buf(3,ipos2) = g(3,1,j,k)
              buf(4,ipos2) = g(4,1,j,k)
              buf(5,ipos2) = g(5,1,j,k)
            end do
          end do

          call MPI_SEND( buf,  &
     &                   10*ny*nz,  &
     &                   dp_type,  &
     &                   north,  &
     &                   from_s,  &
     &                   comm_solve,  &
     &                   IERROR )
        end if

!---------------------------------------------------------------------
!   receive from south
!---------------------------------------------------------------------
        if (south.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,nx+2,j,k) = buf1(1,ipos1)
              g(2,nx+2,j,k) = buf1(2,ipos1)
              g(3,nx+2,j,k) = buf1(3,ipos1)
              g(4,nx+2,j,k) = buf1(4,ipos1)
              g(5,nx+2,j,k) = buf1(5,ipos1)
              g(1,nx+1,j,k) = buf1(1,ipos2)
              g(2,nx+1,j,k) = buf1(2,ipos2)
              g(3,nx+1,j,k) = buf1(3,ipos2)
              g(4,nx+1,j,k) = buf1(4,ipos2)
              g(5,nx+1,j,k) = buf1(5,ipos2)
            end do
          end do
        end if

      else

!---------------------------------------------------------------------
!   communicate in the east and west directions
!---------------------------------------------------------------------
      if (west.ne.-1) then
          call MPI_IRECV( buf1,  &
     &                    10*nx*nz,  &
     &                    dp_type,  &
     &                    west,  &
     &                    from_w,  &
     &                    comm_solve,  &
     &                    mid,  &
     &                    IERROR )
      end if

!---------------------------------------------------------------------
!   send east
!---------------------------------------------------------------------
        if (east.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,ny-1,k)
              buf(2,ipos1) = g(2,i,ny-1,k)
              buf(3,ipos1) = g(3,i,ny-1,k)
              buf(4,ipos1) = g(4,i,ny-1,k)
              buf(5,ipos1) = g(5,i,ny-1,k)
              buf(1,ipos2) = g(1,i,ny,k)
              buf(2,ipos2) = g(2,i,ny,k)
              buf(3,ipos2) = g(3,i,ny,k)
              buf(4,ipos2) = g(4,i,ny,k)
              buf(5,ipos2) = g(5,i,ny,k)
            end do
          end do

          call MPI_SEND( buf,  &
     &                   10*nx*nz,  &
     &                   dp_type,  &
     &                   east,  &
     &                   from_w,  &
     &                   comm_solve,  &
     &                   IERROR )
        end if

!---------------------------------------------------------------------
!   receive from west
!---------------------------------------------------------------------
        if (west.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,-1,k) = buf1(1,ipos1)
              g(2,i,-1,k) = buf1(2,ipos1)
              g(3,i,-1,k) = buf1(3,ipos1)
              g(4,i,-1,k) = buf1(4,ipos1)
              g(5,i,-1,k) = buf1(5,ipos1)
              g(1,i,0,k) = buf1(1,ipos2)
              g(2,i,0,k) = buf1(2,ipos2)
              g(3,i,0,k) = buf1(3,ipos2)
              g(4,i,0,k) = buf1(4,ipos2)
              g(5,i,0,k) = buf1(5,ipos2)
            end do
          end do

        end if

      if (east.ne.-1) then
          call MPI_IRECV( buf1,  &
     &                    10*nx*nz,  &
     &                    dp_type,  &
     &                    east,  &
     &                    from_e,  &
     &                    comm_solve,  &
     &                    mid,  &
     &                    IERROR )
      end if

!---------------------------------------------------------------------
!   send west
!---------------------------------------------------------------------
      if (west.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,2,k)
              buf(2,ipos1) = g(2,i,2,k)
              buf(3,ipos1) = g(3,i,2,k)
              buf(4,ipos1) = g(4,i,2,k)
              buf(5,ipos1) = g(5,i,2,k)
              buf(1,ipos2) = g(1,i,1,k)
              buf(2,ipos2) = g(2,i,1,k)
              buf(3,ipos2) = g(3,i,1,k)
              buf(4,ipos2) = g(4,i,1,k)
              buf(5,ipos2) = g(5,i,1,k)
            end do
          end do

          call MPI_SEND( buf,  &
     &                   10*nx*nz,  &
     &                   dp_type,  &
     &                   west,  &
     &                   from_e,  &
     &                   comm_solve,  &
     &                   IERROR )
        end if

!---------------------------------------------------------------------
!   receive from east
!---------------------------------------------------------------------
        if (east.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,ny+2,k) = buf1(1,ipos1)
              g(2,i,ny+2,k) = buf1(2,ipos1)
              g(3,i,ny+2,k) = buf1(3,ipos1)
              g(4,i,ny+2,k) = buf1(4,ipos1)
              g(5,i,ny+2,k) = buf1(5,ipos1)
              g(1,i,ny+1,k) = buf1(1,ipos2)
              g(2,i,ny+1,k) = buf1(2,ipos2)
              g(3,i,ny+1,k) = buf1(3,ipos2)
              g(4,i,ny+1,k) = buf1(4,ipos2)
              g(5,i,ny+1,k) = buf1(5,ipos2)
            end do
          end do

        end if

      end if

      return
      end     
