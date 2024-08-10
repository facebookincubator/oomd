
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine subdomain

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!  local variables
!---------------------------------------------------------------------
      integer mm, ierror, errorcode


!---------------------------------------------------------------------
!
!   set up the sub-domain sizes
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!   x dimension
!---------------------------------------------------------------------
      mm   = mod(nx0,xdim)
      if (row.le.mm) then
        nx = nx0/xdim + 1
        ipt = (row-1)*nx
      else
        nx = nx0/xdim
        ipt = (row-1)*nx + mm
      end if

!---------------------------------------------------------------------
!   y dimension
!---------------------------------------------------------------------
      mm   = mod(ny0,ydim)
      if (col.le.mm) then
        ny = ny0/ydim + 1
        jpt = (col-1)*ny
      else
        ny = ny0/ydim
        jpt = (col-1)*ny + mm
      end if

!---------------------------------------------------------------------
!   z dimension
!---------------------------------------------------------------------
      nz = nz0

!---------------------------------------------------------------------
!   check the sub-domain size
!---------------------------------------------------------------------
      if ( ( nx .lt. 3 ) .or.  &
     &     ( ny .lt. 3 ) .or.  &
     &     ( nz .lt. 3 ) ) then
         write (*,2001) nx, ny, nz
 2001    format (5x,'SUBDOMAIN SIZE IS TOO SMALL - ',  &
     &        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',  &
     &        /5x,'SO THAT NX, NY AND NZ ARE GREATER THAN OR EQUAL',  &
     &        /5x,'TO 3 THEY ARE CURRENTLY', 3I5)
          ERRORCODE = 1
          CALL MPI_ABORT( MPI_COMM_WORLD,  &
     &                    ERRORCODE,  &
     &                    IERROR )
      end if

      if ( ( nx .gt. isiz1 ) .or.  &
     &     ( ny .gt. isiz2 ) .or.  &
     &     ( nz .gt. isiz3 ) ) then
         write (*,2002) nx, ny, nz
 2002    format (5x,'SUBDOMAIN SIZE IS TOO LARGE - ',  &
     &        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',  &
     &        /5x,'SO THAT NX, NY AND NZ ARE LESS THAN OR EQUAL TO ',  &
     &        /5x,'ISIZ1, ISIZ2 AND ISIZ3 RESPECTIVELY.  THEY ARE',  &
     &        /5x,'CURRENTLY', 3I5)
          ERRORCODE = 1
          CALL MPI_ABORT( MPI_COMM_WORLD,  &
     &                    ERRORCODE,  &
     &                    IERROR )
      end if


!---------------------------------------------------------------------
!   set up the start and end in i and j extents for all processors
!---------------------------------------------------------------------
      ist = 1
      iend = nx
      if (north.eq.-1) ist = 2
      if (south.eq.-1) iend = nx - 1

      jst = 1
      jend = ny
      if (west.eq.-1) jst = 2
      if (east.eq.-1) jend = ny - 1

      return
      end


