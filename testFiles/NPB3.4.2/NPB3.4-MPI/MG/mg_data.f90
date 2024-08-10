!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mg_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mg_data

!---------------------------------------------------------------------
!  Parameter lm is the log-base2 of the edge size max for
!  the partition on a given node, so must be changed either
!  to save space (if running a small case) or made bigger for larger 
!  cases, for example, 512^3. Thus lm=7 means that the largest dimension 
!  of a partition that can be solved on a node is 2^7 = 128. lm is set 
!  automatically in npbparams.h
!  Parameters ndim1, ndim2, ndim3 are the local problem dimensions. 
!---------------------------------------------------------------------

      include 'npbparams.h'

      ! partitioned size in each dimension
      integer ndim1, ndim2, ndim3

      ! log of maximum dimension on a node
      integer lm

      integer nm  &    ! actual dimension including ghost cells for communications
     &      , nv  &    ! size of rhs array
     &      , nr  &    ! size of residual array
     &      , nm2  &   ! size of communication buffer
     &      , maxlevel! maximum number of levels
      parameter (maxlevel = lt_default+1)


      integer maxprocs
      parameter( maxprocs = 131072 )  ! this is the upper proc limit that 
                                      ! the current "nr" parameter can handle
!---------------------------------------------------------------------
      integer nbr(3,-1:1,maxlevel), msg_type(3,-1:1)
      integer msg_id(3,-1:1,2),nx(maxlevel),ny(maxlevel),nz(maxlevel)

      character class

      integer debug_vec(0:7)

      integer ir(maxlevel), m1(maxlevel), m2(maxlevel), m3(maxlevel)
      integer lt, lb

      logical dead(maxlevel), give_ex(3,maxlevel), take_ex(3,maxlevel)

! ... grid
      integer  is1, is2, is3, ie1, ie2, ie3

!---------------------------------------------------------------------
!  Set at m=1024, can handle cases up to 1024^3 case
!---------------------------------------------------------------------
      integer m
!      parameter( m=1037 )

      double precision, allocatable ::  &
     &        buff(:,:)

!---------------------------------------------------------------------
!  Timing constants
!---------------------------------------------------------------------
      integer t_bench, t_init, t_psinv, t_resid, t_rprj3, t_interp,  &
     &        t_norm2u3, t_comm3, t_rcomm, t_last
      parameter (t_bench=1, t_init=2, t_psinv=3, t_resid=4, t_rprj3=5,  &
     &        t_interp=6, t_norm2u3=7, t_comm3=8,  &
     &        t_rcomm=9, t_last=9)

      logical timeron


      end module mg_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mg_fields module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mg_fields

!---------------------------------------------------------------------------c
! These are major data arrays and can be quite large.
! They are always passed as subroutine args.
!---------------------------------------------------------------------------c
      double precision, allocatable :: u(:), v(:), r(:)

      double precision  a(0:3),c(0:3)

      end module mg_fields


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use mg_data
      use mg_fields
      use mpinpb

      implicit none

      integer ios, ierr
      integer log2_size, log_p


!---------------------------------------------------------------------
! set up dimension parameters after partition
!---------------------------------------------------------------------
      log_p  = log(float(nprocs)+0.0001)/log(2.0)

      ! lt is log of largest total dimension
      log2_size = lt_default

      ! log of maximum dimension on a node
      lm = log2_size - log_p/3
      ndim1 = lm
      ndim3 = log2_size - (log_p+2)/3
      ndim2 = log2_size - (log_p+1)/3

      ! array size parameters
      nm = 2+2**lm
      nv = (2+2**ndim1)*(2+2**ndim2)*(2+2**ndim3)
      nm2= 2*nm*nm
      nr = (8*(nv+nm**2+5*nm+14*lt_default-7*lm))/7
      m  = nm + 1

!---------------------------------------------------------------------
!---------------------------------------------------------------------
      allocate (  &
     &          u(nr),  &
     &          v(nv),  &
     &          r(nr),  &
     &          buff(nm2,4),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         call MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER, ierr)
         stop
      endif

      return
      end

