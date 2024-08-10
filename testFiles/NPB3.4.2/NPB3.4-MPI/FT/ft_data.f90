!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  ft_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module ft_data

      include 'npbparams.h'

! total number of grid points in floating point number
      double precision ntotal_f
      parameter (ntotal_f=dble(nx)*ny*nz)

! total dimension scaled by the number of processes
      integer ntdivnp


      double precision seed, a, pi, alpha
      parameter (seed = 314159265.d0, a = 1220703125.d0,  &
     &  pi = 3.141592653589793238d0, alpha=1.0d-6)

! roots of unity array
! relies on x being largest dimension?
      double complex, allocatable :: u(:)


! for checksum data
      double complex sums(0:niter_default)

! number of iterations
      integer niter

! other stuff
      logical debug, debugsynch


!--------------------------------------------------------------------
! Cache blocking params. These values are good for most
! RISC processors.  
! FFT parameters:
!  fftblock controls how many ffts are done at a time. 
!  The default is appropriate for most cache-based machines
!  On vector machines, the FFT can be vectorized with vector
!  length equal to the block size, so the block size should
!  be as large as possible. This is the size of the smallest
!  dimension of the problem: 128 for class A, 256 for class B and
!  512 for class C.
! Transpose parameters:
!  transblock is the blocking factor for the transposes when there
!  is a 1-D layout. On vector machines it should probably be
!  large (largest dimension of the problem).
!--------------------------------------------------------------------

      integer fftblock_default, fftblockpad_default
      parameter (fftblock_default=16, fftblockpad_default=18)
      integer transblock, transblockpad
      parameter(transblock=32, transblockpad=34)
      
      integer fftblock, fftblockpad


!--------------------------------------------------------------------
! 2D processor array -> 2D grid decomposition (by pencils)
! If processor array is 1xN or -> 1D grid decomposition (by planes)
! If processor array is 1x1 -> 0D grid decomposition
! For simplicity, do not treat Nx1 (np2 = 1) specially
!--------------------------------------------------------------------
      integer np1, np2

! basic decomposition strategy
      integer layout_type
      integer layout_0D, layout_1D, layout_2D
      parameter (layout_0D = 0, layout_1D = 1, layout_2D = 2)

!--------------------------------------------------------------------
! There are basically three stages
! 1: x-y-z layout
! 2: after x-transform (before y)
! 3: after y-transform (before z)
! The computation proceeds logically as

! set up initial conditions
! fftx(1)
! transpose (1->2)
! ffty(2)
! transpose (2->3)
! fftz(3)
! time evolution
! fftz(3)
! transpose (3->2)
! ffty(2)
! transpose (2->1)
! fftx(1)
! compute residual(1)

! for the 0D, 1D, 2D strategies, the layouts look like xxx
!        
!            0D        1D        2D
! 1:        xyz       xyz       xyz
! 2:        xyz       xyz       yxz
! 3:        xyz       zyx       zxy
!--------------------------------------------------------------------

! the array dimensions are stored in dims(coord, phase)
      integer dims(3, 3)
      integer xstart(3), ystart(3), zstart(3)
      integer xend(3), yend(3), zend(3)

!--------------------------------------------------------------------
! Timing constants
!--------------------------------------------------------------------
      integer T_total, T_setup, T_fft, T_evolve, T_checksum,  &
     &        T_fftlow, T_fftcopy, T_transpose,  &
     &        T_transxzloc, T_transxzglo, T_transxzfin,  &
     &        T_transxyloc, T_transxyglo, T_transxyfin,  &
     &        T_synch, T_init, T_max
      parameter (T_total = 1, T_setup = 2, T_fft = 3,  &
     &           T_evolve = 4, T_checksum = 5,  &
     &           T_fftlow = 6, T_fftcopy = 7, T_transpose = 8,  &
     &           T_transxzloc = 9, T_transxzglo = 10, T_transxzfin = 11,  &
     &           T_transxyloc = 12, T_transxyglo = 13,  &
     &           T_transxyfin = 14,  T_synch = 15, T_init = 16,  &
     &           T_max = 16)

      logical timers_enabled

!--------------------------------------------------------------------
! external functions
!--------------------------------------------------------------------
      double precision, external :: randlc, timer_read
      integer, external ::          ilog2

      end module ft_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  ft_fields module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module ft_fields

!---------------------------------------------------------------------
! u0, u1, u2 are the main arrays in the problem. 
! Depending on the decomposition, these arrays will have different 
! dimensions. To accomodate all possibilities, we allocate them as 
! one-dimensional arrays and pass them to subroutines for different 
! views
!  - u0 contains the initial (transformed) initial condition
!  - u1 and u2 are working arrays
!---------------------------------------------------------------------
      double complex, allocatable ::  &
     &                 u0(:), u1(:), u2(:)
      double precision, allocatable ::  &
     &                 twiddle(:)

      end module ft_fields


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use ft_data
      use ft_fields
      use mpinpb

      implicit none

      integer ios, ierr


      ntdivnp = ((nx*ny)/np_min)*nz

!---------------------------------------------------------------------
! Padding+3 is to avoid accidental cache problems, 
! since all array sizes are powers of two.
!---------------------------------------------------------------------
      allocate (  &
     &          u0     (ntdivnp+3),  &
     &          u1     (ntdivnp+3),  &
     &          u2     (ntdivnp+3),  &
     &          twiddle(ntdivnp),  &
     &          u      (maxdim),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         call MPI_Abort(MPI_COMM_WORLD, MPI_ERR_OTHER, ierr)
         stop
      endif

      return
      end

