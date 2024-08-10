!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  ft_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module ft_data

      include 'npbparams.h'

! total number of grid points with padding
      integer(kind2) nxp, ntotalp
      parameter (nxp=nx+1)
      parameter (ntotalp=nxp*ny*nz)
      double precision ntotal_f
      parameter (ntotal_f=dble(nx)*ny*nz)


! If processor array is 1x1 -> 0D grid decomposition


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

      include 'blk_par.h'
!      integer fftblock_default, fftblockpad_default
!      parameter (fftblock_default=32, fftblockpad_default=34)
      
      integer fftblock, fftblockpad

! we need a bunch of logic to keep track of how
! arrays are laid out. 


! Note: this serial version is the derived from the parallel 0D case
! of the ft NPB.
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

! the array dimensions are stored in dims(coord, phase)
      integer dims(3)

      integer T_total, T_setup, T_fft, T_evolve, T_checksum,  &
     &        T_fftx, T_ffty,  &
     &        T_fftz, T_max
      parameter (T_total = 1, T_setup = 2, T_fft = 3,  &
     &           T_evolve = 4, T_checksum = 5,  &
     &           T_fftx = 6,  &
     &           T_ffty = 7,  &
     &           T_fftz = 8, T_max = 8)



      logical timers_enabled


      external timer_read
      double precision timer_read
      external ilog2
      integer ilog2

      external randlc
      double precision randlc


! other stuff
      logical debug, debugsynch

      double precision seed, a, pi, alpha
      parameter (seed = 314159265.d0, a = 1220703125.d0,  &
     &  pi = 3.141592653589793238d0, alpha=1.0d-6)


! roots of unity array
! relies on x being largest dimension?
      double complex u(nxp)


! for checksum data
      double complex sums(0:niter_default)

! number of iterations
      integer niter


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
!  - twiddle contains exponents for the time evolution operator. 
!---------------------------------------------------------------------

      double complex, allocatable ::  &
     &                 u0(:), pad1(:),  &
     &                 u1(:), pad2(:)
!     >                 u2(:)
      double precision, allocatable :: twiddle(:)
!---------------------------------------------------------------------
! Large arrays are in module so that they are allocated on the
! heap rather than the stack. This module is not
! referenced directly anywhere else. Padding is to avoid accidental 
! cache problems, since all array sizes are powers of two.
!---------------------------------------------------------------------


      end module ft_fields


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use ft_data, only : ntotalp
      use ft_fields

      implicit none

      integer ios


      allocate (  &
     &          u0(ntotalp), pad1(3),  &
     &          u1(ntotalp), pad2(3),  &
!     >          u2(ntotalp),
     &          twiddle(ntotalp),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         stop
      endif

      return
      end


