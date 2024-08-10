!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  cg_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module cg_data

      include 'npbparams.h'

!---------------------------------------------------------------------
!  Class specific parameters are defined in the npbparams.h
!  include file, which is written by the sys/setparams.c program.
!---------------------------------------------------------------------


! ... dimension parameters
      integer(kz) nz, naz
      parameter( nz = int(na,kz)*(nonzer+1)*(nonzer+1) )
      parameter( naz = int(na,kz)*(nonzer+1) )

! ... main_int_mem
      integer, allocatable ::  colidx(:),  &
     &                         iv(:),  arow(:), acol(:)
      integer(kz), allocatable ::  rowstr(:)

! ... main_flt_mem
      double precision, allocatable ::  &
     &                         v(:), aelt(:), a(:),  &
     &                         x(:),  &
     &                         z(:),  &
     &                         p(:),  &
     &                         q(:),  &
     &                         r(:)

! ... partition size
      integer                  naa,  &
     &                         firstrow,  &
     &                         lastrow,  &
     &                         firstcol,  &
     &                         lastcol
      integer(kz)              nzz

      double precision         amult, tran
!$omp threadprivate (amult, tran)

      external         timer_read
      double precision timer_read

      integer T_init, T_bench, T_conj_grad, T_last
      parameter (T_init=1, T_bench=2, T_conj_grad=3, T_last=3)

      logical timeron

      end module cg_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  tinfo module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module tinfo

      use cg_data, only : kz
      integer        max_threads
      parameter      (max_threads=1024)

      integer(kz)    last_n(0:max_threads)

      integer        myid, num_threads, ilow, ihigh
!$omp threadprivate (myid, num_threads, ilow, ihigh)

      end module tinfo


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use cg_data
      implicit none

      integer ios


      allocate (  &
     &          colidx(nz), rowstr(na+1),  &
     &          iv(nz+na),  arow(na), acol(naz),  &
     &          v(nz), aelt(naz), a(nz),  &
     &          x(na+2),  &
     &          z(na+2),  &
     &          p(na+2),  &
     &          q(na+2),  &
     &          r(na+2),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         stop
      endif

      return
      end

