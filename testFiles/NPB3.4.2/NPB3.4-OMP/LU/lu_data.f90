!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---  lu_data module
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module lu_data

!---------------------------------------------------------------------
!   npbparams.h defines parameters that depend on the class and 
!   number of nodes
!---------------------------------------------------------------------

      include 'npbparams.h'

!---------------------------------------------------------------------
!   parameters which can be overridden in runtime config file
!   isiz1,isiz2,isiz3 give the maximum size
!   ipr = 1 to print out verbose information
!   omega = 2.0 is correct for all classes
!   tolrsd is tolerance levels for steady state residuals
!---------------------------------------------------------------------
      integer ipr_default
      parameter (ipr_default = 1)
      double precision omega_default
      parameter (omega_default = 1.2d0)
      double precision tolrsd1_def, tolrsd2_def, tolrsd3_def,  &
     &                 tolrsd4_def, tolrsd5_def
      parameter (tolrsd1_def=1.0e-08,  &
     &          tolrsd2_def=1.0e-08, tolrsd3_def=1.0e-08,  &
     &          tolrsd4_def=1.0e-08, tolrsd5_def=1.0e-08)

      double precision c1, c2, c3, c4, c5
      parameter( c1 = 1.40d+00, c2 = 0.40d+00,  &
     &           c3 = 1.00d-01, c4 = 1.00d+00,  &
     &           c5 = 1.40d+00 )

!---------------------------------------------------------------------
!   grid
!---------------------------------------------------------------------
      integer nx, ny, nz
      integer nx0, ny0, nz0
      integer ist, iend
      integer jst, jend
      integer ii1, ii2
      integer ji1, ji2
      integer ki1, ki2
      double precision  dxi, deta, dzeta
      double precision  tx1, tx2, tx3
      double precision  ty1, ty2, ty3
      double precision  tz1, tz2, tz3

!---------------------------------------------------------------------
!   dissipation
!---------------------------------------------------------------------
      double precision dx1, dx2, dx3, dx4, dx5
      double precision dy1, dy2, dy3, dy4, dy5
      double precision dz1, dz2, dz3, dz4, dz5
      double precision dssp

!---------------------------------------------------------------------
!   field variables and residuals
!---------------------------------------------------------------------
      double precision, allocatable ::  &
     &                 u   (:,:,:,:),  &
     &                 rsd (:,:,:,:),  &
     &                 frct(:,:,:,:),  &
     &                 qs    (:,:,:),  &
     &                 rho_i (:,:,:)

      double precision flux(5,isiz1)
!$omp threadprivate( flux )


!---------------------------------------------------------------------
!   output control parameters
!---------------------------------------------------------------------
      integer ipr, inorm

!---------------------------------------------------------------------
!   newton-raphson iteration control parameters
!---------------------------------------------------------------------
      integer itmax, invert
      double precision  dt, omega, tolrsd(5),  &
     &        rsdnm(5), errnm(5), frc, ttotal

      double precision a(5,5,isiz1),  &
     &                 b(5,5,isiz1),  &
     &                 c(5,5,isiz1),  &
     &                 d(5,5,isiz1)
!$omp threadprivate( a, b, c, d )

!---------------------------------------------------------------------
!   coefficients of the exact solution
!---------------------------------------------------------------------
      double precision ce(5,13)

!---------------------------------------------------------------------
!   working arrays for surface integral
!---------------------------------------------------------------------
      double precision, allocatable ::  &
     &                 phi1(:,:),  &
     &                 phi2(:,:)

!---------------------------------------------------------------------
!   timers
!---------------------------------------------------------------------
      integer t_rhsx,t_rhsy,t_rhsz,t_rhs,t_jacld,t_blts,  &
     &        t_jacu,t_buts,t_add,t_l2norm,t_last,t_total
      parameter (t_total = 1)
      parameter (t_rhsx = 2)
      parameter (t_rhsy = 3)
      parameter (t_rhsz = 4)
      parameter (t_rhs = 5)
      parameter (t_jacld = 6)
      parameter (t_blts = 7)
      parameter (t_jacu = 8)
      parameter (t_buts = 9)
      parameter (t_add = 10)
      parameter (t_l2norm = 11)
      parameter (t_last = 11)

      logical timeron
      double precision maxtime

      end module lu_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use lu_data
      implicit none

      integer ios

!---------------------------------------------------------------------
!   to improve cache performance, second two dimensions padded by 1 
!   for even number sizes only.
!   Note: corresponding array (called "v") in routines blts, buts, 
!   and l2norm are similarly padded
!---------------------------------------------------------------------

      allocate (  &
     &          u   (5,isiz1/2*2+1,isiz2/2*2+1,isiz3),  &
     &          rsd (5,isiz1/2*2+1,isiz2/2*2+1,isiz3),  &
     &          frct(5,isiz1/2*2+1,isiz2/2*2+1,isiz3),  &
     &          qs    (isiz1/2*2+1,isiz2/2*2+1,isiz3),  &
     &          rho_i (isiz1/2*2+1,isiz2/2*2+1,isiz3),  &
     &          phi1  (0:isiz2+1,0:isiz3+1),  &
     &          phi2  (0:isiz2+1,0:isiz3+1),  &
     &          stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         stop
      endif

      return
      end
