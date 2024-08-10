!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  syncs module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module syncs

      use lu_data, only : isiz2

!---------------------------------------------------------------------
!  Flags used for thread synchronization for pipeline operation
!---------------------------------------------------------------------

      integer padim
      parameter (padim=16)
      integer isync(padim,0:isiz2), mthreadnum, iam
!$omp threadprivate( mthreadnum, iam )

      end module syncs


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_init( jdim )

!---------------------------------------------------------------------
!   Initialize sync-related variables
!---------------------------------------------------------------------

      use syncs
      implicit none

      integer jdim

!$    integer, external :: omp_get_num_threads, omp_get_thread_num

      mthreadnum = 0
!$    mthreadnum = omp_get_num_threads() - 1
      if (mthreadnum .gt. jdim) mthreadnum = jdim
      iam = 0
!$    iam = omp_get_thread_num()
      if (iam .le. mthreadnum) isync(1,iam) = 0

      return
      end

!---------------------------------------------------------------------
!   Thread synchronization for pipeline operation
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_left( ldmx, ldmy, ldmz, v )

!---------------------------------------------------------------------
!   Thread synchronization for pipeline operation
!---------------------------------------------------------------------

      use syncs
      implicit none

      integer ldmx, ldmy, ldmz
      double precision  v( 5, ldmx/2*2+1, ldmy/2*2+1, ldmz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      integer neigh, iv


      if (iam .gt. 0 .and. iam .le. mthreadnum) then
         neigh = iam - 1
!$omp atomic read
         iv = isync(1,neigh)
         do while (iv .eq. 0)
!$omp atomic read
            iv = isync(1,neigh)
         end do
!$omp atomic write
         isync(1,neigh) = 0
      endif
!$omp flush(isync,v)


      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine sync_right( ldmx, ldmy, ldmz, v )

!---------------------------------------------------------------------
!   Thread synchronization for pipeline operation
!---------------------------------------------------------------------

      use syncs
      implicit none

      integer ldmx, ldmy, ldmz
      double precision  v( 5, ldmx/2*2+1, ldmy/2*2+1, ldmz)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      integer iv


!$omp flush(isync,v)
      if (iam .lt. mthreadnum) then
!$omp atomic read
         iv = isync(1,iam)
         do while (iv .eq. 1)
!$omp atomic read
            iv = isync(1,iam)
         end do
!$omp atomic write
         isync(1,iam) = 1
      endif


      return
      end
