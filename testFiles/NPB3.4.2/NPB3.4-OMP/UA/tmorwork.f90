!------------------------------------------------------------------
!------------------------------------------------------------------
!     module for thread-local working arrays
!------------------------------------------------------------------
!------------------------------------------------------------------
      module tmorwork

      double precision, pointer ::  &
     &                   tmorwk(:,:), mormulwk(:,:)

      double precision, pointer ::  &
     &                   tmorl(:), mormull(:)
      integer :: myid, nwthreads
!$omp threadprivate( tmorl, mormull, myid, nwthreads )

      end module tmorwork

