      subroutine mpi_isend(buf,count,datatype,source,  &
     &                     tag,comm,request,ierror)
      implicit none
      integer buf(*), count,datatype,source,tag,comm,request,ierror
      call mpi_error()
      return
      end  

      subroutine mpi_irecv(buf,count,datatype,source,  &
     &                     tag,comm,request,ierror)
      implicit none
      integer buf(*), count,datatype,source,tag,comm,request,ierror
      call mpi_error()
      return
      end

      subroutine mpi_send(buf,count,datatype,dest,tag,comm,ierror)
      implicit none
      integer buf(*), count,datatype,dest,tag,comm,ierror
      call mpi_error()
      return
      end
      
      subroutine mpi_recv(buf,count,datatype,source,  &
     &                    tag,comm,status,ierror)
      implicit none
      integer buf(*), count,datatype,source,tag,comm,status(*),ierror
      call mpi_error()
      return
      end

      subroutine mpi_comm_split(comm,color,key,newcomm,ierror)
      implicit none
      integer comm,color,key,newcomm,ierror
      newcomm = comm
      return
      end

      subroutine mpi_comm_rank(comm, rank,ierr)
      implicit none
      integer comm, rank,ierr
      rank = 0
      return
      end

      subroutine mpi_comm_size(comm, size, ierr)
      implicit none
      integer comm, size, ierr
      size = 1
      return
      end

      double precision function mpi_wtime()
      implicit none
      double precision t
! This function must measure wall clock time, not CPU time. 
! Since there is no portable timer in Fortran (77)
! we call a routine compiled in C (though the C source may have
! to be tweaked). 
      call wtime(t)
! The following is not ok for "official" results because it reports
! CPU time not wall clock time. It may be useful for developing/testing
! on timeshared Crays, though. 
!     call second(t)

      mpi_wtime = t

      return
      end


! may be valid to call this in single processor case
      subroutine mpi_barrier(comm,ierror)
      implicit none
      integer comm,ierror
      return
      end

! may be valid to call this in single processor case
      subroutine mpi_bcast(buf, nitems, dtype, root, comm, ierr)
      implicit none
      integer buf(*), nitems, dtype, root, comm, ierr
      return
      end

      subroutine mpi_comm_dup(oldcomm, newcomm,ierror)
      implicit none
      integer oldcomm, newcomm,ierror
      newcomm= oldcomm
      return
      end

      subroutine mpi_error()
      implicit none
      print *, 'mpi_error called'
      stop
      end 

      subroutine mpi_abort(comm, errcode, ierr)
      implicit none
      integer comm, errcode, ierr
      print *, 'mpi_abort called'
      stop
      end

      subroutine mpi_finalize(ierr)
      implicit none
      integer ierr
      return
      end

      subroutine mpi_init(ierr)
      implicit none
      integer ierr
      return
      end


! assume double precision, which is all SP uses 
      subroutine mpi_reduce(inbuf, outbuf, nitems,  &
     &                      dtype, op, root, comm, ierr)
      implicit none
      include 'mpif.h'
      integer nitems, dtype, op, root, comm, ierr
      double precision inbuf(*), outbuf(*)

      if (dtype .eq. mpi_double_precision) then
         call dmpi_copy_dp(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_double_complex) then
         call dmpi_copy_dc(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_complex) then
         call dmpi_copy_complex(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_real) then
         call dmpi_copy_real(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_integer) then
         call dmpi_copy_int(inbuf, outbuf, nitems)
      else
         print *, 'mpi_reduce: unknown type ', dtype
      end if
      return
      end


      subroutine dmpi_copy_real(inbuf, outbuf, nitems)
      implicit none
      integer nitems, i
      real inbuf(*), outbuf(*)
      do i = 1, nitems
         outbuf(i) = inbuf(i)
      end do
      
      return
      end

      subroutine dmpi_copy_dp(inbuf, outbuf, nitems)
      implicit none
      integer nitems, i
      double precision inbuf(*), outbuf(*)
      do i = 1, nitems
         outbuf(i) = inbuf(i)
      end do
      
      return
      end

      subroutine dmpi_copy_dc(inbuf, outbuf, nitems)
      implicit none
      integer nitems, i
      double complex inbuf(*), outbuf(*)
      do i = 1, nitems
         outbuf(i) = inbuf(i)
      end do
      
      return
      end


      subroutine dmpi_copy_complex(inbuf, outbuf, nitems)
      implicit none
      integer nitems, i
      complex inbuf(*), outbuf(*)
      do i = 1, nitems
         outbuf(i) = inbuf(i)
      end do
      
      return
      end

      subroutine dmpi_copy_int(inbuf, outbuf, nitems)
      implicit none
      integer nitems, i
      integer inbuf(*), outbuf(*)
      do i = 1, nitems
         outbuf(i) = inbuf(i)
      end do
      
      return
      end

      subroutine mpi_allreduce(inbuf, outbuf, nitems,  &
     &                      dtype, op, comm, ierr)
      implicit none
      integer nitems, dtype, op, comm, ierr
      double precision inbuf(*), outbuf(*)

      call mpi_reduce(inbuf, outbuf, nitems,  &
     &                      dtype, op, 0, comm, ierr)
      return
      end

      subroutine mpi_alltoall(inbuf, nitems_in, dtype_in,  &
     &                        outbuf, nitems, dtype, comm, ierr)
      implicit none
      include 'mpif.h'
      integer nitems_in, dtype_in, comm, ierr, nitems, dtype
      double precision inbuf(*), outbuf(*)

      if (dtype .eq. mpi_double_precision) then
         call dmpi_copy_dp(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_double_complex) then
         call dmpi_copy_dc(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_complex) then
         call dmpi_copy_complex(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_real) then
         call dmpi_copy_real(inbuf, outbuf, nitems)
      else if (dtype .eq. mpi_integer) then
         call dmpi_copy_int(inbuf, outbuf, nitems)
      else
         print *, 'mpi_alltoall: unknown type ', dtype
      end if
      return
      end

      subroutine mpi_wait(request,status,ierror)
      implicit none
      integer request,status,ierror
      call mpi_error()
      return
      end

      subroutine mpi_waitall(count,requests,status,ierror)
      implicit none
      integer count,requests(*),status(*),ierror
      call mpi_error()
      return
      end

