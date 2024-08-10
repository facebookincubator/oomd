      program
      implicit none
      double precision t, mpi_wtime
      external mpi_wtime
      t = 0.0
      t = mpi_wtime()
      print *, t
      t = mpi_wtime()
      print *, t
      end
