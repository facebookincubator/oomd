
!---------------------------------------------------------------------
!---------------------------------------------------------------------

       subroutine x_solve

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! this function performs the solution of the approximate factorization
! step in the x-direction for all five matrix components
! simultaneously. The Thomas algorithm is employed to solve the
! systems for the x-lines. Boundary conditions are non-periodic
!---------------------------------------------------------------------

       use sp_data
       use mpinpb

       implicit none


       integer i, j, k, jp, kp, n, iend, jsize, ksize, i1, i2,  &
     &         buffer_size, c, m, p, istart, stage, error,  &
     &         requests(2), statuses(MPI_STATUS_SIZE, 2)
       double precision  r1, r2, d, e, s(5), sm1, sm2,  &
     &                   fac1, fac2



!---------------------------------------------------------------------
!      OK, now we know that there are multiple processors
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! now do a sweep on a layer-by-layer basis, i.e. sweeping through cells
! on this node in the direction of increasing i for the forward sweep,
! and after that reversing the direction for the backsubstitution.
!---------------------------------------------------------------------

       if (timeron) call timer_start(t_xsolve)
!---------------------------------------------------------------------
!                          FORWARD ELIMINATION  
!---------------------------------------------------------------------
       do    stage = 1, ncells
          c         = slice(1,stage)

          istart = 0
          iend   = cell_size(1,c)-1

          jsize     = cell_size(2,c)
          ksize     = cell_size(3,c)
          jp        = cell_coord(2,c)-1
          kp        = cell_coord(3,c)-1

          buffer_size = (jsize-start(2,c)-end(2,c)) *  &
     &                  (ksize-start(3,c)-end(3,c))

          if ( stage .ne. 1) then

!---------------------------------------------------------------------
!            if this is not the first processor in this row of cells, 
!            receive data from predecessor containing the right hand
!            sides and the upper diagonal elements of the previous two rows
!---------------------------------------------------------------------
             if (timeron) call timer_start(t_xcomm)
             call mpi_irecv(in_buffer, 22*buffer_size,  &
     &                      dp_type, predecessor(1),  &
     &                      DEFAULT_TAG,  comm_solve,  &
     &                      requests(1), error)
             if (timeron) call timer_stop(t_xcomm)


!---------------------------------------------------------------------
!            communication has already been started. 
!            compute the left hand side while waiting for the msg
!---------------------------------------------------------------------
             call lhsx(c)

!---------------------------------------------------------------------
!            wait for pending communication to complete
!            This waits on the current receive and on the send
!            from the previous stage. They always come in pairs. 
!---------------------------------------------------------------------

             if (timeron) call timer_start(t_xcomm)
             call mpi_waitall(2, requests, statuses, error)
             if (timeron) call timer_stop(t_xcomm)

!---------------------------------------------------------------------
!            unpack the buffer                                 
!---------------------------------------------------------------------
             i  = istart
             i1 = istart + 1
             n = 0

!---------------------------------------------------------------------
!            create a running pointer
!---------------------------------------------------------------------
             p = 0
             do    k = start(3,c), ksize-end(3,c)-1
                do    j = start(2,c), jsize-end(2,c)-1
                   lhs(i,j,k,n+2,c) = lhs(i,j,k,n+2,c) -  &
     &                       in_buffer(p+1) * lhs(i,j,k,n+1,c)
                   lhs(i,j,k,n+3,c) = lhs(i,j,k,n+3,c) -  &
     &                       in_buffer(p+2) * lhs(i,j,k,n+1,c)
                   do    m = 1, 3
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                       in_buffer(p+2+m) * lhs(i,j,k,n+1,c)
                   end do
                   d            = in_buffer(p+6)
                   e            = in_buffer(p+7)
                   do    m = 1, 3
                      s(m) = in_buffer(p+7+m)
                   end do
                   r1 = lhs(i,j,k,n+2,c)
                   lhs(i,j,k,n+3,c) = lhs(i,j,k,n+3,c) - d * r1
                   lhs(i,j,k,n+4,c) = lhs(i,j,k,n+4,c) - e * r1
                   do    m = 1, 3
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) - s(m) * r1
                   end do
                   r2 = lhs(i1,j,k,n+1,c)
                   lhs(i1,j,k,n+2,c) = lhs(i1,j,k,n+2,c) - d * r2
                   lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) - e * r2
                   do    m = 1, 3
                      rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) - s(m) * r2
                   end do
                   p = p + 10
                end do
             end do

             do    m = 4, 5
                n = (m-3)*5
                do    k = start(3,c), ksize-end(3,c)-1
                   do    j = start(2,c), jsize-end(2,c)-1
                      lhs(i,j,k,n+2,c) = lhs(i,j,k,n+2,c) -  &
     &                          in_buffer(p+1) * lhs(i,j,k,n+1,c)
                      lhs(i,j,k,n+3,c) = lhs(i,j,k,n+3,c) -  &
     &                          in_buffer(p+2) * lhs(i,j,k,n+1,c)
                      rhs(i,j,k,m,c)   = rhs(i,j,k,m,c) -  &
     &                          in_buffer(p+3) * lhs(i,j,k,n+1,c)
                      d                = in_buffer(p+4)
                      e                = in_buffer(p+5)
                      s(m)             = in_buffer(p+6)
                      r1 = lhs(i,j,k,n+2,c)
                      lhs(i,j,k,n+3,c) = lhs(i,j,k,n+3,c) - d * r1
                      lhs(i,j,k,n+4,c) = lhs(i,j,k,n+4,c) - e * r1
                      rhs(i,j,k,m,c)   = rhs(i,j,k,m,c) - s(m) * r1
                      r2 = lhs(i1,j,k,n+1,c)
                      lhs(i1,j,k,n+2,c) = lhs(i1,j,k,n+2,c) - d * r2
                      lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) - e * r2
                      rhs(i1,j,k,m,c)   = rhs(i1,j,k,m,c) - s(m) * r2
                      p = p + 6
                   end do
                end do
             end do

          else            

!---------------------------------------------------------------------
!            if this IS the first cell, we still compute the lhs
!---------------------------------------------------------------------
             call lhsx(c)
          endif

!---------------------------------------------------------------------
!         perform the Thomas algorithm; first, FORWARD ELIMINATION     
!---------------------------------------------------------------------
          n = 0

          do    k = start(3,c), ksize-end(3,c)-1
             do    j = start(2,c), jsize-end(2,c)-1
                do    i = istart, iend-2
                   i1 = i  + 1
                   i2 = i  + 2
                   fac1               = 1.d0/lhs(i,j,k,n+3,c)
                   lhs(i,j,k,n+4,c)   = fac1*lhs(i,j,k,n+4,c)
                   lhs(i,j,k,n+5,c)   = fac1*lhs(i,j,k,n+5,c)
                   do    m = 1, 3
                      rhs(i,j,k,m,c) = fac1*rhs(i,j,k,m,c)
                   end do
                   lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) -  &
     &                         lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+4,c)
                   lhs(i1,j,k,n+4,c) = lhs(i1,j,k,n+4,c) -  &
     &                         lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+5,c)
                   do    m = 1, 3
                      rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) -  &
     &                         lhs(i1,j,k,n+2,c)*rhs(i,j,k,m,c)
                   end do
                   lhs(i2,j,k,n+2,c) = lhs(i2,j,k,n+2,c) -  &
     &                         lhs(i2,j,k,n+1,c)*lhs(i,j,k,n+4,c)
                   lhs(i2,j,k,n+3,c) = lhs(i2,j,k,n+3,c) -  &
     &                         lhs(i2,j,k,n+1,c)*lhs(i,j,k,n+5,c)
                   do    m = 1, 3
                      rhs(i2,j,k,m,c) = rhs(i2,j,k,m,c) -  &
     &                         lhs(i2,j,k,n+1,c)*rhs(i,j,k,m,c)
                   end do
                end do
             end do
          end do

!---------------------------------------------------------------------
!         The last two rows in this grid block are a bit different, 
!         since they do not have two more rows available for the
!         elimination of off-diagonal entries
!---------------------------------------------------------------------

          i  = iend - 1
          i1 = iend
          do    k = start(3,c), ksize-end(3,c)-1
             do    j = start(2,c), jsize-end(2,c)-1
                fac1               = 1.d0/lhs(i,j,k,n+3,c)
                lhs(i,j,k,n+4,c)   = fac1*lhs(i,j,k,n+4,c)
                lhs(i,j,k,n+5,c)   = fac1*lhs(i,j,k,n+5,c)
                do    m = 1, 3
                   rhs(i,j,k,m,c) = fac1*rhs(i,j,k,m,c)
                end do
                lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) -  &
     &                      lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+4,c)
                lhs(i1,j,k,n+4,c) = lhs(i1,j,k,n+4,c) -  &
     &                      lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+5,c)
                do    m = 1, 3
                   rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) -  &
     &                      lhs(i1,j,k,n+2,c)*rhs(i,j,k,m,c)
                end do
!---------------------------------------------------------------------
!               scale the last row immediately (some of this is
!               overkill in case this is the last cell)
!---------------------------------------------------------------------
                fac2               = 1.d0/lhs(i1,j,k,n+3,c)
                lhs(i1,j,k,n+4,c) = fac2*lhs(i1,j,k,n+4,c)
                lhs(i1,j,k,n+5,c) = fac2*lhs(i1,j,k,n+5,c)  
                do    m = 1, 3
                   rhs(i1,j,k,m,c) = fac2*rhs(i1,j,k,m,c)
                end do
             end do
          end do

!---------------------------------------------------------------------
!         do the u+c and the u-c factors                 
!---------------------------------------------------------------------

          do    m = 4, 5
             n = (m-3)*5
             do    k = start(3,c), ksize-end(3,c)-1
                do    j = start(2,c), jsize-end(2,c)-1
                   do    i = istart, iend-2
                   i1 = i  + 1
                   i2 = i  + 2
                   fac1               = 1.d0/lhs(i,j,k,n+3,c)
                   lhs(i,j,k,n+4,c)   = fac1*lhs(i,j,k,n+4,c)
                   lhs(i,j,k,n+5,c)   = fac1*lhs(i,j,k,n+5,c)
                   rhs(i,j,k,m,c) = fac1*rhs(i,j,k,m,c)
                   lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) -  &
     &                         lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+4,c)
                   lhs(i1,j,k,n+4,c) = lhs(i1,j,k,n+4,c) -  &
     &                         lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+5,c)
                   rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) -  &
     &                         lhs(i1,j,k,n+2,c)*rhs(i,j,k,m,c)
                   lhs(i2,j,k,n+2,c) = lhs(i2,j,k,n+2,c) -  &
     &                         lhs(i2,j,k,n+1,c)*lhs(i,j,k,n+4,c)
                   lhs(i2,j,k,n+3,c) = lhs(i2,j,k,n+3,c) -  &
     &                         lhs(i2,j,k,n+1,c)*lhs(i,j,k,n+5,c)
                   rhs(i2,j,k,m,c) = rhs(i2,j,k,m,c) -  &
     &                         lhs(i2,j,k,n+1,c)*rhs(i,j,k,m,c)
                end do
             end do
          end do

!---------------------------------------------------------------------
!            And again the last two rows separately
!---------------------------------------------------------------------
             i  = iend - 1
             i1 = iend
             do    k = start(3,c), ksize-end(3,c)-1
                do    j = start(2,c), jsize-end(2,c)-1
                fac1               = 1.d0/lhs(i,j,k,n+3,c)
                lhs(i,j,k,n+4,c)   = fac1*lhs(i,j,k,n+4,c)
                lhs(i,j,k,n+5,c)   = fac1*lhs(i,j,k,n+5,c)
                rhs(i,j,k,m,c)     = fac1*rhs(i,j,k,m,c)
                lhs(i1,j,k,n+3,c) = lhs(i1,j,k,n+3,c) -  &
     &                      lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+4,c)
                lhs(i1,j,k,n+4,c) = lhs(i1,j,k,n+4,c) -  &
     &                      lhs(i1,j,k,n+2,c)*lhs(i,j,k,n+5,c)
                rhs(i1,j,k,m,c)   = rhs(i1,j,k,m,c) -  &
     &                      lhs(i1,j,k,n+2,c)*rhs(i,j,k,m,c)
!---------------------------------------------------------------------
!               Scale the last row immediately (some of this is overkill
!               if this is the last cell)
!---------------------------------------------------------------------
                fac2               = 1.d0/lhs(i1,j,k,n+3,c)
                lhs(i1,j,k,n+4,c) = fac2*lhs(i1,j,k,n+4,c)
                lhs(i1,j,k,n+5,c) = fac2*lhs(i1,j,k,n+5,c)
                rhs(i1,j,k,m,c)   = fac2*rhs(i1,j,k,m,c)

             end do
          end do
       end do

!---------------------------------------------------------------------
!         send information to the next processor, except when this
!         is the last grid block
!---------------------------------------------------------------------
          if (stage .ne. ncells) then

!---------------------------------------------------------------------
!            create a running pointer for the send buffer  
!---------------------------------------------------------------------
             p = 0
             n = 0
             do    k = start(3,c), ksize-end(3,c)-1
                do    j = start(2,c), jsize-end(2,c)-1
                   do    i = iend-1, iend
                      out_buffer(p+1) = lhs(i,j,k,n+4,c)
                      out_buffer(p+2) = lhs(i,j,k,n+5,c)
                      do    m = 1, 3
                         out_buffer(p+2+m) = rhs(i,j,k,m,c)
                      end do
                      p = p+5
                   end do
                end do
             end do

             do    m = 4, 5
                n = (m-3)*5
                do    k = start(3,c), ksize-end(3,c)-1
                   do    j = start(2,c), jsize-end(2,c)-1
                      do    i = iend-1, iend
                         out_buffer(p+1) = lhs(i,j,k,n+4,c)
                         out_buffer(p+2) = lhs(i,j,k,n+5,c)
                         out_buffer(p+3) = rhs(i,j,k,m,c)
                         p = p + 3
                      end do
                   end do
                end do
             end do

!---------------------------------------------------------------------
! send data to next phase
! can't receive data yet because buffer size will be wrong 
!---------------------------------------------------------------------
             if (timeron) call timer_start(t_xcomm)
             call mpi_isend(out_buffer, 22*buffer_size,  &
     &                     dp_type, successor(1),  &
     &                     DEFAULT_TAG, comm_solve,  &
     &                     requests(2), error)
             if (timeron) call timer_stop(t_xcomm)

          endif
       end do

!---------------------------------------------------------------------
!      now go in the reverse direction                      
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!                         BACKSUBSTITUTION 
!---------------------------------------------------------------------
       do    stage = ncells, 1, -1
          c = slice(1,stage)

          istart = 0
          iend   = cell_size(1,c)-1

          jsize = cell_size(2,c)
          ksize = cell_size(3,c)
          jp    = cell_coord(2,c)-1
          kp    = cell_coord(3,c)-1

          buffer_size = (jsize-start(2,c)-end(2,c)) *  &
     &                  (ksize-start(3,c)-end(3,c))

          if (stage .ne. ncells) then

!---------------------------------------------------------------------
!            if this is not the starting cell in this row of cells, 
!            wait for a message to be received, containing the 
!            solution of the previous two stations     
!---------------------------------------------------------------------
             if (timeron) call timer_start(t_xcomm)
             call mpi_irecv(in_buffer, 10*buffer_size,  &
     &                      dp_type, successor(1),  &
     &                      DEFAULT_TAG, comm_solve,  &
     &                      requests(1), error)
             if (timeron) call timer_stop(t_xcomm)


!---------------------------------------------------------------------
!            communication has already been started
!            while waiting, do the block-diagonal inversion for the 
!            cell that was just finished                
!---------------------------------------------------------------------

             call ninvr(slice(1,stage+1))

!---------------------------------------------------------------------
!            wait for pending communication to complete
!---------------------------------------------------------------------
             if (timeron) call timer_start(t_xcomm)
             call mpi_waitall(2, requests, statuses, error)
             if (timeron) call timer_stop(t_xcomm)

!---------------------------------------------------------------------
!            unpack the buffer for the first three factors         
!---------------------------------------------------------------------
             n = 0
             p = 0
             i  = iend
             i1 = i - 1
             do    m = 1, 3
                do   k = start(3,c), ksize-end(3,c)-1
                   do   j = start(2,c), jsize-end(2,c)-1
                      sm1 = in_buffer(p+1)
                      sm2 = in_buffer(p+2)
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                        lhs(i,j,k,n+4,c)*sm1 -  &
     &                        lhs(i,j,k,n+5,c)*sm2
                      rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) -  &
     &                        lhs(i1,j,k,n+4,c) * rhs(i,j,k,m,c) -  &
     &                        lhs(i1,j,k,n+5,c) * sm1
                      p = p + 2
                   end do
                end do
             end do

!---------------------------------------------------------------------
!            now unpack the buffer for the remaining two factors
!---------------------------------------------------------------------
             do    m = 4, 5
                n = (m-3)*5
                do   k = start(3,c), ksize-end(3,c)-1
                   do   j = start(2,c), jsize-end(2,c)-1
                      sm1 = in_buffer(p+1)
                      sm2 = in_buffer(p+2)
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                        lhs(i,j,k,n+4,c)*sm1 -  &
     &                        lhs(i,j,k,n+5,c)*sm2
                      rhs(i1,j,k,m,c) = rhs(i1,j,k,m,c) -  &
     &                        lhs(i1,j,k,n+4,c) * rhs(i,j,k,m,c) -  &
     &                        lhs(i1,j,k,n+5,c) * sm1
                      p = p + 2
                   end do
                end do
             end do

          else

!---------------------------------------------------------------------
!            now we know this is the first grid block on the back sweep,
!            so we don't need a message to start the substitution. 
!---------------------------------------------------------------------
             i  = iend-1
             i1 = iend
             n = 0
             do   m = 1, 3
                do   k = start(3,c), ksize-end(3,c)-1
                   do   j = start(2,c), jsize-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                             lhs(i,j,k,n+4,c)*rhs(i1,j,k,m,c)
                   end do
                end do
             end do

             do    m = 4, 5
                n = (m-3)*5
                do   k = start(3,c), ksize-end(3,c)-1
                   do   j = start(2,c), jsize-end(2,c)-1
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                             lhs(i,j,k,n+4,c)*rhs(i1,j,k,m,c)
                   end do
                end do
             end do
          endif

!---------------------------------------------------------------------
!         Whether or not this is the last processor, we always have
!         to complete the back-substitution 
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!         The first three factors
!---------------------------------------------------------------------
          n = 0
          do   m = 1, 3
             do   k = start(3,c), ksize-end(3,c)-1
                do   j = start(2,c), jsize-end(2,c)-1
                   do    i = iend-2, istart, -1
                      i1 = i  + 1
                      i2 = i  + 2
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                          lhs(i,j,k,n+4,c)*rhs(i1,j,k,m,c) -  &
     &                          lhs(i,j,k,n+5,c)*rhs(i2,j,k,m,c)
                   end do
                end do
             end do
          end do

!---------------------------------------------------------------------
!         And the remaining two
!---------------------------------------------------------------------
          do    m = 4, 5
             n = (m-3)*5
             do   k = start(3,c), ksize-end(3,c)-1
                do   j = start(2,c), jsize-end(2,c)-1
                   do    i = iend-2, istart, -1
                      i1 = i  + 1
                      i2 = i  + 2
                      rhs(i,j,k,m,c) = rhs(i,j,k,m,c) -  &
     &                          lhs(i,j,k,n+4,c)*rhs(i1,j,k,m,c) -  &
     &                          lhs(i,j,k,n+5,c)*rhs(i2,j,k,m,c)
                   end do
                end do
             end do
          end do

!---------------------------------------------------------------------
!         send on information to the previous processor, if needed
!---------------------------------------------------------------------
          if (stage .ne.  1) then
             i  = istart
             i1 = istart+1
             p = 0
             do    m = 1, 5
                do    k = start(3,c), ksize-end(3,c)-1
                   do    j = start(2,c), jsize-end(2,c)-1
                      out_buffer(p+1) = rhs(i,j,k,m,c)
                       out_buffer(p+2) = rhs(i1,j,k,m,c)
                      p = p + 2
                   end do
                end do
             end do

!---------------------------------------------------------------------
!            pack and send the buffer
!---------------------------------------------------------------------
             if (timeron) call timer_start(t_xcomm)
             call mpi_isend(out_buffer, 10*buffer_size,  &
     &                     dp_type, predecessor(1),  &
     &                     DEFAULT_TAG, comm_solve,  &
     &                     requests(2), error)
             if (timeron) call timer_stop(t_xcomm)

          endif

!---------------------------------------------------------------------
!         If this was the last stage, do the block-diagonal inversion          
!---------------------------------------------------------------------
          if (stage .eq. 1) call ninvr(c)

       end do

       if (timeron) call timer_stop(t_xsolve)

       return
       end
    






