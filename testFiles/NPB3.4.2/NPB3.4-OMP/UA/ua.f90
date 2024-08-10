!-------------------------------------------------------------------------c
!                                                                         c
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         c
!                                                                         c
!                      O p e n M P     V E R S I O N                      c
!                                                                         c
!                                   U A                                   c
!                                                                         c
!-------------------------------------------------------------------------c
!                                                                         c
!    This benchmark is the OpenMP version of the NPB UA code.             c
!    Refer to NAS Technical Report NAS--04-006 for details                c
!                                                                         c
!    Permission to use, copy, distribute and modify this software         c
!    for any purpose with or without fee is hereby granted.  We           c
!    request, however, that all derived work reference the NAS            c
!    Parallel Benchmarks 3.4. This software is provided "as is"           c
!    without express or implied warranty.                                 c
!                                                                         c
!    Information on NPB 3.4, including the technical report, the          c
!    original specifications, source code, results and information        c
!    on how to submit new results, is available at:                       c
!                                                                         c
!           http://www.nas.nasa.gov/Software/NPB/                         c
!                                                                         c
!    Send comments or suggestions to  npb@nas.nasa.gov                    c
!                                                                         c
!          NAS Parallel Benchmarks Group                                  c
!          NASA Ames Research Center                                      c
!          Mail Stop: T27A-1                                              c
!          Moffett Field, CA   94035-1000                                 c
!                                                                         c
!          E-mail:  npb@nas.nasa.gov                                      c
!          Fax:     (650) 604-3957                                        c
!                                                                         c
!-------------------------------------------------------------------------c

!---------------------------------------------------------------------
!
! Author: H. Feng
!         R. Van der Wijngaart
!---------------------------------------------------------------------

      program ua

      use ua_data
      implicit none

      integer          step, ie,iside,i,j, fstatus,k
      external         timer_read
      double precision timer_read, mflops, tmax, nelt_tot
      character        class
      logical          ifmortar, verified
!$    integer          omp_get_max_threads
!$    external         omp_get_max_threads

      double precision t2, trecs(t_last)
      character t_names(t_last)*10

!---------------------------------------------------------------------
!     Read input file (if it exists), else take
!     defaults from parameters
!---------------------------------------------------------------------
          
      call check_timer_flag( timeron )
      if (timeron) then
         t_names(t_total) = 'total'
         t_names(t_init) = 'init'
         t_names(t_convect) = 'convect'
         t_names(t_transfb_c) = 'transfb_c'
         t_names(t_diffusion) = 'diffusion'
         t_names(t_transf) = 'transf'
         t_names(t_transfb) = 'transfb'
         t_names(t_adaptation) = 'adaptation'
         t_names(t_transf2) = 'transf+b'
         t_names(t_add2) = 'add2'
      endif

      write (*,1000) 
      open (unit=2,file='inputua.data',status='old', iostat=fstatus)

      if (fstatus .eq. 0) then
        write(*,233) 
 233    format(' Reading from input file inputua.data')
        read (2,*) fre
        read (2,*) niter
        read (2,*) nmxh
        read (2,*) alpha
        class = 'U'
        close(2)
      else
        write(*,234) 
        fre        = fre_default
        niter      = niter_default
        nmxh       = nmxh_default
        alpha      = alpha_default
        class      = class_default
      endif
 234  format(' No input file inputua.data. Using compiled defaults')

      dlmin = 0.5d0**refine_max
      dtime = 0.04d0*dlmin

      write (*,1001) refine_max
      write (*,1002) fre
      write (*,1003) niter, dtime
      write (*,1004) nmxh
      write (*,1005) alpha
!$    write (*,1006) omp_get_max_threads()
      write (*,*)


 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-OMP)',  &
     &          ' - UA Benchmark', /)
 1001 format(' Levels of refinement:        ', i8)
 1002 format(' Adaptation frequency:        ', i8)
 1003 format(' Time steps:                  ', i8, '    dt: ', g15.6)
 1004 format(' CG iterations:               ', i8)
 1005 format(' Heat source radius:          ', f8.4)
 1006 format(' Number of available threads: ', i8)

      do i = 1, t_last
         call timer_clear(i)
      end do
      if (timeron) call timer_start(t_init)

      call alloc_space

!.....set up initial mesh (single element) and solution (all zero)
      call create_initial_grid

      call r_init_omp(ta1,ntot,0.d0)
      call nr_init_omp(sje,4*6*nelt,0)

      call init_locks

!.....compute tables of coefficients and weights      
      call coef 
      call geom1

!.....compute the discrete laplacian operators
      call setdef

!.....prepare for the preconditioner
      call setpcmo_pre

!.....refine initial mesh and do some preliminary work
      time = 0.d0
      call mortar
      call prepwork
      call adaptation(ifmortar,0)
      if (timeron) call timer_stop(t_init)

      call timer_clear(1)

      time = 0.d0
      do step= 0, niter

        if (step .eq. 1) then
!.........reset the solution and start the timer, keep track of total no elms

          call r_init(ta1,ntot,0.d0)

          time = 0.d0
          nelt_tot = 0.d0
          do i = 1, t_last
             if (i.ne.t_init) call timer_clear(i)
          end do
          call timer_start(1)          
        endif

!.......advance the convection step 
        call convect(ifmortar)

        if (timeron) call timer_start(t_transf2)
!.......prepare the intital guess for cg
        call transf(tmort,ta1)

!.......compute residual for diffusion term based on intital guess

!.......compute the left hand side of equation, lapacian t
!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(ie,k,j,i) 
!$OMP DO 
        do ie = 1,nelt
          call laplacian(ta2(1,1,1,ie),ta1(1,1,1,ie),size_e(ie))
        end do
!$OMP END DO 
!.......compute the residual 
!$OMP DO
        do ie = 1, nelt
          do k=1,lx1
            do j=1,lx1
              do i=1,lx1
                trhs(i,j,k,ie) = trhs(i,j,k,ie) - ta2(i,j,k,ie)
              end do
            end do
          end do
        end do
!$OMP END DO
!$OMP END PARALLEL
!.......get the residual on mortar 
        call transfb(rmor,trhs)
        if (timeron) call timer_stop(t_transf2)

!.......apply boundary condition: zero out the residual on domain boundaries

!.......apply boundary conidtion to trhs
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ie,iside)
        do ie=1,nelt  
          do iside=1,nsides
            if (cbc(iside,ie).eq.0) then
              call facev(trhs(1,1,1,ie),iside,0.d0)
            end if
          end do
        end do
!$OMP END PARALLELDO
!.......apply boundary condition to rmor
        call col2(rmor,tmmor,nmor)

!.......call the conjugate gradient iterative solver
        call diffusion(ifmortar)

!.......add convection and diffusion
        if (timeron) call timer_start(t_add2)
        call add2(ta1,t,ntot)
        if (timeron) call timer_stop(t_add2)

        
!.......perform mesh adaptation
        time=time+dtime
        if ((step.ne.0).and.(step/fre*fre .eq. step)) then
           if (step .ne. niter) then
             call adaptation(ifmortar,step)
           end if
        else
          ifmortar = .false.
        end if
        nelt_tot = nelt_tot + dble(nelt)
      end do

      call timer_stop(1)
      tmax = timer_read(1)
       
      call verify(class, verified)

!.....compute millions of collocation points advanced per second.
!.....diffusion: nmxh advancements, convection: 1 advancement
      mflops = nelt_tot*dble(lx1*lx1*lx1*(nmxh+1))/(tmax*1.d6)

      call print_results('UA', class, refine_max, 0, 0, niter,  &
     &     tmax, mflops, '    coll. point advanced',  &
     &     verified, npbversion,compiletime, cs1, cs2, cs3, cs4, cs5,  &
     &     cs6, '(none)')

!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
      if (.not.timeron) goto 999

      do i=1, t_last
         trecs(i) = timer_read(i)
      end do
      if (tmax .eq. 0.0) tmax = 1.0

      write(*,800)
 800  format('  SECTION     Time (secs)')
      do i=1, t_last
         write(*,810) t_names(i), trecs(i), trecs(i)*100./tmax
         if (i.eq.t_transfb_c) then
            t2 = trecs(t_convect) - trecs(t_transfb_c)
            write(*,820) 'sub-convect', t2, t2*100./tmax
         else if (i.eq.t_transfb) then
            t2 = trecs(t_diffusion) - trecs(t_transf) - trecs(t_transfb)
            write(*,820) 'sub-diffuse', t2, t2*100./tmax
         endif
 810     format(2x,a10,':',f9.3,'  (',f6.2,'%)')
 820     format('    --> ',a11,':',f9.3,'  (',f6.2,'%)')
      end do

 999  continue

      end 

