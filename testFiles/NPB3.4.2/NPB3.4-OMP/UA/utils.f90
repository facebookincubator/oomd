!------------------------------------------------------------------
      subroutine reciprocal (a, n)
!------------------------------------------------------------------
!     initialize double precision array a with length of n
!------------------------------------------------------------------

      implicit none

      integer n, i
      double precision a(n)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(I)
      do i = 1, n
        a(i) = 1.d0/a(i)
      end do
!$OMP END PARALLEL DO

      return
      end
!------------------------------------------------------------------
      subroutine r_init_omp (a, n, const)
!------------------------------------------------------------------
!     initialize double precision array a with length of n
!------------------------------------------------------------------

      implicit none

      integer n, i
      double precision a(n), const

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(I)
      do i = 1, n
        a(i) = const
      end do
!$OMP END PARALLEL DO

      return
      end
!------------------------------------------------------------------
      subroutine r_init (a, n, const)
!------------------------------------------------------------------
!     initialize double precision array a with length of n
!------------------------------------------------------------------

      implicit none

      integer n, i
      double precision a(n), const

      do i = 1, n
        a(i) = const
      end do

      return
      end
!------------------------------------------------------------------
      subroutine nr_init_omp (a, n, const)
!------------------------------------------------------------------
!     initialize integer array a with length of n
!------------------------------------------------------------------

      implicit none

      integer n, i, a(n), const

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(I)
      do i = 1, n
        a(i) = const
      end do
!$OMP END PARALLEL DO

      return
      end

!------------------------------------------------------------------
      subroutine nr_init (a, n, const)
!------------------------------------------------------------------
!     initialize integer array a with length of n
!------------------------------------------------------------------

      implicit none

      integer n, i, a(n), const

      do i = 1, n
        a(i) = const
      end do

      return
      end
!------------------------------------------------------------------
      subroutine l_init_omp (a, n, const)
!------------------------------------------------------------------
!     initialize integer array a with length of n
!------------------------------------------------------------------

      implicit none
      integer n, i
      logical a(n), const

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(I)
      do i = 1, n
        a(i) = const
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      subroutine ncopy (a,b,n)
!------------------------------------------------------------------
!     copy array of integers b to a, the length of array is n
!------------------------------------------------------------------

      implicit none

      integer n,i
      integer a(n), b(n)

      do i = 1, n
        a(i) = b(i)
      end do

      return
      end

!-----------------------------------------------------------------
      subroutine copy (a,b,n)
!------------------------------------------------------------------
!     copy double precision array b to a, the length of array is n
!------------------------------------------------------------------

      implicit none

      integer n,i
      double precision a(n), b(n)

      do i = 1, n
         a(i) = b(i)
      end do

      return
      end

!-----------------------------------------------------------------
      subroutine adds2m1(a,b,c1,n)
!-----------------------------------------------------------------
!     a=b*c1
!-----------------------------------------------------------------
      implicit none

      integer n,i
      double precision a(n),b(n),c1
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,n
        a(i)=a(i)+c1*b(i)
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      subroutine adds1m1(a,b,c1,n )
!-----------------------------------------------------------------
!     a=c1*a+b
!-----------------------------------------------------------------

      implicit none

      integer n,i
      double precision a(n),b(n),c1
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,n
        a(i)=c1*a(i)+b(i)
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      subroutine col2(a,b,n)
!------------------------------------------------------------------
!     a=a*b
!------------------------------------------------------------------

      implicit none

      integer n,i
      double precision a(n),b(n)

!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,n
        a(i)=a(i)*b(i)
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      subroutine nrzero (na,n)
!------------------------------------------------------------------
!     zero out array of integers 
!------------------------------------------------------------------

      implicit none

      integer n,i,na(n)

      do i = 1, n
        na(i ) = 0
      end do

      return
      end

!-----------------------------------------------------------------
      subroutine add2(a,b,n)
!------------------------------------------------------------------
!     a=a+b
!------------------------------------------------------------------

      implicit none

      integer n,i
      double precision  a(n),b(n)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i)
      do i=1,n
        a(i)=a(i)+b(i)
      end do
!$OMP END PARALLEL DO

      return
      end

!-----------------------------------------------------------------
      double precision function calc_norm()
!------------------------------------------------------------------
!     calculate the integral of ta1 over the whole domain
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision total,ieltotal
      integer iel,k,j,i,isize

      total=0.d0
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(i,j,k,isize,ieltotal,iel)  &
!$OMP& REDUCTION(+:total)

      do iel=1,nelt
        ieltotal=0.d0
        isize=size_e(iel)
        do k=1,lx1
          do j=1,lx1
            do i=1,lx1
              ieltotal=ieltotal+ta1(i,j,k,iel)*w3m1(i,j,k)  &
     &                               *jacm1_s(i,j,k,isize)
            end do
          end do
        end do
      total=total+ieltotal
      end do
!$OMP END PARALLEL DO

      calc_norm = total

      return
      end
!-----------------------------------------------------------------
      subroutine parallel_add(frontier)
!-----------------------------------------------------------------
!     input array frontier, perform (potentially) parallel add so that
!     the output frontier(i) has sum of frontier(1)+frontier(2)+...+frontier(i)
!-----------------------------------------------------------------

      use ua_data
      implicit none

      integer nellog,i,ahead,ii,ntemp,n1,ntemp1,frontier(lelt),iel

      nellog=0
      iel=1
   10 iel=iel*2
      nellog=nellog+1
      if (iel.lt.nelt) goto 10

      ntemp=1
      do i=1,nellog
        n1=ntemp*2
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ahead,ii,iel)
        do iel=n1, nelt,n1
          ahead=frontier(iel-ntemp)
          do ii=ntemp-1,0,-1
            frontier(iel-ii)=frontier(iel-ii)+ahead
          end do
        end do
!$OMP END PARALLEL DO

        iel=(nelt/n1+1)*n1
        ntemp1=iel-nelt
        if(ntemp1.lt.ntemp)then
          ahead=frontier(iel-ntemp)
!$OMP PARALLEL DO DEFAULT(SHARED) PRIVATE(ii)
          do ii=ntemp-1,ntemp1,-1
            frontier(iel-ii)=frontier(iel-ii)+ahead
          end do
!$OMP END PARALLEL DO
        end if

        ntemp=n1
      end do

      return
      end 

!------------------------------------------------------------------
      subroutine dssum

!------------------------------------------------------------------
!     Perform stiffness summation: element-mortar-element mapping
!------------------------------------------------------------------

      use ua_data
      implicit none

      call transfb(dpcmor,dpcelm)
      call transf (dpcmor,dpcelm)

      return
      end

!------------------------------------------------------------------
      subroutine facev(a,iface,val)
!------------------------------------------------------------------
!     assign the value val to face(iface,iel) of array a.
!------------------------------------------------------------------

      use ua_data
      implicit none

      double precision a(lx1,lx1,lx1), val
      integer iface, kx1, kx2, ky1, ky2, kz1, kz2, ix, iy, iz

      kx1=1
      ky1=1
      kz1=1
      kx2=lx1
      ky2=lx1
      kz2=lx1
      if (iface.eq.1) kx1=lx1
      if (iface.eq.2) kx2=1
      if (iface.eq.3) ky1=lx1
      if (iface.eq.4) ky2=1
      if (iface.eq.5) kz1=lx1
      if (iface.eq.6) kz2=1

      do ix = kx1, kx2
        do iy = ky1, ky2
          do iz = kz1, kz2
            a(ix,iy,iz)=val
          end do
        end do
      end do

      return
      end


