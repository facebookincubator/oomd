!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  ua_data module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module ua_data

      include 'npbparams.h'

!.....Array dimensions     
      integer lx1, lnje, nsides, nxyz
      parameter(lx1=5, lnje=2, nsides=6,  nxyz=lx1*lx1*lx1)

      integer fre, niter, nmxh
      double precision alpha, dlmin, dtime

      integer nelt, ntot, nmor, nvertex

      double precision x0, y0, z0, time

      double precision velx, vely, velz, visc, x00, y00, z00
      parameter(velx=3.d0, vely=3.d0, velz=3.d0)
      parameter(visc=0.005d0)
      parameter(x00=3.d0/7.d0, y00=2.d0/7.d0, z00=2.d0/7.d0)

!.....double precision arrays associated with collocation points
      double precision, allocatable ::  &
     &       ta1  (:,:,:,:), ta2   (:,:,:,:),  &
     &       trhs (:,:,:,:), t     (:,:,:,:),  &
     &       tmult(:,:,:,:), dpcelm(:,:,:,:),  &
     &       pdiff(:,:,:,:), pdiffp(:,:,:,:)

!.....double precision arays associated with mortar points
      double precision, allocatable ::  &
     &       umor(:), tmmor (:),  &
     &       rmor(:), dpcmor(:), pmorx(:), ppmor(:) 
      double precision, allocatable, target ::  &
     &       mormult(:), tmort(:)

!.... integer arrays associated with element faces
      integer, allocatable ::  &
     &        idmo    (:,:,:,:,:,:),  &
     &        idel    (:,:,    :,:),  &
     &        sje     (:,:,    :,:),  &
     &        sje_new (:,:,    :,:),  &
     &        ijel    (:,      :,:),  &
     &        ijel_new(:,      :,:),  &
     &        cbc     (        :,:),  &
     &        cbc_new (        :,:) 

!.....integer array associated with vertices
      integer, allocatable :: vassign (:,:), emo(:,:,:),   &
     &        nemo (:)

!.....integer array associated with element edges
      integer, allocatable :: diagn  (:,:,:) 

!.... integer arrays associated with elements
      integer, allocatable ::  &
     &        tree (:), mt_to_id    (:),                   &
     &        newc (:), mt_to_id_old(:),  &
     &        newi (:), id_to_mt    (:),  &
     &        newe (:), ref_front_id(:),  &
     &        front(:), action      (:),  &
     &        ich  (:), size_e      (:),  &
     &        treenew(:)

!.....logical arrays associated with vertices
      logical, allocatable :: ifpcmor (:)

!.....logical arrays associated with edge
      logical, allocatable ::  &
     &        eassign  (:,:), if_1_edge(:,:),  &
     &        ncon_edge(:,:)

!.....logical arrays associated with elements
      logical, allocatable :: skip(:), ifcoa(:), ifcoa_id(:)

!.....logical arrays associated with element faces
      logical, allocatable :: fassign(:,:), edgevis(:,:,:)      

!.....small arrays
      double precision qbnew(lx1-2,lx1,2), bqnew(lx1-2,lx1-2,2)

      double precision  &
     &       pcmor_nc1(lx1,lx1,2,2,refine_max),  &
     &       pcmor_nc2(lx1,lx1,2,2,refine_max),  &
     &       pcmor_nc0(lx1,lx1,2,2,refine_max),  &
     &       pcmor_c(lx1,lx1,refine_max), tcpre(lx1,lx1),  &
     &       pcmor_cor(8,refine_max)

!.....gauss-labotto and gauss points
      double precision zgm1(lx1)

!.....weights
      double precision wxm1(lx1),w3m1(lx1,lx1,lx1)

!.....coordinate of element vertices
      double precision, allocatable ::  &
     &       xc(:,:),    yc(:,:),    zc(:,:),  &
     &       xc_new(:,:),yc_new(:,:),zc_new(:,:)

!.....dr/dx, dx/dr  and Jacobian
      double precision jacm1_s(lx1,lx1,lx1,refine_max),  &
     &       rxm1_s(lx1,lx1,lx1,refine_max),  &
     &       xrm1_s(lx1,lx1,lx1,refine_max)

!.....mass matrices (diagonal)
      double precision bm1_s(lx1,lx1,lx1,refine_max)

!.....dertivative matrices d/dr
      double precision dxm1(lx1,lx1), dxtm1(lx1,lx1), wdtdr(lx1,lx1)

!.....interpolation operators
      double precision  &
     &       ixm31(lx1,lx1*2-1), ixtm31(lx1*2-1,lx1), ixmc1(lx1,lx1),  &
     &       ixtmc1(lx1,lx1), ixmc2(lx1,lx1),  ixtmc2(lx1,lx1),  &
     &       map2(lx1),map4(lx1)

!.....collocation location within an element
      double precision xfrac(lx1)

!.....used in laplacian operator
      double precision g1m1_s(lx1,lx1,lx1,refine_max),  &
     &       g4m1_s(lx1,lx1,lx1,refine_max),  &
     &       g5m1_s(lx1,lx1,lx1,refine_max),  &
     &       g6m1_s(lx1,lx1,lx1,refine_max)
      
!.....We store some tables of useful topological constants
!     These constants are intialized as a block data below
      integer f_e_ef(4,6)
      integer e_c(3,8)
      integer local_corner(8,6)
      integer cal_nnb(3,8)
      integer oplc(4)
      integer cal_iijj(2,4)
      integer cal_intempx(4,6)
      integer c_f(4,6)
      integer le_arr(4,0:1,3)
      integer jjface(6)
      integer e_face2(4,6)
      integer op(4)
      integer localedgenumber(6,12)
      integer edgenumber(4,6)
      integer f_c(3,8)
      integer e1v1(6,6),e2v1(6,6),e1v2(6,6),e2v2(6,6)
      integer children(4,6)
      integer iijj(2,4)
      integer v_end(2)
      integer face_l1(3),face_l2(3),face_ld(3)

! ... Timer parameters
      integer t_total,t_init,t_convect,t_transfb_c,  &
     &        t_diffusion,t_transf,t_transfb,t_adaptation,  &
     &        t_transf2,t_add2,t_last
      parameter (t_total=1,t_init=2,t_convect=3,t_transfb_c=4,  &
     &        t_diffusion=5,t_transf=6,t_transfb=7,t_adaptation=8,  &
     &        t_transf2=9,t_add2=10,t_last=10)
      logical timeron

!.....Locks used for atomic updates
!c    integer (kind=omp_lock_kind) tlock(lmor)
!$    integer(8) tlock(lmor)


!------------------------------------------------------------------
!.....We store some tables of useful topological constants
!------------------------------------------------------------------

!     f_e_ef(e,f) returns the other face sharing the e'th local edge of face f.
      data f_e_ef/6,3,5,4, 6,3,5,4, 6,1,5,2, 6,1,5,2, 4,1,3,2, 4,1,3,2/

!.....e_c(n,j) returns n'th edge sharing the vertex j of an element
      data e_c /5,8,11, 1,4,11,  5,6,9, 1,2,9,  &
     &          7,8,12, 3,4,12, 6,7,10, 2,3,10/

!.....local_corner(n,i) returns the local corner index of vertex n on face i
      data local_corner /0,1,0,2,0,3,0,4, 1,0,2,0,3,0,4,0,  &
     &                   0,0,1,2,0,0,3,4, 1,2,0,0,3,4,0,0,  &
     &                   0,0,0,0,1,2,3,4, 1,2,3,4,0,0,0,0/

!.....cal_nnb(n,i) returns the neighbor elements neighbored by n'th edge
!     among the three edges sharing vertex i
!     the elements are the eight children elements ordered as 1 to 8.
      data cal_nnb/5,2,3, 6,1,4, 7,4,1, 8,3,2,  &
     &             1,6,7, 2,5,8, 3,8,5, 4,7,6/

!.....returns the opposite local corner index: 1-4,2-3
      data oplc /4,3,2,1/

!.....cal_iijj(i,n) returns the location of local corner number n on a face 
!     i =1  to get ii, i=2 to get jj
!     (ii,jj) is defined the same as in mortar location (ii,jj)
      data cal_iijj /1,1, 1,2, 2,1, 2,2/

!.....returns the adjacent(neighbored by a face) element's children,
!     assumming a vertex is shared by eight child elements 1-8. 
!     index n is local corner number on the face which is being 
!     assigned the mortar index number
      data cal_intempx /8,6,4,2, 7,5,3,1, 8,7,4,3,  &
     &                  6,5,2,1, 8,7,6,5, 4,3,2,1/

!.....c_f(i,f) returns the vertex number of i'th local corner on face f
      data c_f /2,4,6,8, 1,3,5,7, 3,4,7,8, 1,2,5,6, 5,6,7,8, 1,2,3,4/

!.....on each face of the parent element, there are four children element.
!     le_arr(i,j,n) returns the i'th elements among the four children elements 
!     n refers to the direction: 1 for x, 2 for y and 3 for z direction. 
!     j refers to positive(0) or negative(1) direction on x, y or z direction.
!     n=1,j=0 refers to face 1 and n=1, j=1 refers to face 2, n=2,j=0 refers to
!     face 3.... 
!     The current eight children are ordered as 8,1,2,3,4,5,6,7 
      data    le_arr/8,2,4,6, 1,3,5,7,  &
     &               8,1,4,5, 2,3,6,7,  &
     &               8,1,2,3, 4,5,6,7/

!.....jjface(n) returns the face opposite to face n
      data jjface /2,1,4,3,6,5/

!c.....edgeface(n,f) returns OTHER face which shares local edge n on face f
!      integer edgeface(4,6)
!      data edgeface /6,3,5,4, 6,3,5,4, 6,1,5,2, 
!     $               6,1,5,2, 4,1,3,2, 4,1,3,2/

!.....e_face2(n,f) returns the local edge number of edge n on the
!     other face sharing local edge n on face f
      data e_face2 /2,2,2,2, 4,4,4,4, 3,2,3,2,  &
     &              1,4,1,4, 3,3,3,3, 1,1,1,1/

!.....op(n) returns the local edge number of the edge which 
!     is opposite to local edge n on the same face
      data op /3,4,1,2/

!.....localedgenumber(f,e) returns the local edge number for edge e
!     on face f. A zero result value signifies illegal input
      data localedgenumber /1,0,0,0,0,2, 2,0,2,0,0,0, 3,0,0,0,2,0,  &
     &                      4,0,0,2,0,0, 0,1,0,0,0,4, 0,2,4,0,0,0,  &
     &                      0,3,0,0,4,0, 0,4,0,4,0,0, 0,0,1,0,0,3,  &
     &                      0,0,3,0,3,0, 0,0,0,1,0,1, 0,0,0,3,1,0/

!.....edgenumber(e,f) returns the edge index of local edge e on face f
      data edgenumber / 1,2, 3,4,  5,6, 7,8,  9,2,10,6,  &
     &                 11,4,12,8, 12,3,10,7, 11,1, 9,5/

!.....f_c(c,n) returns the face index of i'th face sharing vertex n 
      data f_c /2,4,6, 1,4,6, 2,3,6, 1,3,6,  &
     &          2,4,5, 1,4,5, 2,3,5, 1,3,5/

!.....if two elements are neighbor by one edge, 
!     e1v1(f1,f2) returns the smaller index of the two vertices on this 
!     edge on one element
!     e1v2 returns the larger index of the two vertices of this edge on 
!     on element. exfor a vertex on element 
!     e2v1 returns the smaller index of the two vertices on this edge on 
!     another element
!     e2v2 returns the larger index of the two vertiex on this edge on
!     another element
      data e1v1/0,0,4,2,6,2, 0,0,3,1,5,1, 4,3,0,0,7,3,  &
     &          2,1,0,0,5,1, 6,5,7,5,0,0, 2,1,3,1,0,0/
      data e2v1/0,0,1,3,1,5, 0,0,2,4,2,6, 1,2,0,0,1,5,  &
     &          3,4,0,0,3,7, 1,2,1,3,0,0, 5,6,5,7,0,0/
      data e1v2/0,0,8,6,8,4, 0,0,7,5,7,3, 8,7,0,0,8,4,  &
     &          6,5,0,0,6,2, 8,7,8,6,0,0, 4,3,4,2,0,0/
      data e2v2/0,0,5,7,3,7, 0,0,6,8,4,8, 5,6,0,0,2,6,  &
     &          7,8,0,0,4,8, 3,4,2,4,0,0, 7,8,6,8,0,0/

!.....children(n1,n)returns the four elements among the eight children 
!     elements to be merged on face n of the parent element
!     the IDs for the eight children are 1,2,3,4,5,6,7,8
      data children/2,4,6,8, 1,3,5,7, 3,4,7,8,  &
     &              1,2,5,6, 5,6,7,8, 1,2,3,4/

!.....iijj(n1,n) returns the location of n's mortar on an element face
!     n1=1 refers to x direction location and n1=2 refers to y direction
      data iijj/1,1,1,2,2,1,2,2/

!.....v_end(n) returns the index of collocation points at two ends of each
!     direction
      data v_end /1,lx1/

!.....face_l1,face_l2,face_ld return for start,end,stride for a loop over faces 
!     used on subroutine  mortar_vertex
      data face_l1 /2,3,1/, face_l2 /3,1,2/, face_ld /1,-2,1/


      end module ua_data


!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_space

!---------------------------------------------------------------------
!---------------------------------------------------------------------

!---------------------------------------------------------------------
! allocate space dynamically for data arrays
!---------------------------------------------------------------------

      use ua_data
      implicit none

      integer ios


      allocate (  &
     &        ta1  (lx1,lx1,lx1,lelt), ta2   (lx1,lx1,lx1,lelt),  &
     &        trhs (lx1,lx1,lx1,lelt), t     (lx1,lx1,lx1,lelt),  &
     &        tmult(lx1,lx1,lx1,lelt), dpcelm(lx1,lx1,lx1,lelt),  &
     &        pdiff(lx1,lx1,lx1,lelt), pdiffp(lx1,lx1,lx1,lelt),  &
     &        stat = ios)

      if (ios .eq. 0) allocate (  &
     &        umor(lmor), tmmor(lmor),  &
     &        rmor(lmor), dpcmor (lmor), pmorx(lmor), ppmor(lmor),  &
     &        mormult(lmor), tmort(lmor),  &
     &        stat = ios)

      if (ios .eq. 0) allocate (  &
     &        idmo    (lx1,lx1,lnje,lnje,nsides,lelt),  &
     &        idel    (lx1,lx1,          nsides,lelt),  &
     &        sje     (2,2,              nsides,lelt),  &
     &        sje_new (2,2,              nsides,lelt),  &
     &        ijel    (2,                nsides,lelt),  &
     &        ijel_new(2,                nsides,lelt),  &
     &        cbc     (                  nsides,lelt),  &
     &        cbc_new (                  nsides,lelt),  &
     &        vassign (8,lelt),       emo(2,8,8*lelt),   &
     &        nemo    (8*lelt),  &
     &        diagn   (2,12,lelt),  &
     &        stat = ios)

      if (ios .eq. 0) allocate (  &
     &        tree   (lelt), mt_to_id    (lelt),                   &
     &        newc   (lelt), mt_to_id_old(lelt),  &
     &        newi   (lelt), id_to_mt    (lelt),  &
     &        newe   (lelt), ref_front_id(lelt),  &
     &        front  (lelt), action      (lelt),  &
     &        ich    (lelt), size_e      (lelt),  &
     &        treenew(lelt),  &
     &        stat = ios)

      if (ios .eq. 0) allocate (  &
     &        ifpcmor  (8* lelt),  &
     &        eassign  (12,lelt),  if_1_edge(12,lelt),  &
     &        ncon_edge(12,lelt),  &
     &        skip (lelt), ifcoa (lelt), ifcoa_id(lelt),  &
     &        fassign(nsides,lelt), edgevis(4,nsides,lelt),    &
     &        stat = ios)

!.....coordinate of element vertices
      if (ios .eq. 0) allocate (  &
     &        xc    (8,lelt),yc    (8,lelt),zc    (8,lelt),  &
     &        xc_new(8,lelt),yc_new(8,lelt),zc_new(8,lelt),  &
     &        stat = ios)

      if (ios .ne. 0) then
         write(*,*) 'Error encountered in allocating space'
         stop
      endif

      return
      end
