!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_base_module_sjpack
!
!  spml/w_base_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��Υ⥸�塼�� 
!  w_module_sjpack �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻��δ���Ū�� 
!  Fortran90 �ؿ����󶡤���.
!
!  ������ ISPACK �� LJPACK(SJPACK) �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�
!  ˡ�ˤĤ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!== ����
!
!      2009/09/03  �ݹ�����  w_base_module ����¤
!      2009/09/20  �ݹ�����  w_base_initialize �ѿ�Ƴ��
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!         ���ȿ����Ǥλ����ϻ����ȿ����Ǥ˷�ᤦ��. 
!
!++
module w_base_module_sjpack
  !
  != w_base_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_base_module_sjpack.f90,v 1.3 2009-09-23 06:35:59 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����.
  !
  ! spml/w_base_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
  ! �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  ! ����Ū�� Fortran90 �ؿ����󶡤���.
  !
  ! ������ ISPACK �� SJPACK Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ���
  ! �ܤ����׻�ˡ�ˤĤ��Ƥ� ISPACK/SJPACK �Υޥ˥奢���
  ! ���Ȥ��줿��.
  !
  use dc_message
  implicit none

  integer               :: im=64            ! �ʻ���������(����)
  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: nm=21            ! �׻������������ȿ�������
  integer               :: nn=22            ! �����ȿ�(���ȿ�)������
  integer               :: mm=21            ! �����ȿ�(�����ȿ�)������
  integer               :: np=1             ! OPENMP ���祹��åɿ�

  logical               :: openmp=.false.   ! OPENMP �����å�

  real(8), allocatable  :: p(:,:), r(:)     ! �Ѵ�������
  integer               :: it(4)            ! �Ѵ�������
  real(8), allocatable  :: t(:)             ! �Ѵ�������

  real(8), allocatable  :: c(:)             ! �������

  real(8), allocatable  :: x_Lon(:), y_Lat(:)                ! ���ٷ���
  real(8), allocatable  :: x_Lon_Weight(:), y_Lat_Weight(:)  ! ��ɸ�Ť�
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  logical               :: w_base_initialize=.false.   ! ������ե�å�

  real(8), parameter    :: pi=3.1415926535897932385D0

  private

  public im, jm, nn, mm, nm                   ! �ʻ�����, �����ȿ�
  public it, t, p, r                          ! �Ѵ��Ѻ������
  public openmp, np                           ! OPENMP ���ѿ�

  public w_base_Initial                       ! ��������֥롼����
  public x_Lon, y_Lat                         ! �ʻҺ�ɸ
  public x_Lon_Weight, y_Lat_Weight           ! �ʻҺ�ɸ�Ť�
  public xy_Lon, xy_Lat                       ! �ʻҺ�ɸ(im,jm)
  public l_nm, nm_l                           ! �ȿ���Ǽ����
  public xy_w, w_xy                           ! �Ѵ��ؿ�

  interface l_nm
     module procedure l_nm_array00
     module procedure l_nm_array01
     module procedure l_nm_array10
     module procedure l_nm_array11
  end interface

  interface nm_l
     module procedure nm_l_int
     module procedure nm_l_array
  end interface

  save im, jm, nm, mm, nn                     ! �ʻ�����, �����ȿ��򵭲�
  save it, t, p, r                            ! �Ѵ�������򵭲�
  save c                                      ! �Ѵ���������礭��
  save openmp, np                             ! �Ѵ���������礭��
  save w_base_initialize                      ! ������ե饰

  contains
  !--------------- ����� -----------------
    subroutine w_base_Initial(n_in,i_in,j_in,np_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ OPENMP ���ѻ���
      ! ���祹��åɿ������ꤹ��.
      !
      ! �ºݤλ��ѤˤϾ�̥��֥롼���� w_Initial ���Ѥ��뤳��.
      !
      integer,intent(in) :: i_in              !(in) �ʻ�����(����), 2�ζҾ�(<=2048)
      integer,intent(in) :: j_in              !(in) �ʻ�����(����), 4 ���ܿ�
      integer,intent(in) :: n_in              !(in) �������ȿ�
      integer,intent(in), optional :: np_in   !(in) OPENMP �Ǥκ��祹��åɿ�

      integer :: iw, i, j

      w_base_initialize = .true.

      im = i_in   ; jm = j_in
      nn = n_in   ; nm = n_in+1 ;  mm = n_in      ! default �ϻ����ȿ�����

      if ( present(np_in) )then
         np = np_in

         if ( np .gt. 1 ) then
            openmp = .true. 
            call MessageNotify('M','w_base_Initial', &
                 'OpenMP computation was set up.')
         else
            openmp = .false. 
         endif

      else
         openmp = .false. 
         np = 1
      endif

      allocate(p(jm/2,mm+4))                  ! �Ѵ�������
      allocate(r((mm+1)*(2*nm-mm-1)+1))       ! �Ѵ�������
      allocate(t(im*6))                       ! �Ѵ�������

      allocate(c((mm+1)*(mm+1)))              ! �Ѵ��Ѻ������

      allocate(x_Lon(0:im-1))                 ! �ʻ�����ɸ��Ǽ����(����)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(y_Lat(1:jm))
      allocate(y_Lat_Weight(1:jm))             ! �ʻ�����ɸ��Ǽ����
      allocate(xy_Lat(0:im-1,1:jm))        ! �ʻ�����ɸ��Ǽ����

      call sjinit(mm,nm,jm,im,p,r,it,t)

      call sjinic(mm,c)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! ���ٺ�ɸ
         x_Lon_Weight(i) = 2*pi/im           ! ���ٺ�ɸ�Ť�
      enddo


      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(p(j,1))        ! ���ٺ�ɸ
         y_Lat(jm/2-j+1) = -asin(p(j,1))        ! ���ٺ�ɸ
         y_Lat_Weight(jm/2+j)   = 2*p(j,2)      ! ���ٽŤ�(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*p(j,2)      ! ���ٽŤ�(Gauss grid)
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      call MessageNotify('M','w_base_initial',&
           'w_base_module_sjpack (2009/09/04) is initialized')

    end subroutine w_base_Initial

  !--------------- �����Ѵ� -----------------

    function l_nm_array00(n,m)
      !
      ! ���ȿ�(n)�������ȿ�(m)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
      ! 
      ! ���� n,m ���Ȥ�������ͤξ��, �����ͤ��֤�. 
      !
      integer               :: l_nm_array00   
      !(out) ���ڥ��ȥ�ǡ����γ�Ǽ���� 

      integer, intent(in)   :: n     !(in) ���ȿ�
      integer, intent(in)   :: m     !(in) �Ӿ��ȿ�           

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','l_nm_array00',&
              'w_base_module not initialize yet. Use sjnm2l routine in ISPACK directly.')
      else
         call sjnm2l(nn,n,m,l_nm_array00)
      endif

    end function l_nm_array00

    function l_nm_array01(n,marray)           ! ���ڥ��ȥ�ǡ����γ�Ǽ���� 
      !
      ! ���ȿ�(n)�������ȿ�(m)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
      ! 
      ! �� 1 ���� n ������, �� 2 ���� marray ������ 1 ��������ξ��, 
      ! marray ��Ʊ���礭���� 1 ��������������֤�. 
      !
      integer, intent(in)  :: n               !(in) ���ȿ�
      integer, intent(in)  :: marray(:)       !(in) �Ӿ��ȿ�
      integer              :: l_nm_array01(size(marray))
      !(out) ���ڥ��ȥ�ǡ�������

      integer              :: i 

      do i=1, size(marray)
         l_nm_array01(i) = l_nm_array00(n,marray(i))
      enddo
    end function l_nm_array01

    function l_nm_array10(narray,m)
      !
      ! ���ȿ�(n)�������ȿ�(m)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
      ! 
      ! �� 1 ���� narray ������ 1 ��������, �� 2 ����  m �������ξ��, 
      ! narray ��Ʊ���礭���� 1 ��������������֤�. 
      !
      integer, intent(in)  :: narray(:)           !(in) ���ȿ�  
      integer, intent(in)  :: m                   !(in) �Ӿ��ȿ�
      integer              :: l_nm_array10(size(narray))
      !(out) ���ڥ��ȥ�ǡ�������

      integer              :: i 

      do i=1, size(narray)
         l_nm_array10(i) = l_nm_array00(narray(i),m)
      enddo
    end function l_nm_array10

    function l_nm_array11(narray,marray)
      !
      ! ���ȿ�(n)�������ȿ�(m)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
      ! 
      ! �� 1,2 ���� narray, marray ���Ȥ������ 1 ��������ξ��, 
      ! narray, marray ��Ʊ���礭���� 1 ��������������֤�. 
      ! narray, marray ��Ʊ���礭���Ǥʤ���Фʤ�ʤ�. 
      !
      integer, intent(in)  :: narray(:)          !(in) ���ȿ�  
      integer, intent(in)  :: marray(:)          !(in) �Ӿ��ȿ�
      integer              :: l_nm_array11(size(narray))
      !(out) ���ڥ��ȥ�ǡ�������

      integer              :: i 

      if ( size(narray) .ne. size(marray) ) then
         call MessageNotify('E','l_nm_array11',&
              'dimensions of input arrays  n and m are different.')
      endif

      do i=1, size(narray)
         l_nm_array11(i) = l_nm_array00(narray(i),marray(i))
      enddo
    end function l_nm_array11

    function nm_l_int(l)
      ! 
      ! ���ڥ��ȥ�ǡ����γ�Ǽ����(l)�������ȿ�(n)�������ȿ�(m)���֤�.
      !
      ! ���� l �������ͤξ��, �б��������ȿ����Ӿ��ȿ���
      ! Ĺ�� 2 �� 1 ���������ͤ��֤�. 
      ! nm_l(1) �����ȿ�, nm_l(2) ���Ӿ��ȿ��Ǥ���. 
      !
      integer               :: nm_l_int(2)  !(out) ���ȿ�, �Ӿ��ȿ�
      integer, intent(in)   :: l            !(in) ���ڥ��ȥ�ǡ����γ�Ǽ����
      
      if ( .not. w_base_initialize ) then
         call MessageNotify('E','nm_l_int',&
              'w_base_module not initialize yet. Use sjl2nm routine in ISPACK directly.')
      else
         call sjl2nm(nn,l,nm_l_int(1),nm_l_int(2))
      endif

    end function nm_l_int

    function nm_l_array(larray)
      ! 
      ! ���ڥ��ȥ�ǡ����γ�Ǽ����(l)�������ȿ�(n)�������ȿ�(m)���֤�.
      !
      ! ���� larray ������ 1 ��������ξ��, 
      ! larray ���б����� n, m ���Ǽ���� 2 ��������������֤�. 
      ! nm_l_array(:,1) �����ȿ�, nm_l_array(:,2) ���Ӿ��ȿ��Ǥ���. 
      !
      integer, intent(in)  :: larray(:)
      !(out) ���ȿ�, �Ӿ��ȿ�

      integer              :: nm_l_array(size(larray),2)
      !(in) ���ڥ��ȥ�ǡ����γ�Ǽ����

      integer              :: i

      do i=1, size(larray)
         nm_l_array(i,:) = nm_l_int(larray(i))
      enddo
    end function nm_l_array

    function xy_w(w_data,ipow,iflag)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(1 ����).
      !
      real(8)               :: xy_w(0:im-1,1:jm)
      !(out) �ʻ����ǡ���

      real(8), intent(in)   :: w_data((mm+1)*(mm+1))
      !(in) ���ڥ��ȥ�ǡ���

      integer, intent(in), optional  :: ipow      
      !(in) ���Ѥ����� 1/cos�� �μ���. ��ά���� 0. 

      integer, intent(in), optional  :: iflag
      !(in) �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    1 : ������ʬ cos�ա���/�ߦ� ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !    ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval

      real(8)             :: w_Rdata((2*nn+1-mm)*mm+nn+1)
      ! ����ѥ��ڥ��ȥ�ǡ���(SJTS2G ������)
      real(8)             :: w_Xdata((mm+1)*(mm+1))
      ! ����ѥ��ڥ��ȥ�ǡ���(SJCS2X ������)
      real(8)             :: w_Ydata((mm+4)*mm+2)
      ! ����ѥ��ڥ��ȥ�ǡ���(SJCS2Y ������)

      real(8)  :: q(jm/2*7*np)               ! �Ѵ��Ѻ������
      real(8)  :: ws(2*(nn+1)*np)            ! �Ѵ��Ѻ������
      real(8)  :: wg((im+2)*jm)              ! �Ѵ��Ѻ������
      real(8)  :: w((jm+1)*im)               ! �Ѵ��Ѻ������

      logical :: first=.true.                    ! ���Ƚ�ꥹ���å�
      save first

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','xy_w',&
              'w_base_module not initialize yet.')
      endif

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      if ( openmp .and. first ) then
         call MessageNotify('M','xy_w', &
              'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
      endif

      if ( ifval==0 ) then
         call sjcrup(mm,nn,w_data,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==-1 ) then
         call sjcs2x(mm,w_data,w_Xdata)
         call sjcrup(mm,nn,w_Xdata,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==1 ) then
         call sjcs2y(mm,w_data,w_Ydata,c)
         if ( openmp ) then
            call sjtsog(mm,nm,nm,im,jm,w_Ydata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nm,im,jm,w_Ydata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==2 ) then
         call sjcrup(mm,nn,w_data,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         xy_w = xy_w * sin(xy_Lat)
      else
         call MessageNotify('E','xy_w','invalid value of iflag')
      endif

      first = .false.

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(1 ����).
      !
      real(8)               :: w_xy((mm+1)*(mm+1))
      !(out) ���ڥ��ȥ�ǡ���

      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) �ʻ����ǡ���

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ� 
      !    1 : ������ʬ 1/cos�ա���(f cos^2��)/�ߦ� ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !  ��ά���� 0.


      integer, parameter  :: ipow_default  = 0    ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0    ! �����å��ǥե������

      integer ipval, ifval, i, j

      real(8)             :: w_Rdata((2*nn+1-mm)*mm+nn+1)
      ! ����ѥ��ڥ��ȥ�ǡ���(SJTS2G ������)
      real(8)             :: w_Xdata((mm+1)*(mm+1))
      ! ����ѥ��ڥ��ȥ�ǡ���(SJCS2X ������)
      real(8)             :: w_Ydata((mm+4)*nm+2)
      ! ����ѥ��ڥ��ȥ�ǡ���(SJCY2S ������)

      real(8)  :: q(jm/2*7*np)               ! �Ѵ��Ѻ������
      real(8)  :: ws(2*(nn+1)*np)            ! �Ѵ��Ѻ������
      real(8)  :: wg((im+2)*jm)              ! �Ѵ��Ѻ������
      real(8)  :: w((jm+1)*im)               ! �Ѵ��Ѻ������

      logical :: first=.true.                     ! ���Ƚ�ꥹ���å�
      save first

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','xy_w',&
              'w_base_module not initialize yet.')
      endif

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif
      
      if ( openmp .and. first ) then
         call MessageNotify('M','w_xy', &
              'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
      endif

      if ( ifval == 0 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_xy)
      else if ( ifval == -1 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_Xdata)
         call sjcs2x(mm,w_Xdata,w_xy)
      else if ( ifval == 1 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nm,im,jm,w_Ydata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nm,im,jm,w_Ydata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcy2s(mm,w_Ydata,w_xy,c)
      else if ( ifval == 2 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data*sin(xy_Lat),&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data*sin(xy_Lat),&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_xy)
      end if

      first = .false.

    end function w_xy

  end module w_base_module_sjpack
