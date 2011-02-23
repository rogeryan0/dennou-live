!--
!----------------------------------------------------------------------
!     Copyright (c) 2002--2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_base_module
!
!  spml/w_base_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư�����Ĵ��ȡ
!  �����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��Υ⥸�塼�� w_module
!  �β����⥸�塼��Ǥ���, ���ڥ��ȥ�׻��δ���Ū�� Fortran90 �ؿ�����
!  ������.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ��
!  ����. ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�
!  ˡ�ˤĤ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!== ����
!
!      2001/12/08  �ݹ�����
!      2001/12/26  �ݹ�����  �ؿ�,�ѿ�̾���ѹ�
!      2002/02/07  �ݹ�����  �ؿ�,�ѿ�̾�����ѹ�
!      2002/03/30  �ݹ�����  �ؿ�,�ѿ�̾���ƺ��ѹ�
!      2002/05/25  �ݹ�����  �ʻ�����ɸ����̤��٤�̿̾ˡ�ѹ�
!      2005/03/13  �ݹ�����  l_nm, nm_l ������ǰ������Ϥ���褦�˳�ĥ
!      2005/07/04  �ݹ�����  OpenMP ���Ѵ��롼������б�
!                            �Х󥯶�����򤱤뤿��κ�������ɲ�
!      2005/07/10  �ݹ�����  OpenMP ���åȥ��åפΥ�å���������
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2008/02/23  ��������ʿ �ʻ����ǡ����������(im,jm) ���� (0:im-1, 0:jm-1)
!                             ���ѹ�.
!      2008/06/25  ��������ʿ �ʻ����ǡ����������(0:im-1,1:jm) ���ѹ�
!      2008/07/04  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/12/28  �ݹ�����   xy_w, w_xy �Υ����Ȥ��ɲ�
!      2009/01/09  �ݹ�����   w_base_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/30  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module w_base_module
  !
  != w_base_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_base_module.f90,v 1.16 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����.
  !
  ! spml/w_base_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿���
  ! �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  ! ����Ū�� Fortran90 �ؿ����󶡤���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼����
  ! ��Ƥ�Ǥ���. ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ
  ! ���Ѵ��ξܤ����׻�ˡ�ˤĤ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ�
  ! �奢��򻲾Ȥ��줿��.
  !
  use dc_message
  implicit none

  integer               :: im=64            ! �ʻ���������(����)
  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: nm=21            ! �����ȿ�������
  integer               :: np=1             ! OPENMP ���祹��åɿ�

  logical               :: openmp=.false.   ! OPENMP �����å�

  integer               :: it(6)            ! �Ѵ�������
  real(8), allocatable  :: t(:)             ! �Ѵ�������
  integer, allocatable  :: ip(:)            ! �Ѵ�������
  real(8), allocatable  :: p(:), r(:)       ! �Ѵ�������
  integer, allocatable  :: ia(:)            ! �Ѵ�������
  real(8), allocatable  :: a(:)             ! �Ѵ�������
  real(8), allocatable  :: y(:,:)           ! �Ѵ�������

  real(8), allocatable  :: q(:)             ! �������
  real(8), allocatable  :: ww(:), ws(:)     ! �������
  real(8), allocatable  :: wv(:)            ! �������(OPENMP��)

  real(8), allocatable  :: x_Lon(:), y_Lat(:)                ! ���ٷ���
  real(8), allocatable  :: x_Lon_Weight(:), y_Lat_Weight(:)  ! ��ɸ�Ť�
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  integer               :: id=65, jd=33     ! xy_work ���礭��
  integer               :: iw               ! ww,ws ���礭��

  real(8), parameter    :: pi=3.1415926535897932385D0

  private

  public im, jm, nm                           ! �ʻ�����, �����ȿ�, Ⱦ��
  public it, t, y, ip, p, r, ia, a            ! �Ѵ��Ѻ������
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

  save im, jm, nm                             ! �ʻ�����, �����ȿ�, Ⱦ�¤򵭲�
  save it, t, y, ip, p, r, ia, a              ! �Ѵ�������򵭲�
  save id, jd, iw                             ! �Ѵ���������礭��

  contains
  !--------------- ����� -----------------
    subroutine w_base_Initial(n_in,i_in,j_in,np_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ OPENMP ���ѻ���
      ! ���祹��åɿ������ꤹ��.
      !
      ! �ºݤλ��ѤˤϾ�̥��֥롼���� w_Initial ���Ѥ��뤳��.
      !
      integer,intent(in) :: i_in              !(in) �ʻ�����(����)
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �������ȿ�
      integer,intent(in), optional :: np_in   !(in) OPENMP �Ǥκ��祹��åɿ�

      integer :: i, j

      im = i_in  ; jm = j_in  ; nm = n_in

      if ( present(np_in) )then
         np = np_in

         if ( np .gt. 1 ) then
            openmp = .true. 
            allocate(wv((nm+4)*(nm+3)*np))
            call MessageNotify('M','w_base_Initial', &
                 'OpenMP computation was set up.')
         else
            openmp = .false. 
         endif

      else
         openmp = .false. 
      endif

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( openmp ) then
         jd = jm
      else if ( jm/2*2 .eq. jm ) then
         jd = jm+1
      else
         jd = jm
      endif
      allocate(t(im*2))                       ! �Ѵ�������
      allocate(ip(((nm+1)/2+nm+1)*2))         ! �Ѵ�������
      allocate(p(((nm+1)/2+nm+1)*jm))         ! �Ѵ�������
      allocate(r(((nm+1)/2*2+3)*(nm/2+1)))    ! �Ѵ�������
      allocate(ia((nm+1)*(nm+1)*4))           ! �Ѵ�������
      allocate(a((nm+1)*(nm+1)*6))            ! �Ѵ�������
      allocate(y(jm/2,4))                     ! �Ѵ�������

      if ( openmp ) then
         iw=(im+nm+1)*3*jm/2
      else
         iw=max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      endif

      allocate(x_Lon(0:im-1))                ! �ʻ�����ɸ��Ǽ����(����)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(y_Lat(1:jm))
      allocate(y_Lat_Weight(1:jm))             ! �ʻ�����ɸ��Ǽ����
      allocate(xy_Lat(0:im-1,1:jm))            ! �ʻ�����ɸ��Ǽ����

      call sninit(nm,im,jm,it,t,y,ip,p,r,ia,a)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! ���ٺ�ɸ
         x_Lon_Weight(i) = 2*pi/im           ! ���ٺ�ɸ�Ť�
      enddo


      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(y(j,1))        ! ���ٺ�ɸ
         y_Lat(jm/2-j+1) = -asin(y(j,1))        ! ���ٺ�ɸ
         y_Lat_Weight(jm/2+j)   = 2*y(j,2)      ! ���ٽŤ�(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*y(j,2)      ! ���ٽŤ�(Gauss grid)
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      call MessageNotify('M','w_base_initial',&
           'w_base_module (2009/07/30) is initialized')

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

      call snnm2l(n,m,l_nm_array00)
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
      
      call snl2nm(l,nm_l_int(1),nm_l_int(2))
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

      real(8), intent(in)   :: w_data((nm+1)*(nm+1))
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
      real(8) :: xy_work(id,jd)                   ! w_xy,xy_w �Ѵ�������
      real(8) :: q(((nm+1)/2+nm+1)*jm)            ! �������
      real(8) :: ws(iw),ww(iw)                    ! ���������

      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i, j

      logical :: first=.true.                    ! ���Ƚ�ꥹ���å�
      save first

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

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','xy_w', &
                 'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntsog(nm,im,id,jm,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call snts2g(nm,im,id,jm,jd,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      do i=0,im-1
        do j=1,jm
          xy_w(i,j) = xy_work(i+1,j)
        enddo
      enddo
      first = .false.

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(1 ����).
      !
      real(8)               :: w_xy((nm+1)*(nm+1))
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

      real(8) :: xy_work(id,jd)                   ! w_xy,xy_w �Ѵ�������
      real(8) :: q(((nm+1)/2+nm+1)*jm)            ! �������
      real(8) :: ws(iw),ww(iw)                    ! ���������

      integer, parameter  :: ipow_default  = 0    ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0    ! �����å��ǥե������

      integer ipval, ifval, i, j

      logical :: first=.true.                     ! ���Ƚ�ꥹ���å�
      save first

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
      
      do i=0,im-1
        do j=1,jm
          xy_work(i+1,j)=xy_data(i,j)
        enddo
      enddo

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','w_xy', &
                 'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntgos(nm,im,id,jm,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call sntg2s(nm,im,id,jm,jd,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      first = .false.

    end function w_xy

  end module w_base_module
