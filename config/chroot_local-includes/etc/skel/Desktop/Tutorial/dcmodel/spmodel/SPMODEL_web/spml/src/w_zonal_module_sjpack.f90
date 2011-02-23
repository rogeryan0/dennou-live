!--
!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_zonal_module_sjpack
!
!   spml/w_zonal_module_sjpack �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ�
!   �Ӿ�Ū 1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
!   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
!
!   ������ l_module_sjpack ���Ѥ��Ƥ���. �ǲ����Ǥϥ른���ɥ�¿�༰�Ѵ�
!   �Υ��󥸥�Ȥ��� ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
!
!   �ؿ�, ���֥롼�����̾���ȵ�ǽ�� w_zonal_module �Τ�Τ�Ʊ�����߷פ��Ƥ���. 
!   �������ä� use ʸ�� l_module ���� w_zonal_module_sjpack ���ѹ���������� 
!   SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
!
!
!����  2009/09/22  �ݹ�����  w_zonal_module �� SJPACK �Ѥ˲�¤
!
!++
module w_zonal_module_sjpack
  !
  != w_zonal_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro
  ! Version:: $Id: w_zonal_module_sjpack.f90,v 1.1 2009-09-24 07:12:09 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !   spml/w_zonal_module_sjpack �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ�
  !   �Ӿ�Ū 1 ����ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
  !   ���ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
  !
  !   ������ l_module_sjpack ���Ѥ��Ƥ���. �ǲ����Ǥϥ른���ɥ�¿�༰�Ѵ�
  !   �Υ��󥸥�Ȥ��� ISPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
  !
  !   �ؿ�, ���֥롼�����̾���ȵ�ǽ�� w_zonal_module �Τ�Τ�Ʊ�����߷פ��Ƥ���. 
  !   �������ä� use ʸ�� l_module ���� w_zonal_module_sjpack ���ѹ���������� 
  !   SJPACK �ε�ǽ���Ȥ���褦�ˤʤ�. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (w_, nm_, n_, xy_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   w_   ::  ���ڥ��ȥ�ǡ���
  !   xy_  ::  2 �����ʻ����ǡ���
  !   nm_  ::  ���ڥ��ȥ�ǡ������¤�� 3 ��������(���ڥ��ȥ�ǡ������¤Ӥ�
  !            ���ȿ� n, �Ӿ��ȿ� m �ǻ��ꤵ��� 2 ��������)
  !   n_   ::  ���ڥ��ȥ�ǡ������¤�� 2 �������� (���ڥ��ȥ�ǡ������¤Ӥ�
  !            ���ȿ� n �ǻ��ꤵ��� 1 ��������)
  !   x_   ::  �������� 1 �����ʻ����ǡ���
  !   y_   ::  �������� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_w_w, _w, _xy, _x, _y) ��, �����ѿ��η����ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _w   :: ���ڥ��ȥ�ǡ���
  !   _w_w :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _xy  :: 2 �����ʻ����ǡ���
  !   _x   :: �������� 1 �����ʻ����ǡ���
  !   _y   :: �������� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * xy : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:im-1,1:jm). 
  !   * im, jm �Ϥ��줾�����, ���ٺ�ɸ�γʻ������Ǥ���, ���֥롼����
  !     w_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * w : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(nm+1). 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, ���֥롼���� w_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. w_module ��������礭�����ۤʤ뤳�Ȥ����. 
  !
  ! * nm : ���ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-nm:nm). 
  !     �� 1 ��������ʿ���ȿ�,  �� 2 �������Ӿ��ȿ�(���ߡ�)��ɽ��. 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, ���֥롼���� w_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���.
  !
  ! * n : ���ڥ��ȥ�ǡ������¤�� 1 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm). 
  !   * �� 1 ��������ʿ���ȿ���ɽ��. nm �ϵ���Ĵ��ȡ���κ������ȿ��Ǥ���, 
  !     ���֥롼���� w_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * x, y : ����, �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1) 
  !     ����� real(8), dimension(1:jm).
  !
  ! * w_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * xy_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! w_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_Lon, y_Lat     ::  �ʻ�����ɸ(����, ���ٺ�ɸ)���Ǽ���� 1 ��������
  ! x_Lon_Weight, y_Lat_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! xy_Lon, xy_Lat   :: �ʻ����ǡ����η��١����ٺ�ɸ(X,Y)
  !                     (�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! xy_w :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! w_xy :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! l_nm, nm_l :: ���ڥ��ȥ�ǡ����γ�Ǽ���֤����ȿ����Ӿ��ȿ����Ѵ� 
  !
  !==== ��ʬ
  !
  ! w_Lapla_w       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! rn              :: ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���. 
  ! irm             :: ������ʬ�黻������
  ! w_LaplaInv_w    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! w_DLon_w        :: ���ڥ��ȥ�ǡ����˷�����ʬ��/�ߦˤ���Ѥ�����
  ! xy_GradLon_w    :: ���ڥ��ȥ�ǡ�����
  !                    ���۷�������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����
  ! xy_GradLat_w    :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! w_DivLon_xy     :: �ʻҥǡ�����
  !                    ȯ����������ʬ 1/cos�ա���/�ߦˤ���Ѥ�����
  ! w_DivLat_xy     :: �ʻҥǡ�����
  !                    ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ�����
  ! w_Div_xy_xy     :: �٥��ȥ���ʬ�Ǥ��� 2 �Ĥγʻҥǡ�����ȯ������Ѥ�����
  ! w_Jacobian_w_w  :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! xy_GradLambda_w :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦˤ���Ѥ�����
  ! xy_GradMu_w     :: ���ڥ��ȥ�ǡ�����
  !                    ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! w_DivLambda_xy  :: �ʻҥǡ�����
  !                    ȯ����������ʬ 1/(1-��^2)����/�ߦˤ���Ѥ�����
  ! w_DivMu_xy      :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  !
  !==== ���
  !
  ! Interpolate_w :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  !
  !==== ��ʬ��ʿ��
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 �����ʻ����ǡ����η���������ʬ�����ʿ��
  ! IntLon_x, AvrLon_x         :: 1 ����(X)�ʻ����ǡ����η���������ʬ�����ʿ��
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y         :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! nm_EnergyFromStreamfunc_w  :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��
  !                               �׻����� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  ! n_EnergyFromStreamfunc_w   :: ή���ؿ����饨�ͥ륮�����ڥ��ȥ��
  !                               �׻����� (��ʿ���ȿ� n ����) 
  ! nm_EnstrophyFromStreamfunc_w  :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  !                                  �׻����� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  ! n_EnstrophyFromStreamfunc_w   :: ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��
  !                                  �׻����� (��ʿ���ȿ� n ����)
  ! w_spectrum_VMiss              ::  ��»��
  !
  !
  use dc_message
  use l_module_sjpack, w_y => l_y, y_w => y_l, &
       y_GradLat_w => y_GradLat_l, w_DivLat_y => l_DivLat_y, &
       w_Lapla_w => l_Lapla_l, w_LaplaInv_w => l_LaplaInv_l, &
       Interpolate_alat_w => Interpolate_l

  implicit none

  integer               :: im=64            ! �ʻ���������(����)
  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: nm=21            ! �����ȿ�������
  integer               :: np=1             ! OPENMP ���祹��åɿ�

  real(8), allocatable  :: x_Lon(:)                  ! ���ٷ���
  real(8), allocatable  :: x_Lon_Weight(:)           ! ��ɸ�Ť�
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  real(8), allocatable  :: rn(:,:)
  integer, allocatable  :: irm(:,:)

  real(8), parameter    :: pi=3.1415926535897932385D0

  real(8) :: w_spectrum_VMiss = -999.000    ! ��»�ͽ����

  private

  public w_Initial                            ! �����

  public x_Lon, y_Lat                         ! �ʻҺ�ɸ
  public x_Lon_weight, y_Lat_Weight           ! �ʻҺ�ɸ�Ť�
  public xy_Lon, xy_Lat                       ! �ʻҺ�ɸ(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! �Ѵ��ؿ�

  public rn, irm                              ! ��ץ饷����/������ʬ�黻������

  public w_Lapla_w, w_LaplaInv_w              ! ��ץ饷����ȵձ黻
  public w_DLon_w                             ! ������ʬ
  public xy_GradLon_w, xy_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xy, w_DivLat_xy             ! ȯ������ʬ
  public w_Div_xy_xy                          ! ȯ������ʬ
  public w_Jacobian_w_w                       ! �䥳�ӥ���
  public xy_GradLambda_w, xy_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xy, w_DivMu_xy           ! ȯ������ʬ(��,�̺�ɸ)

  public Interpolate_w                        ! ��ִؿ�

  public IntLonLat_xy                         ! ���ٷ�����ʬ
  public y_IntLon_xy, IntLon_x                ! ������ʬ    
  public x_IntLat_xy, IntLat_y                ! ������ʬ    
  public AvrLonLat_xy                         ! ���ٷ���ʿ��
  public y_AvrLon_xy, AvrLon_x                ! ����ʿ��    
  public x_AvrLat_xy, AvrLat_y                ! ����ʿ��    

  public nm_EnergyFromStreamfunc_w            ! ���ͥ륮�����ڥ��ȥ�           
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnergyFromStreamfunc_w             ! ���ͥ륮�����ڥ��ȥ�
                                              ! (��ʿ���ȿ� n ����) 
  public nm_EnstrophyFromStreamfunc_w         ! ���󥹥ȥ�ե������ڥ��ȥ�     
                                              ! (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)
  public n_EnstrophyFromStreamfunc_w          ! ���󥹥ȥ�ե������ڥ��ȥ�  
                                              !  (��ʿ���ȿ� n ����)
  public w_spectrum_VMiss                     ! ��»��

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

  save im, jm, nm                             ! �ʻ�����, �����ȿ��򵭲�

contains

  !--------------- ����� -----------------
    subroutine w_initial(n_in,i_in,j_in,np_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ OPENMP ���ѻ���
      ! ���祹��åɿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      ! np_in �� w_module �Ȥθߴ����Τ���ˤ����Ƥ�������Ǥ���. 
      ! OPENMP �׻��ϼ�������Ƥ��ʤ�. 
      !
      integer,intent(in) :: i_in              !(in) �ʻ�����(����)
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �����ȿ�������
      integer,intent(in), optional :: np_in   !(in) OPENMP �Ǥκ��祹��åɿ�

      integer :: i, j, n

      if ( present (np_in) )then
         call MessageNotify('W','w_initial','OPENMP calculation not supported')
      endif

      if ( i_in /= 1  )then
         call MessageNotify('W','w_initial','Longitudinal dimension should be 1')
      endif

      im = i_in  ; jm = j_in  ; nm = n_in

      allocate(x_Lon(0:im-1))                ! �ʻ�����ɸ��Ǽ����(����)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(xy_Lat(0:im-1,1:jm))          ! �ʻ�����ɸ��Ǽ����

      allocate(rn(0:nm,2),irm(nm+1,2))

      call l_initial(n_in,j_in)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! ���ٺ�ɸ
         x_Lon_Weight(i) = 2*pi/im           ! ���ٺ�ɸ�Ť�
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      do n=0,nm
         rn(n,1) = -n*(n+1)
      enddo

      rn(0,2) = 1.0D0
      do n=1,nm
         rn(n,2) = -1/(n*(n+1))
      enddo

      do n=1,nm+1
         irm(n,1) = n
         irm(n,2) = 0
      enddo

      call MessageNotify('M','w_initial',&
           'w_zonal_module_sjpack (2009/09/22) is initialized')

    end subroutine w_initial

  !--------------- �����Ѵ� -----------------

    function l_nm_array00(n,m)
      !
      ! ���ȿ�(n)�������ȿ�(m,���ߡ�)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
      ! 
      ! ���� n,m ���Ȥ�������ͤξ��, �����ͤ��֤�. 
      !
      integer               :: l_nm_array00   
      !(out) ���ڥ��ȥ�ǡ����γ�Ǽ���� 

      integer, intent(in)   :: n     !(in) ���ȿ�
      integer, intent(in)   :: m     !(in) �Ӿ��ȿ�           

      if ( m /= 0 ) then
         call MessageNotify('E','l_nm_array00', &
              'zonal wavenumber should be zero in w_zonal_module')
      end if

      l_nm_array00 = n+1

    end function l_nm_array00

    function l_nm_array01(n,marray)           ! ���ڥ��ȥ�ǡ����γ�Ǽ���� 
      !
      ! ���ȿ�(n)�������ȿ�(m, ���ߡ�)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
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
      ! ���ȿ�(n)�������ȿ�(m,���ߡ�)���餽�Υ��ڥ��ȥ�ǡ����γ�Ǽ���֤��֤�.
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
      
      nm_l_int(1) = l-1
      nm_l_int(2) = 0
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

      real(8), intent(in)   :: w_data(nm+1)
      !(in) ���ڥ��ȥ�ǡ���

      integer, intent(in), optional  :: ipow      
      !(in) ���Ѥ����� 1/cos�� �μ���. ��ά���� 0. 

      integer, intent(in), optional  :: iflag
      !(in) �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !    1 : ������ʬ cos�ա���/�ߦ� ����Ѥ��������Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�(�����)
      !    ��ά���� 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i

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

      if ( ifval == -1 ) then                          ! ������ʬ�� 0 
         xy_w = 0.0D0        
      else if ( ifval == 1 ) then                    ! ������ʬ
         do i=0,im-1
            xy_w(i,:) = y_GradLat_w(w_data) * cos(y_Lat)
         enddo
      else if ( ifval == 2 ) then                     ! sin�� �򤫤������Ѵ�
         do i=0,im-1
            xy_w(i,:) = y_w(w_data)
         enddo
         xy_w = xy_w * sin(xy_Lat)
      else
         do i=0,im-1
            xy_w(i,:) = y_w(w_data)
         enddo
      endif

      if ( ipval /= 0 ) then
         xy_w = xy_w/cos(xy_Lat)**ipval
      end if

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����(1 ����).
      !
      real(8)               :: w_xy(nm+1)
      !(out) ���ڥ��ȥ�ǡ���

      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) �ʻ����ǡ���

      integer, intent(in), optional  :: ipow
      !(in) �Ѵ�����Ʊ���˺��Ѥ����� 1/cos�� �μ���. ��ά���� 0.

      integer, intent(in), optional  :: iflag
      ! �Ѵ��μ���
      !    0 : �̾�����Ѵ�
      !    1 : ������ʬ 1/cos�ա���(f cos^2��)/�ߦ� ����Ѥ��������Ѵ�
      !   -1 : ������ʬ����Ѥ��������Ѵ�
      !    2 : sin�դ���Ѥ��������Ѵ�
      !  ��ά���� 0.


      integer, parameter  :: ipow_default  = 0    ! �����å��ǥե������
      integer, parameter  :: iflag_default = 0    ! �����å��ǥե������

      integer ipval, ifval

      real(8)             :: xy_work(0:im-1,1:jm) ! �ʻ����ǡ����������

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

      xy_work = xy_data/cos(xy_Lat)**ipval

      if ( ifval == -1 ) then                         ! ������ʬ�� 0 
         w_xy = 0.0D0        
      else if ( ifval == 1 ) then                     ! ������ʬ
         xy_work = xy_work * cos(xy_Lat)
         w_xy = w_DivLat_y(xy_work(0,:))
      else if ( ifval == 2 ) then                     ! sin�� �򤫤������Ѵ�
         xy_work = xy_work * sin(xy_Lat)
         w_xy = w_y(xy_work(0,:))
      else
         w_xy = w_y(xy_work(0,:))
      endif
      
    end function w_xy

  !--------------- ��ʬ�׻� -----------------
    function w_DLon_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˷�����ʬ ��/�ߦ� ����Ѥ�����(1 ����).
      !
      ! ���ڥ��ȥ�ǡ����η�����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ������ʬ��/�ߦˤ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      ! 
      real(8)              :: w_DLon_w(nm+1)
      !(out) ���ڥ��ȥ�ǡ����η�����ʬ

      real(8), intent(in)  :: w_data(nm+1)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      w_DLon_w = 0.0D0

    end function w_DLon_w

    function xy_GradLon_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(1 ����).
      !
      real(8)              :: xy_GradLon_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data(nm+1)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xy_GradLon_w = xy_w(w_data,ipow=1,iflag=-1)

    end function xy_GradLon_w

    function xy_GradLat_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: xy_GradLat_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data(nm+1)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xy_GradLat_w = xy_w(w_data,ipow=1,iflag=1)

    end function xy_GradLat_w

    function w_DivLon_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLon_xy(nm+1)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���
      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) ���ϳʻ����ǡ���

      w_DivLon_xy = w_xy(xy_data,ipow=1,iflag=-1)

    end function w_DivLon_xy

    function w_DivLat_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLat_xy(nm+1)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) ���ϳʻ����ǡ���

      w_DivLat_xy = w_xy(xy_data,ipow=1,iflag=1)

    end function w_DivLat_xy

    function w_Div_xy_xy(xy_u,xy_v)
      !
      ! 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ����׻���, 
      ! ���ڥ��ȥ�ǡ����Ȥ����֤�(1 ����).
      !
      real(8)              :: w_Div_xy_xy(nm+1)
      !(out) 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ���Υ��ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xy_u(0:im-1,1:jm)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      real(8), intent(in)  :: xy_v(0:im-1,1:jm)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      w_Div_xy_xy = w_Divlon_xy(xy_u) + w_Divlat_xy(xy_v)

    end function w_Div_xy_xy

    function w_Jacobian_w_w(w_a,w_b)
      ! 2 �ĤΥ��ڥ��ȥ�ǡ����˥䥳�ӥ���
      !
      !   J(f,g) = ��f/�ߦˡ���g/�ߦ� - ��g/�ߦˡ���f/�ߦ�
      !          = ��f/�ߦˡ�1/cos�ա���g/�ߦ�
      !             - ��g/�ߦˡ�1/cos�ա���f/�ߦ�
      !
      ! ����Ѥ�����(1 ����).

      real(8)             :: w_Jacobian_w_w(nm+1)
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      w_Jacobian_w_w = 0.0D0      

    end function w_Jacobian_w_w

  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xy_GradLambda_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(1 ����).
      !
      real(8)              :: xy_GradLambda_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data(nm+1)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      
      xy_GradLambda_w = xy_w(w_data,ipow=0,iflag=-1)

    end function xy_GradLambda_w

    function xy_GradMu_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: xy_GradMu_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data(nm+1)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xy_GradMu_w = xy_w(w_data,ipow=0,iflag=1)

    end function xy_GradMu_w

    function w_DivLambda_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLambda_xy(nm+1)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) ���ϳʻ����ǡ���

      w_DivLambda_xy = w_xy(xy_data,ipow=2,iflag=-1)

    end function w_DivLambda_xy

    function w_DivMu_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivMu_xy(nm+1)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) ���ϳʻ����ǡ���

      w_DivMu_xy = w_xy(xy_data,ipow=2,iflag=1)

    end function w_DivMu_xy

  !--------------- ��ַ׻� -----------------
    function Interpolate_w(w_data,alon,alat)
      real(8), intent(IN) :: w_data(nm+1)   ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alon           ! ��֤������(����)
      real(8), intent(IN) :: alat           ! ��֤������(����)
      real(8)             :: Interpolate_w  ! ��֤�����
      
      Interpolate_w = Interpolate_alat_w(w_data,alat)

    end function Interpolate_w

  !--------------- ��ʬ�׻� -----------------
    function IntLon_x(x_data)
      !
      ! 1 ��������(X)�ʻ����ǡ����� X ������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���.
      !
      real(8), intent(in) :: x_data(0:im-1)   !(in)  1 ��������(X)�ʻ����ǡ���
      real(8)             :: IntLon_x         !(out) ��ʬ��

      IntLon_x = sum(x_data * x_Lon_weight)

    end function IntLon_x

    function x_IntLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)           
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8)             :: x_IntLat_xy(0:im-1) 
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ���

      integer :: j

      x_IntLat_xy = 0.0D0
      do j=1,jm
         x_IntLat_xy = x_IntLat_xy + xy_data(:,j) * y_Lat_weight(j)
      enddo

    end function x_IntLat_xy

    function y_IntLon_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8)             :: y_IntLon_xy(1:jm)
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ���

      integer :: i

      y_IntLon_xy = 0.0D0
      do i=0,im-1
         y_IntLon_xy = y_IntLon_xy + xy_data(i,:) * x_Lon_weight(i)
      enddo

    end function y_IntLon_xy

    function IntLonLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(1 ����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)         
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8) :: IntLonLat_xy                         
      !(out) ��ʬ��

      IntLonLat_xy = IntLon_x(x_IntLat_xy(xy_data))

    end function IntLonLat_xy

  !--------------- ʿ�ѷ׻� -----------------
    function AvrLon_x(x_data)
      !
      ! 1 ����(X)�ʻ����ǡ����η���(X)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: x_data(0:im-1)
      !(in)  1 ��������(X)�ʻ����ǡ���
      real(8)             :: AvrLon_x       
      !(out) ʿ����

      AvrLon_x = IntLon_x(x_data)/sum(x_Lon_weight)

    end function AvrLon_x


    function x_AvrLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)
      real(8)             :: x_AvrLat_xy(im)
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ���

      x_AvrLat_xy = x_IntLat_xy(xy_data)/sum(y_Lat_weight)

    end function x_AvrLat_xy

    function y_AvrLon_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)
      real(8)             :: y_AvrLon_xy(1:jm)
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ���

      y_AvrLon_xy = y_IntLon_xy(xy_data)/sum(x_Lon_weight)

    end function y_AvrLon_xy


    function AvrLonLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���

      real(8) :: AvrLonLat_xy
      !(out) ʿ����

      AvrLonLat_xy = AvrLon_x(x_AvrLat_xy(xy_data))

    end function AvrLonLat_xy

  !--------------- ���ͥ륮�����ڥ��ȥ�׻� -----------------
    function nm_EnergyFromStreamfunc_w(w_Strfunc)
      ! 
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�ͥ륮���ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(1 ����).
      !
      !  * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�� (1/2)n(n+1)��(n,m)^2 �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�4�Ф򤫤�����Τ����̾�Ǥ�
      !    �����ͥ륮����������.
      !
      !  * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !    ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !    (����ͤ� -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnergyFromStreamfunc_w
      !(out) ���ͥ륮�����ڥ��ȥ�(��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer :: n, m

      nm_EnergyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nm_EnergyFromStreamfunc_w(n,m)  = 0.0D0
            nm_EnergyFromStreamfunc_w(n,-m) = 0.0D0
         enddo
         nm_EnergyFromStreamfunc_w(n,0) &
              = 0.5 * n*(n+1) * w_Strfunc(l_nm(n,0))**2
      enddo
    end function nm_EnergyFromStreamfunc_w

    function n_EnergyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(1 ����).
      !
      !  * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���ͥ륮�����ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n(n+1)��(n,m)^2 
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4�Ф򤫤�����Τ�
      !    ���̾�Ǥ������ͥ륮����������.
      !

      real(8), intent(in)      :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm) :: n_EnergyFromStreamfunc_w
      !(out) ���ͥ륮�����ڥ��ȥ� (��ʿ���ȿ� n ����) 

      integer :: n

      do n=0,nm
         n_EnergyFromStreamfunc_w(n)  &
              = 0.5 * n*(n+1) * w_StrFunc(l_nm(n,0))**2
      enddo

    end function n_EnergyFromStreamfunc_w

  !--------------- ���󥹥ȥ�ե������ڥ��ȥ�׻� -----------------
    function nm_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ������饨�󥹥ȥ�ե����ε���Ĵ��ȡ����ʬ
      ! (���ڥ��ȥ�)��׻�����(1 ����). 
      !
      ! * ���ȿ� n, �Ӿ��ȿ� m ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ����
      !    ���󥹥ȥ�ե������ڥ��ȥ�� (1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !
      ! * ���ƤΥ��󥹥ȥ�ե������ڥ��ȥ���ʬ���¤�4��/R^2�򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥ�ե�����������. ������ R �ϵ��̤�Ⱦ�¤Ǥ���.
      !
      ! * �ǡ�����¸�ߤ��ʤ����ȿ� n, �Ӿ��ȿ� m ������ˤϷ�»�ͤ���Ǽ�����.
      !   ��»�ͤ��ͤϥ⥸�塼���ѿ� w_spectrum_VMiss �ˤ�ä�����Ǥ���
      !   (����ͤ� -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnstrophyFromStreamfunc_w
      ! ���󥹥ȥ�ե������ڥ��ȥ� (��ʿ���ȿ� n, �Ӿ��ȿ� m ����)

      integer :: n, m

      nm_EnstrophyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nm_EnstrophyFromStreamfunc_w(n,m) = 0.0
            nm_EnstrophyFromStreamfunc_w(n,-m) = 0.0
         enddo
         nm_EnstrophyFromStreamfunc_w(n,0) &
                 = 0.5 * n**2 * (n+1)**2 * w_Strfunc(l_nm(n,0))**2
      enddo

    end function nm_EnstrophyFromStreamfunc_w

    function n_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! ή���ؿ��Υ��ڥ��ȥ�ǡ�����������ȿ��Υ��ͥ륮����ʬ(���ڥ��ȥ�)��
      ! �׻�����(1 ����)
      !
      ! * ���ȿ� n ��ή���ؿ��Υ��ڥ��ȥ���ʬ��(n,m) ���饨�󥹥ȥ�ե���
      !   ���ڥ��ȥ�Ϧ�[m=-nm]^nm(1/2)n^2(n+1)^2��(n,m)^2 �ȷ׻������.
      !    
      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤� 4��/R^2 �򤫤�����Τ�
      !   ���̾�Ǥ������󥹥ȥե�����������.
      !
      real(8), intent(in)      :: w_Strfunc(:)
      !(in) ή���ؿ�(���ڥ��ȥ�ǡ���)

      real(8), dimension(0:nm) :: n_EnstrophyFromStreamfunc_w  
      !(out) ���󥹥ȥ�ե������ڥ��ȥ�(��ʿ���ȿ� n ����)

      integer :: n

      do n=0,nm
         n_EnstrophyFromStreamfunc_w(n)  &
              = 0.5 * n**2 * (n+1)**2 * w_StrFunc(l_nm(n,0))**2
      enddo
    end function n_EnstrophyFromStreamfunc_w

end module w_zonal_module_sjpack
