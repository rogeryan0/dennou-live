!--
!----------------------------------------------------------------------
! Copyright (c) 2008--2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  l_module
!
!   spml/l_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 1 ����
!   ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻�����
!   ����� Fortran90 �ؿ����󶡤���. 
!
!   ������ ISPACK �� LTPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/LTPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!����  2008/12/24  �ݹ�����  1 ����
!      2009/01/09  �ݹ�����  l_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/10/04  �ݹ�����  ��������� p ��������ѿ����ѹ�
!
!++
module l_module
  !
  != l_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: l_module.f90,v 1.6 2009-10-04 04:06:37 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/l_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū 1 ����
  ! ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻�����
  ! ����� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK �� LTPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/LTPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (l_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   l_ :: ���ڥ��ȥ�(�른���ɥ�¿�༰��ʬ)�ǡ���
  !   y_ :: 1 �������ٳʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(GradLat, DivLat, Lapla, LaplaInv)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_l, _y) ��, �����ѿ��η����ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _l :: ���ڥ���(�른���ɥ�¿�༰��ʬ)��ǡ���
  !   _y :: �������� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * p : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm). 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, ���֥롼���� l_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !
  ! * y : �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(1:jm).
  !
  ! * l_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! l_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! y_Lat        ::  �ʻ�����ɸ(����, ���ٺ�ɸ)���Ǽ���� 1 ��������
  ! y_Lat_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! y_l :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! l_y :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! l_Lapla_l       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! l_LaplaInv_l    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! y_GradLat_l     :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! l_DivLat_y      :: �ʻҥǡ�����ȯ����������ʬ
  !                    1 /cos�ա���(g cos��)/�ߦդ���Ѥ�����
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! y_GradMu_l     :: ���ڥ��ȥ�ǡ�����
  !                   ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! l_DivMu_y      :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  !
  !==== ���
  !
  ! Interpolate_l  :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  !
  !==== ��ʬ��ʿ��
  !
  ! IntLat_y, AvrLat_y :: 1 ����(Y)�ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! ������
  !
  use dc_message, only : MessageNotify

  implicit none

  private

  public l_Initial                            ! �����

  public y_Lat                                ! �ʻҺ�ɸ
  public y_Lat_Weight                         ! �ʻҺ�ɸ�Ť�

  public y_l, l_y                             ! �Ѵ��ؿ�
  public l_Lapla_l, l_LaplaInv_l              ! ��ץ饷����ȵձ黻
  public y_GradLat_l                          ! ���۷���ʬ
  public l_DivLat_y                           ! ȯ������ʬ

  public y_GradMu_l                           ! ���۷���ʬ
  public l_DivMu_y                            ! ȯ������ʬ

  public IntLat_y, AvrLat_y                   ! ����ʿ��

  public Interpolate_l                        ! ��ַ׻�

  integer               :: jm=32            ! �ʻ���������(����)
  integer               :: nm=21            ! �����ȿ�������

  real(8), allocatable  :: q(:,:,:), r(:)   ! �Ѵ�������

  real(8), allocatable  :: y_Lat(:)         ! ���ٷ���
  real(8), allocatable  :: y_Lat_Weight(:)  ! ��ɸ�Ť�

  save jm, nm, q, r, y_Lat, y_Lat_Weight

contains

  !--------------- ����� -----------------
    subroutine l_initial(n_in,j_in)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �����ȿ�������

      integer :: j

      jm = j_in ; nm = n_in

      allocate(q(jm/2,2,0:nm),r((nm+1)*(nm+1))) ! �Ѵ�������
      allocate(y_Lat(jm),y_Lat_Weight(jm))      ! ��ɸ�ѿ�����

      call ltinit(nm,jm,q,r)

      call ltogrd(jm,y_Lat,q)

      do j=1,jm/2
         y_Lat_Weight(jm/2+j)   = 2.0D0*q(j,1,0)
         y_Lat_Weight(jm/2+1-j) = y_Lat_Weight(jm/2+j)
      enddo

      call MessageNotify(&
        'M','l_initial','l_module (2009/10/04) is initialized')

    end subroutine l_initial

  !--------------- �����Ѵ� -----------------
    function y_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����
      !
      real(8)               :: y_l(1:jm)
      !(out) �ʻ����ǡ���

      real(8), intent(in)   :: l_data(0:nm)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)               :: l_in(0:nm)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)               :: p(jm) 
      ! �������

      l_in = l_data       ! lts2gz �����ϥǡ�������¸���ʤ��Τ����촹���Ƥ���

      call lts2gz(nm,jm,l_in,y_l,p,q,r)

    end function y_l

    function l_y(y_data)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����
      !
      real(8)               :: l_y(0:nm)
      !(out) �ʻ����ǡ���

      real(8), intent(in)   :: y_data(1:jm)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)               :: y_in(1:jm)
      !(in) ���ڥ��ȥ�ǡ���

      real(8)               :: p(jm) 
      ! �������

      y_in = y_data    ! ltg2sz �����ϥǡ�������¸���ʤ��Τ����촹���Ƥ���

      call ltg2sz(nm,jm,y_in,l_y,p,q,r)

    end function l_y

  !--------------- ��ʬ�׻� -----------------
    function l_Lapla_l(l_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !    ��^2 = 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ���. 
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: l_Lapla_l(0:nm)
      !(out) ���ϥ��ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      call ltclfz(nm,l_data,l_Lapla_l)

    end function l_Lapla_l

    function l_LaplaInv_l(l_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      !
      !    ��^{-2}
      !      =[1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)]^{-1}
      !
      ! ����Ѥ���. 
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: l_LaplaInv_l(0:nm)
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      call ltclbz(nm,l_data,l_LaplaInv_l)

    end function l_LaplaInv_l

    function y_GradLat_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: y_GradLat_l(1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8)              :: p(jm) 
      ! �������

      call lts2vz(nm,jm,l_data,y_GradLat_l,p,q,r)

    end function y_GradLat_l

    function l_DivLat_y(y_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�. 
      !
      real(8)              :: l_DivLat_y(0:nm)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: y_data(1:jm)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: p(jm) 
      ! �������

      call ltv2sz(nm,jm,y_data,l_DivLat_y,p,q,r)

    end function l_DivLat_y

  !--------------- ��ʬ�׻� (�̺�ɸ����) -----------------
    function y_GradMu_l(l_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�. 
      !
      real(8)              :: y_GradMu_l(1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: l_data(0:nm)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      y_GradMu_l = y_GradLat_l(l_data)*cos(y_Lat)

    end function y_GradMu_l

    function l_DivMu_y(y_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: l_DivMu_y(0:nm)
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: y_data(1:jm)
      !(in) ���ϳʻ����ǡ���

      l_DivMu_y = l_DivLat_y(y_data/cos(y_Lat))

    end function l_DivMu_y

  !--------------- ��ʬ�׻� -----------------
    function IntLat_y(y_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 ��������(Y)�ʻ����ǡ���
      real(8)             :: IntLat_y        !(out) ��ʬ��

      IntLat_y = sum(y_data * y_Lat_weight)

    end function IntLat_y

  !--------------- ʿ�ѷ׻� -----------------
    function AvrLat_y(y_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 �������ٳʻ����ǡ���
      real(8)             :: AvrLat_y        !(out) ʿ����

      AvrLat_y = IntLat_y(y_data)/sum(y_Lat_weight)

    end function AvrLat_y

  !--------------- ��ַ׻� -----------------    
    function Interpolate_l(l_data,alat)
      !
      ! ���� alat �ˤ�����ؿ��ͤ�
      ! ���Υ른���ɥ��Ѵ����� l_data ������ַ׻�����
      !
      real(8), intent(IN) :: l_data(0:nm)           ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: alat                   ! ��֤������(����)
      real(8)             :: Interpolate_l          ! ��֤�����
      
      real(8) :: mu
      real(8) :: y0, y1, y2
      integer :: k

      mu = sin(alat)
      Interpolate_l = 0.0D0

      !---- ��a_n^0 L_n^0 �η׻�
      y2 = 0 ; y1 = 0
      do k=nm,1,-1
         y0 = alpha(k,mu) * y1 + beta(k+1)*y2 + l_data(k)
         y2 = y1 ; y1 = y0
      enddo
      Interpolate_l = beta(1) * y2 + mu*sqrt(3.0D0) * y1 + l_data(0) 

    end function Interpolate_l

  !--------------- �����롼���� -----------------
    function alpha(n,x)
      !
      !  �������� P_n �η���
      !
      integer, intent(IN) :: n
      real(8), intent(IN) :: x
      real(8)             :: alpha

      alpha = sqrt( (2.0D0*n+3)*(2.0D0*n+1)/((n+1)*(n+1)) ) * x
    end function alpha

    function beta(n)
      !
      !  �������� P_{n-1} �η���
      !
      integer, intent(IN) :: n
      real(8)             :: beta

      beta = - sqrt( (2.0D0*n+3)*n*n/((2*n-1)*(n+1)*(n+1)) )
    end function beta

end module l_module
