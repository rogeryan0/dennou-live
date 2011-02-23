!--
!----------------------------------------------------------------------
! Copyright (c) 2008--2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  la_module
!
!   spml/la_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū�Ҹ��� 2 ����
!   ή�α�ư��른���ɥ�¿�༰���Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻�����
!   ����� Fortran90 �ؿ����󶡤���. 
!
!   ������ ISPACK �� LTPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!   �Ĥ��Ƥ� ISPACK/LTPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!����  2008/12/24  �ݹ�����  ¿����
!
!++
module la_module
  !
  != la_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: la_module.f90,v 1.5 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/l_module �⥸�塼��ϵ��̾�Ǥη��������˰��ͤ��Ӿ�Ū�Ҹ��� 2 ����
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
  ! * �ؿ�̾����Ƭ (la_, ya_, l_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   la_ :: ���ڥ��ȥ�(�른���ɥ�¿�༰��ʬ)�ǡ���(��ľ¿��)
  !   ya_ :: 2 �������ٱ�ľ�ʻ����ǡ���
  !   l_  :: ���ڥ��ȥ�(�른���ɥ�¿�༰��ʬ)�ǡ���
  !   y_  :: 1 �������ٳʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(GradLat, DivLat, Lapla, LaplaInv)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_la, _ya, _l, _y) ��, �����ѿ��η����ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _la :: ���ڥ���(�른���ɥ�¿�༰��ʬ)��ǡ���(��ľ¿��)
  !   _ya :: ���ٱ�ľ 2 �����ʻ����ǡ���
  !   _l  :: ���ڥ���(�른���ɥ�¿�༰��ʬ)��ǡ���
  !   _y  :: �������� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * la : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,:). 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, 
  !     ���֥롼���� la_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !
  ! * ya : �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(1:jm,:).
  !
  ! * l : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm). 
  !   * nm �ϥ른���ɥ�¿�༰�κ��缡���Ǥ���, ���֥롼���� la_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !
  ! * y : �������� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(1:jm).
  !
  ! * la_, l_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * ya_, y_ �ǻϤޤ�ؿ����֤��ͤ� 2, 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! la_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! y_Lat        ::  �ʻ�����ɸ(����, ���ٺ�ɸ)���Ǽ���� 1 ��������
  ! y_Lat_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  !
  !==== �����Ѵ�
  !
  ! ya_la :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! la_ya :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! y_l   :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! l_y   :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! la_Lapla_la    :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! la_LaplaInv_la :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! ya_GradLat_la  :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! la_DivLat_ya   :: �ʻҥǡ�����
  !                   ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ�����
  !
  ! l_Lapla_l      :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! l_LaplaInv_l   :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! y_GradLat_l    :: ���ڥ��ȥ�ǡ����˸��۷�������ʬ��/�ߦդ���Ѥ�����
  ! l_DivLat_y     :: �ʻҥǡ�����
  !                   ȯ����������ʬ 1/cos�ա���(g cos��)/�ߦդ���Ѥ�����
  !
  !==== ��ʬ(��,��=sin�� ��ɸ)
  !
  ! ya_GradMu_la   :: ���ڥ��ȥ�ǡ�����
  !                   ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! la_DivMu_ya    :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  ! y_GradMu_l     :: ���ڥ��ȥ�ǡ�����
  !                   ���۷�������ʬ (1-��^2)��/�ߦ̤���Ѥ�����
  ! l_DivMu_y      :: �ʻҥǡ�����ȯ����������ʬ��/�ߦ̤���Ѥ�����
  !
  !==== ���
  !
  ! Interpolate_la :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  ! Interpolate_l  :: ���ڥ��ȥ�ǡ�������Ǥ�դ����Ǥ��ͤ����. 
  !
  !==== ��ʬ��ʿ��
  !
  ! a_IntLat_ya, a_AvrLat_ya   :: 2 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  ! IntLat_y, AvrLat_y         :: 1 �����ʻ����ǡ����ΰ���������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! ������
  !
  use dc_message, only : MessageNotify
  use l_module

  implicit none

  private

  public la_Initial                           ! �����

  public y_Lat                                ! �ʻҺ�ɸ
  public y_Lat_Weight                         ! �ʻҺ�ɸ�Ť�

  public ya_la, la_ya                         ! �Ѵ��ؿ�
  public la_Lapla_la, la_LaplaInv_la          ! ��ץ饷����ȵձ黻
  public ya_GradLat_la                        ! ���۷���ʬ
  public la_DivLat_ya                         ! ȯ������ʬ

  public y_l, l_y                             ! �Ѵ��ؿ�
  public l_Lapla_l, l_LaplaInv_l              ! ��ץ饷����ȵձ黻
  public y_GradLat_l                          ! ���۷���ʬ
  public l_DivLat_y                           ! ȯ������ʬ

  public ya_GradMu_la                         ! ���۷���ʬ
  public la_DivMu_ya                          ! ȯ������ʬ

  public y_GradMu_l                           ! ���۷���ʬ
  public l_DivMu_y                            ! ȯ������ʬ

  public a_IntLat_ya, a_AvrLat_ya             ! ����ʿ��
  public IntLat_y, AvrLat_y                   ! ����ʿ��

  public a_Interpolate_la                     ! ��ַ׻�
  public Interpolate_l                        ! ��ַ׻�

  integer               :: jm=32              ! �ʻ���������(����)
  integer               :: nm=21              ! �����ȿ�������

  save jm, nm

contains

  !--------------- ����� -----------------
    subroutine la_initial(n_in,j_in,l_init)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      integer,intent(in) :: j_in              !(in) �ʻ�����(����)
      integer,intent(in) :: n_in              !(in) �����ȿ�������
      logical,intent(in),optional :: l_init   !(in) l_initial �� call �����å�

      if ( .not. present(l_init) ) then
         call l_Initial(n_in,j_in)
      else if ( l_init ) then
         call l_Initial(n_in,j_in)
      endif

      jm = j_in ; nm = n_in

      call MessageNotify(&
        'M','la_initial','la_module (2009/01/09) is initialized')

    end subroutine la_initial

  !--------------- �����Ѵ� -----------------
    function ya_la(la_data)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����(¿����).
      !
      real(8), intent(in)   :: la_data(:,:)
      !(in) ���ڥ��ȥ�ǡ���(0:nm,:)

      real(8)               :: ya_la(1:jm,size(la_data,2))
      !(out) �ʻ����ǡ���

      integer               :: k

      do k=1,size(la_data,2)
         ya_la(:,k) = y_l(la_data(:,k))
      end do

    end function ya_la

    function la_ya(ya_data)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����(¿����).
      !
      real(8), intent(in)   :: ya_data(:,:)
      !(in) ���ڥ��ȥ�ǡ���(1:jm,:)

      real(8)               :: la_ya(0:nm,size(ya_data,2))
      !(out) �ʻ����ǡ���

      integer               :: k

      do k=1,size(ya_data,2)
         la_ya(:,k) = l_y(ya_data(:,k))
      end do

    end function la_ya

  !--------------- ��ʬ�׻� -----------------
    function la_Lapla_la(la_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !    ��^2 = 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ���(¿����). 
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���(0:nm,:)

      real(8)              :: la_Lapla_la(0:nm,size(la_data,2))
      !(out) ���ϥ��ڥ��ȥ�ǡ����Υ�ץ饷����

      integer               :: k

      do k=1,size(la_data,2)
         la_Lapla_la(:,k) = l_Lapla_l(la_data(:,k))
      end do
      
    end function la_Lapla_la

    function la_LaplaInv_la(la_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      !
      !    ��^{-2}
      !      =[1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)]^{-1}
      !
      ! ����Ѥ���(¿����). 
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���(0:nm,:)

      real(8)              :: la_LaplaInv_la(0:nm,size(la_data,2))
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      integer               :: k

      do k=1,size(la_data,2)
         la_LaplaInv_la(:,k) = l_LaplaInv_l(la_data(:,k))
      end do
    end function la_LaplaInv_la

    function ya_GradLat_la(la_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���(0:nm,:)

      real(8)              :: ya_GradLat_la(1:jm,size(la_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      integer               :: k

      do k=1,size(la_data,2)
         ya_GradLat_la(:,k) = y_GradLat_l(la_data(:,k))
      end do

    end function ya_GradLat_la

    function la_DivLat_ya(ya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).. 
      !
      real(8), intent(in)  :: ya_data(:,:)
      !(in) ���ϳʻ����ǡ���(1:jm,:)

      real(8)              :: la_DivLat_ya(0:nm,size(ya_data,2))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      integer              :: k

      do k=1,size(ya_data,2)
         la_DivLat_ya(:,k) = l_DivLat_y(ya_data(:,k))
      end do

    end function la_DivLat_ya

  !--------------- ��ʬ�׻� (�̺�ɸ����) -----------------
    function ya_GradMu_la(la_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(¿����).. 
      !
      real(8), intent(in)  :: la_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���(0:nm,:)

      real(8)              :: ya_GradMu_la(1:jm,size(la_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      integer              :: k

      do k=1,size(la_data,2)
         ya_GradMu_la(:,k) = y_GradMu_l(la_data(:,k))
      end do

    end function ya_GradMu_la

    function la_DivMu_ya(ya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: ya_data(:,:)
      !(in) ���ϳʻ����ǡ���(1:jm,:)

      real(8)              :: la_DivMu_ya(0:nm,size(ya_data,2))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      integer              :: k

      do k=1,size(ya_data,2)
         la_DivMu_ya(:,k) = l_DivMu_y(ya_data(:,k))
      end do

    end function la_DivMu_ya

  !--------------- ��ʬ�׻� -----------------
    function a_IntLat_ya(ya_data)
      !
      ! 2 �������ٱ�ľ�ʻ����ǡ����ΰ���������ʬ.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  ���ٱ�ľ�ʻ����ǡ���(1:jm,:)

      real(8)             :: a_IntLat_ya(size(ya_data,2))
      !(out) ��ʬ��
      
      integer :: k

      do k=1,size(ya_data,2)
         a_IntLat_ya(k) = sum(ya_data(:,k) * y_Lat_weight)
      end do

    end function a_IntLat_ya

  !--------------- ʿ�ѷ׻� -----------------
    function a_AvrLat_ya(ya_data)
      !
      ! 2 �������ٱ�ľ�ʻ����ǡ����ΰ���(Y)����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  ���ٱ�ľ�ʻ����ǡ���(1:jm,:)

      real(8)             :: a_AvrLat_ya(size(ya_data,2))
      !(out) ʿ����

      a_AvrLat_ya = a_IntLat_ya(ya_data)/sum(y_Lat_weight)

    end function a_AvrLat_ya

  !--------------- ��ַ׻� -----------------    
    function a_Interpolate_la(la_data,alat)
      !
      ! ���� alat �ˤ�����ؿ��ͤ�
      ! ���Υ른���ɥ��Ѵ����� la_data ������ַ׻�����
      !
      real(8), intent(IN) :: la_data(:,:)
      ! ���ڥ��ȥ�ǡ���(0:nm,:)

      real(8), intent(IN) :: alat
      ! ��֤������(����)

      real(8)             :: a_Interpolate_la(size(la_data,2))
      ! ��֤�����
      
      integer               :: k

      do k=1,size(la_data,2)
         a_Interpolate_la(k) = Interpolate_l(la_data(:,k),alat)
      end do

    end function a_Interpolate_la

end module la_module
