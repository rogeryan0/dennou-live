!--
!----------------------------------------------------------------------
! Copyright(c) 2009-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  tee_module
!
!    spml/tee_module �⥸�塼���ʿ��ʿ�Ĵ֤Ǥ� 3 ����ή�α�ư��
!    ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
!    ��ΤǤ���. 
!
!    ��ʿ�����˥ա��ꥨ���Ѵ�����Ӿ岼�ζ����ɤ򰷤������
!    �����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
!    �ؿ����󶡤���. 
!
!    ������ ee_module, at_module ���Ѥ��Ƥ���. �ǲ����Ǥϥա��ꥨ
!    ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
!    ���֥롼������Ѥ��Ƥ���.
!
!
!����  2009/12/19  �ݹ�����  ��������
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!����
!      �ǡ�������� index
!        x : ��ʿ(X)        y : ��ʿ(Y)        z : ��ľ
!        e : �ա��ꥨ�Ѵ����ڥ��ȥ�
!        l : �ա��ꥨ�ؿ����ڥ��ȥ�(X �����ȿ�)
!        m : �ա��ꥨ�ؿ����ڥ��ȥ�(Y �����ȿ�)
!        t : �����ӥ����մؿ����ڥ��ȥ�
!        a : Ǥ�դμ���
!
!        zyx : 3 �����ʻ����ǡ���
!        yx  : ��ʿ 2 �����ʻ����ǡ���
!        zy  : ��ľ 2 �����ʻ����ǡ���
!        zx  : ��ľ 2 �����ʻ����ǡ���
!
!        zee : ��ʿ���ڥ��ȥ��ľ�ʻ����ǡ���
!        tee : ���ڥ��ȥ�ǡ�����ľ�����ӥ����եǡ���
!
!++
module tee_module
  !
  != tee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: tee_module.f90,v 1.2 2010-03-02 10:09:06 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  !    spml/tee_module �⥸�塼���ʿ��ʿ�Ĵ֤Ǥ� 3 ����ή�α�ư��
  !    ���ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� Fortran90 �ؿ����󶡤���
  !    ��ΤǤ���. 
  !
  !    ��ʿ�����˥ա��ꥨ���Ѵ�����Ӿ岼�ζ����ɤ򰷤������
  !    �����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
  !    �ؿ����󶡤���. 
  !
  !    ������ ee_module, at_module ���Ѥ��Ƥ���. �ǲ����Ǥϥա��ꥨ
  !    ����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� ISPACK �� Fortran77 
  !    ���֥롼������Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (tee_, zyx_, zee_, ee_, yx_, x_, y_, z_, a_) ��, 
  !   �֤��ͤη��򼨤��Ƥ���.
  !   tee_  :: ���ڥ��ȥ�ǡ���(2 �ťա��ꥨ�������ӥ������Ѵ�)
  !   zyx_ :: 3 �����ʻ����ǡ���(��ʿ 2 ������ľ 1 ������)
  !   zee_ :: ��ʿ���ڥ��ȥ�, ��ľ�ʻ����ǡ���
  !   e2a_ :: 1 ������������ʿ���ڥ��ȥ�, Ǥ�պ�ɸ�ǡ���
  !   aee_ :: Ǥ�պ�ɸ�ǡ���, ��ʿ���ڥ��ȥ�ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Dz, Lapla,..)
  !   ��, ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (tee_, zyx_, zee_, ee_, yx_, x_, y_, z_, a_) ��, �����ѿ���
  !   �������ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _tee :: ���ڥ��ȥ�ǡ���(2 �ťա��ꥨ�������ӥ������Ѵ�)
  !   _zyx :: 3 �����ʻ����ǡ���(��ʿ 2 ������ľ 1 ������)
  !   _zee :: ��ʿ���ڥ��ȥ�, ��ľ�ʻ����ǡ���
  !   _e2a :: 1 ������������ʿ���ڥ��ȥ�, Ǥ�պ�ɸ�ǡ���
  !   _aee :: Ǥ�պ�ɸ�ǡ���, ��ʿ���ڥ��ȥ�ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * zyx : 3 �����ʻ����ǡ���(��ľ, ��ʿ 2 ����)
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km,0:jm-1,0:im-1). 
  !   * im, jm, km �Ϥ��줾���ʿ X, Y, ��ľ Z ��ɸ�γʻ������Ǥ���, 
  !     ���֥롼���� tee_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !
  ! * tee : ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:nm,-mm:mm,-lm:lm). 
  !   * lm, mm �� X,Y ���������ȿ�, nm �ϥ����ӥ�����¿�༰�κ��缡��
  !     �Ǥ���, ���֥롼���� tee_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !
  ! * zee : ��ʿ���ڥ��ȥ�, ��ľ�ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km,-mm:mm,-lm:lm).
  !
  ! * e2a : 1 ������������ʿ���ڥ��ȥ�, Ǥ�պ�ɸ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension((2*mm+1)*(2*lm+1),:)
  !
  ! * aee :   Ǥ�պ�ɸ�ǡ���, ��ʿ���ڥ��ȥ�ǡ���
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,-mm:mm,-lm:lm).
  !
  ! * tee_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * zyx_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * zee_ �ǻϤޤ�ؿ����֤��ͤϿ�ʿ���ڥ��ȥ�, ư�³ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  ! 
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! tee_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y, z_Z                :: �ʻ�����ɸ(��ʿ X,Y, ��ľ Z ��ɸ)��
  !                                 ��Ǽ����1 ��������
  ! x_X_Weight, y_X_Weight, z_Z_Weight :: �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! zyx_X, zyx_Y, zyx_Z          :: �ʻ����ǡ����ο�ʿ��ľ��ɸ(X,Y,Z)
  !                                 (�ʻ����ǡ����� 3 ��������)
  ! yx_X, yx_Y                   :: �ʻ����ǡ����ο�ʿ��ɸ(X,Y)
  ! zy_Z, zy_Y                   :: �ʻ����ǡ����α�ľ��ʿ��ɸ(Z,Y)
  ! zx_Z, zx_X                   :: �ʻ����ǡ����α�ľ��ʿ��ɸ(Z,X)
  !                                 (�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! zyx_tee, tee_zyx :: ���ڥ��ȥ�ǡ����� 3 �����ʻҥǡ����δ֤��Ѵ�
  !                     (2 �ťա��ꥨ, �����ӥ������Ѵ�)
  ! zyx_zee, zee_zyx :: 3 �����ʻҥǡ����ȿ�ʿ���ڥ��ȥ롦��ľ�ʻҥǡ����Ȥδ�
  !                     ���Ѵ� (2 �ťա��ꥨ�Ѵ�)
  ! zee_tee, tee_zee :: ���ڥ��ȥ�ǡ����ȿ�ʿ���ڥ��ȥ롦��ľ�ʻҥǡ����Ȥδ�
  !                     ���Ѵ� (�����ӥ������Ѵ�)
  ! ee_yx, yx_ee     :: ���ڥ��ȥ�ǡ����� 2 ������ʿ�ʻҥǡ����δ֤��Ѵ�
  !                     (2 �ťա��ꥨ�Ѵ�) 
  ! az_at, at_az     :: Ʊ����ʣ���ĹԤ� (�����ӥ������Ѵ�)�ʻҥǡ�����
  !                     �����ӥ����եǡ����δ֤��Ѵ���
  ! e2a_aee, aee_e2a :: ��ʿ���ڥ��ȥ뼴�� 1 ��������ž��, ž�� ������������.
  !
  !==== ��ʬ
  !
  ! tee_Dx_tee          :: ���ڥ��ȥ�ǡ�����ư����ʬ��/��x ����Ѥ�����
  ! tee_Dy_tee          :: ���ڥ��ȥ�ǡ�����ư����ʬ��/��y ����Ѥ�����
  ! tee_Dz_tee          :: ���ڥ��ȥ�ǡ�����ư����ʬ��/��z ����Ѥ�����
  !
  ! tee_Lapla_tee       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! tee_LaplaH_tee      :: ���ڥ��ȥ�ǡ����˿�ʿ��ץ饷�������Ѥ�����
  ! tee_LaplaHInv_tee   :: ���ڥ��ȥ�ǡ����˵տ�ʿ��ץ饷�������Ѥ�����
  !
  ! tee_Div_zyx_zyx_zyx :: �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����
  !                        ȯ������Ѥ�����
  !
  !==== �ȥ�����ݥ�����׻�����ʬ
  !
  ! tee_ZRot_zyx_zyx         :: �٥��ȥ� v �α��٤�ư�¥٥��ȥ� r ������
  !                             z��(����v) ��׻�����
  ! tee_ZRotRot_zyx_zyx_zyx  :: �٥��ȥ�� v �� z��(���ߢ���v) ��׻�����
  ! tee_Potential2Vector     :: �ȥ�����ݥ�����ݥƥ󥷥�뤫��
  !                             �٥��ȥ���׻�����
  ! tee_Potential2Rotation   :: �ȥ�����ݥ�����ݥƥ󥷥���ɽ�����
  !                             ��ȯ���٥��ȥ��β�ž�γ���ʬ��׻�����
  !
  !==== �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ����
  !
  ! zee_ToroidalEnergySpectrum_tee, zk_ToroidalEnergySpectrum_tee   ::
  !     �ȥ�����ݥƥ󥷥�뤫�饨�ͥ륮���Υա��ꥨ����ʬ��׻�����
  ! zee_PoloidalEnergySpectrum_tee, zk_PoloidalEnergySpectrum_tee   :: 
  !     �ݥ�����ݥƥ󥷥�뤫�饨�ͥ륮���Υա��ꥨ����ʬ��׻�����
  !
  !==== ����������
  !
  ! tee_BoundariesTau, tee_BoundariesGrid, tee_Boundaries                ::
  !     �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ�Ѥ���(����ˡ, ����ˡ)
  !
  ! tee_TorBoundariesTau, tee_TorBoundariesGrid, tee_TorBoundaries       ::
  !     ®�٥ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ,����ˡ) 
  !
  ! zee_LaplaPol2Pol_zee, tee_LaplaPol2PolGrid_tee                       ::
  !     ®�٥ݥ�����ݥƥ󥷥�릵��^2���������
  !     (�����Ϥ����줾������ӥ����ճʻ���,�����ӥ����շ���)
  !
  ! tee_TorMagBoundariesTau, tee_TorMagBoundariesGrid, tee_TorMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥��ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
  !
  ! tee_PolMagBoundariesTau, tee_PolMagBoundariesGrid, tee_PolMagBoundaries ::
  !     ����ȥ�����ݥƥ󥷥�붭���ζ�������Ŭ�Ѥ���(����ˡ, ����ˡ)
  !
  !==== ��ʬ��ʿ��(3 �����ǡ���)
  !
  ! IntZYX_zyx, AvrZYX_zyx     :: 3 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! z_IntYX_zyx, z_AvrYX_zyx   :: 3 �����ʻ����ǡ����ο�ʿ��ʬ�����ʿ��
  ! y_IntZX_zyx, y_AvrZX_zyx   :: 3 �����ʻ����ǡ�����ZX��ʬ�����ʿ��
  ! z_IntZY_zyx, z_AvrZY_zyx   :: 3 �����ʻ����ǡ�����ZY��ʬ�����ʿ��
  ! zy_IntX_zyx, zy_AvrX_zyx   :: 3 �����ʻ����ǡ����ο�ʿX������ʬ�����ʿ��
  ! zx_IntY_zyx, zx_AvrY_zyx   :: 3 �����ʻ����ǡ����ο�ʿY������ʬ�����ʿ��
  ! zx_IntZ_zyx, zx_AvrZ_zyx   :: 3 �����ʻ����ǡ����α�ľ������ʬ�����ʿ��
  !
  !==== ��ʬ��ʿ��(2 �����ǡ���)
  !
  ! IntYX_yx, AvrYX_yx :: 2 �����ʻ����ǡ����ο�ʿ��ʬ�����ʿ��
  ! IntZX_zx, AvrZX_zx :: 2 ����(ZX)�ʻ����ǡ�����ZX��ʬ�����ʿ��
  ! IntZY_zy, AvrZY_zy :: 2 ����(ZY)�ʻ����ǡ�����ZY��ʬ�����ʿ�� 
  ! y_IntX_yx, y_AvrX_yx   :: ��ʿ 2 �����ʻ����ǡ�����X������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx   :: ��ʿ2 �����ʻ����ǡ�����Y������ʬ�����ʿ��
  ! z_IntX_zx, z_AvrX_zx   :: 2 ����(ZX)�ʻ����ǡ�����X������ʬ�����ʿ��
  ! x_IntZ_zx, x_AvrZ_zx   :: 2 ����(ZX)�ʻ����ǡ�����Z������ʬ�����ʿ��
  ! z_IntY_zy, z_AvrY_zy   :: 2 ����(ZY)�ʻ����ǡ�����Y������ʬ�����ʿ��
  ! y_IntZ_zy, y_AvrZ_zy   :: 2 ����(ZY)�ʻ����ǡ�����Z������ʬ�����ʿ��
  !
  !==== ��ʬ��ʿ��(1 �����ǡ���)
  !
  ! IntX_x, AvrX_x  :: 1 ����(X)�ʻ����ǡ�����X������ʬ�����ʿ��
  ! IntY_y, AvrY_y  :: 1 ����(Y)�ʻ����ǡ�����Y������ʬ�����ʿ��
  ! IntZ_z, AvrZ_z  :: 1 ����(Z)�ʻ����ǡ�����Z������ʬ�����ʿ��
  !
  !==== ��ַ׻�
  !
  ! Interpolate_tee :: ���ڥ��ȥ�ǡ�������Ǥ�դ������ͤ���֤���. 
  ! 
  use dc_message
  use lumatrix
  use ee_module
  use at_module, z_Z => g_X, z_Z_WEIGHT => g_X_WEIGHT, &
                 at_az => at_ag, az_at => ag_at, &
                 t_z => t_g, z_t => g_t, &
                 t_Dz_t => t_Dx_t, at_Dz_at => at_Dx_at
  implicit none
  private

  public tee_Initial

  public x_X, x_X_Weight
  public y_Y, y_Y_Weight
  public z_Z, z_Z_Weight
  public yx_X, yx_Y, zy_Z, zy_Y, zx_X, zx_Z
  public zyx_X, zyx_Y, zyx_Z
  public zee_Z
  public tee_VMiss

  public ee_yx, yx_ee
  public at_Dz_at, t_Dz_t, az_at, at_az, z_t, t_z
  public zyx_tee, tee_zyx, zyx_zee, zee_zyx, zee_tee, tee_zee
  public aee_e2a, e2a_aee, ee_e2, e2_ee
  public tee_Dx_tee, tee_Dy_tee, tee_Dz_tee
  public tee_Lapla_tee, tee_LaplaH_tee, tee_LaplaHInv_tee

  public zy_IntX_zyx, zx_IntY_zyx, yx_IntZ_zyx
  public x_IntZY_zyx, y_IntZX_zyx, z_IntYX_zyx
  public IntZYX_zyx

  public x_IntY_yx, y_IntX_yx, IntYX_yx
  public z_IntY_zy, y_IntZ_zy, IntZY_zy
  public z_IntX_zx, x_IntZ_zx, IntZX_zx
  public IntX_x, IntY_y, IntZ_z

  public zy_AvrX_zyx, zx_AvrY_zyx, yx_AvrZ_zyx
  public x_AvrZY_zyx, y_AvrZX_zyx, z_AvrYX_zyx
  public AvrZYX_zyx

  public x_AvrY_yx, y_AvrX_yx, AvrYX_yx
  public z_AvrY_zy, y_AvrZ_zy, AvrZY_zy
  public z_AvrX_zx, x_AvrZ_zx, AvrZX_zx
  public AvrX_x, AvrY_y, AvrZ_z

  public tee_ZRot_zyx_zyx, tee_ZRotRot_zyx_zyx_zyx
  public tee_Potential2vector, tee_Potential2Rotation

  public Interpolate_tee

  public zee_ToroidalEnergySpectrum_tee ! nz_ToroidalEnergySpectrum_wt
  public zee_PoloidalEnergySpectrum_tee ! nz_PoloidalEnergySpectrum_wt

  public tee_Boundaries, zee_LaplaPol2Pol_zee, tee_TorBoundaries
  public tee_TormagBoundaries, tee_PolmagBoundaries

  public tee_BoundariesTau, tee_TorBoundariesTau
  public tee_TormagBoundariesTau, tee_PolmagBoundariesTau

  public tee_BoundariesGrid, tee_LaplaPol2PolGrid_tee, tee_TorBoundariesGrid
  public tee_TormagBoundariesGrid, tee_PolmagBoundariesGrid

  interface tee_Boundaries
     module procedure tee_BoundariesTau
  end interface

  interface tee_TorBoundaries
     module procedure tee_TorBoundariesTau
  end interface

  interface tee_TorMagBoundaries
     module procedure tee_TorMagBoundariesTau
  end interface

  interface tee_PolMagBoundaries
     module procedure tee_PolMagBoundariesTau
  end interface

  integer            :: im=32, jm=32, km=16  ! �ʻ���������(X,Y,Z)
  integer            :: lm=10, mm=10, nm=16  ! �����ȿ�������(��ʿX,Y, ��ľZ)
  real(8)            :: xl, yl, zl           ! �ΰ���礭��(��ʿX,Y, ��ľZ)

  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: zyx_X, zyx_Y, zyx_Z    ! ��ɸ
  real(8), dimension(:,:),   allocatable :: zy_Z, zy_Y             ! ��ɸ
  real(8), dimension(:,:),   allocatable :: zx_Z, zx_X             ! ��ɸ
  real(8), dimension(:,:,:), allocatable :: zee_Z                  ! ��ɸ

  real(8) :: tee_VMiss = -999.0        ! ��»��

  save im, jm, km, lm, mm, nm, xl, yl, zl

  contains
  !--------------- ����� -----------------
   subroutine tee_Initial(i,j,k,l,m,n,xmin,xmax,ymin,ymax,zmin,zmax)
     !
     ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ƺ�ɸ���ϰϤ����ꤹ��.
     !
     ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
     ! ���ʤ���Фʤ�ʤ�. 
     !
     integer,intent(in) :: i              ! �ʻ�����(��ʿX)
     integer,intent(in) :: j              ! �ʻ�����(��ʿY)
     integer,intent(in) :: k              ! �ʻ�����(��ľZ)
     integer,intent(in) :: l              ! �����ȿ�(��ʿX�ȿ�)
     integer,intent(in) :: m              ! �����ȿ�(��ʿY�ȿ�)
     integer,intent(in) :: n              ! �����ȿ�(��ľZ�ȿ�)

     real(8),intent(in) :: xmin, xmax     ! X �����ΰ�
     real(8),intent(in) :: ymin, ymax     ! Y �����ΰ�
     real(8),intent(in) :: zmin, zmax     ! Z �����ΰ�

     integer            :: id

     im = i  ; jm = j ; km = k
     lm = l  ; mm = m ; nm = n 
     xl = xmax - xmin
     yl = ymax - ymin
     zl = zmax - zmin

     call ee_Initial(im,jm,lm,mm,xmin,xmax,ymin,ymax,id)

     call at_Initial(km,nm,zmin,zmax)

     allocate(zyx_X(0:km,0:jm-1,0:im-1))
     allocate(zyx_Y(0:km,0:jm-1,0:im-1))
     allocate(zyx_Z(0:km,0:jm-1,0:im-1))

     allocate(zy_Z(0:km,0:jm-1))
     allocate(zy_Y(0:km,0:jm-1))
     allocate(zx_Z(0:km,0:im-1))
     allocate(zx_X(0:km,0:im-1))

     allocate(zee_Z(0:nm,-mm:mm,-lm:lm))

     zyx_X = spread(yx_X,1,km+1)
     zyx_Y = spread(yx_Y,1,km+1)
     zyx_Z = spread(spread(z_Z,2,jm),3,im)

     zy_Z = spread(z_Z,2,jm)
     zy_Y = spread(y_Y,1,km+1)

     zx_Z = spread(z_Z,2,im)
     zx_X = spread(x_X,1,km+1)

     zee_Z = spread(spread(z_Z,2,2*mm+1),3,2*lm+1)

     call MessageNotify('M','tee_initial', &
          'tee_module (2009/12/19) is initialized')

   end subroutine tee_Initial

  !--------------- �����Ѵ� -----------------

    function zyx_tee(tee)
      !
      ! ���ڥ��ȥ�ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 �ťա��ꥨ�����ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension(0:km,0:jm-1,0:im-1)             :: zyx_tee
      !(out) 3 �����ʻ����ǡ���

      zyx_tee = zyx_zee(zee_tee(tee))

    end function zyx_tee

    function tee_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���
      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_zyx
      !(out) 2 �ťա��ꥨ�����ӥ����ե��ڥ��ȥ�ǡ���

      tee_zyx = tee_zee(zee_zyx(zyx))

    end function tee_zyx

    function zyx_zee(zee)
      !
      ! ��ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ������� 3 �����ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm), intent(in) :: zee
      !(in) 2 ������ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ���
      real(8), dimension(0:km,0:jm-1,0:im-1)             :: zyx_zee
      !(out) 3 �����ʻ����ǡ���

      integer :: k

      do k = 0, km
         zyx_zee(k,:,:) = yx_ee(zee(k,:,:))
      enddo

    end function zyx_zee

    function zee_zyx(zyx)
      !
      ! 3 �����ʻҥǡ��������ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���
      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_zyx
      !(out) 2 �������ڥ��ȥ롦��ľ�ʻ����ǡ���

      integer :: k

      do k = 0, km
         zee_zyx(k,:,:) = ee_yx(zyx(k,:,:))
      enddo

    end function zee_zyx

    function zee_tee(tee)
      !
      ! ���ڥ��ȥ�ǡ��������ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 ������ʿ���ڥ��ȥ��ľ�����ӥ����ե��ڥ��ȥ�ǡ���
      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_tee
      !(out) 2 ������ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ���

      zee_tee = aee_e2a(az_at(e2a_aee(tee)))

    end function zee_tee
    
    function tee_zee(zee)
      !
      ! ��ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ������饹�ڥ��ȥ�ǡ�����(��)�Ѵ�����.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm), intent(in) :: zee
      !(in) 2 ������ʿ���ڥ��ȥ롦��ľ�ʻ����ǡ���
      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_zee
      !(out) 2 ������ʿ��ľ�����ӥ����ե��ڥ��ȥ�ǡ���

      tee_zee = aee_e2a(at_az(e2a_aee(zee)))

    end function tee_zee

    function e2a_aee(aee)
      !
      ! ��ʿ���ڥ��ȥ�� 1 ������ž�֤���. 
      !
      real(8), dimension(:,:,:), intent(in)              :: aee
      !(in) Ǥ�պ�ɸ����ʿ 2 �������ڥ��ȥ�ǡ��� dimension(:,-mm:mm,-lm:lm)
      real(8), dimension((2*lm+1)*(2*mm+1),size(aee,1))  :: e2a_aee
      !(out) 1 ���������줿��ʿ���ڥ��ȥ롦Ǥ�պ�ɸ�ǡ���

      if ( size(aee,2) /= 2*mm+1 ) &
           call MessageNotify('E','e2a_aee',&
                              '2nd dimension of input data invalid')
      if ( size(aee,3) /= 2*lm+1 ) &
           call MessageNotify('E','e2a_aee',&
                              '3rd dimension of input data invalid')

      e2a_aee = transpose(reshape(aee,(/size(aee,1),(2*lm+1)*(2*mm+1)/)))

    end function e2a_aee

    function aee_e2a(e2a)
      !
      ! ��ʿ���ڥ��ȥ��ž��Ÿ������. 
      !
      real(8), dimension(:,:),intent(in)                 :: e2a
      !(in) 1 ���������줿��ʿ���ڥ��ȥ롦Ǥ�պ�ɸ�ǡ��� 
      !     dimmension((2*mm*1)*(2*lm*1),:)
      real(8), dimension(size(e2a,2),-mm:mm,-lm:lm)      :: aee_e2a
      !(out) Ǥ�պ�ɸ����ʿ 2 �������ڥ��ȥ�ǡ���

      if ( size(e2a,1) /= (2*mm+1)*(2*lm+1) ) &
           call MessageNotify('E','aee_e2a',&
                              '1st dimension of input data invalid')

      aee_e2a = reshape(transpose(e2a),(/size(e2a,2),2*mm+1,2*lm+1/))

    end function aee_e2a

    function e2_ee(ee)
      !
      ! ��ʿ���ڥ��ȥ�� 1 ������ž�֤���. 
      !
      real(8), dimension(:,:), intent(in)              :: ee
      !(in) Ǥ�պ�ɸ����ʿ 2 �������ڥ��ȥ�ǡ��� dimension(-mm:mm,-lm:lm)
      real(8), dimension((2*lm+1)*(2*mm+1))            :: e2_ee
      !(out) 1 ���������줿��ʿ���ڥ��ȥ롦Ǥ�պ�ɸ�ǡ���

      e2_ee = reshape(e2a_aee(reshape(ee,(/1,2*mm+1,2*lm+1/))), &
                      (/(2*lm+1)*(2*mm+1)/))

    end function e2_ee

    function ee_e2(e2)
      !
      ! ��ʿ���ڥ��ȥ��ž��Ÿ������. 
      !
      real(8), dimension(:),intent(in)                   :: e2
      !(in) 1 ���������줿��ʿ���ڥ��ȥ롦Ǥ�պ�ɸ�ǡ��� 
      !     dimmension((2*mm*1)*(2*lm*1),:)
      real(8), dimension(-mm:mm,-lm:lm)                  :: ee_e2
      !(out) Ǥ�պ�ɸ����ʿ 2 �������ڥ��ȥ�ǡ���

      ee_e2 = reshape(aee_e2a(reshape(e2,(/(2*mm+1)*(2*lm+1),1/))), &
                      (/2*mm+1,2*lm+1/))

    end function ee_e2
    
  !--------------- ��ʬ�׻� -----------------
    function tee_Dx_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˿�ʿ��ʬ ��/��x ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ�����X��ʬ�Ȥ�, �б�����ʻ����ǡ�����X��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dx_tee
      !(in) X��ʬ���줿���ڥ��ȥ�ǡ���

      integer :: n

      do n=0,nm
         tee_Dx_tee(n,:,:) = ee_Dx_ee(tee(n,:,:))
      enddo

    end function tee_Dx_tee

    function tee_Dy_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˿�ʿ��ʬ ��/��y ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ�����Y��ʬ�Ȥ�, �б�����ʻ����ǡ�����Y��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dy_tee
      !(in) X��ʬ���줿���ڥ��ȥ�ǡ���

      integer :: n

      do n=0,nm
         tee_Dy_tee(n,:,:) = ee_Dy_ee(tee(n,:,:))
      enddo

    end function tee_Dy_tee

    function tee_Dz_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˱�ľ��ʬ ��/��z ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����α�ľ��ʬ�Ȥ�, �б�����ʻ����ǡ����˱�ľ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dz_tee
      !(in) ��ľ��ʬ���줿���ڥ��ȥ�ǡ���

      tee_Dz_tee = aee_e2a(at_Dz_at(e2a_aee(tee)))

    end function tee_Dz_tee

    function tee_Lapla_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !     ��^2 = ��^2/��X^2 + ��^2/��Y^2 + ��^2/��Z^2
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Lapla_tee
      !(out) ��ץ饷�������Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      tee_Lapla_tee = tee_LaplaH_tee(tee) + tee_Dz_tee(tee_Dz_tee(tee))

    end function tee_Lapla_tee

    function tee_LaplaH_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˿�ʿ��ץ饷����
      !
      !     ��^2_H = ��^2/��X^2 + ��^2/��Y^2 
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����ο�ʿ��ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ʿ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_LaplaH_tee
      !(out) ��ʿ��ץ饷�������Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      integer :: n
      
      do n=0,nm
         tee_LaplaH_tee(n,:,:) = ee_Lapla_ee(tee(n,:,:))
      enddo

    end function tee_LaplaH_tee

    function tee_LaplaHInv_tee(tee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵տ�ʿ��ץ饷����
      !
      !     ��^-2_H = (��^2/��X^2 + ��^2/��Y^2)^-1
      !
      ! ����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εտ�ʿ��ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �տ�ʿ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_LaplaHInv_tee
      !(out) �տ�ʿ��ץ饷�������Ѥ��줿 2 �������ڥ��ȥ�ǡ���

      integer :: n
      
      do n=0,nm
         tee_LaplaHInv_tee(n,:,:) = ee_LaplaInv_ee(tee(n,:,:))
      enddo

    end function tee_LaplaHInv_tee

    function tee_Div_zyx_zyx_zyx(zyx_VX,zyx_VY,zyx_VZ)
      !
      ! �٥��ȥ���ʬ�Ǥ��� 3 �Ĥγʻҥǡ�����ȯ������Ѥ�����
      ! ���ڥ��ȥ�ǡ������֤�.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) �٥��ȥ���X��ʬ
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) �٥��ȥ���Y��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VZ
      !(in) �٥��ȥ���Z��ʬ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_Div_zyx_zyx_zyx
      !(out) �٥��ȥ���ȯ��

      tee_Div_zyx_zyx_zyx =   tee_Dx_tee(tee_zyx(zyx_VX)) &
                            + tee_Dy_tee(tee_zyx(zyx_VY)) &
                            + tee_Dz_tee(tee_zyx(zyx_VZ))

    end function tee_Div_zyx_zyx_zyx

  !--------------- ��ʬ�׻� -----------------
    !----(���ϥǡ��� zyx)---
    function zy_IntX_zyx(zyx)  ! X��ʬ
      !
      ! 3 �����ʻ����ǡ�����X������ʬ.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:km,0:jm-1)  :: zy_IntX_zyx
      !(out) X������ʬ���줿 2 ����ZY�ʻ����ǡ���

      integer :: i

      zy_IntX_zyx = 0.0d0
      do i=0,im-1
         zy_IntX_zyx(:,:) = zy_IntX_zyx(:,:) &
                       + zyx(:,:,i) * x_X_Weight(i)
      enddo
    end function zy_IntX_zyx

    function zx_IntY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ�����Y��������ʬ.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:km,0:im-1)  :: zx_IntY_zyx        
      !(out) Y��ʬ���줿 2 ����ZY�ʻ����ǡ���.

      integer :: j

      zx_IntY_zyx = 0.0d0
      do j=0,jm-1
         zx_IntY_zyx(:,:) = zx_IntY_zyx(:,:) &
                       + zyx(:,j,:) * y_Y_Weight(j)
      enddo
    end function zx_IntY_zyx

    function yx_IntZ_zyx(zyx)  ! Z��ʬ
      !
      ! 3 �����ʻ����ǡ�����Z������ʬ.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:jm-1,0:im-1)  :: yx_IntZ_zyx
      !(out) Z��ʬ���줿 2 ����YX(��ʿ)�ʻ����ǡ���

      integer :: k

      yx_IntZ_zyx = 0.0d0
      do k=0,km
         yx_IntZ_zyx(:,:) = yx_IntZ_zyx(:,:) &
                       + zyx(k,:,:) * z_Z_Weight(k) 
      enddo
    end function yx_IntZ_zyx

    function x_IntZY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ�����ZY��ʬ
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_IntZY_zyx
      !(out) ZY(�Ҹ���)��ʬ���줿 1 ����X�ʻ����ǡ���

      integer :: j, k

      x_IntZY_zyx = 0.0D0
      do j=0,jm-1
         do k=0,km
            x_IntZY_zyx = x_IntZY_zyx &
                 + zyx(k,j,:) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function x_IntZY_zyx

    function y_IntZX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ�����ZX��ʬ.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8), dimension(0:jm-1)       :: y_IntZX_zyx
      !(out) ZX��ʬ���줿 1 ����Y�ʻ����ǡ���

      integer :: i, k

      y_IntZX_zyx = 0
      do i=0,im-1
         do k=0,km
            y_IntZX_zyx = y_IntZX_zyx &
                 + zyx(k,:,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function y_IntZX_zyx

    function z_IntYX_zyx(zyx)  ! YX(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ�����YX(��ʿ)��ʬ
      ! 
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8), dimension(0:km)     :: z_IntYX_zyx
      !(out) YX(��ʿ)��ʬ���줿 1 ����Z�ʻ����ǡ���

      integer :: i, j

      z_IntYX_zyx = 0
      do j=0,jm-1
         do i=0,im-1
            z_IntYX_zyx = z_IntYX_zyx &
                 + zyx(:,j,i) * x_X_Weight(i) * y_Y_Weight(j)
         enddo
      enddo
    end function z_IntYX_zyx

    function IntZYX_zyx(zyx) ! ZYX(���ΰ�)��ʬ
      !
      ! 3 �����ʻ����ǡ�����ZYX(���ΰ�)��ʬ
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx 
      !(in) 3 �����ʻ����ǡ���

      real(8)                     :: IntZYX_zyx 
      !(out) ���ΰ���ʬ��

      integer :: i, j, k

      IntZYX_zyx = 0
      do i=0,im-1
         do j=0,jm-1
            do k=0,km
               IntZYX_zyx = IntZYX_zyx &
                    + zyx(k,j,i) * x_X_Weight(i) &
                         * y_Y_Weight(j) * z_Z_Weight(k)
            enddo
         enddo
      enddo
    end function IntZYX_zyx

    !----(���ϥǡ��� zy)---
    function z_IntY_zy(zy)  ! Y��ʬ
      !
      ! 2 ����(ZY)�ʻ����ǡ�����Y������ʬ.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY(�Ҹ���)�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntY_zy
      !(out) Y��ʬ���줿 1 ����Z�ʻ����ǡ���

      integer :: j

      z_IntY_zy = 0.0d0
      do j=0,jm-1
         z_IntY_zy(:) = z_IntY_zy(:) + zy(:,j) * y_Y_Weight(j)
      enddo
    end function z_IntY_zy

    function y_IntZ_zy(zy)  ! Z��ʬ
      !
      ! 2 ����(ZY)�ʻ����ǡ�����Z��������ʬ.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:jm-1)  :: y_IntZ_zy
      !(out) Z��ʬ���줿 1 ����Y�ʻ����ǡ���

      integer :: k

      y_IntZ_zy = 0.0d0
      do k=0,km
         y_IntZ_zy(:) = y_IntZ_zy(:) &
                       + zy(k,:) * z_Z_Weight(k) 
      enddo
    end function y_IntZ_zy

    function IntZY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ�����ZY��ʬ
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: IntZY_zy
      !(out) ��ʬ��
      integer :: j, k

      IntZY_zy = 0
      do j=0,jm-1
         do k=0,km
            IntZY_zy = IntZY_zy &
                 + zy(k,j) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZY_zy

    !----(���ϥǡ��� zx)---
    function z_IntX_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ�����X������ʬ.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_IntX_zx
      !(out) X��ʬ���줿 1 ����Z�ʻ����ǡ���

      integer :: i

      z_IntX_zx = 0.0d0
      do i=0,im-1
         z_IntX_zx(:) = z_IntX_zx(:) + zx(:,i) * x_X_Weight(i)
      enddo

    end function z_IntX_zx

    function x_IntZ_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ�����Z������ʬ.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_IntZ_zx
      !(out) Z��ʬ���줿 1 ����X�ʻ����ǡ���

      integer :: k

      x_IntZ_zx = 0.0d0
      do k=0,km
         x_IntZ_zx(:) = x_IntZ_zx(:) &
                       + zx(k,:) * z_Z_Weight(k) 
      enddo

    end function x_IntZ_zx

    function IntZX_zx(zx)  ! ZX��ʬ
      !
      ! 2 ����(ZX)�ʻ����ǡ�����ZX��ʬ
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8)                                 :: IntZX_zx
      !(out) ��ʬ��

      integer :: i, k

      IntZX_zx = 0
      do i=0,im-1
         do k=0,km
            IntZX_zx = IntZX_zx &
                 + zx(k,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZX_zx

    !----(���ϥǡ��� z)---
    function IntZ_z(z)  ! Z��ʬ
      !
      ! 1 ����(Z)�ʻ����ǡ�����Z������ʬ.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 ����Z�ʻ����ǡ���

      real(8)                              :: IntZ_z
      !(out) ��ʬ��

      integer :: k

      IntZ_z = 0.0d0
      do k=0,km
         IntZ_z = IntZ_z + z(k) * z_Z_Weight(k) 
      enddo
    end function IntZ_z

  !--------------- ʿ�ѷ׻� -----------------
    !----(���ϥǡ��� zyx)---
    function zy_AvrX_zyx(zyx)  ! X��ʬ
      !
      ! 3 �����ʻ����ǡ�����X����ʿ��.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8), dimension(0:km,0:jm-1)  :: zy_AvrX_zyx
      !(out) X����ʿ�Ѥ��줿 2 �����Ҹ��̳ʻ����ǡ���

      zy_AvrX_zyx = zy_IntX_zyx(zyx)/sum(x_X_Weight)

    end function zy_AvrX_zyx

    function zx_AvrY_zyx(zyx)  ! Yʿ��
      !
      ! 3 �����ʻ����ǡ�����Y������ʿ��.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:km,0:im-1)  :: zx_AvrY_zyx
      !(out) Yʿ�Ѥ��줿 2 ����ZX�ʻ����ǡ���

      zx_AvrY_zyx = zx_IntY_zyx(zyx)/sum(y_Y_Weight)

    end function zx_AvrY_zyx

    function yx_AvrZ_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ�����Z����ʿ��.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:jm-1,0:im-1)  :: yx_AvrZ_zyx          
      !(out) Zʿ�Ѥ��줿 2 ����YX(��ʿ)�ʻ����ǡ���

      yx_AvrZ_zyx = yx_IntZ_zyx(zyx)/sum(z_Z_Weight)

    end function yx_AvrZ_zyx

    function x_AvrZY_zyx(zyx)  ! ZY��ʬ
      !
      ! 3 �����ʻ����ǡ�����ZYʿ��
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8), dimension(0:im-1)     :: x_AvrZY_zyx
      !(out) ZYʿ�Ѥ��줿 1 ����X�ʻ����ǡ���

      x_AvrZY_zyx = x_IntZY_zyx(zyx) &
                   /( sum(y_Y_Weight)*sum(z_Z_Weight) )

    end function x_AvrZY_zyx

    function y_AvrZX_zyx(zyx)  ! ZX��ʬ
      !
      ! 3 �����ʻ����ǡ�����ZXʿ��.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:jm-1)       :: y_AvrZX_zyx
      !(out) ZXʿ�Ѥ��줿 1 ����Y�ʻ����ǡ���

      y_AvrZX_zyx = y_IntZX_zyx(zyx) &
                 /(sum(x_X_Weight)*sum(z_Z_Weight))

    end function y_AvrZX_zyx

    function z_AvrYX_zyx(zyx)  ! YX(��ʿ)��ʬ
      !
      ! 3 �����ʻ����ǡ�����YX(��ʿ)��ʬ
      ! 
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 �����ʻ����ǡ���

      real(8), dimension(0:km)     :: z_AvrYX_zyx
      !(out) YX(��ʿ)ʿ�Ѥ��줿 1 ����Z�ʻ����ǡ���

      z_AvrYX_zyx = z_IntYX_zyx(zyx) &
                 /(sum(x_X_Weight)*sum(y_Y_Weight))

    end function z_AvrYX_zyx

    function AvrZYX_zyx(zyx) ! ZYX(���ΰ�)��ʬ
      !
      ! 3 �����ʻ����ǡ�����ZYX(���ΰ�)��ʬ
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 ����ZYX�ʻ����ǡ���

      real(8)                     :: AvrZYX_zyx
      !(out) ���ΰ�ʿ����

      AvrZYX_zyx = IntZYX_zyx(zyx) &
            /(sum(x_X_Weight)*sum(y_Y_Weight) * sum(z_Z_Weight))

    end function AvrZYX_zyx

    !----(���ϥǡ��� zy)---
    function z_AvrY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ�����Y����ʿ��.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_AvrY_zy
      !(out) Yʿ�Ѥ��줿 1 ����Z�ʻ����ǡ���

      z_AvrY_zy = z_IntY_zy(zy)/sum(y_Y_Weight)

    end function z_AvrY_zy

    function y_AvrZ_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ�����Z����ʿ��.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:jm-1)  :: y_AvrZ_zy
      !(out) Zʿ�Ѥ��줿 1 ����Y�ʻ����ǡ���

      y_AvrZ_zy = y_IntZ_zy(zy)/sum(z_Z_Weight)

    end function y_AvrZ_zy

    function AvrZY_zy(zy)  ! ZYʿ��
      !
      ! 2 ����(ZY)�ʻ����ǡ�����ZYʿ��
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 ����ZY(�Ҹ���)�ʻ����ǡ���

      real(8)                   :: AvrZY_zy
      !(out) ʿ����

      AvrZY_zy = IntZY_zy(zy)/(sum(y_Y_Weight)*sum(z_Z_Weight))

    end function AvrZY_zy

    !----(���ϥǡ��� zx)---
    function z_AvrX_zx(zx)  ! X��ʬ
      !
      ! 2 ����(ZX)�ʻ����ǡ�����X����ʿ��.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:km)  :: z_AvrX_zx 
      !(out) Xʿ�Ѥ��줿 1 ����Z�ʻ����ǡ���

      z_AvrX_zx = z_IntX_zx(zx)/sum(x_X_Weight)

    end function z_AvrX_zx

    function x_AvrZ_zx(zx)  ! Zʿ��
      !
      ! 2 ����(ZX)�ʻ����ǡ�����Z����ʿ��.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 ����ZY�ʻ����ǡ���

      real(8), dimension(0:im-1)  :: x_AvrZ_zx
      !(out) Zʿ�Ѥ��줿 1 ����X�ʻ����ǡ���

      x_AvrZ_zx = x_IntZ_zx(zx)/sum(z_Z_Weight)

    end function x_AvrZ_zx

    function AvrZX_zx(zx)  ! ZX��ʬ
      !
      ! 2 ����(ZX)�ʻ����ǡ�����ZXʿ��
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx    
      ! (in) 2 �����ʻ����ǡ���
      real(8)                                 :: AvrZX_zx      
      ! ʿ����

      AvrZX_zx = IntZX_zx(zx)/(sum(x_X_Weight)*sum(z_Z_Weight))

    end function AvrZX_zx

    !----(���ϥǡ��� z)---
    function AvrZ_z(z)
      !
      ! 1 ����(Z)�ʻ����ǡ�����Z����ʿ��.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 ����Z�ʻ����ǡ���
      real(8)                              :: AvrZ_z
      !(out) ʿ����

      AvrZ_z = IntZ_z(z)/sum(z_Z_Weight)

    end function AvrZ_z

!!$  !--------------- �ݥ�����/�ȥ������ǥ�����ʬ -----------------
!!$
!!$    function wt_KxRGrad_wt(wt)
!!$      !
!!$      ! ���ϥ��ڥ��ȥ�ǡ�����X��ʬ k��r���� = ��/�ߦˤ���Ѥ���.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_KxRGrad_wt
!!$      !(out) X��ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���
!!$
!!$      wt_KxRGrad_wt =  wa_Dlon_wa(wt)
!!$
!!$    end function wt_KxRGrad_wt
!!$
!!$    function zyx_KGrad_wt(wt)    ! k���� = cos��/r ��/�ߦ� + sin�բ�/��r
!!$      !
!!$      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻҥǡ����˼�������ʬ 
!!$      !
!!$      !    k���� = cos��/r ��/�ߦ� + sin�բ�/��r 
!!$      !
!!$      ! ����Ѥ������ʻҥǡ������֤����. 
!!$      ! �����ǥ٥��ȥ� k �ϵ���濴�����̶˸�����ñ�̥٥��ȥ�Ǥ���.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
!!$
!!$      real(8), dimension(0:km,0:jm-1,0:im-1)                     :: yzx_KGrad_wt
!!$      !(out) ��������ʬ����Ѥ��줿 2 �������ڥ��ȥ�ǡ���
!!$
!!$      xzy_KGrad_wt =  cos(xyz_Y)*xyz_GradY_wt(wt) &
!!$                    + sin(xyz_Y)*xyz_wt(wt_Drad_wt(wt))
!!$
!!$    end function xyz_KGrad_wt
!!$
!!$    function wt_L2_wt(wt)
!!$      !
!!$      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻��(=-��ʿ��ץ饷����)����Ѥ���.
!!$      !
!!$      ! L^2 �黻�Ҥ�ñ�̵��̾�ο�ʿ��ץ饷����ε����ˤ�����. 
!!$      !  ���ϥ��ڥ��ȥ�� �������б�����ʻ����ǡ����˱黻�� 
!!$      !
!!$      !     L^2 = -1/cos^2�ա���^2/�ߦ�^2 - 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
!!$      !
!!$      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_L2_wt
!!$      !(out) L^2 �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���
!!$
!!$      wt_L2_wt = -wa_Lapla_wa(wt)
!!$
!!$    end function wt_L2_wt
!!$
!!$    function wt_L2Inv_wt(wt)
!!$      !
!!$      ! ���ϥ��ڥ��ȥ�ǡ����� L^2 �黻�Ҥεձ黻(-�տ�ʿ��ץ饷����)��
!!$      ! ���Ѥ���.
!!$      !
!!$      ! ���ڥ��ȥ�ǡ����� L^2 �黻�Ҥ���Ѥ�����ؿ� wt_L2_wt �εշ׻���
!!$      ! �Ԥ��ؿ��Ǥ���.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_L2Inv_wt
!!$      !(out) L^2 �黻�Ҥεձ黻����Ѥ��줿 2 �������ڥ��ȥ�ǡ���
!!$
!!$      wt_L2Inv_wt = -wa_LaplaInv_wa(wt)
!!$
!!$    end function wt_L2Inv_wt
!!$
!!$    function wt_QOperator_wt(wt)
!!$      !
!!$      ! ���ϥ��ڥ��ȥ�ǡ������б�����ʻ����ǡ����˱黻�� 
!!$      !
!!$      !    Q=(k����-1/2(L2 k����+ k����L2)) 
!!$      !
!!$      ! ����Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ����֤����.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 ��������Ĵ��ȡ�������ӥ����ե��ڥ��ȥ�ǡ���
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_QOperator_wt
!!$      !(out) Q �黻�Ҥ���Ѥ��줿 2 �������ڥ��ȥ�ǡ���
!!$
!!$      wt_QOperator_wt = &
!!$             wt_xyz(xyz_KGrad_wt(wt) - xyz_KGrad_wt(wt_L2_wt(wt))/2) &
!!$           - wt_L2_wt(wt_xyz(xyz_KGrad_wt(wt)))/2
!!$
!!$    end function wt_QOperator_wt

    function tee_ZRot_zyx_zyx(zyx_VX,zyx_VY)  ! z��(����v)
      !
      ! �٥��ȥ�α��٤�Z�٥��ȥ������ z��(����v) ��׻�����.
      !
      ! �� 1, 2 ����(v[x], v[y])�����줾��٥��ȥ��X��ʬ, Y��ʬ��ɽ��.
      !
      !    z��(����v) = ��v[y]/��x - ��(v[x])/��y
      !
      ! �Υ��ڥ��ȥ� �ǡ������֤����.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) �٥��ȥ��X��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) �٥��ȥ��Y��ʬ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_ZRot_zyx_zyx
      !(out) �٥��ȥ�α��٤�Z�٥��ȥ������

      tee_ZRot_zyx_zyx = tee_DX_tee(tee_zyx(zyx_VY)) &
                       - tee_DY_tee(tee_zyx(zyx_VX))
      
    end function tee_ZRot_zyx_zyx

    function tee_ZRotRot_zyx_zyx_zyx(zyx_VX,zyx_VY,zyx_VZ) 
      ! 
      ! �٥��ȥ� v ���Ф��� z��(���ߢ���v) ��׻�����.
      !
      ! �� 1, 2, 3 ����(v[x], v[y], v[z])�����줾��٥��ȥ��X��ʬ, 
      ! Y��ʬ, Z��ʬ��ɽ��. 
      !
      !    z��(���ߢ���v)  = ��/��z (��v[x]/��x + ��(v[y])/��y ) ) 
      !                    - ��_H^2 v[z]
      !
      ! �Υ��ڥ��ȥ�ǡ������֤����.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) �٥��ȥ��X��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) �٥��ȥ��Y��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VZ
      !(in) �٥��ȥ��Z��ʬ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_ZRotRot_zyx_zyx_zyx
      !(out) �٥��ȥ� v �� z��(���ߢ���v) 

      tee_ZRotRot_zyx_zyx_zyx = &
               tee_DZ_tee(   tee_DX_tee(tee_zyx(zyx_VX))   &
                           + tee_DY_tee(tee_zyx(zyx_VY)) ) &
             - tee_LaplaH_tee(tee_zyx(zyx_VZ))

    end function tee_ZRotRot_zyx_zyx_zyx

    subroutine tee_Potential2Vector(&
         zyx_VX,zyx_VY,zyx_VZ,tee_Torvel,tee_Polvel)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��z) + ��x��x(��z) 
      !
      ! �γ���ʬ��׻�����
      !
      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VX
      !(out) �٥��ȥ���X��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VY
      !(out) �٥��ȥ���Y��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VZ
      !(out) �٥��ȥ���Z��ʬ

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Torvel
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Polvel
      !(in) �ݥ�����ݥƥ󥷥��

      zyx_VX = zyx_tee(   tee_DY_tee(tee_Torvel) &
                        + tee_DX_tee(tee_DZ_tee(tee_Polvel))  )
      zyx_VY = zyx_tee( - tee_DX_tee(tee_Torvel) &
                        + tee_DY_tee(tee_DZ_tee(tee_Polvel))  )
      zyx_VZ = -zyx_tee(tee_LaplaH_tee(tee_Polvel))

    end subroutine tee_Potential2Vector

    subroutine tee_Potential2Rotation(&
       zyx_RotVX,zyx_RotVY,zyx_RotVZ,tee_Torvel,tee_Polvel)
      !
      ! �ȥ�����ݥ�����ݥƥ󥷥�릷,����ɽ�������ȯ���٥��ȥ��
      !
      !     v = ��x(��z) + ��x��x(��z) 
      !
      ! ���Ф���, ���β�ž
      !
      !     ��xv = ��x��x(��z) + ��x��x��x(��z) = ��x��x(��z) - ��x((��^2��)z)
      !
      ! ��׻�����. 
      
      ! �٥��ȥ��β�ž
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVX
      !(out) ��ž��X��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVY
      !(out) ��ž��Y��ʬ

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVZ
      !(out) ��ž��Z��ʬ

      ! ���ϥ٥��ȥ���ɽ���ݥƥ󥷥��
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Torvel
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Polvel
      !(in) �ݥ�����ݥƥ󥷥��

      call tee_Potential2Vector( &
           zyx_RotVX,zyx_RotVY,zyx_RotVZ, &
           -tee_Lapla_tee(tee_Polvel), tee_Torvel)

    end subroutine tee_Potential2Rotation

  !--------------- ��ַ׻� -----------------
    function Interpolate_tee(tee_data,x,y,z)
      !
      ! (x,y,z) �ˤ�����ؿ��ͤ�
      ! ���Υ��ڥ��ȥ뷸�� tee_data ������ַ׻�����
      !
      real(8), intent(IN) :: tee_data(0:nm,-mm:mm,-lm:lm)  ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN) :: x                             ! ��֤������(X)
      real(8), intent(IN) :: y                             ! ��֤������(Y)
      real(8), intent(IN) :: z                             ! ��֤������(Z)
      real(8) :: Interpolate_tee                           ! ��֤�����
      
      Interpolate_tee = &
           Interpolate_ee(ee_e2(a_Interpolate_at(e2a_aee(tee_data),z)),x,y)

    end function Interpolate_tee

  !--------------- �ݥ�����/�ȥ������ǥ��ѥ��ڥ��ȥ���� ----------------

    function zee_ToroidalEnergySpectrum_tee(tee_TORPOT)
      !
      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
      ! ��ʿ X �ȿ� l, Y  �ȿ� m �γ���ʬ��׻�����
      !
      !  * X �ȿ� l, Y �ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(l,m,z)�����ʿ X �ȿ� l, Y �ȿ� m ��ʬ�Υȥ����륨�ͥ륮��
      !    ���ڥ��ȥ��  (1/2)(l**2+m**2)|��(l,m,z)|^2  �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�Z��ʬ������Τ��ΰ���Ǥ�
      !    ��ʿʿ�ѥ��ͥ륮����������.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_TORPOT
      !(in) �ȥ�����ݥƥ󥷥��

      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_ToroidalEnergySpectrum_tee
      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ

      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_Work
      integer :: l, m

      zee_Work = zee_tee(tee_Torpot)
      do l=-lm,lm
         do m=-mm,mm
            zee_ToroidalEnergySpectrum_tee(:,m,l) &
              = 0.5 * ((2*PI*l/xl)**2+(2*PI*m/yl)**2) &
              * ( zee_Work(:,m,l)**2 + zee_Work(:,-m,-l)**2 )
         enddo
      enddo

    end function zee_ToroidalEnergySpectrum_tee

!!$    function nz_ToroidalEnergySpectrum_wt(wt_TORPOT)
!!$      !
!!$      ! �ȥ�����ݥƥ󥷥�뤫��, �ȥ����륨�ͥ륮����
!!$      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����.
!!$      !
!!$      !  * ���ȿ� n, �Ӿ��ȿ� m �Υȥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
!!$      !    ��(n,m,r)�������ȿ� n ��ʬ�Υȥ����륨�ͥ륮�����ڥ��ȥ��
!!$      !    ��[m=-n]^n(1/2)n(n+1)4��r^2��(n,m,r)^2 �ȷ׻������.
!!$      !
!!$      ! * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�Z��ʬ�������(r^2�νŤ�̵��)
!!$      !    �������Ǥ������ͥ륮����������.
!!$      !
!!$      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: wt_TORPOT
!!$      !(in) �ȥ�����ݥƥ󥷥��
!!$
!!$      real(8), dimension(0:nm,0:km) :: nz_ToroidalEnergySpectrum_wt 
!!$      !(out) ���ͥ륮�����ڥ��ȥ�ȥ�������ʬ
!!$
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA   ! ����ΰ�
!!$      integer :: n, m
!!$
!!$      wz_DATA = wz_wt(wt_TORPOT)
!!$      do n=0,nm
!!$         nz_ToroidalEnergySpectrum_wt(n,:) &
!!$              = 0.5 * n*(n+1)* (4*pi) * z_Z**2 &
!!$                * sum(wz_Data(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
!!$      enddo
!!$
!!$    end function nz_ToroidalEnergySpectrum_wt

    function zee_PoloidalEnergySpectrum_tee(tee_POLPOT)
      !
      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
      ! ��ʿ X �ȿ� l, Y �ȿ� m �γ���ʬ��׻�����.
      !
      !  * X �ȿ� l, Y �ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
      !    ��(l,m,z)���� X �ȿ� l, Y �ȿ� m ��ʬ�Υݥ����륨�ͥ륮��
      !    ���ڥ��ȥ�� 
      !
      !      (1/2)(l**2+m**2){[d��(n,m,z)/dz]^2 + (l**2+m**2)��(n,m,z)^2} 
      !
      !    �ȷ׻������.
      !
      !  * ���ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�Z��ʬ�������
      !    ����ʿʿ�ѥ��ͥ륮����������.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_POLPOT
      !(in) �ݥ�����ݥƥ󥷥��

      real(8), dimension(0:km,-nm:nm,-lm:lm) :: zee_PoloidalEnergySpectrum_tee
      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ


      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_Data   ! ����ΰ�
      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_DData  ! ����ΰ�
      integer :: l, m

      zee_Data = zee_tee(tee_POLPOT)
      zee_DData = zee_tee(tee_DZ_tee(tee_POLPOT))

      do l=-lm,lm
         do m=-mm,mm
            zee_PoloidalEnergySpectrum_tee(:,m,l) =                   &
                 + 0.5* ((2*pi*l/xl)**2+(2*pi*m/yl)**2)               &
                 *(   zee_DData(:,m,l)**2 + zee_DData(:,-m,-l)**2     &
                    + ((2*pi*l/xl)**2+(2*pi*m/yl)**2)                 &
                         *( zee_Data(:,m,l)**2 + zee_Data(:,-m,-l)**2)) 
         enddo
      enddo

    end function zee_PoloidalEnergySpectrum_tee

!!$    function nz_PoloidalEnergySpectrum_wt(wt_POLPOT)
!!$      !
!!$      ! �ݥ�����ݥƥ󥷥�뤫��, �ݥ����륨�ͥ륮����
!!$      ! ����Ĵ��ȡ�����ȿ��γ���ʬ��׻�����
!!$      !
!!$      !  * ���ȿ� n, �Ӿ��ȿ� m �Υݥ�����ݥƥ󥷥��Υ��ڥ��ȥ���ʬ
!!$      !    ��(n,m,r)�������ȿ� n ��ʬ�Υݥ����륨�ͥ륮�����ڥ��ȥ��
!!$      !
!!$      !      ��[m=-n]^n ((1/2)n(n+1)4��r^2{[d(r��(n,m,r))/dr]^2 
!!$      !                 + n(n+1)��(n,m,r)^2} 
!!$      !
!!$      !    �ȷ׻������.
!!$      !
!!$      !  * ���Ƥ����ȿ����Ф��ƤΥ��ͥ륮�����ڥ��ȥ���ʬ���¤�Z��ʬ�������
!!$      !    (r^2�νŤ�̵��)�������Ǥ������ͥ륮����������.
!!$      !
!!$      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: wt_POLPOT
!!$      !(in) �ݥ�����ݥƥ󥷥��
!!$
!!$      real(8), dimension(0:nm,0:km) :: nz_PoloidalEnergySpectrum_wt
!!$      !(out) ���ͥ륮�����ڥ��ȥ�ݥ�������ʬ
!!$
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA1   ! ����ΰ�
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA2   ! ����ΰ�
!!$      integer :: n, m
!!$
!!$      wz_Data1 = wz_wt(wt_POLPOT)
!!$      wz_Data2 = wz_Z*wz_wt(wt_DZ_wt(wt_POLPOT)) &    ! d(r��)/dr
!!$               + wz_wt(wt_POLPOT)                         ! = rd��/dr+��
!!$
!!$      do n=0,nm
!!$         nz_PoloidalEnergySpectrum_wt(n,:) = &
!!$              + 0.5* n*(n+1)* (4*pi) &
!!$              *( sum(wz_Data2(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)  &
!!$                + n*(n+1)*sum(wz_Data1(l_nm(n,(/(m,m=-n,n)/)),:)**2,1) )
!!$      enddo
!!$
!!$    end function nz_PoloidalEnergySpectrum_wt
!!$
!!$
  !--------------- ���������� -----------------

    subroutine tee_BoundariesTau(tee,values,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! Chebyshev ���֤Ǥζ������Ŭ��(����ˡ)
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����
      ! ������ˡ��ȤäƤ���(����ˡ).
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)      :: tee
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(2,-mm:mm,-lm:lm), intent(in), optional :: values
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !     ��ά������/���� 0 �Ȥʤ�. 

      character(len=2), intent(in), optional             :: cond
              !(in) �������. ��ά���� 'DD'
              !        DD : ξü�ǥ��ꥯ����
              !        DN : ��ü�ǥ��ꥯ��, ��ü�Υ��ޥ���
              !        ND : ��ü�Υ��ޥ�, ��ü�ǥ��ꥯ����
              !        NN : ξü�Υ��ޥ���

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)         :: e2t

      real(8), dimension((2*lm+1)*(2*mm+1),2)            :: e22_values

      e2t = e2a_aee(tee)

      if (present(values)) then
         e22_values = e2a_aee(values)
      endif

      if (.not. present(cond)) then
         if (present(values)) then
            call at_BoundariesTau_DD(e2t,e22_values)
         else
            call at_BoundariesTau_DD(e2t)
         endif
         goto 99
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesTau_NN(e2t,e22_values)
         else
            call at_BoundariesTau_NN(e2t)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesTau_DN(e2t,e22_values)
         else
            call at_BoundariesTau_DN(e2t)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesTau_ND(e2t,e22_values)
         else
            call at_BoundariesTau_ND(e2t)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesTau_DD(e2t,e22_values)
         else
            call at_BoundariesTau_DD(e2t)
         endif
      case default
         call MessageNotify('E','tee_BoundariesTau','B.C. not supported')
      end select

99    tee = aee_e2a(e2t)

    end subroutine tee_BoundariesTau

    subroutine tee_BoundariesGrid(tee,values,cond)
      !
      ! ���ڥ��ȥ�ǡ����˥ǥ��ꥯ�졦�Υ��ޥ󶭳�����Ŭ�Ѥ���
      ! �¶��֤Ǥζ������Ŭ��
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! tee_Initial �ˤ����ꤹ������ӥ����������ȿ�(nm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)      :: tee
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(2,-mm:mm,-lm:lm), intent(in), optional :: values
              !(in) �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              !     ��ά������/���� 0 �Ȥʤ�. 

      character(len=2), intent(in), optional             :: cond
              !(in) �������. ��ά���� 'DD'
              !        DD : ξü�ǥ��ꥯ����
              !        DN : ��ü�ǥ��ꥯ��, ��ü�Υ��ޥ���
              !        ND : ��ü�Υ��ޥ�, ��ü�ǥ��ꥯ����
              !        NN : ξü�Υ��ޥ���

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)         :: e2t

      real(8), dimension((2*lm+1)*(2*mm+1),2)            :: e22_values

      e2t = e2a_aee(tee)

      if (present(values)) then
         e22_values = e2a_aee(values)
      endif

      if (.not. present(cond)) then
         if (present(values)) then
            call at_boundariesGrid_DD(e2t,e22_values)
         else
            call at_boundariesGrid_DD(e2t)
         endif
         goto 199
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesGrid_NN(e2t,e22_values)
         else
            call at_BoundariesGrid_NN(e2t)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesGrid_DN(e2t,e22_values)
         else
            call at_BoundariesGrid_DN(e2t)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesGrid_ND(e2t,e22_values)
         else
            call at_BoundariesGrid_ND(e2t)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesGrid_DD(e2t,e22_values)
         else
            call at_BoundariesGrid_DD(e2t)
         endif
      case default
         call MessageNotify('E','tee_BoundariesGrid','B.C. not supported')
      end select

199   tee = aee_e2a(e2t)

    end subroutine tee_BoundariesGrid

    subroutine tee_TorBoundariesTau(tee_TOR,cond,new)
      
      ! �ȥ�����®�٥ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). �ȥ�����ݥƥ󥷥��ζ�������
      !
      !     �� = 0 at boundaries           (Ǵ����) 
      !     �ߦ�/��z = 0 at boundaries    (���Ϥʤ����)
      ! 
      ! �Ǥ��뤫�� tee_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: rigid1, rigid2   ! �������
      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','tee_TorBoundariesTau','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work= 0.0D0 ; e2t_work(:,n)= 1.0D0
            alu(:,:,n) = e2t_work

            ! Ǵ����
            ! �ϳ�Ū���Ǵ���� 
            if ( rigid1 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work = az_at(at_Dz_at(e2t_work))
            endif
            alu(:,nm-1,n) = e2z_work(:,0)

            ! �ϳ�Ū���Ǵ���� 
            if ( rigid2 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work = az_at(at_Dz_at(e2t_work))
            endif
            alu(:,nm,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)
         call MessageNotify('M','tee_TorBoundariesTau',&
                            'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_Tor)

      e2t_work(:,nm-1) = 0.0D0
      e2t_work(:,nm)   = 0.0D0

      tee_Tor = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_TorBoundariesTau

    subroutine tee_TorBoundariesGrid(tee_TOR,cond,new)
      !
      ! �ȥ�����®�٥ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! tee_Initial �ˤ����ꤹ������ӥ����������ȿ�(nm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! �ȥ�����®�٥ݥƥ󥷥��ζ�������
      !
      !     �� = 0 at boundaries           (Ǵ����) 
      !     �ߦ�/��z = 0 at boundaries    (���Ϥʤ����)
      ! 
      ! �Ǥ���Τ� tee_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: rigid1, rigid2   ! �������
      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','tee_TorBoundariesGrid','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_TorBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work          ! �����ΰ���ͤ��Τޤ�.


            ! Ǵ����
            ! �ϳ�Ū���Ǵ���� 
            if ( rigid1 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work=az_at(at_Dz_at(e2t_work))
            endif
            alu(:,0,n) = e2z_work(:,0)

            ! �ϳ�Ū���Ǵ���� 
            if ( rigid2 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work=az_at(at_Dz_at(e2t_work))
            endif
            alu(:,km,n)   = e2z_work(:,km)

         enddo
         call ludecomp(alu,kp)
         call MessageNotify('M','TorBoundariesGrid',&
                            'Matrix to apply  b.c. newly produced.')
      endif
      
      e2z_work = az_at(e2a_aee(tee_Tor))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_TOR = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_TorBoundariesGrid

    function zee_LaplaPol2Pol_zee(zee,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      !
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      ! ���δؿ����Ѥ��뤿��ˤ� wt_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !   ��^2�� = f
      !     �� = const. at boundaries.
      !     �ߦ�/��z = 0 at boundaries           (Ǵ����) 
      !     or ��^2��/��z^2 = 0 at boundaries    (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm),intent(in)  :: zee
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_LaplaPol2Pol_zee
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:km)  :: e2z_work
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','zee_laplapol2pol_zee','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','zee_LaplaPol2Pol_zee', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:km),kp((2*lm+1)*(2*mm+1),0:km))

         do k=0,km
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,k) &
                 = e2a_aee(zee_tee(tee_lapla_tee(tee_zee(aee_e2a(e2z_work)))))
         enddo

         do k=0,km
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0

            ! ��ư��Ū���. ή���϶����ǰ���
            alu(:,0,k)   = e2z_work(:,0)
            alu(:,km,k)  = e2z_work(:,km)

            ! �ϳ�Ū���Ǵ���� 
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0
            if ( rigid1 ) then
               e2z_work=az_at(at_Dz_at(at_az(e2z_work)))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(at_az(e2z_work))))
            endif
            alu(:,1,k) = e2z_work(:,0)

            ! �ϳ�Ū���Ǵ���� 
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0
            if ( rigid2 ) then
               e2z_work=az_at(at_Dz_at(at_az(e2z_work)))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(at_az(e2z_work))))
            endif
            alu(:,km-1,k) = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','zee_LaplaPol2Pol_zee',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work        = e2a_aee(zee)
      e2z_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      e2z_work(:,km-1) = 0.0D0               ! �ϳ�Ū���
      e2z_work(:,0)    = 0.0D0               ! ��ư��Ū���
      e2z_work(:,km)   = 0.0D0               ! ��ư��Ū��� 

      e2z_work = lusolve(alu,kp,e2z_work)

      zee_laplapol2pol_zee = aee_e2a(e2z_work)

    end function zee_LaplaPol2Pol_zee

    function tee_LaplaPol2PolGrid_tee(tee,cond,new)
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵��^2������׻�����.
      ! �����ӥ����ճʻ������֤Ƕ�������Ŭ�Ѥ��Ƥ���. 
      !
      ! ���δؿ����Ѥ��뤿��ˤ� tee_Initial �ˤ����ꤹ��
      ! �����ӥ����������ȿ�(lm)�ȱ�ľ�ʻ�����(km)��������
      ! ���Ƥ���ɬ�פ�����. 
      !
      ! ®�٥ݥ�����ݥƥ󥷥�릵�� f = ��^2���������뼰��
      !
      !    ��^2�� = f
      !      �� = const. at boundaries.
      !      �ߦ�/��z = 0 at boundaries          (Ǵ����) 
      !      or ��^2��/��z^2 = 0 at boundaries   (���Ϥʤ����)
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      ! �ǽ�Ū�˥����ӥ����շ����β��ߤ������ˤ�, tee_LaplaPol2Pol_tee ��
      ! ��٤ƥ����ӥ����� -- �ʻ����Ѵ��� 1 ��ʬ���ʤ��ƺѤ�.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(in) :: tee
              !(in) ���Ϣ�^2��ʬ��

      real(8), dimension(0:nm,-mm:mm,-lm:lm)      :: tee_LaplaPol2PolGrid_tee
              !(out) ���ϥݥ�����ݥƥ󥷥��ʬ��

      character(len=2), intent(in), optional  :: cond
              !(in) ������凉���å�. ��ά���� 'RR'
              !     RR    : ξüǴ����
              !     RF    : ��üǴ��, ��ü���Ϥʤ����
              !     FR    : ��ü���Ϥʤ�, ��üǴ����
              !     FF    : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)  :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km)  :: e2z_work
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','tee_LaplaPol2PolGrid_tee','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_LaplaPol2PolGrid_tee', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0

            ! �ƿ�ʿ�ȿ��˴ؤ�����Ω�μ�
            alu(:,:,n) = e2a_aee(zee_tee(tee_lapla_tee(aee_e2a(e2t_work))))
         enddo

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            e2z_work = az_at(e2t_work)

            ! ��ư��Ū���. ή���϶����ǰ���
            alu(:,0,n)   = e2z_work(:,0)
            alu(:,km,n)  = e2z_work(:,km)

            ! �ϳ�Ū���Ǵ���� 
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            if ( rigid1 ) then
               e2z_work=az_at(at_Dz_at(e2t_work))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(e2t_work)))
            endif
            alu(:,1,n) = e2z_work(:,0)

            ! �ϳ�Ū���Ǵ���� 
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            if ( rigid2 ) then
               e2z_work=az_at(at_Dz_at(e2t_work))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(e2t_work)))
            endif
            alu(:,km-1,n) = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_LaplaPol2PolGrid_tee',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work         = az_at(e2a_aee(tee))
      e2z_work(:,1)    = 0.0D0               ! �ϳ�Ū���
      e2z_work(:,km-1) = 0.0D0               ! �ϳ�Ū���
      e2z_work(:,0)    = 0.0D0               ! ��ư��Ū���
      e2z_work(:,km)   = 0.0D0               ! ��ư��Ū��� 

      tee_LaplaPol2PolGrid_tee = aee_e2a(lusolve(alu,kp,e2z_work))

    end function tee_LaplaPol2PolGrid_tee

    subroutine tee_TormagBoundariesTau(tee_TOR,new)
      
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    tee_psi = 0   at the outer boundary
      ! ��¦
      !    tee_psi = 0   at the inner boundary
      ! 
      ! �Ǥ��뤫�� tee_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���.
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work= 0.0D0 ; e2t_work(:,n)= 1.0D0
            alu(:,:,n) = e2t_work

            ! ���ŵ���Ƴ��
            e2z_work = az_at(e2t_work)
            alu(:,nm-1,n) = e2z_work(:,0)
            alu(:,nm,n)   = e2z_work(:,km)

         enddo

         call ludecomp(alu,kp)
         call MessageNotify('M','tee_TormagBoundariesTau',&
                            'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_Tor)

      e2t_work(:,nm-1) = 0.0D0
      e2t_work(:,nm)   = 0.0D0


      tee_Tor = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_TormagBoundariesTau

    subroutine tee_TormagBoundariesGrid(tee_TOR,new)
      !
      ! ����ȥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��.
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! tee_Initial �ˤ����ꤹ������ӥ����������ȿ�(nm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ȥ�����ݥƥ󥷥��ζ�������
      !
      ! ��¦
      !    tee_psi = 0   at the outer boundary
      ! ��¦
      !    tee_psi = 0   at the inner boundary
      ! 
      ! �Ǥ���Τ� tee_Boundaries ���б���ǽ����, ����Τ������Ӻ������Ƥ���
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work          ! �����ΰ���ͤ��Τޤ�.

            ! ���ŵ���Ƴ��
            alu(:,0,n)  = e2z_work(:,0)
            alu(:,km,n) = e2z_work(:,km)

         enddo
         call ludecomp(alu,kp)
         call MessageNotify('M','TormagBoundariesGrid',&
                            'Matrix to apply  b.c. newly produced.')
      endif
      
      e2z_work = az_at(e2a_aee(tee_Tor))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_TOR = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_TormagBoundariesGrid

    subroutine tee_PolmagBoundariesTau(tee_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! Chebyshev ���֤Ǥζ������Ŭ��
      !
      ! �����ӥ����ն��֤ˤ����ƶ��������������٤��⼡�η�����������ˡ��
      ! �ȤäƤ���(����ˡ). ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ�
      ! �б����Ƥ���. ���ξ��, ����ݥ�����ݥƥ󥷥��γƿ�ʿ���ڥ��ȥ�
      ! ��ʬ h �ˤ������ƶ�����郎Ϳ�����,
      !
      !  * ��¦���� : dh/dz + K h = 0
      !  * ��¦���� : dh/dz - K h = 0
      !
      ! �Ǥ���. ������ K=sqrt(l^2+m^2) �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_kh

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l, m, n, e2index
      save    :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)

         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            alu(:,:,n) = e2t_work(:,:)    ! �����ΰ��Ʊ��
         enddo

         ! ���ŵ���Ƴ��
         do m=-mm,mm
            do l=-lm,lm
               e2index = (2*mm+1)*(l+lm) + (m+mm+1)
               e2t_kh(e2index,:) = sqrt(dble(l**2+m**2))
            enddo
         enddo
         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            e2z_work = az_at(at_Dz_at(e2t_work) + e2t_kh * e2t_work)
            alu(:,lm-1,n) = e2z_work(:,0)

            e2z_work = az_at(at_Dz_at(e2t_work) - e2t_kh * e2t_work)
            alu(:,lm,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_PolmagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_POL)
      e2t_work(:,lm-1) = 0.0D0
      e2t_work(:,lm)   = 0.0D0
      tee_POL = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_PolmagBoundariesTau

    subroutine tee_PolmagBoundariesGrid(tee_POL,new)
      !
      ! ����ݥ�����ݥƥ󥷥����Ф��ƶ�������Ŭ�Ѥ���.
      ! ��ľ�¶��֤Ǥζ������Ŭ��. 
      !
      ! ��ľ�³ʻ������֤ˤ����������ΰ���ͤȶ��������������褦��
      ! ����ݤ��Ƥ���(����ˡ). ���Υ롼������Ѥ��뤿��ˤ� 
      ! tee_Initial �ˤ����ꤹ������ӥ����������ȿ�(nm)�ȱ�ľ�ʻ�����(km)��
      ! ���������Ƥ���ɬ�פ�����. 
      !
      ! ���ߤΤȤ�����ʪ�������ŵ���Ƴ�Τξ��Τ��б����Ƥ���. 
      ! ���ξ��, ����ݥ�����ݥƥ󥷥��γƿ�ʿ���ڥ��ȥ���ʬ h ��
      ! �������ƶ�����郎Ϳ�����,
      !
      !  * ��¦���� : dh/dz + K h = 0
      !  * ��¦���� : dh/dz - K h = 0
      !
      ! �Ǥ���. ������ K=sqrt(l^2+m^2) �� h �ο�ʿ���ȿ��Ǥ���. 
      !
      ! �ǽ�˸ƤФ��Ȥ��ϥ��ץ���ʥ���� new �˴ط��ʤ��������ꤵ���.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_POL
              !(inout) ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_kh

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, m, n, e2index
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_PolMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) )  deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work    ! �����ΰ��Ʊ��
         enddo

         ! ���ŵ���Ƴ��
         do m=-mm,mm
            do l=-lm,lm
               e2index = (2*mm+1)*(l+lm) + (m+mm+1)
               e2t_kh(e2index,:) = sqrt(dble(l**2+m**2))
            enddo
         enddo
         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            e2z_work = az_at(at_Dz_at(e2t_work) + e2t_kh * e2t_work)
            alu(:,0,n) = e2z_work(:,0)

            e2z_work = az_at(at_Dz_at(e2t_work) - e2t_kh * e2t_work)
            alu(:,km,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_PolmagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work = az_at(e2a_aee(tee_POL))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_POL = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_PolmagBoundariesGrid

end module tee_module
