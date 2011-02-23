!--
!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_mpi_module
!
!      spml/eee_module �⥸�塼��ϼ����������β��Ǥ� 3 ��������ΰ��
!      ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ���
!      �󶡤���. 
!
!      ������ ISPACK/P3PACK, P3PACK-MPI �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
!      ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ�
!      ISPACK/P3PACK �Υޥ˥奢��򻲾Ȥ��줿��. 
!
!����  2008/05/21  �ݹ�����  eee_module ����¤
!      2008/06/03  �ݹ�����  ee2f_ZetaFromVor_eef_eef_eef bug fix
!      2008/06/03  �ݹ�����  ESpectralFromZeta bug fix
!      2009/02/23  ��������ʿ  RDoc �ѤΥɥ�����Ȥ���
!      2010/01/07  ��������ʿ  include 'mpif.h' -> use mpi
!
!++
module eee_mpi_module
  !
  != eee_mpi_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eee_mpi_module.f90,v 1.5 2010-02-18 15:28:23 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== ����
  !
  ! spml/eee_module �⥸�塼��ϼ����������β��Ǥ� 3 ��������ΰ��
  ! ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ���
  ! �󶡤���.
  !
  ! ������ ISPACK/P3PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���.
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ�
  ! ISPACK/P3PACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (eef_, zxv_, x_, v_, z_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   eef_ :: ���ڥ��ȥ�ǡ���
  !           (�� 1,2 3 ���������줾�� Z,Y,X �����ȿ�(X �ˤĤ���ʬ������))
  !   ee2f :: 2 �ĤΥ��ڥ��ȥ�ǡ������¤�����
  !   zxv_ :: 3 �����ʻ����ǡ���
  !           (�� 1,2 3 ���������줾�� Z,X,Y �����γʻ���(Y �ˤĤ���ʬ������)
  !   xv_  :: XY ���� 2 �����ʻ����ǡ���, zv_ : YZ ���� 2 �����ʻ����ǡ���
  !   zx_  :: XZ ���� 2 �����ʻ����ǡ���
  !   x_   :: X ���� 1 �����ʻ����ǡ���, v_ : Y ���� 1 �����ʻ����ǡ���
  !   z_   :: Z ���� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Dz, Lapla, LaplaInv)��,
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_eee_eee,_eee,_zyx, _x, _v) ��, �����ѿ��η���
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !    _eef     :: ���ڥ��ȥ�ǡ���
  !    _eef_eef :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !    _ee2f    :: 2 �ĤΥ��ڥ��ȥ�ǡ������¤�����
  !    _zxv     :: 3 �����ʻ����ǡ���
  !    _x       :: X ���� 1 �����ʻ����ǡ���
  !    _v       :: Y ���� 1 �����ʻ����ǡ���.
  !
  !=== �ƥǡ����μ��������
  !
  ! * zxv : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)). 
  !   * im, km �Ϥ��줾�� X, Z ��ɸ�γʻ������Ǥ���, ���֥롼���� 
  !     eee_mpi_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * eee_mpi_Initial �ˤ�äƥץ������ Y ������
  !     ʬ�����֤����ꤵ��, ���ξ��� public �ѿ� js,je,jc �����ꤵ���. 
  !   * js ��ʬ�����֤κǾ��ʻ���, je ������ʻ���, jc ���ʻ������Ǥ���. 
  !   * �� 1 ������ Z ��ɸ�γʻ��������ֹ�, 
  !     �� 2 ������ X ��ɸ�γʻ��������ֹ�, 
  !     �� 3 ������ Y ��ɸ�Ǥ��뤳�Ȥ����(X, Y, Z�ν�ǤϤʤ�).
  !
  ! * eee : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-nm:nm,-mm:mm,2*lc). 
  !   * mm, nm �Ϥ��줾�� Y, Z�����κ����ȿ��Ǥ���, ���֥롼���� 
  !     eee_initial �ˤƤ��餫�������ꤷ�Ƥ���
  !     (X, Y, Z �����ȿ��ν�ǤϤʤ�)���Ȥ����. 
  !   * eee_mpi_Initial �ˤ�äƥץ������ X �����ȿ���
  !     ʬ�����֤����ꤵ��, ���ξ��� public �ѿ� ls,le,lc �����ꤵ���. 
  !   * ls ��ʬ�����֤κǾ��ȿ�, le �������ȿ�, jc ���ȿ��ο��Ǥ���. 
  !     �ºݤγ�Ǽ�Τ������� ls, ls+1,...,le,-le,-le+1,...,-ls �ν�Ǥ���.
  !     ISPACK/P3PACK-MPI �ޥ˥奢�뻲�ȤΤ���.
  !
  ! * ee2f : 2 �ĤΥ��ڥ��ȥ�ǡ����Τʤ��. 
  !   * �ѿ��μ���ȼ����� real(8), dimension(-nm:nm,-mm:mm,2,2*lc). 
  !
  ! * x, v, z : X, Y, Z ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1),
  !     real(8), dimension(js(ip):je(ip)) ����� real(8), dimension(0:km-1).
  !
  ! * eee_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * zxv_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, v_ z_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== �����
  !
  ! eee_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �����ȿ�������
  ! eee_ChangeResolution :: ������������ѹ�
  ! lf_l        :: X �����ȿ��ΰ��־���
  !
  !==== ��ɸ�ѿ�
  !
  ! x_X, v_Y, z_Z    ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, v_Y_Weight,  z_Z_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! zxv_X, zxv_Y, zxv_Z  :: �ʻ����ǡ����� XYZ ��ɸ(X,Y,Z)
  !                         (�ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! zxv_eef   :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! eef_zxv   :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! xyz_zxv   :: ʬ���ʻ����ǡ����ν���
  ! eee2_ee2f :: ʬ�����ڥ��ȥ�ǡ����ν���
  ! ee2f_eee2 :: ���ѥ��ڥ��ȥ�ǡ�����ʬ��
  !
  !==== ��ʬ
  !
  ! eef_Lapla_eef       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! eef_LaplaInv_eef    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! eef_Dx_eef          :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! eef_Dy_eef          :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! eef_Dz_eef          :: ���ڥ��ȥ�ǡ����� Z ��ʬ����Ѥ�����
  !
  !==== �������׻�
  !
  ! ee2f_RotVelxVor_ee2f   :: Euler �����������������׻�����
  ! eef_VorFromZeta_ee2f   :: ���� 2 ��ʬ(��_1, ��_2)�鱲�٤� 1 ��ʬ��׻�����
  ! eef_VelFromZeta_ee2f   :: ���� 2 ��ʬ(��_1, ��_2)��®�٤� 1 ��ʬ��׻�����
  ! ee2f_ZetaFromVor_eef_eef_eef :: ���٤��鱲�� 2 ��ʬ(��_1, ��_2)��׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntZXV_zxv, AvrZXV_zxv   :: 3 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! z_IntXV_zxv, z_AvrXV_zxv :: 3 �����ʻ����ǡ����� X,Y ������ʬ�����ʿ��
  ! x_IntZV_zxv, x_AvrZV_zxv :: 3 �����ʻ����ǡ����� Y,Z ������ʬ�����ʿ��
  ! y_IntZX_zxv, y_AvrZX_zxv :: 3 �����ʻ����ǡ����� Z,X ������ʬ�����ʿ��
  ! zv_IntX_zxv, zv_AvrX_zxv :: 3 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! zx_IntY_zxv, zx_AvrY_zxv :: 3 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! vx_IntZ_zxv, vx_AvrZ_zxv :: 3 �����ʻ����ǡ����� Z ������ʬ�����ʿ��
  !
  ! IntXV_xv,  AvrXV_xv  :: 2 ����(XY)�ʻ����ǡ����� X,Y ������ʬ�����ʿ��
  ! v_IntX_xv, v_AvrX_xv :: 2 ����(XY)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntV_xv, x_AvrV_xv :: 2 ����(XY)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntZX_zx, AvrZX_zx   :: 2 ����(ZX)�ʻ����ǡ����� Z,X ������ʬ�����ʿ��
  ! z_IntX_zx, z_AvrX_zx :: 2 ����(ZX)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntZ_zx, x_AvrZ_zx :: 2 ����(ZX)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  ! IntZV_zv, AvrZV_zv   :: 2 ����(YZ)�ʻ����ǡ����� Y,Z ������ʬ�����ʿ��
  ! v_IntZ_zv, v_AvrZ_zv :: 2 ����(YZ)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  ! z_IntV_zv, z_AvrV_zv :: 2 ����(YZ)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntV_v, AvrV_v       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntZ_z, AvrZ_z       :: 1 ����(Z)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! EnergyHelicityFromZeta_ee2f  :: �����ͥ륮�������إꥷ�ƥ�����׻�����. 
  ! ESpectralFromZeta            :: ���ͥ륮�����ڥ��ȥ��׻�����. 
  !
  use dc_message, only : MessageNotify
  use mpi
  implicit none

  private
  public ls, le, lc
  public js, je, jc
  public lf_l
  public eee_mpi_Initial                                  ! ������롼����
  public eee_ChangeResolution                             ! ������������ѹ�
  public zxv_eef, eef_zxv                                 ! �����Ѵ�
  public xyz_zxv                                          ! �����Ѵ�
  public eee2_ee2f, ee2f_eee2                             ! �����Ѵ�
  public eef_Dx_eef, eef_Dy_eef, eef_Dz_eef               ! ��ʬ
  public eef_Lapla_eef, eef_LaplaInv_eef                  ! ��ʬ
  public ee2f_RotVelxVor_ee2f                             ! �������׻�
  public eef_VorFromZeta_ee2f, eef_VelFromZeta_ee2f       ! ����®�ٷ׻�
  public ee2f_ZetaFromVor_eef_eef_eef                     ! �����Ѵ�

  public IntZXV_zxv, AvrZXV_zxv                           ! ��ʬ��ʿ��
  public z_IntXV_zxv, z_AvrXV_zxv                         ! ��ʬ��ʿ��
  public x_IntZV_zxv, x_AvrZV_zxv                         ! ��ʬ��ʿ��
  public v_IntZX_zxv, v_AvrZX_zxv                         ! ��ʬ��ʿ��
  public zv_IntX_zxv, zv_AvrX_zxv                         ! ��ʬ��ʿ��
  public zx_IntV_zxv, zx_AvrV_zxv                         ! ��ʬ��ʿ��
  public xv_IntZ_zxv, xv_AvrZ_zxv                         ! ��ʬ��ʿ��

  public IntXV_xv,  AvrXV_xv                              ! ��ʬ��ʿ��
  public v_IntX_xv, v_AvrX_xv                             ! ��ʬ��ʿ��
  public x_IntV_xv, x_AvrV_xv                             ! ��ʬ��ʿ��
  public IntZX_zx, AvrZX_zx                               ! ��ʬ��ʿ��
  public z_IntX_zx, z_AvrX_zx                             ! ��ʬ��ʿ��
  public x_IntZ_zx, x_AvrZ_zx                             ! ��ʬ��ʿ��
  public IntZV_zv, AvrZV_zv                               ! ��ʬ��ʿ��
  public v_IntZ_zv, v_AvrZ_zv                             ! ��ʬ��ʿ��
  public z_IntV_zv, z_AvrV_zv                             ! ��ʬ��ʿ��

  public IntX_x, AvrX_x                                   ! ��ʬ��ʿ��
  public IntV_v, AvrV_v                                   ! ��ʬ��ʿ��
  public IntZ_z, AvrZ_z                                   ! ��ʬ��ʿ��

  public EnergyHelicityFromZeta_ee2f                      ! ���ͥ륮���إꥷ�ƥ���
  public ESpectralFromZeta                                ! ���ͥ륮�����ڥ��ȥ�

  public x_X, v_Y, z_Z                                    ! ��ɸ�ѿ�
  public x_X_Weight, v_Y_Weight, z_Z_Weight               ! ��ɸ�ѿ�
  public zxv_X, zxv_Y, zxv_Z                              ! ��ɸ�ѿ�

  integer   :: im=32, jm=32, km=32                      ! �ʻ���������(X,Y,Z)
  integer   :: lm=10, mm=10, nm=10                      ! �����ȿ�������(X,Y,Z)

  integer, dimension(:),   pointer :: le => null()      ! X �ȿ��ϰ�, ��
  integer, dimension(:),   pointer :: ls => null()      ! X �ȿ��ϰ�, ��
  integer, dimension(:),   pointer :: lc => null()      ! X �ȿ��ϰ�, ��
  integer, dimension(:),   pointer :: je => null()      ! Y ��ɸ�ʻ����ϰ�, ��
  integer, dimension(:),   pointer :: js => null()      ! Y ��ɸ�ʻ����ϰ�, ��
  integer, dimension(:),   pointer :: jc => null()      ! Y ��ɸ�ʻ����ϰ�, ��
  integer   :: np, ip                             ! MPI �ץ�����, �ץ��� ID

  integer, dimension(:),   pointer :: itk => null()
  real(8), dimension(:),   pointer :: tk => null()
  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()


  real(8), dimension(:),   pointer :: x_X => null()   ! �ʻ�����ɸ(X)
  real(8), dimension(:),   pointer :: v_Y => null()   ! �ʻ�����ɸ(Y)
  real(8), dimension(:),   pointer :: z_Z => null()   ! �ʻ�����ɸ(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! �ʻ����Ť�(X)
                                         ! X �����γʻ����δֳ֤���Ǽ���Ƥ���.
  real(8), dimension(:),   pointer :: v_Y_Weight => null()
                                         ! �ʻ����Ť�(Y)
                                         ! Y �����γʻ����δֳ֤���Ǽ���Ƥ���.
  real(8), dimension(:),   pointer :: z_Z_Weight => null()
                                         ! �ʻ����Ť�(Y)
                                         ! Z �����γʻ����δֳ֤���Ǽ���Ƥ���.

  real(8), dimension(:,:,:), pointer :: zxv_X => null()
                          ! �ʻ���(X)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� X ��ɸ���Ǽ�����ʻҥǡ���
  real(8), dimension(:,:,:), pointer :: zxv_Y => null()
                          ! �ʻ���(Y)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� Y ��ɸ���Ǽ�����ʻҥǡ���
  real(8), dimension(:,:,:), pointer :: zxv_Z => null()
                          ! �ʻ���(Z)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� Z ��ɸ���Ǽ�����ʻҥǡ���

  real(8), dimension(:), pointer :: w => null(), ws => null(), wg => null()
  real(8), dimension(:), pointer :: sgwork => null()

  integer, parameter :: nparams_max = 10  ! eee_Initial ��Ƥ٤������
  type eee_param                          ! �������ΰ����¤��
     integer   :: im, jm, km
     integer   :: lm, mm, nm
     integer, dimension(:),     pointer :: ls, le, lc
     integer, dimension(:),     pointer :: js, je, jc
     integer, dimension(:),     pointer :: itk
     real(8), dimension(:),     pointer :: tk
     integer, dimension(:),     pointer :: itj
     real(8), dimension(:),     pointer :: tj
     integer, dimension(:),     pointer :: iti
     real(8), dimension(:),     pointer :: ti
     real(8), dimension(:),     pointer :: x_X
     real(8), dimension(:),     pointer :: v_Y
     real(8), dimension(:),     pointer :: z_Z
     real(8), dimension(:),     pointer :: x_X_Weight
     real(8), dimension(:),     pointer :: v_Y_Weight
     real(8), dimension(:),     pointer :: z_Z_Weight
     real(8), dimension(:,:,:), pointer :: zxv_X
     real(8), dimension(:,:,:), pointer :: zxv_Y 
     real(8), dimension(:,:,:), pointer :: zxv_Z
     real(8), dimension(:),     pointer :: w, ws, wg
     real(8), dimension(:),     pointer :: sgwork
  end type eee_param
  type(eee_param) :: params(nparams_max)  ! �������ΰ����
  integer :: nparams                      ! �������ΰ����θĿ�

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, mm, nm, itk, tk, itj, tj, iti, ti
  save ls, le, lc, js, je, jc, np, ip
  save x_X, v_Y, z_Z, x_X_Weight, v_Y_Weight, z_Z_Weight, zxv_X, zxv_Y, zxv_Z
  save params, nparams

  contains
  !--------------- ����� -----------------
    subroutine eee_mpi_Initial(i,j,k,l,m,n,id)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      ! �ΰ���ϰϤ� [0,2pi]x[0,2pi]x[0,2pi] �˷���Ǥ�
      !
      ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      ! ���ץ���ʥ���� id ���Ѥ��ưۤʤ������, �ΰ��Ʊ����
      ! �������Ȥ��Ǥ���. eee_Initial �������, �ΰ褴�Ȥ˸Ƥ��
      ! ���� id �򥭡��פ�, eee_ChangeResolution �����ؤ���.
      !
      integer,intent(in) :: i           ! �ʻ�����(X)
      integer,intent(in) :: j           ! �ʻ�����(Y)
      integer,intent(in) :: k           ! �ʻ�����(Z)
      integer,intent(in) :: l           ! �����ȿ�(X)
      integer,intent(in) :: m           ! �����ȿ�(Y)
      integer,intent(in) :: n           ! �����ȿ�(X)

      integer, intent(out), optional :: id  ! �������ΰ�����ֹ�

      character(len=3) cid
      integer :: ii, jj, kk
      integer :: lls, lle, llc, jjs, jje, jjc
      integer :: iip
      integer :: lp, jp
      integer :: ierr

      im = i         ; jm = j         ; km = k
      lm = l         ; mm = m         ; nm = n

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','eee_initial',&
              'too many call of eee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      lp=lm/np+1
      lls=lp*ip
      lle=min(lp*(ip+1)-1,lm)
      if(lle.ge.lls) then
         llc=lle-lls+1
      else
         llc=0 ;  lls=0  ; lle=0
      end if

      jp=(jm-1)/np+1
      jjs=jp*ip
      jje=min(jp*(ip+1)-1,jm-1)
      if(jje.ge.jjs) then
         jjc=jje-jjs+1
      else
         jjc=0 ; jjs=0 ; jje=0
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%mm = mm
      params(nparams)%nm = nm
      allocate(params(nparams)%ls(0:np-1))
      allocate(params(nparams)%le(0:np-1))
      allocate(params(nparams)%lc(0:np-1))
      allocate(params(nparams)%js(0:np-1))
      allocate(params(nparams)%je(0:np-1))
      allocate(params(nparams)%jc(0:np-1))

      allocate(params(nparams)%itk(5))
      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tk(km*2))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))
      allocate(params(nparams)%w(km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))
      allocate(params(nparams)%ws((2*mm+1)*(2*nm+1)*2*2*(lm+1)))
      allocate(params(nparams)%wg(4*km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))
      allocate(params(nparams)%sgwork(km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%v_Y(jjs:jje))
      allocate(params(nparams)%v_Y_Weight(jjs:jje))
      allocate(params(nparams)%z_Z(0:km-1))
      allocate(params(nparams)%z_Z_Weight(0:km-1))
      allocate(params(nparams)%zxv_X(0:km-1,0:im-1,jjs:jje))
      allocate(params(nparams)%zxv_Y(0:km-1,0:im-1,jjs:jje))
      allocate(params(nparams)%zxv_Z(0:km-1,0:im-1,jjs:jje))

      call eee_ChangeResolution(nparams)

      call p3init(km,jm,im,itk,tk,itj,tj,iti,ti)

      do iip=0,np-1
         lp=lm/np+1
         ls(iip)=lp*iip
         le(iip)=min(lp*(iip+1)-1,lm)
         if(le(iip).ge.ls(iip)) then
            lc(iip)=le(iip)-ls(iip)+1
         else
            lc(iip)=0 ;  ls(iip)=0  ; le(iip)=0
         end if

         jp=(jm-1)/np+1
         js(iip)=jp*iip
         je(iip)=min(jp*(iip+1)-1,jm-1)
         if(je(iip).ge.js(iip)) then
            jc(iip)=je(iip)-js(iip)+1
         else
            jc(iip)=0 ; js(iip)=0 ; je(iip)=0
         end if
      enddo

      do ii=0,im-1
         x_X(ii) = 2*pi/im*ii
      enddo
      x_X_Weight = 2*pi/im

      do jj=js(ip),je(ip)
         v_Y(jj) = 2*pi/jm*jj
      enddo
      v_Y_Weight = 2*pi/jm

      do kk=0,km-1
         z_Z(kk) = 2*pi/km*kk
      enddo
      z_Z_Weight = 2*pi/km

      zxv_X = spread(spread(x_X,1,km),3,jc(ip))
      zxv_Y = spread(spread(v_Y,1,im),1,km)
      zxv_Z = spread(spread(z_Z,2,im),3,jc(ip))

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','eee_mpi_initial','eee_mpi_module is initialized')
      call MessageNotify('M','eee_mpi_initial',&
           'Resolution ID is '//trim(adjustl(cid)))
    end subroutine eee_mpi_Initial

  !--------------- id �ѹ� -----------------
    subroutine eee_ChangeResolution(id)
      !
      ! ������������ѹ�. eee_Initial �����ꤹ��ݤ�
      ! �����äƤ��륪�ץ���ʥ���� id ���ͤ��Ѥ���. 
      !
      integer, intent(in) :: id

      if (id .gt. nparams .or. id .lt. 1) then
         write(*,*)"id is invalid"
      end if

      im = params(id)%im
      jm = params(id)%jm
      km = params(id)%km
      lm = params(id)%lm
      mm = params(id)%mm
      nm = params(id)%nm
      ls => params(id)%ls
      le => params(id)%le
      lc => params(id)%lc
      js => params(id)%js
      je => params(id)%je
      jc => params(id)%jc
      itk => params(id)%itk
      tk  => params(id)%tk
      itj => params(id)%itj
      tj  => params(id)%tj
      iti => params(id)%iti
      ti  => params(id)%ti
      x_X => params(id)%x_X
      v_Y => params(id)%v_Y
      z_Z => params(id)%z_Z
      x_X_Weight => params(id)%x_X_Weight
      v_Y_Weight => params(id)%v_Y_Weight
      z_Z_Weight => params(id)%z_Z_Weight
      zxv_X => params(id)%zxv_X
      zxv_Y => params(id)%zxv_Y
      zxv_Z => params(id)%zxv_Z
      w => params(id)%w
      ws => params(id)%ws
      wg => params(id)%wg
      sgwork => params(id)%sgwork

    end subroutine eee_ChangeResolution

  !--------------- �ȿ������䤤��碌 -----------------
    function lf_l(l)
      !
      ! ���ڥ��ȥ�ǡ��� eef(-nm:nm,-mm,mm,2*lc) �Ǥ�
      ! X �����ȿ� L �ΰ��־���(�� 3 ź����)��Ĵ�٤�.
      !
      ! X �����ȿ��ˤĤ��Ƥ� ls,ls+1,... le, -le,-le+1,...,-ls
      ! �ν�˳�Ǽ����Ƥ���(ISPACK/P3PACK �ޥ˥奢�뻲�ȤΤ���)
      !
      integer, intent(IN)  ::  l        ! X �����ȿ�
      integer              ::  lf_l     ! ���ڥ��ȥ�ǡ����� 3 ź����

      if ( abs(l) < ls(ip) .OR. le(ip) < abs(l) ) then
         call MessageNotify('E','l_l','Input wavenumber out of range')
      endif
      
      if ( l >= 0 ) then
         lf_l = l-ls(ip)+1
      else
         lf_l = ls(ip)+2*lc(ip)-abs(l)
      endif

    end function lf_l

  !--------------- �����Ѵ� -----------------
    function zxv_eef(eef)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip))        :: zxv_eef
                                                        !(out) �ʻ����ǡ���
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in) :: eef
                                                        !(in)  ���ڥ��ȥ�ǡ���
      sgwork(1:(2*nm+1)*(2*mm+1)*2*lc(ip)) &
           = reshape(eef,(/(2*nm+1)*(2*mm+1)*2*lc(ip)/))

      call p3smgb(nm,mm,lm,km,jm,im,sgwork,w,itk,tk,itj,tj,iti,ti)

      zxv_eef = reshape(sgwork(1:im*km*jc(ip)),(/km,im,jc(ip)/))
    end function zxv_eef

    function eef_zxv(zxv)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))                  :: eef_zxv
                                                      !(out)  ���ڥ��ȥ�ǡ���
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(in) :: zxv
                                                      !(in) �ʻ����ǡ���

      sgwork(1:im*jc(ip)*km) = reshape(zxv,(/im*jc(ip)*km/))

      call p3gmsb(nm,mm,lm,km,jm,im,sgwork,w,itk,tk,itj,tj,iti,ti)

      eef_zxv = reshape(sgwork(1:(2*nm+1)*(2*mm+1)*2*lc(ip)),&
                               (/2*nm+1,2*mm+1,2*lc(ip)/))

    end function eef_zxv

    function eee2_ee2f(ee2f)
      !
      ! ʬ�����ڥ��ȥ�ǡ������Ѥ���
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)             :: eee2_ee2f
      !(out)  ���ڥ��ȥ�ǡ���

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)),intent(IN) :: ee2f
      !(in) ʬ�����ڥ��ȥ�ǡ���
      
      real(8), dimension(-nm:nm,-mm:mm,2,0:2*sum(lc)-1)       :: ee2e
      ! ��ȥǡ���
      integer, dimension(0:np-1) :: nm2ls
      integer :: ierr, iip
      integer :: l

      nm2ls(0)   = 0
      do iip=1,np-1
         nm2ls(iip) = nm2ls(iip-1) + (2*nm+1)*(2*mm+1)*2*2*lc(iip-1)
      enddo

      call MPI_ALLGATHERV(ee2f,(2*nm+1)*(2*mm+1)*2*2*lc(ip),MPI_REAL8,&
                          ee2e,(2*nm+1)*(2*mm+1)*2*2*lc,nm2ls,&
                          MPI_REAL8,MPI_COMM_WORLD,IERR)

      do l=ls(0),le(0)
         eee2_ee2f(:,:,l,:) = ee2e(:,:,:,l-ls(0))
         if ( l /= 0 ) then
            eee2_ee2f(:,:,-l,:) = ee2e(:,:,:,2*lc(0)-(l-ls(0)+1))
         endif
      enddo

      do iip=1,np-1
         do l=ls(iip),le(iip)
            eee2_ee2f(:,:,l,:) = ee2e(:,:,:,2*sum(lc(0:iip-1))+(l-ls(iip)))
            eee2_ee2f(:,:,-l,:) = ee2e(:,:,:,2*sum(lc(0:iip))-(l-ls(iip)+1))
         enddo
      enddo

    end function eee2_ee2f

    function ee2f_eee2(eee2)
      !
      ! ���ڥ��ȥ�ǡ�����ʬ������
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))       :: ee2f_eee2
      !(out)  ���ڥ��ȥ�ǡ���

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(IN) :: eee2
      !(in) ʬ�����ڥ��ȥ�ǡ���
      
      ! ��ȥǡ���
      integer :: ierr, iip
      integer :: l 

      do l=ls(ip),le(ip)
         ee2f_eee2(:,:,:,l-ls(ip)+1) = eee2(:,:,l,:)
         ee2f_eee2(:,:,:,2*lc(ip)-(l-ls(ip))) = eee2(:,:,-l,:)
      enddo
      
    end function ee2f_eee2

    function xyz_zxv(zxv)
      !
      ! ʬ���ʻҥǡ����ν���
      !
      real(8), dimension(0:im-1,0:jm-1,0:km-1)             :: xyz_zxv
                                                        !(out) �ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(in)  :: zxv
                                                        !(in) �ʻ����ǡ���
      real(8), dimension(0:km-1,0:im-1,0:jm-1)  :: zxy
      ! ����ѿ�

      integer :: i,j,k
      integer :: ierr

      call MPI_ALLGATHERV(zxv,km*im*jc(ip),MPI_REAL8,&
                          zxy,km*im*jc,km*im*js,MPI_REAL8,&
                          MPI_COMM_WORLD,IERR)

      do k=0,km-1
         do j=0,jm-1
            do i=0,im-1
               xyz_zxv(i,j,k) = zxy(k,i,j)
            enddo
         enddo
      enddo

    end function xyz_zxv
    
  !--------------- ��ʬ�׻� -----------------
    function eef_Lapla_eef(eef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy+��zz)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (l**2 + m**2 + n**2) �򤫤���
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Lapla_eef
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�

      eef_Lapla_eef = 0.0

      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Lapla_eef(n,m,lf_l(l)) = -(l**2+m**2+n**2)*eef(n,m,lf_l(l))
               eef_Lapla_eef(n,m,lf_l(-l)) = -(l**2+m**2+n**2)*eef(n,m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Lapla_eef

    function eef_LaplaInv_eef(eef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy+��zz)**(-1)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (l**2 + m**2 + n**2) �ǳ��
      ! �׻���ԤäƤ���. l=m=n=0 ��ʬ�ˤ� 0 ���������Ƥ���. 
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))             :: eef_LaplaInv_eef
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in) :: eef
      !(in) ���ڥ��ȥ�ǡ���

      integer l,m,n

      eef_LaplaInv_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l.ne.0 .or. m.ne.0 .or. n.ne.0 ) then
                  eef_LaplaInv_eef(n,m,lf_l(l)) = -eef(n,m,lf_l(l))/(l**2+m**2+n**2)
                  eef_LaplaInv_eef(n,m,lf_l(-l)) = -eef(n,m,lf_l(-l))/(l**2+m**2+n**2)
               endif
            enddo
         enddo
      enddo
    end function eef_LaplaInv_eef

    function eef_Dx_eef(eef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� l �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dx_eef
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�

      eef_Dx_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dx_eef(n,m,lf_l(l)) = -l*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dx_eef(n,m,lf_l(l)) = -l*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo
    end function eef_Dx_eef

    function eef_Dy_eef(eef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� Y ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� Y �����ȿ� m �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dy_eef
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�

      eef_Dy_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dy_eef(n,m,lf_l(l)) = -m*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dy_eef(n,m,lf_l(l)) = -m*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Dy_eef

    function eef_Dz_eef(eef)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Z ��ʬ(��z)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� Z ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Z ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� Z �����ȿ� n �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dz_eef
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�

      eef_Dz_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dz_eef(n,m,lf_l(l)) = -n*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dz_eef(n,m,lf_l(l)) = -n*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Dz_eef

  !--------------- ��������׻� -----------------

    function ee2f_RotVelxVor_ee2f(ee2f)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ������� Euler ����������������
      !
      !     ��x(u x ��) 
      !
      !  �� 2 ��ʬ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))     :: ee2f_RotVelxVor_ee2f
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      call p3emnl(nm,mm,lm,km,jm,im,ee2f,ee2f_RotVelxVor_ee2f, &
                  wg,itk,tk,itj,tj,iti,ti)

    end function ee2f_RotVelxVor_ee2f

    function eef_VorFromZeta_ee2f(ee2f,isw)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ������鱲�٤Υ��ڥ��ȥ�� 1 ��ʬ
      !  ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))     :: eef_VorFromZeta_ee2f
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      integer, intent(IN) :: isw
      !(in) ���Ϥ��뱲�٤���ʬ�Υ���ǥå���(1,2,3)

      call p3gmto(nm,mm,lm,ee2f,eef_VorFromZeta_ee2f,isw)

    end function eef_VorFromZeta_ee2f

    function eef_VelFromZeta_ee2f(ee2f,isw)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ�������®�٥��ڥ��ȥ�� 1 ��ʬ
      !  ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))     :: eef_VelFromZeta_ee2f
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      integer, intent(IN) :: isw
      !(in) ���Ϥ��뱲�٤���ʬ�Υ���ǥå���(1,2,3)

      call p3gmtu(nm,mm,lm,ee2f,eef_VelFromZeta_ee2f,isw)

    end function eef_VelFromZeta_ee2f

    function ee2f_ZetaFromVor_eef_eef_eef(eef_1,eef_2,eef_3)
      !
      !  ���٤Υ��ڥ��ȥ뤫�鱲�� 2 ��ʬ(��_1, ��_2)��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))     :: ee2f_ZetaFromVor_eef_eef_eef
      !(out) ���٤� 2 ��ʬ(��_1, ��_2)

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef_1, eef_2, eef_3
      !(in) ���٥��ڥ��ȥ�ǡ����γ���ʬ

      integer :: l,m,n

      ee2f_ZetaFromVor_eef_eef_eef = 0.0D0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_2(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_3(n,m,lf_l(l))
               elseif( m /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_3(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_1(n,m,lf_l(l))
               else
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_1(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_2(n,m,lf_l(l))
               endif
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_2(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_3(n,m,lf_l(l))
               elseif( m /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_3(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_1(n,m,lf_l(l))
               else
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_1(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_2(n,m,lf_l(l))
               endif
            enddo
         enddo
      enddo

    end function ee2f_ZetaFromVor_eef_eef_eef

  !--------------- ��ʬ�׻� -----------------
    function IntZXV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ��������ΰ���ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in)  3 �����ʻ����ǡ���

      real(8)                                    :: IntZXV_zxv
      !(out) ��ʬ��

      real(8) :: IntZXVTMP
      integer :: i, j, k
      integer :: ierr
      ! ����ѿ�

      IntZXV_zxv = 0.0d0
      do j=js(ip),je(ip)
         do i=0,im-1
            do k=0,km-1
               IntZXV_zxv = IntZXV_zxv &
                    + zxv(k,i,j) * z_Z_Weight(k) * v_Y_Weight(j) * x_X_Weight(i)
            enddo
         enddo
      end do

      IntZXVTMP=IntZXV_zxv
      CALL MPI_ALLREDUCE(IntZXVTMP,IntZXV_zxv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntZXV_zxv

    function z_IntXV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X,Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_IntXV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_IntXVTmp
      integer :: i, j
      integer :: ierr
      ! ����ѿ�

      z_IntXV_zxv = 0.0d0
      do i=0,im-1
         do j=js(ip),je(ip)
            z_IntXV_zxv(:) = &
                 z_IntXV_zxv(:) + zxv(:,i,j) * x_X_Weight(i)* v_Y_Weight(j)
         enddo
      enddo

      z_IntXVTMP=z_IntXV_zxv
      CALL MPI_ALLREDUCE(z_IntXVTMP,z_IntXV_zxv,km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntXV_zxv

    function v_IntZX_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X,Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_IntZX_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i, k
      ! ����ѿ�

      v_IntZX_zxv = 0.0d0
      do i=0,im-1
         do k=0,km-1
            v_IntZX_zxv(:) = &
                 v_IntZX_zxv(:) + zxv(k,i,:) * x_X_Weight(i)* z_Z_Weight(k)
         enddo
      enddo
    end function v_IntZX_zxv

    function x_IntZV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Y,Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_IntZV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_IntZVTMP
      integer :: j, k
      integer :: ierr
      ! ����ѿ�

      x_IntZV_zxv = 0.0d0
      do j=js(ip),je(ip)
         do k=0,km-1
            x_IntZV_zxv(:) = &
                 x_IntZV_zxv(:) + zxv(k,:,j) * v_Y_Weight(j)* z_Z_Weight(k)
         enddo
      enddo

      x_IntZVTMP=x_IntZV_zxv
      CALL MPI_ALLREDUCE(x_IntZVTMP,x_IntZV_zxv,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function x_IntZV_zxv

    function zv_IntX_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,js(ip):je(ip))          :: zv_IntX_zxv
      !(out) ��ʬ���줿 2 ����(ZY)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      zv_IntX_zxv = 0.0d0
      do i=0,im-1
         zv_IntX_zxv(:,:) = zv_IntX_zxv(:,:) + zxv(:,i,:) * x_X_Weight(i)
      enddo

    end function zv_IntX_zxv

    function zx_IntV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntVTMP
      integer :: j
      integer :: ierr
      ! ����ѿ�

      zx_IntV_zxv = 0.0d0
      do j=js(ip),je(ip)
         zx_IntV_zxv(:,:) = zx_IntV_zxv(:,:) + zxv(:,:,j) * v_Y_Weight(j)
      enddo

      zx_IntVTMP=zx_IntV_zxv
      CALL MPI_ALLREDUCE(zx_IntVTMP,zx_IntV_zxv,im*km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function zx_IntV_zxv

    function xv_IntZ_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1,js(ip):je(ip))          :: xv_IntZ_zxv
      !(out) ��ʬ���줿 1 ����(YX)�ʻ����ǡ���

      integer :: k 
      ! ����ѿ�

      xv_IntZ_zxv = 0.0d0
      do k=0,km-1
         xv_IntZ_zxv(:,:) = xv_IntZ_zxv(:,:) + zxv(k,:,:) * z_Z_Weight(k)
      enddo

    end function xv_IntZ_zxv

    function IntXV_xv(xv)
      !
      ! 2 ����(XY)�ʻ����ǡ��������ΰ���ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv          
      !(in)  2 ����(YX)�ʻ����ǡ���

      real(8)                             :: IntXV_xv
      !(out) ��ʬ��

      real(8) :: IntXVTMP
      integer :: i, j
      integer :: ierr
      ! ����ѿ�

      IntXV_xv = 0.0d0
      do j=js(ip),je(ip)
         do i=0,im-1
            IntXV_xv = IntXV_xv + xv(i,j) * v_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo

      IntXVTMP=IntXV_xv
      CALL MPI_ALLREDUCE(IntXVTMP,IntXV_xv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntXV_xv

    function v_IntX_xv(xv)
      !
      ! 2 ����(XV)�ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 ����(YX)�ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_IntX_xv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      v_IntX_xv = 0.0d0
      do i=0,im-1
         v_IntX_xv(:) = v_IntX_xv(:) + xv(i,:) * x_X_Weight(i)
      enddo
    end function v_IntX_xv

    function x_IntV_xv(xv)
      !
      ! 2 ����(YX)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv      
      !(in)  2 ����(XV)�ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntV_xv 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntVTMP
      integer :: j
      integer :: ierr
      ! ����ѿ�

      x_IntV_xv = 0.0d0
      do j=js(ip),je(ip)
         x_IntV_xv(:) = x_IntV_xv(:) + xv(:,j) * v_Y_Weight(j)
      enddo

      x_IntVTMP=x_IntV_xv
      CALL MPI_ALLREDUCE(x_IntVTMP,x_IntV_xv,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function x_IntV_xv

    function IntZV_zv(zv)
      !
      ! 2 ����(ZY)�ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8)                             :: IntZV_zv
      !(out) ��ʬ��

      real(8) :: IntZVTMP
      integer :: j, k
      integer :: ierr
      ! ����ѿ�

      IntZV_zv = 0.0d0
      do j=js(ip),je(ip)
         do k=0,km-1
            IntZV_zv = IntZV_zv + zv(k,j) * v_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo

      IntZVTMP=IntZV_zv
      CALL MPI_ALLREDUCE(IntZVTMP,IntZV_zv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntZV_zv

    function v_IntZ_zv(zv)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_IntZ_zv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: k
      ! ����ѿ�

      v_IntZ_zv = 0.0d0
      do k=0,km-1
         v_IntZ_zv(:) = v_IntZ_zv(:) + zv(k,:) * z_Z_Weight(k)
      enddo
    end function v_IntZ_zv

    function z_IntV_zv(zv)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)        :: z_IntV_zv 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      real(8), dimension(0:km-1)        :: z_IntVTMP
      integer :: j
      integer :: ierr
      ! ����ѿ�

      z_IntV_zv = 0.0d0
      do j=js(ip),je(ip)
         z_IntV_zv(:) = z_IntV_zv(:) + zv(:,j) * v_Y_Weight(j)
      enddo

      z_IntVTMP=z_IntV_zv
      CALL MPI_ALLREDUCE(z_IntVTMP,z_IntV_zv,km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntV_zv

    function IntZX_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight, x_X_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 ����(ZX)�ʻ����ǡ���

      real(8)                             :: IntZX_zx
      !(out) ��ʬ��

      integer :: i, k
      ! ����ѿ�

      IntZX_zx = 0.0d0
      do i=0,im-1
         do k=0,km-1
            IntZX_zx = IntZX_zx + zx(k,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZX_zx

    function x_IntZ_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 ����(ZX)�ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_IntZ_zx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: k
      ! ����ѿ�

      x_IntZ_zx = 0.0d0
      do k=0,km-1
         x_IntZ_zx(:) = x_IntZ_zx(:) + zx(k,:) * z_Z_Weight(k)
      enddo
    end function x_IntZ_zx

    function z_IntX_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)        :: z_IntX_zx 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      z_IntX_zx = 0.0d0
      do i=0,im-1
         z_IntX_zx(:) = z_IntX_zx(:) + zx(:,i) * x_X_Weight(i)
      enddo
    end function z_IntX_zx

    function IntX_x(x)
      !
      ! 1 ����(X)�ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1)   :: x          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntX_x     !(out) ��ʬ��

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntV_v(v)
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(js(ip):je(ip))    :: v          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntV_v     !(out) ��ʬ��
      
      real(8) :: IntVTMP
      integer :: ierr

      IntV_v = sum(v*v_Y_Weight)

      IntVTMP=IntV_v
      CALL MPI_ALLREDUCE(IntVTMP,IntV_v,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function IntV_v

    function IntZ_z(z)
      !
      ! 1 ����(Z)�ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1)   :: z         !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntZ_z     !(out) ��ʬ��

      IntZ_z = sum(z*z_Z_Weight)
    end function IntZ_z

  !--------------- ʿ�ѷ׻� -----------------
    function AvrZXV_zxv(zxv)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight, z_Z_Weight ��
      ! ���������¤�׻���, x_X_Weight*y_Y_Weight*z_Z_Weight �����¤ǳ��
      ! ���Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in)  3 �����ʻ����ǡ���

      real(8)                                    :: AvrZXV_zxv
      !(out) ��ʬ��

      real(8) :: Vol, VolTMP
      integer :: ierr
      ! ����ѿ�

      VolTMP=sum(x_X_weight)*sum(v_Y_weight)*sum(z_Z_weight)
      
      CALL MPI_ALLREDUCE(VolTMP,Vol,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrZXV_zxv = IntZXV_zxv(zxv)/Vol

    end function AvrZXV_zxv

    function z_AvrXV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X,Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_AvrXV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! ����ѿ�

      AreaTMP = sum(x_X_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      z_AvrXV_zxv = z_IntXV_zxv(zxv)/Area

    end function z_AvrXV_zxv

    function v_AvrZX_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X,Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_AvrZX_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      v_AvrZX_zxv = v_IntZX_zxv(zxv)/(sum(x_X_weight)*sum(z_Z_weight))

    end function v_AvrZX_zxv

    function x_AvrZV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Y,Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻���, y_Y_Weight*z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrZV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! ����ѿ�

      AreaTMP = sum(z_Z_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      x_AvrZV_zxv = x_IntZV_zxv(zxv)/Area

    end function x_AvrZV_zxv

    function zv_AvrX_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,js(ip):je(ip))          :: zv_AvrX_zxv
      !(out) ��ʬ���줿 2 ����(ZY)�ʻ����ǡ���

      zv_AvrX_zxv = zv_IntX_zxv(zxv)/sum(x_X_weight)

    end function zv_AvrX_zxv

    function zx_AvrV_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤���
      ! ���¤�׻���, v_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1)          :: zx_AvrV_zxv
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! ����ѿ�

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      zx_AvrV_zxv = zx_IntV_zxv(zxv)/Length

    end function zx_AvrV_zxv

    function xv_AvrZ_zxv(zxv)
      !
      ! 3 �����ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤���
      ! ���¤�׻���, z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1,js(ip):je(ip))          :: xv_AvrZ_zxv
      !(out) ��ʬ���줿 1 ����(YX)�ʻ����ǡ���

      xv_AvrZ_zxv = xv_IntZ_zxv(zxv)/sum(z_Z_weight)

    end function xv_AvrZ_zxv

    function AvrXV_xv(xv)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in)  2 �����ʻ����ǡ���

      real(8)                             :: AvrXV_xv    
      !(out) ʿ����

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! ����ѿ�

      AreaTMP = sum(x_X_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrXV_xv = IntXV_xv(xv)/Area

    end function AvrXV_xv

    function v_AvrX_xv(xv)
      !
      ! 2 �����ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_AvrX_xv
      !(out) ʿ�Ѥ��줿 1 ����(Y)�ʻ����ǡ���

      v_AvrX_xv = v_IntX_xv(xv)/sum(x_X_weight)
    end function v_AvrX_xv

    function x_AvrV_xv(xv)
      !
      ! 2 �����ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrV_xv
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! ����ѿ�

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      x_AvrV_xv = x_IntV_xv(xv)/Length
    end function x_AvrV_xv

    function AvrZX_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in)  2 ����(ZX)�ʻ����ǡ���

      real(8)                             :: AvrZX_zx    
      !(out) ʿ����

      AvrZX_zx = IntZX_zx(zx)/(sum(x_X_weight)*sum(z_Z_weight))
    end function AvrZX_zx

    function z_AvrX_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in) 2 ����(ZX)�ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_AvrX_zx
      !(out) ʿ�Ѥ��줿 1 ����(Z)�ʻ����ǡ���

      z_AvrX_zx = z_IntX_zx(zx)/sum(x_X_weight)
    end function z_AvrX_zx

    function x_AvrZ_zx(zx)
      !
      ! 2 ����(ZX)�ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻���, 
      ! z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in) 2 ����(ZX)�ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrZ_zx
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      x_AvrZ_zx = x_IntZ_zx(zx)/sum(z_Z_weight)
    end function x_AvrZ_zx

    function AvrZV_zv(zv)
      !
      ! 2 ����(ZV)�ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, z_Z_Weight*v_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8)                             :: AvrZV_zv    
      !(out) ʿ����

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! ����ѿ�

      AreaTMP = sum(z_Z_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrZV_zv = IntZV_zv(zv)/Area
    end function AvrZV_zv

    function z_AvrV_zv(zv)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤������¤�׻���, 
      ! v_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in) 2 ����(ZV)�ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_AvrV_zv
      !(out) ʿ�Ѥ��줿 1 ����(Z)�ʻ����ǡ���
      real(8) :: Length, LengthTMP
      integer :: ierr
      ! ����ѿ�

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      z_AvrV_zv = z_IntV_zv(zv)/Length
    end function z_AvrV_zv

    function v_AvrZ_zv(zv)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻���, 
      ! z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in) 2 ����(ZY)�ʻ����ǡ���

      real(8), dimension(js(ip):je(ip))          :: v_AvrZ_zv
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      v_AvrZ_zv = v_IntZ_zv(zv)/sum(z_Z_weight)
    end function v_AvrZ_zv

    function AvrX_x(x)
      !
      ! 1 ����(X)�ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1), intent(IN) :: x       !(in)  1 �����ʻ����ǡ���
      real(8)                                :: AvrX_x  !(out) ʿ����

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrV_v(v)
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(js(ip):je(ip)), intent(IN) :: v
      !(in)  1 �����ʻ����ǡ���

      real(8)                               :: AvrV_v !(out) ʿ����

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! ����ѿ�

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrV_v = IntV_v(v)/Length
    end function AvrV_v

    function AvrZ_z(z)
      !
      ! 1 ����(Z)�ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻���, 
      ! z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1), intent(IN) :: z      !(in)  1 �����ʻ����ǡ���
      real(8)                                :: AvrZ_z !(out) ʿ����

      AvrZ_z = IntZ_z(z)/sum(z_Z_weight)
    end function AvrZ_z

  !--------------- ���ڥ��ȥ�׻� -----------------
    function EnergyHelicityFromZeta_ee2f(ee2f)
      !
      ! ������ʬ(��_1, ��_2)�������ΰ�ʿ�ѥ��ͥ륮���ȥإꥷ�ƥ�����׻�����.
      !
      real(8), dimension(2)                 :: EnergyHelicityFromZeta_ee2f
      ! ���ͥ륮���إꥷ�ƥ���

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in) :: ee2f
      ! ������ʬ(��_1, ��_2)

      real(8) :: E, H           ! ���ͥ륮��, �إꥷ�ƥ���
      
      call p3cmsv(nm,mm,lm,ee2f,E,H)

      EnergyHelicityFromZeta_ee2f(1) = E ; EnergyHelicityFromZeta_ee2f(2) = H

    end function EnergyHelicityFromZeta_ee2f

    subroutine ESpectralFromZeta(esp,ee2f)
      !
      ! ������ʬ(��_1, ��_2)���饨�ͥ륮�����ڥ��ȥ��׻�����. 
      !
      !   * esp �Υ������ǵ������ȿ��ϰϤ�������
      !   * esp �����¤� EFFromZeta �ǵ����륨�ͥ륮��
      !     (���ΰ�ʿ����)��������
      !
      real(8), dimension(:), intent(OUT)  :: esp
      ! ���ͥ륮�����ڥ��ȥ�

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in) :: ee2f
      ! ������ʬ(��_1, ��_2)

      integer kmax
      ! ����ѿ�
      real(8), allocatable :: wesp(:)

      kmax=size(esp)
      allocate(wesp(kmax))
      call p3empt(nm,mm,lm,kmax,ee2f,esp,wesp)
      deallocate(wesp)

    end subroutine ESpectralFromZeta

  end module eee_mpi_module
