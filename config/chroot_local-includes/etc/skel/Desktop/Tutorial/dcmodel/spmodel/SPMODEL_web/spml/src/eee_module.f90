!--
!----------------------------------------------------------------------
!     Copyright 2008-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  eee_module
!
!      spml/eee_module �⥸�塼��ϼ����������β��Ǥ� 3 ��������ΰ��
!      ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ���
!      �󶡤���. 
!
!      ������ ISPACK/P3PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!      ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ�
!      ISPACK/P3PACK �Υޥ˥奢��򻲾Ȥ��줿��. 
!
!����  2008/05/03  �ݹ�����
!      2008/05/10  �ݹ����� �������ΰ�ʣ���б�
!      2008/05/11  �ݹ����� xyz_zyx �ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ˽���
!      2009/07/31  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!
!++
module eee_module
  !
  != eee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eee_module.f90,v 1.5 2009-07-31 03:06:18 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
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
  ! * �ؿ�̾����Ƭ (eee_, zyx_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   eee_ :: ���ڥ��ȥ�ǡ���(�� 1,2 3 ���������줾�� Z, Y,X �����ȿ�)
  !   eee2 :: 2 �ĤΥ��ڥ��ȥ�ǡ������¤�����
  !   zyx_ :; 3 �����ʻ����ǡ���(�� 1,2 3 ���������줾�� Z, Y,X �����γʻ���)
  !   yx_ :: XY ���� 2 �����ʻ����ǡ��� 
  !   zy_ :: YZ ���� 2 �����ʻ����ǡ���
  !   zx_ :: XZ ���� 2 �����ʻ����ǡ���
  !   x_ :: X ���� 1 �����ʻ����ǡ���
  !   y_ :: Y ���� 1 �����ʻ����ǡ���
  !   z_ :: Z ���� 1 �����ʻ����ǡ���
  !  
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Dz, Lapla, LaplaInv)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_eee_eee,_eee,_zyx, _x, _y) ��, �����ѿ��η���
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _eee     :: ���ڥ��ȥ�ǡ���
  !   _eee_eee :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _eee2    :: 2 �ĤΥ��ڥ��ȥ�ǡ������¤�����
  !   _zyx     :: 3 �����ʻ����ǡ���
  !   _x       :: X ���� 1 �����ʻ����ǡ���
  !   _y       :: Y ���� 1 �����ʻ����ǡ���.
  !  
  !=== �ƥǡ����μ��������
  !  
  ! * zyx : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:km-1,0:jm-1,0:im-1). 
  !   * im, jm, km �Ϥ��줾�� X, Y Z ��ɸ�γʻ������Ǥ���, ���֥롼���� 
  !     eee_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * �� 1 ������ Z ��ɸ�γʻ��������ֹ�, �� 2 ������ Y ��ɸ, 
  !     �� 3 ������ X ��ɸ�γʻ��������ֹ�Ǥ��뤳�Ȥ����.
  !     (X, Y, Z�ν�ǤϤʤ�)
  !
  ! * eee : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-nm:nm,-mm:mm,-lm:lm). 
  !   * lm, mm, nm �Ϥ��줾�� X, Y, Z�����κ����ȿ��Ǥ���, ���֥롼���� 
  !     eee_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !     (X, Y, Z �����ȿ��ν�ǤϤʤ�)���Ȥ����. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ� ISPACK/P3PACK ��
  !     �ޥ˥奢��򻲾Ȥ��뤳��. 
  !
  ! * eee2 : 2 �ĤΥ��ڥ��ȥ�ǡ����Τʤ��. 
  !   * �ѿ��μ���ȼ����� real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2). 
  !
  ! * x, y, z : X, Y, Z ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� 
  !     real(8), dimension(0:im-1),
  !     real(8), dimension(0:jm-1), 
  !     ����� real(8), dimension(0:km-1).
  !
  ! * eee_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * zyx_ �ǻϤޤ�ؿ����֤��ͤ� 3 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, y_ z_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
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
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y, z_Z    ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, y_Y_Weight,  z_Z_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! zyx_X, zyx_Y, zyx_Z  :: �ʻ����ǡ����� XYZ ��ɸ(X,Y,Z)
  !                         (�ʻ����ǡ����� 3 ��������)
  !
  !==== �����Ѵ�
  !
  ! zyx_eee :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! eee_zyx :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! xyz_zyx :: �ʻ����ǡ�����ź��������Ѵ�
  !
  !==== ��ʬ
  !
  ! eee_Lapla_eee       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! eee_LaplaInv_eee    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! eee_Dx_eee          :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! eee_Dy_eee          :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! eee_Dz_eee          :: ���ڥ��ȥ�ǡ����� Z ��ʬ����Ѥ�����
  !
  !==== �������׻�
  !
  ! eee2_RotVelxVor_eee2   :: Euler �����������������׻�����
  ! eee_VorFromZeta_eee2   :: ���� 2 ��ʬ(��_1, ��_2)�鱲�٤� 1 ��ʬ��׻�����
  ! eee_VelFromZeta_eee2   :: ���� 2 ��ʬ(��_1, ��_2)��®�٤� 1 ��ʬ��׻�����
  ! eee2_ZetaFromVor_eee_eee_eee :: ���٤��鱲�� 2 ��ʬ(��_1, ��_2)��׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntZYX_zyx, AvrZYX_zyx   :: 3 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! z_IntYX_zyx, z_AvrYX_zyx :: 3 �����ʻ����ǡ����� X,Y ������ʬ�����ʿ��
  ! x_IntZY_zyx, x_AvrZY_zyx :: 3 �����ʻ����ǡ����� Y,Z ������ʬ�����ʿ��
  ! y_IntZX_zyx, y_AvrZX_zyx :: 3 �����ʻ����ǡ����� Z,X ������ʬ�����ʿ��
  ! zy_IntX_zyx, zy_AvrX_zyx :: 3 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! zx_IntY_zyx, zx_AvrY_zyx :: 3 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! yx_IntZ_zyx, yx_AvrZ_zyx :: 3 �����ʻ����ǡ����� Z ������ʬ�����ʿ��
  !
  ! IntYX_yx,  AvrYX_yx  :: 2 ����(XY)�ʻ����ǡ����� X,Y ������ʬ�����ʿ��
  ! y_IntX_yx, y_AvrX_yx :: 2 ����(XY)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx :: 2 ����(XY)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntZX_zx, AvrZX_zx   :: 2 ����(ZX)�ʻ����ǡ����� Z,X ������ʬ�����ʿ��
  ! z_IntX_zx, z_AvrX_zx :: 2 ����(ZX)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntZ_zx, x_AvrZ_zx :: 2 ����(ZX)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  ! IntZY_zy, AvrZY_zy   :: 2 ����(YZ)�ʻ����ǡ����� Y,Z ������ʬ�����ʿ��
  ! y_IntZ_zy, y_AvrZ_zy :: 2 ����(YZ)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  ! z_IntY_zy, z_AvrY_zy :: 2 ����(YZ)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntY_y, AvrY_y       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntZ_z, AvrZ_z       :: 1 ����(Z)�ʻ����ǡ����� Z ������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! EnergyHelicityFromZeta_eee2  :: �����ͥ륮�������إꥷ�ƥ�����׻�����. 
  ! ESpectralFromZeta            :: ���ͥ륮�����ڥ��ȥ��׻�����. 
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public eee_Initial                                  ! ������롼����
  public eee_ChangeResolution                         ! ������������ѹ�
  public zyx_eee, eee_zyx, xyz_zyx                    ! �����Ѵ�
  public eee_Dx_eee, eee_Dy_eee, eee_Dz_eee           ! ��ʬ
  public eee_Lapla_eee, eee_LaplaInv_eee              ! ��ʬ
  public eee2_RotVelxVor_eee2                         ! �������׻�
  public eee_VorFromZeta_eee2, eee_VelFromZeta_eee2   ! ����®�ٷ׻�
  public eee2_ZetaFromVor_eee_eee_eee                 ! �����Ѵ�

  public IntZYX_zyx, AvrZYX_zyx                       ! ��ʬ��ʿ��
  public z_IntYX_zyx, z_AvrYX_zyx                     ! ��ʬ��ʿ��
  public x_IntZY_zyx, x_AvrZY_zyx                     ! ��ʬ��ʿ��
  public y_IntZX_zyx, y_AvrZX_zyx                     ! ��ʬ��ʿ��
  public zy_IntX_zyx, zy_AvrX_zyx                     ! ��ʬ��ʿ��
  public zx_IntY_zyx, zx_AvrY_zyx                     ! ��ʬ��ʿ��
  public yx_IntZ_zyx, yx_AvrZ_zyx                     ! ��ʬ��ʿ��

  public IntYX_yx,  AvrYX_yx                          ! ��ʬ��ʿ��
  public y_IntX_yx, y_AvrX_yx                         ! ��ʬ��ʿ��
  public x_IntY_yx, x_AvrY_yx                         ! ��ʬ��ʿ��
  public IntZX_zx, AvrZX_zx                           ! ��ʬ��ʿ��
  public z_IntX_zx, z_AvrX_zx                         ! ��ʬ��ʿ��
  public x_IntZ_zx, x_AvrZ_zx                         ! ��ʬ��ʿ��
  public IntZY_zy, AvrZY_zy                           ! ��ʬ��ʿ��
  public y_IntZ_zy, y_AvrZ_zy                         ! ��ʬ��ʿ��
  public z_IntY_zy, z_AvrY_zy                         ! ��ʬ��ʿ��

  public IntX_x, AvrX_x                               ! ��ʬ��ʿ��
  public IntY_y, AvrY_y                               ! ��ʬ��ʿ��
  public IntZ_z, AvrZ_z                               ! ��ʬ��ʿ��

  public EnergyHelicityFromZeta_eee2                  ! ���ͥ륮���إꥷ�ƥ���
  public ESpectralFromZeta                            ! ���ͥ륮�����ڥ��ȥ�

  public x_X, y_Y, z_Z                                ! ��ɸ�ѿ�
  public x_X_Weight, y_Y_Weight, z_Z_Weight           ! ��ɸ�ѿ�
  public zyx_X, zyx_Y, zyx_Z                          ! ��ɸ�ѿ�

  integer   :: im=32, jm=32, km=32                    ! �ʻ���������(X,Y,Z)
  integer   :: lm=10, mm=10, nm=10                    ! �����ȿ�������(X,Y,Z)
  real(8)   :: xl=1.0, yl=1.0, zl=1.0                 ! �ΰ���礭��


  integer, dimension(:),   pointer :: itk => null()
  real(8), dimension(:),   pointer :: tk => null()
  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()


  real(8), dimension(:),   pointer :: x_X => null()   ! �ʻ�����ɸ(X)
  real(8), dimension(:),   pointer :: y_Y => null()   ! �ʻ�����ɸ(Y)
  real(8), dimension(:),   pointer :: z_Z => null()   ! �ʻ�����ɸ(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! �ʻ����Ť�(X)
                                         ! X �����γʻ����δֳ֤���Ǽ���Ƥ���.
  real(8), dimension(:),   pointer :: y_Y_Weight => null()
                                         ! �ʻ����Ť�(Y)
                                         ! Y �����γʻ����δֳ֤���Ǽ���Ƥ���.
  real(8), dimension(:),   pointer :: z_Z_Weight => null()
                                         ! �ʻ����Ť�(Y)
                                         ! Z �����γʻ����δֳ֤���Ǽ���Ƥ���.

  real(8), dimension(:,:,:), pointer :: zyx_X => null()
                          ! �ʻ���(X)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� X ��ɸ���Ǽ�����ʻҥǡ���
  real(8), dimension(:,:,:), pointer :: zyx_Y => null()
                          ! �ʻ���(Y)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� Y ��ɸ���Ǽ�����ʻҥǡ���
  real(8), dimension(:,:,:), pointer :: zyx_Z => null()
                          ! �ʻ���(Z)��ɸ(3 ����)
                          ! �Ƴʻ���(i,j,k)�ΰ��֤� Z ��ɸ���Ǽ�����ʻҥǡ���


  integer, parameter :: nparams_max = 10  ! eee_Initial ��Ƥ٤������
  type eee_param                          ! �������ΰ����¤��
     integer   :: im, jm, km
     integer   :: lm, mm, nm
     integer, dimension(:),     pointer :: itk
     real(8), dimension(:),     pointer :: tk
     integer, dimension(:),     pointer :: itj
     real(8), dimension(:),     pointer :: tj
     integer, dimension(:),     pointer :: iti
     real(8), dimension(:),     pointer :: ti
     real(8), dimension(:),     pointer :: x_X
     real(8), dimension(:),     pointer :: y_Y
     real(8), dimension(:),     pointer :: z_Z
     real(8), dimension(:),     pointer :: x_X_Weight
     real(8), dimension(:),     pointer :: y_Y_Weight
     real(8), dimension(:),     pointer :: z_Z_Weight
     real(8), dimension(:,:,:), pointer :: zyx_X
     real(8), dimension(:,:,:), pointer :: zyx_Y 
     real(8), dimension(:,:,:), pointer :: zyx_Z
  end type eee_param
  type(eee_param) :: params(nparams_max)  ! �������ΰ����
  integer :: nparams                      ! �������ΰ����θĿ�

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, mm, nm, itk, tk, itj, tj, iti, ti
  save x_X, y_Y, z_Z, x_X_Weight, y_Y_Weight, z_Z_Weight, zyx_X, zyx_Y, zyx_Z
  save params, nparams

  contains
  !--------------- ����� -----------------
    subroutine eee_Initial(i,j,k,l,m,n,id)
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

      im = i         ; jm = j         ; km = k
      lm = l         ; mm = m         ; nm = n

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','eee_initial',&
              'too many call of eee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%mm = mm
      params(nparams)%nm = nm

      allocate(params(nparams)%itk(5))
      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tk(km*2))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%y_Y(0:jm-1))
      allocate(params(nparams)%y_Y_Weight(0:jm-1))
      allocate(params(nparams)%z_Z(0:km-1))
      allocate(params(nparams)%z_Z_Weight(0:km-1))
      allocate(params(nparams)%zyx_X(0:km-1,0:jm-1,0:im-1))
      allocate(params(nparams)%zyx_Y(0:km-1,0:jm-1,0:im-1))
      allocate(params(nparams)%zyx_Z(0:km-1,0:jm-1,0:im-1))

      call eee_ChangeResolution(nparams)

      call p3init(km,jm,im,itk,tk,itj,tj,iti,ti)

      do ii=0,im-1
         x_X(ii) = 2*pi/im*ii
      enddo
      x_X_Weight = 2*pi/im

      do jj=0,jm-1
         y_Y(jj) = 2*pi/jm*jj
      enddo
      y_Y_Weight = 2*pi/jm

      do kk=0,km-1
         z_Z(kk) = 2*pi/km*kk
      enddo
      z_Z_Weight = 2*pi/km

      zyx_X = spread(spread(x_X,1,jm),1,km)
      zyx_Y = spread(spread(y_Y,2,im),1,km)
      zyx_Z = spread(spread(z_Z,2,jm),3,im)

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','eee_initial','eee_module (2009/07/31) is initialized')
      call MessageNotify('M','eee_initial',&
           'Resolution ID is '//trim(adjustl(cid)))
    end subroutine eee_initial

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
      itk => params(id)%itk
      tk => params(id)%tk
      itj => params(id)%itj
      tj => params(id)%tj
      iti => params(id)%iti
      ti => params(id)%ti
      x_X => params(id)%x_X
      y_Y => params(id)%y_Y
      z_Z => params(id)%z_Z
      x_X_Weight => params(id)%x_X_Weight
      y_Y_Weight => params(id)%y_Y_Weight
      z_Z_Weight => params(id)%z_Z_Weight
      zyx_X => params(id)%zyx_X
      zyx_Y => params(id)%zyx_Y
      zyx_Z => params(id)%zyx_Z

    end subroutine eee_ChangeResolution

  !--------------- �����Ѵ� -----------------
    function zyx_eee(eee)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1)             :: zyx_eee 
      !(out) �ʻ����ǡ���

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in) :: eee
      !(in)  ���ڥ��ȥ�ǡ���

      real(8), dimension(km*jm*im)                         :: w
      ! ����ΰ�

      call p3s2ga(nm,mm,lm,km,jm,im,eee,zyx_eee,w,itk,tk,itj,tj,iti,ti)
    end function zyx_eee

    function eee_zyx(zyx)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_zyx
      !(out)  ���ڥ��ȥ�ǡ���

      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(in)  :: zyx
      !(in) �ʻ����ǡ���

      real(8), dimension(km*jm*im)                         :: w
      real(8), dimension(0:km-1,0:jm-1,0:im-1)             :: zyx_tmp
      ! ����ΰ�

      zyx_tmp = zyx
      call p3g2sa(nm,mm,lm,km,jm,im,zyx_tmp,eee_zyx,w,itk,tk,itj,tj,iti,ti)
    end function eee_zyx

    function xyz_zyx(zyx)
      !
      ! �ʻҥǡ�����ź��������ѹ�
      !
      real(8), dimension(0:im-1,0:jm-1,0:km-1)             :: xyz_zyx
                                                        !(out) �ʻ����ǡ���

      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(in)  :: zyx
                                                      !(in) �ʻ����ǡ���
      integer :: i,j,k

      do k=0,km-1
         do j=0,jm-1
            do i=0,im-1
               xyz_zyx(i,j,k) = zyx(k,j,i)
            enddo
         enddo
      enddo

    end function xyz_zyx
    
  !--------------- ��ʬ�׻� -----------------
    function eee_Lapla_eee(eee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy+��zz)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (l**2 + m**2 + n**2) �򤫤���
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Lapla_eee
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�

         do l=-lm,lm
            do m=-mm,mm
               do n=-nm,nm
                  eee_Lapla_eee(n,m,l) = -(l**2+m**2+n**2)*eee(n,m,l)
               enddo
            enddo
         enddo
    end function eee_Lapla_eee

    function eee_LaplaInv_eee(eee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy+��zz)**(-1)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (l**2 + m**2 + n**2) �ǳ��
      ! �׻���ԤäƤ���. l=m=n=0 ��ʬ�ˤ� 0 ���������Ƥ���. 
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)             :: eee_LaplaInv_eee
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in) :: eee
      !(in) ���ڥ��ȥ�ǡ���

      integer l,m,n

      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               if ( l.ne.0 .or. m.ne.0 .or. n.ne.0 ) then
                  eee_LaplaInv_eee(n,m,l) = -eee(n,m,l)/(l**2+m**2+n**2)
               else
                  eee_LaplaInv_eee(n,m,l) = 0.0
               endif
            enddo
         enddo
      enddo
    end function eee_LaplaInv_eee

    function eee_Dx_eee(eee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� l �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dx_eee
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dx_eee(n,m,l) = -l*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dx_eee

    function eee_Dy_eee(eee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� Y ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� Y �����ȿ� m �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dy_eee
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dy_eee(n,m,l) = -m*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dy_eee

    function eee_Dz_eee(eee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Z ��ʬ(��z)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� Z ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Z ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� Z �����ȿ� n �򤫤���
      ! ���� <-> ������ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dz_eee
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer l,m,n
      ! ����ѿ�


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dz_eee(n,m,l) = -n*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dz_eee

  !--------------- ��������׻� -----------------

    function eee2_RotVelxVor_eee2(eee2)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ������� Euler ����������������
      !
      !     ��x(u x ��) 
      !
      !  �� 2 ��ʬ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)     :: eee2_RotVelxVor_eee2
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      real(8), dimension((2*lm+1)*(2*mm+1)*(2*nm+1))      :: ws
      real(8), dimension(km*jm*im*4)                      :: wg

      call p3elnl(nm,mm,lm,km,jm,im,eee2,eee2_RotVelxVor_eee2, &
                  ws,wg,itk,tk,itj,tj,iti,ti)

    end function eee2_RotVelxVor_eee2

    function eee_VorFromZeta_eee2(eee2,isw)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ������鱲�٤Υ��ڥ��ȥ�� 1 ��ʬ
      !  ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)     :: eee_VorFromZeta_eee2
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      integer, intent(IN) :: isw
      !(in) ���Ϥ��뱲�٤���ʬ�Υ���ǥå���(1,2,3)

      call p3geto(nm,mm,lm,eee2,eee_VorFromZeta_eee2,isw)

    end function eee_VorFromZeta_eee2

    function eee_VelFromZeta_eee2(eee2,isw)
      !
      !  ���� 2 ��ʬ(��_1, ��_2)�Υ��ڥ��ȥ�ǡ�������®�٥��ڥ��ȥ�� 1 ��ʬ
      !  ��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)     :: eee_VelFromZeta_eee2
      !(out) ��������Υ��ڥ��ȥ�ǡ����� 2 �Ĥ���ʬ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) ���ϥ��ڥ��ȥ�ǡ���. ���٤� 2 ��ʬ(��_1, ��_2)

      integer, intent(IN) :: isw
      !(in) ���Ϥ��뱲�٤���ʬ�Υ���ǥå���(1,2,3)

      call p3getu(nm,mm,lm,eee2,eee_VelFromZeta_eee2,isw)

    end function eee_VelFromZeta_eee2

    function eee2_ZetaFromVor_eee_eee_eee(eee_1,eee_2,eee_3)
      !
      !  ���٤Υ��ڥ��ȥ뤫�鱲�� 2 ��ʬ(��_1, ��_2)��׻�����.
      !
      !  (��_1, ��_2) �ȱ��� �� �Ȥδط��� ISPACK/P3PACK �Υޥ˥奢��򻲾�
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)     :: eee2_ZetaFromVor_eee_eee_eee
      !(out) ���٤� 2 ��ʬ(��_1, ��_2)

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee_1, eee_2, eee_3
      !(in) ���٥��ڥ��ȥ�ǡ����γ���ʬ

      integer :: l,m,n

      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_2(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_3(n,m,l)
               elseif( m /= 0 ) then
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_3(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_1(n,m,l)
               else
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_1(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_2(n,m,l)
               endif
            enddo
         enddo
      enddo

    end function eee2_ZetaFromVor_eee_eee_eee

  !--------------- ��ʬ�׻� -----------------
    function IntZYX_zyx(zyx)
      !
      ! 2 �����ʻ����ǡ��������ΰ���ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in)  3 �����ʻ����ǡ���

      real(8)                                    :: IntZYX_zyx
      !(out) ��ʬ��

      integer :: i, j, k
      ! ����ѿ�

      IntZYX_zyx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            do k=0,km-1
               IntZYX_zyx = IntZYX_zyx &
                    + zyx(k,j,i) * z_Z_Weight(k) * y_Y_Weight(j) * x_X_Weight(i)
            enddo
         enddo
      end do
    end function IntZYX_zyx

    function z_IntYX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X,Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_IntYX_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i, j
      ! ����ѿ�

      z_IntYX_zyx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            z_IntYX_zyx(:) = &
                 z_IntYX_zyx(:) + zyx(:,j,i) * x_X_Weight(i)* y_Y_Weight(j)
         enddo
      enddo
    end function z_IntYX_zyx

    function y_IntZX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X,Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_IntZX_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i, k
      ! ����ѿ�

      y_IntZX_zyx = 0.0d0
      do i=0,im-1
         do k=0,km-1
            y_IntZX_zyx(:) = &
                 y_IntZX_zyx(:) + zyx(k,:,i) * x_X_Weight(i)* z_Z_Weight(k)
         enddo
      enddo
    end function y_IntZX_zyx

    function x_IntZY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Y,Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_IntZY_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: j, k
      ! ����ѿ�

      x_IntZY_zyx = 0.0d0
      do j=0,jm-1
         do k=0,km-1
            x_IntZY_zyx(:) = &
                 x_IntZY_zyx(:) + zyx(k,j,:) * y_Y_Weight(j)* z_Z_Weight(k)
         enddo
      enddo
    end function x_IntZY_zyx

    function zy_IntX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:jm-1)          :: zy_IntX_zyx
      !(out) ��ʬ���줿 2 ����(ZY)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      zy_IntX_zyx = 0.0d0
      do i=0,im-1
         zy_IntX_zyx(:,:) = zy_IntX_zyx(:,:) + zyx(:,:,i) * x_X_Weight(i)
      enddo

    end function zy_IntX_zyx

    function zx_IntY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntY_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      zx_IntY_zyx = 0.0d0
      do j=0,jm-1
         zx_IntY_zyx(:,:) = zx_IntY_zyx(:,:) + zyx(:,j,:) * y_Y_Weight(j)
      enddo
    end function zx_IntY_zyx

    function yx_IntZ_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm-1,0:im-1)          :: yx_IntZ_zyx
      !(out) ��ʬ���줿 1 ����(YX)�ʻ����ǡ���

      integer :: k 
      ! ����ѿ�

      yx_IntZ_zyx = 0.0d0
      do k=0,km-1
         yx_IntZ_zyx(:,:) = yx_IntZ_zyx(:,:) + zyx(k,:,:) * z_Z_Weight(k)
      enddo

    end function yx_IntZ_zyx

    function IntYX_yx(yx)
      !
      ! 2 ����(YX)�ʻ����ǡ��������ΰ���ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx          
      !(in)  2 ����(YX)�ʻ����ǡ���

      real(8)                             :: IntYX_yx
      !(out) ��ʬ��

      integer :: i, j
      ! ����ѿ�

      IntYX_yx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            IntYX_yx = IntYX_yx + yx(j,i) * y_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo
    end function IntYX_yx

    function y_IntX_yx(yx)
      !
      ! 2 ����(YX)�ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in) 2 ����(YX)�ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_IntX_yx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)
      !
      ! 2 ����(YX)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx      
      !(in)  2 ����(YX)�ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntY_yx 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      x_IntY_yx = 0.0d0
      do j=0,jm-1
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntZY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8)                             :: IntZY_zy
      !(out) ��ʬ��

      integer :: j, k
      ! ����ѿ�

      IntZY_zy = 0.0d0
      do j=0,jm-1
         do k=0,km-1
            IntZY_zy = IntZY_zy + zy(k,j) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZY_zy

    function y_IntZ_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Z ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_IntZ_zy
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: k
      ! ����ѿ�

      y_IntZ_zy = 0.0d0
      do k=0,km-1
         y_IntZ_zy(:) = y_IntZ_zy(:) + zy(k,:) * z_Z_Weight(k)
      enddo
    end function y_IntZ_zy

    function z_IntY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)        :: z_IntY_zy 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      z_IntY_zy = 0.0d0
      do j=0,jm-1
         z_IntY_zy(:) = z_IntY_zy(:) + zy(:,j) * y_Y_Weight(j)
      enddo
    end function z_IntY_zy

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

    function IntY_y(y)
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1)   :: y          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntY_y     !(out) ��ʬ��

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

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
    function AvrZYX_zyx(zyx)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight, z_Z_Weight ��
      ! ���������¤�׻���, x_X_Weight*y_Y_Weight*z_Z_Weight �����¤ǳ��
      ! ���Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in)  3 �����ʻ����ǡ���

      real(8)                                    :: AvrZYX_zyx
      !(out) ��ʬ��

      AvrZYX_zyx = IntZYX_zyx(zyx)/(sum(x_X_weight)*sum(y_Y_weight)*sum(z_Z_weight))

    end function AvrZYX_zyx

    function z_AvrYX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X,Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_AvrYX_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      z_AvrYX_zyx = z_IntYX_zyx(zyx)/(sum(x_X_weight)*sum(y_Y_weight))

    end function z_AvrYX_zyx

    function y_AvrZX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X,Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_AvrZX_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      y_AvrZX_zyx = y_IntZX_zyx(zyx)/(sum(x_X_weight)*sum(z_Z_weight))

    end function y_AvrZX_zyx

    function x_AvrZY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Y,Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight, z_Z_Weight �򤫤���
      ! ���¤�׻���, y_Y_Weight*z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrZY_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      x_AvrZY_zyx = x_IntZY_zyx(zyx)/(sum(y_Y_weight)*sum(z_Z_weight))

    end function x_AvrZY_zyx

    function zy_AvrX_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:jm-1)          :: zy_AvrX_zyx
      !(out) ��ʬ���줿 2 ����(ZY)�ʻ����ǡ���

      zy_AvrX_zyx = zy_IntX_zyx(zyx)/sum(x_X_weight)

    end function zy_AvrX_zyx

    function zx_AvrY_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤���
      ! ���¤�׻���, y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:km-1,0:im-1)          :: zx_AvrY_zyx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      zx_AvrY_zyx = zx_IntY_zyx(zyx)/sum(y_Y_weight)

    end function zx_AvrY_zyx

    function yx_AvrZ_zyx(zyx)
      !
      ! 3 �����ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤���
      ! ���¤�׻���, z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm-1,0:im-1)          :: yx_AvrZ_zyx
      !(out) ��ʬ���줿 1 ����(YX)�ʻ����ǡ���

      yx_AvrZ_zyx = yx_IntZ_zyx(zyx)/sum(z_Z_weight)

    end function yx_AvrZ_zyx

    function AvrYX_yx(yx)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in)  2 �����ʻ����ǡ���

      real(8)                             :: AvrYX_yx    
      !(out) ʿ����

      AvrYX_yx = IntYX_yx(yx)/(sum(x_X_weight)*sum(y_Y_weight))
    end function AvrYX_yx

    function y_AvrX_yx(yx)
      !
      ! 2 �����ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_AvrX_yx
      !(out) ʿ�Ѥ��줿 1 ����(Y)�ʻ����ǡ���

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)
      !
      ! 2 �����ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrY_yx
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

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

    function AvrZY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, z_Z_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in)  2 ����(ZY)�ʻ����ǡ���

      real(8)                             :: AvrZY_zy    
      !(out) ʿ����

      AvrZY_zy = IntZY_zy(zy)/(sum(y_Y_weight)*sum(z_Z_weight))
    end function AvrZY_zy

    function z_AvrY_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in) 2 ����(ZY)�ʻ����ǡ���

      real(8), dimension(0:km-1)          :: z_AvrY_zy
      !(out) ʿ�Ѥ��줿 1 ����(Z)�ʻ����ǡ���

      z_AvrY_zy = z_IntY_zy(zy)/sum(y_Y_weight)
    end function z_AvrY_zy

    function y_AvrZ_zy(zy)
      !
      ! 2 ����(ZY)�ʻ����ǡ����� Z ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� z_Z_Weight �򤫤������¤�׻���, 
      ! z_Z_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in) 2 ����(ZY)�ʻ����ǡ���

      real(8), dimension(0:jm-1)          :: y_AvrZ_zy
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      y_AvrZ_zy = y_IntZ_zy(zy)/sum(z_Z_weight)
    end function y_AvrZ_zy

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

    function AvrY_y(y)
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1), intent(IN) :: y      !(in)  1 �����ʻ����ǡ���
      real(8)                                :: AvrY_y !(out) ʿ����

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

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
    function EnergyHelicityFromZeta_eee2(eee2)
      !
      ! ������ʬ(��_1, ��_2)�������ΰ�ʿ�ѥ��ͥ륮���ȥإꥷ�ƥ�����׻�����. 
      !
      real(8), dimension(2)                         :: EnergyHelicityFromZeta_eee2
      ! ���ͥ륮���إꥷ�ƥ���

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in) :: eee2
      ! ������ʬ(��_1, ��_2)

      real(8) :: E, H           ! ���ͥ륮��, �إꥷ�ƥ���
      
      call p3cnsv(nm,mm,lm,eee2,E,H)

      EnergyHelicityFromZeta_eee2(1) = E ; EnergyHelicityFromZeta_eee2(2) = H

    end function EnergyHelicityFromZeta_eee2

    subroutine ESpectralFromZeta(esp,eee2)
      !
      ! ������ʬ(��_1, ��_2)���饨�ͥ륮�����ڥ��ȥ��׻�����. 
      !
      !   * esp �Υ������ǵ������ȿ��ϰϤ�������
      !   * esp �����¤� EFFromZeta �ǵ����륨�ͥ륮��
      !     (���ΰ�ʿ����)��������
      !
      real(8), dimension(:), intent(OUT)  :: esp
      ! ���ͥ륮�����ڥ��ȥ�

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in) :: eee2
      ! ������ʬ(��_1, ��_2)

      integer kmax
      ! ����ѿ�

      kmax=size(esp)
      call p3espt(nm,mm,lm,kmax,eee2,esp)

    end subroutine ESpectralFromZeta

end module eee_module
