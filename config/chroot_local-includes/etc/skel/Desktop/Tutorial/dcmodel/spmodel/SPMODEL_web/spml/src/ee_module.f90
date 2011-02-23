!--
!----------------------------------------------------------------------
!     Copyright 2001--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module
!
!      spml/ee_module �⥸�塼��ϼ����������β��Ǥ� 2 ��������ΰ��
!      ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ���
!      �󶡤���. 
!
!      ������ ISPACK/P2PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!      ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ�
!      ISPACK/P2PACK �Υޥ˥奢��򻲾Ȥ��줿��. 
!
!����  2001/10/07  �ݹ�����
!      2001/12/25  �ݹ�����  �ؿ�, �ѿ�̿̾ˡ�ѹ�
!      2002/03/25  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/08/19  �ݹ�����  �ʻҥǡ���ź���� gg -> xy ���ѹ�
!      2002/08/20  �ݹ�����  ��ʬ��ʿ�Ѵؿ��ɲ�
!      2005/03/15  �ݹ�����  xy -> yx ����Ƭ�Ҥ��ѹ�
!      2005/07/18  �ݹ�����  ���Ѥ��� ISPACK �饤�֥��� p2pack ���ѹ�
!      2005/10/26  �ݹ�����  ���ͥ륮��, ���󥹥ȥ�ե������ڥ��ȥ��ɲ�
!      2006/03/05  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2006/03/19  �ݹ�����  �ѿ�����³����������򥳥��Ȥ��ɲ�
!      2008/05/10  ��߷���� => �ݹ����� �������ΰ�ʣ���б�
!      2008/10/28  �ݹ�����  ��ַ׻��ɲ�
!      2009/01/09  �ݹ�����  ee_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ˽���
!      2009/07/31  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!
!++
module ee_module
  !
  != ee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ee_module.f90,v 1.22 2009-07-31 03:06:18 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/ee_module �⥸�塼��ϼ����������β��Ǥ� 2 ��������ΰ��
  ! ή�α�ư�򥹥ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ���
  ! �󶡤���. 
  !
  ! ������ ISPACK/P2PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ�
  ! ISPACK/P2PACK �Υޥ˥奢��򻲾Ȥ��줿��. 
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !  
  ! * �ؿ�̾����Ƭ (ee_, yx_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   ee_ :: ���ڥ��ȥ�ǡ���(�� 1,2 ���������줾�� Y,X �����ȿ�)
  !   yx_ :: 2 �����ʻ����ǡ���(�� 1,2 ���������줾�� Y,X �����γʻ���)
  !   x_  :: X ���� 1 �����ʻ����ǡ���, y_ : Y ���� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Lapla, LaplaInv, Jacobian)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_ee_ee, _ee, _yx, _x, _y) ��, �����ѿ��η���
  !   ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _ee    :: ���ڥ��ȥ�ǡ���
  !   _ee_ee :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _yx    :: 2 �����ʻ����ǡ���
  !   _x     :: X ���� 1 �����ʻ����ǡ���
  !   _y     :: Y ���� 1 �����ʻ����ǡ���.
  !  
  !=== �ƥǡ����μ��������
  !  
  ! * yx : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:jm-1,0:im-1). 
  !   * im, jm �Ϥ��줾�� X, Y ��ɸ�γʻ������Ǥ���, ���֥롼���� 
  !     ee_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * �� 1 ������ Y ��ɸ�γʻ��������ֹ�, �� 2 ������ X ��ɸ��
  !     �ʻ��������ֹ�Ǥ��� (X, Y �ν�ǤϤʤ�)���Ȥ����.
  !
  ! * ee : ���ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-lm:lm,-km:km). 
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, ���֥롼���� 
  !     ee_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !     (X, Y �����ȿ��ν�ǤϤʤ�)���Ȥ����. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * x, y : X, Y ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾��
  !     real(8), dimension(0:im-1) ����� real(8), dimension(0:jm-1).
  !
  ! * ee_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
  !
  ! * yx_ �ǻϤޤ�ؿ����֤��ͤ� 2 �����ʻ����ǡ�����Ʊ��.
  !
  ! * x_, y_ �ǻϤޤ�ؿ����֤��ͤ� 1 �����ʻ����ǡ�����Ʊ��.
  !
  ! * ���ڥ��ȥ�ǡ������Ф�����ʬ���κ��ѤȤ�, �б�����ʻ����ǡ�����
  !   ��ʬ�ʤɤ���Ѥ������ǡ����򥹥ڥ��ȥ��Ѵ�������Τ��ȤǤ���.
  !
  !== �ѿ�����³����������
  !
  !==== ����� 
  !
  ! ee_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! ee_ChangeResolutionDomain :: ������, �ΰ�������ѹ�
  !
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y     ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, y_Y_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! yx_X, yx_Y   :: �ʻ����ǡ����� XY ��ɸ(X,Y)(�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! yx_ee :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ee_yx :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! ee_Lapla_ee       :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! ee_LaplaInv_ee    :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! ee_Dx_ee          :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! ee_Dy_ee          :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! ee_Jacobian_ee_ee :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntYX_yx, AvrYX_yx   :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntX_yx, y_AvrX_yx :: 2 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx :: 2 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntY_y, AvrY_y       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  !==== ���ڥ��ȥ����
  !
  ! ee_EnergyFromStreamfunc_ee    :: 
  ! ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����
  !
  ! ee_EnstrophyFromStreamfunc_ee :: 
  ! ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��׻�����
  !
  !==== ��ַ׻�
  !
  ! Interpolate_ee       :: Ǥ�դ������ͤ򥹥ڥ��ȥ�ǡ�������׻�����
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public ee_Initial                                       ! ������롼����
  public ee_ChangeResolutionDomain                        ! �������ΰ������ѹ�
  public yx_ee, ee_yx                                     ! �����Ѵ�
  public ee_Lapla_ee, ee_LaplaInv_ee, ee_Dx_ee, ee_Dy_ee  ! ��ʬ
  public ee_JacobianZ_ee, ee_Jacobian_ee_ee               ! �������׻�
  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! ��ʬ
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! ʿ��
  public ee_EnergyFromStreamfunc_ee                       ! ���ͥ륮��
  public ee_EnstrophyFromStreamfunc_ee                    ! ���󥹥ȥ�ե���
  public Interpolate_ee                                   ! ���
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! ��ɸ�ѿ�

  integer   :: im=32, jm=32                      ! �ʻ���������(X,Y)
  integer   :: km=10, lm=10                      ! �����ȿ�������(X,Y)
  real(8)   :: xl=1.0, yl=1.0                    ! �ΰ���礭��

  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()

  real(8), dimension(:),   pointer :: x_X => null()
                                         ! �ʻ�����ɸ(X)
  real(8), dimension(:),   pointer :: y_Y => null()
                                         ! �ʻ�����ɸ(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! �ʻ����Ť�(X)
                                         ! X �����γʻ����δֳ֤���Ǽ���Ƥ���.
  real(8), dimension(:),   pointer :: y_Y_Weight => null()
                                         ! �ʻ����Ť�(Y)
                                         ! Y �����γʻ����δֳ֤���Ǽ���Ƥ���.

  real(8), dimension(:,:), pointer :: yx_X => null()
                          ! �ʻ���(X)��ɸ(2 ����)
                          ! �Ƴʻ���(i,j)�ΰ��֤� X ��ɸ���Ǽ�����ʻҥǡ���
  real(8), dimension(:,:), pointer :: yx_Y => null()
                          ! �ʻ���(Y)��ɸ(2 ����)
                          ! �Ƴʻ���(i,j)�ΰ��֤� Y ��ɸ���Ǽ�����ʻҥǡ���


  integer, parameter :: nparams_max = 10 ! ee_Initial ��Ƥ٤������
  type ee_param                          ! �������ΰ����¤��
     integer   :: im, jm
     integer   :: km, lm
     real(8)   :: xl, yl
     integer, dimension(:),   pointer :: itj
     real(8), dimension(:),   pointer :: tj
     integer, dimension(:),   pointer :: iti
     real(8), dimension(:),   pointer :: ti
     real(8), dimension(:),   pointer :: x_X
     real(8), dimension(:),   pointer :: y_Y
     real(8), dimension(:),   pointer :: x_X_Weight
     real(8), dimension(:),   pointer :: y_Y_Weight
     real(8), dimension(:,:), pointer :: yx_X
     real(8), dimension(:,:), pointer :: yx_Y 
  end type ee_param
  type(ee_param) :: params(nparams_max)  ! �������ΰ����
  integer :: nparams                     ! �������ΰ����θĿ�

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, itj, tj, iti, ti, xl, yl
  save x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y
  save params, nparams

  contains
  !--------------- ����� -----------------
    subroutine ee_Initial(i,j,k,l,xmin,xmax,ymin,ymax,id)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭�������ꤹ��.
      !
      ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      ! ���ץ���ʥ���� id ���Ѥ��ưۤʤ������, �ΰ��Ʊ����
      ! �������Ȥ��Ǥ���. ee_Initial �������, �ΰ褴�Ȥ˸Ƥ��
      ! ���� id �򥭡��פ�, ee_ChangeResolutionDomain �����ؤ���. 
      !
      integer,intent(in) :: i           ! �ʻ�����(X)
      integer,intent(in) :: j           ! �ʻ�����(Y)
      integer,intent(in) :: K           ! �����ȿ�(X)
      integer,intent(in) :: l           ! �����ȿ�(Y)

      real(8),intent(in) :: xmin, xmax     ! X ��ɸ�ϰ�
      real(8),intent(in) :: ymin, ymax     ! Y ��ɸ�ϰ�

      integer, intent(out), optional :: id  ! �������ΰ�����ֹ�

      character(len=3) cid
      integer :: ii, jj

      im = i         ; jm = j
      km = k         ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','ee_initial',&
              'too many call of ee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%xl = xl
      params(nparams)%yl = yl

      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%y_Y(0:jm-1))
      allocate(params(nparams)%y_Y_Weight(0:jm-1))
      allocate(params(nparams)%yx_X(0:jm-1,0:im-1))
      allocate(params(nparams)%yx_Y(0:jm-1,0:im-1))

      call ee_ChangeResolutionDomain(nparams)

      call p2init(jm,im,itj,tj,iti,ti)

      do ii=0,im-1
         x_X(ii) = xmin + xl/im*ii
      enddo
      x_X_Weight = xl/im

      do jj=0,jm-1
         y_Y(jj) = ymin + yl/jm*jj
      enddo
      y_Y_Weight = yl/jm

      yx_X = spread(x_X,1,jm)
      yx_Y = spread(y_Y,2,im)

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','ee_initial','ee_module (2009/07/31) is initialized')
      call MessageNotify('M','ee_initial',&
           'ResolutionDomain ID is '//trim(adjustl(cid)))
    end subroutine ee_initial

  !--------------- id �ѹ� -----------------
    subroutine ee_ChangeResolutionDomain(id)
      !
      ! ������, �ΰ�������ѹ�. ee_Initial �����ꤹ��ݤ�
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
      xl = params(id)%xl
      yl = params(id)%yl
      itj => params(id)%itj
      tj => params(id)%tj
      iti => params(id)%iti
      ti => params(id)%ti
      x_X => params(id)%x_X
      y_Y => params(id)%y_Y
      x_X_Weight => params(id)%x_X_Weight
      y_Y_Weight => params(id)%y_Y_Weight
      yx_X => params(id)%yx_X
      yx_Y => params(id)%yx_Y

    end subroutine ee_ChangeResolutionDomain

  !--------------- �����Ѵ� -----------------
    function yx_ee(ee)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm-1,0:im-1)             :: yx_ee 
      !(out) �ʻ����ǡ���

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee
      !(in)  ���ڥ��ȥ�ǡ���

      real(8), dimension(jm*im)                     :: w
      ! ���������

      call p2s2ga(lm,km,jm,im,ee,yx_ee,w,itj,tj,iti,ti)
    end function yx_ee

    function ee_yx(yx)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_yx
      !(out)  ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm-1,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      real(8), dimension(jm*im)                     :: w
      real(8), dimension(0:jm-1,0:im-1)             :: yx_tmp
      ! ���������

      yx_tmp = yx
      call p2g2sa(lm,km,jm,im,yx_tmp,ee_yx,w,itj,tj,iti,ti)

    end function ee_yx

  !--------------- ��ʬ�׻� -----------------
    function ee_Lapla_ee(ee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �򤫤���
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Lapla_ee
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer k,l
      ! ����ѿ�

      do k=-km,km
         do l=-lm,lm
            ee_Lapla_ee(l,k) = -((2*pi*k/xl)**2+(2*pi*l/yl)**2)*ee(l,k)
         enddo
      enddo
    end function ee_Lapla_ee

    function ee_LaplaInv_ee(ee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy)**(-1)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �ǳ��
      ! �׻���ԤäƤ���. k=l=0 ��ʬ�ˤ� 0 ���������Ƥ���. 
      !
      real(8), dimension(-lm:lm,-km:km)             :: ee_LaplaInv_ee
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee
      !(in) ���ڥ��ȥ�ǡ���

      integer k,l

      do k=-km,km
         do l=-lm,lm
            if ( k.ne.0 .or. l.ne.0 ) then
               ee_LaplaInv_ee(l,k) = -ee(l,k)/((2*pi*k/xl)**2+(2*pi*l/yl)**2)
            else
               ee_LaplaInv_ee(l,k) = 0.0
            endif
         enddo
      enddo
    end function ee_LaplaInv_ee

    function ee_Dx_ee(ee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Dx_ee
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer k,l
      ! ����ѿ�

      do k=-km,km
         do l=-lm,lm
            ee_Dx_ee(l,k) = -(2*pi*k/xl)*ee(-l,-k)
         enddo
      enddo
    end function ee_Dx_ee

    function ee_Dy_ee(ee)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� l �򤫤���
      ! sin(ky) <-> cos(ky) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Dy_ee
      !(out) ���ڥ��ȥ�ǡ����� Y ��ʬ

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer k,l
      ! ����ѿ�

      do k=-km,km
         do l=-lm,lm
            ee_Dy_ee(l,k) = -(2*pi*l/yl)*ee(-l,-k)
         enddo
      enddo

    end function ee_Dy_ee

    function ee_Jacobian_ee_ee(ee_a,ee_b)
      !
      !  2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ���
      !
      !     J(A,B)=(��xA)(��yB)-(��yA)(��xB)
      !
      !  ��׻�����.
      !
      !  2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���Ȥ�, �б����� 2 �Ĥ�
      !  �ʻ����ǡ����Υ䥳�ӥ���Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Jacobian_ee_ee
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_a
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_b
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      real(8), dimension((2*km+1)*(2*lm+1))          :: ws
      real(8), dimension(jm*im*3)                    :: wg
      ! ����ΰ�

      call p2ajcb(lm,km,jm,im,ee_a,ee_b,ee_Jacobian_ee_ee,ws,wg,itj,tj,iti,ti)

      ee_Jacobian_ee_ee  = (2*pi/xl)*(2*pi/yl) * ee_Jacobian_ee_ee

    end function ee_Jacobian_ee_ee

    function ee_JacobianZ_ee(ee_zeta)
      !
      ! ���٥��ڥ��ȥ�ǡ��� �� ����ή���ؿ��ȱ��٤Υ䥳�ӥ���
      !
      !     J(��,��)=(��x��)(��y��)-(��y��)(��x��)
      !
      !  ��׻�����. �������� �� (��xx+��yy)��=�� ��������ή���ؿ��Ǥ���.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_JacobianZ_ee
      !(out) ή���ؿ��ȱ��٤Υ䥳�ӥ���

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_Zeta
      !(in) ���٥��ڥ��ȥ�ǡ���

      real(8), dimension((2*km+1)*(2*lm+1))          :: ws
      real(8), dimension(jm*im*3)                    :: wg
      ! ����ΰ�

      call p2ajbs(lm,km,jm,im,yl/xl,ee_Zeta,ee_JacobianZ_ee,ws,wg,itj,tj,iti,ti)

      ee_JacobianZ_ee = (2*pi/xl)*(2*pi/yl)/(2*pi/yl)**2 * ee_JacobianZ_ee

    end function ee_JacobianZ_ee

  !--------------- ��ʬ�׻� -----------------
    function IntYX_yx(yx)
      !
      ! 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx          
      !(in)  2 �����ʻ����ǡ���

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
      ! 2 �����ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

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
      ! 2 �����ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx      
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntY_yx 
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      x_IntY_yx = 0.0d0
      do j=0,jm-1
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

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

    function IntY_y(y)      ! Y ������ʬ
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm-1)   :: y          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntY_y     !(out) ��ʬ��

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

  !--------------- ʿ�ѷ׻� -----------------
    function AvrYX_yx(yx)
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx
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
      real(8), dimension(0:jm-1,0:im-1)   :: yx
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
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)          :: x_AvrY_yx
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

    function AvrX_x(x)
      !
      ! 1 ����(X)�ʻ����ǡ����� X ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:im-1)   :: x          !(in)  1 �����ʻ����ǡ���
      real(8)                      :: AvrX_x     !(out) ʿ����

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrY_y(y)
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm-1) :: y          !(in)  1 �����ʻ����ǡ���
      real(8)                    :: AvrY_y     !(out) ʿ����

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  !--------------- ���ڥ��ȥ�׻� -----------------
    function ee_EnergyFromStreamfunc_ee(ee_StrFunc)
      !
      ! ή���ؿ����饨�ͥ륮�����ڥ��ȥ��׻�����. 
      !
      !   E_kl = (1/2)(k^2+l^2)|\psi_kl|^2
      !
      ! * E_kl �����¤����ʿ�ѱ�ư���ͥ륮���Ȥʤ�. 
      ! * ������ΰ�����Ѥ򤫤��������ư���ͥ륮���Ȥʤ�. 
      !
      real(8), dimension(-lm:lm,-km:km)    :: ee_EnergyFromStreamfunc_ee
      ! ���ͥ륮�����ڥ��ȥ�

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee_StrFunc
      ! ή���ؿ�

      integer k,l
      ! ����ѿ�

      do k=-km,km
         do l=-lm,lm
            ee_EnergyFromStreamfunc_ee(l,k) &
                 = 0.5 * ( (2*pi*k/xl)**2 + (2*pi*l/yl)**2 ) & 
                          * (ee_StrFunc(l,k)**2 + ee_StrFunc(-l,-k)**2)
         enddo
      enddo

    end function ee_EnergyFromStreamfunc_ee

    function ee_EnstrophyFromStreamfunc_ee(ee_StrFunc)
      !
      ! ή���ؿ����饨�󥹥ȥ�ե������ڥ��ȥ��׻�����. 
      !
      !   Q_kl = (1/2)(k^2+l^2)^2|\psi_kl|^2
      !
      ! * Q_kl �����¤����ʿ�ѥ��󥹥ȥ�ե����Ȥʤ�. 
      ! * ������ΰ�����Ѥ򤫤���������󥹥ȥ�ե����Ȥʤ�. 
      !

      real(8), dimension(-lm:lm,-km:km)    :: ee_EnstrophyFromStreamfunc_ee
      ! ���󥹥ȥ�ե��������ڥ��ȥ�

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee_StrFunc
      ! ή���ؿ�

      integer k,l
      ! ����ѿ�

      do k=-km,km
         do l=-lm,lm
            ee_EnstrophyFromStreamfunc_ee(l,k) &
                 = 0.5 * ( (2*pi*k/xl)**2 + (2*pi*l/yl)**2 )**2 & 
                          * (ee_StrFunc(l,k)**2 + ee_StrFunc(-l,-k)**2)
         enddo
      enddo

    end function ee_EnstrophyFromStreamfunc_ee

  !--------------- ��ַ׻� -----------------
    function Interpolate_ee( ee_Data, x, y )
      real(8), intent(IN)  :: ee_data(-lm:lm,-km:km)  ! ���ڥ��ȥ�ǡ���
      real(8), intent(IN)  :: x                       ! ��֤������� x ��ɸ 
      real(8), intent(IN)  :: y                       ! ��֤������� y ��ɸ 
      real(8)              :: Interpolate_ee          ! ��֤�����

      integer :: k, l
      real(8) :: xx, yy

      xx =(2*PI/xl)*(x - x_X(0))
      yy =(2*PI/yl)*(y - y_Y(0))

      Interpolate_ee = ee_Data(0,0)

      ! l=0
      do k=1,km
         Interpolate_ee = Interpolate_ee &
              + 2*( ee_Data(0,k)*cos(k*xx) - ee_Data(0,-k)*sin(k*xx) )
      end do

      ! k=0
      do l=1,lm
         Interpolate_ee = Interpolate_ee &
              + 2*( ee_Data(l,0)*cos(l*yy) - ee_Data(-l,0)*sin(l*yy) )
      end do

      ! k*l > 0
      do l=1,lm
         do k=1,km
            Interpolate_ee = Interpolate_ee &
              + 2*(  ee_Data(l,k)*(   cos(k*xx)*cos(l*yy)   &
                                    - sin(k*xx)*sin(l*yy) ) &
                    -ee_Data(-l,-k)*(   sin(k*xx)*cos(l*yy)   &
                                      + cos(k*xx)*sin(l*yy) ) )
         end do
      end do

      ! k*l < 0
      do l=1,lm
         do k=1,km
            Interpolate_ee = Interpolate_ee &
              + 2*(  ee_Data(-l,k)*(   cos(k*xx)*cos(l*yy)   &
                                     + sin(k*xx)*sin(l*yy) ) &
                    -ee_Data(l,-k)*(   sin(k*xx)*cos(l*yy)   &
                                     - cos(k*xx)*sin(l*yy) ) )
         end do
      end do

    end function Interpolate_ee

end module ee_module
