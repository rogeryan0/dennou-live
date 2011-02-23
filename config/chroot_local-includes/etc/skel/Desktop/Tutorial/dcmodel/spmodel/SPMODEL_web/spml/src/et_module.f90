!--
!----------------------------------------------------------------------
! Copyright(c) 2002-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module
!      2 ������ϩ�ΰ�����, Fourier Ÿ�� + Chebyshev Ÿ��ˡ
!
!      spml/et_module �⥸�塼��� 2 ������ϩ�ΰ�Ǥ�ή�α�ư��
!      ���ڥ��ȥ�ˡ�ˤ����ͷ׻���¹Ԥ��뤿��� Fortran90 �ؿ����󶡤���. 
!      ����Ū�ʶ������򰷤������ X �����ؤΥա��ꥨ�Ѵ��ȶ����ɤ򰷤������
!      Y �����Υ����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
!      �ؿ����󶡤���. 
!
!      ������ ae_module, at_module ���Ѥ��Ƥ���. 
!      �ǲ����Ǥϥա��ꥨ�Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
!      ISPACK/FTPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
!
!
!����  2002/01/27  �ݹ�����
!      2002/03/30  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/08/19  �ݹ�����  �ʻҥǡ���ź���� gg -> xy, eg -> ey ���ѹ�
!                            ��ʬ��ʿ�Ѵؿ��ɲ�
!      2005/01/09  �ݹ�����  msgdmp -> MessageNotify ���ѹ�
!      2005/03/15  �ݹ�����  xy -> yx ����Ƭ�Ҥ��ѹ�
!      2006/03/04  �ݹ�����  ;�פ��ѿ�����(et_Jacobian_et_et)
!      2006/03/06  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2007/11/20  �ݹ�����  ����-ή���ؿ��׻��˹������ꥹ���å�Ƴ��
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2009/01/09  �ݹ�����  et_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ˽���
!      2009/07/31  �ݹ�����  �������׻�������� threadprivate ����(OpenMP)
!      2010/03/10  ��������ʿ  threadprivate ���(����ѥ����¸)
!
!++
module et_module
  !
  != et_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: et_module.f90,v 1.18 2010-03-02 10:09:03 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! 2 ������ϩ�ΰ�����, Fourier Ÿ�� + Chebyshev Ÿ��ˡ
  !
  ! spml/et_module �⥸�塼��� 2 ������ϩ�ΰ�Ǥ�ή�α�ư��
  ! ���ڥ��ȥ�ˡ�ˤ����ͷ׻���¹Ԥ��뤿��� Fortran90 �ؿ����󶡤���. 
  ! ����Ū�ʶ������򰷤������ X �����ؤΥա��ꥨ�Ѵ��ȶ����ɤ򰷤������
  ! Y �����Υ����ӥ������Ѵ����Ѥ�����Υ��ڥ��ȥ�׻��Τ���Τ��ޤ��ޤ�
  ! �ؿ����󶡤���. 
  !
  ! ������ ae_module, at_module ���Ѥ��Ƥ���. 
  ! �ǲ����Ǥϥա��ꥨ�Ѵ�����ӥ����ӥ������Ѵ��Υ��󥸥�Ȥ��� 
  ! ISPACK/FTPACK �� Fortran77 ���֥롼������Ѥ��Ƥ���.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (et_, yx_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   et_ :: 2�������ڥ��ȥ�ǡ���
  !   yx_ :: 2 �����ʻ����ǡ���
  !   x_  :: X ���� 1 �����ʻ����ǡ���
  !   y_  :: Y ���� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Lapla, LaplaInv, Jacobian)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_et_et, _et, _yx, _x, _y) ��, �����ѿ��Υ��ڥ��ȥ�ǡ���
  !   ����ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _et    :: 2�������ڥ��ȥ�ǡ���
  !   _et_et :: 2 �Ĥ�2�������ڥ��ȥ�ǡ���
  !   _yx    :: 2 �����ʻ����ǡ���
  !   _x     :: X ���� 1 �����ʻ����ǡ���
  !   _y     :: Y ���� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * yx : 2 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(0:jm,0:im-1). 
  !   * im, jm �Ϥ��줾�� X, Y ��ɸ�γʻ������Ǥ���, ���֥롼����
  !     et_initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * �� 1 ������ Y ��ɸ�γʻ��������ֹ�, �� 2 ������ X ��ɸ��
  !     �ʻ��������ֹ�Ǥ��� (X, Y �ν�ǤϤʤ�)���Ȥ����.
  !
  ! * et : 2 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,0:lm). 
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, ���֥롼����
  !     et_initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * x, y : X, Y ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1)
  !     ����� real(8), dimension(0:jm).
  !
  ! * e, t : 1 �������ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km) 
  !     ����� real(8), dimension(-lm:lm).
  !
  ! * ax, ay : 1 �����ʻ����ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,0:im-1) 
  !     ����� real(8), dimension(:,0:jm).
  !
  ! * ae, at : 1 �������ڥ��ȥ�ǡ������¤�� 2 ��������.
  !   * �ѿ��μ���ȼ����� real(8), dimension(:,-km:km) 
  !     ����� real(8), dimension(:,0:lm).
  !
  ! * et_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
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
  ! et_Initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y     ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, y_Y_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! yx_X, yx_Y   :: �ʻ����ǡ����� XY ��ɸ(X,Y)(�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! yx_et :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! et_yx :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! ax_ae, x_e :: X �����Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ay_at, y_t :: Y �����Υ��ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! ae_ax, e_x :: X �����γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  ! at_ay, t_y :: Y �����γʻ����ǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! et_Lapla_et  :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! et_Dx_et, ae_Dx_ae, e_Dx_e :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! et_Dy_et, at_Dy_at, t_Dy_t :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! et_Jacobian_et_et :: 2 �ĤΥ��ڥ��ȥ�ǡ�������䥳�ӥ����׻�����
  !
  !==== ����������
  !
  ! et_Boundaries  :: �ǥ��ꥯ��, �Υ��ޥ󶭳�����Ŭ��
  ! et_LaplaInv_et :: ���ڥ��ȥ�ǡ����˥�ץ饷����ε��Ѵ�����Ѥ�����
  ! ey_Vor2Strm_ey :: ���٤���ή����׻�����
  !
  !==== ��ʬ��ʿ��
  !
  ! IntYX_yx, AvrYX_yx   :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntX_yx, y_AvrX_yx :: 2 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx :: 2 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntY_y, AvrY_y       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  use dc_message
  use lumatrix
  use ae_module, x_X => g_X, x_X_weight => g_X_Weight, &
                 e_x => e_g, ae_ax => ae_ag, &
                 x_e => g_e, ax_ae => ag_ae
  use at_module, y_Y => g_X, y_Y_Weight => g_X_Weight, &
                 at_ay => at_ag, t_y => t_g, &
                 ay_at => ag_at, y_t => g_t, &
                 t_Dy_t => t_Dx_t, at_Dy_at => at_Dx_at

  implicit none
  private

  public et_Initial                                       ! �����
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! ��ɸ�ѿ�

  public yx_et, et_yx                                     ! �����Ѵ�
  public e_x, x_e, ae_ax, ax_ae                           ! �����Ѵ�
  public t_y, y_t, at_ay, ay_at                           ! �����Ѵ�

  public et_Dx_et, e_Dx_e, ae_Dx_ae                       ! ��ʬ
  public et_Dy_et, t_Dy_t, at_Dy_at                       ! ��ʬ
  public et_Lapla_et                                      ! ��ʬ

  public et_Jacobian_et_et                                ! �������׻�

  public et_Boundaries                                    ! ����������
  public at_Boundaries_DD, at_Boundaries_DN               ! ����������
  public at_Boundaries_ND, at_Boundaries_NN               ! ����������
  public et_LaplaInv_et, ey_Vor2Strm_ey                   ! ����������
  public et_Vor2Strm_et, et_Vor2Strm1_et                  ! ����������

  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! ��ʬ
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! ʿ��

  integer            :: im=32, jm=8      ! �ʻ���������(X,Y)
  integer            :: km=10, lm=5      ! �����ȿ�������(X,Y)
  real(8)            :: xl=2.0, yl=1.0   ! �ΰ���礭��
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:), allocatable :: yx_X, yx_Y

  save im, jm, km, lm, xl, yl

  contains
  !--------------- ����� -----------------
    subroutine et_Initial(i,j,k,l,xmin,xmax,ymin,ymax)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭�������ꤹ��.
      !
      ! ¾�δؿ����ѿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      integer,intent(in) :: i           ! �ʻ�����(X)
      integer,intent(in) :: j           ! �ʻ�����(Y)
      integer,intent(in) :: k           ! �����ȿ�(X)
      integer,intent(in) :: l           ! �����ȿ�(Y)

      real(8),intent(in) :: xmin, xmax     ! X ��ɸ�ϰ�
      real(8),intent(in) :: ymin, ymax     ! Y ��ɸ�ϰ�

      im = i       ; jm = j
      km = k       ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      call ae_initial(im,km,xmin,xmax)
      call at_initial(jm,lm,ymin,ymax)

      allocate(yx_X(0:jm,0:im-1),yx_Y(0:jm,0:im-1))
      yx_X = spread(x_X,1,jm+1)
      yx_Y = spread(y_Y,2,im)

      call MessageNotify('M','et_initial','et_module (2009/07/31) is initialized')
    end subroutine et_initial

  !--------------- �����Ѵ� -----------------

    function yx_et(et)
      !
      ! ���ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_et
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)  :: et
      !(in) ���ڥ��ȥ�ǡ���

      yx_et = ax_ae(transpose(ay_at(et)))

    end function yx_et

    function et_yx(yx)
      !
      ! �ʻҥǡ������饹�ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,0:lm)              :: et_yx
      !(out) ���ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      et_yx = at_ay(transpose(ae_ax(yx)))

    end function et_yx

  !--------------- ��ʬ�׻� -----------------

    function et_Dx_et(et)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,0:lm)                :: et_Dx_et
      real(8), dimension(-km:km,0:lm), intent(in)    :: et
      integer k

      do k=-km,km
         et_Dx_et(k,:)  =  (-2*pi*k/xl)*et(-k,:)
      enddo
    end function et_Dx_et

    function et_Dy_et(et)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm)               :: et_Dy_et
      !(out) ���ڥ��ȥ�ǡ����� Y ��ʬ

      real(8), dimension(-km:km,0:lm), intent(in)   :: et
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      et_Dy_et = at_Dy_at(et)

    end function et_Dy_et

    function et_Lapla_et(et)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm)                :: et_Lapla_et
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-km:km,0:lm), intent(in)    :: et
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      integer k

      do k=-km,km
         et_Lapla_et(k,:) = -(2*pi*k/xl)**2*et(k,:)
      enddo

      et_Lapla_et = et_Lapla_et + et_Dy_et(et_Dy_et(et))

    end function et_Lapla_et

    function et_Jacobian_et_et(et_a,et_b)
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
      real(8), dimension(-km:km,0:lm)                :: et_Jacobian_et_et
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), dimension(-km:km,0:lm), intent(in)    :: et_a
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)    :: et_b
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      et_Jacobian_et_et = et_yx(&
           yx_et(et_Dx_et(et_a)) * yx_et(et_Dy_et(et_b)) &
           -yx_et(et_Dy_et(et_a)) * yx_et(et_Dx_et(et_b)) )

    end function et_Jacobian_et_et


  !--------------- ���������� -----------------

    subroutine et_Boundaries(et,values,cond)
      !
      ! �ǥ��ꥯ��, �Υ��ޥ����Ŭ��. �����ӥ����ն��֤Ǥη׻�
      !
      ! �ºݤˤ���ǸƤФ�Ƥ��� at_module �Υ��֥롼���� at_Boundaries_DD,
      ! at_Boundaries_DN, at_Boundaries_ND, at_Boundaries_NN ���Ѥ��Ƥ���. 
      ! ������ľ�ܸƤ֤��Ȥ�����.
      !
      real(8), dimension(-km:km,0:lm),intent(inout)      :: et
              ! ��������Ŭ�Ѥ���ǡ���. �������줿�ͤ��֤�. 

      real(8), dimension(-km:km,2), intent(in), optional :: values
              ! �����Ǥ� ��/���� ʬ�ۤ��ʿ���ڥ��ȥ��Ѵ�������Τ�Ϳ����. 
              ! ��ά������/���� 0 �Ȥʤ�. 

      character(len=2), intent(in), optional             :: cond
              ! �������. ��ά���� 'DD'
              !   DD : ξü�ǥ��ꥯ��
              !   DN,ND : �ǥ��ꥯ��/�Υ��ޥ���
              !   NN : ξü�Υ��ޥ�

      if (.not. present(cond)) then
         if (present(values)) then
            call at_Boundaries_DD(et,values)
         else
            call at_Boundaries_DD(et)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_Boundaries_NN(et,values)
         else
            call at_Boundaries_NN(et)
         endif
      case ('DN')
         if (present(values)) then
            call at_Boundaries_DN(et,values)
         else
            call at_Boundaries_DN(et)
         endif
      case ('ND')
         if (present(values)) then
            call at_Boundaries_ND(et,values)
         else
            call at_Boundaries_ND(et)
         endif
      case ('DD')
         if (present(values)) then
            call at_Boundaries_DD(et,values)
         else
            call at_Boundaries_DD(et)
         endif
      case default
         call MessageNotify('E','et_Boundaries','B.C. not supported')
      end select

    end subroutine et_Boundaries

    function et_LaplaInv_et(et,values)
      !
      ! �����ǰ��ͤ��ͤ�Ϳ������(�ǥ��ꥯ����)����, 
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy)**(-1)����Ѥ���.
      !
      ! Chebyshev-tau ˡ�ˤ��׻�
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      !(in) ���ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm)             :: et_LaplaInv_et
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-km:km,2), intent(in), optional :: values
      !(in) ������. ��ά���� 0 �����ꤵ���. 

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8), dimension(-km:km)              :: value1, value2   ! ������

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(:,1) ; value2 = values(:,2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         ty_work=ay_at(tt_work)

         do k=-km,km
            alu(k,:,:) = transpose(at_Dy_at(at_Dy_at(tt_work)) &
                                   - (2*pi*k/xl)**2*tt_work)
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et
      et_work(:,lm-1) = value1
      et_work(:,lm)   = value2
      et_LaplaInv_et = lusolve(alu,kp,et_work)

    end function et_LaplaInv_et

    function ey_Vor2Strm_ey(ey,values,cond,new)    ! ���٤���ή�������. 
      !
      ! ���٤���ή�������. 
      ! Y �����ʻ������֤Ǥ� Chebyshev-Collocation ˡ�ˤ��׻�
      !
      ! ���� \zeta ��Ϳ����ή�� \psi �����.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! Ǵ����
      !    \DP{\psi}{y} = 0 at boundaries
      ! ���Ϥʤ����
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !

      real(8), dimension(-km:km,0:jm),intent(in)  :: ey
              !(in) ���ϱ���ʬ��

      real(8), dimension(-km:km,0:jm)             :: ey_Vor2Strm_ey
              !(out) ����ή��ʬ��

      real(8), dimension(2), intent(in), optional :: values
              !(in) ή��������. �����ǰ���ʤΤ��ȿ� 0 ��ʬ�Τ�
	      !     ��ά���� 0.

      character(len=2), intent(in), optional  :: cond
              ! (in) ������凉���å�. ��ά���� 'RR'
              !      RR : ξüǴ����
              !      RF,FR : Ǵ��/���Ϥʤ����
              !      FF : ξü���Ϥʤ����

      logical, intent(IN), optional :: new
              !(in) true ���ȶ������׻��ѹ������Ū�˿����˺��.
              !     default �� false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:jm)         :: ey_work
      real(8), dimension(0:jm,0:jm)           :: yy
      real(8), dimension(0:jm,0:jm)           :: yy_work
      real(8)                                 :: value1, value2   ! ������
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,j
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

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
            call MessageNotify('E','ey_Vor2Strm_ey','B.C. not supported')
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
         allocate(alu(-km:km,0:jm,0:jm),kp(-km:km,0:jm))

         yy=0
         do j=0,jm
            yy(j,j)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose( &
                 ay_at(at_Dy_at(at_Dy_at(at_ay(yy)))) &
                 - (2*pi*k/xl)** 2* yy )
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         yy_work=yy
         do k=-km,km
            alu(k,0,:)   = yy_work(:,0)
            alu(k,jm,:)  = yy_work(:,jm)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            yy_work=ay_at(at_Dy_at(at_ay(yy)))
         else
            yy_work=ay_at(at_Dy_at(at_Dy_at(at_ay(yy))))
         endif
         do k=-km,km
            alu(k,1,:) = yy_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid2 ) then
            yy_work=ay_at(at_Dy_at(at_ay(yy)))
         else
            yy_work=ay_at(at_Dy_at(at_Dy_at(at_ay(yy))))
         endif
         do k=-km,km
            alu(k,jm-1,:) = yy_work(:,jm)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','ey_Vor2Strm_ey',&
                            'Matrix for stream func. calc. produced')
      endif

      ey_work = ey
      ey_work(:,1)    = 0               ! �ϳ�Ū���
      ey_work(:,jm-1) = 0               ! �ϳ�Ū���

      ey_work(:,0) = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      ey_work(0,0) = value1*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      ey_work(:,jm)   = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      ey_work(0,jm)   = value2*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      ey_Vor2Strm_ey = lusolve(alu,kp,ey_work)

    end function ey_Vor2Strm_ey

   !-----  ��� : �׻����ޤ�������. pending ---- 
    function et_Vor2Strm_et(et,values,rigid)
      !
      ! ���٤���ή�������. 
      !
      ! ��� : �ʲ����������ޤ��ʤ�������α��. ���Ѷػ�
      !    * Y �����������ȿ��� lm=jm �����ꤷ�Ƥ����ʤ���
      !      �׻���̤���ư�����԰���Ȥʤ�.
      !    *  �󰵽�ή�η׻��γȻ�����λ���ȯŸ������˷׻��Ǥ��ʤ�. 
      !       Chebyshev-tau�Ȥ���������������ɤ��ʤ��餷��.
      !
      ! Chebyshev-tau ˡ�ˤ��׻�
      ! ���� \zeta ��Ϳ����ή�� \psi �����.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! Ǵ����
      !    \DP{\psi}{y} = 0 at boundaries
      ! ���Ϥʤ����
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !
      ! l=0,1,lm-1,lm ��ʬ�μ�������˶�������Ϳ����. 
      ! ���٤��㼡��ʬ��̵�뤹�뤳�Ȥ�
      ! \nabla^4 \psi = \zeta^2 ��򤤤Ƥ��뤳�Ȥ�����. 
      ! 4 ������ʬ�������ˤ��뤳�ȤǶ������ο��ȤĤ��Ĥޤ�����. 

      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      real(8), dimension(-km:km,0:lm)             :: et_Vor2Strm_et

      ! ή��������. �����ǰ���ʤΤ��ȿ� 0 ��ʬ�Τ�
      real(8), dimension(2), intent(in), optional :: values
      ! ������凉���å�
      logical, dimension(2), intent(in), optional :: rigid

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8)                                 :: value1, value2   ! ������
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

      if (.not. present(rigid)) then
         rigid1=.true. ; rigid2=.true.
      else
         rigid1 = rigid(1) ; rigid2 = rigid(2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose(at_Dy_at(at_Dy_at(tt_work)) &
                                   - (2*pi*k/xl)**2*tt_work)
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         ty_work=ay_at(tt_work)
         do k=-km,km
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,0,:) = ty_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid2 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,1,:) = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et
      et_work(:,0) = 0               ! �ϳ�Ū���
      et_work(:,1) = 0               ! �ϳ�Ū���

      et_work(:,lm-1) = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      et_work(0,lm-1) = value1*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      et_work(:,lm)   = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      et_work(0,lm)   = value2*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      et_Vor2Strm_et = lusolve(alu,kp,et_work)

    end function et_Vor2Strm_et

   !-----  ��� : �׻����ޤ�������. pending ---- 
    function et_Vor2Strm1_et(et,values,rigid) 
      ! ���٤���ή�������. 
      !
      ! ��� : �ʲ����������ޤ��ʤ�������α��. ���Ѷػ�
      !    * Y �����������ȿ��� lm=jm �����ꤷ�Ƥ����ʤ���
      !      �׻���̤���ư�����԰���Ȥʤ�.
      !    *  �󰵽�ή�η׻��γȻ�����λ���ȯŸ������˷׻��Ǥ��ʤ�. 
      !       Chebyshev-tau�Ȥ���������������ɤ��ʤ��餷��.
      !
      ! Chebyshev-tau ˡ�ˤ��׻�
      ! ���� \zeta ��Ϳ����ή�� \psi �����.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! Ǵ����
      !    \DP{\psi}{y} = 0 at boundaries
      ! ���Ϥʤ����
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !
      ! �ǥե���Ȥ�Ǵ����
      !
      ! \nabla^4 \psi = \nabla^2\zeta ���
      ! 4 ������ʬ�������ˤ��뤳�ȤǶ������ο��ȤĤ��Ĥޤ�����. 
      !
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      !(in) ����

      real(8), dimension(-km:km,0:lm)             :: et_Vor2Strm1_et
      !(out)  ή��������. �����ǰ���ʤΤ��ȿ� 0 ��ʬ�Τ�

      real(8), dimension(2), intent(in), optional :: values
      !(in) ������凉���å�

      logical, dimension(2), intent(in), optional :: rigid

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8)                                 :: value1, value2   ! ������
      logical                                 :: rigid1, rigid2   ! �������

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

      if (.not. present(rigid)) then
         rigid1=.true. ; rigid2=.true.
      else
         rigid1 = rigid(1) ; rigid2 = rigid(2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose( &
                 at_Dy_at(at_Dy_at(at_Dy_at(at_Dy_at(tt_work)))) &
                 - 2 * (2*pi*k/xl)**2 * at_Dy_at(at_Dy_at(tt_work)) &
                 + (2*pi*k/xl)**4*tt_work &
                 )
         enddo

         ! ��ư��Ū���. ή���϶����ǰ���
         ty_work=ay_at(tt_work)
         do k=-km,km
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid1 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,lm-3,:) = ty_work(:,0)
         enddo

         ! �ϳ�Ū���Ǵ���� 
         if ( rigid2 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,lm-2,:) = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et_Lapla_et(et)
      et_work(:,lm-3) = 0               ! �ϳ�Ū���
      et_work(:,lm-2) = 0               ! �ϳ�Ū���

      et_work(:,lm-1) = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      et_work(0,lm-1) = value1*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      et_work(:,lm)   = 0            ! ��ư��Ū���. �ȿ� 0 �ʳ��� 0 
      et_work(0,lm)   = value2*2     ! ��ư��Ū���. �ȿ� 0 �ϽŤ� 1/2

      et_Vor2Strm1_et = lusolve(alu,kp,et_work)

    end function et_Vor2Strm1_et

  !--------------- ��ʬ�׻� -----------------
    function IntYX_yx(yx)   ! ���ΰ���ʬ
      !
      ! 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 �����ʻ����ǡ���

      real(8)                           :: IntYX_yx
      !(out) ��ʬ��

      integer :: i, j

      IntYX_yx = 0.0d0
      do i=0,im-1
         do j=0,jm
            IntYX_yx = IntYX_yx + yx(j,i) * y_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo
    end function IntYX_yx

    function y_IntX_yx(yx)  ! X ������ʬ
      !
      ! 2 �����ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm)          :: y_IntX_yx
      !(out) ��ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i
      ! ����ѿ�

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)  ! Y ������ʬ
      !
      ! 2 �����ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntY_yx
      !(out) ��ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: j
      ! ����ѿ�

      x_IntY_yx = 0.0d0
      do j=0,jm
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntX_x(x)      ! X ������ʬ
      !
      ! 1 ����(X)�ʻ����ǡ����� X ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:im-1)   :: x         !(in)  1 �����ʻ����ǡ���
      real(8)                      :: IntX_x    !(out) ��ʬ��

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntY_y(y)      ! Y ������ʬ
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ������ʬ
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), dimension(0:jm)   :: y          !(in)  1 �����ʻ����ǡ���
      real(8)                    :: IntY_y     !(out) ��ʬ��

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
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 �����ʻ����ǡ���

      real(8)                           :: AvrYX_yx
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
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm)          :: y_AvrX_yx
      !(out) ʿ�Ѥ��줿 1 ����(Y)�ʻ���

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)
      !
      ! 2 �����ʻ����ǡ����� Y ����ʿ��
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_AvrY_yx
      !(out) ʿ�Ѥ��줿 1 ����(X)�ʻ���

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
      real(8), dimension(0:jm)   :: y          !(in)  1 �����ʻ����ǡ���
      real(8)                    :: AvrY_y     !(out) ʿ����

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  end module et_module
