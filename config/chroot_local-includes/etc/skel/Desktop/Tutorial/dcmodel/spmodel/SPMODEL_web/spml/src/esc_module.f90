!--
!----------------------------------------------------------------------
!     Copyright (c) 2001-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  esc_module
!
!      spml/esc_module �⥸�塼��� 2 ��������ͥ��ΰ�Ǥ�ή�α�ư��
!      ���ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
!
!      ������ ISPACK/C2PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!      ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ� 
!      ISPACK/C2PACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!����  2001/10/07  �ݹ�����
!      2001/12/26  �ݹ�����  �ؿ�, �ѿ�̿̾ˡ�ѹ�
!      2002/03/25  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/08/19  �ݹ�����  �ʻҥǡ���ź���� gg -> xy ���ѹ�
!      2002/08/20  �ݹ�����  ��ʬ��ʿ�Ѵؿ��ɲ�
!      2005/03/15  �ݹ�����  xy -> yx ����Ƭ�Ҥ��ѹ�
!      2006/03/06  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2006/03/19  �ݹ�����  �ѿ�����³����������򥳥��Ȥ��ɲ�
!      2007/11/12  �ݹ�����  ec_LaplaInv_ec �ؿ��ɲ�
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2009/01/09  �ݹ�����  esc_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ˽���
!      2009/07/30  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!
!++
module esc_module
  !
  != esc_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: esc_module.f90,v 1.15 2009-07-30 12:50:22 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/esc_module �⥸�塼��� 2 ��������ͥ��ΰ�Ǥ�ή�α�ư��
  ! ���ڥ��ȥ�ˡ�ˤ����ͷ׻����뤿��� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK/C2PACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ�ˤĤ��Ƥ� 
  ! ISPACK/C2PACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !== �ؿ����ѿ���̾���ȷ��ˤĤ���
  !
  !=== ̿̾ˡ
  !
  ! * �ؿ�̾����Ƭ (es_, ec_, yx_, x_, y_) ��, �֤��ͤη��򼨤��Ƥ���.
  !   es_,ec_ :: ���ڥ��ȥ�ǡ���(Y ���� SIN Ÿ��, COS Ÿ��)
  !   yx_     :: 2 �����ʻ����ǡ���
  !   x_      :: X ���� 1 �����ʻ����ǡ���
  !   y_      :: Y ���� 1 �����ʻ����ǡ���
  !
  ! * �ؿ�̾�δ֤�ʸ����(Dx, Dy, Lapla, LaplaInv, Jacobian)��, 
  !   ���δؿ��κ��Ѥ�ɽ���Ƥ���.
  !
  ! * �ؿ�̾�κǸ� (_es_es, _es_ec, _es, _ec, _yx, _x, _y) ��, 
  !   �����ѿ��η����ڥ��ȥ�ǡ�������ӳʻ����ǡ����Ǥ��뤳�Ȥ򼨤��Ƥ���.
  !   _es    :: ���ڥ��ȥ�ǡ���(Y ���� SIN ��)
  !   _ec    :: ���ڥ��ȥ�ǡ���(Y ���� COS ��) 
  !   _es_es :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _es_ec :: 2 �ĤΥ��ڥ��ȥ�ǡ���
  !   _yx    :: 2 �����ʻ����ǡ���, 
  !   _x     :: X ���� 1 �����ʻ����ǡ���
  !   _y     :: Y ���� 1 �����ʻ����ǡ���
  !
  !=== �ƥǡ����μ��������
  !
  ! * yx : 2 �����ʻ����ǡ���.
  !  * �ѿ��μ���ȼ����� real(8), dimension(0:jm,0:im-1). 
  !  * im, jm �Ϥ��줾�� X, Y ��ɸ�γʻ������Ǥ���, 
  !    ���֥롼���� esc_Initial �ˤƤ��餫�������ꤷ�Ƥ���. 
  !  * �� 1 ������ Y ��ɸ�γʻ��������ֹ�, �� 2 ������ X ��ɸ�γʻ��������ֹ�
  !    �Ǥ��� (X, Y �ν�ǤϤʤ�)���Ȥ����.
  !
  ! * es : X �����ա��ꥨ��, Y ���� SIN �����ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,lm).
  !   * km, lm �Ϥ��줾�� X, Y �����κ����ȿ��Ǥ���, 
  !     ���֥롼���� esc_Initial �ˤ�
  !     ���餫�������ꤷ�Ƥ���. 
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * ec : X �����ա��ꥨ��, Y ���� COS �����ڥ��ȥ�ǡ���.
  !   * �ѿ��μ���ȼ����� real(8), dimension(-km:km,0:lm). 
  !   * km, lm �Ϥ��줾��
  !     X, Y �����κ����ȿ��Ǥ���, 
  !     ���֥롼���� esc_Initial �ˤƤ��餫�������ꤷ�Ƥ���.
  !   * ���ڥ��ȥ�ǡ����γ�Ǽ�Τ������ˤĤ��Ƥ�...
  !
  ! * x, y : X, Y ���� 1 �����ʻ����ǡ���.
  !   * �ѿ��μ���ȼ����Ϥ��줾�� real(8), dimension(0:im-1) 
  !     ����� real(8), dimension(0:jm).
  !
  ! * es_, ec_ �ǻϤޤ�ؿ����֤��ͤϥ��ڥ��ȥ�ǡ�����Ʊ��.
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
  ! esc_initial :: ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭��������
  ! 
  !==== ��ɸ�ѿ�
  !
  ! x_X, y_Y     ::  �ʻ�����ɸ(X,Y��ɸ)���Ǽ���� 1 ��������
  ! x_X_Weight, y_Y_Weight ::  �Ťߺ�ɸ���Ǽ���� 1 ��������
  ! yx_X, yx_Y   :: �ʻ����ǡ����� XY ��ɸ(X,Y)(�ʻ����ǡ����� 2 ��������)
  !
  !==== �����Ѵ�
  !
  ! yx_es, yx_ec :: ���ڥ��ȥ�ǡ�������ʻҥǡ����ؤ��Ѵ�
  ! es_yx, ec_yx :: �ʻҥǡ������饹�ڥ��ȥ�ǡ����ؤ��Ѵ�
  !
  !==== ��ʬ
  !
  ! es_Lapla_es, ec_Lapla_ec  :: ���ڥ��ȥ�ǡ����˥�ץ饷�������Ѥ�����
  ! es_LaplaInv_es, ec_LaplaInv_ec :: ���ڥ��ȥ�ǡ�����
  !                                   ��ץ饷����ε��Ѵ�����Ѥ�����
  ! es_Dx_es, ec_Dx_ec  :: ���ڥ��ȥ�ǡ����� X ��ʬ����Ѥ�����
  ! ec_Dy_es, es_Dy_ec  :: ���ڥ��ȥ�ǡ����� Y ��ʬ����Ѥ�����
  ! es_Jacobian_es_es, ec_Jacobian_es_ec :: 2 �ĤΥ��ڥ��ȥ�ǡ�������
  !                                         �䥳�ӥ����׻�����
  ! 
  !==== ��ʬ��ʿ��
  !
  ! IntYX_yx, AvrYX_yx   :: 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��
  ! y_IntX_yx, y_AvrX_yx :: 2 �����ʻ����ǡ����� X ������ʬ�����ʿ��
  ! IntX_x, AvrX_x       :: 1 ����(X)�ʻ����ǡ����� X ������ʬ�����ʿ��
  ! x_IntY_yx, x_AvrY_yx :: 2 �����ʻ����ǡ����� Y ������ʬ�����ʿ��
  ! IntY_y, AvrY_y       :: 1 ����(Y)�ʻ����ǡ����� Y ������ʬ�����ʿ��
  !
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public esc_Initial                                      ! ������롼����
  public yx_es, yx_ec, es_yx, ec_yx                       ! �����Ѵ�
  public es_Lapla_es, es_LaplaInv_es, es_Dx_es, ec_Dy_es  ! ��ʬ
  public ec_Lapla_ec, ec_LaplaInv_ec, ec_Dx_ec, es_Dy_ec  ! ��ʬ
  public es_Jacobian_es_es, ec_Jacobian_es_ec             ! �������׻�
  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! ��ʬ
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! ʿ��
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! ��ɸ�ѿ�

  integer   :: im=32, jm=8                                ! �ʻ���������(X,Y)
  integer   :: km=10, lm=5                                ! �����ȿ�������(X,Y)
  real(8)   :: xl=2.0, yl=1.0                             ! �ΰ���礭��

  integer,dimension(5)                  :: itj
  real(8),dimension(:),allocatable      :: tj
  integer,dimension(5)                  :: iti
  real(8),dimension(:),allocatable      :: ti

  real(8), dimension(:), allocatable    :: x_X
  ! �ʻ�����ɸ(X)���Ǽ���� 1 ��������

  real(8), dimension(:), allocatable    :: y_Y
  ! �ʻ�����ɸ(X)���Ǽ���� 1 ��������

  real(8), dimension(:), allocatable    :: x_X_Weight
  ! �Ťߺ�ɸ(X)���Ǽ���� 1 ��������. X �����γʻ����δֳ֤���Ǽ���Ƥ���.

  real(8), dimension(:), allocatable    :: y_Y_Weight
  ! �Ťߺ�ɸ(Y)���Ǽ���� 1 ��������. Y �����γʻ����δֳ֤���Ǽ���Ƥ���.

  real(8), dimension(:,:), allocatable  :: yx_X
  ! �Ƴʻ���(i,j)�ΰ��֤� X ��ɸ���Ǽ�����ʻҥǡ���.

  real(8), dimension(:,:), allocatable  :: yx_Y
  ! �Ƴʻ���(i,j)�ΰ��֤� Y ��ɸ���Ǽ�����ʻҥǡ���.

  real(8), dimension(:),   allocatable  :: wg, ws, wgj
  real(8), dimension(:,:), allocatable  :: yx_work,es_work,ec_work
  real(8), parameter  ::  pi=3.1415926535897932385D0

  save im, jm, km, lm, itj, tj, iti, ti, xl, yl
  save x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y

  contains
  !--------------- ����� -----------------
    subroutine esc_Initial(i,j,k,l,xmin,xmax,ymin,ymax)
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ�, �ΰ���礭�������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�.
      !
      integer,intent(in) :: i           !(in) �ʻ�����(X)
      integer,intent(in) :: j           !(in) �ʻ�����(Y)
      integer,intent(in) :: k           !(in) �����ȿ�(X)
      integer,intent(in) :: l           !(in) �����ȿ�(Y)

      real(8),intent(in) :: xmin, xmax     !(in) X ��ɸ�ϰ�
      real(8),intent(in) :: ymin, ymax     !(in) Y ��ɸ�ϰ�

      integer :: ii, jj

      im = i         ; jm = j
      km = k         ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      allocate(tj(jm*6),ti(im*2))

      call c2init(jm,im,itj,tj,iti,ti)

      allocate(x_X(0:im-1), x_X_Weight(0:im-1))
      allocate(y_Y(0:jm), y_Y_Weight(0:jm))
      allocate(yx_X(0:jm,0:im-1), yx_Y(0:jm,0:im-1))

      do ii=0,im-1
         x_X(ii) = xmin + xl/im*ii
      enddo
      x_X_Weight = xl/im

      do jj=0,jm
         y_Y(jj) = ymin + yl/jm*jj
      enddo
      y_Y_Weight(0) = yl/(2*jm)
      y_Y_Weight(1:jm-1) = yl/jm 
      y_Y_Weight(jm) = yl/(2*jm)

      yx_X = spread(x_X,1,jm+1)
      yx_Y = spread(y_Y,2,im)

      call MessageNotify('M','esc_initial', &
           'esc_module (2009/07/30) is initialized')

    end subroutine esc_Initial

  !--------------- �����Ѵ� -----------------
    function yx_es(es)
      !
      ! SIN(Y)�����ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_es
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)�����ڥ��ȥ�ǡ���

      real(8)                                      :: wg((jm+1)*im)
      ! ����ΰ�

      call c2s2ga(lm,km,jm,im,es,yx_es,wg,itj,tj,iti,ti,1)
    end function yx_es

    function yx_ec(ec)
      !
      ! COS(Y)�����ڥ��ȥ�ǡ�������ʻҥǡ������Ѵ�����.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ec
      !(out) �ʻ����ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) COS(Y)�����ڥ��ȥ�ǡ���

      real(8)                                      :: wg((jm+1)*im)
      ! ����ΰ�

      call c2s2ga(lm,km,jm,im,ec,yx_ec,wg,itj,tj,iti,ti,2)
    end function yx_ec

    function es_yx(yx)
      !
      ! �ʻҥǡ������� SIN(Y)�����ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,lm)                :: es_yx
      !(out) SIN(Y)�����ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) �ʻ����ǡ���

      real(8)                                      :: wg((jm+1)*im)
      real(8)                                      :: yx_work(0:jm,0:im-1)
      ! ����ΰ�

      yx_work = yx
      call c2g2sa(lm,km,jm,im,yx_work,es_yx,wg,itj,tj,iti,ti,1)
    end function es_yx

    function ec_yx(yx)
      !
      ! �ʻҥǡ������� COS(Y)�����ڥ��ȥ�ǡ������Ѵ�����.
      !
      real(8), dimension(-km:km,0:lm)              :: ec_yx
      !(in) COS(Y)�����ڥ��ȥ�ǡ���

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(out) �ʻ����ǡ���

      real(8)                                      :: wg((jm+1)*im)
      real(8)                                      :: yx_work(0:jm,0:im-1)
      ! ����ΰ�

      yx_work = yx
      call c2g2sa(lm,km,jm,im,yx_work,ec_yx,wg,itj,tj,iti,ti,2)
    end function ec_yx

  !--------------- ��ʬ�׻� -----------------
    function es_Lapla_es(es)
      !
      ! SIN(Y)�����ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �򤫤���
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-km:km,lm)                :: es_Lapla_es
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Lapla_es(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*es(k,l)
         enddo
      enddo
    end function es_Lapla_es

    function ec_Lapla_ec(ec)
      !
      ! COS(Y)�����ϥ��ڥ��ȥ�ǡ����˥�ץ饷����(��xx+��yy)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �򤫤���
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-km:km,0:lm)                :: ec_Lapla_ec
      !(out) ���ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), dimension(-km:km,0:lm), intent(in)    :: ec
      !(in) COS(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do l=0,lm
         do k=-km,km
            ec_Lapla_ec(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*ec(k,l)
         enddo
      enddo
    end function ec_Lapla_ec

    function es_LaplaInv_es(es)   ! ���ڥ��ȥ� SINY �˺��Ѥ���� \lapla �黻��
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy)**(-1)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �ǳ��
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-km:km,lm)                :: es_LaplaInv_es
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)�����ڥ��ȥ�ǡ���

      integer k,l

      do l=1,lm
         do k=-km,km
            es_LaplaInv_es(k,l) = -es(k,l)/((2*pi*k/xl)**2+(pi*l/yl)**2)
         enddo
      enddo
    end function es_LaplaInv_es

    function ec_LaplaInv_ec(ec)   ! ���ڥ��ȥ� COSY �˺��Ѥ���� \lapla �黻��
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����(��xx+��yy)**(-1)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ��������ȿ� (k**2 + l**2) �ǳ��
      ! �׻���ԤäƤ���. 
      !
      real(8), dimension(-km:km,0:lm)              :: ec_LaplaInv_ec
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) SIN(Y)�����ڥ��ȥ�ǡ���

      integer k,l

      if ( ec(0,0) .ne. 0.0D0 ) then
           call MessageNotify('W','ec_LaplaInv_ec', &
                              '0-0 component of input data is not zero.')
           call MessageNotify('W','ec_LaplaInv_ec', &
                              '0-0 component of output set to zero.')
      endif

      do l=1,lm
         do k=-km,km
            ec_LaplaInv_ec(k,l) = -ec(k,l)/((2*pi*k/xl)**2+(pi*l/yl)**2)
         enddo
      enddo

      do k=1,km
         ec_LaplaInv_ec(k,0)  = -ec(k,0)/(2*pi*k/xl)**2
         ec_LaplaInv_ec(-k,0) = -ec(-k,0)/(2*pi*k/xl)**2
      enddo

      ec_LaplaInv_ec(0,0) = 0.0

    end function ec_LaplaInv_ec

    function es_Dx_es(es)
      !
      ! SIN(Y)�����ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,lm)                :: es_Dx_es
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Dx_es(k,l)  =  (-2*pi*k/xl)*es(-k,l)
         enddo
      enddo
    end function es_Dx_es

    function ec_Dx_ec(ec)
      !
      ! COS(Y)�����ϥ��ڥ��ȥ�ǡ����� X ��ʬ(��x)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� X ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� k �򤫤���
      ! sin(kx) <-> cos(kx) ��ʬ�����촹����׻���ԤäƤ���.
      !
      real(8), dimension(-km:km,0:lm)                :: ec_Dx_ec
      !(out) ���ڥ��ȥ�ǡ����� X ��ʬ

      real(8), dimension(-km:km,0:lm), intent(in)    :: ec
      !(in) COS(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do l=0,lm
         do k=-km,km
            ec_Dx_ec(k,l)  =  (-2*pi*k/xl)*ec(-k,l)
         enddo
      enddo
    end function ec_Dx_ec

    function ec_Dy_es(es)   ! ���ڥ��ȥ� SINY �˺��Ѥ��� y ��ʬ�黻��
      !
      ! SIN(Y)�����ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� l �򤫤��Ƥ���. 
      !
      real(8), dimension(-km:km,0:lm)              :: ec_Dy_es
      !(out) ���ڥ��ȥ�ǡ����� Y ��ʬ, COS(Y)��.

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do k=-km,km
         ec_Dy_es(k,0)  =  0.0
      enddo
      do l=1,lm
         do k=-km,km
            ec_Dy_es(k,l)  =  (pi*l/yl)*es(k,l)
         enddo
      enddo
    end function ec_Dy_es

    function es_Dy_ec(ec)   ! ���ڥ��ȥ� COSY �˺��Ѥ��� y ��ʬ�黻��
      !
      ! COS(Y)�����ϥ��ڥ��ȥ�ǡ����� Y ��ʬ(��y)����Ѥ���.
      !
      ! ���ڥ��ȥ�ǡ����� X ��ʬ�Ȥ�, �б�����ʻ����ǡ����� Y ��ʬ��
      ! ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      !
      ! �ºݤˤϥ��ڥ��ȥ�ǡ����� X �����ȿ� l �򤫤��Ƥ���. 
      !
      real(8), dimension(-km:km,lm)                 :: es_Dy_ec
      !(out) ���ڥ��ȥ�ǡ����� Y ��ʬ, SIN(Y)��.

      real(8), dimension(-km:km,0:lm), intent(in)   :: ec
      !(in) COS(Y)�����ϥ��ڥ��ȥ�ǡ���

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Dy_ec(k,l)  =  -(pi*l/yl)*ec(k,l)
         enddo
      enddo
    end function es_Dy_ec

 !------------------- ��������׻� ----------------------
    function es_Jacobian_es_es(es_a,es_b) !���ڥ��ȥ� SINY �˺��Ѥ���䥳�ӥ���
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
      real(8), dimension(-km:km,lm)                :: es_Jacobian_es_es
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), dimension(-km:km,lm), intent(in)    :: es_A,es_B
      !(in) 2�Ĥ� SIN(Y)�����ϥ��ڥ��ȥ�ǡ���

      real(8)                                      :: ws((2*km+1)*(lm+1))
      real(8)                                      :: wgj((jm+1)*im*3)
      real(8)                                      :: es_work(-km:km,lm)
      ! ����ΰ�

      integer k,l

      call c2ajcb(lm,km,jm,im,es_A,es_B,es_work,ws,wgj,itj,tj,iti,ti)

      do l=1,lm
         do k=-km,km
            es_Jacobian_es_es(k,l) = (2*pi/xl)*(pi/yl)*es_work(k,l)
         enddo
      enddo
    end function es_Jacobian_es_es

    function ec_Jacobian_es_ec(es,ec)  ! ���ڥ��ȥ� COS(Y) �˺��Ѥ���䥳�ӥ���
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
      real(8), dimension(-km:km,0:lm)              :: ec_Jacobian_es_ec
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) 1���ܤ� SIN(Y)�����ϥ��ڥ��ȥ�ǡ���

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) 2���ܤ� COS(Y)�����ϥ��ڥ��ȥ�ǡ���

      real(8)                                      :: ws((2*km+1)*(lm+1))
      real(8)                                      :: wgj((jm+1)*im*3)
      real(8)                                      :: ec_work(-km:km,0:lm)
      ! ����ΰ�

      integer k,l

      call c2ajcc(lm,km,jm,im,es,ec,ec_work,ws,wgj,itj,tj,iti,ti)

      do l=0,lm
         do k=-km,km
            ec_Jacobian_es_ec(k,l) = (2*pi/xl)*(pi/yl)*ec_work(k,l)
         enddo
      enddo
    end function ec_Jacobian_es_ec

  !--------------- ��ʬ�׻� -----------------
    function IntYX_yx(yx)
      !
      ! 2 �����ʻ����ǡ��������ΰ���ʬ�����ʿ��.
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

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
      ! 1 ����(X)�ʻ����ǡ����� X ������ʬ
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm)          :: y_IntX_yx
      !(out) X ��������ʬ���줿 1 ����(Y)�ʻ����ǡ���

      integer :: i

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)
      !
      ! 2 �����ʻ����ǡ����� Y ������ʬ
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_IntY_yx
      !(out) Y ��������ʬ���줿 1 ����(X)�ʻ����ǡ���

      integer :: j

      x_IntY_yx = 0.0d0
      do j=0,jm
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntX_x(x)      ! X ������ʬ
      !
      ! 1 ����(X)�ʻ����ǡ����� X ������ʬ
      !
      real(8), dimension(0:im-1)   :: x
      !(in) 1 ����(X)�ʻ����ǡ���

      real(8)                      :: IntX_x
      !(out) ��ʬ��

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntY_y(y)      ! Y ������ʬ
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ������ʬ
      !
      real(8), dimension(0:jm)   :: y
      !(in) 1 ����(Y)�ʻ����ǡ���

      real(8)                    :: IntY_y
      !(out) ��ʬ��

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

  !--------------- ʿ�ѷ׻� -----------------
    function AvrYX_yx(yx)    ! ���ΰ�ʿ��
      !
      ! 2 �����ʻ����ǡ��������ΰ�ʿ��
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8)                           :: AvrYX_yx
      !(out) ʿ����

      AvrYX_yx = IntYX_yx(yx)/(sum(x_X_weight)*sum(y_Y_weight))
    end function AvrYX_yx

    function y_AvrX_yx(yx)   ! X ����ʿ��
      !
      ! 1 ����(X)�ʻ����ǡ����� X ����ʿ��
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:jm)          :: y_AvrX_yx
      !(out) X ������ʿ�Ѥ��줿 1 ����(Y)�ʻ����ǡ���

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)   ! Y ����ʿ��
      !
      ! 2 �����ʻ����ǡ����� Y ����ʿ��
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 �����ʻ����ǡ���

      real(8), dimension(0:im-1)        :: x_AvrY_yx
      !(out) Y ������ʿ�Ѥ��줿 1 ����(X)�ʻ����ǡ���

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

    function AvrX_x(x)       ! X ����ʿ��
      !
      ! 1 ����(X)�ʻ����ǡ����� X ����ʿ��
      !
      real(8), dimension(0:im-1)   :: x
      !(in) 1 ����(X)�ʻ����ǡ���

      real(8)                      :: AvrX_x
      !(out) ʿ����

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrY_y(y)       ! Y ����ʿ��
      !
      ! 1 ����(Y)�ʻ����ǡ����� Y ����ʿ��
      !
      real(8), dimension(0:jm)   :: y
      !(in) 1 ����(Y)�ʻ����ǡ���

      real(8)                    :: AvrY_y
      ! ʿ����

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  end module esc_module
