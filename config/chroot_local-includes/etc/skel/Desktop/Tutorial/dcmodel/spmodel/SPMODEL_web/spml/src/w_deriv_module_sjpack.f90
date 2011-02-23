!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2010 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_deriv_module
!
!  spml/w_deriv_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
!  ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!  ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
!  �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
!
!
!����  2001/12/08  �ݹ�����
!      2001/12/26  �ݹ�����  �ؿ�,�ѿ�̾���ѹ�
!      2002/02/07  �ݹ�����  �ؿ�,�ѿ�̾�����ѹ�
!      2002/03/30  �ݹ�����  �ؿ�,�ѿ�̾���ƺ��ѹ�
!      2002/05/25  �ݹ�����  �ʻ�����ɸ����̤��٤�̿̾ˡ�ѹ�
!      2005/07/04  �ݹ�����  OPENMP ���Ѵ��롼������б�
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2008/05/31  �ݹ�����  ������롼�����ʬΥ
!      2008/06/22  ��������ʿ �ʻ����ǡ��������󳫻����� 1 ���� 0 ��.
!      2008/06/23  ��������ʿ �ʻ����ǡ����γ�Ǽ�� (0:im-1, 1:jm) ��.
!      2008/07/01  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/01/09  �ݹ�����  w_deriv_Initial ��å����������դ��ɲ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2009/07/30  �ݹ�����   ����ΰ��������ѿ����ѹ�(for OpenMP)
!      2010/01/26  �ݹ�����   ��ץ饷����黻���ѿ��� public ��
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module w_deriv_module_sjpack
  !
  != w_deriv_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_deriv_module_sjpack.f90,v 1.2 2010-01-26 12:34:06 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_deriv_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  ! ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
  ! �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
  !
  use dc_message, only : MessageNotify
  use w_base_module_sjpack, only : im, jm, nm=>nn, mm, it, t, r, &
                                   w_base_Initial, xy_w, w_xy
  implicit none

  real(8), allocatable  :: D(:)
  ! ��ץ饷����黻������
  !
  ! ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���
  ! ����Υ�������((mm+1)*(mm+1)*2)
  !

  real(8), allocatable  :: rn(:,:)            
  ! ��ץ饷����黻������(w_module �ȸߴ������ݤĤ���)
  !
  ! ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���
  ! ����Υ�������((nm+1)*(nm+1), 2)
  !
  ! r(L,1) �ˤ� L ���ܤγ�Ǽ���֤Υ��ڥ��ȥ���Ф����ץ饷����׻���
  ! ���� -n(n+1) ���ͤ���Ǽ����Ƥ���.
  !

  private

  public w_deriv_Initial                      ! �����
  public w_Lapla_w, w_LaplaInv_w              ! ��ץ饷����ȵձ黻
  public w_DLon_w                             ! ������ʬ
  public xy_GradLon_w, xy_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xy, w_DivLat_xy             ! ȯ������ʬ
  public w_Div_xy_xy                          ! ȯ������ʬ
  public w_Jacobian_w_w                       ! �䥳�ӥ���
  public xy_GradLambda_w, xy_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xy, w_DivMu_xy           ! ȯ������ʬ(��,�̺�ɸ)

  public rn                                   ! ��ץ饷����黻������

  save D, rn

  contains

  !--------------- ����� -----------------
    subroutine w_deriv_initial
      !
      ! ���ڥ��ȥ���ʬ�׻���ɬ�פȤʤ����ΰ�����ꤹ��. 
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ��
      ! �������򤷤ʤ���Фʤ�ʤ�. 
      !
      ! ���Υ��֥롼�����ñ�Ȥ��Ѥ���ΤǤʤ�, 
      ! ��̥��֥롼���� w_Initial ����Ѥ��뤳��.
      !
      allocate(D((nm+1)*(nm+1)*2))           ! ��ץ饷����黻������
      allocate(rn((nm+1)*(nm+1),2))          ! ��ץ饷����黻������

      call sjinid(mm,D)

      rn = reshape(D,(/(nm+1)**2,2/))

      call MessageNotify('M','w_deriv_initial',&
           'w_deriv_module_sjpack (2010/01/26) is initialized')

    end subroutine w_deriv_initial

  !--------------- ��ʬ�׻� -----------------
    function w_Lapla_w(w_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !    ��^2 = 1/cos^2�ա���^2/�ߦ�^2 + 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ���(1 ����).
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: w_Lapla_w((nm+1)*(nm+1))
      !(out) ���ϥ��ڥ��ȥ�ǡ����Υ�ץ饷����

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      call sjclap(mm,w_data,w_Lapla_w,D,1)

    end function w_Lapla_w

    function w_LaplaInv_w(w_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      !
      !    ��^{-2}
      !      =[1/cos^2�ա���^2/�ߦ�^2 + 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)]^{-1}
      !
      ! ����Ѥ���(1 ����).
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8)              :: w_LaplaInv_w((nm+1)*(nm+1))
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      call sjclap(mm,w_data,w_LaplaInv_w,D,2)

    end function w_LaplaInv_w

    function w_DLon_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˷�����ʬ ��/�ߦ� ����Ѥ�����(1 ����).
      !
      ! ���ڥ��ȥ�ǡ����η�����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ������ʬ��/�ߦˤ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      ! 
      real(8)              :: w_DLon_w((nm+1)*(nm+1))
      !(out) ���ڥ��ȥ�ǡ����η�����ʬ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      call sjcs2x(mm,w_data,w_DLon_w)

    end function w_DLon_w

    function xy_GradLon_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(1 ����).
      !
      real(8)              :: xy_GradLon_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
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

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xy_GradLat_w = xy_w(w_data,ipow=1,iflag=1)

    end function xy_GradLat_w

    function w_DivLon_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLon_xy((nm+1)*(nm+1))
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
      real(8)              :: w_DivLat_xy((nm+1)*(nm+1))
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
      real(8)              :: w_Div_xy_xy((nm+1)*(nm+1))
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

      real(8)             :: w_Jacobian_w_w((nm+1)*(nm+1))
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      w_Jacobian_w_w = w_xy( &
                  xy_w(w_DLon_w(w_a))*xy_w(w_b,ipow=2,iflag=1) &
                - xy_w(w_DLon_w(w_b))*xy_w(w_a,ipow=2,iflag=1) )

    end function w_Jacobian_w_w

  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xy_GradLambda_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(1 ����).
      !
      real(8)              :: xy_GradLambda_w(0:im-1,1:jm)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
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

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xy_GradMu_w = xy_w(w_data,ipow=0,iflag=1)

    end function xy_GradMu_w

    function w_DivLambda_xy(xy_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLambda_xy((nm+1)*(nm+1))
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
      real(8)              :: w_DivMu_xy((nm+1)*(nm+1))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) ���ϳʻ����ǡ���

      w_DivMu_xy = w_xy(xy_data,ipow=2,iflag=1)

    end function w_DivMu_xy

  end module w_deriv_module_sjpack
