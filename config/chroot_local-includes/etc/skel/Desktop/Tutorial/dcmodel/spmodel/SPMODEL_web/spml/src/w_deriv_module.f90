!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
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
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
!++
module w_deriv_module
  !
  != w_deriv_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_deriv_module.f90,v 1.18 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_deriv_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� w_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  ! ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_base_initial ��Ƥ��
  ! �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : im, jm, nm, it, t, y, ip, p, r, ia, a, &
                            w_base_Initial, xy_w, w_xy
  implicit none

  real(8), allocatable  :: rn(:,:)            
  ! ��ץ饷����黻������
  !
  ! ���ڥ��ȥ�ǡ����Υ�ץ饷�����׻����뤿��η���
  ! ����Υ�������((nm+1)*(nm+1), 2)
  !
  ! r(L,1) �ˤ� L ���ܤγ�Ǽ���֤Υ��ڥ��ȥ���Ф����ץ饷����׻���
  ! ���� -n(n+1) ���ͤ���Ǽ����Ƥ���.
  !
  integer, allocatable  :: irm(:,:)           
  ! ������ʬ�黻������
  !
  ! ���ڥ��ȥ�ǡ����η�����ʬ��׻����뤿��η���.
  ! ���󥵥����� ( (nm+1)*(nm+1),2 ) �Ǥ���.
  !
  ! L���ܤγ�Ǽ���֤Υ��ڥ��ȥ뤬�����ʤ�, irm(L,1)�ˤ��б���������γ�Ǽ���֤�,
  ! irm(L,2) �ˤ������ȿ� m ����Ǽ����Ƥ���. �ޤ�, L���ܤγ�Ǽ���֤Υ��ڥ��ȥ�
  ! �������ʤ�, irm(L,1)�ˤ��б���������γ�Ǽ���֤�, irm(L,2)�ˤ� -m ����Ǽ����
  ! �Ƥ���.
  !
  integer, allocatable  :: ip2(:), ip3(:)     ! �䥳�ӥ���׻�������
  real(8), allocatable  :: p2(:), p3(:)       ! �䥳�ӥ���׻�������
  real(8), allocatable  :: r2(:), r3(:)       ! �䥳�ӥ���׻�������

  integer iw                                  ! ���������礭��

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

  public rn, irm                              ! ��ץ饷����/������ʬ�黻������

  save rn, irm, ip2, ip3, p2, p3, r2, r3
  save iw

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
      allocate(rn((nm+1)*(nm+1),2))           ! ��ץ饷����黻������
      allocate(irm((nm+1)*(nm+1),2))          ! ������ʬ�黻������
      call spnini(nm,rn)
      call spmini(nm,irm)

      allocate(ip2(2*((nm+1)/2+nm+1)*2))      ! �䥳�ӥ���׻�������
      allocate(p2(2*((nm+1)/2+nm+1)*jm))      ! �䥳�ӥ���׻�������
      allocate(r2(2*((nm+1)/2*2+3)*(nm/2+1))) ! �䥳�ӥ���׻�������
      allocate(ip3(3*((nm+1)/2+nm+1)*2))      ! �䥳�ӥ���׻�������
      allocate(p3(3*((nm+1)/2+nm+1)*jm))      ! �䥳�ӥ���׻�������
      allocate(r3(3*((nm+1)/2*2+3)*(nm/2+1))) ! �䥳�ӥ���׻�������
      call snkini(nm,jm,2,ip,p,r,ip2,p2,r2)
      call snkini(nm,jm,3,ip,p,r,ip3,p3,r3)

      iw=3*max( ((nm+1)/2*2+3)*(nm/2+2)*2, &
                jm*((nm+1)/2+nm+1)*2, jm*im )

      call MessageNotify('M','w_deriv_initial',&
           'w_deriv_module (2009/07/30) is initialized')

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

      call spclap(nm,w_data,w_Lapla_w,rn(1,1))
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

      call spclap(nm,w_data,w_LaplaInv_w,rn(1,2))
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

      call spclam(nm,w_data,w_DLon_w,irm)

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

      real(8) :: q(3*((nm+1)/2+nm+1)*jm)       ! ���������
      real(8) :: ws(iw),ww(iw)                 ! ���������

      call spnjcb(nm,im,im,jm,jm,w_a,w_b,w_Jacobian_w_w,&
           it,t,y,ip2,p2,r2,ip3,p3,r3,ia,a,q,ws,ww)

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

  end module w_deriv_module
