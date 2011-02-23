!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_deriv_module
!
!  spml/wa_deriv_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
!  ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_deriv_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!����  2002/02/02  �ݹ����� 
!      2002/02/07  �ݹ�����  �ؿ�,�ѿ�̾�����ѹ�
!      2002/03/30  �ݹ�����  �⥸�塼��̾�ѹ�
!      2002/05/25  �ݹ�����  �ʻ�����ɸ����̤��٤�̿̾ˡ�ѹ�
!      2002/10/07  �ݹ�����  ��, �̺�ɸ������ʬ�ɲ�
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2007/11/21  �ݹ�����  ��������֥롼�����å���������
!      2008/05/31  �ݹ�����  ��������֥롼������
!      2008/06/21  ��������ʿ ��ʿ�����γʻ����ǡ�������λ����� 1 ���� 0 ��.
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/07/07  �ݹ�����  ����ǽ�����������������Ƥ����ѹ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!
!      ����
!         ���Ѵ�����ʻ����ǡ���, ���ڥ��ȥ�ǡ�����������礭���Ϸ�ᤦ��
!
module wa_deriv_module
  !
  != wa_deriv_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_deriv_module.f90,v 1.14 2009-02-28 21:33:47 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_deriv_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  ! ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_deriv_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  ! ���Υ⥸�塼���Ȥ�����ˤ�����ä� w_initial ��Ƥ��
  ! �����ȿ�, �ʻ�����������򤷤Ƥ���ɬ�פ�����. 
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : im, jm, nm
  use wa_base_module, only : km, wa_base_Initial, xya_wa, wa_xya
  use w_deriv_module, only : rn, irm, w_Jacobian_w_w

  implicit none

  private
 
  public wa_Lapla_wa, wa_LaplaInv_wa          ! ��ץ饷����ȵձ黻
  public wa_DLon_wa                           ! ������ʬ
  public xya_GradLon_wa, xya_GradLat_wa       ! ���۷���ʬ
  public wa_DivLon_xya, wa_DivLat_xya         ! ȯ������ʬ
  public wa_Div_xya_xya                       ! ȯ������ʬ
  public wa_Jacobian_wa_wa                    ! �䥳�ӥ���
  public xya_GradLambda_wa, xya_GradMu_wa     ! ���۷���ʬ(��,�̺�ɸ)
  public wa_DivLambda_xya, wa_DivMu_xya       ! ȯ������ʬ(��,�̺�ɸ)

  contains

  !--------------- ��ʬ�׻� -----------------
    function wa_Lapla_wa(wa_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˥�ץ饷����
      !
      !    ��^2 = 1/cos^2�ա���^2/�ߦ�^2 + 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)
      !
      ! ����Ѥ���(¿����).
      !
      ! ���ڥ��ȥ�ǡ����Υ�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! ��ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: wa_lapla_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) ���ϥ��ڥ��ȥ�ǡ����Υ�ץ饷����
      integer :: l,k

      do k=1,size(wa_data,2)
        do l=1,(nm+1)**2
          wa_Lapla_wa(l,k) = rn(l,1)*wa_data(l,k)
        enddo
      enddo
      
    end function wa_Lapla_wa

    function wa_LaplaInv_wa(wa_data)
      !
      ! ���ϥ��ڥ��ȥ�ǡ����˵ե�ץ饷����
      !
      !    ��^{-2}
      !      =[1/cos^2�ա���^2/�ߦ�^2 + 1/cos�ա���/�ߦ�(cos�բ�/�ߦ�)]^{-1}
      !
      ! ����Ѥ���(¿����).
      !
      ! ���ڥ��ȥ�ǡ����εե�ץ饷����Ȥ�, �б�����ʻ����ǡ�����
      ! �ե�ץ饷�������Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���. 
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: wa_LaplaInv_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ����εե�ץ饷����
      integer :: l,k

      do k=1,size(wa_data,2)
         do l=1,(nm+1)**2
            wa_LaplaInv_wa(l,k) = rn(l,2)*wa_data(l,k)
         enddo
      enddo

    end function wa_LaplaInv_wa

    function wa_DLon_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˷�����ʬ ��/�ߦ� ����Ѥ�����(¿����).
      !
      ! ���ڥ��ȥ�ǡ����η�����ʬ�Ȥ�, �б�����ʻ����ǡ�����
      ! ������ʬ��/�ߦˤ���Ѥ������ǡ����Υ��ڥ��ȥ��Ѵ��Τ��ȤǤ���.
      ! 
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: wa_DLon_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ����η�����ʬ
      integer :: l,k

      do k=1,size(wa_data,2)
         do l=1,(nm+1)**2
            wa_DLon_wa(irm(l,1),k) = irm(l,2)*wa_data(l,k)
         enddo
      enddo

    end function wa_DLon_wa

    function xya_GradLon_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLon_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLon_wa = xya_wa(wa_data,ipow=1,iflag=-1)

    end function xya_GradLon_wa

    function xya_GradLat_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLat_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLat_wa = xya_wa(wa_data,ipow=1,iflag=1)

    end function xya_GradLat_wa

    function wa_DivLon_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLon_xya((nm+1)**2,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLon_xya = wa_xya(xya_data,ipow=1,iflag=-1)

    end function wa_DivLon_xya

    function wa_DivLat_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLat_xya((nm+1)**2,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLat_xya = wa_xya(xya_data,ipow=1,iflag=1)

    end function wa_DivLat_xya

    function wa_Div_xya_xya(xya_u,xya_v)
      !
      ! 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ����׻���, 
      ! ���ڥ��ȥ�ǡ����Ȥ����֤�(¿����).
      !
      real(8), intent(in)  :: xya_u(0:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���
      real(8), intent(in)  :: xya_v(0:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���
      real(8)              :: wa_Div_xya_xya((nm+1)**2,size(xya_u,3))
      !(out) 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ���Υ��ڥ��ȥ�ǡ���

      wa_Div_xya_xya = wa_DivLon_xya(xya_u) + wa_DivLat_xya(xya_v)

    end function wa_Div_xya_xya

    function wa_Jacobian_wa_wa(wa_a,wa_b)
      ! 2 �ĤΥ��ڥ��ȥ�ǡ����˥䥳�ӥ���
      !
      !   J(f,g) = ��f/�ߦˡ���g/�ߦ� - ��g/�ߦˡ���f/�ߦ�
      !          = ��f/�ߦˡ�1/cos�ա���g/�ߦ�
      !             - ��g/�ߦˡ�1/cos�ա���f/�ߦ�
      !
      ! ����Ѥ�����(¿����).
      !
      real(8), intent(in) :: wa_a(:,:)
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���
      real(8), intent(in) :: wa_b(:,:)
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���
      real(8)             :: wa_Jacobian_wa_wa((nm+1)**2,size(wa_a,2))
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���
      integer :: k

      do k=1,size(wa_a,2)
         wa_Jacobian_wa_wa(:,k) = w_Jacobian_w_w(wa_a(:,k),wa_b(:,k))
      end do
    end function wa_Jacobian_wa_wa


  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xya_GradLambda_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradLambda_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradLambda_wa = xya_wa(wa_data,ipow=0,iflag=-1)

    end function xya_GradLambda_wa

    function xya_GradMu_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      real(8)              :: xya_GradMu_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xya_GradMu_wa = xya_wa(wa_data,ipow=0,iflag=1)

    end function xya_GradMu_wa

    function wa_DivLambda_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivLambda_xya((nm+1)**2,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLambda_xya = wa_xya(xya_data,ipow=2,iflag=-1)

    end function wa_DivLambda_xya

    function wa_DivMu_xya(xya_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) ���ϳʻ����ǡ���
      real(8)              :: wa_DivMu_xya((nm+1)**2,size(xya_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivMu_xya = wa_xya(xya_data,ipow=2,iflag=1)

    end function wa_DivMu_xya

  end module wa_deriv_module

