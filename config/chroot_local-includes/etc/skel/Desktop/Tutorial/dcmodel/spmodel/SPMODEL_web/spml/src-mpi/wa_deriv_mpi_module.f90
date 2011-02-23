!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_deriv_mpi_module
!
!  spml/wa_deriv_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
!  ���ͷ׻����뤿��Υ⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
!  ���ڥ��ȥ�ˡ����ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_deriv_mpi_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!����  2008/05/28  �ݹ����� wa_deriv_module ��MPI ����
!
module wa_deriv_mpi_module
  !
  ! wa_deriv_mpi_module
  !
  !  spml/wa_deriv_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  !  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI ���󲽤ˤ�ä�
  !  ���ͷ׻����뤿��Υ⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
  !  ���ڥ��ȥ�ˡ����ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  !  ���̾�� 1 �إ�ǥ��� w_deriv_mpi_module �⥸�塼���¿�إ�ǥ��Ѥ�
  !  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  !  �Ф����Ѵ����Ԥ���.
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use dc_message, only : MessageNotify
  use w_base_module,  only : im, jm, nm
  use w_base_mpi_module,  only : jc
  use wa_base_mpi_module, only : xva_wa, wa_xva
  use w_deriv_mpi_module, only : w_JacobianMPI_w_w

  implicit none

  private
  private im, jm, nm                          ! Intel Fortran �к�
 
  public xva_GradLon_wa, xva_GradLat_wa       ! ���۷���ʬ
  public wa_DivLon_xva, wa_DivLat_xva         ! ȯ������ʬ
  public wa_Div_xva_xva                       ! ȯ������ʬ
  public wa_JacobianMPI_wa_wa                 ! �䥳�ӥ���
  public xva_GradLambda_wa, xva_GradMu_wa     ! ���۷���ʬ(��,�̺�ɸ)
  public wa_DivLambda_xva, wa_DivMu_xva       ! ȯ������ʬ(��,�̺�ɸ)

  contains

  !--------------- ��ʬ�׻� -----------------

    function xva_GradLon_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8)              :: xva_GradLon_wa(im,jc,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xva_GradLon_wa = xva_wa(wa_data,ipow=1,iflag=-1)
    end function xva_GradLon_wa

    function xva_GradLat_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8)              :: xva_GradLat_wa(im,jc,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xva_GradLat_wa = xva_wa(wa_data,ipow=1,iflag=1)
    end function xva_GradLat_wa

    function wa_DivLon_xva(xva_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: wa_DivLon_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLon_xva = wa_xva(xva_data,ipow=1,iflag=-1)
    end function wa_DivLon_xva

    function wa_DivLat_xva(xva_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !

      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: wa_DivLat_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLat_xva = wa_xva(xva_data,ipow=1,iflag=1)
    end function wa_DivLat_xva

    function wa_Div_xva_xva(xva_u,xva_v)
      !
      ! 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ����׻���, 
      ! ���ڥ��ȥ�ǡ����Ȥ����֤�(¿����).
      !
      real(8), intent(in)  :: xva_u(:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      real(8), intent(in)  :: xva_v(:,:,:)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      real(8)              :: wa_Div_xva_xva((nm+1)*(nm+1),size(xva_u,3))
      !(out) 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ���Υ��ڥ��ȥ�ǡ���

      wa_Div_xva_xva = wa_DivLon_xva(xva_u) + wa_DivLat_xva(xva_v)
    end function wa_Div_xva_xva

    function wa_JacobianMPI_wa_wa(wa_a,wa_b)
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

      real(8)             :: wa_JacobianMPI_wa_wa((nm+1)*(nm+1),size(wa_a,2))
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      integer :: k

      do k=1,size(wa_a,2)
         wa_JacobianMPI_wa_wa(:,k) = w_JacobianMPI_w_w(wa_a(:,k),wa_b(:,k))
      end do
    end function wa_JacobianMPI_wa_wa


  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xva_GradLambda_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8)              :: xva_GradLambda_wa(im,jc,size(wa_data,2))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xva_GradLambda_wa = xva_wa(wa_data,ipow=0,iflag=-1)
    end function xva_GradLambda_wa

    function xva_GradMu_wa(wa_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      real(8)              :: xva_GradMu_wa(im,jc,size(wa_data,2))
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      xva_GradMu_wa = xva_wa(wa_data,ipow=0,iflag=1)
    end function xva_GradMu_wa

    function wa_DivLambda_xva(xva_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !
      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: wa_DivLambda_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivLambda_xva = wa_xva(xva_data,ipow=2,iflag=-1)
    end function wa_DivLambda_xva

    function wa_DivMu_xva(xva_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(¿����).
      !

      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) ���ϳʻ����ǡ���

      real(8)              :: wa_DivMu_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      wa_DivMu_xva = wa_xva(xva_data,ipow=2,iflag=1)
    end function wa_DivMu_xva

end module wa_deriv_mpi_module
