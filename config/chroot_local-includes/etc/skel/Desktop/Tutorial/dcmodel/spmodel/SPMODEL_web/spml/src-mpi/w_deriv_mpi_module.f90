!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
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
!
!����  2008/05/26  �ݹ�����  w_deriv_module �� MPI ��
!
module w_deriv_mpi_module
  !
  ! w_deriv_mpi_module
  !
  !  spml/w_deriv_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  !  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�� MPI �ˤ�äƿ��ͷ׻����뤿��� 
  !  �⥸�塼�� w_mpi_module �β����⥸�塼��Ǥ���, ���ڥ��ȥ�ˡ��
  !  ��ʬ�׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : im, jm, nm
  use w_base_mpi_module, only : it, t, y, ip, p, r, ia, a, &
                                jc, id, jd, xv_w, w_xv
  implicit none
  private im, jm, nm                          ! Intel Fortran �к�

  integer, allocatable  :: ip2(:), ip3(:)     ! �䥳�ӥ���׻�������
  real(8), allocatable  :: p2(:), p3(:)       ! �䥳�ӥ���׻�������
  real(8), allocatable  :: r2(:), r3(:)       ! �䥳�ӥ���׻�������

  real(8), allocatable  :: q(:)               ! �������
  real(8), allocatable  :: ww(:),ws(:),w(:)   ! �������

  private

  public w_deriv_mpi_Initial                  ! �����
  public xv_GradLon_w, xv_GradLat_w           ! ���۷���ʬ
  public w_DivLon_xv, w_DivLat_xv             ! ȯ������ʬ
  public w_Div_xv_xv                          ! ȯ������ʬ
  public w_JacobianMPI_w_w                    ! �䥳�ӥ���
  public xv_GradLambda_w, xv_GradMu_w         ! ���۷���ʬ(��,�̺�ɸ)
  public w_DivLambda_xv, w_DivMu_xv           ! ȯ������ʬ(��,�̺�ɸ)

  save ip2, ip3, p2, p3, r2, r3

  contains

  !--------------- ����� -----------------
    subroutine w_deriv_mpi_initial
      !
      ! ���ڥ��ȥ��Ѵ��γʻ�����, �ȿ������ꤹ��.
      !
      ! ¾�δؿ���Ƥ�����, �ǽ�ˤ��Υ��֥롼�����Ƥ�ǽ�������
      ! ���ʤ���Фʤ�ʤ�. 
      !
      integer iw

      allocate(ip2(2*((nm+1)/2+nm+1)*2))      ! �䥳�ӥ���׻�������
      allocate(p2(2*((nm+1)/2+nm+1)*jm))      ! �䥳�ӥ���׻�������
      allocate(r2(2*((nm+1)/2*2+3)*(nm/2+1))) ! �䥳�ӥ���׻�������
      allocate(ip3(3*((nm+1)/2+nm+1)*2))      ! �䥳�ӥ���׻�������
      allocate(p3(3*((nm+1)/2+nm+1)*jm))      ! �䥳�ӥ���׻�������
      allocate(r3(3*((nm+1)/2*2+3)*(nm/2+1))) ! �䥳�ӥ���׻�������
      call snkini(nm,jc,2,ip,p,r,ip2,p2,r2)
      call snkini(nm,jc,3,ip,p,r,ip3,p3,r3)

      allocate(q(3*((nm+1)/2+nm+1)*jm))       ! ���������
      iw=3*max( ((nm+1)/2*2+3)*(nm/2+2)*2, &
                jm*((nm+1)/2+nm+1)*2, jm*jm )
      allocate(ws(iw),ww(iw),w((nm+1)*(nm+1)))   ! ���������

      call MessageNotify('M','w_deriv_mpi_initial', &
           'w_deriv_mpi_module is initialized')

    end subroutine w_deriv_mpi_initial

  !--------------- ��ʬ�׻� -----------------
    function xv_GradLon_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ 1/cos�ա���/�ߦ� ��
      ! ���Ѥ������ʻ����ǡ������֤�(1 ����).
      !
      real(8)              :: xv_GradLon_w(0:im-1,jc)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xv_GradLon_w = xv_w(w_data,ipow=1,iflag=-1)
    end function xv_GradLon_w

    function xv_GradLat_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ�����
      ! �ʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: xv_GradLat_w(0:im-1,jc)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xv_GradLat_w = xv_w(w_data,ipow=1,iflag=1)
    end function xv_GradLat_w

    function w_DivLon_xv(xv_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLon_xv((nm+1)*(nm+1))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���
      
      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) ���ϳʻ����ǡ���

      w_DivLon_xv = w_xv(xv_data,ipow=1,iflag=-1)
    end function w_DivLon_xv

    function w_DivLat_xv(xv_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/cos�ա���(f cos��)/�ߦ� ����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLat_xv((nm+1)*(nm+1))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) ���ϳʻ����ǡ���

      w_DivLat_xv = w_xv(xv_data,ipow=1,iflag=1)
    end function w_DivLat_xv

    function w_Div_xv_xv(xv_u,xv_v)
      !
      ! 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ����׻���, 
      ! ���ڥ��ȥ�ǡ����Ȥ����֤�(1 ����).
      !
      real(8)              :: w_Div_xv_xv((nm+1)*(nm+1))
      !(out) 2 �Ĥ����ϳʻ����ǡ�����٥��ȥ���ʬ�Ȥ���ȯ���Υ��ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xv_u(0:im-1,jc)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      real(8), intent(in)  :: xv_v(0:im-1,jc)
      !(in) �٥��ȥ������ʬ�γʻ����ǡ���

      w_Div_xv_xv = w_Divlon_xv(xv_u) + w_Divlat_xv(xv_v)
    end function w_Div_xv_xv

    function w_JacobianMPI_w_w(w_a,w_b)
      ! 2 �ĤΥ��ڥ��ȥ�ǡ����˥䥳�ӥ���
      !
      !   J(f,g) = ��f/�ߦˡ���g/�ߦ� - ��g/�ߦˡ���f/�ߦ�
      !          = ��f/�ߦˡ�1/cos�ա���g/�ߦ�
      !             - ��g/�ߦˡ�1/cos�ա���f/�ߦ�
      !
      ! ����Ѥ�����(1 ����).

      real(8)             :: w_JacobianMPI_w_w((nm+1)*(nm+1))
      !(out) 2 �ĤΥ��ڥ��ȥ�ǡ����Υ䥳�ӥ���

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1���ܤ����ϥ��ڥ��ȥ�ǡ���
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2���ܤ����ϥ��ڥ��ȥ�ǡ���

      call spmjcb(nm,im,id,jc,jd,w_a,w_b,w_JacobianMPI_w_w,&
           it,t,y,ip2,p2,r2,ip3,p3,r3,ia,a,q,ws,ww,w)
    end function w_JacobianMPI_w_w

  !--------------- ��ʬ�׻� (��,�̺�ɸ����) -----------------
    function xv_GradLambda_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ ��/�ߦ� ����Ѥ���(1 ����).
      !
      real(8)              :: xv_GradLambda_w(0:im-1,jc)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���
      
      xv_GradLambda_w = xv_w(w_data,ipow=0,iflag=-1)
    end function xv_GradLambda_w

    function xv_GradMu_w(w_data)
      !
      ! ���ڥ��ȥ�ǡ����˸��۷�������ʬ (1-��^2)��/�ߦ�  (��=sin��)
      ! ����Ѥ����Ƴʻ����ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: xv_GradMu_w(0:im-1,jc)
      !(out) ���ڥ��ȥ�ǡ�������۷�������ʬ�����ʻ����ǡ���

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) ���ϥ��ڥ��ȥ�ǡ���

      xv_GradMu_w = xv_w(w_data,ipow=0,iflag=1)
    end function xv_GradMu_w

    function w_DivLambda_xv(xv_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ 1/(1-��^2)����/�ߦ� (��=sin��) 
      ! ����Ѥ����ƥ��ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivLambda_xv((nm+1)*(nm+1))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) ���ϳʻ����ǡ���

      w_DivLambda_xv = w_xv(xv_data,ipow=2,iflag=-1)
    end function w_DivLambda_xv

    function w_DivMu_xv(xv_data)
      !
      ! �ʻ����ǡ�����ȯ����������ʬ ��/�ߦ� (��=sin��)����Ѥ�����
      ! ���ڥ��ȥ�ǡ������Ѵ������֤�(1 ����).
      !
      real(8)              :: w_DivMu_xv((nm+1)*(nm+1))
      !(out) �ʻ����ǡ�����ȯ����������ʬ�������ڥ��ȥ�ǡ���

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) ���ϳʻ����ǡ���

      w_DivMu_xv = w_xv(xv_data,ipow=2,iflag=1)
    end function w_DivMu_xv

end module w_deriv_mpi_module
