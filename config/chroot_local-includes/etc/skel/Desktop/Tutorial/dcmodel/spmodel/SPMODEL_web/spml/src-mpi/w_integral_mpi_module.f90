!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_integral_mpi_module
!
!  spml/w_integral_mpi_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ���� MPI ����饤�֥����Ѥ������ڥ��ȥ�ˡ�ˤ�ä�n
!  ���ͷ׻����뤿��� �⥸�塼�� w_mpi_module �β����⥸�塼��Ǥ���, 
!  ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2002/05/25  �ݹ����� 
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2008/06/06  �ݹ�����  MPI ����
!      2010/01/07  ��������ʿ  RDoc �ѤΥɥ�����Ƚ���, 
!                              include 'mpif.h' -> use mpi
!
module w_integral_mpi_module
  !
  ! w_integral_mpi_module
  !
  !  spml/w_integral_mpi_module �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  !  ����Ĵ��ȡ���� MPI ����饤�֥����Ѥ������ڥ��ȥ�ˡ�ˤ�ä�n
  !  ���ͷ׻����뤿��� �⥸�塼�� w_mpi_module �β����⥸�塼��Ǥ���, 
  !  ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use w_base_module, only : im, x_Lon_Weight, y_Lat_Weight
  use w_base_mpi_module, only : jc, v_Lat_Weight
  use w_integral_module, only : IntLon_x, AvrLon_x
  use mpi

  implicit none
  integer :: ierr

  private
  private im
 
  public IntLonLat_xv                      ! ���ٷ�����ʬ
  public v_IntLon_xv                       ! ������ʬ    
  public x_IntLat_xv, IntLat_v             ! ������ʬ    
  public AvrLonLat_xv                      ! ���ٷ���ʿ��
  public v_AvrLon_xv                       ! ����ʿ��    
  public x_AvrLat_xv, AvrLat_v             ! ����ʿ��    

  contains

  !--------------- ��ʬ�׻� -----------------
    function IntLonLat_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(1 ����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, v_V_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xv_data(0:im-1,jc)         
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8) :: IntLonLat_xv                         
      !(out) ��ʬ��

      IntLonLat_xv = IntLon_x(x_IntLat_xv(xv_data))
    end function IntLonLat_xv

    function x_IntLat_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)           
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8)             :: x_IntLat_xv(0:im-1) 
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ���

      real(8)             :: x_IntLatTmp(0:im-1) 
      integer :: j

      x_IntLat_xv = 0
      do j=1,jc
         x_IntLat_xv = x_IntLat_xv + xv_data(:,j) * v_Lat_weight(j)
      enddo

      x_IntLatTmp=x_IntLat_xv
      CALL MPI_ALLREDUCE(x_IntLatTMP,x_IntLat_xv,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function x_IntLat_xv

    function v_IntLon_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8)             :: v_IntLon_xv(jc)
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ���

      integer :: i

      v_IntLon_xv = 0
      do i=0,im-1
         v_IntLon_xv = v_IntLon_xv + xv_data(i,:) * x_Lon_weight(i)
      enddo

    end function v_IntLon_xv

    function IntLat_v(v_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: v_data(jc)      !(in)  1 ��������(Y)�ʻ����ǡ���
      real(8)             :: IntLat_v        !(out) ��ʬ��

      real(8)             :: IntLatTmp

      IntLat_v = sum(v_data * v_Lat_weight)
      IntLatTmp=IntLat_v
      CALL MPI_ALLREDUCE(IntLatTMP,IntLat_v,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLat_v

  !--------------- ʿ�ѷ׻� -----------------
    function AvrLonLat_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, v_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*v_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xv_data(0:im-1,jc)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8) :: AvrLonLat_xv
      !(out) ʿ����

      AvrLonLat_xv = AvrLon_x(x_AvrLat_xv(xv_data))
    end function AvrLonLat_xv

    function x_AvrLat_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Lat_Weight �򤫤������¤�׻���,
      ! y_Lat_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)          ! �ʻ���(0:im-1,jc)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8)             :: x_AvrLat_xv(im)
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ���

      x_AvrLat_xv = x_IntLat_xv(xv_data)/sum(y_Lat_weight)

    end function x_AvrLat_xv

    function v_AvrLon_xv(xv_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,jc)

      real(8)             :: v_AvrLon_xv(jc)
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ���

      v_AvrLon_xv = v_IntLon_xv(xv_data)/sum(x_Lon_weight)

    end function v_AvrLon_xv

    function AvrLat_v(v_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� v_Y_Weight �򤫤������¤�׻���, 
      ! v_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: v_data(jc)          !(in)  1 �������ٳʻ����ǡ���
      real(8)             :: AvrLat_v            !(out) ʿ����

      AvrLat_v = IntLat_v(v_data)/sum(y_Lat_weight)

    end function AvrLat_v

  end module w_integral_mpi_module
