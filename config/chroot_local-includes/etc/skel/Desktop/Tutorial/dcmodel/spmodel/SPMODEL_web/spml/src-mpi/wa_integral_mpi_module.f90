!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_integral_mpi_module
!
!  spml/wa_integral_mpi_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ���� MPI ����饤�֥����Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
!  ���ͷ׻����뤿��� �⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
!  ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ���̾�� 1 �إ�ǥ��� w_integral_module �⥸�塼���¿�إ�ǥ��Ѥ�
!  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
!  �Ф����Ѵ����Ԥ���.
!
!  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2002/05/25  �ݹ����� 
!      2006/03/08  �ݹ�����  �����Ȥ� RDoc �Ѥ˽���
!      2008/06/06  �ݹ�����  MPI ����
!      2008/08/28  �ݹ�����  ����ź�����ѹ�
!      2010/01/07  ��������ʿ  RDoc �ѤΥɥ�����Ƚ���, 
!                              include 'mpif.h' -> use mpi
!
module wa_integral_mpi_module
  !
  ! wa_integral_mpi_module
  !
  !  spml/wa_integral_mpi_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  !  ����Ĵ��ȡ���� MPI ����饤�֥����Ѥ������ڥ��ȥ�ˡ�ˤ�ä�
  !  ���ͷ׻����뤿��� �⥸�塼�� wa_mpi_module �β����⥸�塼��Ǥ���, 
  !  ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  !  ���̾�� 1 �إ�ǥ��� w_integral_mpi_module �⥸�塼���¿�إ�ǥ��Ѥ�
  !  ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  !  �Ф����Ѵ����Ԥ���.
  !
  !  ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  !  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  !  �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !
  use w_base_module, only : im, x_Lon_Weight, y_Lat_Weight
  use w_base_mpi_module, only : jc, v_Lat_Weight
  use wa_integral_module, only : a_IntLon_xa, a_AvrLon_xa
  use mpi

  implicit none
  integer :: ierr

  private
  private im

  public a_IntLonLat_xva                      ! ���ٷ�����ʬ
  public va_IntLon_xva                        ! ������ʬ    
  public xa_IntLat_xva, a_IntLat_va           ! ������ʬ    
  public a_AvrLonLat_xva                      ! ���ٷ���ʿ��
  public va_AvrLon_xva                        ! ����ʿ��    
  public xa_AvrLat_xva, a_AvrLat_va           ! ����ʿ��    

  contains

  !--------------- ��ʬ�׻� -----------------
    function a_IntLonLat_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(¿����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8) :: a_IntLonLat_xva(size(xva_data,3))
      !(out) ��ʬ���줿�ǡ������¤�(*)

      a_IntLonLat_xva = a_IntLon_xa(xa_IntLat_xva(xva_data))
    end function a_IntLonLat_xva

    function xa_IntLat_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8)             :: xa_IntLat_xva(0:im-1,size(xva_data,3))
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ������¤�

      real(8)             :: xa_IntLatTmp(0:im-1,size(xva_data,3))
      Integer :: j

      xa_IntLat_xva = 0
      do j=1,jc
         xa_IntLat_xva = xa_IntLat_xva + xva_data(:,j,:) * v_Lat_Weight(j)
      enddo

      xa_IntLatTmp=xa_IntLat_xva
      CALL MPI_ALLREDUCE(xa_IntLatTMP,xa_IntLat_xva,im*size(xva_data,3),MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function xa_IntLat_xva

    function va_IntLon_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xva_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8)             :: va_IntLon_xva(jc,size(xva_data,3))
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ������¤�

      integer :: i

      va_IntLon_xva = 0
      do i=0,im-1
         va_IntLon_xva = va_IntLon_xva + xva_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function va_IntLon_xva

    function a_IntLat_va(va_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: va_data(:,:)
      !(in)  1 ��������(Y)�ʻ����ǡ������¤�(jc,*)

      real(8)             :: a_IntLat_va(size(va_data,2))
      !(out) ��ʬ�ͤ��¤�(*)

      real(8)             :: a_IntLatTmp(size(va_data,2))
      integer :: j

      a_IntLat_va = 0
      do j=1,jc
         a_IntLat_va = a_IntLat_va + va_data(j,:) * v_Lat_Weight(j)
      enddo

      a_IntLatTmp=a_IntLat_va
      CALL MPI_ALLREDUCE(a_IntLatTMP,a_IntLat_va,size(va_data,2),MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function a_IntLat_va

  !--------------- ʿ�ѷ׻� -----------------
    function a_AvrLonLat_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8) :: a_AvrLonLat_xva(size(xva_data,3))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLonLat_xva = a_AvrLon_xa(xa_AvrLat_xva(xva_data))
    end function a_AvrLonLat_xva

    function xa_AvrLat_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8)             :: xa_AvrLat_xva(0:im-1,size(xva_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ������¤�(0:im-1,*)

      xa_AvrLat_xva = xa_IntLat_xva(xva_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xva

    function va_AvrLon_xva(xva_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,jc,*)

      real(8)             :: va_AvrLon_xva(jc,size(xva_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ������¤�(jc,*)

      va_AvrLon_xva = va_IntLon_xva(xva_data)/sum(x_Lon_Weight)

    end function va_AvrLon_xva

    function a_AvrLat_va(va_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: va_data(:,:)
      !(in)  1 �������ٳʻ����ǡ������¤�(jc,*)

      real(8)             :: a_AvrLat_va(size(va_data,2))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLat_va = a_IntLat_va(va_data)/sum(y_Lat_Weight)

    end function a_AvrLat_va

end module wa_integral_mpi_module
