!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  wa_integral_module
!
!  spml/wa_integral_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, 
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
!      2008/02/23  ��������ʿ ��ʿ�����γʻ����ǡ�������λ����� 1 ���� 0 ��.
!      2008/06/21  ��������ʿ ��ʿ�����γʻ����ǡ�������� 0:im-1,1:jm ��.
!      2008/06/28  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!      2008/07/07  �ݹ�����  ����ǽ�����������������Ƥ����ѹ�
!      2009/01/29  ��������ʿ �����Ȥ� RDoc �Ѥ�������
!
!
!++
module wa_integral_module
  !
  != wa_integral_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_integral_module.f90,v 1.9 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/wa_integral_module �⥸�塼��ϵ��̾�Ǥ�ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� wa_module �β����⥸�塼��Ǥ���, 
  ! ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ���̾�� 1 �إ�ǥ��� w_integral_module �⥸�塼���¿�إ�ǥ��Ѥ�
  ! ��ĥ������ΤǤ���, Ʊ����ʣ���ĤΥ��ڥ��ȥ�ǡ���, �ʻ����ǡ�����
  ! �Ф����Ѵ����Ԥ���.
  !
  ! ������ ISPACK �� SPPACK �� SNPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SNPACK,SPPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  !
  use w_base_module, only : im, jm, x_Lon_Weight, y_Lat_Weight
  use wa_base_module, only : km
  implicit none

  private

  public a_IntLonLat_xya                      ! ���ٷ�����ʬ
  public ya_IntLon_xya, a_IntLon_xa           ! ������ʬ    
  public xa_IntLat_xya, a_IntLat_ya           ! ������ʬ    
  public a_AvrLonLat_xya                      ! ���ٷ���ʿ��
  public ya_AvrLon_xya, a_AvrLon_xa           ! ����ʿ��    
  public xa_AvrLat_xya, a_AvrLat_ya           ! ����ʿ��    

  contains

  !--------------- ��ʬ�׻� -----------------
    function a_IntLonLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(¿����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)
      real(8) :: a_IntLonLat_xya(size(xya_data,3))
      !(out) ��ʬ���줿�ǡ������¤�(*)

      a_IntLonLat_xya = a_IntLon_xa(xa_IntLat_xya(xya_data))

    end function a_IntLonLat_xya

    function xa_IntLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)
      real(8)             :: xa_IntLat_xya(0:im-1,size(xya_data,3))
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ������¤�
      Integer :: j

      xa_IntLat_xya = 0.0D0
      do j=1,jm
         xa_IntLat_xya = xa_IntLat_xya + xya_data(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xa_IntLat_xya

    function ya_IntLon_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: ya_IntLon_xya(1:jm,size(xya_data,3))
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ������¤�

      integer :: i

      ya_IntLon_xya = 0.0D0
      do i=0,im-1
         ya_IntLon_xya = ya_IntLon_xya + xya_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function ya_IntLon_xya

    function a_IntLat_ya(ya_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  1 ��������(Y)�ʻ����ǡ������¤�(1:jm,*)

      real(8)             :: a_IntLat_ya(size(ya_data,2))
      !(out) ��ʬ�ͤ��¤�(*)

      integer :: j

      a_IntLat_ya = 0.0D0
      do j=1,jm
         a_IntLat_ya = a_IntLat_ya + ya_data(j,:) * y_Lat_Weight(j)
      enddo

    end function a_IntLat_ya

    function a_IntLon_xa(xa_data)          ! ������ʬ
      !
      ! 1 ��������(X)�ʻ����ǡ����� X ������ʬ(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 ��������(X)�ʻ����ǡ������¤�(0:im-1,*)
      real(8)             :: a_IntLon_xa(size(xa_data,2))
      !(out) ��ʬ�ͤ��¤�(*)
      integer :: i

      a_IntLon_xa = 0.0D0
      do i=0,im-1
         a_IntLon_xa = a_IntLon_xa + xa_data(i,:) * x_Lon_Weight(i)
      enddo

    end function a_IntLon_xa

  !--------------- ʿ�ѷ׻� -----------------
    function a_AvrLonLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8) :: a_AvrLonLat_xya(size(xya_data,3))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLonLat_xya = a_AvrLon_xa(xa_AvrLat_xya(xya_data))

    end function a_AvrLonLat_xya

    function xa_AvrLat_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: xa_AvrLat_xya(0:im-1,size(xya_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ������¤�(im,*)

      xa_AvrLat_xya = xa_IntLat_xya(xya_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xya

    function ya_AvrLon_xya(xya_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 �������ٰ��ٳʻ����ǡ������¤�(0:im-1,1:jm,*)

      real(8)             :: ya_AvrLon_xya(1:jm,size(xya_data,3))
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ������¤�(1:jm,*)

      ya_AvrLon_xya = ya_IntLon_xya(xya_data)/sum(x_Lon_Weight)

    end function ya_AvrLon_xya

    function a_AvrLat_ya(ya_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  1 �������ٳʻ����ǡ������¤�(1:jm,*)

      real(8)             :: a_AvrLat_ya(size(ya_data,2))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLat_ya = a_IntLat_ya(ya_data)/sum(y_Lat_Weight)

    end function a_AvrLat_ya

    function a_AvrLon_xa(xa_data)          ! ����ʿ��
      !
      ! 1 ����(X)�ʻ����ǡ����η���(X)����ʿ��(¿����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 ��������(X)�ʻ����ǡ������¤�(0:im-1,*)

      real(8)             :: a_AvrLon_xa(size(xa_data,2))
      !(out) ʿ���ͤ��¤�(*)

      a_AvrLon_xa = a_IntLon_xa(xa_data)/sum(x_Lon_Weight)

    end function a_AvrLon_xa

  end module wa_integral_module
