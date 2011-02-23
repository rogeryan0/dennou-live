!--
!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!ɽ��  w_integral_module_sjpack
!
!  spml/w_integral_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
!  ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
!  �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, 
!  ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
!
!  ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
!  ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
!  �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
!
!
!����  2009/09/04  �ݹ�����   w_integral_module ���¤, SJPACK �б�
!
!--
module w_integral_module_sjpack
  !
  != w_integral_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_integral_module_sjpack.f90,v 1.1 2009-09-07 07:26:48 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== ����
  !
  ! spml/w_integral_module_sjpack �⥸�塼��ϵ��̾�Ǥ� 2 ����ή�α�ư��
  ! ����Ĵ��ȡ�����Ѥ������ڥ��ȥ�ˡ�ˤ�äƿ��ͷ׻����뤿��� 
  ! �⥸�塼�� w_module_sjpack �β����⥸�塼��Ǥ���, 
  ! ��ʬ��ʿ�ѷ׻��Τ���� Fortran90 �ؿ����󶡤���. 
  !
  ! ������ ISPACK �� SJPACK �� Fortran77 ���֥롼�����Ƥ�Ǥ���. 
  ! ���ڥ��ȥ�ǡ�������ӳʻ����ǡ����γ�Ǽ��ˡ���Ѵ��ξܤ����׻�ˡ��
  ! �Ĥ��Ƥ� ISPACK/SJPACK �Υޥ˥奢��򻲾Ȥ��줿��.
  !
  use w_base_module_sjpack, only : im, jm, x_Lon_Weight, y_Lat_Weight

  implicit none

  private
 
  public IntLonLat_xy                      ! ���ٷ�����ʬ
  public y_IntLon_xy, IntLon_x             ! ������ʬ    
  public x_IntLat_xy, IntLat_y             ! ������ʬ    
  public AvrLonLat_xy                      ! ���ٷ���ʿ��
  public y_AvrLon_xy, AvrLon_x             ! ����ʿ��    
  public x_AvrLat_xy, AvrLat_y             ! ����ʿ��    

  contains

  !--------------- ��ʬ�׻� -----------------
    function IntLat_y(y_data)
      !
      ! 1 ��������(Y)�ʻ����ǡ����� Y ������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)  !(in)  1 ��������(Y)�ʻ����ǡ���
      real(8)             :: IntLat_y        !(out) ��ʬ��

      IntLat_y = sum(y_data * y_Lat_weight)

    end function IntLat_y

    function IntLon_x(x_data)
      !
      ! 1 ��������(X)�ʻ����ǡ����� X ������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���.
      !
      real(8), intent(in) :: x_data(0:im-1)   !(in)  1 ��������(X)�ʻ����ǡ���
      real(8)             :: IntLon_x     !(out) ��ʬ��

      IntLon_x = sum(x_data * x_Lon_weight)

    end function IntLon_x

    function x_IntLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)           
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8)             :: x_IntLat_xy(0:im-1) 
      !(out) ��ʬ���줿 1 ��������(X)�ʻ����ǡ���

      integer :: j

      x_IntLat_xy = 0.0D0
      do j=1,jm
         x_IntLat_xy = x_IntLat_xy + xy_data(:,j) * y_Lat_weight(j)
      enddo

    end function x_IntLat_xy

    function y_IntLon_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)������ʬ(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻����Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8)             :: y_IntLon_xy(1:jm)
      !(out) ��ʬ���줿 1 ��������(Y)�ʻ����ǡ���

      integer :: i

      y_IntLon_xy = 0.0D0
      do i=0,im-1
         y_IntLon_xy = y_IntLon_xy + xy_data(i,:) * x_Lon_weight(i)
      enddo

    end function y_IntLon_xy

    function IntLonLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ���ʬ(1 ����). 
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻����Ƥ���. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)         
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)

      real(8) :: IntLonLat_xy                         
      !(out) ��ʬ��

      IntLonLat_xy = IntLon_x(x_IntLat_xy(xy_data))

    end function IntLonLat_xy

  !--------------- ʿ�ѷ׻� -----------------
    function AvrLat_y(y_data)
      !
      ! 1 ����(Y)�ʻ����ǡ����ΰ���(Y)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: y_data(1:jm)         
      !(in)  1 �������ٳʻ����ǡ���
      real(8)             :: AvrLat_y            
      !(out) ʿ����

      AvrLat_y = IntLat_y(y_data)/sum(y_Lat_weight)

    end function AvrLat_y

    function AvrLon_x(x_data)
      !
      ! 1 ����(X)�ʻ����ǡ����η���(X)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: x_data(0:im-1)
      !(in)  1 ��������(X)�ʻ����ǡ���
      real(8)             :: AvrLon_x       
      !(out) ʿ����

      AvrLon_x = IntLon_x(x_data)/sum(x_Lon_weight)

    end function AvrLon_x


    function x_AvrLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����ΰ���(Y)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� y_Y_Weight �򤫤������¤�׻���, 
      ! y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)
      real(8)             :: x_AvrLat_xy(im)
      !(out) ʿ�Ѥ��줿 1 ��������(X)�ʻ����ǡ���

      x_AvrLat_xy = x_IntLat_xy(xy_data)/sum(y_Lat_weight)

    end function x_AvrLat_xy

    function y_AvrLon_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ����η���(X)����ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight �򤫤������¤�׻���, 
      ! x_X_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���(0:im-1,1:jm)
      real(8)             :: y_AvrLon_xy(1:jm)
      !(out) ʿ�Ѥ��줿 1 ��������(Y)�ʻ���

      y_AvrLon_xy = y_IntLon_xy(xy_data)/sum(x_Lon_weight)

    end function y_AvrLon_xy


    function AvrLonLat_xy(xy_data)
      !
      ! 2 �������ٷ��ٳʻ����ǡ��������ΰ�ʿ��(1 ����).
      !
      ! �ºݤˤϳʻ����ǡ���������� x_X_Weight, y_Y_Weight �򤫤���
      ! ���¤�׻���, x_X_Weight*y_Y_Weight �����¤ǳ�뤳�Ȥ�ʿ�Ѥ��Ƥ���. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 2 �������ٰ��ٳʻ����ǡ���

      real(8) :: AvrLonLat_xy
      !(out) ʿ����

      AvrLonLat_xy = AvrLon_x(x_AvrLat_xy(xy_data))

    end function AvrLonLat_xy


end module w_integral_module_sjpack

