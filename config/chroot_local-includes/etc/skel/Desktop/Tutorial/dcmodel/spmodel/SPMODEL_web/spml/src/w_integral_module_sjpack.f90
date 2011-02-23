!--
!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_integral_module_sjpack
!
!  spml/w_integral_module_sjpack モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module_sjpack の下部モジュールであり, 
!  積分・平均計算のための Fortran90 関数を提供する. 
!
!  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!
!履歴  2009/09/04  竹広真一   w_integral_module を改造, SJPACK 対応
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
  !== 概要
  !
  ! spml/w_integral_module_sjpack モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module_sjpack の下部モジュールであり, 
  ! 積分・平均計算のための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  use w_base_module_sjpack, only : im, jm, x_Lon_Weight, y_Lat_Weight

  implicit none

  private
 
  public IntLonLat_xy                      ! 緯度経度積分
  public y_IntLon_xy, IntLon_x             ! 経度積分    
  public x_IntLat_xy, IntLat_y             ! 緯度積分    
  public AvrLonLat_xy                      ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x             ! 経度平均    
  public x_AvrLat_xy, AvrLat_y             ! 緯度平均    

  contains

  !--------------- 積分計算 -----------------
    function IntLat_y(y_data)
      !
      ! 1 次元緯度(Y)格子点データの Y 方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: y_data(1:jm)  !(in)  1 次元緯度(Y)格子点データ
      real(8)             :: IntLat_y        !(out) 積分値

      IntLat_y = sum(y_data * y_Lat_weight)

    end function IntLat_y

    function IntLon_x(x_data)
      !
      ! 1 次元経度(X)格子点データの X 方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している.
      !
      real(8), intent(in) :: x_data(0:im-1)   !(in)  1 次元経度(X)格子点データ
      real(8)             :: IntLon_x     !(out) 積分値

      IntLon_x = sum(x_data * x_Lon_weight)

    end function IntLon_x

    function x_IntLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)           
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8)             :: x_IntLat_xy(0:im-1) 
      !(out) 積分された 1 次元経度(X)格子点データ

      integer :: j

      x_IntLat_xy = 0.0D0
      do j=1,jm
         x_IntLat_xy = x_IntLat_xy + xy_data(:,j) * y_Lat_weight(j)
      enddo

    end function x_IntLat_xy

    function y_IntLon_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8)             :: y_IntLon_xy(1:jm)
      !(out) 積分された 1 次元緯度(Y)格子点データ

      integer :: i

      y_IntLon_xy = 0.0D0
      do i=0,im-1
         y_IntLon_xy = y_IntLon_xy + xy_data(i,:) * x_Lon_weight(i)
      enddo

    end function y_IntLon_xy

    function IntLonLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(1 層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)         
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8) :: IntLonLat_xy                         
      !(out) 積分値

      IntLonLat_xy = IntLon_x(x_IntLat_xy(xy_data))

    end function IntLonLat_xy

  !--------------- 平均計算 -----------------
    function AvrLat_y(y_data)
      !
      ! 1 次元(Y)格子点データの緯度(Y)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: y_data(1:jm)         
      !(in)  1 次元緯度格子点データ
      real(8)             :: AvrLat_y            
      !(out) 平均値

      AvrLat_y = IntLat_y(y_data)/sum(y_Lat_weight)

    end function AvrLat_y

    function AvrLon_x(x_data)
      !
      ! 1 次元(X)格子点データの経度(X)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: x_data(0:im-1)
      !(in)  1 次元経度(X)格子点データ
      real(8)             :: AvrLon_x       
      !(out) 平均値

      AvrLon_x = IntLon_x(x_data)/sum(x_Lon_weight)

    end function AvrLon_x


    function x_AvrLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)
      real(8)             :: x_AvrLat_xy(im)
      !(out) 平均された 1 次元経度(X)格子点データ

      x_AvrLat_xy = x_IntLat_xy(xy_data)/sum(y_Lat_weight)

    end function x_AvrLat_xy

    function y_AvrLon_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)
      real(8)             :: y_AvrLon_xy(1:jm)
      !(out) 平均された 1 次元緯度(Y)格子点

      y_AvrLon_xy = y_IntLon_xy(xy_data)/sum(x_Lon_weight)

    end function y_AvrLon_xy


    function AvrLonLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ

      real(8) :: AvrLonLat_xy
      !(out) 平均値

      AvrLonLat_xy = AvrLon_x(x_AvrLat_xy(xy_data))

    end function AvrLonLat_xy


end module w_integral_module_sjpack

