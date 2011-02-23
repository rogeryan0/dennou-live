!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_integral_module
!
!  spml/wa_integral_module モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module の下部モジュールであり, 
!  積分・平均計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_integral_module モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2002/05/25  竹広真一 
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2008/02/23  佐々木洋平 水平方向の格子点データ配列の始点を 1 から 0 へ.
!      2008/06/21  佐々木洋平 水平方向の格子点データ配列を 0:im-1,1:jm へ.
!      2008/06/28  佐々木洋平 コメントを RDoc 用に微修正
!      2008/07/07  竹広真一  配列最終次元の不定性を許容する変更
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
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
  !== 概要
  !
  ! spml/wa_integral_module モジュールは球面上での流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール wa_module の下部モジュールであり, 
  ! 積分・平均計算のための Fortran90 関数を提供する. 
  !
  ! 球面上の 1 層モデル用 w_integral_module モジュールを多層モデル用に
  ! 拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  ! 対する変換が行える.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  !
  use w_base_module, only : im, jm, x_Lon_Weight, y_Lat_Weight
  use wa_base_module, only : km
  implicit none

  private

  public a_IntLonLat_xya                      ! 緯度経度積分
  public ya_IntLon_xya, a_IntLon_xa           ! 経度積分    
  public xa_IntLat_xya, a_IntLat_ya           ! 緯度積分    
  public a_AvrLonLat_xya                      ! 緯度経度平均
  public ya_AvrLon_xya, a_AvrLon_xa           ! 経度平均    
  public xa_AvrLat_xya, a_AvrLat_ya           ! 緯度平均    

  contains

  !--------------- 積分計算 -----------------
    function a_IntLonLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(多層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)
      real(8) :: a_IntLonLat_xya(size(xya_data,3))
      !(out) 積分されたデータの並び(*)

      a_IntLonLat_xya = a_IntLon_xa(xa_IntLat_xya(xya_data))

    end function a_IntLonLat_xya

    function xa_IntLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)
      real(8)             :: xa_IntLat_xya(0:im-1,size(xya_data,3))
      !(out) 積分された 1 次元経度(X)格子点データの並び
      Integer :: j

      xa_IntLat_xya = 0.0D0
      do j=1,jm
         xa_IntLat_xya = xa_IntLat_xya + xya_data(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xa_IntLat_xya

    function ya_IntLon_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: ya_IntLon_xya(1:jm,size(xya_data,3))
      !(out) 積分された 1 次元緯度(Y)格子点データの並び

      integer :: i

      ya_IntLon_xya = 0.0D0
      do i=0,im-1
         ya_IntLon_xya = ya_IntLon_xya + xya_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function ya_IntLon_xya

    function a_IntLat_ya(ya_data)
      !
      ! 1 次元緯度(Y)格子点データの Y 方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  1 次元緯度(Y)格子点データの並び(1:jm,*)

      real(8)             :: a_IntLat_ya(size(ya_data,2))
      !(out) 積分値の並び(*)

      integer :: j

      a_IntLat_ya = 0.0D0
      do j=1,jm
         a_IntLat_ya = a_IntLat_ya + ya_data(j,:) * y_Lat_Weight(j)
      enddo

    end function a_IntLat_ya

    function a_IntLon_xa(xa_data)          ! 経度積分
      !
      ! 1 次元経度(X)格子点データの X 方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 次元経度(X)格子点データの並び(0:im-1,*)
      real(8)             :: a_IntLon_xa(size(xa_data,2))
      !(out) 積分値の並び(*)
      integer :: i

      a_IntLon_xa = 0.0D0
      do i=0,im-1
         a_IntLon_xa = a_IntLon_xa + xa_data(i,:) * x_Lon_Weight(i)
      enddo

    end function a_IntLon_xa

  !--------------- 平均計算 -----------------
    function a_AvrLonLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8) :: a_AvrLonLat_xya(size(xya_data,3))
      !(out) 平均値の並び(*)

      a_AvrLonLat_xya = a_AvrLon_xa(xa_AvrLat_xya(xya_data))

    end function a_AvrLonLat_xya

    function xa_AvrLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: xa_AvrLat_xya(0:im-1,size(xya_data,3))
      !(out) 平均された 1 次元経度(X)格子点データの並び(im,*)

      xa_AvrLat_xya = xa_IntLat_xya(xya_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xya

    function ya_AvrLon_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: ya_AvrLon_xya(1:jm,size(xya_data,3))
      !(out) 平均された 1 次元緯度(Y)格子点の並び(1:jm,*)

      ya_AvrLon_xya = ya_IntLon_xya(xya_data)/sum(x_Lon_Weight)

    end function ya_AvrLon_xya

    function a_AvrLat_ya(ya_data)
      !
      ! 1 次元(Y)格子点データの緯度(Y)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: ya_data(:,:)
      !(in)  1 次元緯度格子点データの並び(1:jm,*)

      real(8)             :: a_AvrLat_ya(size(ya_data,2))
      !(out) 平均値の並び(*)

      a_AvrLat_ya = a_IntLat_ya(ya_data)/sum(y_Lat_Weight)

    end function a_AvrLat_ya

    function a_AvrLon_xa(xa_data)          ! 経度平均
      !
      ! 1 次元(X)格子点データの経度(X)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 次元経度(X)格子点データの並び(0:im-1,*)

      real(8)             :: a_AvrLon_xa(size(xa_data,2))
      !(out) 平均値の並び(*)

      a_AvrLon_xa = a_IntLon_xa(xa_data)/sum(x_Lon_Weight)

    end function a_AvrLon_xa

  end module wa_integral_module
