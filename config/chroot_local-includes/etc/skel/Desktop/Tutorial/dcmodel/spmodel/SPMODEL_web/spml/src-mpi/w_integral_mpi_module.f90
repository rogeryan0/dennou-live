!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_integral_mpi_module
!
!  spml/w_integral_mpi_module モジュールは球面上での 2 次元流体運動を
!  球面調和函数と MPI 並列ライブラリを用いたスペクトル法によってn
!  数値計算するための モジュール w_mpi_module の下部モジュールであり, 
!  積分・平均計算のための Fortran90 関数を提供する. 
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2002/05/25  竹広真一 
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2008/06/06  竹広真一  MPI 並列化
!      2010/01/07  佐々木洋平  RDoc 用のドキュメント修正, 
!                              include 'mpif.h' -> use mpi
!
module w_integral_mpi_module
  !
  ! w_integral_mpi_module
  !
  !  spml/w_integral_mpi_module モジュールは球面上での 2 次元流体運動を
  !  球面調和函数と MPI 並列ライブラリを用いたスペクトル法によってn
  !  数値計算するための モジュール w_mpi_module の下部モジュールであり, 
  !  積分・平均計算のための Fortran90 関数を提供する. 
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use w_base_module, only : im, x_Lon_Weight, y_Lat_Weight
  use w_base_mpi_module, only : jc, v_Lat_Weight
  use w_integral_module, only : IntLon_x, AvrLon_x
  use mpi

  implicit none
  integer :: ierr

  private
  private im
 
  public IntLonLat_xv                      ! 緯度経度積分
  public v_IntLon_xv                       ! 経度積分    
  public x_IntLat_xv, IntLat_v             ! 緯度積分    
  public AvrLonLat_xv                      ! 緯度経度平均
  public v_AvrLon_xv                       ! 経度平均    
  public x_AvrLat_xv, AvrLat_v             ! 緯度平均    

  contains

  !--------------- 積分計算 -----------------
    function IntLonLat_xv(xv_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(1 層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, v_V_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xv_data(0:im-1,jc)         
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8) :: IntLonLat_xv                         
      !(out) 積分値

      IntLonLat_xv = IntLon_x(x_IntLat_xv(xv_data))
    end function IntLonLat_xv

    function x_IntLat_xv(xv_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)           
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8)             :: x_IntLat_xv(0:im-1) 
      !(out) 積分された 1 次元経度(X)格子点データ

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
      ! 2 次元緯度経度格子点データの経度(X)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8)             :: v_IntLon_xv(jc)
      !(out) 積分された 1 次元緯度(Y)格子点データ

      integer :: i

      v_IntLon_xv = 0
      do i=0,im-1
         v_IntLon_xv = v_IntLon_xv + xv_data(i,:) * x_Lon_weight(i)
      enddo

    end function v_IntLon_xv

    function IntLat_v(v_data)
      !
      ! 1 次元緯度(Y)格子点データの Y 方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: v_data(jc)      !(in)  1 次元緯度(Y)格子点データ
      real(8)             :: IntLat_v        !(out) 積分値

      real(8)             :: IntLatTmp

      IntLat_v = sum(v_data * v_Lat_weight)
      IntLatTmp=IntLat_v
      CALL MPI_ALLREDUCE(IntLatTMP,IntLat_v,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLat_v

  !--------------- 平均計算 -----------------
    function AvrLonLat_xv(xv_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, v_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*v_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xv_data(0:im-1,jc)
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8) :: AvrLonLat_xv
      !(out) 平均値

      AvrLonLat_xv = AvrLon_x(x_AvrLat_xv(xv_data))
    end function AvrLonLat_xv

    function x_AvrLat_xv(xv_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に v_Lat_Weight をかけた総和を計算し,
      ! y_Lat_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)          ! 格子点(0:im-1,jc)
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8)             :: x_AvrLat_xv(im)
      !(out) 平均された 1 次元経度(X)格子点データ

      x_AvrLat_xv = x_IntLat_xv(xv_data)/sum(y_Lat_weight)

    end function x_AvrLat_xv

    function v_AvrLon_xv(xv_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xv_data(0:im-1,jc)
      !(in) 2 次元経度緯度格子点データ(0:im-1,jc)

      real(8)             :: v_AvrLon_xv(jc)
      !(out) 平均された 1 次元緯度(Y)格子点

      v_AvrLon_xv = v_IntLon_xv(xv_data)/sum(x_Lon_weight)

    end function v_AvrLon_xv

    function AvrLat_v(v_data)
      !
      ! 1 次元(Y)格子点データの緯度(Y)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた総和を計算し, 
      ! v_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: v_data(jc)          !(in)  1 次元緯度格子点データ
      real(8)             :: AvrLat_v            !(out) 平均値

      AvrLat_v = IntLat_v(v_data)/sum(y_Lat_weight)

    end function AvrLat_v

  end module w_integral_mpi_module
