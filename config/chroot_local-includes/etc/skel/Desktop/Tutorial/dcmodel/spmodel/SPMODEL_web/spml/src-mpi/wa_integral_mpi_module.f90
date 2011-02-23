!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_integral_mpi_module
!
!  spml/wa_integral_mpi_module モジュールは球面上での流体運動を
!  球面調和函数と MPI 並列ライブラリを用いたスペクトル法によって
!  数値計算するための モジュール wa_mpi_module の下部モジュールであり, 
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
!      2008/06/06  竹広真一  MPI 並列化
!      2008/08/28  竹広真一  配列添え字変更
!      2010/01/07  佐々木洋平  RDoc 用のドキュメント修正, 
!                              include 'mpif.h' -> use mpi
!
module wa_integral_mpi_module
  !
  ! wa_integral_mpi_module
  !
  !  spml/wa_integral_mpi_module モジュールは球面上での流体運動を
  !  球面調和函数と MPI 並列ライブラリを用いたスペクトル法によって
  !  数値計算するための モジュール wa_mpi_module の下部モジュールであり, 
  !  積分・平均計算のための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_integral_mpi_module モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
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

  public a_IntLonLat_xva                      ! 緯度経度積分
  public va_IntLon_xva                        ! 経度積分    
  public xa_IntLat_xva, a_IntLat_va           ! 緯度積分    
  public a_AvrLonLat_xva                      ! 緯度経度平均
  public va_AvrLon_xva                        ! 経度平均    
  public xa_AvrLat_xva, a_AvrLat_va           ! 緯度平均    

  contains

  !--------------- 積分計算 -----------------
    function a_IntLonLat_xva(xva_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(多層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8) :: a_IntLonLat_xva(size(xva_data,3))
      !(out) 積分されたデータの並び(*)

      a_IntLonLat_xva = a_IntLon_xa(xa_IntLat_xva(xva_data))
    end function a_IntLonLat_xva

    function xa_IntLat_xva(xva_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8)             :: xa_IntLat_xva(0:im-1,size(xva_data,3))
      !(out) 積分された 1 次元経度(X)格子点データの並び

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
      ! 2 次元緯度経度格子点データの経度(X)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xva_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8)             :: va_IntLon_xva(jc,size(xva_data,3))
      !(out) 積分された 1 次元緯度(Y)格子点データの並び

      integer :: i

      va_IntLon_xva = 0
      do i=0,im-1
         va_IntLon_xva = va_IntLon_xva + xva_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function va_IntLon_xva

    function a_IntLat_va(va_data)
      !
      ! 1 次元緯度(Y)格子点データの Y 方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: va_data(:,:)
      !(in)  1 次元緯度(Y)格子点データの並び(jc,*)

      real(8)             :: a_IntLat_va(size(va_data,2))
      !(out) 積分値の並び(*)

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

  !--------------- 平均計算 -----------------
    function a_AvrLonLat_xva(xva_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8) :: a_AvrLonLat_xva(size(xva_data,3))
      !(out) 平均値の並び(*)

      a_AvrLonLat_xva = a_AvrLon_xa(xa_AvrLat_xva(xva_data))
    end function a_AvrLonLat_xva

    function xa_AvrLat_xva(xva_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8)             :: xa_AvrLat_xva(0:im-1,size(xva_data,3))
      !(out) 平均された 1 次元経度(X)格子点データの並び(0:im-1,*)

      xa_AvrLat_xva = xa_IntLat_xva(xva_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xva

    function va_AvrLon_xva(xva_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xva_data(:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,jc,*)

      real(8)             :: va_AvrLon_xva(jc,size(xva_data,3))
      !(out) 平均された 1 次元緯度(Y)格子点の並び(jc,*)

      va_AvrLon_xva = va_IntLon_xva(xva_data)/sum(x_Lon_Weight)

    end function va_AvrLon_xva

    function a_AvrLat_va(va_data)
      !
      ! 1 次元(Y)格子点データの緯度(Y)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: va_data(:,:)
      !(in)  1 次元緯度格子点データの並び(jc,*)

      real(8)             :: a_AvrLat_va(size(va_data,2))
      !(out) 平均値の並び(*)

      a_AvrLat_va = a_IntLat_va(va_data)/sum(y_Lat_Weight)

    end function a_AvrLat_va

end module wa_integral_mpi_module
