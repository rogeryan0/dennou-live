!--
!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_mpi_module
!
!   spml/w_mpi_module モジュールは球面上での 2 次元流体運動を
!   球面調和函数を用いたスペクトル法と MPI 並列化によって数値計算するための
!   Fortran90 関数を提供する. 
!
!   w_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
!   をそれぞれ担っている下部モジュール w_base_mpi_module,
!   w_deriv_mpi_module, w_module からなっている.
!
!   内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2008/05/26  竹広真一  w_module を MPI 化
!      2009/02/27  佐々木洋平 RDoc 用にドキュメントを微修正
!
!++
module w_mpi_module
  !
  != w_mpi_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_mpi_module.f90,v 1.3 2009-02-27 05:21:52 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法と MPI によって数値計算するための
  ! Fortran90 関数を提供する. 
  !
  ! w_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
  ! をそれぞれ担っている下部モジュール w_base_mpi_module,
  ! w_deriv_mpi_module, w_module からなっている.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (w_, nm_, n_, xy_, xv_, x_, y_, v_) は, 
  !   返す値の形を示している.
  !     w_ :: スペクトルデータ
  !    xy_ :: 2 次元格子点データ
  !    xv_ :: 2 次元分散格子点データ
  !    nm_ :: スペクトルデータの並んだ 3 次元配列(スペクトルデータの並びは
  !           全波数 n, 帯状波数 m で指定される 2 次元配列)
  !     n_ :: スペクトルデータの並んだ 2 次元配列 (スペクトルデータの並びは
  !           全波数 n で指定される 1 次元配列)
  !     x_ :: 経度方向 1 次元格子点データ
  !     y_ :: 緯度方向 1 次元格子点データ
  !     v_ :: 緯度方向 1 次元分散格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_w_w, _w, _xy, _x, _y) は, 入力変数の形スペクトルデータ
  !   および格子点データであることを示している.
  !     _w :: スペクトルデータ
  !   _w_w :: 2 つのスペクトルデータ
  !    _xy :: 2 次元格子点データ
  !    _xv :: 2 次元分散格子点データ
  !     _x :: 経度方向 1 次元格子点データ
  !     _y :: 緯度方向 1 次元格子点データ
  !     _v :: 緯度方向 1 次元分散格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * xy : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(im,jm). 
  !   * im, jm はそれぞれ経度, 緯度座標の格子点数であり, サブルーチン
  !     w_mpi_Initial にてあらかじめ設定しておく.
  !
  ! * xv : 2 次元分散格子点データ.
  !   * 変数の種類と次元は real(8), dimension(im,jc). 
  !   * im, jc はそれぞれ経度の格子点数および
  !     プロセスで保有する緯度方向の格子点数である.  
  !     jc はサブルーチン w_mpi_Initial にて設定して
  !     おくことにより設定される public 変数である. 
  !
  ! * w : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1)). 
  !     nm は球面調和函数の最大全波数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方は
  !     関数 l_nm, nm_l によって調べることができる.
  !
  ! * nm : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm). 
  !     第 1 次元が水平全波数,  第 2 次元が帯状波数を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * n : スペクトルデータの並んだ 1 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン w_Initial にてあらかじめ設定しておく.
  !
  ! * x, y : 経度, 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(im) 
  !     および real(8), dimension(jm).
  !
  ! * w_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xy_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * xv_ で始まる関数が返す値は 2 次元分散格子点データに同じ.
  !
  ! * x_, y_, v_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! w_mpi_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, x_Lon_Weight,  ::  格子点座標(経度)と重みを格納した 1 次元配列
  ! y_Lat, y_Lat_Weight   ::  格子点座標(緯度)と重みを格納した 1 次元配列
  ! v_Lat, v_Lat_Weight   ::  分散格子点座標(緯度)と重みを格納した 1 次元配列
  ! xy_Lon, xy_Lat        ::  格子点データの経度・緯度座標(X,Y)
  !                           (格子点データ型 2 次元配列)
  ! xv_Lon, xv_Lat        ::  分散格子点データの経度・緯度座標(X,Y)
  !                           (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! xy_w :: スペクトルデータから格子データへの変換
  ! w_xy :: 格子データからスペクトルデータへの変換
  ! xv_w :: スペクトルデータから分散格子データへの変換
  ! w_xv :: 分散格子データからスペクトルデータへの変換
  ! l_nm, nm_l :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! w_Lapla_w       :: スペクトルデータにラプラシアンを作用させる
  ! rn              :: スペクトルデータのラプラシアンを計算するための係数. 
  ! irm             :: 経度微分演算用配列
  ! w_LaplaInv_w    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! w_DLon_w        :: スペクトルデータに経度微分∂/∂λを作用させる
  ! xy_GradLon_w    :: スペクトルデータに勾配型経度微分 
  !                    1/cosφ・∂/∂λを作用させる
  ! xy_GradLat_w    :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! w_DivLon_xy     :: 格子データに発散型経度微分 
  !                    1/cosφ・∂/∂λを作用させる
  ! w_DivLat_xy     :: 格子データに発散型緯度微分
  !                    1/cosφ・∂(g cosφ)/∂φを作用させる
  ! w_Div_xy_xy     :: ベクトル成分である 2 つの格子データに発散を作用させる
  ! w_Jacobian_w_w  :: 2 つのスペクトルデータからヤコビアンを計算する
  ! xv_GradLon_w    :: スペクトルデータに勾配型経度微分
  !                    1/cosφ・∂/∂λを作用させる
  ! xv_GradLat_w    :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! w_DivLon_xv     :: 格子データに発散型経度微分 1/cosφ・∂/∂λを作用させる
  ! w_DivLat_xv     :: 格子データに発散型緯度微分
  !                    1/cosφ・∂(g cosφ)/∂φを作用させる
  ! w_Div_xv_xv     :: ベクトル成分である 2 つの格子データに発散を作用させる
  ! w_JacobianMPI_w_w  :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! xy_GradLambda_w :: スペクトルデータに勾配型経度微分∂/∂λを作用させる
  ! xy_GradMu_w     :: スペクトルデータに勾配型緯度微分
  !                    (1-μ^2)∂/∂μを作用させる
  ! w_DivLambda_xy  :: 格子データに発散型経度微分
  !                    1/(1-μ^2)・∂/∂λを作用させる
  ! w_DivMu_xy      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  ! xv_GradLambda_w :: スペクトルデータに勾配型経度微分∂/∂λを作用させる
  ! xv_GradMu_w     :: スペクトルデータに勾配型緯度微分
  !                    (1-μ^2)∂/∂μを作用させる
  ! w_DivLambda_xv  :: 格子データに発散型経度微分
  !                    1/(1-μ^2)・∂/∂λを作用させる
  ! w_DivMu_xv      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  !==== 補間
  !
  ! Interpolate_w :: スペクトルデータから任意の点での値を求める. 
  !
  !==== 積分・平均
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの全領域積分および平均
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 次元格子点データの経度方向積分および平均
  ! IntLon_x, AvrLon_x         :: 1 次元(X)格子点データの経度方向積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 次元格子点データの緯度方向積分および平均
  ! IntLat_y, AvrLat_y         :: 1 次元(Y)格子点データの緯度方向積分および平均
  !
  !==== スペクトル解析
  !
  ! nm_EnergyFromStreamfunc_w  :: 流線関数からエネルギースペクトルを計算する
  !                               (水平全波数 n, 帯状波数 m 空間)
  ! n_EnergyFromStreamfunc_w   :: 流線関数からエネルギースペクトルを計算する
  !                               (水平全波数 n 空間) 
  ! nm_EnstrophyFromStreamfunc_w  :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n, 帯状波数 m 空間)
  ! n_EnstrophyFromStreamfunc_w   :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n 空間)
  ! w_spectrum_VMiss              ::  欠損値
  !
  !
  use w_module
  use w_base_mpi_module
  use w_deriv_mpi_module
  use w_integral_mpi_module

  private

  public x_Lon, y_Lat                         ! 緯度格子座標・重み
  public x_Lon_Weight, y_Lat_Weight           ! 経度格子座標・重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public l_nm, nm_l                           ! 波数格納位置
  public xy_w, w_xy                           ! 変換関数

  public rn, irm                              ! ラプラシアン/経度微分演算用配列
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public Interpolate_w                        ! 補間関数

  public IntLonLat_xy                         ! 緯度経度積分
  public y_IntLon_xy, IntLon_x                ! 経度積分    
  public x_IntLat_xy, IntLat_y                ! 緯度積分    
  public AvrLonLat_xy                         ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x                ! 経度平均    
  public x_AvrLat_xy, AvrLat_y                ! 緯度平均    

  public IntLonLat_xv                         ! 緯度経度積分
  public v_IntLon_xv                          ! 経度積分    
  public x_IntLat_xv, IntLat_v                ! 緯度積分    
  public AvrLonLat_xv                         ! 緯度経度平均
  public v_AvrLon_xv                          ! 経度平均    
  public x_AvrLat_xv, AvrLat_v                ! 緯度平均    

  public nm_EnergyFromStreamfunc_w            ! エネルギースペクトル           
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnergyFromStreamfunc_w             ! エネルギースペクトル
                                              ! (水平全波数 n 空間) 
  public nm_EnstrophyFromStreamfunc_w         ! エンストロフィースペクトル     
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnstrophyFromStreamfunc_w          ! エンストロフィースペクトル  
                                              !  (水平全波数 n 空間)
  public w_spectrum_VMiss                     ! 欠損値

  public w_mpi_Initial                        ! 初期化
  public jc                                   ! 緯度方向分散格子点数

  public v_Lat, v_Lat_Weight                  ! 経度分散格子座標・重み
  public xv_Lon, xv_Lat                       ! 分散格子座標(im,jc)
  public xv_w, w_xv                           ! 変換関数

  public xv_GradLon_w, xv_GradLat_w           ! 勾配型微分
  public w_DivLon_xv, w_DivLat_xv             ! 発散型微分
  public w_Div_xv_xv                          ! 発散型微分
  public w_JacobianMPI_w_w                    ! ヤコビアン
  public xv_GradLambda_w, xv_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xv, w_DivMu_xv           ! 発散型微分(λ,μ座標)

contains

    subroutine w_mpi_initial(n_in,i_in,j_in)
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西)
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断波数の設定

      integer iw

      call w_initial(n_in,i_in,j_in)
      call w_base_mpi_initial
      call w_deriv_mpi_initial

    end subroutine w_mpi_initial

end module w_mpi_module
