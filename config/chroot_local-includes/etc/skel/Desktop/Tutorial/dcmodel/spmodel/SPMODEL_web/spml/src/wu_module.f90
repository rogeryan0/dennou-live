!--
!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wu_module
!
!    spml/wu_module モジュールは球内での流体運動をスペクトル法によって
!    数値計算するための Fortran90 関数を提供するものである. 
!
!    水平方向に球面調和函数変換および動径方向にチェビシェフ変換を用いる
!    場合のスペクトル計算のためのさまざまな関数を提供する. 
!
!    内部で wa_module, au_module を用いている. 最下部では球面調和変換
!    およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
!    サブルーチンを用いている.
!
!
!履歴  2008/01/01  竹広真一  wt_module より改変
!      2008/01/05  竹広真一  wr_Drad2_wu, r_Drad2_u 追加
!      2008/01/10  竹広真一  wu_TormagBoundaryGrid バグfix
!      2008/01/13  竹広真一  wa_initial スイッチ導入
!      2008/07/01  佐々木洋平 水平方向の格子点を 0:im-1,1:jm に修正
!      2008/07/04  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!
!凡例
!      データ種類と index
!        x : 経度         y : 緯度        r : 動径
!        w : 球面調和関数スペクトル
!        n : 球面調和関数スペクトル(水平全波数)
!        m : 球面調和関数スペクトル(帯状波数)
!        u : チェビシェフ関数スペクトル
!        a : 任意の次元
!
!        xyr : 3 次元格子点データ
!        xy  : 水平 2 次元格子点データ
!        yr  : 子午面 2 次元格子点データ
!        xr  : 緯度面 2 次元格子点データ
!
!        wr  : 水平スペクトル動径格子点データ
!        wu  : スペクトルデータ
!
!++
module wu_module
  !
  != wu_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wu_module.f90,v 1.6 2009-02-28 21:33:48 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wu_module モジュールは球内での流体運動をスペクトル法によって
  ! 数値計算するための Fortran90 関数を提供するものである. 
  !
  ! 水平方向に球面調和函数変換および動径方向にチェビシェフ変換を用いる
  ! 場合のスペクトル計算のためのさまざまな関数を提供する. 
  !
  ! 内部で wa_module, au_module を用いている. 最下部では球面調和変換
  ! およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
  ! サブルーチンを用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (wu_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_) は, 
  !   返す値の形を示している.
  !   wu_  :: スペクトルデータ(球面調和函数・チェビシェフ変換)
  !   nmr_ :: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径)
  !   nr_  :: 水平スペクトルデータ(全波数 n, 動径)
  !   xyr_ :: 3 次元格子点データ(経度・緯度・動径)
  !   wr_  :: 水平スペクトル, 動径格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後 (wu_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) は, 入力変数の
  !   形がスペクトルデータおよび格子点データであることを示している.
  !   _wu      :: スペクトルデータ
  !   _xyr     :: 3 次元格子点データ
  !   _xyr_xyr :: 2 つの3 次元格子点データ, ...
  !
  !=== 各データの種類の説明
  !
  ! * xyr : 3 次元格子点データ(経度・緯度・動径)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  !
  ! * wu : スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm は球面調和函数の最大全波数, lm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン wt_Initial にてあらかじめ設定しておく. 
  !   * 水平スペクトルデータの格納のされ方は関数 l_nm, nm_l によって調べる
  !     ことができる.
  !
  ! * nmr : 水平スペクトルデータの並んだ 3 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数, 第 3 次元が動径座標を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wt_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * nr : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,0:km). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  !
  ! * wr : 水平スペクトル, 動径格子点データ.
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:km).
  !
  ! * wu_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xyr_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * wr_ で始まる関数が返す値は水平スペクトル, 動径格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  ! 
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! wu_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, y_Lat, r_Rad          :: 格子点座標(緯度, 経度, 動径座標)を
  !                                 格納した1 次元配列
  ! x_Lon_Weight, y_Lat_Weight, r_Rad_Weight :: 重み座標を格納した 1 次元配列
  ! xyr_Lon, xyr_Lat, xyr_Rad    :: 格子点データの経度・緯度・動径座標(X,Y,Z)
  !                                 (格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! xyr_wu, wu_xyr :: スペクトルデータと 3 次元格子データの
  !                   間の変換 (球面調和函数, チェビシェフ変換)
  ! xyr_wr, wr_xyr :: 3 次元格子データと水平スペクトル・動径格子データとの
  !                   間の変換 (球面調和函数)
  ! wr_wu, wu_wr   :: スペクトルデータと水平スペクトル・動径格子データとの
  !                   間の変換 (チェビシェフ変換)
  ! w_xy, xy_w     :: スペクトルデータと 2 次元水平格子データの
  !                   間の変換(球面調和函数変換) 
  ! l_nm, nm_l     :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wr_DRad_wu          :: スペクトルデータに動径微分
  !                        ∂/∂r を作用させる
  ! wr_DRad2_wu         :: スペクトルデータに動径微分
  !                        ∂^2/∂r^2 を作用させる
  ! wr_DivRad_wu        :: スペクトルデータに発散型動径微分
  !                        1/r^2 ∂/∂r r^2 = ∂/∂r + 2/r を作用させる
  ! wr_RotRad_wu        :: スペクトルデータに回転型動径微分
  !                        1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  ! wr_Lapla_wu         :: スペクトルデータにラプラシアンを作用させる
  ! xyr_GradLon_wu      :: スペクトルデータに勾配型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! xyr_GradLat_wu      :: スペクトルデータに勾配型緯度微分
  !                        1/r・∂/∂φを作用させる
  ! wr_DivLon_xyr       :: 格子データに発散型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! wr_DivLat_xyr       :: 格子データに発散型緯度微分
  !                        1/rcosφ・∂(g cosφ)/∂φを作用させる
  ! wr_Div_xyr_xyr_xyr  :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyr_Div_xyr_xyr_xyr :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyr_RotLon_wu_wu    :: ベクトル場の回転の経度成分を計算する
  ! xyr_RotLat_wu_wu    :: ベクトル場の回転の緯度成分を計算する
  ! wr_RotRad_xyr_xyr   :: ベクトル場の回転の動径成分を計算する
  !
  !==== トロイダルポロイダル計算用微分
  !
  ! wu_KxRGrad_wu            :: スペクトルデータに経度微分
  !                             k×r・▽ = ∂/∂λを作用させる
  ! xyr_KGrad_wu             :: スペクトルデータに軸方向微分
  !                             k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を
  !                             作用させる
  ! wu_L2_wu                 :: スペクトルデータに
  !                             L2 演算子 = -水平ラプラシアンを作用させる
  ! wu_L2Inv_wu              :: スペクトルデータに
  !                             L2 演算子の逆 = -逆水平ラプラシアンを
  !                             作用させる
  ! wu_QOperator_wu          :: スペクトルデータに演算子
  !                             Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) を
  !                             作用させる
  ! wu_RadRot_xyr_xyr        :: ベクトル v の渦度と動径ベクトル r の内積
  !                             r・(▽×v) を計算する
  ! wr_RadRotRot_xyr_xyr_xyr :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wu_Potential2Vector      :: トロイダルポロイダルポテンシャルから
  !                             ベクトル場を計算する
  ! wu_Potential2Rotation    :: トロイダルポロイダルポテンシャルで表される
  !                             非発散ベクトル場の回転の各成分を計算する
  !
  !==== ポロイダル/トロイダルモデル用スペクトル解析
  !
  ! nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu   :: 
  !     トロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  ! nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu   :: 
  !     ポロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  !
  !==== 境界値問題
  !
  ! wt_BoundariesTau, wt_BoundariesGrid, wt_Boundaries                   ::
  !     タウ法, 選点法
  ! wt_TorBoundariesTau, wt_TorBoundariesGrid, wt_TorBoundaries          :: 
  !     速度トロイダルポテンシャルの境界条件を球殻領域に適用する
  !     (タウ法,選点法)
  ! wz_LaplaPol2Pol_wz, wt_LaplaPol2Pol_wt                               :: 
  !     球殻領域の速度ポロイダルポテンシャルΦを▽^2Φから求める
  !     (入出力がそれぞれチェビシェフ格子点,チェビシェフ係数)
  ! wt_TorMagBoundariesTau, wt_TorMagBoundariesGrid, wt_TorMagBoundaries ::
  !     磁場トロイダルポテンシャルの境界条件を球殻領域に適用する
  !     (タウ法, 選点法)
  ! wt_PolMagBoundariesTau, wt_PolMagBoundariesGrid, wt_PolMagBoundaries ::
  !     磁場トロイダルポテンシャル境界の境界条件を球殻領域に適用する
  !     (タウ法, 選点法)
  ! wu_BoundaryTau, wu_BoundaryGrid, wu_Boundary                         ::
  !     球領域にディリクレ, ノイマン境界条件を適用する (タウ法, 選点法)
  ! wu_TorBoundaryTau, wu_TorBoundaryGrid, wu_TorBoundary                ::
  !     球領域の速度トロイダルポテンシャルの境界条件を適用する(タウ法,選点法)
  ! wr_LaplaPol2Pol_wr, wu_LaplaPol2Pol_wu                               ::
  !     球領域の速度ポロイダルポテンシャルΦを▽^2Φから求める
  !     (入出力がそれぞれチェビシェフ格子点,チェビシェフ係数)
  ! wu_TorMagBoundaryTau, wu_TorMagBoundaryGrid, wu_TorMagBoundary       ::
  !    磁場トロイダルポテンシャルの境界条件を球領域に適用する(タウ法, 選点法)
  ! wu_PolMagBoundaryTau, wu_PolMagBoundaryGrid, wu_PolMagBoundary       ::
  !     磁場トロイダルポテンシャル境界の境界条件を球領域に適用する
  !     (タウ法, 選点法)
  !
  !==== 積分・平均(3 次元データ)
  !
  ! IntLonLatRad_xyz, AvrLonLatRad_xyz :: 3 次元格子点データの
  !                                       全領域積分および平均
  ! z_IntLonLat_xyz, z_AvrLonLat_xyz   :: 3 次元格子点データの
  !                                       緯度経度(水平・球面)積分および平均
  ! y_IntLonRad_xyz, y_AvrLonRad_xyz   :: 3 次元格子点データの
  !                                       緯度動径積分および平均
  ! z_IntLatRad_xyz, z_AvrLatRad_xyz   :: 3 次元格子点データの
  !                                       経度動径(子午面)積分および平均
  ! yz_IntLon_xyz, yz_AvrLon_xyz       :: 3 次元格子点データの
  !                                       経度方向積分および平均
  ! xz_IntLat_xyz, xz_AvrLat_xyz       :: 3 次元格子点データの
  !                                       緯度方向積分および平均
  ! xz_IntRad_xyz, xz_AvrRad_xyz       :: 3 次元格子点データの
  !                                       動径方向積分および平均
  ! IntLonLatRad_xyr, AvrLonLatRad_xyr :: 3 次元格子点データの
  !                                       全領域積分および平均
  ! z_IntLonLat_xyr, z_AvrLonLat_xyr   :: 3 次元格子点データの
  !                                       緯度経度(水平・球面)積分および平均
  ! y_IntLonRad_xyr, y_AvrLonRad_xyr   :: 3 次元格子点データの
  !                                       緯度動径積分および平均
  ! z_IntLatRad_xyr, z_AvrLatRad_xyr   :: 3 次元格子点データの
  !                                       経度動径(子午面)積分および平均
  ! yz_IntLon_xyr, yz_AvrLon_xyr       :: 3 次元格子点データの
  !                                       経度方向積分および平均
  ! xz_IntLat_xyr, xz_AvrLat_xyr       :: 3 次元格子点データの
  !                                       緯度方向積分および平均
  ! xz_IntRad_xyr, xz_AvrRad_xyr       :: 3 次元格子点データの
  !                                       動径方向積分および平均
  !
  !==== 積分・平均(2 次元データ)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの水平(球面)積分および平均
  ! IntLonRad_xr, AvrLonRad_xr :: 2 次元(XZ)格子点データの経度動径積分
  !                               および平均
  ! IntLatRad_yr, AvrLatRad_yr :: 2 次元(YZ)格子点データの緯度動径(子午面)
  !                               積分および平均 
  ! y_IntLon_xy, y_AvrLon_xy   :: 水平 2 次元(球面)格子点データの経度方向
  !                               積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 水平2 次元(球面)格子点データの緯度方向積分
  !                               および平均
  ! r_IntLon_xr, r_AvrLon_xr   :: 2 次元(XZ)格子点データの経度方向積分および
  !                               平均
  ! x_IntRad_xr, x_AvrRad_xr   :: 2 次元(XZ)格子点データの動径方向積分および
  !                               平均
  ! r_IntLat_yr, r_AvrLat_yr   :: 2 次元(YZ)格子点データの緯度方向積分および
  !                               平均
  ! y_IntRad_yr, y_AvrRad_yr   :: 2 次元(YZ)格子点データの動径方向積分および
  !                               平均                  
  !
  !==== 積分・平均(1 次元データ)
  !
  ! IntLon_x, AvrLon_x  :: 1 次元(X)格子点データの経度方向積分および平均
  ! IntLat_y, AvrLat_y  :: 1 次元(Y)格子点データの緯度方向積分および平均
  ! IntRad_r, AvrRad_r  :: 1 次元(Z)格子点データの動径方向積分および平均
  !
  ! 
  use dc_message
  use lumatrix
  use wa_module
  use au_module, r_Rad => g_R, r_RAD_WEIGHT => g_R_WEIGHT, &
                 wu_wr => au_ag, wr_wu => ag_au, &
                 r_DRad_t => g_Dr_u, wr_DRad_wu => ag_Dr_au, &
                 r_DRad2_t => g_Dr2_u, wr_DRad2_wu => ag_Dr2_au
  implicit none
  private

  public wu_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public r_Rad, r_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wu_VMiss

  public w_xy, xy_w
  public wr_DRad_wu, r_DRad_t, wr_wu, wu_wr
  public wr_DRad2_wu, r_DRad2_t
  public xyr_wu, wu_xyr, xyr_wr, wr_xyr
  public wr_DivRad_wu, wr_RotRad_wu, wr_Lapla_wu
  public xyr_GradLon_wu, xyr_gradlat_wu
  public wr_DivLon_xyr, wr_DivLat_xyr
  public wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr
  public xyr_RotLon_wu_wu, xyr_RotLat_wu_wu, wr_RotRad_xyr_xyr

  public yr_IntLon_xyr, xr_IntLat_xyr, xy_IntRad_xyr
  public x_IntLatRad_xyr, y_IntLonRad_xyr, r_IntLonLat_xyr
  public IntLonLatRad_xyr

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public r_IntLat_yr, y_IntRad_yr, IntLatRad_yr
  public r_IntLon_xr, x_IntRad_xr, IntLonRad_xr
  public IntLon_x, IntLat_y, IntRad_r

  public yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr
  public x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr
  public AvrLonLatRad_xyr

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr
  public r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr
  public AvrLon_x, AvrLat_y, AvrRad_r

  public wu_KxRGrad_wu, xyr_KGrad_wu, wu_L2_wu, wu_L2Inv_wu, wu_QOperator_wu
  public wu_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr
  public wu_Potential2vector, wu_Potential2Rotation

  public nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu
  public nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu

  public wu_Boundary, wu_TorBoundary, wr_LaplaPol2Pol_wr
  public wu_TormagBoundary, wu_PolmagBoundary

  public wu_BoundaryTau, wu_TorBoundaryTau
  public wu_TormagBoundaryTau, wu_PolmagBoundaryTau

  public wu_BoundaryGrid, wu_TorBoundaryGrid, wu_LaplaPol2PolGrid_wu
  public wu_TormagBoundaryGrid, wu_PolmagBoundaryGrid

  interface wu_Boundary
     module procedure wu_BoundaryTau
  end interface

  interface wu_TorBoundary
     module procedure wu_TorBoundaryTau
  end interface

  interface wu_TorMagBoundary
     module procedure wu_TorMagBoundaryTau
  end interface

  interface wu_PolMagBoundary
     module procedure wu_PolMagBoundaryTau
  end interface

  integer            :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer            :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  real(8)            :: ra=1.0               ! 球半径
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xyr_LON, xyr_LAT, xyr_RAD ! 座標
  real(8), dimension(:,:), allocatable   :: wr_RAD                    ! 座標
  integer, dimension(:), allocatable     :: nd             ! 重み r^n の指数

  real(8) :: wu_VMiss = -999.0        ! 欠損値

  save im, jm, km, nm, lm, ra, nd

  contains
  !--------------- 初期化 -----------------
   subroutine wu_Initial(i,j,k,n,l,r,np,wa_init)
     !
     ! スペクトル変換の格子点数, 波数, 動径座標の範囲を設定する.
     !
     ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
     ! しなければならない. 
     !
     ! np に 1 より大きな値を指定すれば ISPACK の球面調和函数変換 
     ! OPENMP 並列計算ルーチンが用いられる. 並列計算を実行するには, 
     ! 実行時に環境変数 OMP_NUM_THREADS を np 以下の数字に設定する等の
     ! システムに応じた準備が必要となる. 
     !
     ! np に 1 より大きな値を指定しなければ並列計算ルーチンは呼ばれない.
     !
     !
     integer,intent(in) :: i              ! 格子点数(経度λ)
     integer,intent(in) :: j              ! 格子点数(緯度φ)
     integer,intent(in) :: k              ! 格子点数(動径 r)
     integer,intent(in) :: n              ! 切断波数(水平全波数)
     integer,intent(in) :: l              ! 切断波数(動径波数)

     real(8),intent(in) :: r              ! 球半径

     integer,intent(in), optional :: np   ! OPENMP での最大スレッド数
     logical,intent(in), optional :: wa_init   ! wa_initial スイッチ

     logical    :: wa_initialize=.true.   ! wa_initial スイッチ

     integer :: nn, mm

     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ra = r

     if ( present(wa_init) ) then
        wa_initialize = wa_init
     else
        wa_initialize = .true.
     endif

     if ( wa_initialize ) then
        if ( present(np) ) then
           call wa_Initial(nm,im,jm,km+1,np)
        else
           call wa_Initial(nm,im,jm,km+1)
        endif
     endif

     allocate(nd((nm+1)*(nm+1)))

     do nn=0,nm
        do mm=-nn,nn
!!$           nd(l_nm(nn,mm)) = nn                 ! 水平 n 次は r^n から
           nd(l_nm(nn,mm)) = (1-(-1)**nn)/2     ! 奇数次 r, 偶数次 r^0
        enddo
     enddo

     call au_Initial(km,lm,ra,nd)

     allocate(xyr_Lon(0:im-1,1:jm,0:km))
     allocate(xyr_Lat(0:im-1,1:jm,0:km))
     allocate(xyr_Rad(0:im-1,1:jm,0:km))

     allocate(wr_Rad((nm+1)*(nm+1),0:km))

     xyr_Lon = spread(xy_Lon,3,km+1)
     xyr_Lat = spread(xy_Lat,3,km+1)
     xyr_Rad = spread(spread(r_Rad,1,jm),1,im)

     wr_Rad = spread(r_Rad,1,(nm+1)*(nm+1))

     r_Rad_Weight = r_Rad_Weight * r_Rad**2       ! r^2 dr の積分重み

   end subroutine wu_initial

  !--------------- 基本変換 -----------------

    function xyr_wu(wu)
      !
      ! スペクトルデータから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyr_wu
      !(out) 3 次元経度緯度動径格子点データ

      xyr_wu = xya_wa(wr_wu(wu))

    end function xyr_wu

    function wu_xyr(xyr)
      !
      ! 3 次元格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_xyr
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ

      wu_xyr = wu_wr(wa_xya(xyr))

    end function wu_xyr

    function xyr_wr(wr)
      !
      ! 水平スペクトル・動径格子点データから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wr
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyr_wr
      !(out) 3 次元経度緯度動径格子点データ

      xyr_wr = xya_wa(wr)

    end function xyr_wr

    function wr_xyr(xyr)
      !
      ! 3 次元格子データから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_xyr
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ

      wr_xyr = wa_xya(xyr)

    end function wr_xyr

  !--------------- 微分計算 -----------------
    function wr_DivRad_wu(wu)
      ! 
      ! 入力スペクトルデータに発散型動径微分
      !
      !       1/r^2 ∂/∂r (r^2 .)= ∂/∂r + 2/r
      !
      ! を作用する.
      !
      ! スペクトルデータの発散型動径微分とは, 対応する格子点データに
      ! 発散型動径微分を作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_DivRad_wu
      !(out) 発散型動径微分を作用された水平スペクトル動径格子点データ

!!$      wr_DivRad_wu = wr_DRad_wu(wu) + 2/wr_Rad*wr_wu(wu)
      wr_DivRad_wu = wr_Drad_wu(wu_wr(wr_Rad**2*wr_wu(wu)))/wr_Rad**2

    end function wr_DivRad_wu

    function wr_RotRad_wu(wu)
      !
      ! 入力スペクトルデータに回転型動径微分
      !
      !      1/r ∂(r.)/∂r = ∂(.)/∂r + (.)/r
      !
      ! を作用する.
      !
      ! スペクトルデータの回転型動径微分とは, 対応する格子点データに
      ! 回転型動径微分を作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_RotRad_wu
      !(out) 回転型動径微分を作用された水平スペクトル動径格子点データ

      wr_RotRad_wu = wr_Drad_wu(wu) + wr_wu(wu)/wr_Rad
!!$      wr_RotRad_wu = wr_Drad_wu(wu_wr(wr_Rad*wr_wu(wu)))/wr_Rad

    end function wr_RotRad_wu

    function wr_RotRad_wr(wr)
      !
      ! 入力スペクトルデータに回転型動径微分
      !
      !      1/r ∂(r.)/∂r = ∂(.)/∂r + (.)/r
      !
      ! を作用する.
      !
      ! スペクトルデータの回転型動径微分とは, 対応する格子点データに
      ! 回転型動径微分を作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wr
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_RotRad_wr
      !(out) 回転型動径微分を作用された水平スペクトル動径格子点データ

!!$      wr_RotRad_wu = wr_Drad_wu(wu) + wr_wu(wu)/wr_Rad
      wr_RotRad_wr = wr_Drad_wu(wu_wr(wr_Rad*wr))/wr_Rad

    end function wr_RotRad_wr

    function wr_Lapla_wu(wu)
      ! 入力スペクトルデータにラプラシアン
      !
      !     ▽^2 =   1/r^2 cos^2φ・∂^2/∂λ^2 
      !            + 1/r^2 cosφ・∂/∂φ(cosφ∂/∂φ) 
      !            + 1/r^2 ∂/∂r (r^2 ∂/∂r) 
      !              (∂^2/∂r^2 + 2/r∂/∂r)
      !
      ! を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_Lapla_wu
      !(out) ラプラシアンを作用された水平スペクトル動径格子点データ

!!$      wr_Lapla_wu = wr_DivRad_wu(wu_wr(wr_Drad_wu(wu))) &
!!$                   + wr_wu(wa_Lapla_wa(wu))/wr_Rad**2
      wr_Lapla_wu = wr_DRad2_wu(wu) + 2 * wr_Drad_wu(wu)/wr_Rad & 
                   + wr_wu(wa_Lapla_wa(wu))/wr_Rad**2

    end function wr_Lapla_wu

    function xyr_GradLon_wu(wu)
      !
      ! スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λ
      ! を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_GradLon_wu
      !(out) 勾配型経度微分を作用された 2 次元スペクトルデータ

      xyr_GradLon_wu = xya_GradLon_wa(wr_wu(wu))/xyr_Rad

    end function xyr_GradLon_wu

    function xyr_GradLat_wu(wu) 
      !
      ! スペクトルデータに勾配型経度微分 1/r ∂/∂φ を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_GradLat_wu
      !(out) 勾配型緯度微分を作用された 2 次元スペクトルデータ

      xyr_GradLat_wu = xya_GradLat_wa(wr_wu(wu))/xyr_Rad

    end function xyr_GradLat_wu

    function wr_DivLon_xyr(xyr)
      ! 
      ! 格子点データに発散型経度微分 1/rcosφ・∂/∂λ を作用させた
      ! スペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:km)       :: wr_DivLon_xyr
      !(out) 発散型経度微分を作用された水平スペクトル動径格子点データ

      wr_DivLon_xyr = wa_DivLon_xya(xyr/xyr_Rad)

    end function wr_DivLon_xyr

    function wr_DivLat_xyr(xyr)
      !
      ! 格子データに発散型緯度微分 1/rcosφ・∂(f cosφ)/∂φ を
      ! 作用させたスペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:km)       :: wr_DivLat_xyr
      !(out) 発散型緯度微分を作用された水平スペクトル動径格子点データ

      wr_DivLat_xyr = wa_DivLat_xya(xyr/xyr_Rad)

    end function wr_DivLat_xyr

    function wr_Div_xyr_xyr_xyr(xyr_Vlon,xyr_Vlat,xyr_Vrad)
      !
      ! べクトル成分である 3 つの格子データに発散を作用させた
      ! スペクトルデータを返す.
      !
      ! 第 1, 2 ,3 引数(u,v,w)がそれぞれベクトルの経度成分, 緯度成分, 
      ! 動径成分を表し, 発散は 
      !
      !      1/rcosφ・∂u/∂λ + 1/rcosφ・∂(v cosφ)/∂φ 
      !    + 1/r^2 ∂/∂r (r^2 w)
      !
      ! と計算される.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_Div_xyr_xyr_xyr
      !(out) ベクトル場の発散

      wr_Div_xyr_xyr_xyr =   wr_DivLon_xyr(xyr_Vlon) &
                           + wr_DivLat_xyr(xyr_Vlat) &
                           + wr_DivRad_wu(wu_xyr(xyr_Vrad))

    end function wr_Div_xyr_xyr_xyr

    function xyr_Div_xyr_xyr_xyr(xyr_Vlon,xyr_Vlat,xyr_Vrad)
      !
      ! ベクトル成分である 3 つの格子データに発散を作用させる.
      !
      ! 第 1, 2 ,3 引数(u,v,w)がそれぞれベクトルの経度成分, 緯度成分, 
      ! 動径成分を表す.
      !
      ! 極の特異性を回避するためにベクトル場に cosφ/r の重みをかけて
      ! 計算している. 
      !
      !      div V = (r/cosφ)・div (Vcosφ/r) + V_φtanφ/r + V_r/r
      ! 
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,0:km)             :: xyr_Div_xyr_xyr_xyr
      !(out) ベクトル場の発散

      xyr_Div_xyr_xyr_xyr &
           = xyr_Rad/cos(xyr_Lat) &
                * xyr_wr(wr_Div_xyr_xyr_xyr(xyr_VLon*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VLat*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VRad*cos(xyr_Lat)/xyr_Rad ))&
             + xyr_VLat*tan(xyr_Lat)/xyr_Rad &
             + xyr_VRad/xyr_Rad

    end function xyr_Div_xyr_xyr_xyr

    function xyr_RotLon_wu_wu(wu_Vrad,wu_Vlat) 
      !
      ! ベクトル場の動径成分, 緯度成分である第 1, 2 引数 Vrad, Vlat から
      ! 回転の経度成分 
      !
      !    1/r ∂Vrad/∂φ-1/r ∂(r Vlat)/∂r を計算する.
      !
      ! を計算する
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_RotLon_wu_wu
      !(out) ベクトル場の回転の経度成分

        xyr_RotLon_wu_wu =   xyr_GradLat_wu(wu_Vrad) &
                           - xyr_wr(wr_RotRad_wu(wu_Vlat))

    end function xyr_RotLon_wu_wu

    function xyr_RotLat_wu_wu(wu_Vlon,wu_Vrad) 
      !
      ! ベクトル場の経度成分, 動径成分である第 1, 2 引数 Vlon, Vrad から
      ! 回転の緯度成分 
      !
      !    1/r ∂(r Vlon)/∂r - 1/rcosφ・∂Vrad/∂λ
      !
      ! を計算する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_RotLat_wu_wu
      !(out) ベクトル場の回転の緯度成分

        xyr_RotLat_wu_wu =   xyr_wr(wr_RotRad_wu(wu_Vlon)) &
                           - xyr_GradLon_wu(wu_Vrad) 

    end function xyr_RotLat_wu_wu

    function wr_RotRad_xyr_xyr(xyr_Vlat,xyr_Vlon) 
      !
      ! ベクトルの緯度成分, 経度成分である第 1, 2 引数 Vlat, Vlon に対して
      ! ベクトル場の回転の動径成分 
      !
      !    1/rcosφ・∂Vlat/∂λ - 1/rcosφ・∂(Vlon cosφ)/∂φ
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_RotRad_xyr_xyr
      !(out) ベクトル場の回転の動径成分

        wr_RotRad_xyr_xyr =   wr_DivLon_xyr(xyr_Vlat) &
                            - wr_DivLat_xyr(xyr_Vlon)

    end function wr_RotRad_xyr_xyr

  !--------------- 積分計算 -----------------
    !----(入力データ xyr)---
    function yr_IntLon_xyr(xyr)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,0:km)  :: yr_IntLon_xyr
      !(out) 経度方向(帯状)積分された 2 次元子午面格子点データ

      integer :: i

      yr_IntLon_xyr = 0.0d0
      do i=0,im-1
         yr_IntLon_xyr(:,:) = yr_IntLon_xyr(:,:) &
                       + xyr(i,:,:) * x_Lon_Weight(i)
      enddo

    end function yr_IntLon_xyr

    function xr_IntLat_xyr(xyr)
      !
      ! 3 次元格子点データの緯度方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xr_IntLat_xyr
      !(out) 緯度積分された 2 次元緯度動径格子点データ
      ! 緯度円格子点データ

      integer :: j

      xr_IntLat_xyr = 0.0d0
      do j=1,jm
         xr_IntLat_xyr(:,:) = xr_IntLat_xyr(:,:) &
                       + xyr(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xr_IntLat_xyr

    function xy_IntRad_xyr(xyr)  ! 動径積分
      !
      ! 3 次元格子点データの動径方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dr を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyr
      !(out) 動径積分された 2 次元経度緯度(水平, 球面)格子点データ

      integer :: k

      xy_IntRad_xyr = 0.0d0
      do k=0,km
         xy_IntRad_xyr(:,:) = xy_IntRad_xyr(:,:) &
                       + xyr(:,:,k) * r_Rad_Weight(k) 
      enddo

    end function xy_IntRad_xyr

    function x_IntLatRad_xyr(xyr)
      !
      ! 3 次元格子点データの緯度動径(子午面)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyr
      !(out) 緯度動径(子午面)積分された 1 次元経度格子点データ

      integer :: j, k

      x_IntLatRad_xyr = 0.0D0
      do k=0,km
         do j=1,jm
            x_IntLatRad_xyr = x_IntLatRad_xyr &
                 + xyr(:,j,k) * y_Lat_Weight(j) * r_Rad_Weight(k)
         enddo
      enddo

    end function x_IntLatRad_xyr

    function y_IntLonRad_xyr(xyr)
      !
      ! 3 次元格子点データの経度動径(緯度円)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dλdr を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm)       :: y_IntLonRad_xyr
      !(out) 経度動径(緯度円)積分された 1 次元緯度格子点データ

      integer :: i, k

      y_IntLonRad_xyr = 0.0D0
      do k=0,km
         do i=0,im-1
            y_IntLonRad_xyr = y_IntLonRad_xyr &
                 + xyr(i,:,k) * x_Lon_Weight(i) * r_Rad_Weight(k)
         enddo
      enddo

    end function y_IntLonRad_xyr

    function r_IntLonLat_xyr(xyr)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: r_IntLonLat_xyr
      !(out) 緯度経度(水平, 球面)積分された 1 次元動径格子点データ

      integer :: i, j

      r_IntLonLat_xyr = 0
      do j=1,jm
         do i=0,im-1
            r_IntLonLat_xyr = r_IntLonLat_xyr &
                 + xyr(i,j,:) * x_Lon_Weight(i) * y_Lat_Weight(j)
         enddo
      enddo

    end function r_IntLonLat_xyr

    function IntLonLatRad_xyr(xyr) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2cosφ dλdφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr 
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: IntLonLatRad_xyr 
      !(out) 全球積分値

      integer :: i, j, k

      IntLonLatRad_xyr = 0
      do k=0,km
         do j=1,jm
            do i=0,im-1
               IntLonLatRad_xyr = IntLonLatRad_xyr &
                    + xyr(i,j,k) * x_Lon_Weight(i) &
                         * y_Lat_Weight(j) * r_Rad_Weight(k)
            enddo
         enddo
      enddo

    end function IntLonLatRad_xyr

    !----(入力データ yr)---
    function r_IntLat_yr(yr)  ! 緯度積分
      !
      ! 2 次元(YR)格子点データの緯度方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: r_IntLat_yr
      !(out) 緯度積分された 1 次元動径格子点データ

      integer :: j

      r_IntLat_yr = 0.0d0
      do j=1,jm
         r_IntLat_yr(:) = r_IntLat_yr(:) + yr(j,:) * y_Lat_Weight(j)
      enddo

    end function r_IntLat_yr

    function y_IntRad_yr(yr)  ! 動径積分
      !
      ! 2 次元(YR)格子点データの動径方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) r^2dr を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(1:jm)  :: y_IntRad_yr
      !(out) 動径積分された 1 次元緯度格子点データ

      integer :: k

      y_IntRad_yr = 0.0d0
      do k=0,km
         y_IntRad_yr(:) = y_IntRad_yr(:) &
                       + yr(:,k) * r_Rad_Weight(k) 
      enddo

    end function y_IntRad_yr

    function IntLatRad_yr(yr)
      !
      ! 2 次元(YR)格子点データの緯度動径積分(子午面)および平均
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2cosφ dφdr を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ
      real(8)                   :: IntLatRad_yr
      !(out) 積分値
      integer :: j, k

      IntLatRad_yr = 0.0D0
      do k=0,km
         do j=1,jm
            IntLatRad_yr = IntLatRad_yr &
                 + yr(j,k) * y_Lat_Weight(j) * r_Rad_Weight(k)
         enddo
      enddo

    end function IntLatRad_yr

    !----(入力データ xr)---
    function r_IntLon_xr(xr)
      !
      ! 2 次元(XR)格子点データの経度方向積分.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r)dλ を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:km)  :: r_IntLon_xr
      !(out) 経度積分された 1 次元動径格子点データ

      integer :: i

      r_IntLon_xr = 0.0d0
      do i=0,im-1
         r_IntLon_xr(:) = r_IntLon_xr(:) + xr(i,:) * x_Lon_Weight(i)
      enddo

    end function r_IntLon_xr

    function x_IntRad_xr(xr)
      !
      ! 2 次元(XR)格子点データの動径方向域積分.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r) r^2dr を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1)  :: x_IntRad_xr
      !(out) 動径積分された 1 次元経度格子点データ

      integer :: k

      x_IntRad_xr = 0.0d0
      do k=0,km
         x_IntRad_xr(:) = x_IntRad_xr(:) &
                       + xr(:,k) * r_Rad_Weight(k) 
      enddo

    end function x_IntRad_xr

    function IntLonRad_xr(xr)  ! 経度動径(緯度円)積分
      !
      ! 2 次元(XR)格子点データの経度動径積分
      !
      ! 2 次元データ f(λ,r) に対して∫f(λ,r) r^2dλdr を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8)                                 :: IntLonRad_xr
      !(out) 積分値

      integer :: i, k

      IntLonRad_xr = 0.0D0
      do k=0,km
         do i=0,im-1
            IntLonRad_xr = IntLonRad_xr &
                 + xr(i,k) * x_Lon_Weight(i) * r_Rad_Weight(k)
         enddo
      enddo

    end function IntLonRad_xr

    !----(入力データ z)---
    function IntRad_r(z)  ! 動径積分
      !
      ! 1 次元(Z)格子点データの動径方向域積分.
      !
      ! 1 次元データ f(r) に対して ∫f(r) r^2dr を計算する.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 次元動径格子点データ

      real(8)                              :: IntRad_r
      !(out) 積分値

      integer :: k

      IntRad_r = 0.0d0
      do k=0,km
         IntRad_r = IntRad_r + z(k) * r_Rad_Weight(k) 
      enddo

    end function IntRad_r

  !--------------- 平均計算 -----------------
    !----(入力データ xyr)---
    function yr_AvrLon_xyr(xyr)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ/2π を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,0:km)  :: yr_AvrLon_xyr
      !(out) 経度方向(帯状)平均された 2 次元子午面格子点データ

      yr_AvrLon_xyr = yr_IntLon_xyr(xyr)/sum(x_Lon_Weight)

    end function yr_AvrLon_xyr

    function xr_AvrLat_xyr(xyr)  ! 緯度積分
      !
      ! 3 次元格子点データの緯度方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)cosφ dφ/2 を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xr_AvrLat_xyr
      !(out) 緯度平均された 2 次元緯度動径格子点データ

      xr_AvrLat_xyr = xr_IntLat_xyr(xyr)/sum(y_Lat_Weight)

    end function xr_AvrLat_xyr

    function xy_AvrRad_xyr(xyr)
      !
      ! 3 次元格子点データの動径方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して 
      !
      !    ∫f(λ,φ,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyr          
      ! 水平格子点データ
      !(out) 動径平均された 2 次元経度緯度(水平, 球面)格子点データ

      xy_AvrRad_xyr = xy_IntRad_xyr(xyr)/sum(r_Rad_Weight)

    end function xy_AvrRad_xyr

    function x_AvrLatRad_xyr(xyr)  ! 緯度動径(子午面)積分
      !
      ! 3 次元格子点データの緯度動径(子午面)平均
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xyr
      !(out) 緯度動径(子午面)平均された 1 次元経度格子点データ

      x_AvrLatRad_xyr = x_IntLatRad_xyr(xyr) &
                   /( sum(y_Lat_Weight)*sum(r_Rad_Weight) )

    end function x_AvrLatRad_xyr

    function y_AvrLonRad_xyr(xyr)  ! 経度動径(緯度円)積分
      !
      ! 3 次元格子点データの経度動径(緯度円)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2dλdr /(2π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm)       :: y_AvrLonRad_xyr
      !(out) 経度動径(緯度円)平均された 1 次元緯度格子点データ

      y_AvrLonRad_xyr = y_IntLonRad_xyr(xyr) &
                 /(sum(x_Lon_Weight)*sum(r_Rad_Weight))

    end function y_AvrLonRad_xyr

    function r_AvrLonLat_xyr(xyr)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ /4π 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: r_AvrLonLat_xyr
      !(out) 緯度経度(水平, 球面)平均された 1 次元動径格子点データ

      r_AvrLonLat_xyr = r_IntLonLat_xyr(xyr) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function r_AvrLonLat_xyr

    function AvrLonLatRad_xyr(xyr) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dλdφdr /(4π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: AvrLonLatRad_xyr
      !(out) 全球平均値

      AvrLonLatRad_xyr = IntLonLatRad_xyr(xyr) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(r_Rad_Weight))

    end function AvrLonLatRad_xyr

    !----(入力データ yr)---
    function r_AvrLat_yr(yr)
      !
      ! 2 次元(YR)格子点データの緯度方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) cosφ dφ/2 を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: r_AvrLat_yr
      !(out) 緯度平均された 1 次元動径格子点データ

      r_AvrLat_yr = r_IntLat_yr(yr)/sum(y_Lat_Weight)

    end function r_AvrLat_yr

    function y_AvrRad_yr(yr)
      !
      ! 2 次元(YR)格子点データの動径方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(1:jm)  :: y_AvrRad_yr
      !(out) 動径平均された 1 次元緯度格子点データ

      y_AvrRad_yr = y_IntRad_yr(yr)/sum(r_Rad_Weight)

    end function y_AvrRad_yr

    function AvrLatRad_yr(yr)  ! 緯度動径(子午面)積分
      !
      ! 2 次元(YR)格子点データの緯度動径(子午面)平均
      !
      ! 2 次元データ f(φ,r) に対して
      !
      !    ∫f(φ,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: AvrLatRad_yr
      !(out) 平均値

      AvrLatRad_yr = IntLatRad_yr(yr)/(sum(y_Lat_Weight)*sum(r_Rad_Weight))

    end function AvrLatRad_yr

    !----(入力データ xr)---
    function r_AvrLon_xr(xr)  ! 経度(帯状)積分
      !
      ! 2 次元(XR)格子点データの経度方向平均.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r)dλ/2π を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:km)  :: r_AvrLon_xr 
      !(out) 経度平均された 1 次元動径格子点データ

      r_AvrLon_xr = r_IntLon_xr(xr)/sum(x_Lon_Weight)

    end function r_AvrLon_xr

    function x_AvrRad_xr(xr)  ! 動径積分
      !
      ! 2 次元(XR)格子点データの動径方向域平均.
      !
      ! 2 次元データ f(λ,r) に対して
      !
      !   ∫f(λ,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1)  :: x_AvrRad_xr
      !(out) 動径平均された 1 次元経度格子点データ

      x_AvrRad_xr = x_IntRad_xr(xr)/sum(r_Rad_Weight)

    end function x_AvrRad_xr

    function AvrLonRad_xr(xr)  ! 経度動径(緯度円)積分
      !
      ! 2 次元(XR)格子点データの経度動径平均
      !
      ! 2 次元データ f(λ,r) に対して 
      ! 
      !    ∫f(λ,r) r^2dλdr /(2π(r[o]^3-r[i]^3)/3)
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xr    
      ! (in)2 次元格子点データ
      real(8)                                 :: AvrLonRad_xr      
      ! 積分値

      AvrLonRad_xr = IntLonRad_xr(xr)/(sum(x_Lon_Weight)*sum(r_Rad_Weight))

    end function AvrLonRad_xr

    !----(入力データ z)---
    function AvrRad_r(z)
      !
      ! 1 次元(Z)格子点データの動径方向域平均.
      !
      ! 1 次元データ f(r) に対して ∫f(r) r^2dr /((r[o]^3-r[i]^3)/3) を
      ! 計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: z
      !(in) 1 次元動径格子点データ
      real(8)                                 :: AvrRad_r
      !(out) 平均値

      AvrRad_r = IntRad_r(z)/sum(r_Rad_Weight)

    end function AvrRad_r

  !--------------- ポロイダル/トロイダルモデル用微分 -----------------

    function wu_KxRGrad_wu(wu)
      !
      ! 入力スペクトルデータに経度微分 k×r・▽ = ∂/∂λを作用する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_KxRGrad_wu
      !(out) 経度微分を作用された 2 次元スペクトルデータ

      wu_KxRGrad_wu =  wa_Dlon_wa(wu)

    end function wu_KxRGrad_wu

    function xyr_KGrad_wu(wu)    ! k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r
      !
      ! 入力スペクトルデータに対応する格子データに軸方向微分 
      !
      !    k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r 
      !
      ! を作用させた格子データが返される. 
      ! ここでベクトル k は球の中心から北極向きの単位ベクトルである.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyr_KGrad_wu
      !(out) 軸方向微分を作用された 2 次元スペクトルデータ

      xyr_KGrad_wu =  cos(xyr_Lat)*xyr_GradLat_wu(wu) &
                    + sin(xyr_Lat)*xyr_wr(wr_DRad_wu(wu))

    end function xyr_KGrad_wu

    function wu_L2_wu(wu)
      !
      ! 入力スペクトルデータに L^2 演算子(=-水平ラプラシアン)を作用する.
      !
      ! L^2 演算子は単位球面上の水平ラプラシアンの逆符号にあたる. 
      !  入力スペクトルデ ータに対応する格子点データに演算子 
      !
      !     L^2 = -1/cos^2φ・∂^2/∂λ^2 - 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用させたデータのスペクトル変換が返される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_L2_wu
      !(out) L^2 演算子を作用された 2 次元スペクトルデータ

      wu_L2_wu = -wa_Lapla_wa(wu)

    end function wu_L2_wu

    function wu_L2Inv_wu(wu)
      !
      ! 入力スペクトルデータに L^2 演算子の逆演算(-逆水平ラプラシアン)を
      ! 作用する.
      !
      ! スペクトルデータに L^2 演算子を作用させる関数 wu_L2_wu の逆計算を
      ! 行う関数である.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_L2Inv_wu
      !(out) L^2 演算子の逆演算を作用された 2 次元スペクトルデータ

      wu_L2Inv_wu = -wa_LaplaInv_wa(wu)

    end function wu_L2Inv_wu

    function wu_QOperator_wu(wu)
      !
      ! 入力スペクトルデータに対応する格子点データに演算子 
      !
      !    Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) 
      !
      ! を作用させたデータのスペクトル変換が返される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wu_QOperator_wu
      !(out) Q 演算子を作用された 2 次元スペクトルデータ

      wu_QOperator_wu = &
             wu_xyr(xyr_KGrad_wu(wu) - xyr_KGrad_wu(wu_L2_wu(wu))/2) &
           - wu_L2_wu(wu_xyr(xyr_KGrad_wu(wu)))/2

    end function wu_QOperator_wu

    function wu_RadRot_xyr_xyr(xyr_VLON,xyr_VLAT)  ! r・(▽×v)
      !
      ! ベクトルの渦度と動径ベクトルの内積 r・(▽×v) を計算する.
      !
      ! 第 1, 2 引数(v[λ], v[φ])がそれぞれベクトルの経度成分, 緯度成分を表す.
      !
      !    r・(▽×v) = 1/cosφ・∂v[φ]/∂λ - 1/cosφ・∂(v[λ] cosφ)/∂φ
      !
      ! のスペクトル データが返される.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wu_RadRot_xyr_xyr
      !(out) ベクトルの渦度と動径ベクトルの内積

      wu_RadRot_xyr_xyr = wu_wr(wa_DivLon_xya(xyr_VLAT) &
                                - wa_DivLat_xya(xyr_VLON))
      
    end function wu_RadRot_xyr_xyr

    function wr_RadRotRot_xyr_xyr_xyr(xyr_VLON,xyr_VLAT,xyr_VRAD) 
      ! 
      ! ベクトル v に対して r・(▽×▽×v) を計算する.
      !
      ! 第 1, 2, 3 引数(v[λ], v[φ], v[r])がそれぞれベクトルの経度成分, 
      ! 緯度成分, 動径成分を表す. 
      !
      !    r・(▽×▽×v)  = 1/r ∂/∂r (r・( 1/cosφ・∂v[λ]/∂λ 
      !                                  + 1/cosφ・∂(v[φ] cosφ)/∂φ ) ) 
      !                     + L^2 v[r]/r 
      !
      ! のスペクトルデータが返される.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyr_VRAD
      !(in) ベクトルの動径成分

      real(8), dimension((nm+1)*(nm+1),0:km)     :: wr_RadRotRot_xyr_xyr_xyr
      !(out) ベクトル v の r・(▽×▽×v) 

      wr_RadRotRot_xyr_xyr_xyr = &
                   wr_RotRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wr_xyr(xyr_VRAD/xyr_RAD))

    end function wr_RadRotRot_xyr_xyr_xyr

    subroutine wu_Potential2Vector(&
         xyr_VLON,xyr_VLAT,xyr_VRAD,wu_TORPOT,wu_POLPOT)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψr) + ▽x▽x(Φr) 
      !
      ! の各成分を計算する
      !
      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLON
      !(out) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VLAT
      !(out) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyr_VRAD
      !(out) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) ポロイダルポテンシャル

      xyr_VLON =   xyr_RAD * xyr_GradLat_wu(wu_TORPOT) &
                 + xya_GradLon_wa(wr_RotRad_wu(wu_POLPOT))
      xyr_VLAT = - xyr_RAD * xyr_GradLon_wu(wu_TORPOT) &
                 + xya_GradLat_wa(wr_RotRad_wu(wu_POLPOT))
      xyr_VRAD = xyr_wu(wu_L2_wu(wu_POLPOT))/xyr_RAD

    end subroutine wu_Potential2Vector

    subroutine wu_Potential2Rotation(&
       xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD,wu_TORPOT,wu_POLPOT)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψr) + ▽x▽x(Φr) 
      !
      ! に対して, その回転
      !
      !     ▽xv = ▽x▽x(Ψr) + ▽x▽x▽x(Φr) = ▽x▽x(Ψr) - ▽x((▽^2Φ)r)
      !
      ! を計算する. 
      
      ! ベクトル場の回転
      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVLON
      !(out) 回転の経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVLAT
      !(out) 回転の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyr_RotVRAD
      !(out) 回転の動径成分

      ! 入力ベクトル場を表すポテンシャル
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) ポロイダルポテンシャル

      call wu_Potential2Vector( &
           xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD, &
           -wu_wr(wr_Lapla_wu(wu_POLPOT)), wu_TORPOT)

    end subroutine wu_Potential2Rotation

  !--------------- ポロイダル/トロイダルモデル用スペクトル解析 ----------------

    function nmr_ToroidalEnergySpectrum_wu(wu_TORPOT)
      !
      ! トロイダルポテンシャルから, トロイダルエネルギーの
      ! 球面調和函数全波数 n, 帯状波数 m の各成分を計算する
      !
      !  * 全波数 n, 帯状波数 m のトロイダルポテンシャルのスペクトル成分
      !    ψ(n,m,r)から全波数 n, 帯状波数 m 成分のトロイダルエネルギー
      !    スペクトルは  (1/2)n(n+1)4πr^2ψ(n,m,r)^2  と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !    
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    wu_VMiss によって設定できる (初期値は -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmr_ToroidalEnergySpectrum_wu
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA   ! 作業領域
      integer :: n, m

      nmr_ToroidalEnergySpectrum_wu = wu_VMiss

      wr_DATA = wr_wu(wu_TORPOT)
      do n=0,nm
         do m=-n,n
            nmr_ToroidalEnergySpectrum_wu(n,m,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * wr_DATA(l_nm(n,m),:)**2
         enddo
      enddo

    end function nmr_ToroidalEnergySpectrum_wu

    function nr_ToroidalEnergySpectrum_wu(wu_TORPOT)
      !
      ! トロイダルポテンシャルから, トロイダルエネルギーの
      ! 球面調和函数全波数の各成分を計算する.
      !
      !  * 全波数 n, 帯状波数 m のトロイダルポテンシャルのスペクトル成分
      !    ψ(n,m,r)から全波数 n 成分のトロイダルエネルギースペクトルは
      !    Σ[m=-n]^n(1/2)n(n+1)4πr^2ψ(n,m,r)^2 と計算される.
      !
      ! * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,0:km) :: nr_ToroidalEnergySpectrum_wu 
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA   ! 作業領域
      integer :: n, m

      wr_DATA = wr_wu(wu_TORPOT)
      do n=0,nm
         nr_ToroidalEnergySpectrum_wu(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * sum(wr_Data(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
      enddo

    end function nr_ToroidalEnergySpectrum_wu

    function nmr_PoloidalEnergySpectrum_wu(wu_POLPOT)
      !
      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
      ! 球面調和函数全波数 n, 帯状波数 m の各成分を計算する.
      !
      !  * 全波数 n, 帯状波数 m のポロイダルポテンシャルのスペクトル成分
      !    φ(n,m,r)から全波数 n, 帯状波数 m 成分のポロイダルエネルギー
      !    スペクトルは 
      !
      !      (1/2)n(n+1)4πr^2{[d(rφ(n,m,r))/dr]^2 + n(n+1)φ(n,m,r)^2} 
      !
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 wu_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmr_PoloidalEnergySpectrum_wu 
      !(out) エネルギースペクトルポロイダル成分


      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA1   ! 作業領域
      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA2   ! 作業領域
      integer :: n, m

      nmr_PoloidalEnergySpectrum_wu = wu_VMiss

      wr_Data1 = wr_wu(wu_POLPOT)
      wr_Data2 = wr_Rad*wr_DRad_wu(wu_POLPOT) &           ! d(rφ)/dr
               + wr_wu(wu_POLPOT)                         ! = rdφ/dr+φ

      do n=0,nm
         do m=-n,n
            nmr_PoloidalEnergySpectrum_wu(n,m,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2  &
                   + n*(n+1)*wr_Data1(l_nm(n,m),:)**2 )
         enddo
      enddo

    end function nmr_PoloidalEnergySpectrum_wu

    function nr_PoloidalEnergySpectrum_wu(wu_POLPOT)
      !
      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
      ! 球面調和函数全波数の各成分を計算する
      !
      !  * 全波数 n, 帯状波数 m のポロイダルポテンシャルのスペクトル成分
      !    φ(n,m,r)から全波数 n 成分のポロイダルエネルギースペクトルは
      !
      !      Σ[m=-n]^n ((1/2)n(n+1)4πr^2{[d(rφ(n,m,r))/dr]^2 
      !                 + n(n+1)φ(n,m,r)^2} 
      !
      !    と計算される.
      !
      !  * 全ての全波数に対してのエネルギースペクトル成分の和を動径積分したもの
      !    (r^2の重み無し)が球殻内での全エネルギーに等しい.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wu_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,0:km) :: nr_PoloidalEnergySpectrum_wu
      !(out) エネルギースペクトルポロイダル成分

      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA1   ! 作業領域
      real(8), dimension((nm+1)*(nm+1),0:km) ::wr_DATA2   ! 作業領域
      integer :: n, m

      wr_Data1 = wr_wu(wu_POLPOT)
      wr_Data2 = wr_Rad*wr_DRad_wu(wu_POLPOT) &           ! d(rφ)/dr
               + wr_wu(wu_POLPOT)                         ! = rdφ/dr+φ

      do n=0,nm
         nr_PoloidalEnergySpectrum_wu(n,:) = &
              + 0.5* n*(n+1)* (4*pi) &
              *( sum(wr_Data2(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)  &
                + n*(n+1)*sum(wr_Data1(l_nm(n,(/(m,m=-n,n)/)),:)**2,1) )
      enddo

    end function nr_PoloidalEnergySpectrum_wu

  !--------------- 境界値問題 -----------------

    subroutine wu_BoundaryTau(wu,value,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! Chebyshev 空間での境界条件適用(タウ法)
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を
      ! 定める方法をとっている(タウ法).
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional   :: value
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !     省略時は値/勾配 0 となる. 

      character(len=1), intent(in), optional                    :: cond
              !(in) 境界条件. 省略時は 'D'
              !        D : 外側ディリクレ条件
              !        N : 外側ノイマン条件

      if (.not. present(cond)) then
         if (present(value)) then
            call au_BoundaryTau_D(wu,value)
         else
            call au_BoundaryTau_D(wu)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call au_BoundaryTau_N(wu,value)
         else
            call au_BoundaryTau_N(wu)
         endif
      case ('D')
         if (present(value)) then
            call au_BoundaryTau_D(wu,value)
         else
            call au_BoundaryTau_D(wu)
         endif
      case default
         call MessageNotify('E','wu_BoundaryTau','B.C. not supported')
      end select

    end subroutine wu_BoundaryTau

    subroutine wu_BoundaryGrid(wu,value,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wu_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional   :: value
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !    省略時は値/勾配 0 となる. 

      character(len=1), intent(in), optional             :: cond
              !(in) 境界条件. 省略時は 'D'
              !        D : 外側ディリクレ条件
              !        N : 外側ノイマン条件

      if (.not. present(cond)) then
         if (present(value)) then
            call au_BoundaryGrid_D(wu,value)
         else
            call au_BoundaryGrid_D(wu)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call au_BoundaryGrid_N(wu,value)
         else
            call au_BoundaryGrid_N(wu)
         endif
      case ('D')
         if (present(value)) then
            call au_BoundaryGrid_D(wu,value)
         else
            call au_BoundaryGrid_D(wu)
         endif
      case default
         call MessageNotify('E','wu_BoundaryGrid','B.C. not supported')
      end select

    end subroutine wu_BoundaryGrid

    subroutine wu_TorBoundaryTau(wu_TORPOT,value,cond,new) 
      !
      ! 速度トロイダルポテンシャルに対して境界条件を適用する. 
      ! Chebyshev 空間での境界条件適用. 
      !
      ! 速度トロイダルポテンシャルΨに対して与えられる境界条件は
      !
      !   * 粘着条件 : Ψ = Ψb(lon,lat). Ψb は境界球面での速度分布. 
      !                                   default は 0(静止状態).
      !
      !   * 応力なし条件 : ∂(Ψ/r)/∂r = 0.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wu_TORPOT
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional :: value
              !(in) 両端境界でのトロイダルポテンシャル
              !     粘着条件の時のみ有効

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_data
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_data
      logical                                 :: rigid        ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE.
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wu_TorBoundaryTau','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l)=1.0D0
         enddo

         ! 力学的条件粘着条件 
         if ( rigid ) then
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l) = 1.0D0
               wr_data = wr_wu(wu_data)
               alu(:,lm,l) = wr_data(:,0)
            enddo
         else
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l) = 1.0D0
               wr_data = wr_DRad_wu(wu_data) - wr_wu(wu_data)/wr_Rad
               alu(:,lm,l) = wr_data(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wu_TorBoundaryTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wu_TorBoundaryTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wu_TorBoundaryTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wu_TorBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid .AND. present(value) ) then
         wu_torpot(:,lm) = value
      else
         wu_torpot(:,lm) = 0.0D0
      endif

      wu_torpot = lusolve(alu,kp,wu_TORPOT)

    end subroutine wu_TorBoundaryTau

    subroutine wu_TorBoundaryGrid(wu_TORPOT,value,cond,new) 
      !
      ! 速度トロイダルポテンシャルに対して境界条件を適用する.
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wu_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 速度トロイダルポテンシャルΨに対して与えられる境界条件は
      !
      !   * 粘着条件 : Ψ = Ψb(lon,lat). Ψb は境界球面での速度分布. 
      !                                   default は 0 (静止状態).
      !
      !   * 応力なし条件 : ∂(Ψ/r)/∂r = 0.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)    :: wu_TORPOT
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1)), intent(in), optional :: value
              !(in) 両端境界でのトロイダルポテンシャル
              !     粘着条件の時のみ有効

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_TORPOT
      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_data
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_data
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE.
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wu_TorBoundaryGrid','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))

         alu = 0.0D0
         do l=0,lm
            wu_data = 0.0D0
            wu_data(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_data)
         enddo

         if ( .not. rigid ) then
            do l=0,lm
               wu_data = 0.0D0
               wu_data(:,l)=1.0D0
               wr_data = wr_DRad_wu(wu_data) - wr_wu(wu_data)/wr_Rad
               alu(:,0,l) = wr_data(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wu_TorBoundaryGrid',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wu_TorBoundaryGrid',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wu_TorBoundaryGrid',&
                 'Boundary value at k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wu_TorBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_TorPot       = wr_wu(wu_TorPot)

      if ( rigid .AND. present(value) ) then
         wr_TorPot(:,0)  = value
      else
         wr_TorPot(:,0)  = 0.0D0
      endif

      wu_torpot = lusolve(alu,kp,wr_TorPot)

    end subroutine wu_TorBoundaryGrid

    function wr_LaplaPol2Pol_wr(wr,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      !
      ! チェビシェフ格子点空間で境界条件を適用している. 
      ! この関数を用いるためには wu_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !   ▽^2Φ = f
      !     Φ = const. at Boundary.
      !     ∂Φ/∂r = 0 at Boundary           (粘着条件) 
      !     or ∂^2Φ/∂r^2 = 0 at Boundary    (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:km),intent(in)  :: wr
              !(in) 入力▽^2φ分布

      real(8), dimension((nm+1)*(nm+1),0:km)             :: wr_LaplaPol2Pol_wr
              !(out) 出力ポロイダルポテンシャル分布

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_work
      real(8), dimension(0:km,0:km)           :: gg
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wu_laplapol2pol_wu','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','wr_LaplaPol2Pol_wr', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:km),kp((nm+1)*(nm+1),0:km))

         do k=0,km
            wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,k) = wr_Lapla_wu(wu_wr(wr_work))
         enddo

         ! 運動学的条件. 流線は境界で一定

         gg = 0.0D0
         do k=0,km
            gg(k,k)=1.0D0
         enddo
         do n=1,(nm+1)*(nm+1)
            alu(n,0,:)   = gg(:,0)
            alu(n,km,:)  = gg(:,km)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid ) then
            do k=0,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_DRad_wu(wu_wr(wr_work))
               alu(:,1,k) = wr_work(:,0)
            enddo
         else
            do k=0,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_DRad2_wu(wu_wr(wr_work))
               alu(:,1,k) = wr_work(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_work         = wr
      wr_work(:,1)    = 0.0D0               ! 力学的条件
      wr_work(:,0)    = 0.0D0               ! 運動学的条件

      wr_laplapol2pol_wr = lusolve(alu,kp,wr_work)

    end function wr_LaplaPol2Pol_wr

    function wu_LaplaPol2PolGrid_wu(wu,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      ! チェビシェフ格子点空間で境界条件を適用している. 
      !
      ! この関数を用いるためには wu_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !    ▽^2Φ = f
      !      Φ = const. at Boundary.
      !      ∂Φ/∂r = 0 at Boundary          (粘着条件) 
      !      or ∂^2Φ/∂r^2 = 0 at Boundary   (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      ! 最終的にチェビシェフ係数の解が欲しい場合には, wr_LaplaPol2Pol_wr に
      ! 比べてチェビシェフ -- 格子点変換が 1 回分少なくて済む.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(in)  :: wu
              !(in) 入力▽^2φ分布

      real(8), dimension((nm+1)*(nm+1),0:lm)         :: wu_LaplaPol2PolGrid_wu
              !(out) 出力ポロイダルポテンシャル分布

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_work
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wu_work
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE. 
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wu_LaplaPol2PolGrid_wu','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','wu_LaplaPol2PolGrid_wu', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))

         do l=0,lm
            wu_work = 0.0D0 ; wu_work(:,l) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,l) = wr_Lapla_wu(wu_work)
         enddo

         ! 運動学的条件. 流線は境界で一定
         do l=0,lm
            wu_work=0.0D0
            wu_work(:,l)=1.0D0
            wr_work=wr_wu(wu_work)
            alu(:,0,l)=wr_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid ) then
            do l=0,lm
               wu_work=0.0D0
               wu_work(:,l)=1.0D0
               wr_work=wr_DRad_wu(wu_work)
               alu(:,1,l)=wr_work(:,0)
            enddo
         else
            do l=0,lm
               wu_work=0.0D0
               wu_work(:,l)=1.0D0
               wr_work=wr_DRad2_wu(wu_work)
               alu(:,1,l)=wr_work(:,0)
            enddo
         endif

         call ludecomp(alu,kp)

         call MessageNotify('M','wu_LaplaPol2PolGrid_wu',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_work         = wr_wu(wu)
      wr_work(:,1)    = 0.0D0               ! 力学的条件
      wr_work(:,0)    = 0.0D0               ! 運動学的条件

      wu_LaplaPol2PolGrid_wu = lusolve(alu,kp,wr_work)

    end function wu_LaplaPol2PolGrid_wu

    subroutine wu_TormagBoundaryTau(wu_TOR,new)
      
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wu_psi = 0   at the outer boundary
      ! 
      ! であるから wu_Boundary で対応可能だが, 将来のため別途作成しておく.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),0:km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! 非電気伝導体
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            alu(:,lm,l) = wr_Psi(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wu_TOR(:,lm)   = 0.0D0
      wu_TOR = lusolve(alu,kp,wu_TOR)

    end subroutine wu_TormagBoundaryTau

    subroutine wu_TormagBoundaryGrid(wu_TOR,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wu_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wu_psi = 0   at the outer boundary
      ! 
      ! であるので wu_Boundary で対応可能だが, 将来のため別途作成しておく
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_TOR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorMagBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),0:km))

         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_I)              ! 内部領域は値そのまま.
         enddo

         ! 非電気伝導体

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wr_TOR       = wr_wu(wu_TOR)
      wr_TOR(:,0)  = 0.0D0
      wu_TOR = lusolve(alu,kp,wr_TOR)

    end subroutine wu_TormagBoundaryGrid

    subroutine wu_PolmagBoundaryTau(wu_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場ポロイダルポテンシャルの各水平スペクトル
      ! 成分 h にたいして境界条件が与えられ,
      !
      !  * 外側境界 : dh/dr + (n+1)h/r = 0
      !
      ! である. ここで n は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),0:km),wr_DPSIDR((nm+1)*(nm+1),0:km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! 非電気伝導体
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,lm,l) = wr_DPSIDR(n,0) + (nn(1)+1) * wr_PSI(n,0)/r_RAD(0)
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wu_POL(:,lm)   = 0.0D0
      wu_POL = lusolve(alu,kp,wu_POL)

    end subroutine wu_PolmagBoundaryTau

    subroutine wu_PolmagBoundaryGrid(wu_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用. 
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wu_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場ポロイダルポテンシャルの各水平スペクトル成分 h に
      ! たいして境界条件が与えられ,
      !
      !  * 外側境界 : dh/dr + (n+1)h/r = 0
      !
      ! である. ここで n は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wu_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR
      real(8), dimension((nm+1)*(nm+1),0:km)  :: wr_POL

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','PolMagBoundaryGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:km,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wu_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),0:km),wr_DPSIDR((nm+1)*(nm+1),0:km))

         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,:,l) = wr_wu(wu_I)              ! 内部領域は値そのまま.
         enddo

         ! 非電気伝導体
         do l=0,lm
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,0,l) = wr_DPSIDR(n,0) + (nn(1)+1) * wr_PSI(n,0)/r_RAD(0)
            enddo
         end do

         call ludecomp(alu,kp)

         deallocate(wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_POL       = wr_wu(wu_POL)
      wr_POL(:,0)  = 0.0D0
      wu_POL = lusolve(alu,kp,wr_POL)

    end subroutine wu_PolmagBoundaryGrid

  end module wu_module
