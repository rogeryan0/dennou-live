!--
!----------------------------------------------------------------------
! Copyright(c) 2002-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  wt_zonal_module
!
!    spml/wt_zonal_module モジュールは球面上および球殻内での経度方向に
!    一様な帯状的軸対称 2 次元流体運動をスペクトル法によって数値計算する
!    ための Fortran90 関数を提供するものである. 
!
!    水平方向にルジャンドル多項式変換および上下の境界壁を扱うための
!    チェビシェフ多項式変換を用いる場合のスペクトル計算のためのさまざまな
!    関数を提供する. 
!
!    内部で wa_zonal_module, at_module を用いている. 最下部では
!    ルジャンドル多項式およびチェビシェフ多項式変換のエンジンとして 
!    ISPACK の Fortran77 サブルーチンを用いている.
!
!    wt_zonal_modulde で提供される関数・サブルーチンは 2 次元的流体運動を扱う 
!    wt_module モジュールで用いられているものと名前およびインターフェースが
!    共通になるように設計してある. したがって, wt_module を用いて構成された
!    3 次元モデルを帯状方向に一様な 2 次元軸対称モデルへと改造するには
!    次の手順が必要となる. 
!
!      * use 文での wt_module の引用を wt_zonal_module に変更する. 
!      * 配列の大きさを経度方向格子点数 im -> 1 に, 
!        水平波数を (nm+1)**2 -> nm+1 に変更する. 
!      * DO 文で水平波数に関してループを回しているところを
!        (nm+1)**2 -> nm+1 に変更する. 
!      * gtool 出力の次元変数変更する. 
!
!履歴  2008/12/29  竹広真一  wt_module より軸対称モデル用に改造
!      2009/01/08  竹広真一  w_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!凡例
!      データ種類と index
!        x : 経度         y : 緯度        z : 動径
!        w : 球面調和関数スペクトル
!        n : 球面調和関数スペクトル(水平全波数)
!        m : 球面調和関数スペクトル(帯状波数)
!        t : チェビシェフ関数スペクトル
!        a : 任意の次元
!
!        xyz : 3 次元格子点データ
!        xy  : 水平 2 次元格子点データ
!        yz  : 子午面 2 次元格子点データ
!        xz  : 緯度面 2 次元格子点データ
!
!        wz  : 水平スペクトル動径格子点データ
!        wt  : スペクトルデータ
!
!++
module wt_zonal_module
  !
  != wt_zonal_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wt_zonal_module.f90,v 1.6 2010-03-02 10:09:07 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wt_zonal_module モジュールは球面上および球殻内での経度方向に
  ! 一様な帯状的軸対称 2 次元流体運動をスペクトル法によって数値計算する
  ! ための Fortran90 関数を提供するものである. 
  !
  ! 水平方向にルジャンドル多項式変換および上下の境界壁を扱うための
  ! チェビシェフ多項式変換を用いる場合のスペクトル計算のためのさまざまな
  ! 関数を提供する. 
  !
  ! 内部で wa_zonal_module, at_module を用いている. 最下部では
  ! ルジャンドル多項式およびチェビシェフ多項式変換のエンジンとして 
  ! ISPACK の Fortran77 サブルーチンを用いている.
  !
  ! wt_zonal_modulde で提供される関数・サブルーチンは 2 次元的流体運動を扱う 
  ! wt_module モジュールで用いられているものと名前およびインターフェースが
  ! 共通になるように設計してある. したがって, wt_module を用いて構成された
  ! 3 次元モデルを帯状方向に一様な 2 次元軸対称モデルへと改造するには
  ! 次の手順が必要となる. 
  !
  ! 1. use 文での wt_module の引用を wt_zonal_module に変更する. 
  ! 2. 配列の大きさを経度方向格子点数 im -> 1 に, 
  !    水平波数を (nm+1)**2 -> nm+1 に変更する. 
  ! 3. DO 文で水平波数に関してループを回しているところを
  !    (nm+1)**2 -> nm+1 に変更する. 
  ! 4. gtool 出力の次元変数変更する. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (wt_, nmz_, nz_, xyz_, wz_, w_, xy_, x_, y_, z_, a_) は, 
  !   返す値の形を示している.
  !   wt_  :: スペクトルデータ(ルジャンドル多項式・チェビシェフ多項式変換)
  !   nmz_ :: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径)
  !   nz_  :: 水平スペクトルデータ(全波数 n, 動径)
  !   xyz_ :: 3 次元格子点データ(経度・緯度・動径)
  !   wz_  :: 水平スペクトル, 動径格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後 (wt_, xyz_, wz_, w_, xy_, x_, y_, z_, a_) は, 入力変数の
  !   形がスペクトルデータおよび格子点データであることを示している.
  !   _wt      :: スペクトルデータ
  !   _xyz     :: 3 次元格子点データ
  !   _xyz_xyz :: 2 つの3 次元格子点データ, ...
  !
  !=== 各データの種類の説明
  !
  ! * xyz : 3 次元格子点データ(経度・緯度・動径)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  !
  ! * wt : スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension(nm+1,0:lm). 
  !   * nm は球面調和函数の最大全波数, lm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン wt_Initial にてあらかじめ設定しておく. 
  !   * 水平スペクトルデータの格納のされ方は関数 l_nm, nm_l によって調べる
  !     ことができる.
  !
  ! * nmz : 水平スペクトルデータの並んだ 3 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数, 第 3 次元が動径座標を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wt_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * nz : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,0:km). 
  !   * 第 1 次元が水平全波数を表す. 
  !   * nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  !
  ! * wz : 水平スペクトル, 動径格子点データ.
  !   * 変数の種類と次元は real(8), dimension(nm+1,0:km).
  !
  ! * wt_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xyz_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * wz_ で始まる関数が返す値は水平スペクトル, 動径格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  ! 
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! wt_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, y_Lat, z_Rad          :: 格子点座標(緯度, 経度, 動径座標)を
  !                                 格納した1 次元配列
  ! x_Lon_Weight, y_Lat_Weight, z_Rad_Weight :: 重み座標を格納した 1 次元配列
  ! xyz_Lon, xyz_Lat, xyz_Rad    :: 格子点データの経度・緯度・動径座標(X,Y,Z)
  !                                 (格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! xyz_wt, wt_xyz :: スペクトルデータと 3 次元格子データの間の変換
  !                   (球面調和函数, チェビシェフ変換)
  ! xyz_wz, wz_xyz :: 3 次元格子データと水平スペクトル・動径格子データとの
  !                   間の変換 (球面調和函数)
  ! wz_wt, wt_wz   :: スペクトルデータと水平スペクトル・動径格子データとの
  !                   間の変換 (チェビシェフ変換)
  ! w_xy, xy_w     :: スペクトルデータと 2 次元水平格子データの間の変換
  !                   (球面調和函数変換) 
  ! az_at, at_az   :: 同時に複数個行う (チェビシェフ変換)格子データと
  !                   チェビシェフデータの間の変換を
  ! l_nm, nm_l     :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wt_DRad_wt          :: スペクトルデータに動径微分∂/∂r を作用させる
  ! wt_DivRad_wt        :: スペクトルデータに発散型動径微分
  !                        1/r^2 ∂/∂r r^2 = ∂/∂r + 2/r を作用させる
  ! wt_RotRad_wt        :: スペクトルデータに回転型動径微分
  !                        1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  ! wt_Lapla_wt         :: スペクトルデータにラプラシアンを作用させる
  ! xyz_GradLon_wt      :: スペクトルデータに勾配型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! xyz_GradLat_wt      :: スペクトルデータに勾配型緯度微分
  !                        1/r・∂/∂φを作用させる
  ! wt_DivLon_xyz       :: 格子データに発散型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! wt_DivLat_xyz       :: 格子データに発散型緯度微分
  !                        1/rcosφ・∂(g cosφ)/∂φを作用させる
  ! wt_Div_xyz_xyz_xyz  :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyz_Div_xyz_xyz_xyz :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyz_RotLon_wt_wt    :: ベクトル場の回転の経度成分を計算する
  ! xyz_RotLat_wt_wt    :: ベクトル場の回転の緯度成分を計算する
  ! wt_RotRad_xyz_xyz   :: ベクトル場の回転の動径成分を計算する
  !
  !==== トロイダルポロイダル計算用微分
  !
  ! wt_KxRGrad_wt            :: スペクトルデータに経度微分
  !                             k×r・▽ = ∂/∂λを作用させる
  ! xyz_KGrad_wt             :: スペクトルデータに軸方向微分
  !                             k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を
  !                             作用させる
  ! wt_L2_wt                 :: スペクトルデータに 
  !                             L2 演算子 = -水平ラプラシアンを作用させる
  ! wt_L2Inv_wt              :: スペクトルデータに
  !                             L2 演算子の逆 = -逆水平ラプラシアンを作用させる
  ! wt_QOperator_wt          :: スペクトルデータに演算子
  !                             Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) を作用させる
  ! wt_RadRot_xyz_xyz        :: ベクトル v の渦度と動径ベクトル r の内積
  !                             r・(▽×v) を計算する
  ! wt_RadRotRot_xyz_xyz_xyz :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wt_Potential2Vector      :: トロイダルポロイダルポテンシャルから
  !                             ベクトル場を計算する
  ! wt_Potential2Rotation    :: トロイダルポロイダルポテンシャルで表される
  !                             非発散ベクトル場の回転の各成分を計算する
  !
  !==== 非線形計算
  !
  ! wt_VGradV    ::  ベクトル v から v・▽v を計算する
  !
  !==== ポロイダル/トロイダルモデル用スペクトル解析
  !
  ! nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt   ::
  !     トロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  ! nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt   :: 
  !     ポロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  !
  !==== 境界値問題
  !
  ! wt_BoundariesTau, wt_BoundariesGrid, wt_Boundaries                   ::
  !     ディリクレ, ノイマン境界条件を適用する(タウ法, 選点法)
  ! wt_TorBoundariesTau, wt_TorBoundariesGrid, wt_TorBoundaries          ::
  !     速度トロイダルポテンシャルの境界条件を適用する(タウ法,選点法)
  ! wz_LaplaPol2Pol_wz, wt_LaplaPol2Pol_wt                               ::
  !     速度ポロイダルポテンシャルΦを▽^2Φから求める
  !     (入出力がそれぞれチェビシェフ格子点,チェビシェフ係数)
  ! wt_TorMagBoundariesTau, wt_TorMagBoundariesGrid, wt_TorMagBoundaries ::
  !     磁場トロイダルポテンシャルの境界条件を適用する(タウ法, 選点法)
  ! wt_PolMagBoundariesTau, wt_PolMagBoundariesGrid, wt_PolMagBoundaries ::
  !     磁場トロイダルポテンシャル境界の境界条件を適用する(タウ法, 選点法)
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
  !
  !==== 積分・平均(2 次元データ)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの水平(球面)積分および平均
  ! IntLonRad_xz, AvrLonRad_xz :: 2 次元(XZ)格子点データの経度動径積分
  !                               および平均
  ! IntLatRad_yz, AvrLatRad_yz :: 2 次元(YZ)格子点データの緯度動径(子午面)
  !                               積分および平均 
  ! y_IntLon_xy, y_AvrLon_xy   :: 水平 2 次元(球面)格子点データの経度方向
  !                               積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 水平2 次元(球面)格子点データの緯度方向積分
  !                               および平均
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 次元(XZ)格子点データの経度方向積分および
  !                               平均
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 次元(XZ)格子点データの動径方向積分および
  !                               平均
  ! z_IntLat_yz, z_AvrLat_yz   :: 2 次元(YZ)格子点データの緯度方向積分および
  !                               平均
  ! y_IntRad_yz, y_AvrRad_yz   :: 2 次元(YZ)格子点データの動径方向積分および
  !                               平均                  
  !
  !==== 積分・平均(1 次元データ)
  !
  ! IntLon_x, AvrLon_x  :: 1 次元(X)格子点データの経度方向積分および平均
  ! IntLat_y, AvrLat_y  :: 1 次元(Y)格子点データの緯度方向積分および平均
  ! IntRad_z, AvrRad_z  :: 1 次元(Z)格子点データの動径方向積分および平均
  !
  !==== 補間計算
  !
  ! Interpolate_wt :: スペクトルデータから任意の点の値を補間する. 
  ! 
  use dc_message
  use lumatrix
  use wa_zonal_module
  use at_module, z_RAD => g_X, z_RAD_WEIGHT => g_X_WEIGHT, &
                 at_az => at_ag, az_at => ag_at, &
                 t_Dr_t => t_Dx_t, at_Dr_at => at_Dx_at
  implicit none
  private

  public wt_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public z_Rad, z_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyz_Lon, xyz_Lat, xyz_Rad
  public wz_Rad
  public wt_VMiss

  public w_xy, xy_w
  public at_Dr_at, t_Dr_t, az_at, at_az
  public xyz_wt, wt_xyz, xyz_wz, wz_xyz, wz_wt, wt_wz
  public wt_DRad_wt, wt_DivRad_wt, wt_RotRad_wt, wt_Lapla_wt
  public xyz_GradLon_wt, xyz_gradlat_wt
  public wt_DivLon_xyz, wt_DivLat_xyz
  public wt_Div_xyz_xyz_xyz, xyz_Div_xyz_xyz_xyz
  public xyz_RotLon_wt_wt, xyz_RotLat_wt_wt, wt_RotRad_xyz_xyz

  public yz_IntLon_xyz, xz_IntLat_xyz, xy_IntRad_xyz
  public x_IntLatRad_xyz, y_IntLonRad_xyz, z_IntLonLat_xyz
  public IntLonLatRad_xyz

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public z_IntLat_yz, y_IntRad_yz, IntLatRad_yz
  public z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
  public IntLon_x, IntLat_y, IntRad_z

  public yz_AvrLon_xyz, xz_AvrLat_xyz, xy_AvrRad_xyz
  public x_AvrLatRad_xyz, y_AvrLonRad_xyz, z_AvrLonLat_xyz
  public AvrLonLatRad_xyz

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public z_AvrLat_yz, y_AvrRad_yz, AvrLatRad_yz
  public z_AvrLon_xz, x_AvrRad_xz, AvrLonRad_xz
  public AvrLon_x, AvrLat_y, AvrRad_z

  public wt_KxRGrad_wt, xyz_KGrad_wt, wt_L2_wt, wt_L2Inv_wt, wt_QOperator_wt
  public wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz
  public wt_Potential2vector, wt_Potential2Rotation
  public wt_VGradV

  public Interpolate_wt

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  interface wt_Boundaries
     module procedure wt_BoundariesTau
  end interface

  interface wt_TorBoundaries
     module procedure wt_TorBoundariesTau
  end interface

  interface wt_TorMagBoundaries
     module procedure wt_TorMagBoundariesTau
  end interface

  interface wt_PolMagBoundaries
     module procedure wt_PolMagBoundariesTau
  end interface

  integer            :: im=1, jm=32, km=16   ! 格子点の設定(経度, 緯度, 動径)
  integer            :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  real(8)            :: ri=0.0, ro=1.0       ! 球殻内外半径
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xyz_LON, xyz_LAT, xyz_RAD ! 座標
  real(8), dimension(:,:), allocatable   :: wz_RAD                    ! 座標

  real(8) :: wt_VMiss = -999.0        ! 欠損値

  save im, jm, km, nm, lm, ri, ro

  contains
  !--------------- 初期化 -----------------
   subroutine wt_Initial(i,j,k,n,l,r_in,r_out,np,wa_init)
     !
     ! スペクトル変換の格子点数, 波数, 動径座標の範囲を設定する.
     !
     ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
     ! しなければならない. 
     !
     ! np_in は wt_module との互換性のためにおいているだけである. 
     ! OPENMP 計算は実装されていない. 
     !
     integer,intent(in) :: i              ! 格子点数(経度λ)
     integer,intent(in) :: j              ! 格子点数(緯度φ)
     integer,intent(in) :: k              ! 格子点数(動径 r)
     integer,intent(in) :: n              ! 切断波数(水平全波数)
     integer,intent(in) :: l              ! 切断波数(動径波数)

     real(8),intent(in) :: r_in           ! 球殻内半径
     real(8),intent(in) :: r_out          ! 球殻外半径

     integer,intent(in), optional :: np   ! OPENMP での最大スレッド数
     logical,intent(in), optional :: wa_init   ! wa_initial スイッチ

     logical    :: wa_initialize=.true.   ! wa_initial スイッチ
     
     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ri = r_in ; ro = r_out

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

     call at_Initial(km,lm,r_in,r_out)

     allocate(xyz_Lon(0:im-1,1:jm,0:km))
     allocate(xyz_Lat(0:im-1,1:jm,0:km))
     allocate(xyz_Rad(0:im-1,1:jm,0:km))

     allocate(wz_Rad(nm+1,0:km))

     xyz_Lon = spread(xy_Lon,3,km+1)
     xyz_Lat = spread(xy_Lat,3,km+1)
     xyz_Rad = spread(spread(z_Rad,1,jm),1,im)

     wz_Rad = spread(z_Rad,1,nm+1)

     z_Rad_Weight = z_Rad_Weight * z_Rad**2       ! r^2 dr の積分重み

     call MessageNotify('M','wt_initial', &
          'wt_zonal_module (2009/07/31) is initialized')

   end subroutine wt_initial

  !--------------- 基本変換 -----------------

    function xyz_wt(wt)
      !
      ! スペクトルデータから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(nm+1,0:lm), intent(in)          :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyz_wt
      !(out) 3 次元経度緯度動径格子点データ

      xyz_wt = xya_wa(az_at(wt))

    end function xyz_wt

    function wt_xyz(xyz)
      !
      ! 3 次元格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension(nm+1,0:lm)                    :: wt_xyz
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ

      wt_xyz = at_az(wa_xya(xyz))

    end function wt_xyz

    function xyz_wz(wz)
      !
      ! 水平スペクトル・動径格子点データから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(nm+1,0:km), intent(in)          :: wz
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ
      real(8), dimension(0:im-1,1:jm,0:km)               :: xyz_wz
      !(out) 3 次元経度緯度動径格子点データ

      xyz_wz = xya_wa(wz)

    end function xyz_wz

    function wz_xyz(xyz)
      !
      ! 3 次元格子データから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension(nm+1,0:km)                      :: wz_xyz
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ

      wz_xyz = wa_xya(xyz)

    end function wz_xyz

    function wz_wt(wt)
      !
      ! スペクトルデータから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
      real(8), dimension(nm+1,0:km)             :: wz_wt
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ

      wz_wt = az_at(wt)

    end function wz_wt

    function wt_wz(wz)
      !
      ! 水平スペクトル・動径格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension(nm+1,0:km), intent(in) :: wz
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ
      real(8), dimension(nm+1,0:lm)             :: wt_wz
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ

      wt_wz = at_az(wz)

    end function wt_wz

  !--------------- 微分計算 -----------------
    function wt_DRad_wt(wt)
      !
      ! 入力スペクトルデータに動径微分 ∂/∂r を作用する.
      !
      ! スペクトルデータの動径微分とは, 対応する格子点データに動径微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_DRad_wt
      !(in) 動径微分された2 次元球面調和函数チェビシェフスペクトルデータ

      wt_DRad_wt = at_Dr_at(wt)

    end function wt_DRad_wt

    function wt_DivRad_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_DivRad_wt
      !(out) 発散型動径微分を作用された 2 次元スペクトルデータ

      wt_DivRad_wt = wt_Drad_wt(wt) + wt_wz(2/wz_rad*wz_wt(wt))


    end function wt_DivRad_wt

    function wt_RotRad_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_RotRad_wt
      !(out) 回転型動径微分を作用された 2 次元スペクトルデータ

      wt_RotRad_wt = wt_Drad_wt(wt) + wt_wz(1/wz_Rad*wz_wt(wt))

    end function wt_RotRad_wt

    function wt_Lapla_wt(wt)
      ! 入力スペクトルデータにラプラシアン
      !
      !     ▽^2 =   1/r^2 cos^2φ・∂^2/∂λ^2 
      !            + 1/r^2 cosφ・∂/∂φ(cosφ∂/∂φ) 
      !            + 1/r^2 ∂/∂r (r^2 ∂/∂r) 
      !
      ! を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_Lapla_wt
      !(out) ラプラシアンを作用された 2 次元スペクトルデータ

      wt_Lapla_wt = wt_DivRad_wt(wt_Drad_wt(wt)) &
                   + wt_wz(wz_wt(wa_Lapla_wa(wt))/wz_Rad**2)

    end function wt_Lapla_wt

    function xyz_GradLon_wt(wt)
      !
      ! スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λ
      ! を作用させる.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)   :: xyz_GradLon_wt
      !(out) 勾配型経度微分を作用された 2 次元スペクトルデータ

      xyz_GradLon_wt = xya_GradLon_wa(wz_wt(wt))/xyz_Rad

    end function xyz_GradLon_wt

    function xyz_GradLat_wt(wt) 
      !
      ! スペクトルデータに勾配型経度微分 1/r ∂/∂φ を作用させる.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)    :: xyz_GradLat_wt
      !(out) 勾配型緯度微分を作用された 2 次元スペクトルデータ

      xyz_GradLat_wt = xya_GradLat_wa(wz_wt(wt))/xyz_Rad

    end function xyz_GradLat_wt

    function wt_DivLon_xyz(xyz)
      ! 
      ! 格子点データに発散型経度微分 1/rcosφ・∂/∂λ を作用させた
      ! スペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension(nm+1,0:lm)       :: wt_DivLon_xyz
      !(out) 発散型経度微分を作用された 2 次元スペクトルデータ

      wt_DivLon_xyz = wt_wz(wa_DivLon_xya(xyz/xyz_Rad))

    end function wt_DivLon_xyz

    function wt_DivLat_xyz(xyz)
      !
      ! 格子データに発散型緯度微分 1/rcosφ・∂(f cosφ)/∂φ を
      ! 作用させたスペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in)   :: xyz
      !(in) 3 次元経度緯度動径格子点データ
      real(8), dimension(nm+1,0:lm)       :: wt_DivLat_xyz
      !(out) 発散型緯度微分を作用された 2 次元スペクトルデータ

      wt_DivLat_xyz = wt_wz(wa_divlat_xya(xyz/xyz_Rad))

    end function wt_DivLat_xyz

    function wt_Div_xyz_xyz_xyz(xyz_Vlon,xyz_Vlat,xyz_Vrad)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) ベクトル場の経度成分
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(nm+1,0:lm)     :: wt_Div_xyz_xyz_xyz
      !(out) ベクトル場の発散

      wt_Div_xyz_xyz_xyz =   wt_DivLon_xyz(xyz_Vlon) &
                           + wt_DivLat_xyz(xyz_Vlat) &
                           + wt_DivRad_wt(wt_xyz(xyz_Vrad))

    end function wt_Div_xyz_xyz_xyz

    function xyz_Div_xyz_xyz_xyz(xyz_Vlon,xyz_Vlat,xyz_Vrad)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,0:km)             :: xyz_Div_xyz_xyz_xyz
      !(out) ベクトル場の発散

      xyz_Div_xyz_xyz_xyz &
           = xyz_Rad/cos(xyz_Lat) &
                * xyz_wt(wt_Div_xyz_xyz_xyz(xyz_VLon*cos(xyz_Lat)/xyz_Rad,  &
                                            xyz_VLat*cos(xyz_Lat)/xyz_Rad,  &
                                            xyz_VRad*cos(xyz_Lat)/xyz_Rad ))&
             + xyz_VLat*tan(xyz_Lat)/xyz_Rad &
             + xyz_VRad/xyz_Rad

    end function xyz_Div_xyz_xyz_xyz

    function xyz_RotLon_wt_wt(wt_Vrad,wt_Vlat) 
      !
      ! ベクトル場の動径成分, 緯度成分である第 1, 2 引数 Vrad, Vlat から
      ! 回転の経度成分 
      !
      !    1/r ∂Vrad/∂φ-1/r ∂(r Vlat)/∂r を計算する.
      !
      ! を計算する
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_RotLon_wt_wt
      !(out) ベクトル場の回転の経度成分

        xyz_RotLon_wt_wt =   xyz_GradLat_wt(wt_Vrad) &
                           - xyz_wt(wt_RotRad_wt(wt_Vlat))

    end function xyz_RotLon_wt_wt

    function xyz_RotLat_wt_wt(wt_Vlon,wt_Vrad) 
      !
      ! ベクトル場の経度成分, 動径成分である第 1, 2 引数 Vlon, Vrad から
      ! 回転の緯度成分 
      !
      !    1/r ∂(r Vlon)/∂r - 1/rcosφ・∂Vrad/∂λ
      !
      ! を計算する.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_RotLat_wt_wt
      !(out) ベクトル場の回転の緯度成分

        xyz_RotLat_wt_wt =   xyz_wt(wt_RotRad_wt(wt_Vlon)) &
                           - xyz_GradLon_wt(wt_Vrad) 

    end function xyz_RotLat_wt_wt

    function wt_RotRad_xyz_xyz(xyz_Vlat,xyz_Vlon) 
      !
      ! ベクトルの緯度成分, 経度成分である第 1, 2 引数 Vlat, Vlon に対して
      ! ベクトル場の回転の動径成分 
      !
      !    1/rcosφ・∂Vlat/∂λ - 1/rcosφ・∂(Vlon cosφ)/∂φ
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(nm+1,0:lm)     :: wt_RotRad_xyz_xyz
      !(out) ベクトル場の回転の動径成分

        wt_RotRad_xyz_xyz =   wt_DivLon_xyz(xyz_Vlat) &
                            - wt_DivLat_xyz(xyz_Vlon)

    end function wt_RotRad_xyz_xyz

  !--------------- 積分計算 -----------------
    !----(入力データ xyz)---
    function yz_IntLon_xyz(xyz)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,0:km)  :: yz_IntLon_xyz
      !(out) 経度方向(帯状)積分された 2 次元子午面格子点データ

      integer :: i

      yz_IntLon_xyz = 0.0d0
      do i=0,im-1
         yz_IntLon_xyz(:,:) = yz_IntLon_xyz(:,:) &
                       + xyz(i,:,:) * x_Lon_Weight(i)
      enddo
    end function yz_IntLon_xyz

    function xz_IntLat_xyz(xyz)
      !
      ! 3 次元格子点データの緯度方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xz_IntLat_xyz        
      !(out) 緯度積分された 2 次元緯度動径格子点データ.
      ! 緯度円格子点データ

      integer :: j

      xz_IntLat_xyz = 0.0d0
      do j=1,jm
         xz_IntLat_xyz(:,:) = xz_IntLat_xyz(:,:) &
                       + xyz(:,j,:) * y_Lat_Weight(j)
      enddo
    end function xz_IntLat_xyz

    function xy_IntRad_xyz(xyz)  ! 動径積分
      !
      ! 3 次元格子点データの動径方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dr を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyz
      !(out) 動径積分された 2 次元経度緯度(水平, 球面)格子点データ

      integer :: k

      xy_IntRad_xyz = 0.0d0
      do k=0,km
         xy_IntRad_xyz(:,:) = xy_IntRad_xyz(:,:) &
                       + xyz(:,:,k) * z_Rad_Weight(k) 
      enddo
    end function xy_IntRad_xyz

    function x_IntLatRad_xyz(xyz)
      !
      ! 3 次元格子点データの緯度動径(子午面)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyz
      !(out) 緯度動径(子午面)積分された 1 次元経度格子点データ

      integer :: j, k

      x_IntLatRad_xyz = 0.0D0
      do k=0,km
         do j=1,jm
            x_IntLatRad_xyz = x_IntLatRad_xyz &
                 + xyz(:,j,k) * y_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo
    end function x_IntLatRad_xyz

    function y_IntLonRad_xyz(xyz)
      !
      ! 3 次元格子点データの経度動径(緯度円)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dλdr を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm)       :: y_IntLonRad_xyz
      !(out) 経度動径(緯度円)積分された 1 次元緯度格子点データ

      integer :: i, k

      y_IntLonRad_xyz = 0
      do k=0,km
         do i=0,im-1
            y_IntLonRad_xyz = y_IntLonRad_xyz &
                 + xyz(i,:,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function y_IntLonRad_xyz

    function z_IntLonLat_xyz(xyz)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: z_IntLonLat_xyz
      !(out) 緯度経度(水平, 球面)積分された 1 次元動径格子点データ

      integer :: i, j

      z_IntLonLat_xyz = 0
      do j=1,jm
         do i=0,im-1
            z_IntLonLat_xyz = z_IntLonLat_xyz &
                 + xyz(i,j,:) * x_Lon_Weight(i) * y_Lat_Weight(j)
         enddo
      enddo
    end function z_IntLonLat_xyz

    function IntLonLatRad_xyz(xyz) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2cosφ dλdφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz 
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: IntLonLatRad_xyz 
      !(out) 全球積分値

      integer :: i, j, k

      IntLonLatRad_xyz = 0
      do k=0,km
         do j=1,jm
            do i=0,im-1
               IntLonLatRad_xyz = IntLonLatRad_xyz &
                    + xyz(i,j,k) * x_Lon_Weight(i) &
                         * y_Lat_Weight(j) * z_Rad_Weight(k)
            enddo
         enddo
      enddo
    end function IntLonLatRad_xyz

    !----(入力データ yz)---
    function z_IntLat_yz(yz)  ! 緯度積分
      !
      ! 2 次元(YZ)格子点データの緯度方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: z_IntLat_yz
      !(out) 緯度積分された 1 次元動径格子点データ

      integer :: j

      z_IntLat_yz = 0.0d0
      do j=1,jm
         z_IntLat_yz(:) = z_IntLat_yz(:) + yz(j,:) * y_Lat_Weight(j)
      enddo
    end function z_IntLat_yz

    function y_IntRad_yz(yz)  ! 動径積分
      !
      ! 2 次元(YZ)格子点データの動径方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) r^2dr を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(1:jm)  :: y_IntRad_yz
      !(out) 動径積分された 1 次元緯度格子点データ

      integer :: k

      y_IntRad_yz = 0.0d0
      do k=0,km
         y_IntRad_yz(:) = y_IntRad_yz(:) &
                       + yz(:,k) * z_Rad_Weight(k) 
      enddo
    end function y_IntRad_yz

    function IntLatRad_yz(yz)
      !
      ! 2 次元(YZ)格子点データの緯度動径積分(子午面)および平均
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2cosφ dφdr を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: IntLatRad_yz
      !(out) 積分値
      integer :: j, k

      IntLatRad_yz = 0
      do k=0,km
         do j=1,jm
            IntLatRad_yz = IntLatRad_yz &
                 + yz(j,k) * y_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo
    end function IntLatRad_yz

    !----(入力データ xz)---
    function z_IntLon_xz(xz)
      !
      ! 2 次元(XZ)格子点データの経度方向積分.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r)dλ を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:km)  :: z_IntLon_xz
      !(out) 経度積分された 1 次元動径格子点データ

      integer :: i

      z_IntLon_xz = 0.0d0
      do i=0,im-1
         z_IntLon_xz(:) = z_IntLon_xz(:) + xz(i,:) * x_Lon_Weight(i)
      enddo

    end function z_IntLon_xz

    function x_IntRad_xz(xz)
      !
      ! 2 次元(XZ)格子点データの動径方向域積分.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r) r^2dr を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1)  :: x_IntRad_xz
      !(out) 動径積分された 1 次元経度格子点データ

      integer :: k

      x_IntRad_xz = 0.0d0
      do k=0,km
         x_IntRad_xz(:) = x_IntRad_xz(:) &
                       + xz(:,k) * z_Rad_Weight(k) 
      enddo

    end function x_IntRad_xz

    function IntLonRad_xz(xz)  ! 経度動径(緯度円)積分
      !
      ! 2 次元(XZ)格子点データの経度動径積分
      !
      ! 2 次元データ f(λ,r) に対して∫f(λ,r) r^2dλdr を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 次元緯度動径格子点データ

      real(8)                                 :: IntLonRad_xz
      !(out) 積分値

      integer :: i, k

      IntLonRad_xz = 0
      do k=0,km
         do i=0,im-1
            IntLonRad_xz = IntLonRad_xz &
                 + xz(i,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function IntLonRad_xz

    !----(入力データ z)---
    function IntRad_z(z)  ! 動径積分
      !
      ! 1 次元(Z)格子点データの動径方向域積分.
      !
      ! 1 次元データ f(r) に対して ∫f(r) r^2dr を計算する.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 次元動径格子点データ

      real(8)                              :: IntRad_z
      !(out) 積分値

      integer :: k

      IntRad_z = 0.0d0
      do k=0,km
         IntRad_z = IntRad_z + z(k) * z_Rad_Weight(k) 
      enddo
    end function IntRad_z

  !--------------- 平均計算 -----------------
    !----(入力データ xyz)---
    function yz_AvrLon_xyz(xyz)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ/2π を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,0:km)  :: yz_AvrLon_xyz
      !(out) 経度方向(帯状)平均された 2 次元子午面格子点データ

      yz_AvrLon_xyz = yz_IntLon_xyz(xyz)/sum(x_Lon_Weight)

    end function yz_AvrLon_xyz

    function xz_AvrLat_xyz(xyz)  ! 緯度積分
      !
      ! 3 次元格子点データの緯度方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)cosφ dφ/2 を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xz_AvrLat_xyz
      !(out) 緯度平均された 2 次元緯度動径格子点データ

      xz_AvrLat_xyz = xz_IntLat_xyz(xyz)/sum(y_Lat_Weight)

    end function xz_AvrLat_xyz

    function xy_AvrRad_xyz(xyz)
      !
      ! 3 次元格子点データの動径方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して 
      !
      !    ∫f(λ,φ,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyz          
      !(out) 動径平均された 2 次元経度緯度(水平, 球面)格子点データ
      ! 水平格子点データ

      xy_AvrRad_xyz = xy_IntRad_xyz(xyz)/sum(z_Rad_Weight)

    end function xy_AvrRad_xyz

    function x_AvrLatRad_xyz(xyz)  ! 緯度動径(子午面)積分
      !
      ! 3 次元格子点データの緯度動径(子午面)平均
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xyz
      !(out) 緯度動径(子午面)平均された 1 次元経度格子点データ

      x_AvrLatRad_xyz = x_IntLatRad_xyz(xyz) &
                   /( sum(y_Lat_Weight)*sum(z_Rad_Weight) )

    end function x_AvrLatRad_xyz

    function y_AvrLonRad_xyz(xyz)  ! 経度動径(緯度円)積分
      !
      ! 3 次元格子点データの経度動径(緯度円)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2dλdr /(2π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm)       :: y_AvrLonRad_xyz
      !(out) 経度動径(緯度円)平均された 1 次元緯度格子点データ

      y_AvrLonRad_xyz = y_IntLonRad_xyz(xyz) &
                 /(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function y_AvrLonRad_xyz

    function z_AvrLonLat_xyz(xyz)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ /4π 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: z_AvrLonLat_xyz
      !(out) 緯度経度(水平, 球面)平均された 1 次元動径格子点データ

      z_AvrLonLat_xyz = z_IntLonLat_xyz(xyz) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function z_AvrLonLat_xyz

    function AvrLonLatRad_xyz(xyz) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dλdφdr /(4π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: AvrLonLatRad_xyz
      !(out) 全球平均値

      AvrLonLatRad_xyz = IntLonLatRad_xyz(xyz) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(z_Rad_Weight))

    end function AvrLonLatRad_xyz

    !----(入力データ yz)---
    function z_AvrLat_yz(yz)
      !
      ! 2 次元(YZ)格子点データの緯度方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) cosφ dφ/2 を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: z_AvrLat_yz
      !(out) 緯度平均された 1 次元動径格子点データ

      z_AvrLat_yz = z_IntLat_yz(yz)/sum(y_Lat_Weight)

    end function z_AvrLat_yz

    function y_AvrRad_yz(yz)
      !
      ! 2 次元(YZ)格子点データの動径方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(1:jm)  :: y_AvrRad_yz
      !(out) 動径平均された 1 次元緯度格子点データ

      y_AvrRad_yz = y_IntRad_yz(yz)/sum(z_Rad_Weight)

    end function y_AvrRad_yz

    function AvrLatRad_yz(yz)  ! 緯度動径(子午面)積分
      !
      ! 2 次元(YZ)格子点データの緯度動径(子午面)平均
      !
      ! 2 次元データ f(φ,r) に対して
      !
      !    ∫f(φ,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(1:jm,0:km), intent(in) :: yz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: AvrLatRad_yz
      !(out) 平均値

      AvrLatRad_yz = IntLatRad_yz(yz)/(sum(y_Lat_Weight)*sum(z_Rad_Weight))

    end function AvrLatRad_yz

    !----(入力データ xz)---
    function z_AvrLon_xz(xz)  ! 経度(帯状)積分
      !
      ! 2 次元(XZ)格子点データの経度方向平均.
      !
      ! 2 次元データ f(λ,r) に対して ∫f(λ,r)dλ/2π を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:km)  :: z_AvrLon_xz 
      !(out) 経度平均された 1 次元動径格子点データ

      z_AvrLon_xz = z_IntLon_xz(xz)/sum(x_Lon_Weight)

    end function z_AvrLon_xz

    function x_AvrRad_xz(xz)  ! 動径積分
      !
      ! 2 次元(XZ)格子点データの動径方向域平均.
      !
      ! 2 次元データ f(λ,r) に対して
      !
      !   ∫f(λ,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1)  :: x_AvrRad_xz
      !(out) 動径平均された 1 次元経度格子点データ

      x_AvrRad_xz = x_IntRad_xz(xz)/sum(z_Rad_Weight)

    end function x_AvrRad_xz

    function AvrLonRad_xz(xz)  ! 経度動径(緯度円)積分
      !
      ! 2 次元(XZ)格子点データの経度動径平均
      !
      ! 2 次元データ f(λ,r) に対して 
      ! 
      !    ∫f(λ,r) r^2dλdr /(2π(r[o]^3-r[i]^3)/3)
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,0:km), intent(in) :: xz    
      ! (in) 2 次元格子点データ
      real(8)                                 :: AvrLonRad_xz      
      ! 積分値

      AvrLonRad_xz = IntLonRad_xz(xz)/(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function AvrLonRad_xz

    !----(入力データ z)---
    function AvrRad_z(z)
      !
      ! 1 次元(Z)格子点データの動径方向域平均.
      !
      ! 1 次元データ f(r) に対して ∫f(r) r^2dr /((r[o]^3-r[i]^3)/3) を
      ! 計算する.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 次元動径格子点データ
      real(8)                              :: AvrRad_z
      !(out) 平均値

      AvrRad_z = IntRad_z(z)/sum(z_Rad_Weight)

    end function AvrRad_z

  !--------------- ポロイダル/トロイダルモデル用微分 -----------------

    function wt_KxRGrad_wt(wt)
      !
      ! 入力スペクトルデータに経度微分 k×r・▽ = ∂/∂λを作用する.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_KxRGrad_wt
      !(out) 経度微分を作用された 2 次元スペクトルデータ

      wt_KxRGrad_wt =  wa_Dlon_wa(wt)

    end function wt_KxRGrad_wt

    function xyz_KGrad_wt(wt)    ! k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r
      !
      ! 入力スペクトルデータに対応する格子データに軸方向微分 
      !
      !    k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r 
      !
      ! を作用させた格子データが返される. 
      ! ここでベクトル k は球の中心から北極向きの単位ベクトルである.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,0:km)                     :: xyz_KGrad_wt
      !(out) 軸方向微分を作用された 2 次元スペクトルデータ

      xyz_KGrad_wt =  cos(xyz_Lat)*xyz_GradLat_wt(wt) &
                    + sin(xyz_Lat)*xyz_wt(wt_Drad_wt(wt))

    end function xyz_KGrad_wt

    function wt_L2_wt(wt)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_L2_wt
      !(out) L^2 演算子を作用された 2 次元スペクトルデータ

      wt_L2_wt = -wa_Lapla_wa(wt)

    end function wt_L2_wt

    function wt_L2Inv_wt(wt)
      !
      ! 入力スペクトルデータに L^2 演算子の逆演算(-逆水平ラプラシアン)を
      ! 作用する.
      !
      ! スペクトルデータに L^2 演算子を作用させる関数 wt_L2_wt の逆計算を
      ! 行う関数である.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_L2Inv_wt
      !(out) L^2 演算子の逆演算を作用された 2 次元スペクトルデータ

      wt_L2Inv_wt = -wa_LaplaInv_wa(wt)

    end function wt_L2Inv_wt

    function wt_QOperator_wt(wt)
      !
      ! 入力スペクトルデータに対応する格子点データに演算子 
      !
      !    Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) 
      !
      ! を作用させたデータのスペクトル変換が返される.
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(nm+1,0:lm)             :: wt_QOperator_wt
      !(out) Q 演算子を作用された 2 次元スペクトルデータ

      wt_QOperator_wt = &
             wt_xyz(xyz_KGrad_wt(wt) - xyz_KGrad_wt(wt_L2_wt(wt))/2) &
           - wt_L2_wt(wt_xyz(xyz_KGrad_wt(wt)))/2

    end function wt_QOperator_wt

    function wt_RadRot_xyz_xyz(xyz_VLON,xyz_VLAT)  ! r・(▽×v)
      !
      ! ベクトルの渦度と動径ベクトルの内積 r・(▽×v) を計算する.
      !
      ! 第 1, 2 引数(v[λ], v[φ])がそれぞれベクトルの経度成分, 緯度成分を表す.
      !
      !    r・(▽×v) = 1/cosφ・∂v[φ]/∂λ - 1/cosφ・∂(v[λ] cosφ)/∂φ
      !
      ! のスペクトル データが返される.
      !
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(nm+1,0:lm)     :: wt_RadRot_xyz_xyz
      !(out) ベクトルの渦度と動径ベクトルの内積

      wt_RadRot_xyz_xyz = wt_wz(wa_DivLon_xya(xyz_VLAT) &
                                - wa_DivLat_xya(xyz_VLON))
      
    end function wt_RadRot_xyz_xyz

    function wt_RadRotRot_xyz_xyz_xyz(xyz_VLON,xyz_VLAT,xyz_VRAD) 
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(in) :: xyz_VRAD
      !(in) ベクトルの動径成分

      real(8), dimension(nm+1,0:lm)     :: wt_RadRotRot_xyz_xyz_xyz
      !(out) ベクトル v の r・(▽×▽×v) 

      wt_RadRotRot_xyz_xyz_xyz = &
               wt_RotRad_wt(wt_wz( &
                   (wa_DivLon_xya(xyz_VLON)+ wa_DivLat_xya(xyz_VLAT)))) &
             + wt_L2_wt(wt_xyz(xyz_VRAD/xyz_RAD))

    end function wt_RadRotRot_xyz_xyz_xyz

    subroutine wt_Potential2Vector(&
         xyz_VLON,xyz_VLAT,xyz_VRAD,wt_TORPOT,wt_POLPOT)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψr) + ▽x▽x(Φr) 
      !
      ! の各成分を計算する
      !
      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLON
      !(out) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VLAT
      !(out) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km)     :: xyz_VRAD
      !(out) ベクトル場の動径成分

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      xyz_VLON =   xyz_RAD * xyz_GradLat_wt(wt_TORPOT) &
                 + xya_GradLon_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xyz_VLAT = - xyz_RAD * xyz_GradLon_wt(wt_TORPOT) &
                 + xya_GradLat_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xyz_VRAD = xyz_wt(wt_L2_wt(wt_POLPOT))/xyz_RAD

    end subroutine wt_Potential2Vector

    subroutine wt_Potential2Rotation(&
       xyz_RotVLON,xyz_RotVLAT,xyz_RotVRAD,wt_TORPOT,wt_POLPOT)
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
      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVLON
      !(out) 回転の経度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVLAT
      !(out) 回転の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km), intent(OUT) :: xyz_RotVRAD
      !(out) 回転の動径成分

      ! 入力ベクトル場を表すポテンシャル
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      call wt_Potential2Vector( &
           xyz_RotVLON,xyz_RotVLAT,xyz_RotVRAD, &
           -wt_Lapla_wt(wt_POLPOT), wt_TORPOT)

    end subroutine wt_Potential2Rotation

 !------------------- 非線形項計算 ----------------------
    subroutine wt_VGradV(xyz_VGRADV_LON,xyz_VGRADV_LAT,xyz_VGRADV_RAD, &
                          xyz_VLON,xyz_VLAT,xyz_VRAD )
      !
      ! ベクトル場の v・▽v を計算する.
      !
      ! ベクトル場 v=(v[λ],v[φ],v[r]) に対するv・▽vの各成分は
      ! 次のように計算される.
      !
      !   (v・▽v)[λ] = ▽・(v[λ]v) + v[λ]v[r]/r - v[λ]v[φ]tan(φ)/r
      !   (v・▽v)[φ] = ▽・(v[φ]v) + v[φ]v[r]/r - v[λ]^2tan(φ)/r
      !   (v・▽v)[r] = ▽・(v[r]v) + (v[λ]^2+v[φ]^2)/r
      !
      ! 非発散速度場に対してはポテンシャルから wt_Potential2Rotation を
      ! 用いて回転を計算し, 恒等式 v・▽v = ▽(v[2^/2) - vx▽xv を
      ! 用いる方がよいだろう.
      !
      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LON
      !(out) (v・▽v) 経度成分

      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_LAT
      !(out) (v・▽v) 緯度成分

      real(8), dimension(0:im-1,1:jm,0:km),intent(out)   :: xyz_VGRADV_RAD
      !(out) (v・▽v) 動径成分

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLON
      !(in) ベクトル場 v の経度成分

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VLAT
      !(in) ベクトル場 v の緯度成分

      real(8), dimension(0:im-1,1:jm,0:km),intent(in)    :: xyz_VRAD
      !(in) ベクトル場 v の動径成分

      xyz_VGRADV_LON = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VLON * xyz_VLON, xyz_VLON*xyz_VLAT, xyz_VLON*xyz_VRAD ) &
            + xyz_VLON*xyz_VRAD/xyz_RAD              &
            - xyz_VLON*xyz_VLAT*tan(xyz_LAT)/xyz_RAD 

      xyz_VGRADV_LAT = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VLAT*xyz_VLON, xyz_VLAT*xyz_VLAT, xyz_VLAT*xyz_VRAD ) &
            + xyz_VLAT*xyz_VRAD/xyz_RAD        &
            + xyz_VLON**2*tan(xyz_LAT)/xyz_RAD 

      xyz_VGRADV_RAD = &
              xyz_Div_xyz_xyz_xyz( &
                  xyz_VRAD*xyz_VLON, xyz_VRAD*xyz_VLAT, xyz_VRAD*xyz_VRAD ) &
            - (xyz_VLON**2 + xyz_VLAT**2)/xyz_RAD 

    end subroutine wt_VGradV

  !--------------- 補間計算 -----------------
    function Interpolate_wt(wt_data,alon,alat,arad)
      !
      ! 緯度 alon, 経度 alat 動径 arad における関数値を
      ! その球面調和変換係数 wa_data から補間計算する
      !
      real(8), intent(IN) :: wt_data(nm+1,0:km)       ! スペクトルデータ
      real(8), intent(IN) :: alon                     ! 補間する位置(経度)
      real(8), intent(IN) :: alat                     ! 補間する位置(緯度)
      real(8), intent(IN) :: arad                     ! 補間する位置(動径)
      real(8) :: Interpolate_wt                       ! 補間した値
      
      Interpolate_wt = &
           Interpolate_w(a_Interpolate_at(wt_data,arad),alon,alat)

    end function Interpolate_wt

  !--------------- ポロイダル/トロイダルモデル用スペクトル解析 ----------------

    function nmz_ToroidalEnergySpectrum_wt(wt_TORPOT)
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
      !    wt_VMiss によって設定できる (初期値は -999.0)
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmz_ToroidalEnergySpectrum_wt
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension(nm+1,0:km) ::wz_DATA   ! 作業領域
      integer :: n, m

      nmz_ToroidalEnergySpectrum_wt = wt_VMiss

      wz_DATA = wz_wt(wt_TORPOT)
      do n=0,nm
         do m=1,n
            nmz_ToroidalEnergySpectrum_wt(n,m,:) = 0.0D0
            nmz_ToroidalEnergySpectrum_wt(n,-m,:) = 0.0D0
         enddo
         nmz_ToroidalEnergySpectrum_wt(n,0,:) &
              = 0.5 * n*(n+1)* (4*pi) * z_Rad**2 &
                * wz_DATA(l_nm(n,0),:)**2
      enddo

    end function nmz_ToroidalEnergySpectrum_wt

    function nz_ToroidalEnergySpectrum_wt(wt_TORPOT)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,0:km) :: nz_ToroidalEnergySpectrum_wt 
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension(nm+1,0:km) ::wz_DATA   ! 作業領域
      integer :: n, m

      wz_DATA = wz_wt(wt_TORPOT)
      do n=0,nm
         nz_ToroidalEnergySpectrum_wt(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * z_Rad**2 * wz_Data(l_nm(n,0),:)**2
      enddo

    end function nz_ToroidalEnergySpectrum_wt

    function nmz_PoloidalEnergySpectrum_wt(wt_POLPOT)
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
      !    欠損値の値はモジュール変数 wt_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,0:km) :: nmz_PoloidalEnergySpectrum_wt 
      !(out) エネルギースペクトルポロイダル成分


      real(8), dimension(nm+1,0:km) ::wz_DATA1   ! 作業領域
      real(8), dimension(nm+1,0:km) ::wz_DATA2   ! 作業領域
      integer :: n, m

      nmz_PoloidalEnergySpectrum_wt = wt_VMiss

      wz_Data1 = wz_wt(wt_POLPOT)
      wz_Data2 = wz_Rad*wz_wt(wt_DRad_wt(wt_POLPOT)) &    ! d(rφ)/dr
               + wz_wt(wt_POLPOT)                         ! = rdφ/dr+φ


      do n=0,nm
         do m=1,n
            nmz_PoloidalEnergySpectrum_wt(n,m,:) = 0.0D0
            nmz_PoloidalEnergySpectrum_wt(n,-m,:) = 0.0D0
         enddo
         nmz_PoloidalEnergySpectrum_wt(n,0,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wz_Data2(l_nm(n,0),:)**2  &
                   + n*(n+1)*wz_Data1(l_nm(n,0),:)**2 )
      enddo

    end function nmz_PoloidalEnergySpectrum_wt

    function nz_PoloidalEnergySpectrum_wt(wt_POLPOT)
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
      real(8), dimension(nm+1,0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,0:km) :: nz_PoloidalEnergySpectrum_wt
      !(out) エネルギースペクトルポロイダル成分

      real(8), dimension(nm+1,0:km) ::wz_DATA1   ! 作業領域
      real(8), dimension(nm+1,0:km) ::wz_DATA2   ! 作業領域
      integer :: n, m

      wz_Data1 = wz_wt(wt_POLPOT)
      wz_Data2 = wz_Rad*wz_wt(wt_DRad_wt(wt_POLPOT)) &    ! d(rφ)/dr
               + wz_wt(wt_POLPOT)                         ! = rdφ/dr+φ

      do n=0,nm
         nz_PoloidalEnergySpectrum_wt(n,:) = &
              + 0.5* n*(n+1)* (4*pi) &
              *( wz_Data2(l_nm(n,0),:)**2  &
                + n*(n+1)*wz_Data1(l_nm(n,0),:)**2 )
      enddo

    end function nz_PoloidalEnergySpectrum_wt


  !--------------- 境界値問題 -----------------

    subroutine wt_BoundariesTau(wt,values,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! Chebyshev 空間での境界条件適用(タウ法)
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を
      ! 定める方法をとっている(タウ法).
      !
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !     省略時は値/勾配 0 となる. 

      character(len=2), intent(in), optional             :: cond
              !(in) 境界条件. 省略時は 'DD'
              !        DD : 両端ディリクレ条件
              !        DN : 上端ディリクレ, 下端ノイマン条件
              !        ND : 上端ノイマン, 下端ディリクレ条件
              !        NN : 両端ノイマン条件

      if (.not. present(cond)) then
         if (present(values)) then
            call at_BoundariesTau_DD(wt,values)
         else
            call at_BoundariesTau_DD(wt)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesTau_NN(wt,values)
         else
            call at_BoundariesTau_NN(wt)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesTau_DN(wt,values)
         else
            call at_BoundariesTau_DN(wt)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesTau_ND(wt,values)
         else
            call at_BoundariesTau_ND(wt)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesTau_DD(wt,values)
         else
            call at_BoundariesTau_DD(wt)
         endif
      case default
         call MessageNotify('E','wt_BoundariesTau','B.C. not supported')
      end select

    end subroutine wt_BoundariesTau

    subroutine wt_BoundariesGrid(wt,values,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wt_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !    省略時は値/勾配 0 となる. 

      character(len=2), intent(in), optional             :: cond
              !(in) 境界条件. 省略時は 'DD'
              !        DD : 両端ディリクレ条件
              !        DN : 上端ディリクレ, 下端ノイマン条件
              !        ND : 上端ノイマン, 下端ディリクレ条件
              !        NN : 両端ノイマン条件

      if (.not. present(cond)) then
         if (present(values)) then
            call at_boundariesGrid_DD(wt,values)
         else
            call at_boundariesGrid_DD(wt)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesGrid_NN(wt,values)
         else
            call at_BoundariesGrid_NN(wt)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesGrid_DN(wt,values)
         else
            call at_BoundariesGrid_DN(wt)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesGrid_ND(wt,values)
         else
            call at_BoundariesGrid_ND(wt)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesGrid_DD(wt,values)
         else
            call at_BoundariesGrid_DD(wt)
         endif
      case default
         call MessageNotify('E','wt_BoundariesGrid','B.C. not supported')
      end select

    end subroutine wt_BoundariesGrid

    subroutine wt_TorBoundariesTau(wt_TORPOT,values,cond,new) 
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
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt_TORPOT
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) 両端境界でのトロイダルポテンシャル
              !     粘着条件の時のみ有効

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:), allocatable  :: alu
      integer, dimension(:), allocatable    :: kp
      real(8), dimension(0:lm,0:lm)         :: tt_data
      real(8), dimension(0:lm,0:km)         :: tg_data
      logical                               :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_TorBoundariesTau','B.C. not supported')
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
         allocate(alu(0:lm,0:lm),kp(0:lm))

         tt_data = 0.0D0
         do l=0,lm
            tt_data(l,l)=1
         enddo
         alu = tt_data

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            tg_data = az_at(tt_data)
         else
            tg_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(lm-1,:) = tg_data(:,0)       ! 境界 k=0 での条件式代入

         if ( rigid2 ) then
            tg_data = az_at(tt_data)    
         else
            tg_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(lm,:)   = tg_data(:,km)      ! 境界 k=km での条件式代入

         call ludecomp(alu,kp)

         if ( rigid1 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid1 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid1) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         if ( rigid2 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid2 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesTau',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid2) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesTau',&
                 'Boundary value k=0 cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wt_TorBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid1 .AND. present(values) ) then
         wt_torpot(:,lm-1) = values(:,1)
      else
         wt_torpot(:,lm-1) = 0.0D0
      endif
      if ( rigid2 .AND. present(values) ) then
         wt_torpot(:,lm)   = values(:,2)
      else
         wt_torpot(:,lm) = 0.0D0
      endif

      wt_torpot = lusolve(alu,kp,wt_TORPOT)

    end subroutine wt_TorBoundariesTau

    subroutine wt_TorBoundariesGrid(wt_TORPOT,values,cond,new) 
      !
      ! 速度トロイダルポテンシャルに対して境界条件を適用する.
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wt_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
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
      real(8), dimension(nm+1,0:lm),intent(inout)      :: wt_TORPOT
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(nm+1,2), intent(in), optional :: values
              !(in) 両端境界でのトロイダルポテンシャル
              !     粘着条件の時のみ有効

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(nm+1,0:km):: wz_TORPOT
      real(8), dimension(:,:), allocatable  :: alu
      integer, dimension(:), allocatable    :: kp
      real(8), dimension(0:lm,0:lm)         :: tt_data
      real(8), dimension(0:lm,0:km)         :: tz_data
      logical                               :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_TorBoundariesGrid','B.C. not supported')
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
            call MessageNotify('E','TorBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(0:km,0:lm),kp(0:lm))

         tt_data = 0.0D0
         do l=0,lm
            tt_data(l,l)=1
         enddo
         tz_data = az_at(tt_data)
         alu = transpose(tz_data)       ! 内部領域での値はそのまま

         if ( rigid1 ) then
            tz_data = az_at(tt_data)
         else
            tz_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(0,:) = tz_data(:,0)        ! 境界 k=0 での条件式代入

         if ( rigid2 ) then
            tz_data = az_at(tt_data)
         else
            tz_data = az_at(at_dr_at(at_az( &
                 az_at(tt_data)/spread(z_rad,1,lm+1))))
         endif
         alu(km,:)   = tz_data(:,km)    ! 境界 k=km での条件式代入

         call ludecomp(alu,kp)

         if ( rigid1 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=0 was given by the optional variable.')
         else if ( rigid1 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=0 was set to zero.')
         else if ( (.NOT. rigid1) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesGrid',&
                 'Boundary value at k=0 cannot be set under stress-free condition.')
         endif

         if ( rigid2 .AND. present(values) ) then 
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid2 .AND. (.NOT.present(values)) ) then
            call MessageNotify('M','wt_TorBoundariesGrid',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid2) .AND. present(values) ) then
            call MessageNotify('W','wt_TorBoundariesGrid',&
                 'Boundary value at k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wt_TorBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_TorPot       = wz_wt(wt_TorPot)

      if ( rigid1 .AND. present(values) ) then
         wz_TorPot(:,0)  = values(:,1)
      else
         wz_TorPot(:,0)  = 0.0D0
      endif

      if ( rigid2 .AND. present(values) ) then
         wz_TorPot(:,km) = values(:,2)
      else
         wz_TorPot(:,km) = 0.0D0
      endif

      wt_torpot = lusolve(alu,kp,wz_TorPot)

    end subroutine wt_TorBoundariesGrid

    function wz_LaplaPol2Pol_wz(wz,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      !
      ! チェビシェフ格子点空間で境界条件を適用している. 
      ! この関数を用いるためには wt_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !   ▽^2Φ = f
      !     Φ = const. at boundaries.
      !     ∂Φ/∂r = 0 at boundaries           (粘着条件) 
      !     or ∂^2Φ/∂r^2 = 0 at boundaries    (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(nm+1,0:km),intent(in)  :: wz
              !(in) 入力▽^2φ分布

      real(8), dimension(nm+1,0:km)             :: wz_LaplaPol2Pol_wz
              !(out) 出力ポロイダルポテンシャル分布

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(nm+1,0:km)  :: wz_work
      real(8), dimension(0:km,0:km)           :: gg
      real(8), dimension(0:km,0:km)           :: gg_work
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_laplapol2pol_wt','B.C. not supported')
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
            call MessageNotify('E','wz_LaplaPol2Pol_wz', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(nm+1,0:km,0:km),kp(nm+1,0:km))

         do k=0,km
            wz_work = 0.0D0 ; wz_work(:,k) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,k) = wz_wt(wt_lapla_wt(wt_wz(wz_work)))
         enddo

         ! 運動学的条件. 流線は境界で一定
         gg = 0.0D0
         do k=0,km
            gg(k,k)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,0,:)   = gg(:,0)
            alu(n,km,:)  = gg(:,km)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            gg_work=az_at(at_dr_at(at_az(gg)))
         else
            gg_work=az_at(at_dr_at(at_dr_at(at_az(gg))))
         endif
         do n=1,nm+1
            alu(n,1,:) = gg_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid2 ) then
            gg_work=az_at(at_dr_at(at_az(gg)))
         else
            gg_work=az_at(at_dr_at(at_dr_at(at_az(gg))))
         endif
         do n=1,nm+1
            alu(n,km-1,:) = gg_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','wz_LaplaPol2Pol_wz',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_work         = wz
      wz_work(:,1)    = 0.0D0               ! 力学的条件
      wz_work(:,km-1) = 0.0D0               ! 力学的条件
      wz_work(:,0)    = 0.0D0               ! 運動学的条件
      wz_work(:,km)   = 0.0D0               ! 運動学的条件 

      wz_laplapol2pol_wz = lusolve(alu,kp,wz_work)

    end function wz_LaplaPol2Pol_wz

    function wt_LaplaPol2PolGrid_wt(wt,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      ! チェビシェフ格子点空間で境界条件を適用している. 
      !
      ! この関数を用いるためには wt_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !    ▽^2Φ = f
      !      Φ = const. at boundaries.
      !      ∂Φ/∂r = 0 at boundaries          (粘着条件) 
      !      or ∂^2Φ/∂r^2 = 0 at boundaries   (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      ! 最終的にチェビシェフ係数の解が欲しい場合には, wz_LaplaPol2Pol_wz に
      ! 比べてチェビシェフ -- 格子点変換が 1 回分少なくて済む.
      !
      real(8), dimension(nm+1,0:lm),intent(in)  :: wt
              !(in) 入力▽^2φ分布

      real(8), dimension(nm+1,0:lm)             :: wt_LaplaPol2PolGrid_wt
              !(out) 出力ポロイダルポテンシャル分布

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(nm+1,0:km)  :: wz_work
      real(8), dimension(nm+1,0:lm)  :: wt_work
      real(8), dimension(0:lm,0:lm)           :: tt_I
      real(8), dimension(0:lm,0:km)           :: tz_work
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l,n
      save    :: alu, kp, first

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','wt_LaplaPol2PolGrid_wt','B.C. not supported')
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
            call MessageNotify('E','wt_LaplaPol2PolGrid_wt', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(nm+1,0:km,0:lm),kp(nm+1,0:lm))

         do l=0,lm
            wt_work = 0.0D0 ; wt_work(:,l) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,l) = wz_wt(wt_Lapla_wt(wt_work))
         enddo

         ! 運動学的条件. 流線は境界で一定
         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo

         ! 非電気伝導体
         tz_work = az_at(tt_I)

         do n=1,nm+1
            alu(n,0,:)  = tz_work(:,0)
            alu(n,km,:) = tz_work(:,km)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            tz_work=az_at(at_Dr_at(tt_I))
         else
            tz_work=az_at(at_Dr_at(at_Dr_at(tt_I)))
         endif
         do n=1,nm+1
            alu(n,1,:) = tz_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid2 ) then
            tz_work=az_at(at_Dr_at(tt_I))
         else
            tz_work=az_at(at_Dr_at(at_Dr_at(tt_I)))
         endif
         do n=1,nm+1
            alu(n,km-1,:) = tz_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','wt_LaplaPol2PolGrid_wt',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_work         = wz_wt(wt)
      wz_work(:,1)    = 0.0D0               ! 力学的条件
      wz_work(:,km-1) = 0.0D0               ! 力学的条件
      wz_work(:,0)    = 0.0D0               ! 運動学的条件
      wz_work(:,km)   = 0.0D0               ! 運動学的条件 

      wt_LaplaPol2PolGrid_wt = lusolve(alu,kp,wz_work)

    end function wt_LaplaPol2PolGrid_wt

    subroutine wt_TormagBoundariesTau(wt_TOR,new)
      
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wt_psi = 0   at the outer boundary
      ! 内側
      !    wt_psi = 0       at the inner boundary
      ! 
      ! であるから wt_Boundaries で対応可能だが, 将来のため別途作成しておく.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n
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
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = tt_I
         enddo

         ! 非電気伝導体
         tz_PSI = az_at(tt_I)

         do n=1,nm+1
            alu(n,lm-1,:) = tz_PSI(:,0)
            alu(n,lm,:)   = tz_PSI(:,km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI)

         call MessageNotify('M','TormagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wt_TOR(:,lm-1) = 0.0D0
      wt_TOR(:,lm)   = 0.0D0
      wt_TOR = lusolve(alu,kp,wt_TOR)

    end subroutine wt_TormagBoundariesTau

    subroutine wt_TormagBoundariesGrid(wt_TOR,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wt_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wt_psi = 0   at the outer boundary
      ! 内側
      !    wt_psi = 0       at the inner boundary
      ! 
      ! であるので wt_Boundaries で対応可能だが, 将来のため別途作成しておく
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(nm+1,0:km)  :: wz_TOR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lm /= km ) then
            call MessageNotify('E','TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         allocate(alu(nm+1,0:km,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = transpose(az_at(tt_I))   ! 内部領域は値そのまま.
         enddo

         ! 非電気伝導体
         tz_PSI = az_at(tt_I)

         do n=1,nm+1
            alu(n,0,:) = tz_PSI(:,0)
            alu(n,km,:)   = tz_PSI(:,km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI)

         call MessageNotify('M','TormagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wz_TOR       = wz_wt(wt_TOR)
      wz_TOR(:,0)  = 0.0D0
      wz_TOR(:,km) = 0.0D0
      wt_TOR = lusolve(alu,kp,wz_TOR)

    end subroutine wt_TormagBoundariesGrid

    subroutine wt_PolmagBoundariesTau(wt_POL,new)
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
      !  * 内側境界 : dh/dr - nh/r = 0
      !
      ! である. ここで n は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(:,:), allocatable    :: tz_DPSIDR

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
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         if ( allocated(tz_DPSIDR) ) deallocate(tz_DPSIDR)

         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km),tz_DPSIDR(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = tt_I
         enddo

         ! 非電気伝導体
         tz_PSI = az_at(tt_I)
         tz_DPSIDR = az_at(at_dr_at(tt_I))

         do n=1,nm+1
            nn=nm_l(n)
            alu(n,lm-1,:) = tz_DPSIDR(:,0) + (nn(1)+1) * tz_PSI(:,0)/z_RAD(0)
            alu(n,lm,:)   = tz_DPSIDR(:,km) - nn(1) * tz_PSI(:,km)/z_RAD(km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI,tz_DPSIDR)

         call MessageNotify('M','PolmagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wt_POL(:,lm-1) = 0.0D0
      wt_POL(:,lm)   = 0.0D0
      wt_POL = lusolve(alu,kp,wt_POL)

    end subroutine wt_PolmagBoundariesTau

    subroutine wt_PolmagBoundariesGrid(wt_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用. 
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wt_Initial にて設定するチェビシェフ切断波数(lm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場ポロイダルポテンシャルの各水平スペクトル成分 h に
      ! たいして境界条件が与えられ,
      !
      !  * 外側境界 : dh/dr + (n+1)h/r = 0
      !  * 内側境界 : dh/dr - nh/r = 0
      !
      ! である. ここで n は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(nm+1,0:lm),intent(inout)   :: wt_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: tt_I
      real(8), dimension(:,:), allocatable    :: tz_PSI
      real(8), dimension(:,:), allocatable    :: tz_DPSIDR
      real(8), dimension(nm+1,0:km)  :: wz_POL

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
            call MessageNotify('E','PolMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(tt_I) ) deallocate(tt_I)
         if ( allocated(tz_PSI) ) deallocate(tz_PSI)
         if ( allocated(tz_DPSIDR) ) deallocate(tz_DPSIDR)

         allocate(alu(nm+1,0:lm,0:lm),kp(nm+1,0:lm))
         allocate(tt_I(0:lm,0:lm),tz_PSI(0:lm,0:km),tz_DPSIDR(0:lm,0:km))

         tt_I = 0.0D0
         do l=0,lm
            tt_I(l,l)=1.0D0
         enddo
         do n=1,nm+1
            alu(n,:,:) = transpose(az_at(tt_I))  ! 内部領域は値を保存
         enddo

         ! 非電気伝導体
         tz_PSI = az_at(tt_I)
         tz_DPSIDR = az_at(at_dr_at(tt_I))

         do n=1,nm+1
            nn=nm_l(n)
            alu(n,0,:)  = tz_DPSIDR(:,0) + (nn(1)+1) * tz_PSI(:,0)/z_RAD(0)
            alu(n,km,:) = tz_DPSIDR(:,km) - nn(1) * tz_PSI(:,km)/z_RAD(km)
         enddo
         call ludecomp(alu,kp)

         deallocate(tt_I,tz_PSI,tz_DPSIDR)

         call MessageNotify('M','PolmagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wz_POL       = wz_wt(wt_POL)
      wz_POL(:,0)  = 0.0D0
      wz_POL(:,km) = 0.0D0
      wt_POL = lusolve(alu,kp,wz_POL)

    end subroutine wt_PolmagBoundariesGrid

end module wt_zonal_module
