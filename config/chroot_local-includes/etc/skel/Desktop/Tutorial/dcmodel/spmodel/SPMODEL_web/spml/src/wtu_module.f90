!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  wtu_module
!
!    spml/wtu_module モジュールは球とその外側の球殻領域での流体運動を
!    スペクトル法によって数値計算するための Fortran90 関数を提供する
!    ものである. 
!
!    水平方向に球面調和函数変換および動径方向にチェビシェフ変換を用いる
!    場合のスペクトル計算のためのさまざまな関数を提供する. 
!
!    内部で wt_module, wu_module を用いている. 最下部では球面調和変換
!    およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
!    サブルーチンを用いている.
!
!
!履歴  2008/01/12  竹広真一  wt_module, wu_module より改変
!      2008/07/04  佐々木洋平 0 -> 0.0D0, 1 -> 1.0D0 に修正
!      2008/07/05  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/09  竹広真一  wtu_Initial メッセージ出力
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!凡例
!      データ種類と index
!        x : 経度         y : 緯度    z : 動径(球殻内)    r : 動径(球内)
!        w : 球面調和関数スペクトル
!        n : 球面調和関数スペクトル(水平全波数)
!        m : 球面調和関数スペクトル(帯状波数)
!        t : チェビシェフ関数スペクトル(球殻内)
!        u : チェビシェフ関数スペクトル(球内)
!        a : 任意の次元
!
!        xyz : 球殻内 3 次元格子点データ
!        xyr : 球内の 3 次元格子点データ
!        xy  : 水平 2 次元格子点データ
!        yr  : 子午面 2 次元格子点データ
!        xr  : 緯度面 2 次元格子点データ
!
!        wz  : 水平スペクトル動径格子点データ(球殻内)
!        wr  : 水平スペクトル動径格子点データ(球内)
!        wt  : スペクトルデータ(球殻内)
!        wu  : スペクトルデータ(球内)
!
!++
module wtu_module
  !
  != wtu_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wtu_module.f90,v 1.7 2010-03-02 10:09:07 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wtu_module モジュールは球とその外側の球殻領域での流体運動を
  ! スペクトル法によって数値計算するための Fortran90 関数を提供する
  ! ものである. 
  !
  ! 水平方向に球面調和函数変換および動径方向にチェビシェフ変換を用いる
  ! 場合のスペクトル計算のためのさまざまな関数を提供する. 
  !
  ! 内部で wt_module, wu_module を用いている. 最下部では球面調和変換
  ! およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
  ! サブルーチンを用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭
  !   (wu_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_, wt_, nmz_, nz_, 
  !   xyz_, wz_, w_, xy_, x_, y_, z_, a_) は, 返す値の形を示している.
  !   wt_  :: スペクトルデータ(球面調和函数・チェビシェフ変換-球殻)
  !   nmz_ :: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径-球殻)
  !   nz_  :: 水平スペクトルデータ(全波数 n, 動径-球殻)
  !   xyz_ :: 3 次元格子点データ(経度・緯度・動径-球殻)
  !   wz_  :: 水平スペクトル, 動径格子点データ, 球殻
  !   wu_  :: スペクトルデータ(球面調和函数・チェビシェフ変換-球)
  !   nmr_ :: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径-球)
  !   nr_  :: 水平スペクトルデータ(全波数 n, 動径-球)
  !   xyr_ :: 3 次元格子点データ(経度・緯度・動径-球)
  !   wr_  :: 水平スペクトル, 動径格子点データ, 球
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後
  !   (wt_, xyz_, wz_, w_, xy_, x_, y_, z_, a_,
  !   wu_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) は, 入力変数の形が
  !   スペクトルデータおよび格子点データであることを示している.
  !   _wt      :: スペクトルデータ
  !   _xyz     :: 3 次元格子点データ
  !   _xyz_xyz :: 2 つの3 次元格子点データ, ...
  !   _wu      :: スペクトルデータ
  !   _xyr     :: 3 次元格子点データ
  !   _xyr_xyr :: 2 つの3 次元格子点データ, ...
  !
  !=== 各データの種類の説明
  !
  !
  ! * xyz : 3 次元格子点データ(経度・緯度・動径-球殻)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  ! 
  ! * wt : スペクトルデータ-球殻
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm は球面調和函数の最大全波数, lm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン wt_Initial にてあらかじめ設定しておく. 
  !   * 水平スペクトルデータの格納のされ方は関数 l_nm, nm_l によって調べる
  !     ことができる.
  ! 
  ! * nmz : 水平スペクトルデータの並んだ 3 次元配列(球殻).
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数, 第 3 次元が動径座標を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wt_Initial にて
  !     あらかじめ設定しておく.
  ! 
  ! * nz : スペクトルデータの並んだ 2 次元配列(球殻).
  !   * 変数の種類と次元は real(8), dimension(0:nm,0:km). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  ! 
  ! * wz : 水平スペクトル, 動径格子点データ(球殻).
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:km).
  ! 
  ! * xyr : 3 次元格子点データ(経度・緯度・動径-球)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,0:km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  ! 
  ! * wu : スペクトルデータ(球)
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm は球面調和函数の最大全波数, lm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン wt_Initial にてあらかじめ設定しておく. 
  !   * 水平スペクトルデータの格納のされ方は関数 l_nm, nm_l によって調べる
  !     ことができる.
  ! 
  ! * nmr : 水平スペクトルデータの並んだ 3 次元配列(球).
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,0:km). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数, 第 3 次元が動径座標を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wt_Initial にて
  !     あらかじめ設定しておく.
  ! 
  ! * nr : スペクトルデータの並んだ 2 次元配列(球).
  !   * 変数の種類と次元は real(8), dimension(0:nm,0:km). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  ! 
  ! * wr : 水平スペクトル, 動径格子点データ(球).
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:km).
  ! 
  ! * wt_ で始まる関数が返す値はスペクトルデータに同じ.
  ! 
  ! * xyz_ で始まる関数が返す値は 3 次元格子点データに同じ.
  ! 
  ! * wz_ で始まる関数が返す値は水平スペクトル, 動径格子点データに同じ.
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
  ! wtu_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, y_Lat               :: 格子点座標(緯度, 経度)を格納した1 次元配列
  ! z_Rad                      :: 格子点座標(動径, 球殻)を格納した1 次元配列
  ! r_Rad                      :: 格子点座標(動径, 球)を格納した1 次元配列
  ! x_Lon_Weight, y_Lat_Weight :: 重み座標を格納した 1 次元配列
  ! z_Rad_Weight               :: 重み座標を格納した 1 次元配列
  ! r_Rad_Weight               :: 重み座標を格納した 1 次元配列
  ! xyz_Lon, xyz_Lat           :: 格子点データの経度・緯度(X,Y) 
  !                               (格子点データ型 3 次元配列)
  ! xyz_Rad                    :: 格子点データの動径(球殻)(Z) 
  !                               (格子点データ型 3 次元配列)
  ! xyr_Lon, xyr_Lat           :: 格子点データの経度・緯度(X,Y)
  !                               (格子点データ型 3 次元配列)
  ! xyr_Rad                    :: 格子点データの動径(球) 
  !                               (格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! xyz_wt, wt_xyz :: スペクトルデータと 3 次元格子データの間の変換
  !                   (球面調和函数, チェビシェフ変換)
  ! xyz_wz, wz_xyz :: 3 次元格子データと水平スペクトル・動径格子データとの
  !                   間の変換 (球面調和函数)
  ! wz_wt, wt_wz   :: スペクトルデータと水平スペクトル・動径格子データとの
  !                   間の変換 (チェビシェフ変換)
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
  ! wt_DRad_wt          :: スペクトルデータに動径微分
  !                        ∂/∂r を作用させる
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
  ! wt_KxRGrad_wt            :: スペクトルデータに経度微分
  !                             k×r・▽ = ∂/∂λを作用させる
  ! xyz_KGrad_wt             :: スペクトルデータに軸方向微分
  !                             k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を
  !                             作用させる
  ! wt_L2_wt                 :: スペクトルデータに
  !                             L2 演算子 = -水平ラプラシアンを
  !                             作用させる
  ! wt_L2Inv_wt              :: スペクトルデータに 
  !                             L2 演算子の逆 = -逆水平ラプラシアンを
  !                             作用させる
  ! wt_QOperator_wt          :: スペクトルデータに演算子
  !                             Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) を
  !                             作用させる
  ! wt_RadRot_xyz_xyz        :: ベクトル v の渦度と動径ベクトル r の内積
  !                             r・(▽×v) を計算する
  ! wt_RadRotRot_xyz_xyz_xyz :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wt_Potential2Vector      :: トロイダルポロイダルポテンシャルから
  !                             ベクトル場を計算する
  ! wt_Potential2Rotation    :: トロイダルポロイダルポテンシャルで表される
  !                             非発散ベクトル場の回転の各成分を計算する
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
  ! nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt   ::
  !     トロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  ! nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt   :: 
  !     ポロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
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
  ! wtu_TormagBoundariesTau, wtu_TormagBoundariesGrid                    ::
  !     磁場トロイダルポテンシャル境界の境界条件を球および球殻領域全体に
  !     適用する(タウ法, 選点法)
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
  !
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
  ! IntLonRad_xr, AvrLonRad_xr :: 2 次元(XZ)格子点データの経度動径積分
  !                               および平均
  ! IntLatRad_yr, AvrLatRad_yr :: 2 次元(YZ)格子点データの緯度動径(子午面)
  !                               積分および平均 
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
  ! IntRad_z, AvrRad_z  :: 1 次元(Z)格子点データの動径方向積分および平均
  ! IntRad_r, AvrRad_r  :: 1 次元(Z)格子点データの動径方向積分および平均
  !
  ! 
  use dc_message
  use lumatrix
  use wt_module
  use wu_module, only : wu_initial, &
       r_Rad, r_Rad_Weight, xyr_Lon, xyr_Lat, xyr_Rad, wr_Rad, wu_VMiss, &
       wr_DRad_wu, r_DRad_t, wr_wu, wu_wr, wr_DRad2_wu, r_DRad2_t, &
       xyr_wu, wu_xyr, xyr_wr, wr_xyr, &
       wr_DivRad_wu, wr_RotRad_wu, wr_Lapla_wu, &
       xyr_GradLon_wu, xyr_gradlat_wu, wr_DivLon_xyr, wr_DivLat_xyr, &
       wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr, &
       xyr_RotLon_wu_wu, xyr_RotLat_wu_wu, wr_RotRad_xyr_xyr, &
       yr_IntLon_xyr, xr_IntLat_xyr, xy_IntRad_xyr, &
       x_IntLatRad_xyr, y_IntLonRad_xyr, r_IntLonLat_xyr, &
       IntLonLatRad_xyr, &
       r_IntLat_yr, y_IntRad_yr, IntLatRad_yr, &
       r_IntLon_xr, x_IntRad_xr, IntLonRad_xr, &
       IntRad_r, &
       yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr, &
       x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr, &
       AvrLonLatRad_xyr, &
       r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr, &
       r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr, &
       AvrLon_x, AvrLat_y, AvrRad_r, &
       wu_KxRGrad_wu, xyr_KGrad_wu, wu_L2_wu, wu_L2Inv_wu, wu_QOperator_wu, &
       wu_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr, &
       wu_Potential2vector, wu_Potential2Rotation, &
       nmr_ToroidalEnergySpectrum_wu, nr_ToroidalEnergySpectrum_wu,&
       nmr_PoloidalEnergySpectrum_wu, nr_PoloidalEnergySpectrum_wu, &
       wu_Boundary, wu_TorBoundary, wr_LaplaPol2Pol_wr, &
       wu_TormagBoundary, wu_PolmagBoundary, &
       wu_BoundaryTau, wu_TorBoundaryTau, &
       wu_TormagBoundaryTau, wu_PolmagBoundaryTau, &
       wu_BoundaryGrid, wu_TorBoundaryGrid, wu_LaplaPol2PolGrid_wu, &
       wu_TormagBoundaryGrid, wu_PolmagBoundaryGrid

  implicit none
  private

  public wtu_Initial

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

  public r_Rad, r_Rad_Weight
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wu_VMiss

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

  public r_IntLat_yr, y_IntRad_yr, IntLatRad_yr
  public r_IntLon_xr, x_IntRad_xr, IntLonRad_xr
  public IntRad_r

  public yr_AvrLon_xyr, xr_AvrLat_xyr, xy_AvrRad_xyr
  public x_AvrLatRad_xyr, y_AvrLonRad_xyr, r_AvrLonLat_xyr
  public AvrLonLatRad_xyr

  public r_AvrLat_yr, y_AvrRad_yr, AvrLatRad_yr
  public r_AvrLon_xr, x_AvrRad_xr, AvrLonRad_xr
  public AvrRad_r

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

  public wtu_TormagBoundaries, wtu_TormagBoundariesTau, wtu_TormagBoundariesGrid
  public wtu_PolmagBoundaries, wtu_PolmagBoundariesTau, wtu_PolmagBoundariesGrid

  interface wtu_TorMagBoundaries
     module procedure wtu_TorMagBoundariesTau
  end interface

  interface wtu_PolMagBoundaries
     module procedure wtu_PolMagBoundariesTau
  end interface

  integer            :: im=64, jm=32        ! 水平格子点の設定(経度, 緯度)
  integer            :: nm=21               ! 水平切断波数の設定
  integer            :: kmo=16, kmi=16      ! 鉛直格子点の設定(球殻, 球)
  integer            :: lmo=16, lmi=16      ! 鉛直切断波数の設定(球殻, 球)  
                                     
  real(8)            :: ri=1.0D0, ro=2.0D0  ! 内外球半径
  real(8), parameter :: pi=3.1415926535897932385D0

  save im, jm, kmo, kmi, nm, lmo, lmi, ri, ro

  contains
  !--------------- 初期化 -----------------
   subroutine wtu_Initial(i,j,ki,ko,n,li,lo,r_in,r_out,np)
     !
     ! スペクトル変換の格子点数, 波数, 球および球殻動径座標の範囲を設定する.
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
     integer,intent(in) :: ki             ! 格子点数(動径 r)
     integer,intent(in) :: ko             ! 格子点数(動径 z)
     integer,intent(in) :: n              ! 切断波数(水平全波数)
     integer,intent(in) :: li             ! 切断波数(球動径波数)
     integer,intent(in) :: lo             ! 切断波数(球殻動径波数)

     real(8),intent(in) :: r_in, r_out    ! 球半径

     integer,intent(in), optional :: np   ! OPENMP での最大スレッド数

     im = i  ; jm = j ; kmo = ko ; kmi = ki
     nm = n  ; lmo = lo ; lmi=li
     ri = r_in ; ro = r_out

     if ( present(np) ) then
        if ( kmo > kmi ) then
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,np)
           call wu_Initial(im,jm,kmi,nm,lmi,ri,np,wa_init=.false.)
        else
           call wu_Initial(im,jm,kmi,nm,lmi,ri,np)
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,np,wa_init=.false.)
        endif
     else
        if ( kmo > kmi ) then
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro)
           call wu_Initial(im,jm,kmi,nm,lmi,ri,wa_init=.false.)
        else
           call wu_Initial(im,jm,kmi,nm,lmi,ri)
           call wt_Initial(im,jm,kmo,nm,lmo,ri,ro,wa_init=.false.)
        endif
     endif

     call MessageNotify('M','wtu_initial','wtu_module (2009/07/31) is initialized')

   end subroutine wtu_Initial

  !--------------- 境界値問題 -----------------
    subroutine wtu_TormagBoundariesTau(wt_TOR,wu_TOR,Pmo,Pmi,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ外側境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    Psi_o = 0   at the outer boundary
      ! 球--球殻境界
      !    Psi_o = Psi_i, Pm_o DrDPsi_o = Pm_i DrDPsi_i    at the boundary
      ! 
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8),intent(in)           :: Pmo
              !(in) 球殻の磁気プランドル数

      real(8),intent(in)           :: Pmi
              !(in) 内球の磁気プランドル数

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_PSI

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
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lmo+lmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo+lmi+1
            wt_I = 0.0D0
            alu(:,l,l) = 1.0D0      ! 低波数はそのまま
         enddo

         ! 球--球殻境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,lmo-1,l)   = wz_PSI(:,0)
            alu(:,lmo,l)   = Pmo * wz_DPSIDR(:,kmo) 
            alu(:,lmo+lmi+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,lmo,lmo+1+l)   = -Pmi * wr_DPSIDR(:,0) 
            alu(:,lmo+lmi+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wtu_PSI(:,0:lmo)            = wt_TOR
      wtu_PSI(:,lmo+1:lmo+lmi+1)  = wu_TOR
      wtu_PSI(:,lmo-1)      = 0.0D0
      wtu_PSI(:,lmo)        = 0.0D0
      wtu_PSI(:,lmo+lmi+1)  = 0.0D0
      wtu_PSI = lusolve(alu,kp,wtu_PSI)

      wt_TOR = wtu_PSI(:,0:lmo)
      wu_TOR = wtu_PSI(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_TormagBoundariesTau

    subroutine wtu_TormagBoundariesGrid(wt_TOR,wu_TOR,Pmo,Pmi,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wtu_Initial にて設定するチェビシェフ切断波数(lmo,lmi)と
      ! 鉛直格子点数(kmo,kmi)を等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    Psi_o = 0   at the outer boundary
      ! 球--球殻境界
      !    Psi_o = Psi_i, Pm_o DrDPsi_o = Pm_i DrDPsi_i    at the boundary
      ! 
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8),intent(in)           :: Pmo
              !(in) 球殻の磁気プランドル数

      real(8),intent(in)           :: Pmi
              !(in) 内球の磁気プランドル数

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_PSI
      real(8), dimension((nm+1)*(nm+1),0:kmo+kmi+1) :: wzr_PSI

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

         if ( lmo /= kmo .OR. lmi /= kmi ) then
            call MessageNotify('E','wtu_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:kmo+kmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l)=1.0D0
            alu(:,0:kmo,l) = wz_wt(wt_I)              ! 内部領域は値そのまま.
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,kmo+1:kmo+kmi+1,lmo+1+l) = wr_wu(wu_I)! 内部領域は値そのまま.
         enddo

         ! 球--球殻境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,kmo,l)   = Pmo * wz_DPSIDR(:,kmo) 
            alu(:,kmo+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,kmo,lmo+1+l)   = -Pmi * wr_DPSIDR(:,0) 
            alu(:,kmo+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','TormagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wzr_PSI(:,0:kmo)            = wz_wt(wt_TOR)
      wzr_PSI(:,kmo+1:kmo+kmi+1)  = wr_wu(wu_TOR)
      wzr_PSI(:,0)      = 0.0D0
      wzr_PSI(:,kmo)    = 0.0D0
      wzr_PSI(:,kmo+1)  = 0.0D0
      wtu_PSI = lusolve(alu,kp,wzr_PSI)

      wt_TOR = wtu_PSI(:,0:lmo)
      wu_TOR = wtu_PSI(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_TormagBoundariesGrid

    subroutine wtu_PolmagBoundariesTau(wt_Pol,wu_Pol,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ外側境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 現在のところ外側境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    dPol_o/dr +(n+1)/r Pol_o= 0   at the outer boundary
      ! 球--球殻境界
      !    Pol_o = Phi_i, DrDPol_o = DrDPol_i    at the boundary
      ! 
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_Pol
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_Pol
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_Pol

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n,l,nn(2)
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
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lmo+lmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo+lmi+1
            alu(:,l,l) = 1.0d0   ! 低波数はそのまま.
         enddo

         ! 球殻外側境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,lmo-1,l) = wz_DPSIDR(n,0) + (nn(1)+1)*wz_PSI(n,0)/z_RAD(0)
            enddo
         enddo

         ! 球--球殻境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,lmo,l)       = wz_DPSIDR(:,kmo) 
            alu(:,lmo+lmi+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,lmo,lmo+1+l)   = - wr_DPSIDR(:,0) 
            alu(:,lmo+lmi+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wtu_Pol(:,0:lmo)            = wt_Pol
      wtu_Pol(:,lmo+1:lmo+lmi+1)  = wu_Pol
      wtu_Pol(:,lmo-1)  = 0.0D0
      wtu_Pol(:,lmo)    = 0.0D0
      wtu_Pol(:,lmo+1)  = 0.0D0
      wtu_Pol = lusolve(alu,kp,wtu_Pol)

      wt_Pol = wtu_Pol(:,0:lmo)
      wu_Pol = wtu_Pol(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_PolmagBoundariesTau

    subroutine wtu_PolmagBoundariesGrid(wt_Pol,wu_Pol,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! wtu_Initial にて設定するチェビシェフ切断波数(lmo,lmi)と
      ! 鉛直格子点数(kmo,kmi)を等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    dPol_o/dr +(n+1)/r Pol_o= 0   at the outer boundary
      ! 球--球殻境界
      !    Pol_o = Phi_i, DrDPol_o = DrDPol_i    at the boundary
      ! 
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lmo),intent(inout)   :: wt_Pol
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension((nm+1)*(nm+1),0:lmi),intent(inout)   :: wu_Pol
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wt_I
      real(8), dimension(:,:), allocatable    :: wz_PSI
      real(8), dimension(:,:), allocatable    :: wz_DPSIDR
      real(8), dimension(:,:), allocatable    :: wu_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      real(8), dimension((nm+1)*(nm+1),0:lmo+lmi+1) :: wtu_Pol
      real(8), dimension((nm+1)*(nm+1),0:kmo+kmi+1) :: wzr_Pol

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n,l,nn(2)
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( lmo /= kmo .OR. lmi /= kmi ) then
            call MessageNotify('E','wtu_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wt_I) ) deallocate(wt_I)
         if ( allocated(wz_PSI) ) deallocate(wz_PSI)
         if ( allocated(wu_I) ) deallocate(wu_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:kmo+kmi+1,0:lmo+lmi+1))
         allocate(kp((nm+1)*(nm+1),0:lmo+lmi+1))
         allocate(wt_I((nm+1)*(nm+1),0:lmo),wz_PSI((nm+1)*(nm+1),0:kmo))
         allocate(wu_I((nm+1)*(nm+1),0:lmi),wr_PSI((nm+1)*(nm+1),0:kmi))
         allocate(wz_DPSIDR((nm+1)*(nm+1),0:kmo))
         allocate(wr_DPSIDR((nm+1)*(nm+1),0:kmi))

         alu = 0.0D0
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l)=1.0D0
            alu(:,0:kmo,l) = wz_wt(wt_I)              ! 内部領域は値そのまま.
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l)=1.0D0
            alu(:,kmo+1:kmo+kmi+1,lmo+1+l) = wr_wu(wu_I)! 内部領域は値そのまま.
         enddo

         ! 球殻外側境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,0,l) = wz_DPSIDR(n,0) + (nn(1)+1) * wz_PSI(n,0)/z_RAD(0)
            enddo
         enddo

         ! 球--球殻境界
         do l=0,lmo
            wt_I = 0.0D0
            wt_I(:,l) = 1.0D0
            wz_PSI = wz_wt(wt_I)
            wz_DPSIDR = wz_wt(wt_DRad_wt(wt_I))

            alu(:,kmo,l)   = wz_DPSIDR(:,kmo) 
            alu(:,kmo+1,l) = wz_PSI(:,kmo)
         enddo
         do l=0,lmi
            wu_I = 0.0D0
            wu_I(:,l) = 1.0D0
            wr_PSI = wr_wu(wu_I)
            wr_DPSIDR = wr_DRad_wu(wu_I)

            alu(:,kmo,lmo+1+l)   = - wr_DPSIDR(:,0) 
            alu(:,kmo+1,lmo+1+l) = - wr_PSI(:,0)
         enddo

         call ludecomp(alu,kp)

         deallocate(wt_I,wz_PSI,wz_DPSIDR,wu_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif
      
      wzr_Pol(:,0:kmo)            = wz_wt(wt_Pol)
      wzr_Pol(:,kmo+1:kmo+kmi+1)  = wr_wu(wu_Pol)
      wzr_Pol(:,0)      = 0.0D0
      wzr_Pol(:,kmo)    = 0.0D0
      wzr_Pol(:,kmo+1)  = 0.0D0
      wtu_Pol = lusolve(alu,kp,wzr_Pol)

      wt_Pol = wtu_Pol(:,0:lmo)
      wu_Pol = wtu_Pol(:,lmo+1:lmo+lmi+1)  

    end subroutine wtu_PolmagBoundariesGrid

end module wtu_module
