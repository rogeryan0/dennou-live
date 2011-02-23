!--
!----------------------------------------------------------------------
! Copyright (c) 2002-2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  wt_mpi_module
!
!    spml/wt_mpi_module モジュールは球面上および球殻内での流体運動を
!    スペクトル法と MPI 並列化によって数値計算するための Fortran90 
!    関数を提供するものである. 
!
!    水平方向に球面調和函数変換および上下の境界壁を扱うための
!    チェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
!    関数を提供する. 
!
!    内部で wa_mpi_module, wt_module を用いている. 最下部では球面調和変換
!    およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
!    サブルーチンを用いている.
!
!
!履歴  2002/05/19  竹広真一  yt_module よりモジュール名変更.
!                            格子点次元を区別する必要があるため. 
!      2002/06/10  竹広真一  ポロイダル磁場境界条件ルーチン追加
!      2002/11/10  竹広真一  実空間での境界条件ルーチン追加
!      2002/11/24  竹広真一  チェビシェフ係数入出力の
!                            ラプラシアン逆解きルーチンを追加
!      2002/11/28  竹広真一  VGradV の計算変更.
!      2005/01/09  竹広真一  msgdmp -> MessageNotify に変更
!      2005/01/09  竹広真一  ベクトル場の回転の各成分を計算する関数を追加.
!      2005/02/19  竹広真一  トロイダル粘着境界条件にて非零速度場を
!                             与えられるようにオプションを追加
!      2005/02/19  竹広真一  変数 xy_Lon, xy_Lat を追加
!      2005/03/18  竹広真一  積分・平均関数を追加
!                            z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
!                            z_InRad_xz, y_IntRad_yz, IntLatRad_yz
!                            IntRad_z 
!                            z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
!                            z_InRad_xz, y_IntRad_yz, IntLatRad_yz
!                            IntRad_z 
!      2005/04/24  竹広真一  スペクトル解析計算ルーチンを追加
!                            nmz_ToroidalEnergySpectrum_wt
!                            nz_ToroidalEnergySpectrum_wt
!                            nmz_PoloidalEnergySpectrum_wt
!                            nz_PoloidalEnergySpectrum_wt
!                            内部変数欠損値 wt_VMiss を追加
!      2005/07/09  竹広真一  OPENMP 版変換ルーチンに対応
!      2006/03/03  竹広真一  配列サイズミス修正
!                            wt_wz, nmz_ToroidalEnergySpectrum_wt,
!                            nmz_PoloidalEnergySpectrum_wt
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2006/03/19  竹広真一  変数・手続き群の要約をコメントに追加
!      2007/08/11  竹広真一  境界値ルーチンのコメントを増強
!      2007/09/15  竹広真一  微分ルーチンのコメントを修正
!      2007/11/02  竹広真一  補間計算ルーチン追加
!      2007/11/11  竹広真一  境界条件関数サブルーチンに行列設定スイッチ導入
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2008/01/07  竹広真一  xyz_RotLon_wt_wt, xyz_RotLat_wt_wt バグ修正
!      2008/01/13  竹広真一  wa_initial スイッチ導入
!      2008/05/29  竹広真一  MPI 並列化
!      2008/06/11  竹広真一  OPENMP スイッチ導入
!      2008/06/11  竹広真一  配列添え字変更
!      2009/02/27  佐々木洋平 RDoc 用のコメントを修正
!      2010/01/07  佐々木洋平  RDoc 用のドキュメント修正, 
!                              include 'mpif.h' -> use mpi

!凡例
!      データ種類と index
!        x : 経度         y : 緯度    v : 緯度(分散格子)     z : 動径
!        w : 球面調和関数スペクトル
!        n : 球面調和関数スペクトル(水平全波数)
!        m : 球面調和関数スペクトル(帯状波数)
!        t : チェビシェフ関数スペクトル
!        a : 任意の次元
!
!        xyz : 3 次元格子点データ
!        xvz : 3 次元分散格子点データ
!        xy  : 水平 2 次元格子点データ
!        yz  : 子午面 2 次元格子点データ
!        xz  : 緯度面 2 次元格子点データ
!
!        wz  : 水平スペクトル動径格子点データ
!        wt  : スペクトルデータ
!
!++
module wt_mpi_module
  !
  != wt_mpi_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wt_mpi_module.f90,v 1.9 2010-02-18 15:28:27 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wt_mpi_module モジュールは球面上および球殻内での流体運動を
  ! スペクトル法と MPI 並列化によって数値計算するための Fortran90 
  ! 関数を提供するものである. 
  !
  ! 水平方向に球面調和函数変換および上下の境界壁を扱うための
  ! チェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
  ! 関数を提供する. 
  !
  ! 内部で wt_module, wa_mpi_module を用いている. 最下部では球面調和変換
  ! およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
  ! サブルーチンを用いている.
  !
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭(wt_, nmz_, nz_, xyz_, xvz_ wz_, w_, xy_, x_, y_, z_, a_)は,
  !   返す値の形を示している.
  !      wt_ :: スペクトルデータ(球面調和函数・チェビシェフ変換)
  !      nmz_:: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径)
  !      nz_ :: 水平スペクトルデータ(全波数 n, 動径)
  !     xyz_ :: 3 次元格子点データ(経度・緯度・動径)
  !     xvz_ :: 3 次元分散格子点データ(経度・緯度・動径)
  !      wz_ :: 水平スペクトル, 動径格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_wt, _xyz, _xvz, _wz_, _w, _xy, _xv, _x, _y, _z, _a) は, 
  !   入力変数の形がスペクトルデータおよび格子点データであることを示している.
  !         _wt :: スペクトルデータ
  !        _xyz :: 3 次元格子点データ
  !    _xyz_xyz :: 2 つの3 次元格子点データ, ...
  !        _xvz :: 3 次元分散格子点データ
  !    _xvz_xvz :: 2 つの3 次元分散格子点データ, ...
  !
  !=== 各データの種類の説明
  !
  ! * xyz : 3 次元格子点データ(経度・緯度・動径)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,jm,0:km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wt_mpi_Initial にてあらかじめ設定しておく.
  !
  ! * xvz : 3 次元分散格子点データ(経度・緯度・動径)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,jc,0:km). 
  !   * im, km はそれぞれ経度, 動径座標の格子点数であり, 
  !   * jc はこのプロセスで保有する緯度格子点数である. 
  !     サブルーチン wt_mpi_Initial を呼ぶと jc が設定される. 
  !
  ! * wt : スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:lm). 
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
  !     第 1 次元が水平全波数を表す. 
  !   * nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wt_Initial にてあらかじめ設定しておく.
  !
  ! * wz : 水平スペクトル, 動径格子点データ.
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:km).
  !
  ! * wt_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xyz_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * xvz_ で始まる関数が返す値は 3 次元分散格子点データに同じ.
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
  ! wt_mpi_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, x_Lon_Weight,  ::  格子点座標(経度)と重みを格納した 1 次元配列
  ! y_Lat, y_Lat_Weight   ::  格子点座標(緯度)と重みを格納した 1 次元配列
  ! v_Lat, v_Lat_Weight   ::  分散格子点座標(緯度)と重みを格納した 1 次元配列
  ! z_Rad, z_Rad_Weight   ::  格子点座標(同形)と重みを格納した 1 次元配列
  ! xyz_Lon, xyz_Lat, xyz_Rad :: 格子点データの経度・緯度・動径座標(X,Y,Z) (格子点データ型 3 次元配列)
  ! xvz_Lon, xvz_Lat, xvz_Rad :: 格子点データの経度・緯度・動径座標(X,Y,Z) (分散格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! xyz_wt, wt_xyz :: スペクトルデータと 3 次元格子データの間の変換 (球面調和函数, チェビシェフ変換)
  !
  ! xvz_wt, wt_xvz :: スペクトルデータと 3 次元分散格子データの間の変換 (球面調和函数, チェビシェフ変換)
  !
  ! xyz_wz, wz_xyz :: 3 次元格子データと水平スペクトル・動径格子データとの間の変換 (球面調和函数)
  !
  ! xvz_wz, wz_xvz :: 3 次元分散格子データと水平スペクトル・動径格子データとの間の変換 (球面調和函数)
  !
  ! wz_wt, wt_wz   :: スペクトルデータと水平スペクトル・動径格子データとの間の変換 (チェビシェフ変換)
  !
  ! w_xy, xy_w     :: スペクトルデータと 2 次元水平格子データの間の変換(球面調和函数変換) 
  ! w_xv, xv_w     :: スペクトルデータと 2 次元水平分散格子データの間の変換(球面調和函数変換) 
  !
  ! az_at, at_az   :: 同時に複数個行う (チェビシェフ変換)格子データとチェビシェフデータの間の変換を
  !
  ! l_nm, nm_l     :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wt_DRad_wt     :: スペクトルデータに動径微分∂/∂r を作用させる
  ! wt_DivRad_wt   :: スペクトルデータに発散型動径微分 1 /r^2 ∂/∂r r^2 = ∂/∂r + 2/r を作用させる
  !
  ! wt_RotRad_wt   :: スペクトルデータに回転型動径微分 1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  !
  ! wt_Lapla_wt    :: スペクトルデータにラプラシアンを作用させる
  !
  ! xyz_GradLon_wt :: スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λを作用させる
  ! xvz_GradLon_wt :: スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λを作用させる
  !
  ! xyz_GradLat_wt :: スペクトルデータに勾配型緯度微分 1/r・∂/∂φを作用させる
  ! xvz_GradLat_wt :: スペクトルデータに勾配型緯度微分 1/r・∂/∂φを作用させる(分散格子)
  ! wt_DivLon_xyz  :: 格子データに発散型経度微分 1/rcosφ・∂/∂λを作用させる
  ! wt_DivLon_xvz  :: 分散格子データに発散型経度微分 1/rcosφ・∂/∂λを作用させるa
  
  ! wt_DivLat_xyz  :: 格子データに発散型緯度微分 1/rcosφ・∂(g cosφ)/∂φを作用させる
  ! wt_DivLat_xvz  :: 分散格子データに発散型緯度微分 1/rcosφ・∂(g cosφ)/∂φを作用させる(分散格子)
  !
  ! wt_Div_xyz_xyz_xyz  :: ベクトル成分である 3 つの格子データに発散を作用させる
  ! wt_Div_xvz_xvz_xvz  :: ベクトル成分である 3 つの分散格子データに発散を作用させる
  ! xyz_Div_xyz_xyz_xyz :: ベクトル成分である 3 つの格子データに発散を作用させる
  ! xvz_Div_xvz_xvz_xvz :: ベクトル成分である 3 つの分散格子データに発散を作用させる
  ! xyz_RotLon_wt_wt  :: ベクトル場の回転の経度成分を計算する
  ! xvz_RotLon_wt_wt  :: ベクトル場の回転の経度成分を計算する(分散格子)
  ! xyz_RotLat_wt_wt  :: ベクトル場の回転の緯度成分を計算する
  ! xvz_RotLat_wt_wt  :: ベクトル場の回転の緯度成分を計算する(分散格子)
  ! wt_RotRad_xyz_xyz :: ベクトル場の回転の動径成分を計算する
  ! wt_RotRad_xvz_xvz :: ベクトル場の回転の動径成分を計算する(分散格子)
  !
  !==== トロイダルポロイダル計算用微分
  !
  ! wt_KxRGrad_wt   :: スペクトルデータに経度微分 k×r・▽ = ∂/∂λを作用させる
  !
  ! xyz_KGrad_wt    :: スペクトルデータに軸方向微分 k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を作用させる
  ! xvz_KGrad_wt    :: スペクトルデータに軸方向微分 k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を作用させる(分散格子)
  ! wt_L2_wt        :: スペクトルデータに L2 演算子 = -水平ラプラシアンを作用させる
  !
  ! wt_L2Inv_wt     :: スペクトルデータに L2 演算子の逆 = -逆水平ラプラシアンを作用させる
  !
  ! wt_QOperator_wt :: スペクトルデータに演算子 Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) を作用させる
  !
  ! wt_RadRot_xyz_xyz :: ベクトル v の渦度と動径ベクトル r の内積 r・(▽×v) を計算する
  ! wt_RadRot_xvz_xvz :: ベクトル v の渦度と動径ベクトル r の内積 r・(▽×v) を計算する(分散格子)
  ! wt_RadRotRot_xyz_xyz_xyz :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wt_RadRotRot_xvz_xvz_xvz :: ベクトルの v の r・(▽×▽×v) を計算する(分散格子)
  ! wt_Potential2Vector      :: トロイダルポロイダルポテンシャルからベクトル場を計算する
  ! wt_Potential2VectorMPI   :: トロイダルポロイダルポテンシャルからベクトル場を計算する(分散格子)
  !
  ! wt_Potential2Rotation    :: トロイダルポロイダルポテンシャルで表される非発散ベクトル場の回転の各成分を計算する
  ! wt_Potential2RotationMPI :: トロイダルポロイダルポテンシャルで表される非発散ベクトル場の回転の各成分を計算する(分散格子)
  !
  !
  !==== 非線形計算
  !
  ! wt_VGradV    ::  ベクトル v から v・▽v を計算する
  ! wt_VGradVMPI ::  ベクトル v から v・▽v を計算する(分散格子)
  !
  !==== ポロイダル/トロイダルモデル用スペクトル解析
  !
  ! nmz_ToroidalEnergySpectrum_wt, :: トロイダルポテンシャルからエネルギーの
  ! nz_ToroidalEnergySpectrum_wt   :: 球面調和函数各成分を計算する
  ! 
  ! nmz_PoloidalEnergySpectrum_wt, :: ポロイダルポテンシャルからエネルギーの
  ! nz_PoloidalEnergySpectrum_wt   :: 球面調和函数各成分を計算する
  !
  !==== 境界値問題
  !
  ! wt_BoundariesTau,    :: ディリクレ, ノイマン境界条件を適用する 
  ! wt_BoundariesGrid,   ::(タウ法, 選点法)
  ! wt_Boundaries        ::
  !
  ! wt_TorBoundariesTau,  :: 速度トロイダルポテンシャルの境界条件を
  ! wt_TorBoundariesGrid, :: 適用する(タウ法,選点法)            │
  ! wt_TorBoundaries      ::
  !
  ! wz_LaplaPol2Pol_wz,   :: 速度ポロイダルポテンシャルΦを▽^2Φから
  ! wt_LaplaPol2Pol_wt    :: 求める (入出力がそれぞれチェビシェフ格子点,
  !                       :: チェビシェフ係数)
  !
  ! wt_TorMagBoundariesTau,  :: 磁場トロイダルポテンシャルの境界条件を
  ! wt_TorMagBoundariesGrid, :: 適用する(タウ法, 選点法)
  ! wt_TorMagBoundaries      ::
  !
  ! wt_PolMagBoundariesTau,  :: 磁場トロイダルポテンシャル境界の境界条件を
  ! wt_PolMagBoundariesGrid, :: 適用する(タウ法, 選点法)
  ! wt_PolMagBoundaries      ::                                         
  !
  !==== 積分・平均(3 次元データ)
  !
  ! IntLonLatRad_xyz, AvrLonLatRad_xyz :: 3 次元格子点データの全領域積分および平均
  !
  ! z_IntLonLat_xyz, z_AvrLonLat_xyz :: 3 次元格子点データの緯度経度(水平・球面)積分および平均               
  !
  ! y_IntLonRad_xyz, y_AvrLonRad_xyz :: 3 次元格子点データの緯度動径積分および平均
  !
  ! z_IntLatRad_xyz, z_AvrLatRad_xyz :: 3 次元格子点データの経度動径(子午面)積分および平均              
  !
  ! yz_IntLon_xyz, yz_AvrLon_xyz :: 3 次元格子点データの経度方向積分および平均
  ! xz_IntLat_xyz, xz_AvrLat_xyz :: 3 次元格子点データの緯度方向積分および平均
  ! xz_IntRad_xyz, xz_AvrRad_xyz :: 3 次元格子点データの動径方向積分および平均
  !
  ! IntLonLatRad_xvz, AvrLonLatRad_xvz :: 3 次元格子点データの全領域積分および平均
  !
  ! z_IntLonLat_xvz, z_AvrLonLat_xvz :: 3 次元格子点データの緯度経度(水平・球面)積分および平均               
  !
  ! v_IntLonRad_xvz, v_AvrLonRad_xvz :: 3 次元格子点データの緯度動径積分および平均
  !
  ! z_IntLatRad_xvz, z_AvrLatRad_xvz :: 3 次元格子点データの経度動径(子午面)積分および平均              
  !
  ! vz_IntLon_xvz, vz_AvrLon_xvz :: 3 次元格子点データの経度方向積分および平均
  ! xz_IntLat_xvz, xz_AvrLat_xvz :: 3 次元格子点データの緯度方向積分および平均
  ! xy_IntRad_xvz, xy_AvrRad_xvz :: 3 次元格子点データの動径方向積分および平均
  !
  !==== 積分・平均(2 次元データ)
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの水平(球面)積分および平均
  ! IntLonRad_xz, AvrLonRad_xz :: 2 次元(XZ)格子点データの経度動径積分
  !                            :: および平均
  ! IntLatRad_yz, AvrLatRad_yz :: 2 次元(YZ)格子点データの緯度動径(子午面)
  !                            :: 積分および平均 
  ! y_IntLon_xy, y_AvrLon_xy   :: 水平 2 次元(球面)格子点データの経度方向
  !                            :: 積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 水平2 次元(球面)格子点データの緯度方向積分
  !                            :: および平均
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 次元(XZ)格子点データの経度方向積分および
  !                            :: 平均
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 次元(XZ)格子点データの動径方向積分および
  !                            :: 平均
  ! z_IntLat_yz, z_AvrLat_yz   :: 2 次元(YZ)格子点データの緯度方向積分および
  !                            :: 平均
  ! y_IntRad_yz, y_AvrRad_yz   :: 2 次元(YZ)格子点データの動径方向積分および
  !                            :: 平均                  
  !
  ! IntLonLat_xv, AvrLonLat_xv :: 2 次元格子点データの水平(球面)積分および平均
  ! IntLonRad_xz, AvrLonRad_xz :: 2 次元(XZ)格子点データの経度動径積分
  !                            :: および平均
  ! IntLatRad_vz, AvrLatRad_vz :: 2 次元(YZ)格子点データの緯度動径(子午面)
  !                            :: 積分および平均 
  ! v_IntLon_xv, v_AvrLon_xv   :: 水平 2 次元(球面)格子点データの経度方向
  !                            :: 積分および平均
  ! v_IntLat_xv, x_AvrLat_xv   :: 水平2 次元(球面)格子点データの緯度方向積分
  !                            :: および平均
  ! z_IntLon_xz, z_AvrLon_xz   :: 2 次元(XZ)格子点データの経度方向積分および
  !                            :: 平均
  ! x_IntRad_xz, x_AvrRad_xz   :: 2 次元(XZ)格子点データの動径方向積分および
  !                            :: 平均
  ! z_IntLat_vz, z_AvrLat_vz   :: 2 次元(YZ)格子点データの緯度方向積分および
  !                            :: 平均
  ! v_IntRad_vz, v_AvrRad_vz   :: 2 次元(YZ)格子点データの動径方向積分および
  !                            :: 平均                  
  !
  !==== 積分・平均(1 次元データ)
  !
  ! IntLon_x, AvrLon_x  :: 1 次元(X)格子点データの経度方向積分および平均
  ! IntLat_y, AvrLat_y  :: 1 次元(Y)格子点データの緯度方向積分および平均
  ! IntRad_z, AvrRad_z  :: 1 次元(Z)格子点データの動径方向積分および平均
  !
  ! IntLat_v, AvrLat_v  :: 1 次元(Y)格子点データの緯度方向積分および平均
  !
  !==== 補間計算
  !
  ! Interpolate_wt :: スペクトルデータから任意の点の値を補間する. 
  ! 
  use dc_message
  use lumatrix
  use mpi
  use w_base_mpi_module
  use w_deriv_mpi_module
  use w_integral_mpi_module
  use wa_base_mpi_module
  use wa_deriv_mpi_module
  use wt_module

  implicit none
  integer :: ierr

  private

  public wt_mpi_Initial
  public jc

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public v_Lat, v_Lat_Weight
  public z_Rad, z_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xv_Lon, xv_Lat
  public xyz_Lon, xyz_Lat, xyz_Rad
  public xvz_Lon, xvz_Lat, xvz_Rad
  public wz_Rad
  public wt_VMiss

  public w_xy, xy_w
  public w_xv, xv_w
  public at_Dr_at, t_Dr_t, az_at, at_az
  public xyz_wt, wt_xyz, xyz_wz, wz_xyz, wz_wt, wt_wz
  public xvz_wt, wt_xvz, xvz_wz, wz_xvz
  public wt_DRad_wt, wt_DivRad_wt, wt_RotRad_wt, wt_Lapla_wt
  public xyz_GradLon_wt, xyz_Gradlat_wt
  public xvz_GradLon_wt, xvz_Gradlat_wt
  public wt_DivLon_xyz, wt_DivLat_xyz
  public wt_DivLon_xvz, wt_DivLat_xvz
  public wt_Div_xyz_xyz_xyz, xyz_Div_xyz_xyz_xyz
  public wt_Div_xvz_xvz_xvz, xvz_Div_xvz_xvz_xvz
  public xyz_RotLon_wt_wt, xyz_RotLat_wt_wt, wt_RotRad_xyz_xyz
  public xvz_RotLon_wt_wt, xvz_RotLat_wt_wt, wt_RotRad_xvz_xvz

  public yz_IntLon_xyz, xz_IntLat_xyz, xy_IntRad_xyz
  public x_IntLatRad_xyz, y_IntLonRad_xyz, z_IntLonLat_xyz
  public IntLonLatRad_xyz

  public vz_IntLon_xvz, xz_IntLat_xvz, xv_IntRad_xvz
  public x_IntLatRad_xvz, v_IntLonRad_xvz, z_IntLonLat_xvz
  public IntLonLatRad_xvz

  public x_IntLat_xy, y_IntLon_xy, IntLonLat_xy
  public z_IntLat_yz, y_IntRad_yz, IntLatRad_yz
  public z_IntLon_xz, x_IntRad_xz, IntLonRad_xz
  public IntLon_x, IntLat_y, IntRad_z

  public x_IntLat_xv, v_IntLon_xv, IntLonLat_xv
  public z_IntLat_vz, v_IntRad_vz, IntLatRad_vz
  public IntLat_v

  public yz_AvrLon_xyz, xz_AvrLat_xyz, xy_AvrRad_xyz
  public x_AvrLatRad_xyz, y_AvrLonRad_xyz, z_AvrLonLat_xyz
  public AvrLonLatRad_xyz

  public vz_AvrLon_xvz, xz_AvrLat_xvz, xv_AvrRad_xvz
  public x_AvrLatRad_xvz, v_AvrLonRad_xvz, z_AvrLonLat_xvz
  public AvrLonLatRad_xvz

  public x_AvrLat_xy, y_AvrLon_xy, AvrLonLat_xy
  public z_AvrLat_yz, y_AvrRad_yz, AvrLatRad_yz
  public z_AvrLon_xz, x_AvrRad_xz, AvrLonRad_xz
  public AvrLon_x, AvrLat_y, AvrRad_z

  public x_AvrLat_xv, v_AvrLon_xv, AvrLonLat_xv
  public z_AvrLat_vz, v_AvrRad_vz, AvrLatRad_vz
  public AvrLat_v

  public wt_KxRGrad_wt, xvz_KGrad_wt, wt_L2_wt, wt_L2Inv_wt, wt_QOperator_wt
  public wt_RadRot_xyz_xyz, wt_RadRotRot_xyz_xyz_xyz
  public wt_RadRot_xvz_xvz, wt_RadRotRot_xvz_xvz_xvz
  public wt_Potential2vector, wt_Potential2Rotation
  public wt_Potential2vectorMPI, wt_Potential2RotationMPI
  public wt_VGradV
  public wt_VGradVMPI

  public Interpolate_wt

  public nmz_ToroidalEnergySpectrum_wt, nz_ToroidalEnergySpectrum_wt
  public nmz_PoloidalEnergySpectrum_wt, nz_PoloidalEnergySpectrum_wt

  public wt_Boundaries, wt_TorBoundaries, wz_LaplaPol2Pol_wz
  public wt_TormagBoundaries, wt_PolmagBoundaries

  public wt_BoundariesTau, wt_TorBoundariesTau
  public wt_TormagBoundariesTau, wt_PolmagBoundariesTau

  public wt_BoundariesGrid, wt_TorBoundariesGrid, wt_LaplaPol2PolGrid_wt
  public wt_TormagBoundariesGrid, wt_PolmagBoundariesGrid

  integer            :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer            :: nm=21, lm=16         ! 切断波数の設定(水平, 動径)
  real(8)            :: ri=0.0, ro=1.0       ! 球殻内外半径
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: xvz_LON, xvz_LAT, xvz_RAD ! 座標

  save im, jm, km, nm, lm, ri, ro

  contains
  !--------------- 初期化 -----------------
   subroutine wt_mpi_Initial(i,j,k,n,l,r_in,r_out,np)
     !
     ! スペクトル変換の格子点数, 波数, 動径座標の範囲を設定する.
     !
     ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
     ! しなければならない. 
     !
     integer,intent(in) :: i              ! 格子点数(経度λ)
     integer,intent(in) :: j              ! 格子点数(緯度φ)
     integer,intent(in) :: k              ! 格子点数(動径 r)
     integer,intent(in) :: n              ! 切断波数(水平全波数)
     integer,intent(in) :: l              ! 切断波数(動径波数)

     real(8),intent(in) :: r_in           ! 球殻内半径
     real(8),intent(in) :: r_out          ! 球殻外半径

     integer,intent(in), optional :: np   ! OPENMP での最大スレッド数

     im = i  ; jm = j ; km = k
     nm = n  ; lm = l
     ri = r_in ; ro = r_out

     if ( present(np) ) then
        call wt_Initial(im,jm,km,nm,lm,ri,ro,np=np)
     else
        call wt_Initial(im,jm,km,nm,lm,ri,ro)
     endif
     call w_base_mpi_Initial
     call w_deriv_mpi_Initial
     call wa_base_mpi_Initial

     allocate(xvz_Lon(0:im-1,jc,0:km))
     allocate(xvz_Lat(0:im-1,jc,0:km))
     allocate(xvz_Rad(0:im-1,jc,0:km))

     xvz_Lon = spread(xv_Lon,3,km+1)
     xvz_Lat = spread(xv_Lat,3,km+1)
     xvz_Rad = spread(spread(z_Rad,1,jc),1,im)

   end subroutine wt_mpi_Initial

  !--------------- 基本変換 -----------------

    function xvz_wt(wt)
      !
      ! スペクトルデータから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_wt
      !(out) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      xvz_wt = xva_wa(az_at(wt))

    end function xvz_wt

    function wt_xvz(xvz)
      !
      ! 3 次元格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wt_xvz
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,jc,0:km), intent(in)         :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      wt_xvz = at_az(wa_xva(xvz))

    end function wt_xvz

    function xvz_wz(wz)
      !
      ! 水平スペクトル・動径格子点データから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_wz
      !(out) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:km), intent(in) :: wz
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ

      xvz_wz = xva_wa(wz)

    end function xvz_wz

    function wz_xvz(xvz)
      !
      ! 3 次元格子データから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:km)             :: wz_xvz
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ

      real(8), dimension(0:im-1,jc,0:km), intent(in)         :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      wz_xvz = wa_xva(xvz)

    end function wz_xvz

  !--------------- 微分計算 -----------------
    function xvz_GradLon_wt(wt)
      !
      ! スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λ
      ! を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_GradLon_wt
      !(out) 勾配型経度微分を作用された 2 次元スペクトルデータ

      xvz_GradLon_wt = xva_GradLon_wa(wz_wt(wt))/xvz_Rad

    end function xvz_GradLon_wt

    function xvz_GradLat_wt(wt) 
      !
      ! スペクトルデータに勾配型経度微分 1/r ∂/∂φ を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_GradLat_wt
      !(out) 勾配型緯度微分を作用された 2 次元スペクトルデータ

      xvz_GradLat_wt = xva_GradLat_wa(wz_wt(wt))/xvz_Rad
    end function xvz_GradLat_wt

    function wt_DivLon_xvz(xvz)
      ! 
      ! 格子点データに発散型経度微分 1/rcosφ・∂/∂λ を作用させた
      ! スペクトルデータを返す.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in)   :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:lm)       :: wt_DivLon_xvz
      !(out) 発散型経度微分を作用された 2 次元スペクトルデータ

      wt_DivLon_xvz = wt_wz(wa_DivLon_xva(xvz/xvz_Rad))
    end function wt_DivLon_xvz

    function wt_DivLat_xvz(xvz)
      !
      ! 格子データに発散型緯度微分 1/rcosφ・∂(f cosφ)/∂φ を
      ! 作用させたスペクトルデータを返す.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in)   :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:lm)       :: wt_DivLat_xvz
      !(out) 発散型緯度微分を作用された 2 次元スペクトルデータ

      wt_DivLat_xvz = wt_wz(wa_divlat_xva(xvz/xvz_Rad))
    end function wt_DivLat_xvz

    function wt_Div_xvz_xvz_xvz(xvz_Vlon,xvz_Vlat,xvz_Vrad)
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
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_Div_xvz_xvz_xvz
      !(out) ベクトル場の発散

      wt_Div_xvz_xvz_xvz =   wt_DivLon_xvz(xvz_Vlon) &
                           + wt_DivLat_xvz(xvz_Vlat) &
                           + wt_DivRad_wt(wt_xvz(xvz_Vrad))

    end function wt_Div_xvz_xvz_xvz

    function xvz_Div_xvz_xvz_xvz(xvz_Vlon,xvz_Vlat,xvz_Vrad)
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
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,jc,0:km)             :: xvz_Div_xvz_xvz_xvz
      !(out) ベクトル場の発散

      xvz_Div_xvz_xvz_xvz &
           = xvz_Rad/cos(xvz_Lat) &
                * xvz_wt(wt_Div_xvz_xvz_xvz(xvz_VLon*cos(xvz_Lat)/xvz_Rad,  &
                                            xvz_VLat*cos(xvz_Lat)/xvz_Rad,  &
                                            xvz_VRad*cos(xvz_Lat)/xvz_Rad ))&
             + xvz_VLat*tan(xvz_Lat)/xvz_Rad &
             + xvz_VRad/xvz_Rad

    end function xvz_Div_xvz_xvz_xvz

    function xvz_RotLon_wt_wt(wt_Vrad,wt_Vlat) 
      !
      ! ベクトル場の動径成分, 緯度成分である第 1, 2 引数 Vrad, Vlat から
      ! 回転の経度成分 
      !
      !    1/r ∂Vrad/∂φ-1/r ∂(r Vlat)/∂r を計算する.
      !
      ! を計算する
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_RotLon_wt_wt
      !(out) ベクトル場の回転の経度成分

        xvz_RotLon_wt_wt =   xvz_GradLat_wt(wt_Vrad) &
                           - xvz_wt(wt_RotRad_wt(wt_Vlat))

    end function xvz_RotLon_wt_wt

    function xvz_RotLat_wt_wt(wt_Vlon,wt_Vrad) 
      !
      ! ベクトル場の経度成分, 動径成分である第 1, 2 引数 Vlon, Vrad から
      ! 回転の緯度成分 
      !
      !    1/r ∂(r Vlon)/∂r - 1/rcosφ・∂Vrad/∂λ
      !
      ! を計算する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_RotLat_wt_wt
      !(out) ベクトル場の回転の緯度成分

        xvz_RotLat_wt_wt =   xvz_wt(wt_RotRad_wt(wt_Vlon)) &
                           - xvz_GradLon_wt(wt_Vrad) 

    end function xvz_RotLat_wt_wt

    function wt_RotRad_xvz_xvz(xvz_Vlat,xvz_Vlon) 
      !
      ! ベクトルの緯度成分, 経度成分である第 1, 2 引数 Vlat, Vlon に対して
      ! ベクトル場の回転の動径成分 
      !
      !    1/rcosφ・∂Vlat/∂λ - 1/rcosφ・∂(Vlon cosφ)/∂φ
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RotRad_xvz_xvz
      !(out) ベクトル場の回転の動径成分

        wt_RotRad_xvz_xvz =   wt_DivLon_xvz(xvz_Vlat) &
                            - wt_DivLat_xvz(xvz_Vlon)

    end function wt_RotRad_xvz_xvz

  !--------------- ポロイダル/トロイダルモデル用微分 -----------------

    function xvz_KGrad_wt(wt)    ! k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r
      !
      ! 入力スペクトルデータに対応する格子データに軸方向微分 
      !
      !    k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r 
      !
      ! を作用させた格子データが返される. 
      ! ここでベクトル k は球の中心から北極向きの単位ベクトルである.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,jc,0:km)                     :: xvz_KGrad_wt
      !(out) 軸方向微分を作用された 2 次元スペクトルデータ

      xvz_KGrad_wt =  cos(xvz_Lat)*xvz_GradLat_wt(wt) &
                    + sin(xvz_Lat)*xvz_wt(wt_Drad_wt(wt))
    end function xvz_KGrad_wt

    function wt_RadRot_xvz_xvz(xvz_VLON,xvz_VLAT)  ! r・(▽×v)
      !
      ! ベクトルの渦度と動径ベクトルの内積 r・(▽×v) を計算する.
      !
      ! 第 1, 2 引数(v[λ], v[φ])がそれぞれベクトルの経度成分, 緯度成分を表す.
      !
      !    r・(▽×v) = 1/cosφ・∂v[φ]/∂λ - 1/cosφ・∂(v[λ] cosφ)/∂φ
      !
      ! のスペクトル データが返される.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RadRot_xvz_xvz
      !(out) ベクトルの渦度と動径ベクトルの内積

      wt_RadRot_xvz_xvz = wt_wz(wa_DivLon_xva(xvz_VLAT) &
                                - wa_DivLat_xva(xvz_VLON))
      
    end function wt_RadRot_xvz_xvz

    function wt_RadRotRot_xvz_xvz_xvz(xvz_VLON,xvz_VLAT,xvz_VRAD) 
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
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz_VRAD
      !(in) ベクトルの動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wt_RadRotRot_xvz_xvz_xvz
      !(out) ベクトル v の r・(▽×▽×v) 

      wt_RadRotRot_xvz_xvz_xvz = &
               wt_RotRad_wt(wt_wz( &
                   (wa_DivLon_xva(xvz_VLON)+ wa_DivLat_xva(xvz_VLAT)))) &
             + wt_L2_wt(wt_xvz(xvz_VRAD/xvz_RAD))

    end function wt_RadRotRot_xvz_xvz_xvz

    subroutine wt_Potential2VectorMPI(&
         xvz_VLON,xvz_VLAT,xvz_VRAD,wt_TORPOT,wt_POLPOT)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψr) + ▽x▽x(Φr) 
      !
      ! の各成分を計算する
      !
      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VLON
      !(out) ベクトル場の経度成分

      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VLAT
      !(out) ベクトル場の緯度成分

      real(8), dimension(0:im-1,jc,0:km)     :: xvz_VRAD
      !(out) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      xvz_VLON =   xvz_RAD * xvz_GradLat_wt(wt_TORPOT) &
                 + xva_GradLon_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xvz_VLAT = - xvz_RAD * xvz_GradLon_wt(wt_TORPOT) &
                 + xva_GradLat_wa(wz_wt(wt_RotRad_wt(wt_POLPOT)))
      xvz_VRAD = xvz_wt(wt_L2_wt(wt_POLPOT))/xvz_RAD

    end subroutine wt_Potential2VectorMPI

    subroutine wt_Potential2RotationMPI(&
       xvz_RotVLON,xvz_RotVLAT,xvz_RotVRAD,wt_TORPOT,wt_POLPOT)
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
      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVLON
      !(out) 回転の経度成分

      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVLAT
      !(out) 回転の緯度成分

      real(8), dimension(0:im-1,jc,0:km), intent(OUT) :: xvz_RotVRAD
      !(out) 回転の動径成分

      ! 入力ベクトル場を表すポテンシャル
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wt_POLPOT
      !(in) ポロイダルポテンシャル

      call wt_Potential2VectorMPI( &
           xvz_RotVLON,xvz_RotVLAT,xvz_RotVRAD, &
           -wt_Lapla_wt(wt_POLPOT), wt_TORPOT)

    end subroutine wt_Potential2RotationMPI

 !------------------- 非線形項計算 ----------------------
    subroutine wt_VGradVMPI(xvz_VGRADV_LON,xvz_VGRADV_LAT,xvz_VGRADV_RAD, &
                            xvz_VLON,xvz_VLAT,xvz_VRAD )
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
      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LON
      !(out) (v・▽v) 経度成分

      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_LAT
      !(out) (v・▽v) 緯度成分

      real(8), dimension(0:im-1,jc,0:km),intent(out)   :: xvz_VGRADV_RAD
      !(out) (v・▽v) 動径成分

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLON
      !(in) ベクトル場 v の経度成分

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VLAT
      !(in) ベクトル場 v の緯度成分

      real(8), dimension(0:im-1,jc,0:km),intent(in)    :: xvz_VRAD
      !(in) ベクトル場 v の動径成分

      xvz_VGRADV_LON = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VLON * xvz_VLON, xvz_VLON*xvz_VLAT, xvz_VLON*xvz_VRAD ) &
            + xvz_VLON*xvz_VRAD/xvz_RAD              &
            - xvz_VLON*xvz_VLAT*tan(xvz_LAT)/xvz_RAD 

      xvz_VGRADV_LAT = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VLAT*xvz_VLON, xvz_VLAT*xvz_VLAT, xvz_VLAT*xvz_VRAD ) &
            + xvz_VLAT*xvz_VRAD/xvz_RAD        &
            + xvz_VLON**2*tan(xvz_LAT)/xvz_RAD 

      xvz_VGRADV_RAD = &
              xvz_Div_xvz_xvz_xvz( &
                  xvz_VRAD*xvz_VLON, xvz_VRAD*xvz_VLAT, xvz_VRAD*xvz_VRAD ) &
            - (xvz_VLON**2 + xvz_VLAT**2)/xvz_RAD 

    end subroutine wt_VGradVMPI

  !--------------- 積分計算 -----------------
    !----(入力データ xvz)---
    function vz_IntLon_xvz(xvz)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(jc,0:km)  :: vz_IntLon_xvz
      !(out) 経度方向(帯状)積分された 2 次元子午面格子点データ

      integer :: i

      vz_IntLon_xvz = 0.0d0
      do i=0,im-1
         vz_IntLon_xvz(:,:) = vz_IntLon_xvz(:,:) &
                       + xvz(i,:,:) * x_Lon_Weight(i)
      enddo
    end function vz_IntLon_xvz

    function xz_IntLat_xvz(xvz)
      !
      ! 3 次元格子点データの緯度方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xz_IntLat_xvz        ! 緯度円格子点データ
      !(out) 緯度積分された 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1,0:km)  :: xz_IntLatTMP
      integer :: j

      xz_IntLat_xvz = 0.0d0
      do j=1,jc
         xz_IntLat_xvz(:,:) = xz_IntLat_xvz(:,:) &
                       + xvz(:,j,:) * v_Lat_Weight(j)
      enddo
      xz_IntLatTmp=xz_IntLat_xvz
      CALL MPI_ALLREDUCE(xz_IntLatTMP,xz_IntLat_xvz,im*(km+1),MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function xz_IntLat_xvz

    function xv_IntRad_xvz(xvz)  ! 動径積分
      !
      ! 3 次元格子点データの動径方向域積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dr を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,jc)  :: xv_IntRad_xvz
      !(out) 動径積分された 2 次元経度緯度(水平, 球面)格子点データ

      integer :: k

      xv_IntRad_xvz = 0.0d0
      do k=0,km
         xv_IntRad_xvz(:,:) = xv_IntRad_xvz(:,:) &
                       + xvz(:,:,k) * z_Rad_Weight(k) 
      enddo
    end function xv_IntRad_xvz

    function x_IntLatRad_xvz(xvz)
      !
      ! 3 次元格子点データの緯度動径(子午面)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_IntLatRad_xvz
      !(out) 緯度動径(子午面)積分された 1 次元経度格子点データ

      real(8), dimension(0:im-1)     :: x_IntLatRadTMP
      integer :: j, k

      x_IntLatRad_xvz = 0
      do k=0,km
         do j=1,jc
            x_IntLatRad_xvz = x_IntLatRad_xvz &
                 + xvz(:,j,k) * v_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo

      x_IntLatRadTmp=x_IntLatRad_xvz
      CALL MPI_ALLREDUCE(x_IntLatRadTMP,x_IntLatRad_xvz,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function x_IntLatRad_xvz

    function v_IntLonRad_xvz(xvz)
      !
      ! 3 次元格子点データの経度動径(緯度円)積分.
      !
      ! 3 次元データ f(λ,φ,r) に対して∫f(λ,φ,r) r^2dλdr を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(jc)       :: v_IntLonRad_xvz
      !(out) 経度動径(緯度円)積分された 1 次元緯度格子点データ

      integer :: i, k

      v_IntLonRad_xvz = 0
      do k=0,km
         do i=0,im-1
            v_IntLonRad_xvz = v_IntLonRad_xvz &
                 + xvz(i,:,k) * x_Lon_Weight(i) * z_Rad_Weight(k)
         enddo
      enddo
    end function v_IntLonRad_xvz

    function z_IntLonLat_xvz(xvz)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: z_IntLonLat_xvz
      !(out) 緯度経度(水平, 球面)積分された 1 次元動径格子点データ

      real(8), dimension(0:km)     :: z_IntLonLatTMP
      integer :: i, j

      z_IntLonLat_xvz = 0
      do j=1,jc
         do i=0,im-1
            z_IntLonLat_xvz = z_IntLonLat_xvz &
                 + xvz(i,j,:) * x_Lon_Weight(i) * v_Lat_Weight(j)
         enddo
      enddo

      z_IntLonLatTmp=z_IntLonLat_xvz
      CALL MPI_ALLREDUCE(z_IntLonLatTMP,z_IntLonLat_xvz,km+1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntLonLat_xvz

    function IntLonLatRad_xvz(xvz) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2cosφ dλdφdr 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz 
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: IntLonLatRad_xvz 
      !(out) 全球積分値

      real(8)                     :: IntLonLatRadTMP
      integer :: i, j, k

      IntLonLatRad_xvz = 0
      do k=0,km
         do j=1,jc
            do i=0,im-1
               IntLonLatRad_xvz = IntLonLatRad_xvz &
                    + xvz(i,j,k) * x_Lon_Weight(i) &
                         * v_Lat_Weight(j) * z_Rad_Weight(k)
            enddo
         enddo
      enddo

      IntLonLatRadTmp=IntLonLatRad_xvz
      CALL MPI_ALLREDUCE(IntLonLatRadTMP,IntLonLatRad_xvz,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLonLatRad_xvz

    !----(入力データ vz)---
    function z_IntLat_vz(vz)  ! 緯度積分
      !
      ! 2 次元(VZ)格子点データの緯度方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) cosφ dφ を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: z_IntLat_vz
      !(out) 緯度積分された 1 次元動径格子点データ

      real(8), dimension(0:km)  :: z_IntLatTMP
      integer :: j

      z_IntLat_vz = 0.0d0
      do j=1,jc
         z_IntLat_vz(:) = z_IntLat_vz(:) + vz(j,:) * v_Lat_Weight(j)
      enddo
      z_IntLatTmp=z_IntLat_vz
      CALL MPI_ALLREDUCE(z_IntLatTMP,z_IntLat_vz,km+1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntLat_vz

    function v_IntRad_vz(vz)  ! 動径積分
      !
      ! 2 次元(VZ)格子点データの動径方向域積分.
      !
      ! 2 次元データ f(φ,r) に対して∫f(φ,r) r^2dr を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(jc)  :: v_IntRad_vz
      !(out) 動径積分された 1 次元緯度格子点データ

      integer :: k

      v_IntRad_vz = 0.0d0
      do k=0,km
         v_IntRad_vz(:) = v_IntRad_vz(:) &
                       + vz(:,k) * z_Rad_Weight(k) 
      enddo
    end function v_IntRad_vz

    function IntLatRad_vz(vz)
      !
      ! 2 次元(VZ)格子点データの緯度動径積分(子午面)および平均
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2cosφ dφdr を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: IntLatRad_vz
      !(out) 積分値

      real(8)                   :: IntLatRadTMP
      integer :: j, k

      IntLatRad_vz = 0
      do k=0,km
         do j=1,jc
            IntLatRad_vz = IntLatRad_vz &
                 + vz(j,k) * v_Lat_Weight(j) * z_Rad_Weight(k)
         enddo
      enddo

      IntLatRadTmp=IntLatRad_vz
      CALL MPI_ALLREDUCE(IntLatRadTMP,IntLatRad_vz,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntLatRad_vz

  !--------------- 平均計算 -----------------
    !----(入力データ xvz)---
    function vz_AvrLon_xvz(xvz)  ! 経度(帯状)積分
      !
      ! 3 次元格子点データの経度方向(帯状)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)dλ/2π を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(jc,0:km)  :: vz_AvrLon_xvz
      !(out) 経度方向(帯状)平均された 2 次元子午面格子点データ

      vz_AvrLon_xvz = vz_IntLon_xvz(xvz)/sum(x_Lon_Weight)

    end function vz_AvrLon_xvz

    function xz_AvrLat_xvz(xvz)  ! 緯度積分
      !
      ! 3 次元格子点データの緯度方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)cosφ dφ/2 を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(im,0:km)  :: xz_AvrLat_xvz
      !(out) 緯度平均された 2 次元緯度動径格子点データ

      xz_AvrLat_xvz = xz_IntLat_xvz(xvz)/sum(y_Lat_Weight)

    end function xz_AvrLat_xvz

    function xv_AvrRad_xvz(xvz)
      !
      ! 3 次元格子点データの動径方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して 
      !
      !    ∫f(λ,φ,r) r^2dr/((r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,jc)  :: xv_AvrRad_xvz          ! 水平格子点データ
      !(out) 動径平均された 2 次元経度緯度(水平, 球面)格子点データ

      xv_AvrRad_xvz = xv_IntRad_xvz(xvz)/sum(z_Rad_Weight)

    end function xv_AvrRad_xvz

    function x_AvrLatRad_xvz(xvz)  ! 緯度動径(子午面)積分
      !
      ! 3 次元格子点データの緯度動径(子午面)平均
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_AvrLatRad_xvz
      !(out) 緯度動径(子午面)平均された 1 次元経度格子点データ

      x_AvrLatRad_xvz = x_IntLatRad_xvz(xvz) &
                   /( sum(y_Lat_Weight)*sum(z_Rad_Weight) )

    end function x_AvrLatRad_xvz

    function v_AvrLonRad_xvz(xvz)  ! 経度動径(緯度円)積分
      !
      ! 3 次元格子点データの経度動径(緯度円)平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !     ∫f(λ,φ,r) r^2dλdr /(2π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(jc)       :: v_AvrLonRad_xvz
      !(out) 経度動径(緯度円)平均された 1 次元緯度格子点データ

      v_AvrLonRad_xvz = v_IntLonRad_xvz(xvz) &
                 /(sum(x_Lon_Weight)*sum(z_Rad_Weight))

    end function v_AvrLonRad_xvz

    function z_AvrLonLat_xvz(xvz)  ! 緯度経度(水平)積分
      !
      ! 3 次元格子点データの緯度経度(水平, 球面)積分
      ! 
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) cosφ dλdφ /4π 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:km)     :: z_AvrLonLat_xvz
      !(out) 緯度経度(水平, 球面)平均された 1 次元動径格子点データ

      z_AvrLonLat_xvz = z_IntLonLat_xvz(xvz) &
                 /(sum(x_Lon_Weight)*sum(y_Lat_Weight))

    end function z_AvrLonLat_xvz

    function AvrLonLatRad_xvz(xvz) ! 緯度経度動径(全球)積分
      !
      ! 3 次元格子点データの緯度経度動径(全球)積分
      !
      ! 3 次元データ f(λ,φ,r) に対して
      !
      !    ∫f(λ,φ,r) r^2cosφ dλdφdr /(4π(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,jc,0:km), intent(in) :: xvz
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: AvrLonLatRad_xvz
      !(out) 全球平均値

      AvrLonLatRad_xvz = IntLonLatRad_xvz(xvz) &
            /(sum(x_Lon_Weight)*sum(y_Lat_Weight) * sum(z_Rad_Weight))

    end function AvrLonLatRad_xvz

    !----(入力データ vz)---
    function z_AvrLat_vz(vz)
      !
      ! 2 次元(VZ)格子点データの緯度方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) cosφ dφ/2 を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(0:km)  :: z_AvrLat_vz
      !(out) 緯度平均された 1 次元動径格子点データ

      z_AvrLat_vz = z_IntLat_vz(vz)/sum(y_Lat_Weight)
    end function z_AvrLat_vz

    function v_AvrRad_vz(vz)
      !
      ! 2 次元(VZ)格子点データの動径方向域平均.
      !
      ! 2 次元データ f(φ,r) に対して ∫f(φ,r) r^2dr /((r[o]^3-r[i]^3)/3) 
      ! を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(jc)  :: v_AvrRad_vz
      !(out) 動径平均された 1 次元緯度格子点データ

      v_AvrRad_vz = v_IntRad_vz(vz)/sum(z_Rad_Weight)

    end function v_AvrRad_vz

    function AvrLatRad_vz(vz)  ! 緯度動径(子午面)積分
      !
      ! 2 次元(VZ)格子点データの緯度動径(子午面)平均
      !
      ! 2 次元データ f(φ,r) に対して
      !
      !    ∫f(φ,r) r^2cosφ dφdr /(2(r[o]^3-r[i]^3)/3) 
      !
      ! を計算する.
      !
      real(8), dimension(jc,0:km), intent(in) :: vz
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: AvrLatRad_vz
      !(out) 平均値

      AvrLatRad_vz = IntLatRad_vz(vz)/(sum(y_Lat_Weight)*sum(z_Rad_Weight))

    end function AvrLatRad_vz

end module wt_mpi_module
