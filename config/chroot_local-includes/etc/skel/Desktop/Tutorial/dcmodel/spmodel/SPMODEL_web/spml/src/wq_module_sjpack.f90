!--
!----------------------------------------------------------------------
! Copyright(c) 2009-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  wq_module_sjpack
!
!    spml/wq_module_sjpack モジュールは球内での流体運動をスペクトル法に
!    よって数値計算するための Fortran90 関数を提供するものである. 
!
!    水平方向に球面調和函数変換および動径方向に
!    Matsushima and Marcus (1994) で提唱された多項式を用いた
!    スペクトル計算のためのさまざまな関数を提供する. 
!
!    内部で wa_module_sjpack, aq_module を用いている. 
!    最下部では球面調和変換変換のエンジンとして 
!    ISPACK の Fortran77 サブルーチンを用いている.
!
!    関数, サブルーチンの名前と機能は wq_module のものと同じである. 
!    したがって use 文を wq_module から wq_module_sjpack に
!    変更するだけで SJPACK の機能が使えるようになる. 
! 
!    ただし l_nm, nm_l の使い方には注意されたい. wq_module の l_nm,
!    nm_l はwq_Initial で初期化しなくとも用いることができる(結果が切断
!    波数に依らない)が, wq_module_sjpack のものは初期化したのちにしか使
!    うことができない.
!
!    Matsushima and Marcus (1994) の多項式に関する説明は 
!    doc/spectral_radial.tex を参照のこと. 
!
!
!履歴  2008/04/03  竹広真一  wu_module より改変
!      2008/05/02  竹広真一  コメント追加
!      2008/07/01  佐々木洋平 水平方向の格子点を 0:im-1,1:jm に修正
!      2008/07/04  佐々木洋平 コメントを RDoc 用に微修正
!      2008/07/20  竹広真一  wq_Rad2_wq, wq_Rad2Inv_wq, wq_Lapla_wq 追加
!      2008/07/21  竹広真一  行列設定計算変更
!      2009/01/09  竹広真一  wu_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2009/12/06  竹広真一  先頭コメントを修正, threadprivate コメントアウト
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!凡例
!      データ種類と index
!        x : 経度         y : 緯度        r : 動径
!        w : 球面調和関数スペクトル
!        n : 球面調和関数スペクトル(水平全波数)
!        m : 球面調和関数スペクトル(帯状波数)
!        q : スペクトル関数スペクトル
!        a : 任意の次元
!
!        xyr : 3 次元格子点データ
!        xy  : 水平 2 次元格子点データ
!        yr  : 子午面 2 次元格子点データ
!        xr  : 緯度面 2 次元格子点データ
!
!        wr  : 水平スペクトル動径格子点データ
!        wq  : スペクトルデータ
!
!++
module wq_module_sjpack
  !
  != wq_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wq_module_sjpack.f90,v 1.2 2010-03-02 10:09:06 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wq_module_sjpack モジュールは球内での流体運動をスペクトル法に
  ! よって数値計算するための Fortran90 関数を提供するものである. 
  !
  ! 水平方向に球面調和函数変換および動径方向に
  ! Matsushima and Marcus (1994) で提唱された多項式を用いた
  ! スペクトル計算のためのさまざまな関数を提供する. 
  !
  ! 内部で wa_module, aq_module を用いている. 
  ! 最下部では球面調和変換のエンジンとして 
  ! ISPACK の Fortran77 サブルーチンを用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (wq_, nmr_, nr_, xyr_, wr_, w_, xy_, x_, y_, r_, a_) は, 
  !   返す値の形を示している.
  !   wq_  :: スペクトルデータ(球面調和函数・チェビシェフ変換)
  !   nmr_ :: 水平スペクトルデータ(全波数 n, 帯状波数各成分, 動径)
  !   nr_  :: 水平スペクトルデータ(全波数 n, 動径)
  !   xyr_ :: 3 次元格子点データ(経度・緯度・動径)
  !   wr_  :: 水平スペクトル, 動径格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後 (wq_, xyz_, wr_, w_, xy_, x_, y_, r_, a_) は, 入力変数の
  !   形がスペクトルデータおよび格子点データであることを示している.
  !   _wq      :: スペクトルデータ
  !   _xyr     :: 3 次元格子点データ
  !   _xyr_xyr :: 2 つの3 次元格子点データ, ...
  !
  !=== 各データの種類の説明
  !
  ! * xyr : 3 次元格子点データ(経度・緯度・動径)
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,km). 
  !   * im, jm, km はそれぞれ経度, 緯度, 動径座標の格子点数であり, 
  !     サブルーチン wq_Initial にてあらかじめ設定しておく.
  !
  ! * wq : スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),0:lm). 
  !   * nm は球面調和函数の最大全波数, lm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン wq_Initial にてあらかじめ設定しておく. 
  !   * 水平スペクトルデータの格納のされ方は関数 l_nm, nm_l によって調べる
  !     ことができる. 
  !   * 動径スペクトルデータの格納方法については aq_module.f90 を
  !     参照のこと. km < 2*im でなければならない. 
  !
  ! * nmr : 水平スペクトルデータの並んだ 3 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,km). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数, 第 3 次元が動径座標を表す. 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wq_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * nr : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,km). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wq_Initial にてあらかじめ設定しておく.
  !
  ! * wr : 水平スペクトル, 動径格子点データ.
  !   * 変数の種類と次元は real(8), dimension((nm+1)*(nm+1),km).
  !
  ! * wq_ で始まる関数が返す値はスペクトルデータに同じ.
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
  ! wq_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
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
  ! xyr_wq, wq_xyr :: スペクトルデータと 3 次元格子データの間の変換
  !                   (球面調和函数, チェビシェフ変換)
  !
  ! xyr_wr, wr_xyr :: 3 次元格子データと水平スペクトル・動径格子データとの
  !                   間の変換 (球面調和函数)
  !
  ! wr_wq, wq_wr   :: スペクトルデータと水平スペクトル・動径格子データとの
  !                   間の変換 (チェビシェフ変換)
  !
  ! w_xy, xy_w     :: スペクトルデータと 2 次元水平格子データの
  !                   間の変換(球面調和函数変換) 
  !
  ! l_nm, nm_l     :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wq_RadDRad_wq       :: スペクトルデータに動径微分 r∂/∂r を作用させる
  ! wr_DivRad_wq        :: スペクトルデータに発散型動径微分
  !                        1/r^2 ∂/∂r r^2 = ∂/∂r + 2/r を作用させる
  ! wr_RotDRad_wq       :: スペクトルデータに回転型動径微分
  !                        1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  ! wr_RotDRad_wr       :: スペクトルデータに回転型動径微分
  !                        1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  ! wq_RotDRad_wr       :: スペクトルデータに回転型動径微分
  !                        1/r ∂/∂rr = ∂/∂r + 1/r を作用させる
  ! wq_Lapla_wq         :: スペクトルデータにラプラシアンを作用させる
  ! xyr_GradLon_wq      :: スペクトルデータに勾配型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! xyr_GradLat_wq      :: スペクトルデータに勾配型緯度微分
  !                        1/r・∂/∂φを作用させる
  ! wr_DivLon_xyr       :: 格子データに発散型経度微分
  !                        1/rcosφ・∂/∂λを作用させる
  ! wr_DivLat_xyr       :: 格子データに発散型緯度微分
  !                        1/rcosφ・∂(g cosφ)/∂φを作用させる
  ! wr_Div_xyr_xyr_xyr  :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyr_Div_xyr_xyr_xyr :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  ! xyr_RotLon_wq_wq    :: ベクトル場の回転の経度成分を計算する
  ! xyr_RotLat_wq_wq    :: ベクトル場の回転の緯度成分を計算する
  ! wr_RotRad_xyr_xyr   :: ベクトル場の回転の動径成分を計算する
  !
  !==== トロイダルポロイダル計算用微分
  !
  ! wq_KxRGrad_wq     :: スペクトルデータに経度微分
  !                      k×r・▽ = ∂/∂λを作用させる
  ! xyr_KGrad_wq      :: スペクトルデータに軸方向微分
  !                      k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r を作用させる
  ! wq_L2_wq          :: スペクトルデータに L2 演算子 = -水平ラプラシアンを
  !                      作用させる
  ! wq_L2Inv_wq       :: スペクトルデータに L2 演算子の逆 = -逆水平ラプラシアン
  !                      を作用させる
  ! wq_QOperator_wq   :: スペクトルデータに演算子
  !                      Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) を作用させる
  ! wr_RadRot_xyr_xyr :: ベクトル v の渦度と動径ベクトル r の内積 r・(▽×v) を
  !                      計算する
  ! wr_RadRotRot_xyr_xyr_xyr :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wq_RadRotRot_xyr_xyr_xyr :: ベクトルの v の r・(▽×▽×v) を計算する
  ! wq_Potential2Vector      :: トロイダルポロイダルポテンシャルから
  !                             ベクトル場を計算する
  ! wq_Potential2Rotation    :: トロイダルポロイダルポテンシャルで表される
  !                             非発散ベクトル場の回転の各成分を計算する
  !
  !==== ポロイダル/トロイダルモデル用スペクトル解析
  !
  ! nmr_ToroidalEnergySpectrum_wq, nr_ToroidalEnergySpectrum_wq  ::
  !     トロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  ! nmr_PoloidalEnergySpectrum_wq, nr_PoloidalEnergySpectrum_wq  ::
  !     ポロイダルポテンシャルからエネルギーの球面調和函数各成分を計算する
  !
  !==== 境界値問題
  !
  ! wq_BoundaryTau, wr_BoundaryGrid, wq_Boundary                         ::
  !     ディリクレ, ノイマン境界条件を適用する(タウ法, 選点法)
  ! wq_TorBoundaryTau, wr_TorBoundaryGrid, wq_TorBoundary                ::
  !     速度トロイダルポテンシャルの境界条件を適用する(タウ法,選点法)       
  ! wq_LaplaPol2Pol_wq, wq_LaplaPol2PolTau_wq                            ::
  !     速度ポロイダルポテンシャルΦを▽^2Φから求める
  !     (入出力がそれぞれ格子点およびスペクトル係数)
  ! wq_TorMagBoundaryTau, wr_TorMagBoundaryGrid, wq_TorMagBoundary       ::
  !     磁場トロイダルポテンシャルの境界条件を適用する(タウ法, 選点法)
  ! wq_PolMagBoundaryTau, wr_PolMagBoundaryGrid, wq_PolMagBoundary       ::
  !     磁場トロイダルポテンシャル境界の境界条件を適用する(タウ法, 選点法)
  !
  !==== 積分・平均(3 次元データ)
  !
  ! IntLonLatRad_xyr, AvrLonLatRad_xyr :: 3 次元格子点データの
  !                                       全領域積分および平均
  ! r_IntLonLat_xyr, r_AvrLonLat_xyr   :: 3 次元格子点データの
  !                                       緯度経度(水平・球面)積分および平均
  ! y_IntLonRad_xyr, y_AvrLonRad_xyr   :: 3 次元格子点データの
  !                                       緯度動径積分および平均
  ! r_IntLatRad_xyr, r_AvrLatRad_xyr   :: 3 次元格子点データの
  !                                       経度動径(子午面)積分および平均
  ! yr_IntLon_xyr, yr_AvrLon_xyr       :: 3 次元格子点データの
  !                                       経度方向積分および平均
  ! xr_IntLat_xyr, xr_AvrLat_xyr       :: 3 次元格子点データの
  !                                       緯度方向積分および平均
  ! xr_IntRad_xyr, xr_AvrRad_xyr       :: 3 次元格子点データの
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
  use wa_module_sjpack
  use aq_module, r_Rad => g_R, r_RAD_WEIGHT => g_R_WEIGHT, &
                 aq_ar => aq_ag, ar_aq => ag_aq, &
                 q_RadDRad_q => q_rDr_q, wq_RadDRad_wq => aq_rDr_aq, &
                 wq_Rad2_wq => aq_r2_aq, q_Rad2_q => q_r2_q, &
                 wq_Rad2Inv_wq => aq_r2Inv_aq, q_Rad2Inv_q => q_r2Inv_q
  implicit none
  private

  public wq_Initial

  public x_Lon, x_Lon_Weight
  public y_Lat, y_Lat_Weight
  public r_Rad, r_Rad_Weight
  public l_nm, nm_l
  public xy_Lon, xy_Lat
  public xyr_Lon, xyr_Lat, xyr_Rad
  public wr_Rad
  public wq_VMiss

  public w_xy, xy_w
  public wq_RadDRad_wq, q_RadDRad_q, wr_wq, wq_wr
  public wq_Rad2_wq, q_Rad2_q, wq_Rad2Inv_wq, q_Rad2Inv_q
  public xyr_wq, wq_xyr, xyr_wr, wr_xyr
  public wr_DivRad_wq, wr_RotDRad_wq, wr_RotDRad_wr, wq_Lapla_wq
  public wq_RotDRad_wr
  public xyr_GradLon_wq, xyr_GradLat_wq
  public wr_DivLon_xyr, wr_DivLat_xyr
  public wr_Div_xyr_xyr_xyr, xyr_Div_xyr_xyr_xyr
  public xyr_RotLon_wq_wq, xyr_RotLat_wq_wq, wr_RotRad_xyr_xyr

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

  public wq_KxRGrad_wq, xyr_KGrad_wq, wq_L2_wq, wq_L2Inv_wq, wq_QOperator_wq
  public wr_RadRot_xyr_xyr, wr_RadRotRot_xyr_xyr_xyr
  public wq_RadRotRot_xyr_xyr_xyr
  public wq_Potential2vector, wq_Potential2Rotation

  public nmr_ToroidalEnergySpectrum_wq, nr_ToroidalEnergySpectrum_wq
  public nmr_PoloidalEnergySpectrum_wq, nr_PoloidalEnergySpectrum_wq

  public wq_Boundary, wq_TorBoundary, wq_LaplaPol2Pol_wq ! wr_LaplaPol2Pol_wr
  public wq_TormagBoundary, wq_PolmagBoundary

  public wq_BoundaryTau, wq_TorBoundaryTau, wq_LaplaPol2PolTau_wq
  public wq_TormagBoundaryTau, wq_PolmagBoundaryTau

  public wr_BoundaryGrid, wr_TorBoundaryGrid
  public wr_TormagBoundaryGrid, wr_PolmagBoundaryGrid

  interface wq_Boundary
     module procedure wq_BoundaryTau
  end interface

  interface wq_TorBoundary
     module procedure wq_TorBoundaryTau
  end interface

  interface wq_LaplaPol2Pol_wq
     module procedure wq_LaplaPol2PolTau_wq
  end interface

  interface wq_TorMagBoundary
     module procedure wq_TorMagBoundaryTau
  end interface

  interface wq_PolMagBoundary
     module procedure wq_PolMagBoundaryTau
  end interface

  integer            :: im=64, jm=32, km=16  ! 格子点の設定(経度, 緯度, 動径)
  integer            :: nm=21, lm=31         ! 切断波数の設定(水平, 動径)
  real(8)            :: ra=1.0               ! 球半径
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), parameter :: alpha = 1.0D0      ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta  = 2.0D0      ! 展開多項式パラメター  0 < β

  real(8), dimension(:,:,:), allocatable :: xyr_LON, xyr_LAT, xyr_RAD ! 座標
  real(8), dimension(:,:), allocatable   :: wr_RAD                    ! 座標
  integer, dimension(:), allocatable     :: nd             ! 重み r^n の指数

  real(8) :: wq_VMiss = -999.0        ! 欠損値

  save im, jm, km, nm, lm, ra, nd, xyr_Lon, xyr_Lat, xyr_Rad, wr_Rad

  contains
  !--------------- 初期化 -----------------
   subroutine wq_Initial(i,j,k,n,l,r,np,wa_init)
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
           call wa_Initial(nm,im,jm,km,np)
        else
           call wa_Initial(nm,im,jm,km)
        endif
     endif

     allocate(nd((nm+1)*(nm+1)))

     do nn=0,nm
        do mm=-nn,nn
           nd(l_nm(nn,mm)) = nn                 ! 水平 n 次は r^n から
        enddo
     enddo

     call aq_Initial(km,lm,ra,alpha,beta,nd)

     allocate(xyr_Lon(0:im-1,1:jm,km))
     allocate(xyr_Lat(0:im-1,1:jm,km))
     allocate(xyr_Rad(0:im-1,1:jm,km))

     allocate(wr_Rad((nm+1)*(nm+1),km))

     xyr_Lon = spread(xy_Lon,3,km)
     xyr_Lat = spread(xy_Lat,3,km)
     xyr_Rad = spread(spread(r_Rad,1,jm),1,im)

     wr_Rad = spread(r_Rad,1,(nm+1)*(nm+1))

    call MessageNotify('M','wq_initial','wq_module_sjpack (2009/12/06) is initialized')

   end subroutine wq_initial

  !--------------- 基本変換 -----------------

    function xyr_wq(wq)
      !
      ! スペクトルデータから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:im-1,1:jm,km)                       :: xyr_wq
      !(out) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      xyr_wq = xya_wa(wr_wq(wq))

    end function xyr_wq

    function wq_xyr(xyr)
      !
      ! 3 次元格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_xyr
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,km), intent(in)           :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      wq_xyr = wq_wr(wa_xya(xyr))

    end function wq_xyr

    function xyr_wr(wr)
      !
      ! 水平スペクトル・動径格子点データから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:im-1,1:jm,km)                     :: xyr_wr
      !(out) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ

      xyr_wr = xya_wa(wr)

    end function xyr_wr

    function wr_xyr(xyr)
      !
      ! 3 次元格子データから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),km)               :: wr_xyr
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ

      real(8), dimension(0:im-1,1:jm,km), intent(in)         :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      wr_xyr = wa_xya(xyr)

    end function wr_xyr

    function wr_wq(wq)
      !
      ! スペクトルデータから水平スペクトル・動径格子点データへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),km) :: wr_wq
      !(out) 2 次元球面調和函数スペクトル・動径格子点データ
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      wr_wq = ar_aq(wq)

    end function wr_wq

    function wq_wr(wr)
      !
      ! 水平スペクトル・動径格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm) :: wq_wr
      !(out) 2 次元球面調和函数チェビシェフスペクトルデータ
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 次元球面調和函数スペクトル・動径格子点データ

      wq_wr = aq_ar(wr)
      
    end function wq_wr


  !--------------- 微分計算 -----------------
    function wr_DivRad_wq(wq)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),km)             :: wr_DivRad_wq
      !(out) 発散型動径微分を作用された水平スペクトル動径格子点データ

      wr_DivRad_wq = wr_wq(wq_RadDRad_wq(wq))/wr_Rad + 2/wr_Rad * wr_wq(wq)

    end function wr_DivRad_wq

    function wr_RotDRad_wq(wq)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),km)               :: wr_RotDRad_wq
      !(out) 回転型動径微分を作用された水平スペクトル動径格子点データ

      wr_RotDRad_wq = wr_wq(wq_RadDrad_wq(wq))/wr_Rad + wr_wq(wq)/wr_Rad

    end function wr_RotDRad_wq

    function wr_RotDRad_wr(wr)
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
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),km)               :: wr_RotDRad_wr
      !(out) 回転型動径微分を作用された水平スペクトル動径格子点データ

      wr_RotDRad_wr = wr_wq(wq_RadDRad_wq(wq_wr(wr*wr_Rad)))/wr_Rad**2

    end function wr_RotDRad_wr

    function wq_RotDRad_wr(wr)
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
      real(8), dimension((nm+1)*(nm+1),km), intent(in) :: wr
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)               :: wq_RotDRad_wr
      !(out) 回転型動径微分を作用された水平スペクトル動径格子点データ

      wq_RotDRad_wr = wq_Rad2Inv_wq(wq_RadDRad_wq(wq_wr(wr*wr_Rad)))

    end function wq_RotDRad_wr

    function wq_Lapla_wq(wq)
      ! 入力スペクトルデータにラプラシアン
      !
      !     ▽^2 =   1/r^2 cos^2φ・∂^2/∂λ^2 
      !            + 1/r^2 cosφ・∂/∂φ(cosφ∂/∂φ) 
      !            + 1/r^2 ∂/∂r (r^2 ∂/∂r) 
      !              [1/r^2 (r∂/∂r)(r∂/∂r) + 1/r^2(r∂/∂r)]
      !
      ! を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_Lapla_wq
      !(out) ラプラシアンを作用された水平スペクトルデータ

      wq_Lapla_wq = wq_Rad2Inv_wq(  wq_RadDRad_wq(wq_RadDRad_wq(wq)) &
                                  + wq_RadDRad_wq(wq)+ wa_Lapla_wa(wq) )

    end function wq_Lapla_wq

    function xyr_GradLon_wq(wq)
      !
      ! スペクトルデータに勾配型経度微分 1/rcosφ・∂/∂λ
      ! を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,km)    :: xyr_GradLon_wq
      !(out) 勾配型経度微分を作用された 2 次元スペクトルデータ

      xyr_GradLon_wq = xya_GradLon_wa(wr_wq(wq))/xyr_Rad

    end function xyr_GradLon_wq

    function xyr_GradLat_wq(wq) 
      !
      ! スペクトルデータに勾配型経度微分 1/r ∂/∂φ を作用させる.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,km)    :: xyr_GradLat_wq
      !(out) 勾配型緯度微分を作用された 2 次元スペクトルデータ

      xyr_GradLat_wq = xya_GradLat_wa(wr_wq(wq))/xyr_Rad

    end function xyr_GradLat_wq

    function wr_DivLon_xyr(xyr)
      ! 
      ! 格子点データに発散型経度微分 1/rcosφ・∂/∂λ を作用させた
      ! スペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in)     :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_DivLon_xyr
      !(out) 発散型経度微分を作用された水平スペクトル動径格子点データ

      wr_DivLon_xyr = wa_DivLon_xya(xyr/xyr_Rad)

    end function wr_DivLon_xyr

    function wr_DivLat_xyr(xyr)
      !
      ! 格子データに発散型緯度微分 1/rcosφ・∂(f cosφ)/∂φ を
      ! 作用させたスペクトルデータを返す.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in)     :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_DivLat_xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_Div_xyr_xyr_xyr
      !(out) ベクトル場の発散

      wr_Div_xyr_xyr_xyr =   wr_DivLon_xyr(xyr_Vlon) &
                           + wr_DivLat_xyr(xyr_Vlat) &
                           + wr_DivRad_wq(wq_xyr(xyr_Vrad))

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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,km)             :: xyr_Div_xyr_xyr_xyr
      !(out) ベクトル場の発散

      xyr_Div_xyr_xyr_xyr &
           = xyr_Rad/cos(xyr_Lat) &
                * xyr_wr(wr_Div_xyr_xyr_xyr(xyr_VLon*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VLat*cos(xyr_Lat)/xyr_Rad,  &
                                            xyr_VRad*cos(xyr_Lat)/xyr_Rad ))&
             + xyr_VLat*tan(xyr_Lat)/xyr_Rad &
             + xyr_VRad/xyr_Rad

    end function xyr_Div_xyr_xyr_xyr

    function xyr_RotLon_wq_wq(wq_Vrad,wq_Vlat) 
      !
      ! ベクトル場の動径成分, 緯度成分である第 1, 2 引数 Vrad, Vlat から
      ! 回転の経度成分 
      !
      !    1/r ∂Vrad/∂φ-1/r ∂(r Vlat)/∂r を計算する.
      !
      ! を計算する
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_RotLon_wq_wq
      !(out) ベクトル場の回転の経度成分

        xyr_RotLon_wq_wq =   xyr_GradLat_wq(wq_Vrad) &
                           - xyr_wr(wr_RotDRad_wq(wq_Vlat))

    end function xyr_RotLon_wq_wq

    function xyr_RotLat_wq_wq(wq_Vlon,wq_Vrad) 
      !
      ! ベクトル場の経度成分, 動径成分である第 1, 2 引数 Vlon, Vrad から
      ! 回転の緯度成分 
      !
      !    1/r ∂(r Vlon)/∂r - 1/rcosφ・∂Vrad/∂λ
      !
      ! を計算する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_Vrad
      !(in) ベクトル場の動径成分

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_RotLat_wq_wq
      !(out) ベクトル場の回転の緯度成分

        xyr_RotLat_wq_wq =   xyr_wr(wr_RotDRad_wq(wq_Vlon)) &
                           - xyr_GradLon_wq(wq_Vrad) 

    end function xyr_RotLat_wq_wq

    function wr_RotRad_xyr_xyr(xyr_Vlat,xyr_Vlon) 
      !
      ! ベクトルの緯度成分, 経度成分である第 1, 2 引数 Vlat, Vlon に対して
      ! ベクトル場の回転の動径成分 
      !
      !    1/rcosφ・∂Vlat/∂λ - 1/rcosφ・∂(Vlon cosφ)/∂φ
      !
      ! を計算する.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlat
      !(in) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_Vlon
      !(in) ベクトル場の経度成分

      real(8), dimension((nm+1)*(nm+1),km)       :: wr_RotRad_xyr_xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,km)  :: yr_IntLon_xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,km)  :: xr_IntLat_xyr        
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_IntRad_xyr
      !(out) 動径積分された 2 次元経度緯度(水平, 球面)格子点データ

      integer :: k

      xy_IntRad_xyr = 0.0d0
      do k=1,km
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1)     :: x_IntLatRad_xyr
      !(out) 緯度動径(子午面)積分された 1 次元経度格子点データ

      integer :: j, k

      x_IntLatRad_xyr = 0.0D0
      do k=1,km
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm)       :: y_IntLonRad_xyr
      !(out) 経度動径(緯度円)積分された 1 次元緯度格子点データ

      integer :: i, k

      y_IntLonRad_xyr = 0.0D0
      do k=1,km
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(km)     :: r_IntLonLat_xyr
      !(out) 緯度経度(水平, 球面)積分された 1 次元動径格子点データ

      integer :: i, j

      r_IntLonLat_xyr = 0.0D0
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr 
      !(in) 3 次元経度緯度動径格子点データ

      real(8)                     :: IntLonLatRad_xyr 
      !(out) 全球積分値

      integer :: i, j, k

      IntLonLatRad_xyr = 0.0D0
      do k=1,km
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
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(km)  :: r_IntLat_yr
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
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(1:jm)  :: y_IntRad_yr
      !(out) 動径積分された 1 次元緯度格子点データ

      integer :: k

      y_IntRad_yr = 0.0d0
      do k=1,km
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
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8)                   :: IntLatRad_yr
      !(out) 積分値
      integer :: j, k

      IntLatRad_yr = 0.0D0
      do k=1,km
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(km)  :: r_IntLon_xr
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(0:im-1)  :: x_IntRad_xr
      !(out) 動径積分された 1 次元経度格子点データ

      integer :: k

      x_IntRad_xr = 0.0d0
      do k=1,km
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8)                                 :: IntLonRad_xr
      !(out) 積分値

      integer :: i, k

      IntLonRad_xr = 0.0D0
      do k=1,km
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
      real(8), dimension(km), intent(in) :: z
      !(in) 1 次元動径格子点データ

      real(8)                              :: IntRad_r
      !(out) 積分値

      integer :: k

      IntRad_r = 0.0d0
      do k=1,km
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(1:jm,km)  :: yr_AvrLon_xyr
      !(out) 経度方向(帯状)平均された 2 次元子午面格子点データ

      yr_AvrLon_xyr = yr_IntLon_xyr(xyr)/sum(x_Lon_Weight)

    end function yr_AvrLon_xyr

    function xr_AvrLat_xyr(xyr)  ! 緯度積分
      !
      ! 3 次元格子点データの緯度方向域平均.
      !
      ! 3 次元データ f(λ,φ,r) に対して ∫f(λ,φ,r)cosφ dφ/2 を計算する.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,km)  :: xr_AvrLat_xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(0:im-1,1:jm)  :: xy_AvrRad_xyr          
      !(out) 動径平均された 2 次元経度緯度(水平, 球面)格子点データ
      ! 水平格子点データ

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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
      !(in) 3 次元経度緯度動径格子点データ

      real(8), dimension(km)     :: r_AvrLonLat_xyr
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr
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
      real(8), dimension(1:jm,km), intent(in) :: yr
      !(in) 2 次元緯度動径(子午面)格子点データ

      real(8), dimension(km)  :: r_AvrLat_yr
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
      real(8), dimension(1:jm,km), intent(in) :: yr
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
      real(8), dimension(1:jm,km), intent(in) :: yr
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
      !(in) 2 次元緯度動径格子点データ

      real(8), dimension(km)  :: r_AvrLon_xr 
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
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
      real(8), dimension(0:im-1,km), intent(in) :: xr
      ! (in) 2 次元格子点データ
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
      real(8), dimension(0:im-1,km), intent(in) :: z
      !(in) 1 次元動径格子点データ
      real(8)                                 :: AvrRad_r
      !(out) 平均値

      AvrRad_r = IntRad_r(z)/sum(r_Rad_Weight)

    end function AvrRad_r

  !--------------- ポロイダル/トロイダルモデル用微分 -----------------

    function wq_KxRGrad_wq(wq)
      !
      ! 入力スペクトルデータに経度微分 k×r・▽ = ∂/∂λを作用する.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_KxRGrad_wq
      !(out) 経度微分を作用された 2 次元スペクトルデータ

      wq_KxRGrad_wq =  wa_Dlon_wa(wq)

    end function wq_KxRGrad_wq

    function xyr_KGrad_wq(wq)    ! k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r
      !
      ! 入力スペクトルデータに対応する格子データに軸方向微分 
      !
      !    k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r 
      !
      ! を作用させた格子データが返される. 
      ! ここでベクトル k は球の中心から北極向きの単位ベクトルである.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:im-1,1:jm,km)                 :: xyr_KGrad_wq
      !(out) 軸方向微分を作用された 2 次元スペクトルデータ

      xyr_KGrad_wq =  cos(xyr_Lat)*xyr_GradLat_wq(wq) &
                    + sin(xyr_Lat)*xyr_wq(wq_RadDRad_wq(wq))/xyr_Rad

    end function xyr_KGrad_wq

    function wq_L2_wq(wq)
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
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_L2_wq
      !(out) L^2 演算子を作用された 2 次元スペクトルデータ

      wq_L2_wq = -wa_Lapla_wa(wq)

    end function wq_L2_wq

    function wq_L2Inv_wq(wq)
      !
      ! 入力スペクトルデータに L^2 演算子の逆演算(-逆水平ラプラシアン)を
      ! 作用する.
      !
      ! スペクトルデータに L^2 演算子を作用させる関数 wq_L2_wq の逆計算を
      ! 行う関数である.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_L2Inv_wq
      !(out) L^2 演算子の逆演算を作用された 2 次元スペクトルデータ

      wq_L2Inv_wq = -wa_LaplaInv_wa(wq)

    end function wq_L2Inv_wq

    function wq_QOperator_wq(wq)
      !
      ! 入力スペクトルデータに対応する格子点データに演算子 
      !
      !    Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) 
      !
      ! を作用させたデータのスペクトル変換が返される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension((nm+1)*(nm+1),0:lm)             :: wq_QOperator_wq
      !(out) Q 演算子を作用された 2 次元スペクトルデータ

      wq_QOperator_wq = &
             wq_xyr(xyr_KGrad_wq(wq) - xyr_KGrad_wq(wq_L2_wq(wq))/2) &
           - wq_L2_wq(wq_xyr(xyr_KGrad_wq(wq)))/2

    end function wq_QOperator_wq

    function wr_RadRot_xyr_xyr(xyr_VLON,xyr_VLAT)  ! r・(▽×v)
      !
      ! ベクトルの渦度と動径ベクトルの内積 r・(▽×v) を計算する.
      !
      ! 第 1, 2 引数(v[λ], v[φ])がそれぞれベクトルの経度成分, 緯度成分を表す.
      !
      !    r・(▽×v) = 1/cosφ・∂v[φ]/∂λ - 1/cosφ・∂(v[λ] cosφ)/∂φ
      !
      ! のスペクトル データが返される.
      !
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_RadRot_xyr_xyr
      !(out) ベクトルの渦度と動径ベクトルの内積

      wr_RadRot_xyr_xyr = wa_DivLon_xya(xyr_VLAT) - wa_DivLat_xya(xyr_VLON)
      
    end function wr_RadRot_xyr_xyr

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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VRAD
      !(in) ベクトルの動径成分

      real(8), dimension((nm+1)*(nm+1),km)     :: wr_RadRotRot_xyr_xyr_xyr
      !(out) ベクトル v の r・(▽×▽×v) 

      wr_RadRotRot_xyr_xyr_xyr = &
                   wr_RotDRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wr_xyr(xyr_VRAD/xyr_RAD))

    end function wr_RadRotRot_xyr_xyr_xyr

    function wq_RadRotRot_xyr_xyr_xyr(xyr_VLON,xyr_VLAT,xyr_VRAD) 
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
      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLON
      !(in) ベクトルの経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VLAT
      !(in) ベクトルの緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(in) :: xyr_VRAD
      !(in) ベクトルの動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm)     :: wq_RadRotRot_xyr_xyr_xyr
      !(out) ベクトル v の r・(▽×▽×v) 

      wq_RadRotRot_xyr_xyr_xyr = &
                   wq_RotDRad_wr( &
                      wa_DivLon_xya(xyr_VLON)+ wa_DivLat_xya(xyr_VLAT)) &
             - wa_Lapla_wa(wq_xyr(xyr_VRAD/xyr_RAD))

    end function wq_RadRotRot_xyr_xyr_xyr

    subroutine wq_Potential2Vector(&
         xyr_VLON,xyr_VLAT,xyr_VRAD,wq_TORPOT,wq_POLPOT)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψr) + ▽x▽x(Φr) 
      !
      ! の各成分を計算する
      !
      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLON
      !(out) ベクトル場の経度成分

      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VLAT
      !(out) ベクトル場の緯度成分

      real(8), dimension(0:im-1,1:jm,km)     :: xyr_VRAD
      !(out) ベクトル場の動径成分

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) ポロイダルポテンシャル

      xyr_VLON =   xyr_RAD * xyr_GradLat_wq(wq_TORPOT) &
                 + xya_GradLon_wa(wr_RotDRad_wq(wq_POLPOT))
      xyr_VLAT = - xyr_RAD * xyr_GradLon_wq(wq_TORPOT) &
                 + xya_GradLat_wa(wr_RotDRad_wq(wq_POLPOT))
      xyr_VRAD = xyr_wq(wq_L2_wq(wq_POLPOT))/xyr_RAD

    end subroutine wq_Potential2Vector

    subroutine wq_Potential2Rotation(&
       xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD,wq_TORPOT,wq_POLPOT)
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
      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVLON
      !(out) 回転の経度成分

      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVLAT
      !(out) 回転の緯度成分

      real(8), dimension(0:im-1,1:jm,km), intent(OUT) :: xyr_RotVRAD
      !(out) 回転の動径成分

      ! 入力ベクトル場を表すポテンシャル
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) ポロイダルポテンシャル

      call wq_Potential2Vector( &
           xyr_RotVLON,xyr_RotVLAT,xyr_RotVRAD, &
           -wq_Lapla_wq(wq_POLPOT), wq_TORPOT)

    end subroutine wq_Potential2Rotation

  !--------------- ポロイダル/トロイダルモデル用スペクトル解析 ----------------

    function nmr_ToroidalEnergySpectrum_wq(wq_TORPOT)
      !
      ! トロイダルポテンシャルから, トロイダルエネルギーの
      ! 球面調和函数全波数 n, 帯状波数 m の各成分を計算する
      !
      !  * 全波数 n, 帯状波数 m のトロイダルポテンシャルのスペクトル成分
      !    ψ(n,m,r)から全波数 n, 帯状波数 m 成分のトロイダルエネルギー
      !    スペクトルは  (1/2)n(n+1)4πr^2|ψ(n,m,r)|^2  と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !    
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    wq_VMiss によって設定できる (初期値は -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,km) :: nmr_ToroidalEnergySpectrum_wq
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA   ! 作業領域
      integer :: n, m

      nmr_ToroidalEnergySpectrum_wq = wq_VMiss

      wr_DATA = wr_wq(wq_TORPOT)

      do n=0,nm
         nmr_ToroidalEnergySpectrum_wq(n,0,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * wr_DATA(l_nm(n,0),:)**2
         do m=1,n
            nmr_ToroidalEnergySpectrum_wq(n,m,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 &
                * (wr_DATA(l_nm(n,m),:)**2+wr_DATA(l_nm(n,-m),:)**2)
            nmr_ToroidalEnergySpectrum_wq(n,-m,:) &
                 = nmr_ToroidalEnergySpectrum_wq(n,m,:) 
         enddo
      enddo

    end function nmr_ToroidalEnergySpectrum_wq

    function nr_ToroidalEnergySpectrum_wq(wq_TORPOT)
      !
      ! トロイダルポテンシャルから, トロイダルエネルギーの
      ! 球面調和函数全波数の各成分を計算する.
      !
      !  * 全波数 n, 帯状波数 m のトロイダルポテンシャルのスペクトル成分
      !    ψ(n,m,r)から全波数 n 成分のトロイダルエネルギースペクトルは
      !    Σ[m=-n]^n(1/2)n(n+1)4πr^2|ψ(n,m,r)|^2 と計算される.
      !
      ! * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,km) :: nr_ToroidalEnergySpectrum_wq 
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA   ! 作業領域
      integer :: n, m

      wr_DATA = wr_wq(wq_TORPOT)
      do n=0,nm
         nr_ToroidalEnergySpectrum_wq(n,:) &
              = 0.5 * n*(n+1)* (4*pi) * r_Rad**2 * wr_Data(l_nm(n,0),:)**2
         do m=1,n
            nr_ToroidalEnergySpectrum_wq(n,:) &
                 = nr_ToroidalEnergySpectrum_wq(n,:) &
                 + 0.5 * n*(n+1)* (4*pi) * r_Rad**2  &
                 * 2* (wr_Data(l_nm(n,m),:)**2 + wr_Data(l_nm(n,-m),:)**2)
         enddo
      enddo

    end function nr_ToroidalEnergySpectrum_wq

    function nmr_PoloidalEnergySpectrum_wq(wq_POLPOT)
      !
      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
      ! 球面調和函数全波数 n, 帯状波数 m の各成分を計算する.
      !
      !  * 全波数 n, 帯状波数 m のポロイダルポテンシャルのスペクトル成分
      !    φ(n,m,r)から全波数 n, 帯状波数 m 成分のポロイダルエネルギー
      !    スペクトルは 
      !
      !      (1/2)n(n+1)4π{|d(rφ(n,m,r))/dr|^2 + n(n+1)|φ(n,m,r)|^2} 
      !
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和を動径積分したもの(r^2の重み無し)
      !    が球殻内での全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 wq_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,-nm:nm,km) :: nmr_PoloidalEnergySpectrum_wq 
      !(out) エネルギースペクトルポロイダル成分


      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA1   ! 作業領域
      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA2   ! 作業領域
      integer :: n, m

      nmr_PoloidalEnergySpectrum_wq = wq_VMiss

      wr_Data1 = wr_wq(wq_POLPOT)
      wr_Data2 = wr_wq(wq_RadDRad_wq(wq_POLPOT)+wq_POLPOT)  ! d(rφ)/dr
                                                            ! = rdφ/dr+φ

      do n=0,nm
         nmr_PoloidalEnergySpectrum_wq(n,0,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,0),:)**2  &
                   + n*(n+1)*wr_Data1(l_nm(n,0),:)**2 )
         do m=1,n
            nmr_PoloidalEnergySpectrum_wq(n,m,:) = &
                 + 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2 + wr_Data2(l_nm(n,-m),:)**2 &
                 + n*(n+1)* ( wr_Data1(l_nm(n,m),:)**2 + wr_Data1(l_nm(n,-m),:)**2))
            nmr_PoloidalEnergySpectrum_wq(n,-m,:) = &
                 nmr_PoloidalEnergySpectrum_wq(n,m,:)
         enddo
      enddo

    end function nmr_PoloidalEnergySpectrum_wq

    function nr_PoloidalEnergySpectrum_wq(wq_POLPOT)
      !
      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
      ! 球面調和函数全波数の各成分を計算する
      !
      !  * 全波数 n, 帯状波数 m のポロイダルポテンシャルのスペクトル成分
      !    φ(n,m,r)から全波数 n 成分のポロイダルエネルギースペクトルは
      !
      !      Σ[m=-n]^n ((1/2)n(n+1)4π{|d(rφ(n,m,r))/dr|^2 
      !                 + n(n+1)|φ(n,m,r)|^2} 
      !
      !    と計算される.
      !
      !  * 全ての全波数に対してのエネルギースペクトル成分の和を動径積分したもの
      !    (r^2の重み無し)が球殻内での全エネルギーに等しい.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm), intent(in) :: wq_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:nm,km) :: nr_PoloidalEnergySpectrum_wq
      !(out) エネルギースペクトルポロイダル成分

      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA1   ! 作業領域
      real(8), dimension((nm+1)*(nm+1),km) ::wr_DATA2   ! 作業領域
      integer :: n, m

      wr_Data1 = wr_wq(wq_POLPOT)
      wr_Data2 = wr_wq(wq_RadDRad_wq(wq_POLPOT)+wq_POLPOT)  ! d(rφ)/dr
                                                            ! = rdφ/dr+φ

      do n=0,nm
         nr_PoloidalEnergySpectrum_wq(n,:) &
              = 0.5* n*(n+1)* (4*pi) &
              *( wr_Data2(l_nm(n,0),:)**2  + n*(n+1)*wr_Data1(l_nm(n,0),:)**2 )
         do m=1,n
            nr_PoloidalEnergySpectrum_wq(n,:) &
                 = nr_PoloidalEnergySpectrum_wq(n,:) &
                 + 2 * 0.5* n*(n+1)* (4*pi) &
                 *( wr_Data2(l_nm(n,m),:)**2 + wr_Data2(l_nm(n,-m),:)**2 &
                 + n*(n+1)*(wr_Data1(l_nm(n,m),:)**2 +wr_Data1(l_nm(n,-m),:)**2))
         enddo
      enddo

    end function nr_PoloidalEnergySpectrum_wq

  !--------------- 境界値問題 -----------------

    subroutine wq_BoundaryTau(wq,value,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! Chebyshev 空間での境界条件適用(タウ法)
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を
      ! 定める方法をとっている(タウ法).
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wq
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
            call aq_BoundaryTau_D(wq,value)
         else
            call aq_BoundaryTau_D(wq)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call aq_BoundaryTau_N(wq,value)
         else
            call aq_BoundaryTau_N(wq)
         endif
      case ('D')
         if (present(value)) then
            call aq_BoundaryTau_D(wq,value)
         else
            call aq_BoundaryTau_D(wq)
         endif
      case default
         call MessageNotify('E','wq_BoundaryTau','B.C. not supported')
      end select

    end subroutine wq_BoundaryTau

    subroutine wr_BoundaryGrid(wr,value,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). 
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)      :: wr
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
            call ag_BoundaryGrid_D(wr,value)
         else
            call ag_BoundaryGrid_D(wr)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call ag_BoundaryGrid_N(wr,value)
         else
            call ag_BoundaryGrid_N(wr)
         endif
      case ('D')
         if (present(value)) then
            call ag_BoundaryGrid_D(wr,value)
         else
            call ag_BoundaryGrid_D(wr)
         endif
      case default
         call MessageNotify('E','wr_BoundaryGrid','B.C. not supported')
      end select

    end subroutine wr_BoundaryGrid

    subroutine wq_TorBoundaryTau(wq_TORPOT,value,cond,new) 
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)      :: wq_TORPOT
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
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_data
      real(8), dimension((nm+1)*(nm+1),km)    :: wr_data
      logical                                 :: rigid        ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n, l, lend
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
            call MessageNotify('E','wq_TorBoundaryTau','B.C. not supported')
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

         ! 力学的条件

         do l=0,lm
            wq_data = 0.0 ; wq_data(:,l) = 1.0D0

            if ( rigid ) then          ! 力学的条件粘着
               wr_data = wr_wq(wq_data)
            else                       ! 力学的条件自由すべり
               wr_data = wr_wq(wq_RadDRad_wq(wq_data)- wq_data)/wr_Rad
            endif

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  alu(n,lm,l) = wr_data(n,km)
               else
                  alu(n,lm-1,l) = wr_data(n,km)
               endif
            end do
         enddo

         ! 関係ないところを 0 で埋める.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wq_TorBoundaryTau',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wq_TorBoundaryTau',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wq_TorBoundaryTau',&
                 'Boundary value k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wq_TorBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            lend = lm
         else
            lend = lm-1
         endif

         if ( rigid .AND. present(value) ) then
            wq_torpot(n,lend) = value(n)
         else
            wq_torpot(n,lend) = 0.0D0
         endif
      enddo

      wq_torpot = lusolve(alu,kp,wq_TORPOT)

    end subroutine wq_TorBoundaryTau

    subroutine wr_TorBoundaryGrid(wr_TORPOT,value,cond,new) 
      !
      ! 速度トロイダルポテンシャルに対して境界条件を適用する.
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). 
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
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)    :: wr_TORPOT
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
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_data
      real(8), dimension((nm+1)*(nm+1),km)    :: wr_data
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
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
            call MessageNotify('E','wr_TorBoundaryGrid','B.C. not supported')
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
         allocate(alu((nm+1)*(nm+1),km,km),kp((nm+1)*(nm+1),km))

         alu = 0.0D0
         do k=1,km
            wr_data = 0.0D0
            wr_data(:,k)=1.0D0
            alu(:,:,k) = wr_data
         enddo

         if ( .not. rigid ) then
            do k=1,km
               wr_data = 0.0D0
               wr_data(:,k)=1.0D0
               wq_data = wq_wr(wr_data)
               wr_data = wr_wq(wq_RadDRad_wq(wq_data) - wq_data)/wr_Rad
               alu(:,km,k) = wr_data(:,km)
            enddo
         endif

         call ludecomp(alu,kp)

         if ( rigid .AND. present(value) ) then 
            call MessageNotify('M','wr_TorBoundaryGrid',&
                 'Toroidal potential at k=km was given by the optional variable.')
         else if ( rigid .AND. (.NOT.present(value)) ) then
            call MessageNotify('M','wr_TorBoundaryGrid',&
                 'Toroidal potential at k=km was set to zero.')
         else if ( (.NOT. rigid) .AND. present(value) ) then
            call MessageNotify('W','wr_TorBoundaryGrid',&
                 'Boundary value at k=km cannot be set under stress-free condition.')
         endif

         call MessageNotify('M','wr_TorBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      if ( rigid .AND. present(value) ) then
         wr_TorPot(:,km)  = value
      else
         wr_TorPot(:,km)  = 0.0D0
      endif

      wr_TorPot = lusolve(alu,kp,wr_TorPot)

    end subroutine wr_TorBoundaryGrid

    function wq_LaplaPol2PolTau_wq(wq,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      !
      ! スペクトル空間で境界条件を適用している(タウ法). 
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(in)  :: wq
              !(in) 入力▽^2φ分布

      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_LaplaPol2PolTau_wq
              !(out) 出力ポロイダルポテンシャル分布

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu     ! 内部領域計算用
      integer, dimension(:,:), allocatable    :: kp      ! 内部領域計算用

      real(8), dimension(:,:,:), allocatable  :: alub    ! 境界条件計算用
      integer, dimension(:,:), allocatable    :: kpb     ! 境界条件計算用

      real(8), dimension((nm+1)*(nm+1),km)    :: wr_work
      real(8), dimension((nm+1)*(nm+1),0:lm)  :: wq_work
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l,n, lend
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wq_laplapol2pol_wq','B.C. not supported')
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
         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(alub((nm+1)*(nm+1),0:lm,0:lm),kpb((nm+1)*(nm+1),0:lm))


         !---- 内部領域計算用行列 -----
         do l=0,lm
            wq_work = 0.0
            wq_work(:,l) = 1.0D0
            alu(:,:,l) = wq_Lapla_wq(wq_work)
         enddo

         ! 0 成分のところを 1 で埋める.
         do n=1,(nm+1)**2
            do l=0,nd(n)-1
               alu(n,l,l) = 1.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,l,l) = 1.0D0
            enddo
         enddo

         ! alu(:,:,nd(n)) 列は 0 なので 1 をいれておく. 
         ! l=nd(n) 成分は境界条件で決める. 
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               alu(n,lm,nd(n)) = 1.0D0
            else
               alu(n,lm-1,nd(n)) = 1.0D0
            endif
         enddo

         call ludecomp(alu,kp)

         !---- 境界条件計算用行列 -----

         alub = 0.0D0
         do l=0,lm
            alub(:,l,l) = 1.0D0
         enddo

         do l=0,lm
            wq_work=0.0D0 ; wq_work(:,l)=1.0D0
            wr_work = wr_wq(wq_work)

            ! 運動学的条件. 流線は境界で一定
            !     l=nd(n) 成分を境界条件で決める. 
            do n=1,(nm+1)**2
               alub(n,nd(n),l) = wr_work(n,km)
            enddo

            ! 力学的条件粘着条件 
            !     l=lend 成分を境界条件で決める. 
            if ( rigid ) then
               wr_work=wr_wq(wq_RadDRad_wq(wq_work))/wr_Rad
            else
               wr_work=wr_wq(wq_RadDRad_wq(wq_RadDRad_wq(wq_work)) &
                                           -wq_RadDRad_wq(wq_work) )&
                            /wr_Rad**2
            endif
            
            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  lend = lm
               else
                  lend = lm-1
               endif
               alub(n,lend,l) = wr_work(n,km)
            enddo
         enddo

         ! 関係ないところを 0 で埋める.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alub(n,nd(n),l) = 0.0D0
               alub(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alub(n,nd(n),l) = 0.0D0
               alub(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','wq_LaplaPol2PolTau_wq',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','wq_LaplaPol2PolTau_wq',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      ! 内部領域計算
      wq_work = wq

      wq_work = lusolve(alu,kp,wq_work)

      ! 境界条件計算
      do n=1,(nm+1)**2
         wq_work(n,nd(n)) = 0
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_work(n,lm)   = 0
         else
            wq_work(n,lm-1) = 0
         endif
      enddo

      wq_laplapol2polTau_wq = lusolve(alub,kpb,wq_work)

    end function wq_LaplaPol2PolTau_wq

    function wr_LaplaPol2Pol_wr(wr,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      !
      ! 格子点空間で境界条件を適用している. 
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
      ! 注意 : この関数は完成していない. 使用禁止. 
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(in)  :: wr
              !(in) 入力▽^2φ分布

      real(8), dimension((nm+1)*(nm+1),km)         :: wr_LaplaPol2Pol_wr
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

      real(8), dimension(:,:,:), allocatable  :: alub
      integer, dimension(:,:), allocatable    :: kpb

      real(8), dimension((nm+1)*(nm+1),km)    :: wr_work
      logical                                 :: rigid   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','wr_laplapol2pol_wr','B.C. not supported')
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

         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alub((nm+1)*(nm+1),km,km),kpb((nm+1)*(nm+1),km))

         !---- 内部領域計算用行列 -----
         do k=1,km
            wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,k) = wr_wq(wq_Lapla_wq(wq_wr(wr_work)))
         enddo

         do k=1,km
            wr_work=0.0D0 ; wr_work(:,k)=1.0D0
            alu(:,km,k)  = wr_work(:,km)
         enddo


         !---- 境界条件計算用 ----
         alub = 0.0D0
         do k=1,km
            alub(:,k,k) = 1.0D0
         enddo

         ! 運動学的条件. 流線は境界で一定
         !   k=km の値を定める
         do k=1,km
            wr_work=0.0D0 ; wr_work(:,k)=1.0D0
            alub(:,km,k)  = wr_work(:,km)
         enddo

         ! 力学的条件粘着条件 
         !   k=km-1 の値を定める
         if ( rigid ) then
            do k=1,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_wq(wq_RadDRad_wq(wq_wr(wr_work)))/wr_Rad
               alub(:,km-1,k) = wr_work(:,km)
            enddo
         else
            do k=1,km
               wr_work = 0.0D0 ; wr_work(:,k) = 1.0D0
               wr_work=wr_wq(wq_RadDRad_wq(wq_RadDRad_wq(wq_wr(wr_work))) &
                                          -wq_RadDRad_wq(wq_wr(wr_work)))&
                       /wr_Rad**2
               alub(:,km-1,k) = wr_work(:,km)
            enddo
         endif

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','wr_LaplaPol2Pol_wr',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      wr_work         = wr
      wr_work         = lusolve(alu,kp,wr_work)

      wr_work(:,km-1) = 0.0D0               ! 力学的条件
      wr_work(:,km)   = 0.0D0               ! 運動学的条件

      wr_laplapol2pol_wr = lusolve(alub,kpb,wr_work)

    end function wr_LaplaPol2Pol_wr

    subroutine wq_TormagBoundaryTau(wq_TOR,new)
      
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wq_psi = 0   at the outer boundary
      ! 
      ! であるから wq_Boundary で対応可能だが, 将来のため別途作成しておく.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wq_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wq_I
      real(8), dimension(:,:), allocatable    :: wr_PSI

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n, l, lend
      save    :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         if ( allocated(wq_I) ) deallocate(wq_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wq_I((nm+1)*(nm+1),0:lm),wr_PSI((nm+1)*(nm+1),km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         do l=0,lm
            wq_I = 0.0 ; wq_I(:,l) = 1.0
            ! 非電気伝導体
            wr_PSI = wr_wq(wq_I)

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  alu(n,lm,l) = wr_Psi(n,km)
                  lend = lm
               else
                  alu(n,lm-1,l) = wr_Psi(n,km)
               endif
            enddo
         enddo

         ! 関係ないところを 0 で埋める.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wq_I,wr_PSI)

         call MessageNotify('M','TormagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_TOR(n,lm)   = 0.0
         else
            wq_TOR(n,lm-1) = 0.0
         endif
      enddo
      wq_TOR = lusolve(alu,kp,wq_TOR)

    end subroutine wq_TormagBoundaryTau

    subroutine wr_TormagBoundaryGrid(wr_TOR,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 外側
      !    wq_psi = 0   at the outer boundary
      ! 
      ! であるので wq_Boundary で対応可能だが, 将来のため別途作成しておく
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)   :: wr_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 
      
      logical, intent(IN), optional :: new
              !(in) (ダミー) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      wr_TOR(:,km)  = 0.0D0

    end subroutine wr_TormagBoundaryGrid

    subroutine wq_PolmagBoundaryTau(wq_POL,new)
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
      real(8), dimension((nm+1)*(nm+1),0:lm),intent(inout)   :: wq_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wq_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, n, nn(2), lend
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
         if ( allocated(wq_I) ) deallocate(wq_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),0:lm,0:lm),kp((nm+1)*(nm+1),0:lm))
         allocate(wq_I((nm+1)*(nm+1),0:lm))
         allocate(wr_PSI((nm+1)*(nm+1),km),wr_DPSIDR((nm+1)*(nm+1),km))

         alu = 0.0D0
         do l=0,lm
            alu(:,l,l) = 1.0D0
         enddo

         ! 非電気伝導体
         do l=0,lm
            wq_I = 0.0D0  ; wq_I(:,l) = 1.0D0
            wr_PSI = wr_wq(wq_I)
            wr_DPSIDR = wr_wq(wq_RadDRad_wq(wq_I))/wr_Rad

            do n=1,(nm+1)**2
               if ( mod(nd(n),2) .eq. mod(lm,2) ) then
                  lend = lm
               else
                  lend = lm-1
               endif
               nn=nm_l(n)
               alu(n,lend,l) = wr_DPSIDR(n,km) + (nn(1)+1) * wr_PSI(n,km)/r_RAD(km)
            enddo
         enddo

         ! 関係ないところを 0 で埋める.
         do n=1,(nm+1)**2
            if ( mod(nd(n),2) .eq. mod(lm,2) ) then
               lend = lm
            else
               lend = lm-1
            endif

            do l=0,nd(n)-1
               alu(n,lend,l) = 0.0D0
            enddo
            do l=nd(n)+1,lm,2
               alu(n,lend,l) = 0.0D0
            enddo
         enddo

         call ludecomp(alu,kp)

         deallocate(wq_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      do n=1,(nm+1)**2
         if ( mod(nd(n),2) .eq. mod(lm,2) ) then
            wq_POL(n,lm)   = 0.0
         else
            wq_POL(n,lm-1) = 0.0
         endif
      enddo
      wq_POL = lusolve(alu,kp,wq_POL)

    end subroutine wq_PolmagBoundaryTau

    subroutine wr_PolmagBoundaryGrid(wr_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用. 
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). 
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
      real(8), dimension((nm+1)*(nm+1),km),intent(inout)   :: wr_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(:,:), allocatable    :: wr_I
      real(8), dimension(:,:), allocatable    :: wr_PSI
      real(8), dimension(:,:), allocatable    :: wr_DPSIDR

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: k, n, nn(2)
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
         if ( allocated(wr_I) ) deallocate(wr_I)
         if ( allocated(wr_PSI) ) deallocate(wr_PSI)
         if ( allocated(wr_DPSIDR) ) deallocate(wr_DPSIDR)

         allocate(alu((nm+1)*(nm+1),km,km),kp((nm+1)*(nm+1),km))
         allocate(wr_I((nm+1)*(nm+1),km))
         allocate(wr_PSI((nm+1)*(nm+1),km),wr_DPSIDR((nm+1)*(nm+1),km))

         do k=1,km
            wr_I = 0.0D0
            wr_I(:,k)=1.0D0
            alu(:,:,k) = wr_I                 ! 内部領域は値そのまま.
         enddo

         ! 非電気伝導体
         do k=1,km
            wr_I = 0.0D0
            wr_I(:,k) = 1.0D0
            wr_PSI = wr_I
            wr_DPSIDR = wr_wq(wq_RadDRad_wq(wq_wr(wr_I)))/wr_Rad

            do n=1,(nm+1)*(nm+1)
               nn=nm_l(n)
               alu(n,km,k) = wr_DPSIDR(n,km) + (nn(1)+1) * wr_PSI(n,km)/r_RAD(km)
            enddo
         end do

         call ludecomp(alu,kp)

         deallocate(wr_I,wr_PSI,wr_DPSIDR)

         call MessageNotify('M','PolmagBoundaryGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      wr_POL(:,km)  = 0.0D0
      wr_POL = lusolve(alu,kp,wr_POL)

    end subroutine wr_PolmagBoundaryGrid

end module wq_module_sjpack
