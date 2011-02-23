!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2010 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_deriv_module
!
!  spml/w_deriv_module モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module の下部モジュールであり, スペクトル法の
!  微分計算のための Fortran90 関数を提供する. 
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!  このモジュールを使うためには前もって w_base_initial を呼んで
!  切断波数, 格子点数の設定をしておく必要がある. 
!
!
!履歴  2001/12/08  竹広真一
!      2001/12/26  竹広真一  関数,変数名前変更
!      2002/02/07  竹広真一  関数,変数名前再変更
!      2002/03/30  竹広真一  関数,変数名前再再変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2005/07/04  竹広真一  OPENMP 版変換ルーチンに対応
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2008/05/31  竹広真一  初期化ルーチンを分離
!      2008/06/22  佐々木洋平 格子点データの配列開始点を 1 から 0 へ.
!      2008/06/23  佐々木洋平 格子点データの格納は (0:im-1, 1:jm) へ.
!      2008/07/01  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/09  竹広真一  w_deriv_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/30  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!      2010/01/26  竹広真一   ラプラシアン演算用変数の public 化
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module w_deriv_module_sjpack
  !
  != w_deriv_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_deriv_module_sjpack.f90,v 1.2 2010-01-26 12:34:06 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_deriv_module_sjpack モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module_sjpack の下部モジュールであり, スペクトル法の
  ! 微分計算のための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  ! このモジュールを使うためには前もって w_base_initial を呼んで
  ! 切断波数, 格子点数の設定をしておく必要がある. 
  !
  use dc_message, only : MessageNotify
  use w_base_module_sjpack, only : im, jm, nm=>nn, mm, it, t, r, &
                                   w_base_Initial, xy_w, w_xy
  implicit none

  real(8), allocatable  :: D(:)
  ! ラプラシアン演算用配列
  !
  ! スペクトルデータのラプラシアンを計算するための係数
  ! 配列のサイズは((mm+1)*(mm+1)*2)
  !

  real(8), allocatable  :: rn(:,:)            
  ! ラプラシアン演算用配列(w_module と互換性を保つため)
  !
  ! スペクトルデータのラプラシアンを計算するための係数
  ! 配列のサイズは((nm+1)*(nm+1), 2)
  !
  ! r(L,1) には L 番目の格納位置のスペクトルに対するラプラシアン計算の
  ! 係数 -n(n+1) の値が格納されている.
  !

  private

  public w_deriv_Initial                      ! 初期化
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public rn                                   ! ラプラシアン演算用配列

  save D, rn

  contains

  !--------------- 初期化 -----------------
    subroutine w_deriv_initial
      !
      ! スペクトル微分計算に必要となる作業領域を設定する. 
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない. 
      !
      ! このサブルーチンを単独で用いるのでなく, 
      ! 上位サブルーチン w_Initial を使用すること.
      !
      allocate(D((nm+1)*(nm+1)*2))           ! ラプラシアン演算用配列
      allocate(rn((nm+1)*(nm+1),2))          ! ラプラシアン演算用配列

      call sjinid(mm,D)

      rn = reshape(D,(/(nm+1)**2,2/))

      call MessageNotify('M','w_deriv_initial',&
           'w_deriv_module_sjpack (2010/01/26) is initialized')

    end subroutine w_deriv_initial

  !--------------- 微分計算 -----------------
    function w_Lapla_w(w_data)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !    ▽^2 = 1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用する(1 層用).
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: w_Lapla_w((nm+1)*(nm+1))
      !(out) 入力スペクトルデータのラプラシアン

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call sjclap(mm,w_data,w_Lapla_w,D,1)

    end function w_Lapla_w

    function w_LaplaInv_w(w_data)
      !
      ! 入力スペクトルデータに逆ラプラシアン
      !
      !    ▽^{-2}
      !      =[1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)]^{-1}
      !
      ! を作用する(1 層用).
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: w_LaplaInv_w((nm+1)*(nm+1))
      !(out) スペクトルデータの逆ラプラシアン

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call sjclap(mm,w_data,w_LaplaInv_w,D,2)

    end function w_LaplaInv_w

    function w_DLon_w(w_data)
      !
      ! スペクトルデータに経度微分 ∂/∂λ を作用させる(1 層用).
      !
      ! スペクトルデータの経度微分とは, 対応する格子点データに
      ! 経度微分∂/∂λを作用させたデータのスペクトル変換のことである.
      ! 
      real(8)              :: w_DLon_w((nm+1)*(nm+1))
      !(out) スペクトルデータの経度微分

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      call sjcs2x(mm,w_data,w_DLon_w)

    end function w_DLon_w

    function xy_GradLon_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(1 層用).
      !
      real(8)              :: xy_GradLon_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradLon_w = xy_w(w_data,ipow=1,iflag=-1)

    end function xy_GradLon_w

    function xy_GradLat_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(1 層用).
      !
      real(8)              :: xy_GradLat_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradLat_w = xy_w(w_data,ipow=1,iflag=1)

    end function xy_GradLat_w

    function w_DivLon_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLon_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ
      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLon_xy = w_xy(xy_data,ipow=1,iflag=-1)

    end function w_DivLon_xy

    function w_DivLat_xy(xy_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLat_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLat_xy = w_xy(xy_data,ipow=1,iflag=1)

    end function w_DivLat_xy

    function w_Div_xy_xy(xy_u,xy_v)
      !
      ! 2 つの入力格子点データをベクトル成分とする発散を計算し, 
      ! スペクトルデータとして返す(1 層用).
      !
      real(8)              :: w_Div_xy_xy((nm+1)*(nm+1))
      !(out) 2 つの入力格子点データをベクトル成分とする発散のスペクトルデータ

      real(8), intent(in)  :: xy_u(0:im-1,1:jm)
      !(in) ベクトル経度成分の格子点データ

      real(8), intent(in)  :: xy_v(0:im-1,1:jm)
      !(in) ベクトル緯度成分の格子点データ

      w_Div_xy_xy = w_Divlon_xy(xy_u) + w_Divlat_xy(xy_v)

    end function w_Div_xy_xy

    function w_Jacobian_w_w(w_a,w_b)
      ! 2 つのスペクトルデータにヤコビアン
      !
      !   J(f,g) = ∂f/∂λ・∂g/∂μ - ∂g/∂λ・∂f/∂μ
      !          = ∂f/∂λ・1/cosφ・∂g/∂φ
      !             - ∂g/∂λ・1/cosφ・∂f/∂φ
      !
      ! を作用させる(1 層用).

      real(8)             :: w_Jacobian_w_w((nm+1)*(nm+1))
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1つ目の入力スペクトルデータ
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2つ目の入力スペクトルデータ

      w_Jacobian_w_w = w_xy( &
                  xy_w(w_DLon_w(w_a))*xy_w(w_b,ipow=2,iflag=1) &
                - xy_w(w_DLon_w(w_b))*xy_w(w_a,ipow=2,iflag=1) )

    end function w_Jacobian_w_w

  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xy_GradLambda_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(1 層用).
      !
      real(8)              :: xy_GradLambda_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ
      
      xy_GradLambda_w = xy_w(w_data,ipow=0,iflag=-1)

    end function xy_GradLambda_w

    function xy_GradMu_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(1 層用).
      !
      real(8)              :: xy_GradMu_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xy_GradMu_w = xy_w(w_data,ipow=0,iflag=1)

    end function xy_GradMu_w

    function w_DivLambda_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLambda_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivLambda_xy = w_xy(xy_data,ipow=2,iflag=-1)

    end function w_DivLambda_xy

    function w_DivMu_xy(xy_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivMu_xy((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivMu_xy = w_xy(xy_data,ipow=2,iflag=1)

    end function w_DivMu_xy

  end module w_deriv_module_sjpack
