!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_deriv_module
!
!  spml/wa_deriv_module_sjpack モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module_sjpack の下部モジュールであり, スペクトル法の
!  微分計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_deriv_module_sjpack モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!履歴  2009/09/06  竹広真一 wa_deriv_module より SJPACK 用に改造
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
module wa_deriv_module_sjpack
  !
  != wa_deriv_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_deriv_module_sjpack.f90,v 1.1 2009-09-07 07:26:49 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !  spml/wa_deriv_module_sjpack モジュールは球面上での流体運動を
  !  球面調和函数を用いたスペクトル法によって数値計算するための 
  !  モジュール wa_module_sjpack の下部モジュールであり, スペクトル法の
  !  微分計算のための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_deriv_module_sjpack モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  use dc_message, only : MessageNotify
  use w_base_module_sjpack, only : im, jm, nm=>nn
  use wa_base_module_sjpack, only : xya_wa, wa_xya
  use w_module_sjpack, only : w_xy, xy_w, w_Lapla_w, w_LaplaInv_w, &
                              w_DLon_w, w_Jacobian_w_w

  implicit none

  private
 
  public wa_Lapla_wa, wa_LaplaInv_wa          ! ラプラシアンと逆演算
  public wa_DLon_wa                           ! 経度微分
  public xya_GradLon_wa, xya_GradLat_wa       ! 勾配型微分
  public wa_DivLon_xya, wa_DivLat_xya         ! 発散型微分
  public wa_Div_xya_xya                       ! 発散型微分
  public wa_Jacobian_wa_wa                    ! ヤコビアン
  public xya_GradLambda_wa, xya_GradMu_wa     ! 勾配型微分(λ,μ座標)
  public wa_DivLambda_xya, wa_DivMu_xya       ! 発散型微分(λ,μ座標)

  contains

  !--------------- 微分計算 -----------------
    function wa_Lapla_wa(wa_data)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !    ▽^2 = 1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用する(多層用).
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: wa_lapla_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) 入力スペクトルデータのラプラシアン
      integer :: k

      do k=1,size(wa_data,2)
         wa_Lapla_wa(:,k) = w_Lapla_w(wa_data(:,k))
      enddo
      
    end function wa_Lapla_wa

    function wa_LaplaInv_wa(wa_data)
      !
      ! 入力スペクトルデータに逆ラプラシアン
      !
      !    ▽^{-2}
      !      =[1/cos^2φ・∂^2/∂λ^2 + 1/cosφ・∂/∂φ(cosφ∂/∂φ)]^{-1}
      !
      ! を作用する(多層用).
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: wa_LaplaInv_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) スペクトルデータの逆ラプラシアン
      integer :: k

      do k=1,size(wa_data,2)
         wa_LaplaInv_wa(:,k) = w_LaplaInv_w(wa_data(:,k))
      enddo

    end function wa_LaplaInv_wa

    function wa_DLon_wa(wa_data)
      !
      ! スペクトルデータに経度微分 ∂/∂λ を作用させる(多層用).
      !
      ! スペクトルデータの経度微分とは, 対応する格子点データに
      ! 経度微分∂/∂λを作用させたデータのスペクトル変換のことである.
      ! 
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: wa_DLon_wa((nm+1)*(nm+1),size(wa_data,2))
      !(out) スペクトルデータの経度微分
      integer :: l,k

      do k=1,size(wa_data,2)
         wa_DLon_wa(:,k) = w_DLon_w(wa_Data(:,k))
      enddo

    end function wa_DLon_wa

    function xya_GradLon_wa(wa_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: xya_GradLon_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      xya_GradLon_wa = xya_wa(wa_data,ipow=1,iflag=-1)

    end function xya_GradLon_wa

    function xya_GradLat_wa(wa_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: xya_GradLat_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      xya_GradLat_wa = xya_wa(wa_data,ipow=1,iflag=1)

    end function xya_GradLat_wa

    function wa_DivLon_xya(xya_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) 入力格子点データ
      real(8)              :: wa_DivLon_xya((nm+1)**2,size(xya_data,3))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      wa_DivLon_xya = wa_xya(xya_data,ipow=1,iflag=-1)

    end function wa_DivLon_xya

    function wa_DivLat_xya(xya_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) 入力格子点データ
      real(8)              :: wa_DivLat_xya((nm+1)**2,size(xya_data,3))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      wa_DivLat_xya = wa_xya(xya_data,ipow=1,iflag=1)

    end function wa_DivLat_xya

    function wa_Div_xya_xya(xya_u,xya_v)
      !
      ! 2 つの入力格子点データをベクトル成分とする発散を計算し, 
      ! スペクトルデータとして返す(多層用).
      !
      real(8), intent(in)  :: xya_u(0:,:,:)
      !(in) ベクトル経度成分の格子点データ
      real(8), intent(in)  :: xya_v(0:,:,:)
      !(in) ベクトル緯度成分の格子点データ
      real(8)              :: wa_Div_xya_xya((nm+1)**2,size(xya_u,3))
      !(out) 2 つの入力格子点データをベクトル成分とする発散のスペクトルデータ

      wa_Div_xya_xya = wa_DivLon_xya(xya_u) + wa_DivLat_xya(xya_v)

    end function wa_Div_xya_xya

    function wa_Jacobian_wa_wa(wa_a,wa_b)
      ! 2 つのスペクトルデータにヤコビアン
      !
      !   J(f,g) = ∂f/∂λ・∂g/∂μ - ∂g/∂λ・∂f/∂μ
      !          = ∂f/∂λ・1/cosφ・∂g/∂φ
      !             - ∂g/∂λ・1/cosφ・∂f/∂φ
      !
      ! を作用させる(多層用).
      !
      real(8), intent(in) :: wa_a(:,:)
      !(in) 1つ目の入力スペクトルデータ
      real(8), intent(in) :: wa_b(:,:)
      !(in) 2つ目の入力スペクトルデータ
      real(8)             :: wa_Jacobian_wa_wa((nm+1)**2,size(wa_a,2))
      !(out) 2 つのスペクトルデータのヤコビアン
      integer :: k

      do k=1,size(wa_a,2)
         wa_Jacobian_wa_wa(:,k) = w_Jacobian_w_w(wa_a(:,k),wa_b(:,k))
      end do
    end function wa_Jacobian_wa_wa


  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xya_GradLambda_wa(wa_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: xya_GradLambda_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      xya_GradLambda_wa = xya_wa(wa_data,ipow=0,iflag=-1)

    end function xya_GradLambda_wa

    function xya_GradMu_wa(wa_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: xya_GradMu_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      xya_GradMu_wa = xya_wa(wa_data,ipow=0,iflag=1)

    end function xya_GradMu_wa

    function wa_DivLambda_xya(xya_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) 入力格子点データ
      real(8)              :: wa_DivLambda_xya((nm+1)**2,size(xya_data,3))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      wa_DivLambda_xya = wa_xya(xya_data,ipow=2,iflag=-1)

    end function wa_DivLambda_xya

    function wa_DivMu_xya(xya_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xya_data(0:,:,:)
      !(in) 入力格子点データ
      real(8)              :: wa_DivMu_xya((nm+1)**2,size(xya_data,3))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      wa_DivMu_xya = wa_xya(xya_data,ipow=2,iflag=1)

    end function wa_DivMu_xya

end module wa_deriv_module_sjpack

