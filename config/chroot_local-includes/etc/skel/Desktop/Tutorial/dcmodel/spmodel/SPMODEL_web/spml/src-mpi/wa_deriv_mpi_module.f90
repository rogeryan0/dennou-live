!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_deriv_mpi_module
!
!  spml/wa_deriv_module モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法と MPI 並列化によって
!  数値計算するためのモジュール wa_mpi_module の下部モジュールであり, 
!  スペクトル法の微分計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_deriv_mpi_module モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!履歴  2008/05/28  竹広真一 wa_deriv_module をMPI 並列化
!
module wa_deriv_mpi_module
  !
  ! wa_deriv_mpi_module
  !
  !  spml/wa_deriv_module モジュールは球面上での流体運動を
  !  球面調和函数を用いたスペクトル法と MPI 並列化によって
  !  数値計算するためのモジュール wa_mpi_module の下部モジュールであり, 
  !  スペクトル法の微分計算のための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_deriv_mpi_module モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use dc_message, only : MessageNotify
  use w_base_module,  only : im, jm, nm
  use w_base_mpi_module,  only : jc
  use wa_base_mpi_module, only : xva_wa, wa_xva
  use w_deriv_mpi_module, only : w_JacobianMPI_w_w

  implicit none

  private
  private im, jm, nm                          ! Intel Fortran 対策
 
  public xva_GradLon_wa, xva_GradLat_wa       ! 勾配型微分
  public wa_DivLon_xva, wa_DivLat_xva         ! 発散型微分
  public wa_Div_xva_xva                       ! 発散型微分
  public wa_JacobianMPI_wa_wa                 ! ヤコビアン
  public xva_GradLambda_wa, xva_GradMu_wa     ! 勾配型微分(λ,μ座標)
  public wa_DivLambda_xva, wa_DivMu_xva       ! 発散型微分(λ,μ座標)

  contains

  !--------------- 微分計算 -----------------

    function xva_GradLon_wa(wa_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ

      real(8)              :: xva_GradLon_wa(im,jc,size(wa_data,2))
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      xva_GradLon_wa = xva_wa(wa_data,ipow=1,iflag=-1)
    end function xva_GradLon_wa

    function xva_GradLat_wa(wa_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ

      real(8)              :: xva_GradLat_wa(im,jc,size(wa_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      xva_GradLat_wa = xva_wa(wa_data,ipow=1,iflag=1)
    end function xva_GradLat_wa

    function wa_DivLon_xva(xva_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) 入力格子点データ

      real(8)              :: wa_DivLon_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      wa_DivLon_xva = wa_xva(xva_data,ipow=1,iflag=-1)
    end function wa_DivLon_xva

    function wa_DivLat_xva(xva_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !

      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) 入力格子点データ

      real(8)              :: wa_DivLat_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      wa_DivLat_xva = wa_xva(xva_data,ipow=1,iflag=1)
    end function wa_DivLat_xva

    function wa_Div_xva_xva(xva_u,xva_v)
      !
      ! 2 つの入力格子点データをベクトル成分とする発散を計算し, 
      ! スペクトルデータとして返す(多層用).
      !
      real(8), intent(in)  :: xva_u(:,:,:)
      !(in) ベクトル経度成分の格子点データ

      real(8), intent(in)  :: xva_v(:,:,:)
      !(in) ベクトル緯度成分の格子点データ

      real(8)              :: wa_Div_xva_xva((nm+1)*(nm+1),size(xva_u,3))
      !(out) 2 つの入力格子点データをベクトル成分とする発散のスペクトルデータ

      wa_Div_xva_xva = wa_DivLon_xva(xva_u) + wa_DivLat_xva(xva_v)
    end function wa_Div_xva_xva

    function wa_JacobianMPI_wa_wa(wa_a,wa_b)
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

      real(8)             :: wa_JacobianMPI_wa_wa((nm+1)*(nm+1),size(wa_a,2))
      !(out) 2 つのスペクトルデータのヤコビアン

      integer :: k

      do k=1,size(wa_a,2)
         wa_JacobianMPI_wa_wa(:,k) = w_JacobianMPI_w_w(wa_a(:,k),wa_b(:,k))
      end do
    end function wa_JacobianMPI_wa_wa


  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xva_GradLambda_wa(wa_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8)              :: xva_GradLambda_wa(im,jc,size(wa_data,2))
      !(in) 入力スペクトルデータ

      xva_GradLambda_wa = xva_wa(wa_data,ipow=0,iflag=-1)
    end function xva_GradLambda_wa

    function xva_GradMu_wa(wa_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(多層用).
      !
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ

      real(8)              :: xva_GradMu_wa(im,jc,size(wa_data,2))
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      xva_GradMu_wa = xva_wa(wa_data,ipow=0,iflag=1)
    end function xva_GradMu_wa

    function wa_DivLambda_xva(xva_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(多層用).
      !
      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) 入力格子点データ

      real(8)              :: wa_DivLambda_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      wa_DivLambda_xva = wa_xva(xva_data,ipow=2,iflag=-1)
    end function wa_DivLambda_xva

    function wa_DivMu_xva(xva_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(多層用).
      !

      real(8), intent(in)  :: xva_data(:,:,:)
      !(in) 入力格子点データ

      real(8)              :: wa_DivMu_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      wa_DivMu_xva = wa_xva(xva_data,ipow=2,iflag=1)
    end function wa_DivMu_xva

end module wa_deriv_mpi_module
