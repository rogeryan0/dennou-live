!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
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
!
!履歴  2008/05/26  竹広真一  w_deriv_module を MPI 化
!
module w_deriv_mpi_module
  !
  ! w_deriv_mpi_module
  !
  !  spml/w_deriv_module モジュールは球面上での 2 次元流体運動を
  !  球面調和函数を用いたスペクトル法と MPI によって数値計算するための 
  !  モジュール w_mpi_module の下部モジュールであり, スペクトル法の
  !  微分計算のための Fortran90 関数を提供する. 
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : im, jm, nm
  use w_base_mpi_module, only : it, t, y, ip, p, r, ia, a, &
                                jc, id, jd, xv_w, w_xv
  implicit none
  private im, jm, nm                          ! Intel Fortran 対策

  integer, allocatable  :: ip2(:), ip3(:)     ! ヤコビアン計算用配列
  real(8), allocatable  :: p2(:), p3(:)       ! ヤコビアン計算用配列
  real(8), allocatable  :: r2(:), r3(:)       ! ヤコビアン計算用配列

  real(8), allocatable  :: q(:)               ! 作業配列
  real(8), allocatable  :: ww(:),ws(:),w(:)   ! 作業配列

  private

  public w_deriv_mpi_Initial                  ! 初期化
  public xv_GradLon_w, xv_GradLat_w           ! 勾配型微分
  public w_DivLon_xv, w_DivLat_xv             ! 発散型微分
  public w_Div_xv_xv                          ! 発散型微分
  public w_JacobianMPI_w_w                    ! ヤコビアン
  public xv_GradLambda_w, xv_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xv, w_DivMu_xv           ! 発散型微分(λ,μ座標)

  save ip2, ip3, p2, p3, r2, r3

  contains

  !--------------- 初期化 -----------------
    subroutine w_deriv_mpi_initial
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      integer iw

      allocate(ip2(2*((nm+1)/2+nm+1)*2))      ! ヤコビアン計算用配列
      allocate(p2(2*((nm+1)/2+nm+1)*jm))      ! ヤコビアン計算用配列
      allocate(r2(2*((nm+1)/2*2+3)*(nm/2+1))) ! ヤコビアン計算用配列
      allocate(ip3(3*((nm+1)/2+nm+1)*2))      ! ヤコビアン計算用配列
      allocate(p3(3*((nm+1)/2+nm+1)*jm))      ! ヤコビアン計算用配列
      allocate(r3(3*((nm+1)/2*2+3)*(nm/2+1))) ! ヤコビアン計算用配列
      call snkini(nm,jc,2,ip,p,r,ip2,p2,r2)
      call snkini(nm,jc,3,ip,p,r,ip3,p3,r3)

      allocate(q(3*((nm+1)/2+nm+1)*jm))       ! 作業用配列
      iw=3*max( ((nm+1)/2*2+3)*(nm/2+2)*2, &
                jm*((nm+1)/2+nm+1)*2, jm*jm )
      allocate(ws(iw),ww(iw),w((nm+1)*(nm+1)))   ! 作業用配列

      call MessageNotify('M','w_deriv_mpi_initial', &
           'w_deriv_mpi_module is initialized')

    end subroutine w_deriv_mpi_initial

  !--------------- 微分計算 -----------------
    function xv_GradLon_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(1 層用).
      !
      real(8)              :: xv_GradLon_w(0:im-1,jc)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xv_GradLon_w = xv_w(w_data,ipow=1,iflag=-1)
    end function xv_GradLon_w

    function xv_GradLat_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(1 層用).
      !
      real(8)              :: xv_GradLat_w(0:im-1,jc)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xv_GradLat_w = xv_w(w_data,ipow=1,iflag=1)
    end function xv_GradLat_w

    function w_DivLon_xv(xv_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLon_xv((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ
      
      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) 入力格子点データ

      w_DivLon_xv = w_xv(xv_data,ipow=1,iflag=-1)
    end function w_DivLon_xv

    function w_DivLat_xv(xv_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLat_xv((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) 入力格子点データ

      w_DivLat_xv = w_xv(xv_data,ipow=1,iflag=1)
    end function w_DivLat_xv

    function w_Div_xv_xv(xv_u,xv_v)
      !
      ! 2 つの入力格子点データをベクトル成分とする発散を計算し, 
      ! スペクトルデータとして返す(1 層用).
      !
      real(8)              :: w_Div_xv_xv((nm+1)*(nm+1))
      !(out) 2 つの入力格子点データをベクトル成分とする発散のスペクトルデータ

      real(8), intent(in)  :: xv_u(0:im-1,jc)
      !(in) ベクトル経度成分の格子点データ

      real(8), intent(in)  :: xv_v(0:im-1,jc)
      !(in) ベクトル緯度成分の格子点データ

      w_Div_xv_xv = w_Divlon_xv(xv_u) + w_Divlat_xv(xv_v)
    end function w_Div_xv_xv

    function w_JacobianMPI_w_w(w_a,w_b)
      ! 2 つのスペクトルデータにヤコビアン
      !
      !   J(f,g) = ∂f/∂λ・∂g/∂μ - ∂g/∂λ・∂f/∂μ
      !          = ∂f/∂λ・1/cosφ・∂g/∂φ
      !             - ∂g/∂λ・1/cosφ・∂f/∂φ
      !
      ! を作用させる(1 層用).

      real(8)             :: w_JacobianMPI_w_w((nm+1)*(nm+1))
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1つ目の入力スペクトルデータ
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2つ目の入力スペクトルデータ

      call spmjcb(nm,im,id,jc,jd,w_a,w_b,w_JacobianMPI_w_w,&
           it,t,y,ip2,p2,r2,ip3,p3,r3,ia,a,q,ws,ww,w)
    end function w_JacobianMPI_w_w

  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xv_GradLambda_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(1 層用).
      !
      real(8)              :: xv_GradLambda_w(0:im-1,jc)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ
      
      xv_GradLambda_w = xv_w(w_data,ipow=0,iflag=-1)
    end function xv_GradLambda_w

    function xv_GradMu_w(w_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す(1 層用).
      !
      real(8)              :: xv_GradMu_w(0:im-1,jc)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: w_data((nm+1)*(nm+1))
      !(in) 入力スペクトルデータ

      xv_GradMu_w = xv_w(w_data,ipow=0,iflag=1)
    end function xv_GradMu_w

    function w_DivLambda_xv(xv_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLambda_xv((nm+1)*(nm+1))
      !(out) 格子点データを発散型経度微分したスペクトルデータ

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) 入力格子点データ

      w_DivLambda_xv = w_xv(xv_data,ipow=2,iflag=-1)
    end function w_DivLambda_xv

    function w_DivMu_xv(xv_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivMu_xv((nm+1)*(nm+1))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xv_data(0:im-1,jc)
      !(in) 入力格子点データ

      w_DivMu_xv = w_xv(xv_data,ipow=2,iflag=1)
    end function w_DivMu_xv

end module w_deriv_mpi_module
