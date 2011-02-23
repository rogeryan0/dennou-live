!--
!----------------------------------------------------------------------
! Copyright (c) 2008--2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  l_module
!
!   spml/l_module モジュールは球面上での経度方向に一様な帯状的 1 次元
!   流体運動をルジャンドル多項式を用いたスペクトル法によって数値計算する
!   ための Fortran90 関数を提供する. 
!
!   内部で ISPACK の LTPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/LTPACK のマニュアルを参照されたい.
!
!履歴  2008/12/24  竹広真一  1 層用
!      2009/01/09  竹広真一  l_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/10/04  竹広真一  作業用配列 p をローカル変数に変更
!
!++
module l_module
  !
  != l_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: l_module.f90,v 1.6 2009-10-04 04:06:37 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/l_module モジュールは球面上での経度方向に一様な帯状的 1 次元
  ! 流体運動をルジャンドル多項式を用いたスペクトル法によって数値計算する
  ! ための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の LTPACK の Fortran77 サブルーチンを呼んでいる.
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/LTPACK のマニュアルを参照されたい.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (l_, y_) は, 返す値の形を示している.
  !   l_ :: スペクトル(ルジャンドル多項式成分)データ
  !   y_ :: 1 次元緯度格子点データ
  !
  ! * 関数名の間の文字列(GradLat, DivLat, Lapla, LaplaInv)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_l, _y) は, 入力変数の形スペクトルデータ
  !   および格子点データであることを示している.
  !   _l :: スペクト(ルジャンドル多項式成分)ルデータ
  !   _y :: 緯度方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * p : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(0:nm). 
  !   * nm はルジャンドル多項式の最大次数であり, サブルーチン l_Initial にて
  !     あらかじめ設定しておく. 
  !
  ! * y : 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(1:jm).
  !
  ! * l_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * y_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! l_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! y_Lat        ::  格子点座標(緯度, 経度座標)を格納した 1 次元配列
  ! y_Lat_Weight ::  重み座標を格納した 1 次元配列
  !
  !==== 基本変換
  !
  ! y_l :: スペクトルデータから格子データへの変換
  ! l_y :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! l_Lapla_l       :: スペクトルデータにラプラシアンを作用させる
  ! l_LaplaInv_l    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! y_GradLat_l     :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! l_DivLat_y      :: 格子データに発散型緯度微分
  !                    1 /cosφ・∂(g cosφ)/∂φを作用させる
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! y_GradMu_l     :: スペクトルデータに
  !                   勾配型緯度微分 (1-μ^2)∂/∂μを作用させる
  ! l_DivMu_y      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  !==== 補間
  !
  ! Interpolate_l  :: スペクトルデータから任意の点での値を求める. 
  !
  !==== 積分・平均
  !
  ! IntLat_y, AvrLat_y :: 1 次元(Y)格子点データの緯度方向積分および平均
  !
  !==== スペクトル解析
  !
  ! 作成中
  !
  use dc_message, only : MessageNotify

  implicit none

  private

  public l_Initial                            ! 初期化

  public y_Lat                                ! 格子座標
  public y_Lat_Weight                         ! 格子座標重み

  public y_l, l_y                             ! 変換関数
  public l_Lapla_l, l_LaplaInv_l              ! ラプラシアンと逆演算
  public y_GradLat_l                          ! 勾配型微分
  public l_DivLat_y                           ! 発散型微分

  public y_GradMu_l                           ! 勾配型微分
  public l_DivMu_y                            ! 発散型微分

  public IntLat_y, AvrLat_y                   ! 緯度平均

  public Interpolate_l                        ! 補間計算

  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: nm=21            ! 切断波数の設定

  real(8), allocatable  :: q(:,:,:), r(:)   ! 変換用配列

  real(8), allocatable  :: y_Lat(:)         ! 緯度経度
  real(8), allocatable  :: y_Lat_Weight(:)  ! 座標重み

  save jm, nm, q, r, y_Lat, y_Lat_Weight

contains

  !--------------- 初期化 -----------------
    subroutine l_initial(n_in,j_in)
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断波数の設定

      integer :: j

      jm = j_in ; nm = n_in

      allocate(q(jm/2,2,0:nm),r((nm+1)*(nm+1))) ! 変換用配列
      allocate(y_Lat(jm),y_Lat_Weight(jm))      ! 座標変数配列

      call ltinit(nm,jm,q,r)

      call ltogrd(jm,y_Lat,q)

      do j=1,jm/2
         y_Lat_Weight(jm/2+j)   = 2.0D0*q(j,1,0)
         y_Lat_Weight(jm/2+1-j) = y_Lat_Weight(jm/2+j)
      enddo

      call MessageNotify(&
        'M','l_initial','l_module (2009/10/04) is initialized')

    end subroutine l_initial

  !--------------- 基本変換 -----------------
    function y_l(l_data)
      !
      ! スペクトルデータから格子データへ変換する
      !
      real(8)               :: y_l(1:jm)
      !(out) 格子点データ

      real(8), intent(in)   :: l_data(0:nm)
      !(in) スペクトルデータ

      real(8)               :: l_in(0:nm)
      !(in) スペクトルデータ

      real(8)               :: p(jm) 
      ! 作業配列

      l_in = l_data       ! lts2gz は入力データを保存しないので入れ換えておく

      call lts2gz(nm,jm,l_in,y_l,p,q,r)

    end function y_l

    function l_y(y_data)
      !
      ! 格子データからスペクトルデータへ変換する
      !
      real(8)               :: l_y(0:nm)
      !(out) 格子点データ

      real(8), intent(in)   :: y_data(1:jm)
      !(in) スペクトルデータ

      real(8)               :: y_in(1:jm)
      !(in) スペクトルデータ

      real(8)               :: p(jm) 
      ! 作業配列

      y_in = y_data    ! ltg2sz は入力データを保存しないので入れ換えておく

      call ltg2sz(nm,jm,y_in,l_y,p,q,r)

    end function l_y

  !--------------- 微分計算 -----------------
    function l_Lapla_l(l_data)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !    ▽^2 = 1/cosφ・∂/∂φ(cosφ∂/∂φ)
      !
      ! を作用する. 
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: l_Lapla_l(0:nm)
      !(out) 入力スペクトルデータのラプラシアン

      real(8), intent(in)  :: l_data(0:nm)
      !(in) 入力スペクトルデータ

      call ltclfz(nm,l_data,l_Lapla_l)

    end function l_Lapla_l

    function l_LaplaInv_l(l_data)
      !
      ! 入力スペクトルデータに逆ラプラシアン
      !
      !    ▽^{-2}
      !      =[1/cosφ・∂/∂φ(cosφ∂/∂φ)]^{-1}
      !
      ! を作用する. 
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8)              :: l_LaplaInv_l(0:nm)
      !(out) スペクトルデータの逆ラプラシアン

      real(8), intent(in)  :: l_data(0:nm)
      !(in) 入力スペクトルデータ

      call ltclbz(nm,l_data,l_LaplaInv_l)

    end function l_LaplaInv_l

    function y_GradLat_l(l_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(1 層用).
      !
      real(8)              :: y_GradLat_l(1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: l_data(0:nm)
      !(in) 入力スペクトルデータ

      real(8)              :: p(jm) 
      ! 作業配列

      call lts2vz(nm,jm,l_data,y_GradLat_l,p,q,r)

    end function y_GradLat_l

    function l_DivLat_y(y_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す. 
      !
      real(8)              :: l_DivLat_y(0:nm)
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: y_data(1:jm)
      !(in) 入力格子点データ

      real(8)              :: p(jm) 
      ! 作業配列

      call ltv2sz(nm,jm,y_data,l_DivLat_y,p,q,r)

    end function l_DivLat_y

  !--------------- 微分計算 (μ座標系用) -----------------
    function y_GradMu_l(l_data)
      !
      ! スペクトルデータに勾配型緯度微分 (1-μ^2)∂/∂μ  (μ=sinφ)
      ! を作用させて格子点データに変換して返す. 
      !
      real(8)              :: y_GradMu_l(1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: l_data(0:nm)
      !(in) 入力スペクトルデータ

      y_GradMu_l = y_GradLat_l(l_data)*cos(y_Lat)

    end function y_GradMu_l

    function l_DivMu_y(y_data)
      !
      ! 格子点データに発散型緯度微分 ∂/∂μ (μ=sinφ)を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: l_DivMu_y(0:nm)
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: y_data(1:jm)
      !(in) 入力格子点データ

      l_DivMu_y = l_DivLat_y(y_data/cos(y_Lat))

    end function l_DivMu_y

  !--------------- 積分計算 -----------------
    function IntLat_y(y_data)
      !
      ! 1 次元緯度(Y)格子点データの Y 方向積分.
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 次元緯度(Y)格子点データ
      real(8)             :: IntLat_y        !(out) 積分値

      IntLat_y = sum(y_data * y_Lat_weight)

    end function IntLat_y

  !--------------- 平均計算 -----------------
    function AvrLat_y(y_data)
      !
      ! 1 次元(Y)格子点データの緯度(Y)方向平均.
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: y_data(1:jm)    !(in)  1 次元緯度格子点データ
      real(8)             :: AvrLat_y        !(out) 平均値

      AvrLat_y = IntLat_y(y_data)/sum(y_Lat_weight)

    end function AvrLat_y

  !--------------- 補間計算 -----------------    
    function Interpolate_l(l_data,alat)
      !
      ! 緯度 alat における関数値を
      ! そのルジャンドル変換係数 l_data から補間計算する
      !
      real(8), intent(IN) :: l_data(0:nm)           ! スペクトルデータ
      real(8), intent(IN) :: alat                   ! 補間する位置(緯度)
      real(8)             :: Interpolate_l          ! 補間した値
      
      real(8) :: mu
      real(8) :: y0, y1, y2
      integer :: k

      mu = sin(alat)
      Interpolate_l = 0.0D0

      !---- Σa_n^0 L_n^0 の計算
      y2 = 0 ; y1 = 0
      do k=nm,1,-1
         y0 = alpha(k,mu) * y1 + beta(k+1)*y2 + l_data(k)
         y2 = y1 ; y1 = y0
      enddo
      Interpolate_l = beta(1) * y2 + mu*sqrt(3.0D0) * y1 + l_data(0) 

    end function Interpolate_l

  !--------------- 下部ルーチン -----------------
    function alpha(n,x)
      !
      !  漸化式の P_n の係数
      !
      integer, intent(IN) :: n
      real(8), intent(IN) :: x
      real(8)             :: alpha

      alpha = sqrt( (2.0D0*n+3)*(2.0D0*n+1)/((n+1)*(n+1)) ) * x
    end function alpha

    function beta(n)
      !
      !  漸化式の P_{n-1} の係数
      !
      integer, intent(IN) :: n
      real(8)             :: beta

      beta = - sqrt( (2.0D0*n+3)*n*n/((2*n-1)*(n+1)*(n+1)) )
    end function beta

end module l_module
