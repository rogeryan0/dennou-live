!--
!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  l_module_sjpack
!
!   spml/l_module_sjpack モジュールは球面上での経度方向に一様な帯状的 
!   1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
!   数値計算するための Fortran90 関数を提供する. 
!
!   内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!   関数, サブルーチンの名前と機能は l_module のものと同じに設計してある. 
!   したがって use 文を l_module から l_module_sjpack に変更するだけで 
!   SJPACK の機能が使えるようになる. 
!
!履歴  2009/09/22  竹広真一  l_module を元に SJPACK 用に改造
!
!++
module l_module_sjpack
  !
  != l_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: l_module_sjpack.f90,v 1.1 2009-09-24 07:12:09 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !   spml/l_module_sjpack モジュールは球面上での経度方向に一様な帯状的 
  !   1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
  !   数値計算するための Fortran90 関数を提供する. 
  !
  !   内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる.
  !   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !   ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  !   関数, サブルーチンの名前と機能は l_module のものと同じに設計してある. 
  !   したがって use 文を l_module から l_module_sjpack に変更するだけで 
  !   SJPACK の機能が使えるようになる. 
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
  integer               :: nm=21            ! 計算する最大の全波数の設定
  integer               :: nn=22            ! 切断波数(全波数)の設定
  integer               :: mm=1             ! 切断波数(東西波数)の設定

  real(8), allocatable  :: p(:,:), r(:)     ! 変換用配列
  real(8), allocatable  :: c(:)             ! 微分用配列

  real(8), allocatable  :: y_Lat(:)         ! 緯度経度
  real(8), allocatable  :: y_Lat_Weight(:)  ! 座標重み

  save jm, nm, nn, mm, p, r, c, y_Lat, y_Lat_Weight

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

      jm = j_in ; nn = n_in ; nm = n_in+1 ; mm = 1

      allocate(p(jm/2,mm+4))                  ! 変換用配列
      allocate(r((mm+1)*(2*nm-mm-1)+1))       ! 変換用配列
      allocate(c((nn+1)*(nn+1)))              ! 微分用配列

      allocate(y_Lat(jm),y_Lat_Weight(jm))      ! 座標変数配列

      call ljinit(mm,nm,jm,p,r)

      call sjinic(nn,c)

      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(p(j,1))        ! 緯度座標
         y_Lat(jm/2-j+1) = -asin(p(j,1))        ! 緯度座標
         y_Lat_Weight(jm/2+j)   = 2*p(j,2)      ! 緯度重み(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*p(j,2)      ! 緯度重み(Gauss grid)
      enddo

      call MessageNotify(&
        'M','l_initial','l_module_sjpack (2009/09/22) is initialized')

    end subroutine l_initial

  !--------------- 基本変換 -----------------
    function y_l(l_data)
      !
      ! スペクトルデータから格子データへ変換する(1 層用).
      !
      real(8)               :: y_l(1:jm)
      !(out) 格子点データ

      real(8), intent(in)   :: l_data(0:nn)
      !(in) スペクトルデータ

      real(8)  :: q(jm/2*7)                ! 変換用作業配列
      real(8)  :: ws(nn+1)                 ! 変換用作業配列

      call ljtszg(nm,nn,jm,l_data,y_l,p,q,r,ws,0)

    end function y_l

    function l_y(y_data)
      !
      ! スペクトルデータから格子データへ変換する(1 層用).
      !
      real(8)               :: l_y(0:nn)
      !(in) スペクトルデータ

      real(8), intent(in)   :: y_data(1:jm)
      !(in) 格子点データ


      real(8)  :: q(jm/2*7)               ! 変換a用作業配列
      real(8)  :: ws(nn+1)                ! 変換用作業配列

      call ljtgzs(nm,nn,jm,l_y,y_data,p,q,r,ws,0)

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
      real(8)              :: l_Lapla_l(0:nn)
      !(out) 入力スペクトルデータのラプラシアン

      real(8), intent(in)  :: l_data(0:nn)
      !(in) 入力スペクトルデータ

      integer :: n

      do n=0,nn
         l_Lapla_l(n) = -n*(n+1)*l_data(n)
      enddo

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

      integer ::  n

      l_LaplaInv_l(0) = 0.0D0
      do n=1,nn
         l_LaplaInv_l(n) = -l_data(n)/(n*(n+1))
      enddo

    end function l_LaplaInv_l

    function y_GradLat_l(l_data)
      !
      ! スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させて
      ! 格子点データに変換して返す(1 層用).
      !
      real(8)              :: y_GradLat_l(1:jm)
      !(out) スペクトルデータを勾配型緯度微分した格子点データ

      real(8), intent(in)  :: l_data(0:nn)
      !(in) 入力スペクトルデータ

      real(8)              :: l_ydata(0:nn+1)
      ! 作業スペクトルデータ

      real(8)  :: q(jm/2*7)                ! 変換用作業配列
      real(8)  :: ws(nn+1)                 ! 変換用作業配列

      call ljcszy(nn,l_data,l_ydata,c)
      call ljtszg(nm,nn+1,jm,l_ydata,y_GradLat_l,p,q,r,ws,1)

    end function y_GradLat_l

    function l_DivLat_y(y_data)
      !
      ! 格子点データに発散型緯度微分 1/cosφ・∂(f cosφ)/∂φ を作用させて
      ! スペクトルデータに変換して返す. 
      !
      real(8)              :: l_DivLat_y(0:nn)
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: y_data(1:jm)
      !(in) 入力格子点データ

      real(8)              :: l_ydata(0:nn+1)
      ! 作業スペクトルデータ

      real(8)  :: q(jm/2*7)                ! 変換用作業配列
      real(8)  :: ws(nn+1)                 ! 変換用作業配列

      call ljtgzs(nm,nn+1,jm,l_ydata,y_data,p,q,r,ws,1)
      call ljcyzs(nn,l_ydata,l_DivLat_y,c)

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
      real(8), intent(IN) :: l_data(0:nn)           ! スペクトルデータ
      real(8), intent(IN) :: alat                   ! 補間する位置(緯度)
      real(8)             :: Interpolate_l          ! 補間した値
      
      real(8) :: mu
      real(8) :: y0, y1, y2
      integer :: k

      mu = sin(alat)
      Interpolate_l = 0.0D0

      !---- Σa_n^0 L_n^0 の計算
      y2 = 0 ; y1 = 0
      do k=nn,1,-1
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

end module l_module_sjpack
