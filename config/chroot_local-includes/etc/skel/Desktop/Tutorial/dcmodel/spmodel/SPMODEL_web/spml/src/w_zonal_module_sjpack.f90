!--
!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_zonal_module_sjpack
!
!   spml/w_zonal_module_sjpack モジュールは球面上での経度方向に一様な
!   帯状的 1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
!   数値計算するための Fortran90 関数を提供する. 
!
!   内部で l_module_sjpack を用いている. 最下部ではルジャンドル多項式変換
!   のエンジンとして ISPACK の Fortran77 サブルーチンを用いている.
!
!   関数, サブルーチンの名前と機能は w_zonal_module のものと同じに設計してある. 
!   したがって use 文を l_module から w_zonal_module_sjpack に変更するだけで 
!   SJPACK の機能が使えるようになる. 
!
!
!履歴  2009/09/22  竹広真一  w_zonal_module を SJPACK 用に改造
!
!++
module w_zonal_module_sjpack
  !
  != w_zonal_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro
  ! Version:: $Id: w_zonal_module_sjpack.f90,v 1.1 2009-09-24 07:12:09 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !   spml/w_zonal_module_sjpack モジュールは球面上での経度方向に一様な
  !   帯状的 1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
  !   数値計算するための Fortran90 関数を提供する. 
  !
  !   内部で l_module_sjpack を用いている. 最下部ではルジャンドル多項式変換
  !   のエンジンとして ISPACK の Fortran77 サブルーチンを用いている.
  !
  !   関数, サブルーチンの名前と機能は w_zonal_module のものと同じに設計してある. 
  !   したがって use 文を l_module から w_zonal_module_sjpack に変更するだけで 
  !   SJPACK の機能が使えるようになる. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (w_, nm_, n_, xy_, x_, y_) は, 返す値の形を示している.
  !   w_   ::  スペクトルデータ
  !   xy_  ::  2 次元格子点データ
  !   nm_  ::  スペクトルデータの並んだ 3 次元配列(スペクトルデータの並びは
  !            全波数 n, 帯状波数 m で指定される 2 次元配列)
  !   n_   ::  スペクトルデータの並んだ 2 次元配列 (スペクトルデータの並びは
  !            全波数 n で指定される 1 次元配列)
  !   x_   ::  経度方向 1 次元格子点データ
  !   y_   ::  緯度方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_w_w, _w, _xy, _x, _y) は, 入力変数の形スペクトルデータ
  !   および格子点データであることを示している.
  !   _w   :: スペクトルデータ
  !   _w_w :: 2 つのスペクトルデータ
  !   _xy  :: 2 次元格子点データ
  !   _x   :: 経度方向 1 次元格子点データ
  !   _y   :: 緯度方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * xy : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm). 
  !   * im, jm はそれぞれ経度, 緯度座標の格子点数であり, サブルーチン
  !     w_Initial にてあらかじめ設定しておく.
  !
  ! * w : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(nm+1). 
  !   * nm はルジャンドル多項式の最大次数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく. w_module と配列の大きさが異なることに注意. 
  !
  ! * nm : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm). 
  !     第 1 次元が水平全波数,  第 2 次元が帯状波数(ダミー)を表す. 
  !   * nm はルジャンドル多項式の最大次数であり, サブルーチン w_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * n : スペクトルデータの並んだ 1 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm). 
  !   * 第 1 次元が水平全波数を表す. nm は球面調和函数の最大全波数であり, 
  !     サブルーチン w_Initial にてあらかじめ設定しておく.
  !
  ! * x, y : 経度, 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1) 
  !     および real(8), dimension(1:jm).
  !
  ! * w_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * xy_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * x_, y_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! w_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_Lon, y_Lat     ::  格子点座標(緯度, 経度座標)を格納した 1 次元配列
  ! x_Lon_Weight, y_Lat_Weight ::  重み座標を格納した 1 次元配列
  ! xy_Lon, xy_Lat   :: 格子点データの経度・緯度座標(X,Y)
  !                     (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! xy_w :: スペクトルデータから格子データへの変換
  ! w_xy :: 格子データからスペクトルデータへの変換
  ! l_nm, nm_l :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! w_Lapla_w       :: スペクトルデータにラプラシアンを作用させる
  ! rn              :: スペクトルデータのラプラシアンを計算するための係数. 
  ! irm             :: 経度微分演算用配列
  ! w_LaplaInv_w    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! w_DLon_w        :: スペクトルデータに経度微分∂/∂λを作用させる
  ! xy_GradLon_w    :: スペクトルデータに
  !                    勾配型経度微分 1/cosφ・∂/∂λを作用させる
  ! xy_GradLat_w    :: スペクトルデータに勾配型緯度微分∂/∂φを作用させる
  ! w_DivLon_xy     :: 格子データに
  !                    発散型経度微分 1/cosφ・∂/∂λを作用させる
  ! w_DivLat_xy     :: 格子データに
  !                    発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる
  ! w_Div_xy_xy     :: ベクトル成分である 2 つの格子データに発散を作用させる
  ! w_Jacobian_w_w  :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! xy_GradLambda_w :: スペクトルデータに勾配型経度微分∂/∂λを作用させる
  ! xy_GradMu_w     :: スペクトルデータに
  !                    勾配型緯度微分 (1-μ^2)∂/∂μを作用させる
  ! w_DivLambda_xy  :: 格子データに
  !                    発散型経度微分 1/(1-μ^2)・∂/∂λを作用させる
  ! w_DivMu_xy      :: 格子データに発散型緯度微分∂/∂μを作用させる
  !
  !==== 補間
  !
  ! Interpolate_w :: スペクトルデータから任意の点での値を求める. 
  !
  !==== 積分・平均
  !
  ! IntLonLat_xy, AvrLonLat_xy :: 2 次元格子点データの全領域積分および平均
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 次元格子点データの経度方向積分および平均
  ! IntLon_x, AvrLon_x         :: 1 次元(X)格子点データの経度方向積分および平均
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 次元格子点データの緯度方向積分および平均
  ! IntLat_y, AvrLat_y         :: 1 次元(Y)格子点データの緯度方向積分および平均
  !
  !==== スペクトル解析
  !
  ! nm_EnergyFromStreamfunc_w  :: 流線関数からエネルギースペクトルを
  !                               計算する (水平全波数 n, 帯状波数 m 空間)
  ! n_EnergyFromStreamfunc_w   :: 流線関数からエネルギースペクトルを
  !                               計算する (水平全波数 n 空間) 
  ! nm_EnstrophyFromStreamfunc_w  :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n, 帯状波数 m 空間)
  ! n_EnstrophyFromStreamfunc_w   :: 流線関数からエンストロフィースペクトルを
  !                                  計算する (水平全波数 n 空間)
  ! w_spectrum_VMiss              ::  欠損値
  !
  !
  use dc_message
  use l_module_sjpack, w_y => l_y, y_w => y_l, &
       y_GradLat_w => y_GradLat_l, w_DivLat_y => l_DivLat_y, &
       w_Lapla_w => l_Lapla_l, w_LaplaInv_w => l_LaplaInv_l, &
       Interpolate_alat_w => Interpolate_l

  implicit none

  integer               :: im=64            ! 格子点の設定(東西)
  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: nm=21            ! 切断波数の設定
  integer               :: np=1             ! OPENMP 最大スレッド数

  real(8), allocatable  :: x_Lon(:)                  ! 経度経度
  real(8), allocatable  :: x_Lon_Weight(:)           ! 座標重み
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  real(8), allocatable  :: rn(:,:)
  integer, allocatable  :: irm(:,:)

  real(8), parameter    :: pi=3.1415926535897932385D0

  real(8) :: w_spectrum_VMiss = -999.000    ! 欠損値初期値

  private

  public w_Initial                            ! 初期化

  public x_Lon, y_Lat                         ! 格子座標
  public x_Lon_weight, y_Lat_Weight           ! 格子座標重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! 変換関数

  public rn, irm                              ! ラプラシアン/経度微分演算用配列

  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public Interpolate_w                        ! 補間関数

  public IntLonLat_xy                         ! 緯度経度積分
  public y_IntLon_xy, IntLon_x                ! 経度積分    
  public x_IntLat_xy, IntLat_y                ! 緯度積分    
  public AvrLonLat_xy                         ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x                ! 経度平均    
  public x_AvrLat_xy, AvrLat_y                ! 緯度平均    

  public nm_EnergyFromStreamfunc_w            ! エネルギースペクトル           
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnergyFromStreamfunc_w             ! エネルギースペクトル
                                              ! (水平全波数 n 空間) 
  public nm_EnstrophyFromStreamfunc_w         ! エンストロフィースペクトル     
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnstrophyFromStreamfunc_w          ! エンストロフィースペクトル  
                                              !  (水平全波数 n 空間)
  public w_spectrum_VMiss                     ! 欠損値

  interface l_nm
     module procedure l_nm_array00
     module procedure l_nm_array01
     module procedure l_nm_array10
     module procedure l_nm_array11
  end interface

  interface nm_l
     module procedure nm_l_int
     module procedure nm_l_array
  end interface

  save im, jm, nm                             ! 格子点数, 切断波数を記憶

contains

  !--------------- 初期化 -----------------
    subroutine w_initial(n_in,i_in,j_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数および OPENMP 使用時の
      ! 最大スレッド数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      ! np_in は w_module との互換性のためにおいているだけである. 
      ! OPENMP 計算は実装されていない. 
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西)
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断波数の設定
      integer,intent(in), optional :: np_in   !(in) OPENMP での最大スレッド数

      integer :: i, j, n

      if ( present (np_in) )then
         call MessageNotify('W','w_initial','OPENMP calculation not supported')
      endif

      if ( i_in /= 1  )then
         call MessageNotify('W','w_initial','Longitudinal dimension should be 1')
      endif

      im = i_in  ; jm = j_in  ; nm = n_in

      allocate(x_Lon(0:im-1))                ! 格子点座標格納配列(経度)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(xy_Lat(0:im-1,1:jm))          ! 格子点座標格納配列

      allocate(rn(0:nm,2),irm(nm+1,2))

      call l_initial(n_in,j_in)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! 経度座標
         x_Lon_Weight(i) = 2*pi/im           ! 経度座標重み
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      do n=0,nm
         rn(n,1) = -n*(n+1)
      enddo

      rn(0,2) = 1.0D0
      do n=1,nm
         rn(n,2) = -1/(n*(n+1))
      enddo

      do n=1,nm+1
         irm(n,1) = n
         irm(n,2) = 0
      enddo

      call MessageNotify('M','w_initial',&
           'w_zonal_module_sjpack (2009/09/22) is initialized')

    end subroutine w_initial

  !--------------- 基本変換 -----------------

    function l_nm_array00(n,m)
      !
      ! 全波数(n)と東西波数(m,ダミー)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 引数 n,m がともに整数値の場合, 整数値を返す. 
      !
      integer               :: l_nm_array00   
      !(out) スペクトルデータの格納位置 

      integer, intent(in)   :: n     !(in) 全波数
      integer, intent(in)   :: m     !(in) 帯状波数           

      if ( m /= 0 ) then
         call MessageNotify('E','l_nm_array00', &
              'zonal wavenumber should be zero in w_zonal_module')
      end if

      l_nm_array00 = n+1

    end function l_nm_array00

    function l_nm_array01(n,marray)           ! スペクトルデータの格納位置 
      !
      ! 全波数(n)と東西波数(m, ダミー)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 n が整数, 第 2 引数 marray が整数 1 次元配列の場合, 
      ! marray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: n               !(in) 全波数
      integer, intent(in)  :: marray(:)       !(in) 帯状波数
      integer              :: l_nm_array01(size(marray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(marray)
         l_nm_array01(i) = l_nm_array00(n,marray(i))
      enddo
    end function l_nm_array01

    function l_nm_array10(narray,m)
      !
      ! 全波数(n)と東西波数(m,ダミー)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 narray が整数 1 次元配列, 第 2 引数  m が整数の場合, 
      ! narray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: narray(:)           !(in) 全波数  
      integer, intent(in)  :: m                   !(in) 帯状波数
      integer              :: l_nm_array10(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(narray)
         l_nm_array10(i) = l_nm_array00(narray(i),m)
      enddo
    end function l_nm_array10

    function l_nm_array11(narray,marray)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1,2 引数 narray, marray がともに整数 1 次元配列の場合, 
      ! narray, marray と同じ大きさの 1 次元整数配列を返す. 
      ! narray, marray は同じ大きさでなければならない. 
      !
      integer, intent(in)  :: narray(:)          !(in) 全波数  
      integer, intent(in)  :: marray(:)          !(in) 帯状波数
      integer              :: l_nm_array11(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      if ( size(narray) .ne. size(marray) ) then
         call MessageNotify('E','l_nm_array11',&
              'dimensions of input arrays  n and m are different.')
      endif

      do i=1, size(narray)
         l_nm_array11(i) = l_nm_array00(narray(i),marray(i))
      enddo
    end function l_nm_array11

    function nm_l_int(l)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 l が整数値の場合, 対応する全波数と帯状波数を
      ! 長さ 2 の 1 次元整数値を返す. 
      ! nm_l(1) が全波数, nm_l(2) が帯状波数である. 
      !
      integer               :: nm_l_int(2)  !(out) 全波数, 帯状波数
      integer, intent(in)   :: l            !(in) スペクトルデータの格納位置
      
      nm_l_int(1) = l-1
      nm_l_int(2) = 0
    end function nm_l_int

    function nm_l_array(larray)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 larray が整数 1 次元配列の場合, 
      ! larray に対応する n, m を格納した 2 次元整数配列を返す. 
      ! nm_l_array(:,1) が全波数, nm_l_array(:,2) が帯状波数である. 
      !
      integer, intent(in)  :: larray(:)
      !(out) 全波数, 帯状波数

      integer              :: nm_l_array(size(larray),2)
      !(in) スペクトルデータの格納位置

      integer              :: i

      do i=1, size(larray)
         nm_l_array(i,:) = nm_l_int(larray(i))
      enddo
    end function nm_l_array

    function xy_w(w_data,ipow,iflag)
      !
      ! スペクトルデータから格子データへ変換する(1 層用).
      !
      real(8)               :: xy_w(0:im-1,1:jm)
      !(out) 格子点データ

      real(8), intent(in)   :: w_data(nm+1)
      !(in) スペクトルデータ

      integer, intent(in), optional  :: ipow      
      !(in) 作用させる 1/cosφ の次数. 省略時は 0. 

      integer, intent(in), optional  :: iflag
      !(in) 変換の種類
      !    0 : 通常の正変換
      !    1 : 緯度微分 cosφ・∂/∂φ を作用させた逆変換
      !   -1 : 経度微分を作用させた逆変換
      !    2 : sinφを作用させた逆変換(非実装)
      !    省略時は 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      if ( ifval == -1 ) then                          ! 経度微分は 0 
         xy_w = 0.0D0        
      else if ( ifval == 1 ) then                    ! 緯度微分
         do i=0,im-1
            xy_w(i,:) = y_GradLat_w(w_data) * cos(y_Lat)
         enddo
      else if ( ifval == 2 ) then                     ! sinφ をかけた逆変換
         do i=0,im-1
            xy_w(i,:) = y_w(w_data)
         enddo
         xy_w = xy_w * sin(xy_Lat)
      else
         do i=0,im-1
            xy_w(i,:) = y_w(w_data)
         enddo
      endif

      if ( ipval /= 0 ) then
         xy_w = xy_w/cos(xy_Lat)**ipval
      end if

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! 格子データからスペクトルデータへ(正)変換する(1 層用).
      !
      real(8)               :: w_xy(nm+1)
      !(out) スペクトルデータ

      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 格子点データ

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !    0 : 通常の正変換
      !    1 : 緯度微分 1/cosφ・∂(f cos^2φ)/∂φ を作用させた正変換
      !   -1 : 緯度微分を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.


      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval

      real(8)             :: xy_work(0:im-1,1:jm) ! 格子点データ作業配列

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      xy_work = xy_data/cos(xy_Lat)**ipval

      if ( ifval == -1 ) then                         ! 経度微分は 0 
         w_xy = 0.0D0        
      else if ( ifval == 1 ) then                     ! 緯度微分
         xy_work = xy_work * cos(xy_Lat)
         w_xy = w_DivLat_y(xy_work(0,:))
      else if ( ifval == 2 ) then                     ! sinφ をかけた正変換
         xy_work = xy_work * sin(xy_Lat)
         w_xy = w_y(xy_work(0,:))
      else
         w_xy = w_y(xy_work(0,:))
      endif
      
    end function w_xy

  !--------------- 微分計算 -----------------
    function w_DLon_w(w_data)
      !
      ! スペクトルデータに経度微分 ∂/∂λ を作用させる(1 層用).
      !
      ! スペクトルデータの経度微分とは, 対応する格子点データに
      ! 経度微分∂/∂λを作用させたデータのスペクトル変換のことである.
      ! 
      real(8)              :: w_DLon_w(nm+1)
      !(out) スペクトルデータの経度微分

      real(8), intent(in)  :: w_data(nm+1)
      !(in) 入力スペクトルデータ

      w_DLon_w = 0.0D0

    end function w_DLon_w

    function xy_GradLon_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を
      ! 作用させた格子点データを返す(1 層用).
      !
      real(8)              :: xy_GradLon_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data(nm+1)
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

      real(8), intent(in)  :: w_data(nm+1)
      !(in) 入力スペクトルデータ

      xy_GradLat_w = xy_w(w_data,ipow=1,iflag=1)

    end function xy_GradLat_w

    function w_DivLon_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/cosφ・∂/∂λ を作用させて
      ! スペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLon_xy(nm+1)
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
      real(8)              :: w_DivLat_xy(nm+1)
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
      real(8)              :: w_Div_xy_xy(nm+1)
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

      real(8)             :: w_Jacobian_w_w(nm+1)
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), intent(in) :: w_a((nm+1)*(nm+1))
      !(in) 1つ目の入力スペクトルデータ
      
      real(8), intent(in) :: w_b((nm+1)*(nm+1))
      !(in) 2つ目の入力スペクトルデータ

      w_Jacobian_w_w = 0.0D0      

    end function w_Jacobian_w_w

  !--------------- 微分計算 (λ,μ座標系用) -----------------
    function xy_GradLambda_w(w_data)
      !
      ! スペクトルデータに勾配型経度微分 ∂/∂λ を作用する(1 層用).
      !
      real(8)              :: xy_GradLambda_w(0:im-1,1:jm)
      !(out) スペクトルデータを勾配型経度微分した格子点データ

      real(8), intent(in)  :: w_data(nm+1)
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

      real(8), intent(in)  :: w_data(nm+1)
      !(in) 入力スペクトルデータ

      xy_GradMu_w = xy_w(w_data,ipow=0,iflag=1)

    end function xy_GradMu_w

    function w_DivLambda_xy(xy_data)
      !
      ! 格子点データに発散型経度微分 1/(1-μ^2)・∂/∂λ (μ=sinφ) 
      ! を作用させてスペクトルデータに変換して返す(1 層用).
      !
      real(8)              :: w_DivLambda_xy(nm+1)
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
      real(8)              :: w_DivMu_xy(nm+1)
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      real(8), intent(in)  :: xy_data(0:im-1,1:jm)
      !(in) 入力格子点データ

      w_DivMu_xy = w_xy(xy_data,ipow=2,iflag=1)

    end function w_DivMu_xy

  !--------------- 補間計算 -----------------
    function Interpolate_w(w_data,alon,alat)
      real(8), intent(IN) :: w_data(nm+1)   ! スペクトルデータ
      real(8), intent(IN) :: alon           ! 補間する位置(経度)
      real(8), intent(IN) :: alat           ! 補間する位置(緯度)
      real(8)             :: Interpolate_w  ! 補間した値
      
      Interpolate_w = Interpolate_alat_w(w_data,alat)

    end function Interpolate_w

  !--------------- 積分計算 -----------------
    function IntLon_x(x_data)
      !
      ! 1 次元経度(X)格子点データの X 方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している.
      !
      real(8), intent(in) :: x_data(0:im-1)   !(in)  1 次元経度(X)格子点データ
      real(8)             :: IntLon_x         !(out) 積分値

      IntLon_x = sum(x_data * x_Lon_weight)

    end function IntLon_x

    function x_IntLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)           
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8)             :: x_IntLat_xy(0:im-1) 
      !(out) 積分された 1 次元経度(X)格子点データ

      integer :: j

      x_IntLat_xy = 0.0D0
      do j=1,jm
         x_IntLat_xy = x_IntLat_xy + xy_data(:,j) * y_Lat_weight(j)
      enddo

    end function x_IntLat_xy

    function y_IntLon_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向積分(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8)             :: y_IntLon_xy(1:jm)
      !(out) 積分された 1 次元緯度(Y)格子点データ

      integer :: i

      y_IntLon_xy = 0.0D0
      do i=0,im-1
         y_IntLon_xy = y_IntLon_xy + xy_data(i,:) * x_Lon_weight(i)
      enddo

    end function y_IntLon_xy

    function IntLonLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(1 層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)         
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)

      real(8) :: IntLonLat_xy                         
      !(out) 積分値

      IntLonLat_xy = IntLon_x(x_IntLat_xy(xy_data))

    end function IntLonLat_xy

  !--------------- 平均計算 -----------------
    function AvrLon_x(x_data)
      !
      ! 1 次元(X)格子点データの経度(X)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: x_data(0:im-1)
      !(in)  1 次元経度(X)格子点データ
      real(8)             :: AvrLon_x       
      !(out) 平均値

      AvrLon_x = IntLon_x(x_data)/sum(x_Lon_weight)

    end function AvrLon_x


    function x_AvrLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)
      real(8)             :: x_AvrLat_xy(im)
      !(out) 平均された 1 次元経度(X)格子点データ

      x_AvrLat_xy = x_IntLat_xy(xy_data)/sum(y_Lat_weight)

    end function x_AvrLat_xy

    function y_AvrLon_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ(0:im-1,1:jm)
      real(8)             :: y_AvrLon_xy(1:jm)
      !(out) 平均された 1 次元緯度(Y)格子点

      y_AvrLon_xy = y_IntLon_xy(xy_data)/sum(x_Lon_weight)

    end function y_AvrLon_xy


    function AvrLonLat_xy(xy_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(1 層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 2 次元経度緯度格子点データ

      real(8) :: AvrLonLat_xy
      !(out) 平均値

      AvrLonLat_xy = AvrLon_x(x_AvrLat_xy(xy_data))

    end function AvrLonLat_xy

  !--------------- エネルギースペクトル計算 -----------------
    function nm_EnergyFromStreamfunc_w(w_Strfunc)
      ! 
      ! 流線関数のスペクトルデータからエネルギーの球面調和函数成分
      ! (スペクトル)を計算する(1 層用).
      !
      !  * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルは (1/2)n(n+1)ψ(n,m)^2 と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に4πをかけたものが球面上での
      !    全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 w_spectrum_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnergyFromStreamfunc_w
      !(out) エネルギースペクトル(水平全波数 n, 帯状波数 m 空間)

      integer :: n, m

      nm_EnergyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nm_EnergyFromStreamfunc_w(n,m)  = 0.0D0
            nm_EnergyFromStreamfunc_w(n,-m) = 0.0D0
         enddo
         nm_EnergyFromStreamfunc_w(n,0) &
              = 0.5 * n*(n+1) * w_Strfunc(l_nm(n,0))**2
      enddo
    end function nm_EnergyFromStreamfunc_w

    function n_EnergyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用).
      !
      !  * 全波数 n の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルはΣ[m=-nm]^nm(1/2)n(n+1)ψ(n,m)^2 
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に 4πをかけたものが
      !    球面上での全エネルギーに等しい.
      !

      real(8), intent(in)      :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm) :: n_EnergyFromStreamfunc_w
      !(out) エネルギースペクトル (水平全波数 n 空間) 

      integer :: n

      do n=0,nm
         n_EnergyFromStreamfunc_w(n)  &
              = 0.5 * n*(n+1) * w_StrFunc(l_nm(n,0))**2
      enddo

    end function n_EnergyFromStreamfunc_w

  !--------------- エンストロフィースペクトル計算 -----------------
    function nm_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータからエンストロフィーの球面調和函数成分
      ! (スペクトル)を計算する(1 層用). 
      !
      ! * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エンストロフィースペクトルは (1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !
      ! * 全てのエンストロフィースペクトル成分の和に4π/R^2をかけたものが
      !   球面上での全エンストロフィーに等しい. ここで R は球面の半径である.
      !
      ! * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !   欠損値の値はモジュール変数 w_spectrum_VMiss によって設定できる
      !   (初期値は -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnstrophyFromStreamfunc_w
      ! エンストロフィースペクトル (水平全波数 n, 帯状波数 m 空間)

      integer :: n, m

      nm_EnstrophyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nm_EnstrophyFromStreamfunc_w(n,m) = 0.0
            nm_EnstrophyFromStreamfunc_w(n,-m) = 0.0
         enddo
         nm_EnstrophyFromStreamfunc_w(n,0) &
                 = 0.5 * n**2 * (n+1)**2 * w_Strfunc(l_nm(n,0))**2
      enddo

    end function nm_EnstrophyFromStreamfunc_w

    function n_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用)
      !
      ! * 全波数 n の流線関数のスペクトル成分ψ(n,m) からエンストロフィー
      !   スペクトルはΣ[m=-nm]^nm(1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !    
      ! * 全てのエネルギースペクトル成分の和に 4π/R^2 をかけたものが
      !   球面上での全エンストフィーに等しい.
      !
      real(8), intent(in)      :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm) :: n_EnstrophyFromStreamfunc_w  
      !(out) エンストロフィースペクトル(水平全波数 n 空間)

      integer :: n

      do n=0,nm
         n_EnstrophyFromStreamfunc_w(n)  &
              = 0.5 * n**2 * (n+1)**2 * w_StrFunc(l_nm(n,0))**2
      enddo
    end function n_EnstrophyFromStreamfunc_w

end module w_zonal_module_sjpack
