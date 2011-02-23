!--
!----------------------------------------------------------------------
!     Copyright (c) 2008--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_zonal_module
!
!   spml/wa_zonal_module モジュールは球面上での経度方向に一様な帯状的 
!   1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
!   数値計算するための Fortran90 関数を提供する. 
!
!   球面上の 1 層モデル用 w_zonal_module モジュールを多層モデル用に
!   拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!   対する変換が行える.
!
!   内部で l_module/la_module を用いている. 最下部ではルジャンドル
!   多項式変換のエンジンとして ISPACK の Fortran77 サブルーチンを
!   用いている.
!
!   wa_zonal_modulde で提供される関数・サブルーチンは 2 次元的流体運動を扱う 
!   wa_module モジュールで用いられているものと名前およびインターフェースが
!   共通になるように設計してある. したがって, wa_module を用いて構成された
!   2 次元モデルを帯状方向に一様な 1 次元軸対称モデルへと改造するには
!   次の手順が必要となる. 
!
!   * use 文での wa_module の引用を wa_zonal_module に変更する. 
!   * 配列の大きさを経度方向格子点数 im -> 1 に, 
!     水平波数を (nm+1)**2 -> nm+1 に変更する. 
!   * DO 文で水平波数に関してループを回しているところを
!     (nm+1)**2 -> nm+1 に変更する. 
!   * gtool 出力の次元変数変更する. 
!
!
!履歴  2008/12/29  竹広真一  wa_module をもとに軸対称多層用に改造
!      2009/01/08  竹広真一  wa_Initial メッセージに日付を追加
!
!++
module wa_zonal_module
  !
  != wa_zonal_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_zonal_module.f90,v 1.6 2009-02-28 21:33:48 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !  spml/wa_zonal_module モジュールは球面上での経度方向に一様な帯状的 
  !  1 次元流体運動をルジャンドル多項式を用いたスペクトル法によって
  !  数値計算するための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_zonal_module モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で l_module/la_module を用いている. 最下部ではルジャンドル
  !  多項式変換のエンジンとして ISPACK の Fortran77 サブルーチンを
  !  用いている.
  !
  !  wa_zonal_modulde で提供される関数・サブルーチンは 2 次元的流体運動を扱う 
  !  wa_module モジュールで用いられているものと名前およびインターフェースが
  !  共通になるように設計してある. したがって, wa_module を用いて構成された
  !  2 次元モデルを帯状方向に一様な 1 次元軸対称モデルへと改造するには
  !  次の手順が必要となる. 
  !
  !  1. use 文での wa_module の引用を wa_zonal_module に変更する. 
  !  2. 配列の大きさを経度方向格子点数 im -> 1 に, 
  !     水平波数を (nm+1)**2 -> nm+1 に変更する. 
  !  3. DO 文で水平波数に関してループを回しているところを
  !     (nm+1)**2 -> nm+1 に変更する. 
  !  4. gtool 出力の次元変数変更する. 
  !
  !== 関数・変数の名前と型について
  !
  ! w_zonal_module でのものに追加される関数・変数の名前と型についてのみ
  ! 説明する. w_zonal_module での関数・変数の名前と型については 
  ! w_zonal_module の該当項を参照されたい.
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (wa_, nma_, na_, xya_, xa_, ya_, w_, xy_, x_, y_, a_) は, 
  !   返す値の形を示している.
  !   wa_  :: 水平スペクトルデータの並んだ 2 次元配列(スペクトルデータの
  !           並びは LTPACK/ISPACK に従ったもの)
  !   nma_ :: スペクトルデータの並んだ 3 次元配列 (スペクトルデータの並びは
  !           全波数 n, 帯状波数 m で指定される 2 次元配列)
  !   na_  :: スペクトルデータの並んだ 2 次元配列 (スペクトルデータの並びは
  !           全波数 n で指定される 1 次元配列)
  !   xya_ :: 2 次元格子点データの並んだ 3 次元配列
  !   xa_  :: 経度方向 1 次元格子点データの並んだ 2 次元配列
  !   ya_  :: 緯度方向 1 次元格子点データの並んだ 2 次元配列
  !
  ! * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !   LaplaInv, Jacobian)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_wa_wa, _wa, _xya, _xa, _ya, _w_w, _w, _xy, _x, _y) は, 
  !   入力変数の形スペクトルデータおよび格子点データであることを示している.
  !   _wa    :: スペクトルデータの並んだ 2 次元配列
  !   _wa_wa :: 2 つのスペクトルデータの並んだ 2 次元配列
  !   _xya   :: 2 次元格子点データの並んだ 3 次元配列
  !   _xa    :: 経度方向 1 次元格子点データの並んだ 2 次元配列
  !   _ya    :: 緯度方向 1 次元格子点データの並んだ 2 次元配列
  !
  !=== 各データの種類の説明
  !
  ! * xya : 2 次元格子点データの並んだ 3 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:im-1,1:jm,:). 
  !   * im, jm はそれぞれ経度, 緯度座標の格子点数であり, サブルーチン
  !     wa_Initial にてあらかじめ設定しておく. 
  !   * 扱う第 3 次元の大きさの最大値を wa_Initial で設定しておく.
  !
  ! * wa : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(nm+1,:). 
  !   * nm は球面調和函数の最大全波数であり, サブルーチン wa_Initial にて
  !     あらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方は関数 l_nm, nm_l によって
  !     調べることができる. 
  !   * 扱う第 3 次元の大きさの
  !     最大値を wa_Initial で設定しておく.
  !
  ! * xa, ya : 経度, 緯度方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1,:) 
  !     および real(8), dimension(1:jm,:).
  !
  ! * nma : スペクトルデータの並んだ 3 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,-nm:nm,:). 
  !   * 第 1 次元が水平全波数, 第 2 次元が帯状波数を表す. 
  !   * nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wa_Initial にてあらかじめ設定しておく.
  !
  ! * na : スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(0:nm,:). 
  !   * 第 1 次元が水平全波数を表す.nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wa_Initial にてあらかじめ設定しておく.
  !
  ! * wa_ で始まる関数が返す値はスペクトルデータの並んだ 2 次元配列に同じ.
  !
  ! * nma_ で始まる関数が返す値はスペクトルデータの並んだ 3 次元配列に同じ.
  !
  ! * na_ で始まる関数が返す値はスペクトルデータの並んだ 2 次元配列に同じ.
  !
  ! * xya_ で始まる関数が返す値は 2 次元格子点データの並んだ 3 次元配列に
  !   同じ.
  !
  ! * xa_, ya_ で始まる関数が返す値は 1 次元格子点データの並んだ 2 次元
  !   配列に同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! wa_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさ, 
  !            :: および同時に計算するデータの個数の最大値設定
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
  ! xya_wa, xy_w :: スペクトルデータから格子データへの変換(多層, 1 層用)
  ! wa_xya, w_xy :: 格子データからスペクトルデータへの変換(多層, 1 層用)
  ! l_nm, nm_l :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wa_Lapla_wa, w_Lapla_w        :: スペクトルデータにラプラシアンを
  !                                  作用させる(多層, 1 層用)
  ! rn                            :: スペクトルデータのラプラシアンを
  !                                  計算するための係数
  ! irm                           :: 経度微分演算用配列
  ! wa_LaplaInv_wa, w_LaplaInv_w  :: スペクトルデータにラプラシアンの逆変換を
  !                                  作用させる(多層, 1 層用)
  ! wa_DLon_wa, w_DLon_w          :: スペクトルデータに
  !                                  経度微分∂/∂λを作用させる(多層, 1 層用)
  ! xya_GradLon_wa, xy_GradLon_w  :: スペクトルデータに
  !                                  勾配型経度微分 1/cosφ・∂/∂λ を
  !                                  作用させる(多層, 1 層用)
  ! xya_GradLat_wa, xy_GradLat_w  :: スペクトルデータに
  !                                  勾配型緯度微分 ∂/∂φ を作用させる
  !                                  (多層, 1 層用)
  ! wa_DivLon_xya, w_DivLon_xy    :: 格子データに
  !                                  発散型経度微分 1/cosφ・∂/∂λを
  !                                  作用させる(多層, 1 層用)
  ! wa_DivLat_xya, w_DivLat_xy    :: 格子データに
  !                                  発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを
  !                                  作用させる (多層, 1 層用)
  ! wa_Div_xya_xya, w_Div_xy_xy   :: ベクトル成分である 2 つの格子データに
  !                                  発散を作用させる(多層, 1 層用)
  ! wa_Jacobian_wa_wa,w_Jacobian_w_w :: 2 つのスペクトルデータからヤコビアンを
  !                                     計算する(多層, 1 層用)
  !
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! xya_GradLambda_wa,xy_GradLambda_w  :: スペクトルデータに
  !                                       勾配型経度微分∂/∂λを
  !                                       作用させる(多層, 1 層用)
  !
  ! xya_GradMu_wa,xy_GradMu_w          :: スペクトルデータに勾配型緯度微分
  !                                       (1-μ^2)∂/∂μを作用させる
  !                                       (多層, 1 層用)
  !
  ! wa_DivLambda_xya,w_DivLambda_xy    :: 格子データに発散型経度微分
  !                                       1 /(1-μ^2)・∂/∂λを作用させる
  !                                       (多層, 1 層用)
  !
  ! wa_DivMu_xya,w_DivMu_xy            :: 格子データに発散型緯度微分
  !                                       ∂/∂μを作用させる(多層, 1 層用)
  !
  !==== 補間計算
  !
  ! a_Interpolate_wa, Interpolate_w :: スペクトルデータから任意の点の
  !                                    関数値を計算する.
  !
  !==== 積分・平均
  !
  ! a_IntLonLat_xya, a_AvrLonLat_xya :: 2 次元格子点データの
  !                                     全領域積分および平均(多層用)
  ! IntLonLat_xy, AvrLonLat_xy       :: 2 次元格子点データの
  !                                     全領域積分および平均(1 層用)
  ! ya_IntLon_xya, ya_AvrLon_xya     :: 2 次元格子点データの
  !                                     経度方向積分および平均(多層用)
  ! y_IntLon_xy, y_AvrLon_xy         :: 2 次元格子点データの
  !                                     経度方向積分および平均(1 層用)
  ! a_IntLon_xa, a_AvrLon_xa         :: 1 次元(X)格子点データの
  !                                     経度方向積分および平均(多層用)
  ! IntLon_x, AvrLon_x               :: 1 次元(X)格子点データの
  !                                     経度方向積分および平均(1 層用)
  ! xa_IntLat_xya, x_AvrLat_xy       :: 2 次元格子点データの
  !                                     緯度方向積分および平均(多層用)
  ! x_IntLat_xy, x_AvrLat_xy         :: 2 次元格子点データの
  !                                     緯度方向積分および平均(1 層用)
  ! a_IntLat_ya, a_AvrLat_ya         :: 1 次元(Y)格子点データの
  !                                     緯度方向積分および平均(多層用)
  ! IntLat_y, AvrLat_y               :: 1 次元(Y)格子点データの
  !                                     緯度方向積分および平均(1 層用)
  !
  !==== スペクトル解析
  !
  ! nma_EnergyFromStreamfunc_wa,nm_EnergyFromStreamfunc_w   :: 
  ! 流線関数からエネルギースペクトルを計算する
  ! (水平全波数 n, 帯状波数 m 空間)(多層, 1 層用)
  ! 
  ! na_EnergyFromStreamfunc_wa,n_EnergyFromStreamfunc_w     ::
  ! 流線関数からエネルギースペクトルを計算する
  ! (水平全波数 n 空間) (多層, 1 層用)
  ! 
  ! nma_EnstrophyFromStreamfunc_wa,nm_EnstrophyFromStreamfunc_w   ::
  ! 流線関数からエンストロフィースペクトルを計算する
  ! (水平全波数 n, 帯状波数 m 空間)(多層, 1 層用)
  ! 
  ! na_EnstrophyFromStreamfunc_wa,n_EnstrophyFromStreamfunc_w :: 
  ! 流線関数からエンストロフィースペクトルを計算する(水平全波数 n 空間)
  ! (多層, 1 層用)
  ! 
  ! w_spectrum_VMiss              ::  欠損値
  !
  use dc_message, only : MessageNotify
  use w_zonal_module
  use la_module, wa_ya => la_ya, ya_wa => ya_la, &
       ya_GradLat_wa => ya_GradLat_la, wa_DivLat_ya => la_DivLat_ya, &
       wa_Lapla_wa => la_Lapla_la, wa_LaplaInv_wa => la_LaplaInv_la, &
       a_Interpolate_alat_wa => a_Interpolate_la
  
  implicit none

  integer               :: im=64            ! 格子点の設定(東西)
  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: km=16            ! 層の数の最大値の設定
  integer               :: nm=21            ! 切断波数の設定
  integer               :: np=1             ! OPENMP 最大スレッド数

  real(8) :: wa_spectrum_VMiss = -999.000   ! 欠損値初期値

  private

  public wa_Initial                           ! 初期化

  public x_Lon, y_Lat                         ! 格子座標
  public x_Lon_Weight, y_Lat_Weight           ! 格子座標重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public xy_w, w_xy, l_nm, nm_l               ! 変換関数
  public xya_wa, wa_xya                       ! 変換関数
  
  public rn, irm                              ! ラプラシアンの係数, 経度微分の配列
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xy_GradLon_w, xy_GradLat_w           ! 勾配型微分
  public w_DivLon_xy, w_DivLat_xy             ! 発散型微分
  public w_Div_xy_xy                          ! 発散型微分
  public w_Jacobian_w_w                       ! ヤコビアン
  public xy_GradLambda_w, xy_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xy, w_DivMu_xy           ! 発散型微分(λ,μ座標)

  public wa_Lapla_wa, wa_LaplaInv_wa          ! ラプラシアンと逆演算
  public wa_DLon_wa                           ! 経度微分
  public xya_GradLon_wa, xya_GradLat_wa       ! 勾配型微分
  public wa_DivLon_xya, wa_DivLat_xya         ! 発散型微分
  public wa_Div_xya_xya                       ! 発散型微分
  public wa_Jacobian_wa_wa                    ! ヤコビアン
  public xya_GradLambda_wa, xya_GradMu_wa     ! 勾配型微分(λ,μ座標)
  public wa_DivLambda_xya, wa_DivMu_xya       ! 発散型微分(λ,μ座標)

  public a_Interpolate_wa, Interpolate_w      ! 補間関数

  public IntLonLat_xy                         ! 緯度経度積分
  public y_IntLon_xy, IntLon_x                ! 経度積分    
  public x_IntLat_xy, IntLat_y                ! 緯度積分    
  public AvrLonLat_xy                         ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x                ! 経度平均    
  public x_AvrLat_xy, AvrLat_y                ! 緯度平均    

  public a_IntLonLat_xya                      ! 緯度経度積分
  public ya_IntLon_xya, a_IntLon_xa           ! 経度積分    
  public xa_IntLat_xya, a_IntLat_ya           ! 緯度積分    
  public a_AvrLonLat_xya                      ! 緯度経度平均
  public ya_AvrLon_xya, a_AvrLon_xa           ! 経度平均    
  public xa_AvrLat_xya, a_AvrLat_ya           ! 緯度平均    

  public nma_EnergyFromStreamfunc_wa          ! エネルギースペクトル
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public na_EnergyFromStreamfunc_wa           ! エネルギースペクトル
                                              ! (水平全波数 n 空間)
  public nma_EnstrophyFromStreamfunc_wa       ! エンストロフィースペクトル
                                              ! (水平全波数 n, 帯状波数 m 空間)
  public na_EnstrophyFromStreamfunc_wa        ! エンストロフィースペクトル
                                              !  (水平全波数 n 空間)
  public wa_spectrum_VMiss                    ! 欠損値

  public nm_EnergyFromStreamfunc_w          ! エネルギースペクトル           
                                            ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnergyFromStreamfunc_w           ! エネルギースペクトル
                                            ! (水平全波数 n 空間) 
  public nm_EnstrophyFromStreamfunc_w       ! エンストロフィースペクトル     
                                            ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnstrophyFromStreamfunc_w        ! エンストロフィースペクトル  
                                            !  (水平全波数 n 空間)
  public w_spectrum_VMiss                   ! 欠損値

  save im, jm, km, nm                       ! 格子点数, 切断波数を記憶

contains
  !--------------- 初期化 -----------------
    subroutine wa_Initial(n_in,i_in,j_in,k_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数, 最大データ数(層数)および 
      ! OPENMP 使用時の最大スレッド数を設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
      ! しなければならない. 
      !
      ! 最大データ層数は wa_module との互換性のためにおいている. 
      ! 実際にはこの設定以上の層数のデータも正しく取り扱える. 
      !
      ! np_in は wa_module との互換性のためにおいているだけである. 
      ! OPENMP 計算は実装されていない. 
      !
      integer,intent(in) :: i_in                ! 格子点数(東西)
      integer,intent(in) :: j_in                ! 格子点数(南北)
      integer,intent(in) :: n_in                ! 切断波数
      integer,intent(in) :: k_in                ! 最大データ数(層数)
      integer,intent(in), optional :: np_in     ! OPENMP での最大スレッド数

      call w_Initial(n_in,i_in,j_in)
      call la_Initial(n_in,j_in,l_init=.false.)

      nm = n_in ; im = i_in ; jm = j_in ; km = k_in

      call MessageNotify('M','wa_initial',&
           'wa_zonal_module (2009/01/08) is initialized')

    end subroutine wa_Initial

  !--------------- 基本変換 -----------------

    function xya_wa(wa_data,ipow,iflag)
      !
      ! スペクトルデータから格子データへ変換する(多層用).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) スペクトルデータ(nm+1,:)
      !
      real(8)               :: xya_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) 格子点データ(0:im-1,1:jm,:)

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

      if ( ifval == -1 ) then                        ! 経度微分は 0 
         xya_wa = 0.0D0        
      else if ( ifval == 1 ) then                    ! 緯度微分
         do i=0,im-1
            xya_wa(i,:,:) = ya_GradLat_wa(wa_data) &
                 * spread(cos(y_Lat),2,size(wa_data,2))
         enddo
      else if ( ifval == 2 ) then                     ! sinφ をかけた逆変換
         do i=0,im-1
            xya_wa(i,:,:) = ya_wa(wa_data)
         enddo
         xya_wa = xya_wa * spread(sin(xy_Lat),3,size(wa_data,2))
      else
         do i=0,im-1
            xya_wa(i,:,:) = ya_wa(wa_data)
         enddo
      endif

      if ( ipval /= 0 ) then
         xya_wa = xya_wa/spread(cos(xy_Lat)**ipval,3,size(wa_data,2))
      end if

    end function xya_wa

    function wa_xya(xya_data,ipow,iflag) ! 格子点 -> 球面調和関数スペクトル
      !
      ! 格子データからスペクトルデータへ(正)変換する(多層用).
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 格子点データ(0:im-1,1:jm,:)

      real(8)               :: wa_xya(nm+1,size(xya_data,3))
      !(out) スペクトルデータ(nm+1,:)

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた正変換
      !    1 : 緯度微分を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.

      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval

      real(8)             :: xya_work(0:im-1,1:jm,size(xya_data,3))
      ! 格子点データ作業配列

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

      xya_work = xya_data/spread(cos(xy_Lat)**ipval,3,size(xya_data,3))

      if ( ifval == -1 ) then                         ! 経度微分は 0 
         wa_xya = 0.0D0        
      else if ( ifval == 1 ) then                     ! 緯度微分
         xya_work = xya_work * spread(cos(xy_Lat),3,size(xya_data,3))
         wa_xya = wa_DivLat_ya(xya_work(0,:,:))
      else if ( ifval == 2 ) then                     ! sinφ をかけた正変換
         xya_work = xya_work * spread(sin(xy_Lat),3,size(xya_data,3))
         wa_xya = wa_ya(xya_work(0,:,:))
      else
         wa_xya = wa_ya(xya_work(0,:,:))
      endif
      
    end function wa_xya

  !--------------- 微分計算 -----------------
    function wa_DLon_wa(wa_data)
      !
      ! スペクトルデータに経度微分 ∂/∂λ を作用させる(多層用).
      !
      ! スペクトルデータの経度微分とは, 対応する格子点データに
      ! 経度微分∂/∂λを作用させたデータのスペクトル変換のことである.
      ! 
      real(8), intent(in)  :: wa_data(:,:)
      !(in) 入力スペクトルデータ
      real(8)              :: wa_DLon_wa(nm+1,size(wa_data,2))
      !(out) スペクトルデータの経度微分

      wa_DLon_wa = 0.0D0

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
      real(8)              :: wa_DivLon_xya(nm+1,size(xya_data,3))
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
      real(8)              :: wa_DivLat_xya(nm+1,size(xya_data,3))
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
      real(8)              :: wa_Div_xya_xya(nm+1,size(xya_u,3))
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
      real(8)             :: wa_Jacobian_wa_wa(nm+1,size(wa_a,2))
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
      real(8)              :: wa_DivLambda_xya(nm+1,size(xya_data,3))
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
      real(8)              :: wa_DivMu_xya(nm+1,size(xya_data,3))
      !(out) 格子点データを発散型緯度微分したスペクトルデータ

      wa_DivMu_xya = wa_xya(xya_data,ipow=2,iflag=1)

    end function wa_DivMu_xya

  !--------------- 補間計算 -----------------
    function a_Interpolate_wa(wa_data,alon,alat)
      !
      ! 緯度 alon, 経度 alat における関数値を
      ! その球面調和変換係数 wa_data から補間計算する
      !
      real(8), intent(IN) :: wa_data(:,:)             ! スペクトルデータ
      real(8), intent(IN) :: alon                     ! 補間する位置(経度)
      real(8), intent(IN) :: alat                     ! 補間する位置(緯度)
      real(8) :: a_Interpolate_wa(size(wa_data,2))    ! 補間した値

      integer :: k

      do k=1,size(wa_data,2)
         a_Interpolate_wa(k) = Interpolate_w(wa_data(:,k),alon,alat)
      enddo

    end function a_Interpolate_wa
      
  !--------------- 積分計算 -----------------
    function a_IntLonLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの全領域積分(多層用). 
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)
      real(8) :: a_IntLonLat_xya(size(xya_data,3))
      !(out) 積分されたデータの並び(*)

      a_IntLonLat_xya = a_IntLon_xa(xa_IntLat_xya(xya_data))

    end function a_IntLonLat_xya

    function xa_IntLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)
      real(8)             :: xa_IntLat_xya(0:im-1,size(xya_data,3))
      !(out) 積分された 1 次元経度(X)格子点データの並び
      Integer :: j

      xa_IntLat_xya = 0.0D0
      do j=1,jm
         xa_IntLat_xya = xa_IntLat_xya + xya_data(:,j,:) * y_Lat_Weight(j)
      enddo

    end function xa_IntLat_xya

    function ya_IntLon_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: ya_IntLon_xya(1:jm,size(xya_data,3))
      !(out) 積分された 1 次元緯度(Y)格子点データの並び

      integer :: i

      ya_IntLon_xya = 0.0D0
      do i=0,im-1
         ya_IntLon_xya = ya_IntLon_xya + xya_data(i,:,:) * x_Lon_Weight(i)
      enddo

    end function ya_IntLon_xya

    function a_IntLon_xa(xa_data)          ! 経度積分
      !
      ! 1 次元経度(X)格子点データの X 方向積分(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 次元経度(X)格子点データの並び(0:im-1,*)
      real(8)             :: a_IntLon_xa(size(xa_data,2))
      !(out) 積分値の並び(*)
      integer :: i

      a_IntLon_xa = 0.0D0
      do i=0,im-1
         a_IntLon_xa = a_IntLon_xa + xa_data(i,:) * x_Lon_Weight(i)
      enddo

    end function a_IntLon_xa

  !--------------- 平均計算 -----------------
    function a_AvrLonLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの全領域平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8) :: a_AvrLonLat_xya(size(xya_data,3))
      !(out) 平均値の並び(*)

      a_AvrLonLat_xya = a_AvrLon_xa(xa_AvrLat_xya(xya_data))

    end function a_AvrLonLat_xya

    function xa_AvrLat_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの緯度(Y)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: xa_AvrLat_xya(0:im-1,size(xya_data,3))
      !(out) 平均された 1 次元経度(X)格子点データの並び(im,*)

      xa_AvrLat_xya = xa_IntLat_xya(xya_data)/sum(y_Lat_Weight)

    end function xa_AvrLat_xya

    function ya_AvrLon_xya(xya_data)
      !
      ! 2 次元緯度経度格子点データの経度(X)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xya_data(0:,:,:)
      !(in) 2 次元経度緯度格子点データの並び(0:im-1,1:jm,*)

      real(8)             :: ya_AvrLon_xya(1:jm,size(xya_data,3))
      !(out) 平均された 1 次元緯度(Y)格子点の並び(1:jm,*)

      ya_AvrLon_xya = ya_IntLon_xya(xya_data)/sum(x_Lon_Weight)

    end function ya_AvrLon_xya

    function a_AvrLon_xa(xa_data)          ! 経度平均
      !
      ! 1 次元(X)格子点データの経度(X)方向平均(多層用).
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), intent(in) :: xa_data(0:,:)
      !(in)  1 次元経度(X)格子点データの並び(0:im-1,*)

      real(8)             :: a_AvrLon_xa(size(xa_data,2))
      !(out) 平均値の並び(*)

      a_AvrLon_xa = a_IntLon_xa(xa_data)/sum(x_Lon_Weight)

    end function a_AvrLon_xa

  !--------------- エネルギースペクトル計算 -----------------
    function nma_EnergyFromStreamfunc_wa(wa_Strfunc)
      ! 
      ! 流線関数のスペクトルデータからエネルギーの球面調和函数成分
      ! (スペクトル)を計算する(多層用).
      !
      !  * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルは (1/2)n(n+1)ψ(n,m)^2 と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に4πをかけたものが球面上での
      !    全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 wa_spectrum_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnergyFromStreamfunc_wa
      !(out) エネルギースペクトル(水平全波数 n, 帯状波数 m 空間)

      integer :: n, m

      nma_EnergyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nma_EnergyFromStreamfunc_wa(n,m,:)  = 0.0D0
            nma_EnergyFromStreamfunc_wa(n,-m,:) = 0.0D0
         enddo
         nma_EnergyFromStreamfunc_wa(n,0,:) &
              = 0.5 * n*(n+1) * wa_Strfunc(l_nm(n,0),:)**2
      enddo

    end function nma_EnergyFromStreamfunc_wa

    function na_EnergyFromStreamfunc_wa(wa_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(多層用).
      !
      !  * 全波数 n の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルはΣ[m=-nm]^nm(1/2)n(n+1)ψ(n,m)^2 
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に 4πをかけたものが
      !    球面上での全エネルギーに等しい.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnergyFromStreamfunc_wa
      !(out) エネルギースペクトル (水平全波数 n 空間) 

      integer :: n

      do n=0,nm
         na_EnergyFromStreamfunc_wa(n,:)  &
              = 0.5 * n*(n+1) * wa_StrFunc(l_nm(n,0),:)**2
      enddo

    end function na_EnergyFromStreamfunc_wa

  !--------------- エンストロフィースペクトル計算 -----------------

    function nma_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! 流線関数のスペクトルデータからエンストロフィーの球面調和函数成分
      ! (スペクトル)を計算する(多層用). 
      !
      ! * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エンストロフィースペクトルは (1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !
      ! * 全てのエンストロフィースペクトル成分の和に4π/R^2をかけたものが
      !   球面上での全エンストロフィーに等しい. ここで R は球面の半径である.
      !
      ! * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !   欠損値の値はモジュール変数 wa_spectrum_VMiss によって設定できる
      !   (初期値は -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnstrophyFromStreamfunc_wa
      ! エンストロフィースペクトル (水平全波数 n, 帯状波数 m 空間)

      integer :: n, m

      nma_EnstrophyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         do m=1,n
            nma_EnstrophyFromStreamfunc_wa(n,m,:) = 0.0
            nma_EnstrophyFromStreamfunc_wa(n,-m,:) = 0.0
         enddo
         nma_EnstrophyFromStreamfunc_wa(n,0,:) &
                 = 0.5 * n**2 * (n+1)**2 * wa_Strfunc(l_nm(n,0),:)**2
      enddo

    end function nma_EnstrophyFromStreamfunc_wa

    function na_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(多層用)
      !
      ! * 全波数 n の流線関数のスペクトル成分ψ(n,m) からエンストロフィー
      !   スペクトルはΣ[m=-nm]^nm(1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !    
      ! * 全てのエネルギースペクトル成分の和に 4π/R^2 をかけたものが
      !   球面上での全エンストフィーに等しい.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnstrophyFromStreamfunc_wa      ! エンストロフィースペクトル
      !(out) エンストロフィースペクトル(水平全波数 n 空間)

      integer :: n

      do n=0,nm
         na_EnstrophyFromStreamfunc_wa(n,:)  &
              = 0.5 * n**2 * (n+1)**2 * wa_StrFunc(l_nm(n,0),:)**2
      enddo

    end function na_EnstrophyFromStreamfunc_wa

end module wa_zonal_module
