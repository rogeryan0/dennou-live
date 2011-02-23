!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_mpi_module
!
!   spml/wa_mpi_module モジュールは球面上での 2 次元流体運動を
!   球面調和函数を用いたスペクトル法と MPI 並列化によって
!   数値計算するための Fortran90 関数を提供する. 
!
!   球面上の 1 層モデル用 w_module モジュールを多層モデル用に
!   拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!   対する変換が行える.
!
!   wa_mpi_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
!   をそれぞれ担っている下部モジュール wa_base_mpi_module, wa_deriv_mpi_module, 
!   wa_integral_module, wa_spectrum_module および 1 層用のモジュール
!   w_mpi_module からなっている.
!
!   内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
!   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!   ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2008/05/28  竹広真一  wa_module を MPI 化
!
module wa_mpi_module
  !
  != wa_mpi_module
  !
  !== 概要
  !
  !   spml/wa_mpi_module モジュールは球面上での 2 次元流体運動を
  !   球面調和函数を用いたスペクトル法と MPI 並列化によって
  !   数値計算するための Fortran90 関数を提供する. 
  !
  !   球面上の 1 層モデル用 w_module モジュールを多層モデル用に
  !   拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !   対する変換が行える.
  !
  !   wa_mpi_module は実際には基本変換, 微分計算, 積分・平均計算, スペクトル解析
  !   をそれぞれ担っている下部モジュール wa_base_mpi_module, wa_deriv_mpi_module, 
  !   wa_integral_module, wa_spectrum_module および 1 層用のモジュール
  !   w_mpi_module からなっている.
  !
  !   内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる.
  !   スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !   ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  !
  !== 関数・変数の名前と型について
  !
  !   w_mpi_module でのものに追加される関数・変数の名前と型についてのみ説明する. 
  !   w_mpi_module での関数・変数の名前と型については w_mpi_module の該当項を参照
  !   されたい.
  !
  !=== 命名法
  !
  !   * 関数名の先頭 (wa_, nma_, na_, xya_, xa_, ya_, w_, xy_, x_, y_, a_) は, 
  !     返す値の形を示している.
  !        wa_ : 水平スペクトルデータの並んだ 2 次元配列(スペクトルデータの
  !              並びは SNPACK/ISPACK に従ったもの)
  !       nma_ : スペクトルデータの並んだ 3 次元配列 (スペクトルデータの並びは
  !              全波数 n, 帯状波数 m で指定される 2 次元配列)
  !        na_ : スペクトルデータの並んだ 2 次元配列 (スペクトルデータの並びは
  !              全波数 n で指定される 1 次元配列)
  !       xya_ : 2 次元格子点データの並んだ 3 次元配列
  !       xva_ : 2 次元分散格子点データの並んだ 3 次元配列
  !        xa_ : 経度方向 1 次元格子点データの並んだ 2 次元配列
  !        ya_ : 緯度方向 1 次元格子点データの並んだ 2 次元配列
  !
  !   * 関数名の間の文字列(DLon, GradLat, GradLat, DivLon, DivLat, Lapla, 
  !     LaplaInv, Jacobian)は, その関数の作用を表している.
  !
  !   * 関数名の最後 (_wa_wa, _wa, _xya, _xa, _ya, _w_w, _w, _xy, _x, _y) は, 
  !     入力変数の形スペクトルデータおよび格子点データであることを示している.
  !          _wa : スペクトルデータの並んだ 2 次元配列
  !       _wa_wa : 2 つのスペクトルデータの並んだ 2 次元配列
  !         _xva : 2 次元分散格子点データの並んだ 3 次元配列
  !          _xa : 経度方向 1 次元格子点データの並んだ 2 次元配列
  !          _ya : 緯度方向 1 次元格子点データの並んだ 2 次元配列
  !
  !=== 各データの種類の説明
  !
  !   * xya : 2 次元格子点データの並んだ 3 次元配列.
  !     変数の種類と次元は real(8), dimension(im,jm,:). 
  !     im, jm はそれぞれ経度, 緯度座標の格子点数であり, サブルーチン
  !     wa_Initial にてあらかじめ設定しておく. 
  !     扱う第 3 次元の大きさの最大値を wa_Initial で設定しておく.
  !
  !   * xva : 2 次元分散格子点データの並んだ 3 次元配列.
  !     変数の種類と次元は real(8), dimension(im,jc,:). 
  !     im, jc はそれぞれ経度の格子点数および
  !     プロセスで保有する緯度方向の格子点数である.  
  !     jc サブルーチン wa_mpi_Initial にて設定して
  !     おくことにより設定される public 変数である. 
  !     扱う第 3 次元の大きさの最大値を wa_mpi_Initial で設定しておく.
  !
  !   * wa : スペクトルデータの並んだ 2 次元配列.
  !     変数の種類と次元は real(8), dimension((nm+1)*(nm+1),:). 
  !     nm は球面調和函数の最大全波数であり, サブルーチン wa_Initial にて
  !     あらかじめ設定しておく. スペクトルデータの格納のされ方は関数 
  !     l_nm, nm_l によって調べることができる. 扱う第 3 次元の大きさの
  !     最大値を wa_Initial で設定しておく.
  !
  !   * xa, ya : 経度, 緯度方向 1 次元格子点データ.
  !     変数の種類と次元はそれぞれ real(8), dimension(im,:) 
  !     および real(8), dimension(jm,:).
  !
  !   * nma : スペクトルデータの並んだ 3 次元配列.
  !     変数の種類と次元は real(8), dimension(0:nm,-nm:nm,:). 
  !     第 1 次元が水平全波数, 第 2 次元が帯状波数を表す. nm は球面調和函数の
  !     最大全波数であり, サブルーチン wa_Initial にてあらかじめ設定しておく.
  !
  !   * na : スペクトルデータの並んだ 2 次元配列.
  !     変数の種類と次元は real(8), dimension(0:nm,:). 
  !     第 1 次元が水平全波数を表す.nm は球面調和函数の最大全波数であり, 
  !     サブルーチン wa_Initial にてあらかじめ設定しておく.
  !
  !   * wa_ で始まる関数が返す値はスペクトルデータの並んだ 2 次元配列に同じ.
  !
  !   * nma_ で始まる関数が返す値はスペクトルデータの並んだ 3 次元配列に同じ.
  !
  !   * na_ で始まる関数が返す値はスペクトルデータの並んだ 2 次元配列に同じ.
  !
  !   * xya_ で始まる関数が返す値は 2 次元格子点データの並んだ 3 次元配列に
  !     同じ.
  !
  !   * xva_ で始まる関数が返す値は 2 次元分散格子点データの並んだ 3 次元配列に
  !     同じ.
  !
  !   * xa_, ya_ で始まる関数が返す値は 1 次元格子点データの並んだ 2 次元
  !     配列に同じ.
  !
  !   * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !     微分などを作用させたデータをスペクトル変換したものことである.
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
  ! x_Lon, x_Lon_Weight,  ::  格子点座標(経度)と重みを格納した 1 次元配列
  ! y_Lat, y_Lat_Weight   ::  格子点座標(緯度)と重みを格納した 1 次元配列
  ! v_Lat, v_Lat_Weight   ::  分散格子点座標(緯度)と重みを格納した 1 次元配列
  ! xy_Lon, xy_Lat        ::  格子点データの経度・緯度座標(X,Y)
  !                           (格子点データ型 2 次元配列)
  ! xv_Lon, xv_Lat        ::  分散格子点データの経度・緯度座標(X,Y)
  !                           (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! xya_wa, xy_w :: スペクトルデータから格子データへの変換(多層, 1 層用)
  ! wa_xya, w_xy :: 格子データからスペクトルデータへの変換(多層, 1 層用)
  ! xva_wa, xv_w :: スペクトルデータから分散格子データへの変換(多層, 1 層用)
  ! wa_xva, w_xv :: 分散格子データからスペクトルデータへの変換(多層, 1 層用)
  ! l_nm, nm_l :: スペクトルデータの格納位置と全波数・帯状波数の変換 
  !
  !==== 微分
  !
  ! wa_Lapla_wa, w_Lapla_w        :: スペクトルデータにラプラシアンを作用させる(多層, 1 層用)
  ! rn                            :: スペクトルデータのラプラシアンを計算するための係数
  ! irm                           :: 経度微分演算用配列
  ! wa_LaplaInv_wa, w_LaplaInv_w  :: スペクトルデータにラプラシアンの逆変換を作用させる(多層, 1 層用)
  ! wa_DLon_wa, w_DLon_w          :: スペクトルデータに経度微分∂/∂λを作用させる(多層, 1 層用)
  ! xya_GradLon_wa, xy_GradLon_w  :: スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を作用させる(多層, 1 層用)
  ! xya_GradLat_wa, xy_GradLat_w  :: スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させる(多層, 1 層用)
  ! wa_DivLon_xya, w_DivLon_xy    :: 分散格子データに発散型経度微分 1/cosφ・∂/∂λを作用させる(多層, 1 層用)
  ! wa_DivLat_xya, w_DivLat_xy    :: 分散格子データに発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる (多層, 1 層用)
  ! wa_Div_xya_xya, w_Div_xy_xy   :: ベクトル成分である 2 つの分散格子データに発散を作用させる(多層, 1 層用)
  ! wa_Jacobian_wa_wa,            :: 2 つのスペクトルデータからヤコビアンを
  ! w_Jacobian_w_w                :: 計算する(多層, 1 層用)
  !
  ! xva_GradLon_wa, xv_GradLon_w  :: スペクトルデータに勾配型経度微分 1/cosφ・∂/∂λ を作用させる(多層, 1 層用)
  ! xva_GradLat_wa, xv_GradLat_w  :: スペクトルデータに勾配型緯度微分 ∂/∂φ を作用させる(多層, 1 層用)
  ! wa_DivLon_xva, w_DivLon_xv    :: 分散格子データに発散型経度微分 1/cosφ・∂/∂λを作用させる(多層, 1 層用)
  ! wa_DivLat_xva, w_DivLat_xv    :: 分散格子データに発散型緯度微分 1/cosφ・∂(g cosφ)/∂φを作用させる (多層, 1 層用)
  ! wa_Div_xva_xva, w_Div_xv_xv   :: ベクトル成分である 2 つの分散格子データに発散を作用させる(多層, 1 層用)
  ! wa_JacobianMPI_wa_wa,         :: 2 つのスペクトルデータからヤコビアンを
  ! w_JacobianMPI_w_w             :: 計算する(多層, 1 層用)
  !
  !
  !==== 微分(λ,μ=sinφ 座標)
  !
  ! xya_GradLambda_wa,  :: スペクトルデータに勾配型経度微分
  ! xy_GradLambda_w     :: ∂/∂λを作用させる(多層, 1 層用)
  !
  ! xya_GradMu_wa,      :: スペクトルデータに勾配型緯度微分
  ! xy_GradMu_w         :: (1-μ^2)∂/∂μを作用させる(多層, 1 層用)
  !
  ! wa_DivLambda_xya,   :: 分散格子データに発散型経度微分
  ! w_DivLambda_xy      :: 1 /(1-μ^2)・∂/∂λを作用させる(多層, 1 層用)
  !
  ! wa_DivMu_xya,       :: 分散格子データに発散型緯度微分
  ! w_DivMu_xy          :: ∂/∂μを作用させる(多層, 1 層用)
  !
  ! xva_GradLambda_wa,  :: スペクトルデータに勾配型経度微分
  ! xv_GradLambda_w     :: ∂/∂λを作用させる(多層, 1 層用)
  !
  ! xva_GradMu_wa,      :: スペクトルデータに勾配型緯度微分
  ! xv_GradMu_w         :: (1-μ^2)∂/∂μを作用させる(多層, 1 層用)
  !
  ! wa_DivLambda_xva,   :: 分散格子データに発散型経度微分
  ! w_DivLambda_xv      :: 1 /(1-μ^2)・∂/∂λを作用させる(多層, 1 層用)
  !
  ! wa_DivMu_xva,       :: 分散格子データに発散型緯度微分
  ! w_DivMu_xv          :: ∂/∂μを作用させる(多層, 1 層用)
  !
  !==== 補間計算
  !
  ! a_Interpolate_wa, Interpolate_w :: スペクトルデータから任意の点の関数値を計算する.
  !
  !==== 積分・平均
  !
  ! a_IntLonLat_xya, a_AvrLonLat_xya :: 2 次元格子点データの全領域積分および平均(多層用)
  ! IntLonLat_xy, AvrLonLat_xy    :: 2 次元格子点データの全領域積分および平均(1 層用)
  ! ya_IntLon_xya, ya_AvrLon_xya  :: 2 次元格子点データの経度方向積分および平均(多層用)
  ! y_IntLon_xy, y_AvrLon_xy   :: 2 次元格子点データの経度方向積分および平均(1 層用)
  ! a_IntLon_xa, a_AvrLon_xa   :: 1 次元(X)格子点データの経度方向積分および平均(多層用)
  ! IntLon_x, AvrLon_x         :: 1 次元(X)格子点データの経度方向積分および平均(1 層用)
  ! xa_IntLat_xya, x_AvrLat_xy  :: 2 次元格子点データの緯度方向積分および平均(多層用)
  ! x_IntLat_xy, x_AvrLat_xy   :: 2 次元格子点データの緯度方向積分および平均(1 層用)
  ! a_IntLat_ya, a_AvrLat_ya   :: 1 次元(Y)格子点データの緯度方向積分および平均(多層用)
  ! IntLat_y, AvrLat_y         :: 1 次元(Y)格子点データの緯度方向積分および平均(1 層用)
  !
  ! a_IntLonLat_xva, a_AvrLonLat_xva  :: 2 次元格子点データの全領域積分および平均(多層用)
  ! va_IntLon_xva, va_AvrLon_xva      :: 2 次元格子点データの経度方向積分および平均(多層用)
  ! xa_IntLat_xva, xa_AvrLat_xva      :: 2 次元格子点データの緯度方向積分および平均(多層用)
  ! a_IntLat_va, a_AvrLat_va          :: 1 次元(Y)格子点データの緯度方向積分および平均(多層用)
  !
  !==== スペクトル解析
  !
  ! nma_EnergyFromStreamfunc_wa :: 流線関数からエネルギースペクトルを計算する
  ! nm_EnergyFromStreamfunc_w   :: (水平全波数 n, 帯状波数 m 空間)(多層, 1 層用)
  ! 
  ! na_EnergyFromStreamfunc_wa   :: 流線関数からエネルギースペクトルを計算する
  ! n_EnergyFromStreamfunc_w     :: (水平全波数 n 空間) (多層, 1 層用)
  ! 
  ! nma_EnstrophyFromStreamfunc_wa :: 流線関数からエンストロフィースペクトルを
  ! nm_EnstrophyFromStreamfunc_w   :: 計算する(水平全波数 n, 帯状波数 m 空間)(多層, 1 層用)
  ! 
  ! na_EnstrophyFromStreamfunc_wa :: 流線関数からエンストロフィースペクトルを
  ! n_EnstrophyFromStreamfunc_w   :: 計算する(水平全波数 n 空間)(多層, 1 層用)
  ! 
  ! w_spectrum_VMiss              ::  欠損値
  !
  !
  use dc_message, only : MessageNotify
  use wa_module
  use w_base_mpi_module
  use w_deriv_mpi_module
  use w_integral_mpi_module
  use wa_base_mpi_module
  use wa_deriv_mpi_module
  use wa_integral_mpi_module

  private

  public wa_mpi_Initial                       ! 初期化
  public jc                                   ! 緯度方向分散格子点数

  public x_Lon, x_Lon_Weight                  ! 緯度格子座標・重み
  public v_Lat, v_Lat_Weight                  ! 経度分散格子座標・重み
  public y_Lat, y_Lat_Weight                  ! 経度格子座標・重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public xv_Lon, xv_Lat                       ! 分散格子座標(im,jc)
  public xy_w, w_xy, l_nm, nm_l               ! 変換関数
  public xv_w, w_xv                           ! 変換関数
  public xya_wa, wa_xya                       ! 変換関数
  public xva_wa, wa_xva                       ! 変換関数
  
  public rn, irm                              ! ラプラシアンの係数, 経度微分の配列
  public w_Lapla_w, w_LaplaInv_w              ! ラプラシアンと逆演算
  public w_DLon_w                             ! 経度微分
  public xv_GradLon_w, xv_GradLat_w           ! 勾配型微分
  public w_DivLon_xv, w_DivLat_xv             ! 発散型微分
  public w_Div_xv_xv                          ! 発散型微分
  public w_JacobianMPI_w_w                    ! ヤコビアン
  public xv_GradLambda_w, xv_GradMu_w         ! 勾配型微分(λ,μ座標)
  public w_DivLambda_xv, w_DivMu_xv           ! 発散型微分(λ,μ座標)

  public wa_Lapla_wa, wa_LaplaInv_wa          ! ラプラシアンと逆演算
  public wa_DLon_wa                           ! 経度微分
  public xva_GradLon_wa, xva_GradLat_wa       ! 勾配型微分
  public wa_DivLon_xva, wa_DivLat_xva         ! 発散型微分
  public wa_Div_xva_xva                       ! 発散型微分
  public wa_JacobianMPI_wa_wa                 ! ヤコビアン
  public xva_GradLambda_wa, xva_GradMu_wa     ! 勾配型微分(λ,μ座標)
  public wa_DivLambda_xva, wa_DivMu_xva       ! 発散型微分(λ,μ座標)

  public a_Interpolate_wa, Interpolate_w      ! 補間関数

  public IntLonLat_xy                         ! 緯度経度積分
  public y_IntLon_xy, IntLon_x                ! 経度積分    
  public x_IntLat_xy, IntLat_y                ! 緯度積分    
  public AvrLonLat_xy                         ! 緯度経度平均
  public y_AvrLon_xy, AvrLon_x                ! 経度平均    
  public x_AvrLat_xy, AvrLat_y                ! 緯度平均    

  public IntLonLat_xv                         ! 緯度経度積分
  public v_IntLon_xv                          ! 経度積分    
  public x_IntLat_xv, IntLat_v                ! 緯度積分    
  public AvrLonLat_xv                         ! 緯度経度平均
  public v_AvrLon_xv                          ! 経度平均    
  public x_AvrLat_xv, AvrLat_v                ! 緯度平均    

  public a_IntLonLat_xya                      ! 緯度経度積分
  public ya_IntLon_xya, a_IntLon_xa           ! 経度積分    
  public xa_IntLat_xya, a_IntLat_ya           ! 緯度積分    
  public a_AvrLonLat_xya                      ! 緯度経度平均
  public ya_AvrLon_xya, a_AvrLon_xa           ! 経度平均    
  public xa_AvrLat_xya, a_AvrLat_ya           ! 緯度平均    

  public a_IntLonLat_xva                      ! 緯度経度積分
  public va_IntLon_xva                        ! 経度積分    
  public xa_IntLat_xva, a_IntLat_va           ! 緯度積分    
  public a_AvrLonLat_xva                      ! 緯度経度平均
  public va_AvrLon_xva                        ! 経度平均    
  public xa_AvrLat_xva, a_AvrLat_va           ! 緯度平均    

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

contains
  subroutine wa_mpi_Initial(n_in,i_in,j_in,k_in)
    !
    ! スペクトル変換の格子点数, 波数, 最大データ数を設定する.
    !
    ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
    ! しなければならない. 
    !
    integer,intent(in) :: i_in                ! 格子点数(東西)
    integer,intent(in) :: j_in                ! 格子点数(南北)
    integer,intent(in) :: n_in                ! 切断波数
    integer,intent(in) :: k_in                ! 最大データ数(層数)

    call wa_Initial(n_in,i_in,j_in,k_in)
    call w_base_mpi_Initial
    call w_deriv_mpi_initial
    call wa_base_mpi_Initial

    call MessageNotify('M','wa_mpi_initial','wa_mpi_module is initialized')

  end subroutine wa_mpi_Initial

end module wa_mpi_module
