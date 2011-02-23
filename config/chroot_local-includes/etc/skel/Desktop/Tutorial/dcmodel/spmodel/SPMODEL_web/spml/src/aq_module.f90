!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module
!
!      spml/aq_module モジュールは 1 次元有限領域の下での流体運動を
!      Matsushima and Marcus (1994) で提唱された多項式を用いた
!      スペクトル数値計算ための Fortran90 関数を提供する. 
!      このルーチンは離散化にチェビシェフ--ガウス--ラダウ格子点を適用
!      しており, 主に 2 次元極座標,  円筒座標, 球座標の原点の
!      特異性を回避しながらスペクトル計算を行うために用いることを
!      念頭においている. 
!
!      2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
!      関数も提供しており, 2, 3 次元領域での計算のベースも提供する. 
!
!      Matsushima and Marcus (1994) の多項式に関する説明は
!      doc/spectral_radial.tex を参照のこと. 
!
!履歴  2008/03/26  竹広真一  新規作成
!      2008/05/02  竹広真一  コメント追加
!      2008/07/07  佐々木洋平 配列宣言修正, コメントを RDoc 用に微修正
!      2008/07/20  竹広真一  aq_r2_aq, q_r2_q, aq_r2Inv_aq, q_r2Inv_q 追加
!                            ag_aq 行列積和計算修正
!      2008/10/28  竹広真一  DGamma を削除, gammaln を直接使用
!      2009/01/09  竹広真一  aq_Initial メッセージに日付を追加
!      2009/01/23  佐々木洋平 rdoc 用にドキュメントを整形
!      2009/07/31  竹広真一  ローカル計算用配列を threadprivate 指定(OpenMP)
!      2009/12/06  竹広真一  threadprivate コメントアウト
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!++
module aq_module
  !
  != aq_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: aq_module.f90,v 1.13 2010-03-02 10:09:01 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/aq_module モジュールは 1 次元有限領域の下での流体運動を
  ! Matsushima and Marcus (1994) で提唱された多項式を用いたスペクトル法によって
  ! 数値計算するための Fortran90 関数を提供する.
  !
  ! このルーチンは離散化にチェビシェフ--ガウス--ラダウ格子点を適用して
  ! おり, 主に 2 次元極座標, 円筒座標, 球座標の原点の特異性を回避しな
  ! がらスペクトル計算を行うために用いることを念頭においている.
  !
  ! 2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
  ! 関数も提供しており, 2, 3 次元領域での計算のベースも提供する.
  !
  ! Matsushima and Marcus (1994) の多項式に関する説明は
  ! 動径座標のスペクトル法(spectral_radial.pdf[link:spectral_radial.pdf]) 
  ! を参照のこと.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (u_, g_, aq_, ag_) は, 返す値の形を示している.
  !   複数並んだ 2 次元データの第 1 次元の大きさは aq_Initial で
  !   設定する重みの指数の配列 nd の大きさと同じでなければならない.
  !   q_  :: スペクトルデータ
  !   g_  :: 1 次元格子点データ
  !   aq_ :: 1 次元スペクトルデータが複数並んだ 2 次元データ
  !   ag_ :: 1 次元格子点データが複数並んだ 2 次元データ.
  !
  ! * 関数名の間の文字列(Dr)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_e, _aq, _g, _ag) は, 入力変数の形がスペクトルデータ
  !   および格子点データであることを示している.
  !   _q  :: スペクトルデータ
  !   _g  :: 1 次元格子点データ
  !   _aq :: 1 次元スペクトルデータが複数並んだ 2 次元データ
  !   _ag :: 1 次元格子点データが複数並んだ 2 次元データ
  !
  !=== 各データの種類の説明
  !
  ! * g : 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(im). 
  !   * im は R 座標の格子点数であり, サブルーチン aq_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * q : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(0:km). 
  !   * km は R 方向の最大波数であり, サブルーチン aq_Initial にて
  !     あらかじめ設定しておく. スペクトルデータの格納のされ方については...
  !
  ! * ag : 1 次元(R)格子点データの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(size(nd),im). 
  !     第 2 次元が R 方向を表す.
  !
  ! * aq : 1 次元スペクトルデータの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(size(nd),0:km). 
  !     第 2 次元がスペクトルを表す.
  !
  ! * g_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * q_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * ag_ で始まる関数が返す値は 1 次元格子点データの並んだ 
  !   2 次元データに同じ.
  !
  ! * aq_ で始まる関数が返す値は 1 次元スペクトルデータの並んだ
  !   2 次元データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したもののことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! aq_Initial  :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! g_R        :: 格子点座標(R)を格納した 1 次元配列
  ! g_R_Weight :: 重み座標を格納した 1 次元配列
  !
  !==== 基本変換
  !
  ! g_q, ag_aq :: スペクトルデータから格子データへの変換
  ! q_g, aq_ag :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! q_rDr_q, aq_rDr_aq :: スペクトルデータに r(d/dR) 微分を作用させる
  !
  !==== 積
  !
  ! q_r2_q,    aq_r2_aq    :: スペクトルデータに r^2 をかける
  ! q_r2Inv_q, aq_r2Inv_aq :: スペクトルデータに r^-2 をかける
  !
  !==== 積分・平均
  !
  ! a_Int_ag, a_Avr_ag :: 1 次元格子点データの並んだ 2 次元配列の積分および平均
  ! Int_g, Avr_g       :: 1 次元格子点データの積分および平均
  !
  !==== 境界値問題
  !
  ! aq_Boundary_D, aq_Boundary_N         :: 外側ディリクレ条件, ノイマン条件
  ! aq_BoundaryTau_D, aq_BoundaryTau_N   :: 外側ディリクレ条件, 
  !                                         ノイマン条件(タウ法)
  ! ag_BoundaryGrid_D, ag_BoundaryGrid_N :: 外側ディリクレ条件, 
  !                                         ノイマン条件(選点法)
  !
  use dc_message
  use lumatrix
  implicit none

  private

  public  aq_initial                           ! 初期化  
  public  g_R, g_R_Weight                      ! 座標変数
                                        
  public  ag_aq, aq_ag, g_q, q_g               ! 基本変換
                                        
  public  aq_rDr_aq, q_rDr_q                   ! 微分    
  public  aq_r2_aq, q_r2_q                     ! 積 
  public  aq_r2Inv_aq, q_r2Inv_q               ! 積 

  public  a_Int_ag, a_Avr_ag                   ! 積分・平均
  public  Int_g, Avr_g                         ! 積分・平均

  public  aq_Boundary_D, aq_Boundary_N         ! 境界条件
  public  aq_BoundaryTau_D, aq_BoundaryTau_N   ! 境界条件
  public  ag_BoundaryGrid_D, ag_BoundaryGrid_N ! 境界条件

  interface aq_Boundary_D
     !
     ! 外側境界ディリクレ型境界条件の適用(タウ法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)   :: aq_data
     !   !(inout) 境界条件を適用するスペクトルデータ
     !  
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元チェビシェフデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するスペクトルデータ
     !  
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure aq_BoundaryTau_D_1d, aq_BoundaryTau_D_2d
  end interface

  interface aq_Boundary_N
     !
     ! 外側境界ノイマン型境界条件の適用(タウ法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) 境界条件を適用するスペクトルデータ
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元チェビシェフデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するスペクトルデータ
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure aq_BoundaryTau_N_1d, aq_BoundaryTau_N_2d
  end interface

  interface aq_BoundaryTau_D
     !
     ! 外側境界ディリクレ型境界条件の適用(タウ法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !  
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元スペクトルデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !  
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure aq_BoundaryTau_D_1d, aq_BoundaryTau_D_2d
  end interface

  interface aq_BoundaryTau_N
     !
     ! 外側境界ノイマン型境界条件の適用(タウ法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元スペクトルデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure aq_BoundaryTau_N_1d, aq_BoundaryTau_N_2d
  end interface

  interface ag_BoundaryGrid_D
     !
     ! 外側境界ディリクレ型境界条件の適用(選点法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元スペクトルデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure ag_BoundaryGrid_D_1d, ag_BoundaryGrid_D_2d
  end interface

  interface ag_BoundaryGrid_N
     !
     ! 外側境界ノイマン型境界条件の適用(選点法). 
     !
     ! * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !   使い分けている. ユーザーインターフェースは共通であるので
     !   下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     ! * 1 次元スペクトルデータの並んだ 2 次元配列の場合
     !
     !   real(8), dimension(size(md),0:km),intent(inout)    :: aq_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), dimension(size(md)), intent(in), optional :: value
     !   !(in) 適用する境界値
     !
     ! * 1 次元スペクトルデータの場合
     !
     !   real(8), dimension(0:km),intent(inout)       :: q_data
     !   !(inout) 境界条件を適用するチェビシェフデータ
     !   
     !   real(8), intent(in), optional                :: value
     !   !(in) 適用する境界値
     !
     module procedure ag_BoundaryGrid_N_1d, ag_BoundaryGrid_N_2d
  end interface

  integer :: im, km                       ! 格子点数, 切断波数
  real(8) :: ra                           ! 領域の大きさ

  real(8) :: alpha                        ! 展開多項式パラメター  0 < α <= 1
  real(8) :: beta                         ! 展開多項式パラメター  0 < β
  real(8) :: gamma                        ! 展開多項式パラメター  γ=2α+β
  integer, allocatable :: md(:)           ! 展開多項式上付次数のならび

  integer :: jmax                         ! a 座標(データ第 1 次元)の大きさ

  real(8), allocatable :: g_R(:)          ! ガウス--ラダウ格子点
  real(8), allocatable :: g_R_Weight(:)   ! ガウス重み

  real(8), allocatable :: CF(:,:,:)       ! 正変換用行列
  real(8), allocatable :: CB(:,:,:)       ! 逆変換用行列


  logical :: first_r2inv  =.true.
  logical :: first_Tau_D  =.true.
  logical :: first_Tau_N  =.true.
  logical :: first_Grid_N =.true.

  save im, km, ra, alpha, beta, gamma, CF, CB, g_R, g_R_Weight, md, jmax
  save first_r2inv, first_Tau_D, first_Tau_N, first_Grid_N

contains

! --- 初期化
  subroutine aq_Initial(i_in,k_in,r_in,alpha_in,beta_in,md_in)
    !
    ! スペクトル変換の格子点数, 波数, 領域の大きさ, 重みを設定する.
    ! 
    ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
    ! 初期設定をしなければならない.
    !
    integer,intent(in) :: i_in              !(in) 格子点数
    integer,intent(in) :: k_in              !(in) 切断波数
    real(8),intent(in) :: r_in              !(in) 外側境界の座標(半径)
    real(8),intent(in) :: alpha_in          !(in) 展開多項式パラメター
    real(8),intent(in) :: beta_in           !(in) 展開多項式パラメター 
    integer,intent(in) :: md_in(:)          !(in) 展開多項式上付次数のならび

    integer i, j, n

   !--- パラメターのチェック・記憶
    im=i_in ; km=k_in ; ra = r_in; alpha=alpha_in; beta=beta_in

    if ( km .ge. 2*im ) then
       call MessageNotify('E','aq_initial','KM shoud be less than 2*IM')
    endif

    if ( alpha .le. 0.0D0 ) then
       call MessageNotify('E','aq_initial','alpha must be larger than 0')
    endif
    if ( alpha .gt. 1.0D0 ) then
       call MessageNotify('E','aq_initial','alpha must be smaller equal to 1')
    endif
    if ( beta .le. 0.0D0 ) then
       call MessageNotify('E','aq_initial','beta must be larger than 0')
    endif

    gamma = 2.0D0 * alpha + beta

   !--- 次数情報の計算
    jmax = size(md_in)
    if ( allocated(md) ) deallocate(md)
    allocate(md(jmax))

    md = md_in

   !--- 格子点, 重みの設定
    if ( allocated(g_R) ) deallocate(g_R)
    if ( allocated(g_R_Weight) ) deallocate(g_R_Weight)
    allocate(g_R(im),g_R_Weight(im))

    call gauss_radau(2*im, g_R, g_R_Weight)
    g_R = ra * g_R
    g_R_Weight = ra**(gamma-1) * g_R_Weight

   !--- 変換行列の設定
    if ( allocated(CF) ) deallocate(CF)
    if ( allocated(CB) ) deallocate(CB)
    allocate(CF(jmax,0:km,im),CB(jmax,im,0:km))

    CF = 0.0D0
    do j=1,jmax
       do n=md(j),km,2
          do i=1,im
             CF(j,n,i) = Phi(g_R(i)/ra,n,md(j)) * g_R_Weight(i)/ra**(gamma-1)
          enddo
       enddo
    enddo

    CB = 0.0D0
    do j=1,jmax
       do i=1,im
          do n=md(j),km,2
             CB(j,i,n) = Phi(g_R(i)/ra,n,md(j)) 
          enddo
       enddo
    enddo

   !--- 各ルーチン変換行列の初期化スイッチ

    first_r2inv  = .true.
    first_Tau_D  = .true.
    first_Tau_N  = .true. 
    first_Grid_N = .true.

    call MessageNotify(&
      'M','aq_initial','aq_module (2009/07/31) is initialized')

  end subroutine aq_Initial

! ---- 逆変換 ---- 
  function ag_aq(aq_data)
    !
    ! スペクトルデータから格子データへ変換する(2 次元配列用).
    !
    real(8), dimension(:,0:), intent(in)    :: aq_data
    !(in) スペクトルデータ

    real(8), dimension(size(aq_data,1),im)  :: ag_aq
    !(out) 格子点データ

    integer :: i, j

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','ag_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','ag_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','ag_aq', &
            'The spectral dimension of input data too large.')
    endif

    ag_aq = 0.0D0
    do i=1,im
!$omp parallel do
       do j=1,jmax
             ag_aq(j,i) = sum(CB(j,i,md(j):km:2)*aq_data(j,md(j):km:2))
       enddo
!$omp end parallel do
    enddo

  end function ag_aq

  function g_q(q_data)
    !
    ! スペクトルデータから格子データへ変換する(1 次元配列用).
    !
    real(8), dimension(:), intent(in)  :: q_data
    !(in) スペクトルデータ

    real(8), dimension(im)             :: g_q
    !(out) 格子点データ

    real(8), dimension(1,size(q_data)) :: q_work
    ! 作業用配列
    real(8), dimension(1,im)         :: g_work
    ! 作業用配列

    q_work(1,:) = q_data  
    g_work = ag_aq(q_work)
    g_q = g_work(1,:)

  end function g_q

! ---- 正変換 ---- 
  function aq_ag(ag_data)
    !
    ! 格子データからスペクトルデータへ変換する(2 次元配列用).
    !
    real(8), dimension(:,:), intent(in)      :: ag_data
    !(in) 格子点データ

    real(8), dimension(size(ag_data,1),0:km) :: aq_ag
    !(out) スペクトルデータ

    integer :: j, n

    if ( size(ag_data,1) /= jmax ) then
       call MessageNotify('E','aq_ag', &
            '1st dim. of the grid data should be same as dim. of MD.')
    end if

    if ( size(ag_data,2) < im ) then
       call MessageNotify('E','aq_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag_data,2) > im ) then
       call MessageNotify('W','aq_ag', &
            'The Grid points of input data too large.')
    endif

    aq_ag = 0.0D0
!$omp parallel do private(n)
    do j=1,jmax
       do n=md(j),km,2
          aq_ag(j,n) = sum(CF(j,n,:)*ag_data(j,:))
       enddo
    enddo
!$omp end parallel do          

  end function aq_ag

  function q_g(g_data)
    !
    ! 格子データからスペクトルデータへ変換する(1 次元配列用).
    !
    real(8), dimension(:), intent(in)     :: g_data
    !(in) 格子点データ

    real(8), dimension(0:km)              :: q_g
    !(out) スペクトルデータ

    real(8), dimension(1,size(g_data)) :: ag_work
    real(8), dimension(1,0:km)         :: aq_work

    ag_work(1,:) = g_data
    aq_work = aq_ag(ag_work)
    q_g = aq_work(1,:)
    
  end function q_g

! ---- 微分計算 ---- 
  function aq_rDr_aq(aq_data)
    !
    ! 入力スペクトルデータに対して微分 r(d/dr) のスペクトル係数
    ! を計算する(2 次元配列用).
    !
    !   a_n = aq_data/sqrt(Inm),  b_n = aq_rDr_aq/sqrt(Inm)
    !
    !   b_n = (2n+gamma-1)/(2n+gamma+3)b_n+2
    !       + (2n+gamma-1)(n+gamma+1)/(2n+gamma+3)a_n+2 + n a_n
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) 入力スペクトルデータ

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_rDr_aq
    !(out) 出力スペクトルデータ

    integer :: j, n
    integer :: nstr, nend
    real(8) :: sqInm, sqInm2

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_rDr_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_rDr_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_rDr_aq', &
            'The spectral dimension of input data too large.')
    endif

    aq_rDr_aq = 0.0D0
!$omp parallel do private(nstr,nend,sqInm,sqInm2,n)
    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          nstr=md(j) ; nend=km
       else
          nstr=md(j) ; nend=km-1
       endif
       
       sqInm = sqrt(Inm(nend,md(j)))
       aq_rDr_aq(j,nend) = nend * aq_data(j,nend)

       do n=nend-2,nstr,-2
          sqInm2 = sqInm
          sqInm = sqrt(Inm(n,md(j)))

          aq_rDr_aq(j,n) = sqInm/sqInm2 * (&
                 (2*n+gamma-1)/(2*n+gamma+3) * aq_rDr_aq(j,n+2) &
               + (2*n+gamma-1)*(n+gamma+1)/(2*n+gamma+3) * aq_data(j,n+2) )&
               + n * aq_data(j,n)
       enddo
    enddo
!$omp end parallel do

  end function aq_rDr_aq

  function q_rDr_q(q_data)
    !
    ! 入力スペクトルデータに r(d/dR) 微分を作用する(1 次元配列用).
    !
    ! スペクトルデータの r(d/dR) 微分とは, 対応する格子点データに R 微分を
    ! 作用させたデータのスペクトル変換のことである.
    !
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(0:km)            :: q_rDr_q
    !(out) チェビシェフデータの R 微分

    real(8), dimension(1,size(q_data))  :: aq_work
    ! 作業用配列

    aq_work(1,:) = q_data
    aq_work = aq_rDr_aq(aq_work)
    q_rDr_q = aq_work(1,:)

  end function q_rDr_q

! ---- r^2 積計算 ---- 
  function aq_r2_aq(aq_data)
    !
    ! 入力スペクトルデータに対して積 r^2 のスペクトル係数
    ! を計算する(2 次元配列用).
    !
    !   a_n^m = aq_data/sqrt(Inm),  b_n^m = aq_rDr_aq/sqrt(Inm)
    !
    !   b_n^m = (n-|m|)(n+|m|+beta-1)/((2n+gamma-5)(2n+gamma-3)) a_n-2^m
    !       + (2n(n+gamma-1) + 2|m|(|m|+beta-1)+(gamma-3)(beta+1)
    !            /((2n+gamma+1)(2n+gamma-3)) a_n^m
    !       + (n-|m|+gamma-beta)(n+|m|+gamma-1)
    !            /((2n+gamma+3)(2n+gamma+1)) a_n+2^m
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) 入力スペクトルデータ

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_r2_aq
    !(out) 出力スペクトルデータ

    integer :: j, n, m
    integer :: nstr, nend
    real(8) :: sqrInp2m, sqrInm, sqrInm2m

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_r2_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_r2_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_r2_aq', &
            'The spectral dimension of input data too large.')
    endif

    aq_r2_aq = 0.0D0

!$omp parallel do private(nstr,nend,sqrInm,sqrInm2m,sqrInp2m,n,m)
    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          nstr=md(j) ; nend=km
       else
          nstr=md(j) ; nend=km-1
       endif

       m = abs(md(j))

       n = nstr
       sqrInm   = sqrt(Inm(n,m))
       sqrInp2m = sqrt(Inm(n+2,m))
       aq_r2_aq(j,n) =                                            & 
            (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
               /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      &  
          + (n-m+gamma-beta)*(n+m+gamma-1)                        &
               /((2*n+gamma+3)*(2*n+gamma+1))                     &
                * aq_data(j,n+2) * (sqrInm/sqrInp2m)

       do n=nstr+2,nend-2,2
          sqrInm2m = sqrInm
          sqrInm   = sqrInp2m
          sqrInp2m = sqrt(Inm(n+2,m))

          aq_r2_aq(j,n) = &
             (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
             * aq_data(j,n-2)* (sqrInm/sqrInm2m)                   &
           + (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      &
           + (n-m+gamma-beta)*(n+m+gamma-1)                        &
                /((2*n+gamma+3)*(2*n+gamma+1))                     &
                * aq_data(j,n+2) * (sqrInm/sqrInp2m)
       enddo

       n = nend
       sqrInm2m = sqrInm
       sqrInm   = sqrInp2m
       aq_r2_aq(j,n) = &
            (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))       &
             * aq_data(j,n-2)* (sqrInm/sqrInm2m)                   &
           + (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                /((2*n+gamma+1)*(2*n+gamma-3)) * aq_data(j,n)      
    enddo
!$omp end parallel do

    aq_r2_aq = aq_r2_aq * ra**2

  end function aq_r2_aq

  function q_r2_q(q_data)
    !
    ! 入力スペクトルデータに対して積 r^2 のスペクトル係数
    ! を計算する(1 次元配列用).
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(0:km)            :: q_r2_q
    !(out) チェビシェフデータの R 微分

    real(8), dimension(1,size(q_data))  :: aq_work
    ! 作業用配列

    aq_work(1,:) = q_data
    aq_work = aq_r2_aq(aq_work)
    q_r2_q = aq_work(1,:)

  end function q_r2_q

! ---- r^-2 積計算 ---- 
  function aq_r2Inv_aq(aq_data)
    !
    ! 入力スペクトルデータに対して積 r^-2 のスペクトル係数
    ! を計算する(2 次元配列用).
    !
    !   a_n^m = aq_r2Inv_aq/sqrt(Inm),  b_n^m = aq_data/sqrt(Inm)
    !
    !   b_n^m = (n-|m|)(n+|m|+beta-1)/((2n+gamma-5)(2n+gamma-3)) a_n-2^m
    !       + (2n(n+gamma-1) + 2|m|(|m|+beta-1)+(gamma-3)(beta+1)
    !            /((2n+gamma+1)(2n+gamma-3)) a_n^m
    !       + (n-|m|+gamma-beta)(n+|m|+gamma-1)
    !            /((2n+gamma+3)(2n+gamma+1)) a_n+2^m
    !
    real(8), dimension(:,0:), intent(in)     :: aq_data
    !(in) 入力スペクトルデータ

    real(8), dimension(size(aq_data,1),0:size(aq_data,2)-1) :: aq_r2Inv_aq
    !(out) 出力スペクトルデータ

    real(8), dimension(:,:,:), allocatable    :: R2MTX
    integer, dimension(:,:), allocatable      :: kp

    integer :: j, n, m
    integer :: nstr, nend
    real(8) :: sqrInp2m, sqrInm, sqrInm2m

    save R2MTX, kp

    if ( size(aq_data,1) /= jmax ) then
       call MessageNotify('E','aq_r2Inv_aq', &
            '1st dim. of the spectral data should be same as dim. of MD.')
    end if

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_r2Inv_aq', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_r2Inv_aq', &
            'The spectral dimension of input data too large.')
    endif

    if ( first_r2inv ) then
       first_r2inv = .false.
       if ( allocated(R2MTX) ) deallocate(R2MTX)
       if ( allocated(kp) ) deallocate(kp)
       allocate(R2MTX(jmax,0:km,0:km),kp(jmax,0:km))

       R2MTX(:,:,:) = 0.0D0
       do n=0,km
          R2MTX(:,n,n) = 1.0D0
       enddo

!$omp parallel do private(nstr,nend,sqrInm,sqrInm2m,sqrInp2m,n,m)
       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             nstr=md(j) ; nend=km
          else
             nstr=md(j) ; nend=km-1
          endif

          m = abs(md(j))

          n = nstr

          sqrInm   = sqrt(Inm(n,m))
          sqrInp2m = sqrt(Inm(n+2,m))

          R2MTX(j,n,n)   = &
               (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
               /((2*n+gamma+1)*(2*n+gamma-3))
          
          R2MTX(j,n,n+2) = &
               (n-m+gamma-beta)*(n+m+gamma-1)                        &
               /((2*n+gamma+3)*(2*n+gamma+1))                        &
               * (sqrInm/sqrInp2m)

          do n=nstr+2,nend-2,2
             sqrInm2m = sqrInm
             sqrInm   = sqrInp2m
             sqrInp2m = sqrt(Inm(n+2,m))

             R2MTX(j,n,n-2) = &
                (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
                *(sqrInm/sqrInm2m)

             R2MTX(j,n,n)   = &
                (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                   /((2*n+gamma+1)*(2*n+gamma-3))

             R2MTX(j,n,n+2) = &
                (n-m+gamma-beta)*(n+m+gamma-1)                        &
                   /((2*n+gamma+3)*(2*n+gamma+1))                     &
                   * (sqrInm/sqrInp2m)
          enddo

          n = nend
          sqrInm2m = sqrInm
          sqrInm   = sqrInp2m
             R2MTX(j,n,n-2) = &
                (n-m)*(n+m+beta-1)/((2*n+gamma-5)*(2*n+gamma-3))      &
                *(sqrInm/sqrInm2m)
             R2MTX(j,n,n)   = &
                (2*n*(n+gamma-1) + 2*m*(m+beta-1)+(gamma-3)*(beta+1)) &
                   /((2*n+gamma+1)*(2*n+gamma-3))
       enddo
!$omp end parallel do

       R2MTX = R2MTX * ra**2

       call LuDecomp(R2MTX,kp)

    end if

    aq_r2Inv_aq = LuSolve(R2MTX,kp,aq_data)
    
  end function aq_r2Inv_aq

  function q_r2Inv_q(q_data)
    !
    ! 入力スペクトルデータに対して積 r^2 のスペクトル係数
    ! を計算する(1 次元配列用).
    !
    real(8), dimension(:), intent(in)   :: q_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(0:km)            :: q_r2Inv_q
    !(out) チェビシェフデータの R 微分

    real(8), dimension(1,size(q_data))  :: aq_work
    ! 作業用配列

    aq_work(1,:) = q_data
    aq_work = aq_r2Inv_aq(aq_work)
    q_r2Inv_q = aq_work(1,:)

  end function q_r2Inv_q

!--------------- 積分計算 -----------------
  function a_Int_ag(ag)
    !
    ! 1 次元格子点データが並んだ 2 次元配列の積分
    !
    !      \int_0^a f(R) W(R) dR, W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! を計算する. 
    !
    real(8), dimension(:,:), intent(in)      :: ag
    !(in)入力格子点データ

    real(8), dimension(size(ag,1))           :: a_Int_ag
    !(out) 積分したデータ
    integer :: i

    if ( size(ag,2) < im ) then
       call MessageNotify('E','a_Int_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag,2) > im ) then
       call MessageNotify('W','a_Int_ag', &
            'The Grid points of input data too large.')
    endif

    a_Int_ag = 0.0d0
    do i=1,im
       a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_R_Weight(i)
    enddo
  end function a_Int_ag

  function Int_g(g)
    !
    ! 1 次元格子点データの積分
    !
    !      \int_0^a f(R) W(R) dR, W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! を行う
    !
    real(8), dimension(im), intent(in)   :: g
    !(in) 格子点データ

    real(8)                              :: Int_g
    !(out) 積分値

    Int_g = sum(g*g_R_Weight)

  end function Int_g

  function a_Avr_ag(ag)
    !
    ! 1 次元格子点データが並んだ 2 次元配列の平均
    !
    !      \int_0^a f(R) W(R) dR/\int_0^a W(R) dR, 
    !          W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! を計算する. 
    !
    real(8), dimension(:,0:), intent(in)   :: ag
    !(in)入力格子点データ

    real(8), dimension(size(ag,1))         :: a_Avr_ag
    !(out) 平均したデータ

    a_Avr_ag = a_Int_ag(ag)/sum(g_R_Weight)

  end function a_Avr_ag

  function Avr_g(g)
    !
    ! 1 次元格子点データの平均
    !
    !      \int_0^a f(R) W(R) dR/\int_0^a W(R) dR, 
    !          W(R) = R^beta/(a^2-R^2)^(1-alpha)
    !
    ! を計算する. 
    !
    real(8), dimension(im), intent(in)   :: g
    !(in) 格子点データ

    real(8)                              :: Avr_g
    !(out) 積分値

    Avr_g = Int_g(g)/sum(g_R_Weight)
  end function Avr_g

!---- Dirichlet 型境界条件(タウ法) ----

  subroutine aq_BoundaryTau_D_2d(aq_data,value)
    !
    ! Dirichlet 型境界条件の適用(タウ法, 2 次元配列用)
    ! * 外側境界(i=im)での値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: aq_data 
    !(inout) 境界条件を適用するチェビシェフデータ(jmax,0:km)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) 境界値(jmax)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(aq_data,1),0:km):: aq_work
    real(8), dimension(size(aq_data,1),im)  :: ag_work
    real(8), dimension(size(aq_data,1))     :: value0           ! 境界値

    integer :: k, j, kstr, kend
    save    :: alu, kp

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_BoundaryTau_D', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_BoundaryTau_D', &
            'The spectral dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Tau_D ) then
       first_Tau_D = .false.

       allocate(alu(size(aq_data,1),0:km,0:km),kp(size(aq_data,1),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             kstr = md(j) ; kend = km
          else
             kstr = md(j) ; kend = km-1
          endif

          do k=kstr,kend,2
             aq_work = 0.0D0
             aq_work(j,k) = 1.0D0
             ag_work = ag_aq(aq_work)
             alu(j,kend,k) = ag_work(j,im)
          enddo
       enddo

       call ludecomp(alu,kp)
    endif

    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          aq_data(j,km)   = value0(j)
       else
          aq_data(j,km-1)   = value0(j)
       endif
    enddo

    aq_data = lusolve(alu,kp,aq_data)

  end subroutine aq_BoundaryTau_D_2d

  subroutine aq_BoundaryTau_D_1d(q_data,value)
    !
    ! Dirichlet 型境界条件の適用(タウ法, 1 次元配列用)
    ! * 両境界での値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: q_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: aq_work
    real(8), dimension(1)                        :: vwork  ! 境界値

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    aq_work(1,:)=q_data
    call aq_BoundaryTau_D_2d(aq_work,vwork)
    q_data=aq_work(1,:)

  end subroutine aq_BoundaryTau_D_1d

!---- Neumann 型境界条件(タウ法) ----

  subroutine aq_BoundaryTau_N_2d(aq_data,value)
    !
    ! 外側境界 Neumann 型境界条件の適用(タウ法, 2 次元配列用)
    ! * i=im で勾配の値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: aq_data
    !(inout) 境界条件を適用するチェビシェフデータ(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable    :: alu
    integer, dimension(:,:), allocatable      :: kp
    real(8), dimension(size(aq_data,1),0:km)  :: aq_work
    real(8), dimension(size(aq_data,1),im)    :: ag_work
    real(8), dimension(size(aq_data,1))       :: value0           ! 境界値

    integer :: j, k, kstr, kend

    save    :: alu, kp

    if ( size(aq_data,2)-1 < km ) then
       call MessageNotify('E','aq_BoundaryTau_N', &
            'The spectral dimension of input data too small.')
    elseif ( size(aq_data,2)-1 > km ) then
       call MessageNotify('W','aq_BoundaryTau_N', &
            'The spectral dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Tau_N ) then
       first_Tau_N = .false.
       allocate(alu(jmax,0:km,0:km),kp(jmax,0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do j=1,jmax
          if ( mod(md(j),2) .eq. mod(km,2) ) then
             kstr=md(j) ; kend = km
          else
             kstr=md(j) ; kend = km-1
          endif

          do k=kstr,kend,2
             aq_work = 0.0D0
             aq_work(j,k) = 1.0D0
             ag_work = ag_aq(aq_rDr_aq(aq_work))/spread(g_R,1,jmax)
             alu(j,kend,k) = ag_work(j,im)
          enddo
       enddo

       call ludecomp(alu,kp)
    endif

    do j=1,jmax
       if ( mod(md(j),2) .eq. mod(km,2) ) then
          aq_data(j,km)   = value0(j)
       else
          aq_data(j,km-1)   = value0(j)
       endif
    enddo

    aq_data = lusolve(alu,kp,aq_data)

  end subroutine aq_BoundaryTau_N_2d

  subroutine aq_BoundaryTau_N_1d(q_data,value)
    !
    ! Dirichlet/Neumann 型境界条件の適用(タウ法, 1 次元配列用)
    ! * i=0 で勾配の値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: q_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: aq_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    aq_work(1,:)=q_data
    call aq_BoundaryTau_N_2d(aq_work,vwork)
    q_data=aq_work(1,:)

  end subroutine aq_BoundaryTau_N_1d

!---- Dirichlet 型境界条件(選点法) ----

  subroutine ag_BoundaryGrid_D_2d(ag_data,value)
    !
    ! Dirichlet 型境界条件の適用(選点法, 2 次元配列用)
    ! * 外側境界(i=im)での値を与える.
    !
    real(8), dimension(:,:),intent(inout)         :: ag_data
    !(inout) 境界条件を適用するチェビシェフデータ(jmax,im)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) 境界値(jmax)

    real(8), dimension(size(ag_data,1))       :: value0 

    if (.not. present(value)) then
       value0=0.0d0
    else
       value0 = value
    endif
    ag_data(:,im) = value0

  end subroutine ag_BoundaryGrid_D_2d

  subroutine ag_BoundaryGrid_D_1d(g_data,value)
    !
    ! Dirichlet 型境界条件の適用(タウ法, 1 次元配列用)
    ! * 両境界での値を与える.
    !
    real(8), dimension(im),intent(inout)       :: g_data
    !(inout) 境界条件を適用するチェビシェフデータ(im)

    real(8), intent(in), optional              :: value
    !(in) 境界値

    real(8), dimension(1,im)                   :: ag_work
    real(8), dimension(1)                      :: vwork

    if (.not. present(value)) then
       vwork(1)=0.0d0
    else
       vwork(1) = value
    endif

    ag_work(1,:)=g_data
    call ag_BoundaryGrid_D_2d(ag_work,vwork)
    g_data=ag_work(1,:)

  end subroutine ag_BoundaryGrid_D_1d

!---- Neumann 型境界条件(選点法) ----

  subroutine ag_BoundaryGrid_N_2d(ag_data,value)
    !
    ! 外側境界 Neumann 型境界条件の適用(選点法, 2 次元配列用)
    ! * i=im で勾配の値を与える.
    !
    real(8), dimension(:,:),intent(inout)         :: ag_data
    !(inout) 境界条件を適用するチェビシェフデータ(m,im)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable    :: alu
    integer, dimension(:,:), allocatable      :: kp
    real(8), dimension(size(ag_data,1),im)    :: ag_work
    real(8), dimension(size(ag_data,1))       :: value0           ! 境界値

    integer :: i
    save    :: alu, kp

    if ( size(ag_data,2) < im ) then
       call MessageNotify('E','aq_BoundaryGrid_N', &
            'The dimension of input data too small.')
    elseif ( size(ag_data,2) > im ) then
       call MessageNotify('W','aq_BoundaryGrid_N', &
            'The dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0.0D0
    else
       value0 = value
    endif

    if ( first_Grid_N ) then
       first_Grid_N = .false.
       allocate(alu(jmax,im,im),kp(jmax,im))

       alu=0.0D0
       do i=1,im
          alu(:,i,i) = 1.0D0
       enddo

       do i=1,im
          ag_work = 0.0D0
          ag_work(:,i) = 1.0D0
          ag_work = ag_aq(aq_rDr_aq(aq_ag(ag_work)))/spread(g_R,1,jmax)
          alu(:,im,i) = ag_work(:,im)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data(:,im) = value0

    ag_data = lusolve(alu,kp,ag_data)

  end subroutine ag_BoundaryGrid_N_2d

  subroutine ag_BoundaryGrid_N_1d(g_data,value)
    !
    ! Dirichlet/Neumann 型境界条件の適用(選点法, 1 次元配列用)
    ! * i=0 で勾配の値を与える.
    !
    real(8), dimension(im),intent(inout)       :: g_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,im)                   :: ag_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0.0D0
    else
       vwork(1) = value
    endif

    ag_work(1,:)=g_data
    call ag_BoundaryGrid_N_2d(ag_work,vwork)
    g_data=ag_work(1,:)

  end subroutine ag_BoundaryGrid_N_1d

!---- 下部ルーチン
  subroutine gauss_radau(M,g_R,g_W)
    !
    ! M 次の多項式 π(r) の零点から
    ! ガウス-ラダウ格子点とガウス重みを計算する
    !
    integer, intent(in)  :: M                
    ! (in) 多項式打切次数(0 < M, M even)
    real(8), intent(out) :: g_R(M/2)         ! ガウス-ラダウ格子点
    real(8), intent(out) :: g_W(M/2)         ! ガウス重み

    real(8) :: r1,r2,dr
    real(8) :: pi1, pi2
    integer :: i, ir
    character(len=4) :: CNUM
    real(8), parameter :: eps = 1.0D-15      ! 打切誤差
    integer, parameter :: nrfact = 100       ! 格子点走査間隔を定める数

    !--- 入力チェック
    if ( mod(M,2) .ne. 0 ) then
       call MessageNotify('E','gauss_radau','M must be even')
    endif

    !--- ガウス--ラダウ格子点
    dr = 1.0D0/(nrfact*M)

    i = 0.0D0
    do ir = 1, nrfact*M
       r1  = ir*dr     ;  r2  = (ir+1)*dr 
       pi1 = pir(r1,M) ;  pi2 = pir(r2,M)

       if ( pi1*pi2 .le. 0.0D0 ) then
          i = i+1
          g_R(i) = bisec_pir( r1, r2, M , eps )

          if ( i .eq. M/2-1 ) then
             i = i+1
             g_R(M/2) = 1.0D0
             goto 10
          endif
       end if
    enddo

    write( CNUM,'(I4)') i
    call MessageNotify('E','gauss_radau',&
         'Only '//CNUM//' Gauss-Radau points are found')

    !--- ガウス重み
10  continue
    write( CNUM,'(I4)') i
    call MessageNotify('M','gauss_radau',CNUM//' Gauss-Radau points are set up.')

    do i=1,M/2-1
       g_W(i) = 2*(2*M+gamma-5)*g_R(i)**2     &
                /((M+gamma-beta-2)*(M+gamma-3)*Phi(g_R(i),M-2,0)**2)
    enddo

    g_W(M/2) = Inm(0,0) - sum(g_W(1:M/2-1))

  end subroutine gauss_radau

  function bisec_pir( r1, r2, n, eps )
    !
    !  2 分法で π(r,n) の零点を区間 [r1,r2] の間で誤差 eps で探す
    !
    real(8)             :: r1, r2
    integer, intent(in) :: n
    real(8), intent(in) :: eps
    real(8)             :: bisec_pir
    
    real(8) :: rm, pim
    real(8) :: pi1, pi2

    pi1 = pir(r1,n)
    if ( abs(pi1) .lt. eps ) then
       bisec_pir = r1 
       return
    end if

    pi2 = pir(r2,n)
    if ( abs(pi2) .lt. eps ) then
       bisec_pir = r2
       return
    end if

10  rm = (r1+r2)/2
    pim = pir(rm,n)

    if ( abs(pim) .lt. eps ) then
       bisec_pir = rm 
       return
    end if

    if ( abs(r1-r2) .lt. eps ) then
       bisec_pir = rm 
       return
    end if
    
    if ( pim * pi1 .gt. 0.0D0 ) then
       r1 = rm
       pi1 = pim
    else
       r2 = rm
       pi2 = pim
    endif
    goto 10

  end function bisec_pir

  function pir(r,n)
    !
    ! ガウス-ラダウ格子点を定義するための関数
    !
    real(8), intent(in)  :: r                ! 動径座標 ( 0 < r < ra )
    integer, intent(in)  :: n                ! 多項式次数(0 <= n)
    real(8)              :: pir              ! 多項式の値

    pir = qnm(r,n,0)-(n+gamma-beta-2)*(n+gamma-3)*qnm(r,n-2,0)/(n*(n+beta-1))

  end function pir

  function Phi(r,n_ind,m_ind)
    !
    ! 正規化された多項式計算
    !
    real(8), intent(in)  :: r                ! 動径座標 ( 0 < r < ra )
    integer, intent(in)  :: n_ind            ! 多項式次数(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! 多項式次数(0 <= |m| <= n)
    real(8)              :: Phi              ! 多項式の値

    Phi = qnm(r,n_ind,m_ind)/sqrt(Inm(n_ind,m_ind))
  end function Phi

  function qnm(r,n_ind,m_ind)
    !
    ! 漸化式による多項式計算
    !
    real(8), intent(in)  :: r                ! 動径座標 ( 0 < r < ra )
    integer, intent(in)  :: n_ind            ! 多項式次数(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! 多項式次数(0 <= |m| <= n)
    real(8)              :: qnm              ! 多項式の値

    real(8) :: qnp2m, qnm2m
    integer :: m, n

    m = abs(m_ind)

    !--- 入力チェック
    if ( n_ind .lt. 0 ) then
       call MessageNotify('E','qnm','n must be larger equal to 0')
    endif

    if ( m .gt. n_ind ) then
       call MessageNotify('E','qnm','abs(m) must be smaller equal to n')
    endif
    
    if ( mod(n_ind+m_ind,2) .ne. 0 ) then
       call MessageNotify('E','qnm','n+m must be even')
    endif

    !--- 初期値
    if ( m .eq. n_ind ) then
       qnm = r**m
       return
    endif

    if ( n_ind .eq. m+2 ) then
       qnm = (2*m+gamma-1)/2*((2*m+gamma+1)/(2*m+beta+1)*r**2 -1) * r**m
       return
    endif

    qnm2m = r**m
    qnm = (2*m+gamma-1)/2*((2*m+gamma+1)/(2*m+beta+1)*r**2 -1)* r**m

    !--- 漸化式

    do n = m+2, n_ind-2, 2
       qnp2m =( ( 2*n + gamma - 1 ) &
                * (  ( 2*n + gamma - 3 )*( 2*n + gamma + 1) * r**2       &
                    - 2*n * (n + gamma -1) - 2*m * (m + beta -1)       &
                    -(gamma-3) * (beta+1)                            )*qnm &
               -(n-m+gamma-beta-2)*(n+m+gamma-3)*(2*n+gamma+1)*qnm2m )&
              /(( n - m + 2 )*( n + m + beta + 1 )*( 2*n + gamma - 3 ))
       qnm2m = qnm
       qnm = qnp2m
    enddo

    return
  end function qnm

  function Inm(n_ind,m_ind)
    !
    ! 多項式の規格化定数を漸化式により求める
    !
    integer, intent(in)  :: n_ind            ! 多項式次数(0 <= |m| <= n)
    integer, intent(in)  :: m_ind            ! 多項式次数(0 <= |m| <= n)
    real(8)              :: Inm              ! 規格化定数

    integer :: n,m
    real(8) :: Inm2m
    real(8) :: Inmln                         ! 規格化定数のlog
    real(8) :: gammaln
    external gammaln

    m=abs(m_ind)

    !--- 入力チェック
    if ( n_ind .lt. 0 ) then
       call MessageNotify('E','Inm','n must be larger equal to 0')
    endif

    if ( m .gt. n_ind ) then
       call MessageNotify('E','Inm','abs(m) must be smaller equal to n')
    endif
    
    if ( mod(n_ind+m_ind,2) .ne. 0 ) then
       call MessageNotify('E','Inm','n+m must be even')
    endif

    !--- 初期値
    Inmln = gammaln((gamma-beta)/2) + gammaln(m+(beta+1)/2) &
          - gammaln(m+(gamma+1)/2) - log(2.0D0)
    Inm = exp(Inmln)

    !--- 漸化式
    do n=m+2, n_ind, 2
       Inm2m = Inm
       Inm = (2*n+gamma-5)*(n-m+gamma-beta-2)*(n+m+gamma-3) &
               /((n-m)*(n+m+beta-1)*(2*n+gamma-1)) * Inm2m
    enddo

    return
  end function Inm
       
end module aq_module
