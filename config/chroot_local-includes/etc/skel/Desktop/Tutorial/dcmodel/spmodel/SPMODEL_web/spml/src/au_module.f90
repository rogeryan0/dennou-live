!--
!----------------------------------------------------------------------
!     Copyright (c) 2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  au_module
!
!      spml/au_module モジュールは 1 次元有限領域の下での流体運動を
!      2 次の引数のシフトされたチェビシェフ関数 T_k(2r^2-1)で展開する
!      スペクトル数値計算ための Fortran90 関数を提供する. 
!      このルーチンは離散化にチェビシェフ--ガウス--ラダウ格子点を適用
!      しており, 主に 2 次元極座標,  円筒座標, 球座標の原点の
!      特異性を回避しながらスペクトル計算を行うために用いることを
!      念頭においている. 
!
!      2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
!      関数も提供しており, 2, 3 次元領域での計算のベースも提供する. 
!
!
!履歴  2007/12/27  竹広真一  新規作成
!      2008/01/10  竹広真一  コメント修正
!      2009/01/09  竹広真一  au_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントをRDoc用に修正
!
!++
module au_module
  !
  != au_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: au_module.f90,v 1.4 2009-02-28 21:33:46 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/au_module モジュールは 1 次元有限領域の下での流体運動を
  ! 2 次の引数のシフトされたチェビシェフ関数 T_k(2r^2/a^2-1)で
  ! 展開するスペクトル数値計算ための Fortran90 関数を提供する. 
  ! このルーチンは離散化にチェビシェフ--ガウス--ラダウ格子点を適用
  ! しており, 主に 2 次元極座標,  円筒座標, 球座標の原点の
  ! 特異性を回避しながらスペクトル計算を行うために用いることを
  ! 念頭においている. 
  !
  ! 2 次元データの 1 次元に関して同時にスペクトル計算を実行するための
  ! 関数も提供しており, 2, 3 次元領域での計算のベースも提供する. 
  !
  ! 具体的な展開の仕方は
  !
  !         f(r)=Σf_k r^n T_k(2r^2/a^2-1)
  !
  ! である. 1 次元チェビシェフデータが複数並んだ 2 次元データを
  ! 扱う際には重みの指数 n を各データ毎に設定できる. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (u_, g_, au_, ag_) は, 返す値の形を示している.
  !   u_  :: チェビシェフデータ
  !   g_  :: 1 次元格子点データ
  !   au_ :: 1 次元チェビシェフデータが複数並んだ 2 次元データ
  !   ag_ :: 1 次元格子点データが複数並んだ 2 次元データ.
  !
  !   複数並んだ 2 次元データの第 1 次元の大きさは au_Initial で
  !   設定する重みの指数の配列 nd の大きさと同じでなければならない.
  !
  ! * 関数名の間の文字列(Dr)は, その関数の作用を表している.
  !
  ! * 関数名の最後 (_e,_au,_g, _ag) は, 入力変数の形がチェビシェフデータ
  !   および格子点データであることを示している.
  !    _u  :: チェビシェフデータ
  !    _g  :: 1 次元格子点データ
  !    _au :: 1 次元チェビシェフデータが複数並んだ 2 次元データ
  !    _ag :: 1 次元格子点データが複数並んだ 2 次元データ
  !
  !=== 各データの種類の説明
  !
  ! * g : 1 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:im). 
  !   * im は R 座標の格子点数であり, サブルーチン au_Initial にて
  !     あらかじめ設定しておく.
  !
  ! * u : チェビシェフデータ.
  !   * 変数の種類と次元は real(8), dimension(0:km). 
  !     km は R 方向の最大波数であり, サブルーチン au_Initial にて
  !     あらかじめ設定しておく. スペクトルデータの格納のされ方については...
  !
  ! * ag : 1 次元(R)格子点データの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(size(nd),0:im). 
  !     第 2 次元が R 方向を表す.
  !
  ! * au : 1 次元チェビシェフデータの並んだ 2 次元データ.
  !   * 変数の種類と次元は real(8), dimension(size(nd),0:km). 
  !     第 2 次元がスペクトルを表す.
  !
  ! * g_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * u_ で始まる関数が返す値はチェビシェフデータに同じ.
  !
  ! * ag_ で始まる関数が返す値は 1 次元格子点データの並んだ 
  !   2 次元データに同じ.
  !
  ! * au_ で始まる関数が返す値は 1 次元チェビシェフデータの並んだ
  !   2 次元データに同じ.
  !
  ! * チェビシェフデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをチェビシェフ変換したもののことである.
  !
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! au_Initial  :: チェビシェフ変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! g_R        :: 格子点座標(R)を格納した 1 次元配列
  ! g_R_Weight :: 重み座標を格納した 1 次元配列
  !
  !==== 基本変換
  !
  ! g_u, ag_au :: チェビシェフデータから格子データへの変換
  ! u_g, au_ag :: 格子データからチェビシェフデータへの変換
  !
  !==== 微分
  !
  ! g_Dr_u, ag_Dr_au   :: チェビシェフデータに R 微分を作用させる
  ! g_Dr2_u, ag_Dr2_au :: チェビシェフデータに 2 階 R 微分を作用させる
  !
  !==== 積分・平均
  !
  ! a_Int_ag, a_Avr_ag :: 1 次元格子点データの並んだ 2 次元配列の積分および平均
  ! Int_g, Avr_g       :: 1 次元格子点データの積分および平均
  !
  !==== 境界値問題
  !
  ! au_Boundary_D, au_Boundary_N         :: ディリクレ,ノイマン外側境界条件
  ! au_BoundaryTau_D, au_BoundaryTau_N   :: ディリクレ,ノイマン外側境界条件
  ! au_BoundaryGrid_D, au_BoundaryGrid_N :: ディリクレ,ノイマン外側境界条件
  !
  use dc_message
  use lumatrix
  implicit none
  private
  public g_R, g_R_Weight                        ! 座標変数
  public au_Initial                             ! 初期化
  public ag_au, au_ag, g_u, u_g                 ! 基本変換
  public ag_Dr_au, g_Dr_u                       ! 微分
  public ag_Dr2_au, g_Dr2_u                     ! 微分
  public a_Int_ag, Int_g, a_Avr_ag, Avr_g       ! 積分・平均
  public au_Boundary_D, au_Boundary_N           ! 境界条件
  public au_BoundaryTau_D, au_BoundaryTau_N     ! 境界条件
  public au_BoundaryGrid_D, au_BoundaryGrid_N   ! 境界条件

  interface au_Boundary_D
     !
     ! 外側境界ディリクレ型境界条件の適用(タウ法). 
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryTau_D_1d, au_BoundaryTau_D_2d
  end interface

  interface au_Boundary_N
     !
     ! 外側ノイマン型境界条件の適用(タウ法).
     ! i=0 で勾配の値を与える.
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(:,0:km),intent(inout)       :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(at_data,1)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: t_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryTau_N_1d, au_BoundaryTau_N_2d
  end interface

  interface au_BoundaryTau_D
     !
     ! 外側境界ディリクレ型境界条件の適用(タウ法). 
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryTau_D_1d, au_BoundaryTau_D_2d
  end interface

  interface au_BoundaryTau_N
     !
     ! 外側ノイマン型境界条件の適用(タウ法).
     ! i=0 で勾配の値を与える.
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: t_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryTau_N_1d, au_BoundaryTau_N_2d
  end interface

  interface au_BoundaryGrid_D
     !
     ! 外側ディリクレ型境界条件の適用(実空間での評価).
     ! 外側境界(i=0)での値を与える.
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                 :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryGrid_D_1d, au_BoundaryGrid_D_2d
  end interface

  interface au_BoundaryGrid_N
     !
     ! 外側境界ノイマン型境界条件の適用(実空間での評価).
     ! i=0 で勾配の値を与える.
     !
     !  * 境界条件を適用する配列の次元によって内部でサブルーチンを
     !    使い分けている. ユーザーインターフェースは共通であるので
     !    下部ルーチンを呼ぶ必要はない.
     !
     ! 引数と結果の型
     !
     !  * 1 次元チェビシェフデータの並んだ 2 次元配列の場合
     !
     !    real(8), dimension(size(nd),0:km),intent(inout)    :: au_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), dimension(size(nd)), intent(in), optional :: value
     !    !(in) 適用する境界値
     !
     !  * 1 次元チェビシェフデータの場合
     !
     !    real(8), dimension(0:km),intent(inout)       :: u_data
     !    !(inout) 境界条件を適用するチェビシェフデータ
     !
     !    real(8), intent(in), optional                :: value
     !    !(in) 適用する境界値
     !
     module procedure au_BoundaryGrid_N_1d, au_BoundaryGrid_N_2d
  end interface

  real(8), parameter                 :: pi=3.1415926535897932385D0

  integer :: im, km                        ! 格子点数, 切断波数
  real(8) :: ra                            ! 領域の大きさ

  integer, allocatable :: nd(:)
  ! r^n の指数

  real(8), allocatable :: g_R(:)
  ! 格子点座標
  ! Chebyshev-Gauss-Radau 格子点

  real(8), allocatable :: g_R_Weight(:)
  ! 格子点重み座標
  ! 各格子点における積分のための重みが格納してある

  real(8), allocatable :: CF(:,:), CB(:,:)   ! 正変換・逆変換用行列

  save :: im, km, ra, CF, CB, g_R, g_R_Weight, nd

contains

! ---- 初期化 ---- 
  subroutine au_Initial(i_in,k_in,r_in,nd_in)
    !
    ! チェビシェフ変換の格子点数, 波数, 領域の大きさ, 重みを設定する.
    ! 
    ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
    ! 初期設定をしなければならない.
    !
    integer,intent(in) :: i_in              !(in) 格子点数
    integer,intent(in) :: k_in              !(in) 切断波数
    real(8),intent(in) :: r_in              !(in) 外側境界の座標(半径)
    integer,intent(in) :: nd_in(:)          !(in) 重み r^n の指数

    real(8), allocatable :: c(:), w(:)   ! 重み関数
    real(8) :: t
    integer :: ii,kk

    im=i_in ; km=k_in ; ra = r_in

    if ( km .gt. im ) then
       call MessageNotify('E','au_initial','KM shoud be less equal to IM')
    endif

    if ( allocated(nd) ) deallocate(nd)
    allocate(nd(size(nd_in)))
    nd = nd_in

    if ( allocated(CF) ) deallocate(CF)
    if ( allocated(CB) ) deallocate(CB)
    allocate(CF(0:im,0:im),CB(0:im,0:im))

    if ( allocated(C) ) deallocate(W)
    if ( allocated(W) ) deallocate(W)
    allocate(c(0:im),w(0:im))

    c = 1.0D0 ; c(0) = 2.0D0
    w = 2*pi/(2*im+1) ; w(0) = pi/(2*im+1)

    do ii=0,im
       do kk=0,im
          CF(kk,ii)=2.0D0/(pi*c(kk))*cos(2*pi*ii*kk/(2*im+1))*w(ii)
       enddo
    enddo

    do kk=0,im
       do ii=0,im
          CB(ii,kk)=cos(2*pi*ii*kk/(2*im+1))
       enddo
    enddo

    if ( allocated(g_R) ) deallocate(g_R)
    allocate(g_R(0:im))

    do ii=0,im
       t = 2*pi*ii/(2*im+1)
       g_R(ii) = ra*sqrt((1+cos(t))/2)    ! cos(t) = 2(r/a)^2 - 1
    enddo

    if ( allocated(g_R_Weight) ) deallocate(g_R_Weight)
    allocate(g_R_Weight(0:im))
    do ii=0,im
       g_R_Weight(ii) = 0.0
       do kk=0,km
          g_R_Weight(ii) = g_R_Weight(ii) &
               + 1.0D0/(1.0D0/4.0D0-kk**2) &
               * cos(2*kk*ii*pi/(2*im+1))/c(kk)
       enddo
       g_R_Weight(ii) = ra/(2*pi) * g_R_Weight(ii)* w(ii)
    enddo

    deallocate(c,w)

    call MessageNotify('M','au_initial','au_module (2009/01/09) is initialized')
  end subroutine au_Initial

! ---- 逆変換 ---- 
  function ag_au(au_data)
    !
    ! チェビシェフデータから格子データへ変換する(2 次元配列用).
    !
    double precision, dimension(:,:), intent(in)       :: au_data
    !(in) チェビシェフデータ

    double precision, dimension(size(au_data,1),0:im)  :: ag_au
    !(out) 格子点データ

    double precision, dimension(size(au_data,1),0:im)  :: au_work
    ! 作業用配列

    integer :: m, mm, i

    mm = size(au_data,1)
    if ( mm /= size(nd) ) then
       call MessageNotify('E','ag_au', &
            '1st dim. of the Chebyshev data should be same as dim. of ND.')
    end if

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','ag_au', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','ag_au', &
            'The Chebyshev dimension of input data too large.')
    endif

    au_work = 0.0 ; au_work(:,0:km) = au_data
    do m=1,mm
       do i=0,im
          ag_au(m,i) = sum(CB(i,:)*au_work(m,:))*g_R(i)**nd(m)
       enddo
    enddo

  end function ag_au

  function g_u(u_data)
    !
    ! チェビシェフデータから格子データへ変換する(1 次元配列用).
    !
    double precision, dimension(:), intent(in)  :: u_data
    !(in) チェビシェフデータ

    double precision, dimension(0:im)           :: g_u
    !(out) 格子点データ

    double precision, dimension(1,size(u_data)) :: u_work
    ! 作業用配列
    double precision, dimension(1,0:im)         :: g_work
    ! 作業用配列

    u_work(1,:) = u_data  
    g_work = ag_au(u_work)
    g_u = g_work(1,:)

  end function g_u

! ---- 正変換 ---- 
  function au_ag(ag_data)
    !
    ! 格子データからチェビシェフデータへ変換する(2 次元配列用).
    !
    double precision, dimension(:,:), intent(in)      :: ag_data
    !(in) 格子点データ

    double precision, dimension(size(ag_data,1),0:km) :: au_ag
    !(out) チェビシェフデータ

    double precision, dimension(size(ag_data,1),0:im) :: au_work
    !作業用配列

    integer :: m, mm, k

    mm = size(ag_data,1)
    if ( mm /= size(nd) ) then
       call MessageNotify('E','ag_au', &
            '1st dim. of the Chebyshev data should be same as dim. of ND.')
    end if

    if ( size(ag_data,2)-1 < im ) then
       call MessageNotify('E','au_ag', &
            'The Grid points of input data too small.')
    elseif ( size(ag_data,2)-1 > im ) then
       call MessageNotify('W','au_ag', &
            'The Grid points of input data too large.')
    endif

    do m=1,mm
       do k=0,im
          au_work(m,k) = sum(CF(k,:)*ag_data(m,:)/g_R**nd(m))
       enddo
    enddo
    au_ag = au_work(:,0:km)

  end function au_ag

  function u_g(g_data)  ! 格子 -> スペクトル
    !
    ! 格子データからチェビシェフデータへ変換する(1 次元配列用).
    !
    double precision, dimension(:), intent(in)     :: g_data
    !(in) 格子点データ

    double precision, dimension(0:km)              :: u_g
    !(out) チェビシェフデータ

    real(8), dimension(1,size(g_data)) :: ag_work
    real(8), dimension(1,0:km)         :: au_work

    ag_work(1,:) = g_data
    au_work = au_ag(ag_work)
    u_g = au_work(1,:)
    
  end function u_g

! ---- 微分計算 ---- 
  function au_Dx_au(au_data)
    !
    ! 入力チェビシェフデータ f_k T_k(x) に対して x 微分のチェビシェフ係数
    ! g_k T_k(x) = f_k T_k'(x) を計算する(2 次元配列用).
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(size(au_data,1),0:size(au_data,2)-1) :: au_Dx_au
    !(in) 出力チェビシェフデータ

    integer :: m, k
    integer :: nm, kmax

    nm=size(au_data,1)
    kmax=size(au_data,2)-1
    if ( kmax  < km ) then
       call MessageNotify('W','au_Dx_au', &
            'The Chebyshev dimension of input data too small.')
    elseif ( kmax > km ) then
       call MessageNotify('E','au_Dx_au', &
            'The Chebyshev dimension of input data too large.')
    endif

    do m=1,nm
       au_Dx_au(m,kmax)   = 0. 
       au_Dx_au(m,kmax-1) = 2 * kmax * au_data(m,kmax) 
    enddo

    do k=kmax-2,1,-1
       do m=1,nm
          au_Dx_au(m,k) = au_Dx_au(m,k+2) + 2*(k+1)*au_data(m,k+1)
       enddo
    enddo

    do m=1,nm
       au_Dx_au(m,0) = (au_Dx_au(m,2) + 2*au_data(m,1))/2.0D0
    enddo

  end function au_Dx_au

  function ag_Dr_au(au_data)
    !
    ! 入力チェビシェフデータに R 微分を作用する(2 次元配列用).
    !
    ! チェビシェフデータの R 微分とは, 対応する格子点データに R 微分を
    ! 作用させたデータのチェビシェフ変換のことである.
    !
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(size(au_data,1),0:im) :: ag_Dr_au
    !(out) チェビシェフデータの R 微分

    integer :: nm

    nm=size(au_data,1)

    ag_Dr_au = ag_au(au_Dx_au(au_data)) * 4*spread(g_R,1,nm)/ra**2&
             + spread(nd,2,im+1)/spread(g_R,1,nm) * ag_au(au_data)
  end function ag_Dr_au

  function g_Dr_u(u_data)
    !
    ! 入力チェビシェフデータに R 微分を作用する(1 次元配列用).
    !
    ! チェビシェフデータの R 微分とは, 対応する格子点データに R 微分を
    ! 作用させたデータのチェビシェフ変換のことである.
    !
    !
    real(8), dimension(:), intent(in)   :: u_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(0:im)            :: g_Dr_u
    !(out) チェビシェフデータの R 微分

    real(8), dimension(1,size(u_data))  :: au_work
    ! 作業用配列

    real(8), dimension(1,0:im)          :: ag_work
    ! 作業用配列
    
    au_work(1,:) = u_data
    ag_work = ag_Dr_au(au_work)
    g_Dr_u = ag_work(1,:)

  end function g_Dr_u

  function ag_Dr2_au(au_data)
    !
    ! 入力チェビシェフデータに 2 階 R 微分を作用する(2 次元配列用).
    !
    ! チェビシェフデータの 2 階 R 微分とは, 対応する格子点データに
    ! 2 階 R 微分を作用させたデータのチェビシェフ変換のことである.
    !
    !
    real(8), dimension(:,0:), intent(in)     :: au_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(size(au_data,1),0:im) :: ag_Dr2_au
    !(out) チェビシェフデータの 2 階 R 微分

    real(8), dimension(size(au_data,1),0:size(au_data,2)-1)  :: au_g
    real(8), dimension(size(au_data,1),0:size(au_data,2)-1)  :: au_h
    ! 作業配列

    integer :: nm

    nm=size(au_data,1)

    au_g = au_Dx_au(au_data)
    au_h = au_Dx_au(au_g)

    ag_Dr2_au = 16*spread(g_R,1,nm)**2/ra**4 * ag_au(au_h)  &
              + 4*spread(2*nd+1,2,im+1)/ra**2 * ag_au(au_g) &
              + spread(nd*(nd-1),2,im+1)/spread(g_R**2,1,nm) * ag_au(au_data)
  end function ag_Dr2_au

  function g_Dr2_u(u_data)
    !
    ! 入力チェビシェフデータに 2 階 R 微分を作用する(1 次元配列用).
    !
    ! チェビシェフデータの 2 階 R 微分とは, 対応する格子点データに 
    ! 2 階 R 微分を作用させたデータのチェビシェフ変換のことである.
    !
    !
    real(8), dimension(:), intent(in)   :: u_data
    !(in) 入力チェビシェフデータ

    real(8), dimension(0:im)            :: g_Dr2_u
    !(out) チェビシェフデータの 2 階 R 微分

    real(8), dimension(1,size(u_data))  :: au_work
    ! 作業用配列

    real(8), dimension(1,0:im)          :: ag_work
    ! 作業用配列
    
    au_work(1,:) = u_data
    ag_work = ag_Dr2_au(au_work)
    g_Dr2_u = ag_work(1,:)

  end function g_Dr2_u

  !--------------- 積分計算 -----------------
    function a_Int_ag(ag)
      !
      ! 1 次元格子点データが並んだ 2 次元配列の積分
      !
      real(8), dimension(:,0:), intent(in)     :: ag
      !(in)入力格子点データ

      real(8), dimension(size(ag,1))           :: a_Int_ag
      !(out) 積分したデータ
      integer :: i

      if ( size(ag,2) < im+1 ) then
         call MessageNotify('E','a_Int_ag', &
              'The Grid points of input data too small.')
      elseif ( size(ag,2) > im+1 ) then
         call MessageNotify('W','a_Int_ag', &
              'The Grid points of input data too large.')
      endif

      a_Int_ag = 0.0d0
      do i=0,im
         a_Int_ag(:) = a_Int_ag(:) + ag(:,i)*g_R_Weight(i)
      enddo
    end function a_Int_ag

    function Int_g(g)
      !
      ! 1 次元格子点データの積分および平均.
      !
      real(8), dimension(0:im), intent(in)   :: g
      !(in) 格子点データ

      real(8)                                :: Int_g
      !(out) 積分値

      Int_g = sum(g*g_R_Weight)
    end function Int_g

    function a_Avr_ag(ag)
      !
      ! 1 次元格子点データが並んだ 2 次元配列の平均
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
      real(8), dimension(0:im), intent(in)   :: g
      !(in) 格子点データ

      real(8)                                :: Avr_g
      !(out) 積分値

      Avr_g = Int_g(g)/sum(g_R_Weight)
    end function Avr_g

!---- Dirichlet 型境界条件(タウ法) ----

  subroutine au_BoundaryTau_D_2d(au_data,value)
    !
    ! Dirichlet 型境界条件の適用(タウ法, 2 次元配列用)
    ! 外側境界(i=0)での値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data ! データ(m,0:km)
    !(inout) 境界条件を適用するチェビシェフデータ(m,0:km)

    real(8), dimension(:), intent(in), optional  :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))   :: value0           ! 境界値

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryTau_D', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryTau_D', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.

       allocate(alu(size(nd),0:km,0:km),kp(size(nd),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do k=0,km
          au_work = 0.0
          au_work(:,k) = 1.0
          ag_work = ag_au(au_work)
          alu(:,km,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    au_data(:,km)   = value0
    au_data = lusolve(alu,kp,au_data)

  end subroutine au_BoundaryTau_D_2d

  subroutine au_BoundaryTau_D_1d(u_data,value)
    !
    ! Dirichlet 型境界条件の適用(タウ法, 1 次元配列用)
    ! 両境界での値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryTau_D_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryTau_D_1d

!---- Neumann 型境界条件(タウ法) ----

  subroutine au_BoundaryTau_N_2d(au_data,value)
    !
    ! 外側境界 Neumann 型境界条件の適用(タウ法, 2 次元配列用)
    ! i=0 で勾配の値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) 境界条件を適用するチェビシェフデータ(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable  :: alu
    integer, dimension(:,:), allocatable    :: kp
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))   :: value0           ! 境界値

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryTau_DN', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryTau_DN', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:km,0:km),kp(size(nd),0:km))

       alu=0.0D0
       do k=0,km
          alu(:,k,k) = 1.0D0
       enddo

       do k=0,km
          au_work = 0.0
          au_work(:,k) = 1.0
          ag_work = ag_Dr_au(au_work)
          alu(:,km,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    au_data(:,km)   = value0
    au_data = lusolve(alu,kp,au_data)

  end subroutine au_BoundaryTau_N_2d

  subroutine au_BoundaryTau_N_1d(u_data,value)
    !
    ! Dirichlet/Neumann 型境界条件の適用(タウ法, 1 次元配列用)
    ! i=0 で勾配の値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryTau_N_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryTau_N_1d

!---- Dirichlet 型境界条件(実空間での評価) ----

  subroutine au_BoundaryGrid_D_2d(au_data,value)
    !
    ! Dirichlet 型境界条件の適用(実空間での評価, 2 次元配列用)
    ! 外側境界での値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) 境界条件を適用するチェビシェフデータ(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable     :: alu
    integer, dimension(:,:), allocatable       :: kp
    real(8), dimension(size(au_data,1),0:im)   :: ag_data
    real(8), dimension(size(nd),0:km)          :: au_work
    real(8), dimension(size(au_data,1))        :: value0 ! 境界値

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( im /= km ) then
       call MessageNotify('E','au_BoundaryGrid_DD', &
            'Chebyshev truncation and number of grid points should be same.')
    endif

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryGrid_DD', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryGrid_DD', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0=value   
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:im,0:km),kp(size(nd),0:im))

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          alu(:,:,k) = ag_au(au_work)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data = ag_au(au_data)
    ag_data(:,0)  = value0
    au_data = lusolve(alu,kp,ag_data)

  end subroutine au_BoundaryGrid_D_2d

  subroutine au_BoundaryGrid_D_1d(u_data,value)
    !
    ! Dirichlet 型境界条件の適用(実空間での評価, 1 次元配列用)
    ! 外側境界での値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryGrid_D_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryGrid_D_1d

!---- Neumann 型境界条件(実空間での評価) ----

  subroutine au_BoundaryGrid_N_2d(au_data,value)
    !
    ! 外側境界 Neumann 型境界条件の適用(実空間での評価, 2 次元配列用)
    ! i=0 で勾配の値を与える.
    !
    real(8), dimension(:,0:),intent(inout)         :: au_data
    !(inout) 境界条件を適用するチェビシェフデータ(m,0:km)

    real(8), dimension(:), intent(in), optional    :: value
    !(in) 境界値(m)

    real(8), dimension(:,:,:), allocatable   :: alu
    integer, dimension(:,:), allocatable     :: kp
    real(8), dimension(size(au_data,1),0:im) :: ag_data
    real(8), dimension(size(nd),0:km)        :: au_work
    real(8), dimension(size(nd),0:im)        :: ag_work
    real(8), dimension(size(au_data,1))      :: value0   ! 境界値

    logical :: first = .true.
    integer :: k
    save    :: alu, kp, first

    if ( im /= km ) then
       call MessageNotify('E','au_BoundaryGrid_N', &
            'Chebyshev truncation and number of grid points should be same.')
    endif

    if ( size(au_data,2)-1 < km ) then
       call MessageNotify('E','au_BoundaryGrid_N', &
            'The Chebyshev dimension of input data too small.')
    elseif ( size(au_data,2)-1 > km ) then
       call MessageNotify('W','au_BoundaryGrid_DN', &
            'The Chebyshev dimension of input data too large.')
    endif

    if (.not. present(value)) then
       value0=0
    else
       value0 = value
    endif

    if ( first ) then
       first = .false.
       allocate(alu(size(nd),0:im,0:km),kp(size(nd),0:im))

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          ag_work = ag_au(au_work)
          alu(:,:,k) = ag_work
       enddo

       do k=0,km
          au_work = 0
          au_work(:,k)=1.0
          ag_work = ag_Dr_au(au_work)
          alu(:,0,k) = ag_work(:,0)
       enddo

       call ludecomp(alu,kp)
    endif

    ag_data = ag_au(au_data)
    ag_data(:,0)  = value0
    au_data = lusolve(alu,kp,ag_data)

  end subroutine au_BoundaryGrid_N_2d

  subroutine au_BoundaryGrid_N_1d(u_data,value)
    !
    ! 外側境界 Neumann 型境界条件の適用(実空間での評価, 1 次元配列用)
    ! i=0 で勾配の値を与える.
    !
    real(8), dimension(0:km),intent(inout)       :: u_data
    !(inout) 境界条件を適用するチェビシェフデータ(0:km)

    real(8), intent(in), optional                :: value
    !(in) 境界値

    real(8), dimension(1,0:km)                   :: au_work
    real(8), dimension(1)                        :: vwork           ! 境界値

    if (.not. present(value)) then
       vwork(1)=0
    else
       vwork(1) = value
    endif

    au_work(1,:)=u_data
    call au_BoundaryGrid_N_2d(au_work,vwork)
    u_data=au_work(1,:)

  end subroutine au_BoundaryGrid_N_1d

end module au_module

