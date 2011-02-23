!--
!----------------------------------------------------------------------
! Copyright(c) 2008-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eq_module
!      2 次元円盤領域問題, Fourier 展開 + 多項式展開法
!
!      spml/eq_module モジュールは 2 次元円盤領域での流体運動を
!      スペクトル法により数値計算を実行するための Fortran90 関数を提供する. 
!      周期的な境界条件を扱うための方位角方向へのフーリエ変換と
!      境界壁を扱うための動径方向の多項式変換を用いる場合の
!      スペクトル計算のためのさまざまな関数を提供する. 
!
!      内部で ae_module, aq_module を用いている. 
!      最下部ではフーリエ変換およびチェビシェフ変換のエンジンとして 
!      ISPACK/FTPACK の Fortran77 サブルーチンを用いている.
!
!      Matsushima and Marcus (1994) の多項式に関する説明は 
!      doc/spectral_radial.tex を参照のこと. 
!
!
!履歴  2008/04/11  竹広真一  et_module より改造
!      2008/05/02  竹広真一  コメント修正
!      2008/10/29  竹広真一  eq_Vor2Strm_eq 行列生成ルーチン改良
!      2009/01/09  竹広真一  eq_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に修正
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!++
module eq_module
  !
  != eq_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eq_module.f90,v 1.8 2010-03-02 10:09:02 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/eq_module モジュールは 2 次元円盤領域での流体運動を
  ! スペクトル法により数値計算を実行するための Fortran90 関数を提供する. 
  ! 周期的な境界条件を扱うための方位角方向へのフーリエ変換と
  ! 境界壁を扱うための動径方向の多項式変換を用いる場合の
  ! スペクトル計算のためのさまざまな関数を提供する. 
  !
  ! 内部で ae_module, aq_module を用いている. 
  ! 最下部ではフーリエ変換およびチェビシェフ変換のエンジンとして 
  ! ISPACK/FTPACK の Fortran77 サブルーチンを用いている.
  !
  ! Matsushima and Marcus (1994) の多項式に関する説明は 
  ! 動径座標のスペクトル法(spectral_radial.pdf[link:./spectral_radial.pdf])を
  ! 参照のこと. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (eq_, rp_, r_, p_) は, 返す値の形を示している.
  !   eq_ :: 2次元スペクトルデータ
  !   rp_ :: 2 次元格子点データ
  !   r_  :: 動径方向 1 次元格子点データ
  !   p_  :: 方位角方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dr, Dp, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_eq_eq,_eq,_rp, _r, _p) は, 入力変数のスペクトルデータ
  !   および格子点データであることを示している.
  !   _eq    :: 2次元スペクトルデータ
  !   _eq_eq :: 2 つの2次元スペクトルデータ
  !   _rp    :: 2 次元格子点データ
  !   _r     :: 動径方向 1 次元格子点データ
  !   _p     :: 方位角方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * rp : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(jm,0:im-1). 
  !   * im, jm はそれぞれ方位角, 動径座標の格子点数であり, 
  !     サブルーチン eq_initial にてあらかじめ設定しておく.
  !   * 第 1 次元が動径座標の格子点位置番号, 第 2 次元が方位角座標の
  !     格子点位置番号である. 
  !
  ! * eq : 2 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km,0:lm). 
  !   * km, lm はそれぞれ方位角, 動径方向の最大波数であり, 
  !     サブルーチン eq_initial にてあらかじめ設定しておく. 
  !   * 動径スペクトルデータの格納のされ方については 
  !     aq_module.f90 を参照のこと.
  !
  ! * p, r : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1)
  !     および real(8), dimension(jm).
  !
  ! * e, q : 1 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km) 
  !     および real(8), dimension(0:lm).
  !
  ! * ap, ar : 1 次元格子点データの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,0:im-1) 
  !     および real(8), dimension(:,jm).
  !
  ! * ae, aq : 1 次元スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,-km:km) 
  !     および real(8), dimension(:,0:lm).
  !
  ! * eq_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * rp_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * p_, p_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! eq_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! p_Phi, r_Rad               ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! p_Phi_Weight, r_Rad_Weight ::  重み座標を格納した 1 次元配列
  ! rp_Phi, rp_Rad             ::  格子点データの XY 座標(X,Y)
  !                                (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! rp_eq :: スペクトルデータから格子データへの変換
  ! eq_rp :: 格子データからスペクトルデータへの変換
  ! ap_ae, p_e :: 方位角方向のスペクトルデータから格子データへの変換
  ! ar_aq, r_q :: 動径方向のスペクトルデータから格子データへの変換
  ! ae_ap, e_p :: 方位角方向の格子点データからスペクトルデータへの変換
  ! aq_ar, q_r :: 動径方向の格子点データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! eq_Lapla_eq  :: スペクトルデータにラプラシアンを作用させる
  ! eq_DPhi_eq, ae_DPhi_ae, e_DPhi_e :: スペクトルデータに
  !                                     方位角微分を作用させる
  ! eq_RadDRad_eq, aq_RadDRad_aq, q_RadDRad_q :: スペクトルデータに
  !                                              動径微分を作用させる
  ! eq_Jacobian_eq_eq :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !==== 境界値問題
  !
  ! eq_Boundary    :: ディリクレ, ノイマン境界条件の適用
  ! eq_LaplaInv_eq :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! eq_Vor2Strm_eq :: 渦度から流線を計算する
  !
  !==== 積分・平均
  !
  ! IntRadPhi_rp, AvrRadPhi_rp   :: 2 次元格子点データの全領域積分および平均
  ! r_IntPhi_rp, r_AvrPhi_rp :: 2 次元格子点データの方位角方向積分および平均
  ! IntPhi_p, AvrPhi_p       :: 1 次元(X)格子点データの方位角方向積分および平均
  ! p_IntRad_rp, p_AvrRad_rp :: 2 次元格子点データの動径方向積分および平均
  ! IntRad_r, AvrRad_r       :: 1 次元(Y)格子点データの動径方向積分および平均
  !
  use dc_message
  use lumatrix
  use ae_module, p_Phi => g_X, p_Phi_weight => g_X_Weight, &
                 e_p => e_g, ae_ap => ae_ag, &
                 p_e => g_e, ap_ae => ag_ae, &
                 ae_DPhi_ae => ae_Dx_ae, e_DPhi_e => e_Dx_e
  use aq_module, r_Rad => g_R, r_Rad_Weight => g_R_Weight, &
                 aq_ar => aq_ag, q_r => q_g, &
                 ar_aq => ag_aq, r_q => g_q, &
                 q_RadDRad_q => q_rDr_q, aq_RadDRad_aq => aq_rDr_aq

  implicit none
  private

  public eq_Initial                                       ! 初期化

  public p_Phi, p_Phi_Weight, rp_Phi                      ! 座標変数
  public r_Rad, r_Rad_Weight, rp_Rad, er_Rad              ! 座標変数

  public rp_eq, eq_rp                                     ! 基本変換
  public er_eq, eq_er                                     ! 基本変換
  public er_rp, rp_er                                     ! 基本変換
  public e_p, p_e, ae_ap, ap_ae                           ! 基本変換
  public q_r, r_q, aq_ar, ar_aq                           ! 基本変換

  public eq_DPhi_eq, e_DPhi_e, ae_DPhi_ae                 ! 微分
  public eq_RadDRad_eq, q_RadDRad_q, aq_RadDRad_aq        ! 微分
  public er_Lapla_eq, eq_Lapla_eq                         ! 微分

  public eq_Jacobian_eq_eq                                ! 非線形計算

  public eq_Boundary                                      ! 境界値問題
  public aq_Boundary_D, aq_Boundary_N                     ! 境界値問題
  public eq_LaplaInv_eq, eq_Vor2Strm_eq                   ! 境界値問題

  public IntRadPhi_rp, r_IntPhi_rp, p_IntRad_rp, IntPhi_p, IntRad_r   ! 積分
  public AvrRadPhi_rp, r_AvrPhi_rp, p_AvrRad_rp, AvrPhi_p, AvrRad_r   ! 平均

  integer            :: im=32, jm=8      ! 格子点の設定(Phi,Rad)
  integer            :: km=10, lm=5      ! 切断波数の設定(Phi,Rad)
  real(8)            :: ra=1.0           ! 領域の大きさ
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), parameter :: alpha = 1.0D0        ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta  = 1.0D0        ! 展開多項式パラメター  0 < β

  real(8), dimension(:,:), allocatable :: rp_Phi, rp_Rad
  real(8), dimension(:,:), allocatable :: er_Rad
  integer, dimension(:), allocatable   :: md

  save im, jm, km, lm, ra, md

  contains
  !--------------- 初期化 -----------------
    subroutine eq_Initial(i,j,k,l,ra_in)
      !
      ! スペクトル変換の格子点数, 波数, 領域の大きさを設定する.
      !
      ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      integer,intent(in) :: i           ! 格子点数(X)
      integer,intent(in) :: j           ! 格子点数(Y)
      integer,intent(in) :: k           ! 切断波数(X)
      integer,intent(in) :: l           ! 切断波数(Y)

      real(8),intent(in) :: ra_in       ! 半径

      integer :: kk

      im = i       ; jm = j
      km = k       ; lm = l
      ra = ra_in

      allocate(md(-km:km))

      do kk=-km,km
         md(kk) = abs(kk)
      enddo

      call ae_initial(im,km,0.0D0,2*pi)
      call aq_Initial(jm,lm,ra,alpha,beta,md)

      allocate(rp_Phi(jm,0:im-1),rp_Rad(jm,0:im-1))
      rp_Phi = spread(p_Phi,1,jm)
      rp_Rad = spread(r_Rad,2,im)

      allocate(er_Rad(-km:km,jm))
      er_Rad = spread(r_Rad,1,2*km+1)

      call MessageNotify('M','eq_initial','eq_module (2009/07/31) is initialized')
    end subroutine eq_initial

  !--------------- 基本変換 -----------------

    function rp_eq(eq)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(jm,0:im-1)                :: rp_eq
      !(out) 格子点データ

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) スペクトルデータ

      rp_eq = ap_ae(transpose(ar_aq(eq)))

    end function rp_eq

    function eq_rp(rp)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,0:lm)              :: eq_rp
      !(out) スペクトルデータ

      real(8), dimension(jm,0:im-1), intent(in)    :: rp
      !(in) 格子点データ

      eq_rp = aq_ar(transpose(ae_ap(rp)))

    end function eq_rp

    function eq_er(er)
      !
      ! 動径格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,0:lm)              :: eq_er
      !(out) スペクトルデータ

      real(8), dimension(-km:km,jm), intent(in)    :: er
      !(in) 格子点データ

      eq_er = aq_ar(er)

    end function eq_er

    function er_eq(eq)
      !
      ! 動径格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,jm)                :: er_eq
      !(out) 格子点データ

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(out) スペクトルデータ

      er_eq = ar_aq(eq)

    end function er_eq

    function rp_er(er)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(jm,0:im-1)              :: rp_er
      !(out) 格子点データ

      real(8), dimension(-km:km,jm), intent(in)  :: er
      !(in) スペクトルデータ

      rp_er = ap_ae(transpose(er))

    end function rp_er

    function er_rp(rp)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,jm)                :: er_rp
      !(out) スペクトルデータ

      real(8), dimension(jm,0:im-1), intent(in)    :: rp
      !(in) 格子点データ

      er_rp = transpose(ae_ap(rp))

    end function er_rp

  !--------------- 微分計算 -----------------

    function eq_DPhi_eq(eq)
      !
      ! 入力スペクトルデータに方位角微分(∂φ)を作用する.
      !
      ! スペクトルデータのφ微分とは, 対応する格子点データにφ微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,0:lm)                :: eq_DPhi_eq
      real(8), dimension(-km:km,0:lm), intent(in)    :: eq
      integer k

      do k=-km,km
         eq_DPhi_eq(k,:)  =  -k*eq(-k,:)
      enddo
    end function eq_DPhi_eq

    function eq_RadDRad_eq(eq)
      !
      ! 入力スペクトルデータに動径微分(r∂r)を作用する.
      !
      ! スペクトルデータの動径微分とは, 対応する格子点データに動径微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm)               :: eq_RadDRad_eq
      !(out) スペクトルデータの動径微分

      real(8), dimension(-km:km,0:lm), intent(in)   :: eq
      !(in) 入力スペクトルデータ

      eq_RadDRad_eq = aq_RadDRad_aq(eq)

    end function eq_RadDRad_eq

    function er_Lapla_eq(eq)
      !
      ! 入力スペクトルデータにラプラシアン 
      !  (1/r)(∂r(r∂r)+ (1/r^2) ∂φφ を作用する.
      !
      real(8), dimension(-km:km,jm)                :: er_Lapla_eq
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) 入力スペクトルデータ

      real(8), dimension(-km:km,0:lm)              :: eq_work

      integer k

      do k=-km,km
         eq_work(k,:) = -k**2 * eq(k,:)
      enddo

      er_Lapla_eq = er_eq(eq_work + eq_RadDRad_eq(eq_RadDRad_eq(eq)))/er_Rad**2

    end function er_Lapla_eq

    function eq_Lapla_eq(eq)
      !
      ! 入力スペクトルデータにラプラシアン 
      !  (1/r)(∂r(r∂r)+ (1/r^2) ∂φφ を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 
      ! 対応する格子点データにラプラシアンを作用させたデータの
      ! スペクトル変換のことである. 
      !
      real(8), dimension(-km:km,0:lm)              :: eq_Lapla_eq
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-km:km,0:lm), intent(in)  :: eq
      !(in) 入力スペクトルデータ

      eq_Lapla_eq = eq_er(er_Lapla_eq(eq))

    end function eq_Lapla_eq

    function eq_Jacobian_eq_eq(eq_a,eq_b)
      !
      !  2 つのスペクトルデータからヤコビアン
      !
      !     J(A,B)=1/r[(∂rA)(∂φB)-(∂φA)(∂rB)]
      !
      !  を計算する. 1/r のファクターがついていることに注意. 
      !
      !  2 つのスペクトルデータのヤコビアンとは, 対応する 2 つの
      !  格子点データのヤコビアンのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm)                :: eq_Jacobian_eq_eq
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-km:km,0:lm), intent(in)    :: eq_a
      !(in) 1つ目の入力スペクトルデータ

      real(8), dimension(-km:km,0:lm), intent(in)    :: eq_b
      !(in) 2つ目の入力スペクトルデータ

      eq_Jacobian_eq_eq = eq_rp(&
           (  rp_eq(eq_RadDRad_eq(eq_a)) * rp_eq(eq_DPhi_eq(eq_b))   &
             -rp_eq(eq_DPhi_eq(eq_a)) * rp_eq(eq_RadDRad_eq(eq_b)) ) &
           /rp_Rad**2)

    end function eq_Jacobian_eq_eq


  !--------------- 境界値問題 -----------------

    subroutine eq_Boundary(eq,value,cond)
      !
      ! ディリクレ, ノイマン条件の適用. チェビシェフ空間での計算
      !
      ! 実際には中で呼ばれている aq_module のサブルーチン 
      ! aq_Boundary_D,, aq_Boundary_N を用いている. 
      ! これらを直接呼ぶことも出来る.
      !
      real(8), dimension(-km:km,0:lm),intent(inout)      :: eq
              ! 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(-km:km), intent(in), optional   :: value
              ! 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              ! 省略時は値/勾配 0 となる. 

      character(len=1), intent(in), optional             :: cond
              ! 境界条件. 省略時は 'D'
              !   D : 両端ディリクレ
              !   N : 両端ノイマン

      if (.not. present(cond)) then
         if (present(value)) then
            call aq_Boundary_D(eq,value)
         else
            call aq_Boundary_D(eq)
         endif
         return
      endif

      select case(cond)
      case ('N')
         if (present(value)) then
            call aq_Boundary_N(eq,value)
         else
            call aq_Boundary_N(eq)
         endif
      case ('D')
         if (present(value)) then
            call aq_Boundary_D(eq,value)
         else
            call aq_Boundary_D(eq)
         endif
      case default
         call MessageNotify('E','eq_Boundaries','B.C. not supported')
      end select

    end subroutine eq_Boundary

    function eq_LaplaInv_eq(eq,value)
      !
      ! 境界で値を与える条件(ディリクレ条件)下で, 
      ! 入力スペクトルデータに逆ラプラシアン
      ! [(1/r)(∂r(r∂r)+ (1/r^2) ∂φφ]^{-1} を作用する.
      !
      ! タウ法による計算
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: eq
      !(in) スペクトルデータ

      real(8), dimension(-km:km,0:lm)             :: eq_LaplaInv_eq
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-km:km), intent(in), optional :: value
      !(in) 境界値. 省略時は 0 が設定される. 

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: eq_work
      real(8), dimension(-km:km,jm)           :: er_work
      real(8), dimension(-km:km)              :: value1       ! 境界値

      logical :: first = .true.
      integer :: k, l
      save    :: alu, kp, first

      if (.not. present(value)) then
         value1=0
      else
         value1 = value
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l) = 1.0
            alu(:,:,l) = eq_er(er_Lapla_eq(eq_work))
         enddo

         ! 0 成分のところを 1 で埋める.
         do k=-km,km
            do l=0,md(k)-1
               alu(k,l,l) = 1.0D0
            enddo
            do l=md(k)+1,lm,2
               alu(k,l,l) = 1.0D0
            enddo
         enddo

         ! 境界条件 r=ra で値を与える. 
         do k=-km,km
            do l=0,lm
               eq_work=0 ; eq_work(k,l)=1.0
               er_work=er_eq(eq_work)
               if ( mod(md(k),2) .eq. mod(lm,2) ) then
                  alu(k,lm,l) = er_work(k,jm)
               else
                  alu(k,lm-1,l) = er_work(k,jm)
               endif
            enddo
         enddo

         call ludecomp(alu,kp)
      endif

      eq_work = eq
      do k=-km,km
         if ( mod(md(k),2) .eq. mod(lm,2) ) then
            eq_work(k,lm)   = value1(k)
         else
            eq_work(k,lm-1) = value1(k)
         endif
      enddo
      eq_LaplaInv_eq = lusolve(alu,kp,eq_work)

    end function eq_LaplaInv_eq

    function eq_Vor2Strm_eq(eq,value,cond,new)
      !
      ! 渦度から流線を求める. 
      !
      ! Chebyshev-tau 法による計算
      ! 渦度 \zeta を与えて流線 \psi を求める.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at the boundary
      ! 粘着条件
      !    \DP{\psi}{r} = 0 at the boundary
      ! 応力なし条件
      !    r\DP{}{r}(1/r\DP{\psi}{r})  = 0 at the boundary
      !
      ! l=0,lm 成分の式の代わりに境界条件を与える. 
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: eq
              !(in) 入力渦度分布

      real(8), dimension(-km:km,0:lm)             :: eq_Vor2Strm_eq
              !(out) 出力流線関数分布

      real(8), intent(in), optional               :: value
              ! 流線境界値. 境界で一定なので波数 0 成分のみ

      character(len=1), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'R'
              !     R    : 上側粘着条件
              !     F    : 上側応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu, alub
      integer, dimension(:,:), allocatable    :: kp, kpb

      real(8), dimension(-km:km,0:lm)         :: eq_work
      real(8), dimension(-km:km,jm)           :: er_work
      real(8)                                 :: value1          ! 境界値
      logical                                 :: rigid

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k, l, ll
      save    :: alu, kp, first
      save    :: alub, kpb

      if (.not. present(value)) then
         value1 = 0
      else
         value1 = value
      endif

      if (.not. present(cond)) then
         rigid=.TRUE. 
      else
         select case (cond)
         case ('R')
            rigid = .TRUE.
         case ('F')
            rigid = .FALSE.
         case default
            call MessageNotify('E','eq_Vor2Strm_eq','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         if ( allocated(alub) ) deallocate(alub)
         if ( allocated(kpb) ) deallocate(kpb)
         allocate(alub(-km:km,0:lm,0:lm),kpb(-km:km,0:lm))

         ! 内部条件
         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l) = 1.0
            alu(:,:,l) = eq_er(er_Lapla_eq(eq_work))
         enddo

         ! 0 成分のところを 1 で埋める.
         do k=-km,km
            do l=0,md(k)-1
               alu(k,l,l) = 1.0D0
            enddo
            do l=md(k)+1,lm,2
               alu(k,l,l) = 1.0D0
            enddo
         enddo
         
         ! alu(:,:,nd(k)) 列は 0 なので 1 をいれておく. 
         ! l=md(k) 成分は境界条件で決める. 
         do k=-km,km
            if ( mod(md(k),2) .eq. mod(lm,2) ) then
               alu(k,lm,md(k)) = 1.0D0
            else
               alu(k,lm-1,md(k)) = 1.0D0
            endif
         enddo

         call ludecomp(alu,kp)

         !---- 境界条件計算用行列 -----
         alub = 0.0
         do l=0,lm
            alub(:,l,l) = 1.0D0
         enddo

         ! 運動学的条件. 流線は境界で一定
         !     l=nd(n) 成分を境界条件で決める. 
         do l=0,lm
            eq_work = 0.0 ; eq_work(:,l)=1.0D0
            er_work = er_eq(eq_work)
            do k=-km,km
               alub(k,md(k),l) = er_work(k,jm)
            enddo
         enddo

         ! 力学的条件粘着条件 
         !     l=lm or lm-1 成分を境界条件で決める. 
         if ( rigid ) then
            do l=0,lm
               eq_work = 0.0 ;  eq_work(:,l)=1.0D0
               er_work=er_eq(eq_RadDRad_eq(eq_work))/er_Rad
               do k=-km,km
                  if ( mod(md(k),2) .eq. mod(lm,2) ) then
                     alub(k,lm,l) = er_work(k,jm)
                  else
                     alub(k,lm-1,l) = er_work(k,jm)
                  endif
               end do
            enddo
         else
            do l=0,lm
               eq_work = 0.0 ; eq_work(:,l)=1.0D0
               er_work=er_eq(eq_RadDRad_eq(eq_RadDRad_eq(eq_work)) &
                            -2*eq_RadDRad_eq(eq_work))/er_Rad**2
               do k=-km,km
                  if ( mod(md(k),2) .eq. mod(lm,2) ) then
                     alub(k,lm,l) = er_work(k,jm)
                  else
                     alub(k,lm-1,l) = er_work(k,jm)
                  endif
               end do
            enddo
         endif

         call ludecomp(alub,kpb)

         if ( rigid ) then
            call MessageNotify('M','eq_Vor2Strm_eq',&
                              'Matrix to apply rigid b.c. newly produced.')
         else
            call MessageNotify('M','eq_Vor2Strm_eq',&
                              'Matrix to apply stress-free b.c. newly produced.')
         endif
      endif

      ! 内部領域計算
      eq_work = eq

      eq_work = lusolve(alu,kp,eq_work)

      ! 境界条件計算
      do k=-km,km
         eq_work(k,md(k)) = 0
         if ( mod(md(k),2) .eq. mod(lm,2) ) then
            eq_work(k,lm)   = 0
         else
            eq_work(k,lm-1) = 0
         endif
      enddo
      eq_work(0,0)   = value1*2     ! 運動学的条件. 波数 0 は重み 1/2

      eq_Vor2Strm_eq = lusolve(alub,kpb,eq_work)

    end function eq_Vor2Strm_eq

  !--------------- 積分計算 -----------------
    function IntRadPhi_rp(rp)
      !
      ! 2 次元格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight, r_Rad_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 次元格子点データ

      real(8)                         :: IntRadPhi_rp
      !(out) 積分値

      integer :: i, j

      IntRadPhi_rp = 0.0d0
      do i=0,im-1
         do j=1,jm
            IntRadPhi_rp = IntRadPhi_rp + rp(j,i) * r_Rad_Weight(j) * p_Phi_Weight(i)
         enddo
      enddo
    end function IntRadPhi_rp

    function r_IntPhi_rp(rp)
      !
      ! 2 次元格子点データの Phi 方向積分
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight をかけた総和を計算している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 次元格子点データ

      real(8), dimension(jm)          :: r_IntPhi_rp
      !(out) 積分された 1 次元(Rad)格子点データ

      integer :: i
      ! 作業変数

      r_IntPhi_rp = 0.0d0
      do i=0,im-1
         r_IntPhi_rp(:) = r_IntPhi_rp(:) + rp(:,i) * p_Phi_Weight(i)
      enddo
    end function r_IntPhi_rp

    function p_IntRad_rp(rp)
      !
      ! 2 次元格子点データの Rad 方向積分
      !
      ! 実際には格子点データ各点毎に r_Rad_Weight をかけた総和を計算している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 次元格子点データ

      real(8), dimension(0:im-1)      :: p_IntRad_rp
      !(out) 積分された 1 次元(Phi)格子点データ

      integer :: j
      ! 作業変数

      p_IntRad_rp = 0.0d0
      do j=1,jm
         p_IntRad_rp(:) = p_IntRad_rp(:) + rp(j,:) * r_Rad_Weight(j)
      enddo
    end function p_IntRad_rp

    function IntPhi_p(p)
      !
      ! 1 次元(Phi)格子点データの Phi 方向積分
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:im-1)   :: p         !(in)  1 次元格子点データ
      real(8)                      :: IntPhi_p    !(out) 積分値

      IntPhi_p = sum(p*p_Phi_Weight)
    end function IntPhi_p

    function IntRad_r(r) 
      !
      ! 1 次元(Rad)格子点データの Rad 方向積分
      !
      ! 実際には格子点データ各点毎に r_Rad_Weight をかけた総和を計算している. 
      !
      real(8), dimension(jm)   :: r          !(in)  1 次元格子点データ
      real(8)                  :: IntRad_r     !(out) 積分値

      IntRad_r = sum(r*r_Rad_Weight)
    end function IntRad_r

  !--------------- 平均計算 -----------------
    function AvrRadPhi_rp(rp)
      !
      ! 2 次元格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight, r_Rad_Weight をかけた
      ! 総和を計算し, p_Phi_Weight*r_Rad_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in)  2 次元格子点データ

      real(8)                         :: AvrRadPhi_rp
      !(out) 平均値

      AvrRadPhi_rp = IntRadPhi_rp(rp)/(sum(p_Phi_weight)*sum(r_Rad_weight))
    end function AvrRadPhi_rp

    function r_AvrPhi_rp(rp)
      !
      ! 2 次元格子点データの Phi 方向平均
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight をかけた総和を計算し, 
      ! p_Phi_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 次元格子点データ

      real(8), dimension(jm)          :: r_AvrPhi_rp
      !(out) 平均された 1 次元(Rad)格子点

      r_AvrPhi_rp = r_IntPhi_rp(rp)/sum(p_Phi_weight)
    end function r_AvrPhi_rp

    function p_AvrRad_rp(rp)
      !
      ! 2 次元格子点データの Rad 方向平均
      !
      ! 実際には格子点データ各点毎に r_Rad_Weight をかけた総和を計算し, 
      ! r_Rad_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(jm,0:im-1)   :: rp
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)      :: p_AvrRad_rp
      !(out) 平均された 1 次元(Phi)格子点

      p_AvrRad_rp = p_IntRad_rp(rp)/sum(r_Rad_weight)
    end function p_AvrRad_rp

    function AvrPhi_p(p)
      !
      ! 1 次元(Phi)格子点データの Phi 方向平均
      !
      ! 実際には格子点データ各点毎に p_Phi_Weight をかけた総和を計算し, 
      ! p_Phi_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1)   :: p          !(in)  1 次元格子点データ
      real(8)                      :: AvrPhi_p     !(out) 平均値

      AvrPhi_p = IntPhi_p(p)/sum(p_Phi_weight)
    end function AvrPhi_p

    function AvrRad_r(r)
      !
      ! 1 次元(Rad)格子点データの Rad 方向平均
      !
      ! 実際には格子点データ各点毎に r_Rad_Weight をかけた総和を計算し, 
      ! r_Rad_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(jm)   :: r            !(in)  1 次元格子点データ
      real(8)                  :: AvrRad_r     !(out) 平均値

      AvrRad_r = IntRad_r(r)/sum(r_Rad_weight)
    end function AvrRad_r

end module eq_module
