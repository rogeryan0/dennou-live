!--
!----------------------------------------------------------------------
! Copyright(c) 2002-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  et_module
!      2 次元水路領域問題, Fourier 展開 + Chebyshev 展開法
!
!      spml/et_module モジュールは 2 次元水路領域での流体運動を
!      スペクトル法により数値計算を実行するための Fortran90 関数を提供する. 
!      周期的な境界条件を扱うための X 方向へのフーリエ変換と境界壁を扱うための
!      Y 方向のチェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
!      関数を提供する. 
!
!      内部で ae_module, at_module を用いている. 
!      最下部ではフーリエ変換およびチェビシェフ変換のエンジンとして 
!      ISPACK/FTPACK の Fortran77 サブルーチンを用いている.
!
!
!履歴  2002/01/27  竹広真一
!      2002/03/30  竹広真一  モジュール名変更
!      2002/08/19  竹広真一  格子データ添字を gg -> xy, eg -> ey に変更
!                            積分・平均関数追加
!      2005/01/09  竹広真一  msgdmp -> MessageNotify に変更
!      2005/03/15  竹広真一  xy -> yx に接頭子を変更
!      2006/03/04  竹広真一  余計な変数を削除(et_Jacobian_et_et)
!      2006/03/06  竹広真一  コメントを RDoc 用に修正
!      2007/11/20  竹広真一  渦度-流線関数計算に行列設定スイッチ導入
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2009/01/09  竹広真一  et_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に修正
!      2009/07/31  竹広真一  境界条件計算用配列を threadprivate 指定(OpenMP)
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!++
module et_module
  !
  != et_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: et_module.f90,v 1.18 2010-03-02 10:09:03 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! 2 次元水路領域問題, Fourier 展開 + Chebyshev 展開法
  !
  ! spml/et_module モジュールは 2 次元水路領域での流体運動を
  ! スペクトル法により数値計算を実行するための Fortran90 関数を提供する. 
  ! 周期的な境界条件を扱うための X 方向へのフーリエ変換と境界壁を扱うための
  ! Y 方向のチェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
  ! 関数を提供する. 
  !
  ! 内部で ae_module, at_module を用いている. 
  ! 最下部ではフーリエ変換およびチェビシェフ変換のエンジンとして 
  ! ISPACK/FTPACK の Fortran77 サブルーチンを用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (et_, yx_, x_, y_) は, 返す値の形を示している.
  !   et_ :: 2次元スペクトルデータ
  !   yx_ :: 2 次元格子点データ
  !   x_  :: X 方向 1 次元格子点データ
  !   y_  :: Y 方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dx, Dy, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_et_et, _et, _yx, _x, _y) は, 入力変数のスペクトルデータ
  !   および格子点データであることを示している.
  !   _et    :: 2次元スペクトルデータ
  !   _et_et :: 2 つの2次元スペクトルデータ
  !   _yx    :: 2 次元格子点データ
  !   _x     :: X 方向 1 次元格子点データ
  !   _y     :: Y 方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * yx : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:jm,0:im-1). 
  !   * im, jm はそれぞれ X, Y 座標の格子点数であり, サブルーチン
  !     et_initial にてあらかじめ設定しておく.
  !   * 第 1 次元が Y 座標の格子点位置番号, 第 2 次元が X 座標の
  !     格子点位置番号である (X, Y の順ではない)ことに注意.
  !
  ! * et : 2 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km,0:lm). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン
  !     et_initial にてあらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方については...
  !
  ! * x, y : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1)
  !     および real(8), dimension(0:jm).
  !
  ! * e, t : 1 次元スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km) 
  !     および real(8), dimension(-lm:lm).
  !
  ! * ax, ay : 1 次元格子点データの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,0:im-1) 
  !     および real(8), dimension(:,0:jm).
  !
  ! * ae, at : 1 次元スペクトルデータの並んだ 2 次元配列.
  !   * 変数の種類と次元は real(8), dimension(:,-km:km) 
  !     および real(8), dimension(:,0:lm).
  !
  ! * et_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * yx_ で始まる関数が返す値は 2 次元格子点データに同じ.
  !
  ! * x_, y_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! et_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_X, y_Y     ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight ::  重み座標を格納した 1 次元配列
  ! yx_X, yx_Y   :: 格子点データの XY 座標(X,Y)(格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! yx_et :: スペクトルデータから格子データへの変換
  ! et_yx :: 格子データからスペクトルデータへの変換
  ! ax_ae, x_e :: X 方向のスペクトルデータから格子データへの変換
  ! ay_at, y_t :: Y 方向のスペクトルデータから格子データへの変換
  ! ae_ax, e_x :: X 方向の格子点データからスペクトルデータへの変換
  ! at_ay, t_y :: Y 方向の格子点データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! et_Lapla_et  :: スペクトルデータにラプラシアンを作用させる
  ! et_Dx_et, ae_Dx_ae, e_Dx_e :: スペクトルデータに X 微分を作用させる
  ! et_Dy_et, at_Dy_at, t_Dy_t :: スペクトルデータに Y 微分を作用させる
  ! et_Jacobian_et_et :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !==== 境界値問題
  !
  ! et_Boundaries  :: ディリクレ, ノイマン境界条件の適用
  ! et_LaplaInv_et :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! ey_Vor2Strm_ey :: 渦度から流線を計算する
  !
  !==== 積分・平均
  !
  ! IntYX_yx, AvrYX_yx   :: 2 次元格子点データの全領域積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元格子点データの X 方向積分および平均
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元格子点データの Y 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  !
  use dc_message
  use lumatrix
  use ae_module, x_X => g_X, x_X_weight => g_X_Weight, &
                 e_x => e_g, ae_ax => ae_ag, &
                 x_e => g_e, ax_ae => ag_ae
  use at_module, y_Y => g_X, y_Y_Weight => g_X_Weight, &
                 at_ay => at_ag, t_y => t_g, &
                 ay_at => ag_at, y_t => g_t, &
                 t_Dy_t => t_Dx_t, at_Dy_at => at_Dx_at

  implicit none
  private

  public et_Initial                                       ! 初期化
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! 座標変数

  public yx_et, et_yx                                     ! 基本変換
  public e_x, x_e, ae_ax, ax_ae                           ! 基本変換
  public t_y, y_t, at_ay, ay_at                           ! 基本変換

  public et_Dx_et, e_Dx_e, ae_Dx_ae                       ! 微分
  public et_Dy_et, t_Dy_t, at_Dy_at                       ! 微分
  public et_Lapla_et                                      ! 微分

  public et_Jacobian_et_et                                ! 非線形計算

  public et_Boundaries                                    ! 境界値問題
  public at_Boundaries_DD, at_Boundaries_DN               ! 境界値問題
  public at_Boundaries_ND, at_Boundaries_NN               ! 境界値問題
  public et_LaplaInv_et, ey_Vor2Strm_ey                   ! 境界値問題
  public et_Vor2Strm_et, et_Vor2Strm1_et                  ! 境界値問題

  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! 積分
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! 平均

  integer            :: im=32, jm=8      ! 格子点の設定(X,Y)
  integer            :: km=10, lm=5      ! 切断波数の設定(X,Y)
  real(8)            :: xl=2.0, yl=1.0   ! 領域の大きさ
  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:), allocatable :: yx_X, yx_Y

  save im, jm, km, lm, xl, yl

  contains
  !--------------- 初期化 -----------------
    subroutine et_Initial(i,j,k,l,xmin,xmax,ymin,ymax)
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

      real(8),intent(in) :: xmin, xmax     ! X 座標範囲
      real(8),intent(in) :: ymin, ymax     ! Y 座標範囲

      im = i       ; jm = j
      km = k       ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      call ae_initial(im,km,xmin,xmax)
      call at_initial(jm,lm,ymin,ymax)

      allocate(yx_X(0:jm,0:im-1),yx_Y(0:jm,0:im-1))
      yx_X = spread(x_X,1,jm+1)
      yx_Y = spread(y_Y,2,im)

      call MessageNotify('M','et_initial','et_module (2009/07/31) is initialized')
    end subroutine et_initial

  !--------------- 基本変換 -----------------

    function yx_et(et)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_et
      !(out) 格子点データ

      real(8), dimension(-km:km,0:lm), intent(in)  :: et
      !(in) スペクトルデータ

      yx_et = ax_ae(transpose(ay_at(et)))

    end function yx_et

    function et_yx(yx)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,0:lm)              :: et_yx
      !(out) スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      et_yx = at_ay(transpose(ae_ax(yx)))

    end function et_yx

  !--------------- 微分計算 -----------------

    function et_Dx_et(et)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,0:lm)                :: et_Dx_et
      real(8), dimension(-km:km,0:lm), intent(in)    :: et
      integer k

      do k=-km,km
         et_Dx_et(k,:)  =  (-2*pi*k/xl)*et(-k,:)
      enddo
    end function et_Dx_et

    function et_Dy_et(et)
      !
      ! 入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm)               :: et_Dy_et
      !(out) スペクトルデータの Y 微分

      real(8), dimension(-km:km,0:lm), intent(in)   :: et
      !(in) 入力スペクトルデータ

      et_Dy_et = at_Dy_at(et)

    end function et_Dy_et

    function et_Lapla_et(et)
      !
      ! 入力スペクトルデータにラプラシアン(∂xx+∂yy)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm)                :: et_Lapla_et
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-km:km,0:lm), intent(in)    :: et
      !(in) 入力スペクトルデータ

      integer k

      do k=-km,km
         et_Lapla_et(k,:) = -(2*pi*k/xl)**2*et(k,:)
      enddo

      et_Lapla_et = et_Lapla_et + et_Dy_et(et_Dy_et(et))

    end function et_Lapla_et

    function et_Jacobian_et_et(et_a,et_b)
      !
      !  2 つのスペクトルデータからヤコビアン
      !
      !     J(A,B)=(∂xA)(∂yB)-(∂yA)(∂xB)
      !
      !  を計算する.
      !
      !  2 つのスペクトルデータのヤコビアンとは, 対応する 2 つの
      !  格子点データのヤコビアンのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm)                :: et_Jacobian_et_et
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-km:km,0:lm), intent(in)    :: et_a
      !(in) 1つ目の入力スペクトルデータ

      real(8), dimension(-km:km,0:lm), intent(in)    :: et_b
      !(in) 2つ目の入力スペクトルデータ

      et_Jacobian_et_et = et_yx(&
           yx_et(et_Dx_et(et_a)) * yx_et(et_Dy_et(et_b)) &
           -yx_et(et_Dy_et(et_a)) * yx_et(et_Dx_et(et_b)) )

    end function et_Jacobian_et_et


  !--------------- 境界値問題 -----------------

    subroutine et_Boundaries(et,values,cond)
      !
      ! ディリクレ, ノイマン条件の適用. チェビシェフ空間での計算
      !
      ! 実際には中で呼ばれている at_module のサブルーチン at_Boundaries_DD,
      ! at_Boundaries_DN, at_Boundaries_ND, at_Boundaries_NN を用いている. 
      ! これらを直接呼ぶことも出来る.
      !
      real(8), dimension(-km:km,0:lm),intent(inout)      :: et
              ! 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(-km:km,2), intent(in), optional :: values
              ! 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              ! 省略時は値/勾配 0 となる. 

      character(len=2), intent(in), optional             :: cond
              ! 境界条件. 省略時は 'DD'
              !   DD : 両端ディリクレ
              !   DN,ND : ディリクレ/ノイマン条件
              !   NN : 両端ノイマン

      if (.not. present(cond)) then
         if (present(values)) then
            call at_Boundaries_DD(et,values)
         else
            call at_Boundaries_DD(et)
         endif
         return
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_Boundaries_NN(et,values)
         else
            call at_Boundaries_NN(et)
         endif
      case ('DN')
         if (present(values)) then
            call at_Boundaries_DN(et,values)
         else
            call at_Boundaries_DN(et)
         endif
      case ('ND')
         if (present(values)) then
            call at_Boundaries_ND(et,values)
         else
            call at_Boundaries_ND(et)
         endif
      case ('DD')
         if (present(values)) then
            call at_Boundaries_DD(et,values)
         else
            call at_Boundaries_DD(et)
         endif
      case default
         call MessageNotify('E','et_Boundaries','B.C. not supported')
      end select

    end subroutine et_Boundaries

    function et_LaplaInv_et(et,values)
      !
      ! 境界で一様な値を与える条件(ディリクレ条件)下で, 
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy)**(-1)を作用する.
      !
      ! Chebyshev-tau 法による計算
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      !(in) スペクトルデータ

      real(8), dimension(-km:km,0:lm)             :: et_LaplaInv_et
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-km:km,2), intent(in), optional :: values
      !(in) 境界値. 省略時は 0 が設定される. 

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8), dimension(-km:km)              :: value1, value2   ! 境界値

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(:,1) ; value2 = values(:,2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         ty_work=ay_at(tt_work)

         do k=-km,km
            alu(k,:,:) = transpose(at_Dy_at(at_Dy_at(tt_work)) &
                                   - (2*pi*k/xl)**2*tt_work)
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et
      et_work(:,lm-1) = value1
      et_work(:,lm)   = value2
      et_LaplaInv_et = lusolve(alu,kp,et_work)

    end function et_LaplaInv_et

    function ey_Vor2Strm_ey(ey,values,cond,new)    ! 渦度から流線を求める. 
      !
      ! 渦度から流線を求める. 
      ! Y 方向格子点空間での Chebyshev-Collocation 法による計算
      !
      ! 渦度 \zeta を与えて流線 \psi を求める.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! 粘着条件
      !    \DP{\psi}{y} = 0 at boundaries
      ! 応力なし条件
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !

      real(8), dimension(-km:km,0:jm),intent(in)  :: ey
              !(in) 入力渦度分布

      real(8), dimension(-km:km,0:jm)             :: ey_Vor2Strm_ey
              !(out) 出力流線分布

      real(8), dimension(2), intent(in), optional :: values
              !(in) 流線境界値. 境界で一定なので波数 0 成分のみ
	      !     省略時は 0.

      character(len=2), intent(in), optional  :: cond
              ! (in) 境界条件スイッチ. 省略時は 'RR'
              !      RR : 両端粘着条件
              !      RF,FR : 粘着/応力なし条件
              !      FF : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:jm)         :: ey_work
      real(8), dimension(0:jm,0:jm)           :: yy
      real(8), dimension(0:jm,0:jm)           :: yy_work
      real(8)                                 :: value1, value2   ! 境界値
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k,j
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

      if (.not. present(cond)) then
         rigid1=.TRUE. ; rigid2=.TRUE.
      else
         select case (cond)
         case ('RR')
            rigid1 = .TRUE.  ; rigid2 = .TRUE.
         case ('RF')
            rigid1 = .TRUE.  ; rigid2 = .FALSE.
         case ('FR')
            rigid1 = .FALSE. ; rigid2 = .TRUE.
         case ('FF')
            rigid1 = .FALSE. ; rigid2 = .FALSE.
         case default
            call MessageNotify('E','ey_Vor2Strm_ey','B.C. not supported')
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
         allocate(alu(-km:km,0:jm,0:jm),kp(-km:km,0:jm))

         yy=0
         do j=0,jm
            yy(j,j)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose( &
                 ay_at(at_Dy_at(at_Dy_at(at_ay(yy)))) &
                 - (2*pi*k/xl)** 2* yy )
         enddo

         ! 運動学的条件. 流線は境界で一定
         yy_work=yy
         do k=-km,km
            alu(k,0,:)   = yy_work(:,0)
            alu(k,jm,:)  = yy_work(:,jm)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            yy_work=ay_at(at_Dy_at(at_ay(yy)))
         else
            yy_work=ay_at(at_Dy_at(at_Dy_at(at_ay(yy))))
         endif
         do k=-km,km
            alu(k,1,:) = yy_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid2 ) then
            yy_work=ay_at(at_Dy_at(at_ay(yy)))
         else
            yy_work=ay_at(at_Dy_at(at_Dy_at(at_ay(yy))))
         endif
         do k=-km,km
            alu(k,jm-1,:) = yy_work(:,jm)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','ey_Vor2Strm_ey',&
                            'Matrix for stream func. calc. produced')
      endif

      ey_work = ey
      ey_work(:,1)    = 0               ! 力学的条件
      ey_work(:,jm-1) = 0               ! 力学的条件

      ey_work(:,0) = 0            ! 運動学的条件. 波数 0 以外は 0 
      ey_work(0,0) = value1*2     ! 運動学的条件. 波数 0 は重み 1/2

      ey_work(:,jm)   = 0            ! 運動学的条件. 波数 0 以外は 0 
      ey_work(0,jm)   = value2*2     ! 運動学的条件. 波数 0 は重み 1/2

      ey_Vor2Strm_ey = lusolve(alu,kp,ey_work)

    end function ey_Vor2Strm_ey

   !-----  注意 : 計算うまくいかず. pending ---- 
    function et_Vor2Strm_et(et,values,rigid)
      !
      ! 渦度から流線を求める. 
      !
      ! 注意 : 以下の点がうまくなく現在保留中. 使用禁止
      !    * Y 方向の切断波数を lm=jm と設定しておかないと
      !      計算結果が振動して不安定となる.
      !    *  非圧縮流体計算の拡散問題の時間発展が安定に計算できない. 
      !       Chebyshev-tauとこの問題の相性は良くないらしい.
      !
      ! Chebyshev-tau 法による計算
      ! 渦度 \zeta を与えて流線 \psi を求める.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! 粘着条件
      !    \DP{\psi}{y} = 0 at boundaries
      ! 応力なし条件
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !
      ! l=0,1,lm-1,lm 成分の式の代わりに境界条件を与える. 
      ! 渦度の低次成分を無視することは
      ! \nabla^4 \psi = \zeta^2 を解いていることに相当. 
      ! 4 階の微分方程式にすることで境界条件の数とつじつまがあう. 

      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      real(8), dimension(-km:km,0:lm)             :: et_Vor2Strm_et

      ! 流線境界値. 境界で一定なので波数 0 成分のみ
      real(8), dimension(2), intent(in), optional :: values
      ! 境界条件スイッチ
      logical, dimension(2), intent(in), optional :: rigid

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8)                                 :: value1, value2   ! 境界値
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

      if (.not. present(rigid)) then
         rigid1=.true. ; rigid2=.true.
      else
         rigid1 = rigid(1) ; rigid2 = rigid(2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose(at_Dy_at(at_Dy_at(tt_work)) &
                                   - (2*pi*k/xl)**2*tt_work)
         enddo

         ! 運動学的条件. 流線は境界で一定
         ty_work=ay_at(tt_work)
         do k=-km,km
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,0,:) = ty_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid2 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,1,:) = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et
      et_work(:,0) = 0               ! 力学的条件
      et_work(:,1) = 0               ! 力学的条件

      et_work(:,lm-1) = 0            ! 運動学的条件. 波数 0 以外は 0 
      et_work(0,lm-1) = value1*2     ! 運動学的条件. 波数 0 は重み 1/2

      et_work(:,lm)   = 0            ! 運動学的条件. 波数 0 以外は 0 
      et_work(0,lm)   = value2*2     ! 運動学的条件. 波数 0 は重み 1/2

      et_Vor2Strm_et = lusolve(alu,kp,et_work)

    end function et_Vor2Strm_et

   !-----  注意 : 計算うまくいかず. pending ---- 
    function et_Vor2Strm1_et(et,values,rigid) 
      ! 渦度から流線を求める. 
      !
      ! 注意 : 以下の点がうまくなく現在保留中. 使用禁止
      !    * Y 方向の切断波数を lm=jm と設定しておかないと
      !      計算結果が振動して不安定となる.
      !    *  非圧縮流体計算の拡散問題の時間発展が安定に計算できない. 
      !       Chebyshev-tauとこの問題の相性は良くないらしい.
      !
      ! Chebyshev-tau 法による計算
      ! 渦度 \zeta を与えて流線 \psi を求める.
      !    \nabla^2 \psi = \zeta, 
      !    \psi = const. at boundaries.
      ! 粘着条件
      !    \DP{\psi}{y} = 0 at boundaries
      ! 応力なし条件
      !    \DP[2]{\psi}{y} = 0 at boundaries
      !
      ! デフォルトは粘着条件
      !
      ! \nabla^4 \psi = \nabla^2\zeta を解く
      ! 4 階の微分方程式にすることで境界条件の数とつじつまがあう. 
      !
      !
      real(8), dimension(-km:km,0:lm),intent(in)  :: et
      !(in) 渦度

      real(8), dimension(-km:km,0:lm)             :: et_Vor2Strm1_et
      !(out)  流線境界値. 境界で一定なので波数 0 成分のみ

      real(8), dimension(2), intent(in), optional :: values
      !(in) 境界条件スイッチ

      logical, dimension(2), intent(in), optional :: rigid

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension(-km:km,0:lm)         :: et_work
      real(8), dimension(0:lm,0:lm)           :: tt_work
      real(8), dimension(0:lm,0:jm)           :: ty_work
      real(8)                                 :: value1, value2   ! 境界値
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      integer :: k,l
      save    :: alu, kp, first

      if (.not. present(values)) then
         value1=0 ; value2=0
      else
         value1 = values(1) ; value2 = values(2)
      endif

      if (.not. present(rigid)) then
         rigid1=.true. ; rigid2=.true.
      else
         rigid1 = rigid(1) ; rigid2 = rigid(2)
      endif

      if ( first ) then
         first = .false.

         allocate(alu(-km:km,0:lm,0:lm),kp(-km:km,0:lm))

         tt_work=0
         do l=0,lm
            tt_work(l,l)=1
         enddo
         do k=-km,km
            alu(k,:,:) = transpose( &
                 at_Dy_at(at_Dy_at(at_Dy_at(at_Dy_at(tt_work)))) &
                 - 2 * (2*pi*k/xl)**2 * at_Dy_at(at_Dy_at(tt_work)) &
                 + (2*pi*k/xl)**4*tt_work &
                 )
         enddo

         ! 運動学的条件. 流線は境界で一定
         ty_work=ay_at(tt_work)
         do k=-km,km
            alu(k,lm-1,:) = ty_work(:,0)
            alu(k,lm,:)   = ty_work(:,jm)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid1 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,lm-3,:) = ty_work(:,0)
         enddo

         ! 力学的条件粘着条件 
         if ( rigid2 ) then
            ty_work=ay_at(at_Dy_at(tt_work))
         else
            ty_work=ay_at(at_Dy_at(at_Dy_at(tt_work)))
         endif
         do k=-km,km
            alu(k,lm-2,:) = ty_work(:,jm)
         enddo

         call ludecomp(alu,kp)
      endif

      et_work = et_Lapla_et(et)
      et_work(:,lm-3) = 0               ! 力学的条件
      et_work(:,lm-2) = 0               ! 力学的条件

      et_work(:,lm-1) = 0            ! 運動学的条件. 波数 0 以外は 0 
      et_work(0,lm-1) = value1*2     ! 運動学的条件. 波数 0 は重み 1/2

      et_work(:,lm)   = 0            ! 運動学的条件. 波数 0 以外は 0 
      et_work(0,lm)   = value2*2     ! 運動学的条件. 波数 0 は重み 1/2

      et_Vor2Strm1_et = lusolve(alu,kp,et_work)

    end function et_Vor2Strm1_et

  !--------------- 積分計算 -----------------
    function IntYX_yx(yx)   ! 全領域積分
      !
      ! 2 次元格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 次元格子点データ

      real(8)                           :: IntYX_yx
      !(out) 積分値

      integer :: i, j

      IntYX_yx = 0.0d0
      do i=0,im-1
         do j=0,jm
            IntYX_yx = IntYX_yx + yx(j,i) * y_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo
    end function IntYX_yx

    function y_IntX_yx(yx)  ! X 方向積分
      !
      ! 2 次元格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm)          :: y_IntX_yx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i
      ! 作業変数

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)  ! Y 方向積分
      !
      ! 2 次元格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 次元格子点データ

      real(8), dimension(0:im-1)        :: x_IntY_yx
      !(out) 積分された 1 次元(X)格子点データ

      integer :: j
      ! 作業変数

      x_IntY_yx = 0.0d0
      do j=0,jm
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntX_x(x)      ! X 方向積分
      !
      ! 1 次元(X)格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:im-1)   :: x         !(in)  1 次元格子点データ
      real(8)                      :: IntX_x    !(out) 積分値

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntY_y(y)      ! Y 方向積分
      !
      ! 1 次元(Y)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm)   :: y          !(in)  1 次元格子点データ
      real(8)                    :: IntY_y     !(out) 積分値

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

  !--------------- 平均計算 -----------------
    function AvrYX_yx(yx)
      !
      ! 2 次元格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in)  2 次元格子点データ

      real(8)                           :: AvrYX_yx
      !(out) 平均値

      AvrYX_yx = IntYX_yx(yx)/(sum(x_X_weight)*sum(y_Y_weight))
    end function AvrYX_yx

    function y_AvrX_yx(yx)
      !
      ! 2 次元格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm)          :: y_AvrX_yx
      !(out) 平均された 1 次元(Y)格子点

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)
      !
      ! 2 次元格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)        :: x_AvrY_yx
      !(out) 平均された 1 次元(X)格子点

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

    function AvrX_x(x)
      !
      ! 1 次元(X)格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1)   :: x          !(in)  1 次元格子点データ
      real(8)                      :: AvrX_x     !(out) 平均値

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrY_y(y)
      !
      ! 1 次元(Y)格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm)   :: y          !(in)  1 次元格子点データ
      real(8)                    :: AvrY_y     !(out) 平均値

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  end module et_module
