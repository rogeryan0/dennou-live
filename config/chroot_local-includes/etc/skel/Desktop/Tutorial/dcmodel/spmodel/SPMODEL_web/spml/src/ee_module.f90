!--
!----------------------------------------------------------------------
!     Copyright 2001--2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ee_module
!
!      spml/ee_module モジュールは周期境界条件の下での 2 次元矩形領域の
!      流体運動をスペクトル法により数値計算するための Fortran90 関数を
!      提供する. 
!
!      内部で ISPACK/P2PACK の Fortran77 サブルーチンを呼んでいる. 
!      スペクトルデータおよび格子点データの格納方法については
!      ISPACK/P2PACK のマニュアルを参照されたい. 
!
!履歴  2001/10/07  竹広真一
!      2001/12/25  竹広真一  関数, 変数命名法変更
!      2002/03/25  竹広真一  モジュール名変更
!      2002/08/19  竹広真一  格子データ添字を gg -> xy に変更
!      2002/08/20  竹広真一  積分・平均関数追加
!      2005/03/15  竹広真一  xy -> yx に接頭子を変更
!      2005/07/18  竹広真一  使用する ISPACK ライブラリを p2pack に変更
!      2005/10/26  竹広真一  エネルギー, エンストロフィースペクトル追加
!      2006/03/05  竹広真一  コメントを RDoc 用に修正
!      2006/03/19  竹広真一  変数・手続き群の要約をコメントに追加
!      2008/05/10  西澤誠也 => 竹広真一 解像度領域複数対応
!      2008/10/28  竹広真一  補間計算追加
!      2009/01/09  竹広真一  ee_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に修正
!      2009/07/31  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!
!++
module ee_module
  !
  != ee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ee_module.f90,v 1.22 2009-07-31 03:06:18 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/ee_module モジュールは周期境界条件の下での 2 次元矩形領域の
  ! 流体運動をスペクトル法により数値計算するための Fortran90 関数を
  ! 提供する. 
  !
  ! 内部で ISPACK/P2PACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法については
  ! ISPACK/P2PACK のマニュアルを参照されたい. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !  
  ! * 関数名の先頭 (ee_, yx_, x_, y_) は, 返す値の形を示している.
  !   ee_ :: スペクトルデータ(第 1,2 次元がそれぞれ Y,X 方向波数)
  !   yx_ :: 2 次元格子点データ(第 1,2 次元がそれぞれ Y,X 方向の格子点)
  !   x_  :: X 方向 1 次元格子点データ, y_ : Y 方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dx, Dy, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_ee_ee, _ee, _yx, _x, _y) は, 入力変数の形が
  !   スペクトルデータおよび格子点データであることを示している.
  !   _ee    :: スペクトルデータ
  !   _ee_ee :: 2 つのスペクトルデータ
  !   _yx    :: 2 次元格子点データ
  !   _x     :: X 方向 1 次元格子点データ
  !   _y     :: Y 方向 1 次元格子点データ.
  !  
  !=== 各データの種類の説明
  !  
  ! * yx : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:jm-1,0:im-1). 
  !   * im, jm はそれぞれ X, Y 座標の格子点数であり, サブルーチン 
  !     ee_initial にてあらかじめ設定しておく.
  !   * 第 1 次元が Y 座標の格子点位置番号, 第 2 次元が X 座標の
  !     格子点位置番号である (X, Y の順ではない)ことに注意.
  !
  ! * ee : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-lm:lm,-km:km). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン 
  !     ee_initial にてあらかじめ設定しておく.
  !     (X, Y 方向波数の順ではない)ことに注意. 
  !   * スペクトルデータの格納のされ方については...
  !
  ! * x, y : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ
  !     real(8), dimension(0:im-1) および real(8), dimension(0:jm-1).
  !
  ! * ee_ で始まる関数が返す値はスペクトルデータに同じ.
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
  ! ee_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! ee_ChangeResolutionDomain :: 解像度, 領域設定の変更
  !
  !==== 座標変数
  !
  ! x_X, y_Y     ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight ::  重み座標を格納した 1 次元配列
  ! yx_X, yx_Y   :: 格子点データの XY 座標(X,Y)(格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! yx_ee :: スペクトルデータから格子データへの変換
  ! ee_yx :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! ee_Lapla_ee       :: スペクトルデータにラプラシアンを作用させる
  ! ee_LaplaInv_ee    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! ee_Dx_ee          :: スペクトルデータに X 微分を作用させる
  ! ee_Dy_ee          :: スペクトルデータに Y 微分を作用させる
  ! ee_Jacobian_ee_ee :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !==== 積分・平均
  !
  ! IntYX_yx, AvrYX_yx   :: 2 次元格子点データの全領域積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元格子点データの X 方向積分および平均
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元格子点データの Y 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  !
  !==== スペクトル解析
  !
  ! ee_EnergyFromStreamfunc_ee    :: 
  ! 流線関数からエネルギースペクトルを計算する
  !
  ! ee_EnstrophyFromStreamfunc_ee :: 
  ! 流線関数からエンストロフィースペクトルを計算する
  !
  !==== 補間計算
  !
  ! Interpolate_ee       :: 任意の点の値をスペクトルデータから計算する
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public ee_Initial                                       ! 初期化ルーチン
  public ee_ChangeResolutionDomain                        ! 解像度領域設定変更
  public yx_ee, ee_yx                                     ! 基本変換
  public ee_Lapla_ee, ee_LaplaInv_ee, ee_Dx_ee, ee_Dy_ee  ! 微分
  public ee_JacobianZ_ee, ee_Jacobian_ee_ee               ! 非線形計算
  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! 積分
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! 平均
  public ee_EnergyFromStreamfunc_ee                       ! エネルギー
  public ee_EnstrophyFromStreamfunc_ee                    ! エンストロフィー
  public Interpolate_ee                                   ! 補間
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! 座標変数

  integer   :: im=32, jm=32                      ! 格子点の設定(X,Y)
  integer   :: km=10, lm=10                      ! 切断波数の設定(X,Y)
  real(8)   :: xl=1.0, yl=1.0                    ! 領域の大きさ

  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()

  real(8), dimension(:),   pointer :: x_X => null()
                                         ! 格子点座標(X)
  real(8), dimension(:),   pointer :: y_Y => null()
                                         ! 格子点座標(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! 格子点重み(X)
                                         ! X 方向の格子点の間隔が格納してある.
  real(8), dimension(:),   pointer :: y_Y_Weight => null()
                                         ! 格子点重み(Y)
                                         ! Y 方向の格子点の間隔が格納してある.

  real(8), dimension(:,:), pointer :: yx_X => null()
                          ! 格子点(X)座標(2 次元)
                          ! 各格子点(i,j)の位置の X 座標を格納した格子データ
  real(8), dimension(:,:), pointer :: yx_Y => null()
                          ! 格子点(Y)座標(2 次元)
                          ! 各格子点(i,j)の位置の Y 座標を格納した格子データ


  integer, parameter :: nparams_max = 10 ! ee_Initial を呼べる最大回数
  type ee_param                          ! 解像度領域情報構造体
     integer   :: im, jm
     integer   :: km, lm
     real(8)   :: xl, yl
     integer, dimension(:),   pointer :: itj
     real(8), dimension(:),   pointer :: tj
     integer, dimension(:),   pointer :: iti
     real(8), dimension(:),   pointer :: ti
     real(8), dimension(:),   pointer :: x_X
     real(8), dimension(:),   pointer :: y_Y
     real(8), dimension(:),   pointer :: x_X_Weight
     real(8), dimension(:),   pointer :: y_Y_Weight
     real(8), dimension(:,:), pointer :: yx_X
     real(8), dimension(:,:), pointer :: yx_Y 
  end type ee_param
  type(ee_param) :: params(nparams_max)  ! 解像度領域情報
  integer :: nparams                     ! 解像度領域情報の個数

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, itj, tj, iti, ti, xl, yl
  save x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y
  save params, nparams

  contains
  !--------------- 初期化 -----------------
    subroutine ee_Initial(i,j,k,l,xmin,xmax,ymin,ymax,id)
      !
      ! スペクトル変換の格子点数, 波数, 領域の大きさを設定する.
      !
      ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      ! オプショナル引数 id を用いて異なる解像度, 領域を同時に
      ! 扱うことができる. ee_Initial を解像度, 領域ごとに呼んで
      ! 引数 id をキープし, ee_ChangeResolutionDomain で切替える. 
      !
      integer,intent(in) :: i           ! 格子点数(X)
      integer,intent(in) :: j           ! 格子点数(Y)
      integer,intent(in) :: K           ! 切断波数(X)
      integer,intent(in) :: l           ! 切断波数(Y)

      real(8),intent(in) :: xmin, xmax     ! X 座標範囲
      real(8),intent(in) :: ymin, ymax     ! Y 座標範囲

      integer, intent(out), optional :: id  ! 解像度領域情報番号

      character(len=3) cid
      integer :: ii, jj

      im = i         ; jm = j
      km = k         ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','ee_initial',&
              'too many call of ee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%xl = xl
      params(nparams)%yl = yl

      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%y_Y(0:jm-1))
      allocate(params(nparams)%y_Y_Weight(0:jm-1))
      allocate(params(nparams)%yx_X(0:jm-1,0:im-1))
      allocate(params(nparams)%yx_Y(0:jm-1,0:im-1))

      call ee_ChangeResolutionDomain(nparams)

      call p2init(jm,im,itj,tj,iti,ti)

      do ii=0,im-1
         x_X(ii) = xmin + xl/im*ii
      enddo
      x_X_Weight = xl/im

      do jj=0,jm-1
         y_Y(jj) = ymin + yl/jm*jj
      enddo
      y_Y_Weight = yl/jm

      yx_X = spread(x_X,1,jm)
      yx_Y = spread(y_Y,2,im)

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','ee_initial','ee_module (2009/07/31) is initialized')
      call MessageNotify('M','ee_initial',&
           'ResolutionDomain ID is '//trim(adjustl(cid)))
    end subroutine ee_initial

  !--------------- id 変更 -----------------
    subroutine ee_ChangeResolutionDomain(id)
      !
      ! 解像度, 領域設定の変更. ee_Initial で設定する際に
      ! かえってくるオプショナル引数 id の値を用いる. 
      ! 
      integer, intent(in) :: id

      if (id .gt. nparams .or. id .lt. 1) then
         write(*,*)"id is invalid"
      end if

      im = params(id)%im
      jm = params(id)%jm
      km = params(id)%km
      lm = params(id)%lm
      xl = params(id)%xl
      yl = params(id)%yl
      itj => params(id)%itj
      tj => params(id)%tj
      iti => params(id)%iti
      ti => params(id)%ti
      x_X => params(id)%x_X
      y_Y => params(id)%y_Y
      x_X_Weight => params(id)%x_X_Weight
      y_Y_Weight => params(id)%y_Y_Weight
      yx_X => params(id)%yx_X
      yx_Y => params(id)%yx_Y

    end subroutine ee_ChangeResolutionDomain

  !--------------- 基本変換 -----------------
    function yx_ee(ee)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:jm-1,0:im-1)             :: yx_ee 
      !(out) 格子点データ

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee
      !(in)  スペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call p2s2ga(lm,km,jm,im,ee,yx_ee,w,itj,tj,iti,ti)
    end function yx_ee

    function ee_yx(yx)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_yx
      !(out)  スペクトルデータ

      real(8), dimension(0:jm-1,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      real(8), dimension(jm*im)                     :: w
      real(8), dimension(0:jm-1,0:im-1)             :: yx_tmp
      ! 作業用配列

      yx_tmp = yx
      call p2g2sa(lm,km,jm,im,yx_tmp,ee_yx,w,itj,tj,iti,ti)

    end function ee_yx

  !--------------- 微分計算 -----------------
    function ee_Lapla_ee(ee)
      !
      ! 入力スペクトルデータにラプラシアン(∂xx+∂yy)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) をかける
      ! 計算を行っている. 
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Lapla_ee
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) 入力スペクトルデータ

      integer k,l
      ! 作業変数

      do k=-km,km
         do l=-lm,lm
            ee_Lapla_ee(l,k) = -((2*pi*k/xl)**2+(2*pi*l/yl)**2)*ee(l,k)
         enddo
      enddo
    end function ee_Lapla_ee

    function ee_LaplaInv_ee(ee)
      !
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy)**(-1)を作用する.
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) で割る
      ! 計算を行っている. k=l=0 成分には 0 を代入している. 
      !
      real(8), dimension(-lm:lm,-km:km)             :: ee_LaplaInv_ee
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee
      !(in) スペクトルデータ

      integer k,l

      do k=-km,km
         do l=-lm,lm
            if ( k.ne.0 .or. l.ne.0 ) then
               ee_LaplaInv_ee(l,k) = -ee(l,k)/((2*pi*k/xl)**2+(2*pi*l/yl)**2)
            else
               ee_LaplaInv_ee(l,k) = 0.0
            endif
         enddo
      enddo
    end function ee_LaplaInv_ee

    function ee_Dx_ee(ee)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Dx_ee
      !(out) スペクトルデータの X 微分

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) 入力スペクトルデータ

      integer k,l
      ! 作業変数

      do k=-km,km
         do l=-lm,lm
            ee_Dx_ee(l,k) = -(2*pi*k/xl)*ee(-l,-k)
         enddo
      enddo
    end function ee_Dx_ee

    function ee_Dy_ee(ee)
      !
      ! 入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 l をかけて
      ! sin(ky) <-> cos(ky) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_Dy_ee
      !(out) スペクトルデータの Y 微分

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee
      !(in) 入力スペクトルデータ

      integer k,l
      ! 作業変数

      do k=-km,km
         do l=-lm,lm
            ee_Dy_ee(l,k) = -(2*pi*l/yl)*ee(-l,-k)
         enddo
      enddo

    end function ee_Dy_ee

    function ee_Jacobian_ee_ee(ee_a,ee_b)
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
      real(8), dimension(-lm:lm,-km:km)              :: ee_Jacobian_ee_ee
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_a
      !(in) 1つ目の入力スペクトルデータ

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_b
      !(in) 2つ目の入力スペクトルデータ

      real(8), dimension((2*km+1)*(2*lm+1))          :: ws
      real(8), dimension(jm*im*3)                    :: wg
      ! 作業領域

      call p2ajcb(lm,km,jm,im,ee_a,ee_b,ee_Jacobian_ee_ee,ws,wg,itj,tj,iti,ti)

      ee_Jacobian_ee_ee  = (2*pi/xl)*(2*pi/yl) * ee_Jacobian_ee_ee

    end function ee_Jacobian_ee_ee

    function ee_JacobianZ_ee(ee_zeta)
      !
      ! 渦度スペクトルデータ ζ から流線関数と渦度のヤコビアン
      !
      !     J(ψ,ζ)=(∂xψ)(∂yζ)-(∂yψ)(∂xζ)
      !
      !  を計算する. ただしψ は (∂xx+∂yy)ψ=ζ を満たす流線関数である.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ee_JacobianZ_ee
      !(out) 流線関数と渦度のヤコビアン

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ee_Zeta
      !(in) 渦度スペクトルデータ

      real(8), dimension((2*km+1)*(2*lm+1))          :: ws
      real(8), dimension(jm*im*3)                    :: wg
      ! 作業領域

      call p2ajbs(lm,km,jm,im,yl/xl,ee_Zeta,ee_JacobianZ_ee,ws,wg,itj,tj,iti,ti)

      ee_JacobianZ_ee = (2*pi/xl)*(2*pi/yl)/(2*pi/yl)**2 * ee_JacobianZ_ee

    end function ee_JacobianZ_ee

  !--------------- 積分計算 -----------------
    function IntYX_yx(yx)
      !
      ! 2 次元格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx          
      !(in)  2 次元格子点データ

      real(8)                             :: IntYX_yx
      !(out) 積分値

      integer :: i, j
      ! 作業変数

      IntYX_yx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            IntYX_yx = IntYX_yx + yx(j,i) * y_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo
    end function IntYX_yx

    function y_IntX_yx(yx)
      !
      ! 2 次元格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1)          :: y_IntX_yx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i
      ! 作業変数

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)
      !
      ! 2 次元格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx      
      !(in)  2 次元格子点データ

      real(8), dimension(0:im-1)        :: x_IntY_yx 
      !(out) 積分された 1 次元(X)格子点データ

      integer :: j
      ! 作業変数

      x_IntY_yx = 0.0d0
      do j=0,jm-1
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntX_x(x)
      !
      ! 1 次元(X)格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:im-1)   :: x          !(in)  1 次元格子点データ
      real(8)                      :: IntX_x     !(out) 積分値

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntY_y(y)      ! Y 方向積分
      !
      ! 1 次元(Y)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1)   :: y          !(in)  1 次元格子点データ
      real(8)                      :: IntY_y     !(out) 積分値

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
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in)  2 次元格子点データ

      real(8)                             :: AvrYX_yx    
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
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1)          :: y_AvrX_yx
      !(out) 平均された 1 次元(Y)格子点データ

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)
      !
      ! 2 次元格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm-1,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_AvrY_yx
      !(out) 平均された 1 次元(X)格子点データ

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
      real(8), dimension(0:jm-1) :: y          !(in)  1 次元格子点データ
      real(8)                    :: AvrY_y     !(out) 平均値

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  !--------------- スペクトル計算 -----------------
    function ee_EnergyFromStreamfunc_ee(ee_StrFunc)
      !
      ! 流線関数からエネルギースペクトルを計算する. 
      !
      !   E_kl = (1/2)(k^2+l^2)|\psi_kl|^2
      !
      ! * E_kl の総和が場の平均運動エネルギーとなる. 
      ! * それに領域の面積をかけると全運動エネルギーとなる. 
      !
      real(8), dimension(-lm:lm,-km:km)    :: ee_EnergyFromStreamfunc_ee
      ! エネルギースペクトル

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee_StrFunc
      ! 流線関数

      integer k,l
      ! 作業変数

      do k=-km,km
         do l=-lm,lm
            ee_EnergyFromStreamfunc_ee(l,k) &
                 = 0.5 * ( (2*pi*k/xl)**2 + (2*pi*l/yl)**2 ) & 
                          * (ee_StrFunc(l,k)**2 + ee_StrFunc(-l,-k)**2)
         enddo
      enddo

    end function ee_EnergyFromStreamfunc_ee

    function ee_EnstrophyFromStreamfunc_ee(ee_StrFunc)
      !
      ! 流線関数からエンストロフィースペクトルを計算する. 
      !
      !   Q_kl = (1/2)(k^2+l^2)^2|\psi_kl|^2
      !
      ! * Q_kl の総和が場の平均エンストロフィーとなる. 
      ! * それに領域の面積をかけると全エンストロフィーとなる. 
      !

      real(8), dimension(-lm:lm,-km:km)    :: ee_EnstrophyFromStreamfunc_ee
      ! エンストロフィーースペクトル

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ee_StrFunc
      ! 流線関数

      integer k,l
      ! 作業変数

      do k=-km,km
         do l=-lm,lm
            ee_EnstrophyFromStreamfunc_ee(l,k) &
                 = 0.5 * ( (2*pi*k/xl)**2 + (2*pi*l/yl)**2 )**2 & 
                          * (ee_StrFunc(l,k)**2 + ee_StrFunc(-l,-k)**2)
         enddo
      enddo

    end function ee_EnstrophyFromStreamfunc_ee

  !--------------- 補間計算 -----------------
    function Interpolate_ee( ee_Data, x, y )
      real(8), intent(IN)  :: ee_data(-lm:lm,-km:km)  ! スペクトルデータ
      real(8), intent(IN)  :: x                       ! 補間する点の x 座標 
      real(8), intent(IN)  :: y                       ! 補間する点の y 座標 
      real(8)              :: Interpolate_ee          ! 補間した値

      integer :: k, l
      real(8) :: xx, yy

      xx =(2*PI/xl)*(x - x_X(0))
      yy =(2*PI/yl)*(y - y_Y(0))

      Interpolate_ee = ee_Data(0,0)

      ! l=0
      do k=1,km
         Interpolate_ee = Interpolate_ee &
              + 2*( ee_Data(0,k)*cos(k*xx) - ee_Data(0,-k)*sin(k*xx) )
      end do

      ! k=0
      do l=1,lm
         Interpolate_ee = Interpolate_ee &
              + 2*( ee_Data(l,0)*cos(l*yy) - ee_Data(-l,0)*sin(l*yy) )
      end do

      ! k*l > 0
      do l=1,lm
         do k=1,km
            Interpolate_ee = Interpolate_ee &
              + 2*(  ee_Data(l,k)*(   cos(k*xx)*cos(l*yy)   &
                                    - sin(k*xx)*sin(l*yy) ) &
                    -ee_Data(-l,-k)*(   sin(k*xx)*cos(l*yy)   &
                                      + cos(k*xx)*sin(l*yy) ) )
         end do
      end do

      ! k*l < 0
      do l=1,lm
         do k=1,km
            Interpolate_ee = Interpolate_ee &
              + 2*(  ee_Data(-l,k)*(   cos(k*xx)*cos(l*yy)   &
                                     + sin(k*xx)*sin(l*yy) ) &
                    -ee_Data(l,-k)*(   sin(k*xx)*cos(l*yy)   &
                                     - cos(k*xx)*sin(l*yy) ) )
         end do
      end do

    end function Interpolate_ee

end module ee_module
