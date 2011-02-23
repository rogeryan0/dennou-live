!--
!----------------------------------------------------------------------
!     Copyright (c) 2001-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  esc_module
!
!      spml/esc_module モジュールは 2 次元チャネル領域での流体運動を
!      スペクトル法により数値計算するための Fortran90 関数を提供する. 
!
!      内部で ISPACK/C2PACK の Fortran77 サブルーチンを呼んでいる. 
!      スペクトルデータおよび格子点データの格納方法については 
!      ISPACK/C2PACK のマニュアルを参照されたい.
!
!履歴  2001/10/07  竹広真一
!      2001/12/26  竹広真一  関数, 変数命名法変更
!      2002/03/25  竹広真一  モジュール名変更
!      2002/08/19  竹広真一  格子データ添字を gg -> xy に変更
!      2002/08/20  竹広真一  積分・平均関数追加
!      2005/03/15  竹広真一  xy -> yx に接頭子を変更
!      2006/03/06  竹広真一  コメントを RDoc 用に修正
!      2006/03/19  竹広真一  変数・手続き群の要約をコメントに追加
!      2007/11/12  竹広真一  ec_LaplaInv_ec 関数追加
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2009/01/09  竹広真一  esc_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に修正
!      2009/07/30  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!
!++
module esc_module
  !
  != esc_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: esc_module.f90,v 1.15 2009-07-30 12:50:22 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/esc_module モジュールは 2 次元チャネル領域での流体運動を
  ! スペクトル法により数値計算するための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK/C2PACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法については 
  ! ISPACK/C2PACK のマニュアルを参照されたい.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (es_, ec_, yx_, x_, y_) は, 返す値の形を示している.
  !   es_,ec_ :: スペクトルデータ(Y 方向 SIN 展開, COS 展開)
  !   yx_     :: 2 次元格子点データ
  !   x_      :: X 方向 1 次元格子点データ
  !   y_      :: Y 方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dx, Dy, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_es_es, _es_ec, _es, _ec, _yx, _x, _y) は, 
  !   入力変数の形スペクトルデータおよび格子点データであることを示している.
  !   _es    :: スペクトルデータ(Y 方向 SIN 型)
  !   _ec    :: スペクトルデータ(Y 方向 COS 型) 
  !   _es_es :: 2 つのスペクトルデータ
  !   _es_ec :: 2 つのスペクトルデータ
  !   _yx    :: 2 次元格子点データ, 
  !   _x     :: X 方向 1 次元格子点データ
  !   _y     :: Y 方向 1 次元格子点データ
  !
  !=== 各データの種類の説明
  !
  ! * yx : 2 次元格子点データ.
  !  * 変数の種類と次元は real(8), dimension(0:jm,0:im-1). 
  !  * im, jm はそれぞれ X, Y 座標の格子点数であり, 
  !    サブルーチン esc_Initial にてあらかじめ設定しておく. 
  !  * 第 1 次元が Y 座標の格子点位置番号, 第 2 次元が X 座標の格子点位置番号
  !    である (X, Y の順ではない)ことに注意.
  !
  ! * es : X 方向フーリエ型, Y 方向 SIN 型スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km,lm).
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, 
  !     サブルーチン esc_Initial にて
  !     あらかじめ設定しておく. 
  !   * スペクトルデータの格納のされ方については...
  !
  ! * ec : X 方向フーリエ型, Y 方向 COS 型スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-km:km,0:lm). 
  !   * km, lm はそれぞれ
  !     X, Y 方向の最大波数であり, 
  !     サブルーチン esc_Initial にてあらかじめ設定しておく.
  !   * スペクトルデータの格納のされ方については...
  !
  ! * x, y : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1) 
  !     および real(8), dimension(0:jm).
  !
  ! * es_, ec_ で始まる関数が返す値はスペクトルデータに同じ.
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
  ! esc_initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_X, y_Y     ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight ::  重み座標を格納した 1 次元配列
  ! yx_X, yx_Y   :: 格子点データの XY 座標(X,Y)(格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! yx_es, yx_ec :: スペクトルデータから格子データへの変換
  ! es_yx, ec_yx :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! es_Lapla_es, ec_Lapla_ec  :: スペクトルデータにラプラシアンを作用させる
  ! es_LaplaInv_es, ec_LaplaInv_ec :: スペクトルデータに
  !                                   ラプラシアンの逆変換を作用させる
  ! es_Dx_es, ec_Dx_ec  :: スペクトルデータに X 微分を作用させる
  ! ec_Dy_es, es_Dy_ec  :: スペクトルデータに Y 微分を作用させる
  ! es_Jacobian_es_es, ec_Jacobian_es_ec :: 2 つのスペクトルデータから
  !                                         ヤコビアンを計算する
  ! 
  !==== 積分・平均
  !
  ! IntYX_yx, AvrYX_yx   :: 2 次元格子点データの全領域積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元格子点データの X 方向積分および平均
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元格子点データの Y 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  !
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public esc_Initial                                      ! 初期化ルーチン
  public yx_es, yx_ec, es_yx, ec_yx                       ! 基本変換
  public es_Lapla_es, es_LaplaInv_es, es_Dx_es, ec_Dy_es  ! 微分
  public ec_Lapla_ec, ec_LaplaInv_ec, ec_Dx_ec, es_Dy_ec  ! 微分
  public es_Jacobian_es_es, ec_Jacobian_es_ec             ! 非線形計算
  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! 積分
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! 平均
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! 座標変数

  integer   :: im=32, jm=8                                ! 格子点の設定(X,Y)
  integer   :: km=10, lm=5                                ! 切断波数の設定(X,Y)
  real(8)   :: xl=2.0, yl=1.0                             ! 領域の大きさ

  integer,dimension(5)                  :: itj
  real(8),dimension(:),allocatable      :: tj
  integer,dimension(5)                  :: iti
  real(8),dimension(:),allocatable      :: ti

  real(8), dimension(:), allocatable    :: x_X
  ! 格子点座標(X)を格納した 1 次元配列

  real(8), dimension(:), allocatable    :: y_Y
  ! 格子点座標(X)を格納した 1 次元配列

  real(8), dimension(:), allocatable    :: x_X_Weight
  ! 重み座標(X)を格納した 1 次元配列. X 方向の格子点の間隔が格納してある.

  real(8), dimension(:), allocatable    :: y_Y_Weight
  ! 重み座標(Y)を格納した 1 次元配列. Y 方向の格子点の間隔が格納してある.

  real(8), dimension(:,:), allocatable  :: yx_X
  ! 各格子点(i,j)の位置の X 座標を格納した格子データ.

  real(8), dimension(:,:), allocatable  :: yx_Y
  ! 各格子点(i,j)の位置の Y 座標を格納した格子データ.

  real(8), dimension(:),   allocatable  :: wg, ws, wgj
  real(8), dimension(:,:), allocatable  :: yx_work,es_work,ec_work
  real(8), parameter  ::  pi=3.1415926535897932385D0

  save im, jm, km, lm, itj, tj, iti, ti, xl, yl
  save x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y

  contains
  !--------------- 初期化 -----------------
    subroutine esc_Initial(i,j,k,l,xmin,xmax,ymin,ymax)
      !
      ! スペクトル変換の格子点数, 波数, 領域の大きさを設定する.
      !
      ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      integer,intent(in) :: i           !(in) 格子点数(X)
      integer,intent(in) :: j           !(in) 格子点数(Y)
      integer,intent(in) :: k           !(in) 切断波数(X)
      integer,intent(in) :: l           !(in) 切断波数(Y)

      real(8),intent(in) :: xmin, xmax     !(in) X 座標範囲
      real(8),intent(in) :: ymin, ymax     !(in) Y 座標範囲

      integer :: ii, jj

      im = i         ; jm = j
      km = k         ; lm = l
      xl = xmax-xmin ; yl = ymax-ymin

      allocate(tj(jm*6),ti(im*2))

      call c2init(jm,im,itj,tj,iti,ti)

      allocate(x_X(0:im-1), x_X_Weight(0:im-1))
      allocate(y_Y(0:jm), y_Y_Weight(0:jm))
      allocate(yx_X(0:jm,0:im-1), yx_Y(0:jm,0:im-1))

      do ii=0,im-1
         x_X(ii) = xmin + xl/im*ii
      enddo
      x_X_Weight = xl/im

      do jj=0,jm
         y_Y(jj) = ymin + yl/jm*jj
      enddo
      y_Y_Weight(0) = yl/(2*jm)
      y_Y_Weight(1:jm-1) = yl/jm 
      y_Y_Weight(jm) = yl/(2*jm)

      yx_X = spread(x_X,1,jm+1)
      yx_Y = spread(y_Y,2,im)

      call MessageNotify('M','esc_initial', &
           'esc_module (2009/07/30) is initialized')

    end subroutine esc_Initial

  !--------------- 基本変換 -----------------
    function yx_es(es)
      !
      ! SIN(Y)型スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_es
      !(out) 格子点データ

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)型スペクトルデータ

      real(8)                                      :: wg((jm+1)*im)
      ! 作業領域

      call c2s2ga(lm,km,jm,im,es,yx_es,wg,itj,tj,iti,ti,1)
    end function yx_es

    function yx_ec(ec)
      !
      ! COS(Y)型スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:jm,0:im-1)              :: yx_ec
      !(out) 格子点データ

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) COS(Y)型スペクトルデータ

      real(8)                                      :: wg((jm+1)*im)
      ! 作業領域

      call c2s2ga(lm,km,jm,im,ec,yx_ec,wg,itj,tj,iti,ti,2)
    end function yx_ec

    function es_yx(yx)
      !
      ! 格子データから SIN(Y)型スペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,lm)                :: es_yx
      !(out) SIN(Y)型スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      real(8)                                      :: wg((jm+1)*im)
      real(8)                                      :: yx_work(0:jm,0:im-1)
      ! 作業領域

      yx_work = yx
      call c2g2sa(lm,km,jm,im,yx_work,es_yx,wg,itj,tj,iti,ti,1)
    end function es_yx

    function ec_yx(yx)
      !
      ! 格子データから COS(Y)型スペクトルデータへ変換する.
      !
      real(8), dimension(-km:km,0:lm)              :: ec_yx
      !(in) COS(Y)型スペクトルデータ

      real(8), dimension(0:jm,0:im-1), intent(in)  :: yx
      !(out) 格子点データ

      real(8)                                      :: wg((jm+1)*im)
      real(8)                                      :: yx_work(0:jm,0:im-1)
      ! 作業領域

      yx_work = yx
      call c2g2sa(lm,km,jm,im,yx_work,ec_yx,wg,itj,tj,iti,ti,2)
    end function ec_yx

  !--------------- 微分計算 -----------------
    function es_Lapla_es(es)
      !
      ! SIN(Y)型入力スペクトルデータにラプラシアン(∂xx+∂yy)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) をかける
      ! 計算を行っている. 
      !
      real(8), dimension(-km:km,lm)                :: es_Lapla_es
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)型入力スペクトルデータ

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Lapla_es(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*es(k,l)
         enddo
      enddo
    end function es_Lapla_es

    function ec_Lapla_ec(ec)
      !
      ! COS(Y)型入力スペクトルデータにラプラシアン(∂xx+∂yy)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) をかける
      ! 計算を行っている. 
      !
      real(8), dimension(-km:km,0:lm)                :: ec_Lapla_ec
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-km:km,0:lm), intent(in)    :: ec
      !(in) COS(Y)型入力スペクトルデータ

      integer k,l

      do l=0,lm
         do k=-km,km
            ec_Lapla_ec(k,l) = -((2*pi*k/xl)**2+(pi*l/yl)**2)*ec(k,l)
         enddo
      enddo
    end function ec_Lapla_ec

    function es_LaplaInv_es(es)   ! スペクトル SINY に作用する逆 \lapla 演算子
      !
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy)**(-1)を作用する.
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) で割る
      ! 計算を行っている. 
      !
      real(8), dimension(-km:km,lm)                :: es_LaplaInv_es
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)型スペクトルデータ

      integer k,l

      do l=1,lm
         do k=-km,km
            es_LaplaInv_es(k,l) = -es(k,l)/((2*pi*k/xl)**2+(pi*l/yl)**2)
         enddo
      enddo
    end function es_LaplaInv_es

    function ec_LaplaInv_ec(ec)   ! スペクトル COSY に作用する逆 \lapla 演算子
      !
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy)**(-1)を作用する.
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (k**2 + l**2) で割る
      ! 計算を行っている. 
      !
      real(8), dimension(-km:km,0:lm)              :: ec_LaplaInv_ec
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) SIN(Y)型スペクトルデータ

      integer k,l

      if ( ec(0,0) .ne. 0.0D0 ) then
           call MessageNotify('W','ec_LaplaInv_ec', &
                              '0-0 component of input data is not zero.')
           call MessageNotify('W','ec_LaplaInv_ec', &
                              '0-0 component of output set to zero.')
      endif

      do l=1,lm
         do k=-km,km
            ec_LaplaInv_ec(k,l) = -ec(k,l)/((2*pi*k/xl)**2+(pi*l/yl)**2)
         enddo
      enddo

      do k=1,km
         ec_LaplaInv_ec(k,0)  = -ec(k,0)/(2*pi*k/xl)**2
         ec_LaplaInv_ec(-k,0) = -ec(-k,0)/(2*pi*k/xl)**2
      enddo

      ec_LaplaInv_ec(0,0) = 0.0

    end function ec_LaplaInv_ec

    function es_Dx_es(es)
      !
      ! SIN(Y)型入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,lm)                :: es_Dx_es
      !(out) スペクトルデータの X 微分

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)型入力スペクトルデータ

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Dx_es(k,l)  =  (-2*pi*k/xl)*es(-k,l)
         enddo
      enddo
    end function es_Dx_es

    function ec_Dx_ec(ec)
      !
      ! COS(Y)型入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 k をかけて
      ! sin(kx) <-> cos(kx) 成分に入れ換える計算を行っている.
      !
      real(8), dimension(-km:km,0:lm)                :: ec_Dx_ec
      !(out) スペクトルデータの X 微分

      real(8), dimension(-km:km,0:lm), intent(in)    :: ec
      !(in) COS(Y)型入力スペクトルデータ

      integer k,l

      do l=0,lm
         do k=-km,km
            ec_Dx_ec(k,l)  =  (-2*pi*k/xl)*ec(-k,l)
         enddo
      enddo
    end function ec_Dx_ec

    function ec_Dy_es(es)   ! スペクトル SINY に作用する y 微分演算子
      !
      ! SIN(Y)型入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 l をかけている. 
      !
      real(8), dimension(-km:km,0:lm)              :: ec_Dy_es
      !(out) スペクトルデータの Y 微分, COS(Y)型.

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) SIN(Y)型入力スペクトルデータ

      integer k,l

      do k=-km,km
         ec_Dy_es(k,0)  =  0.0
      enddo
      do l=1,lm
         do k=-km,km
            ec_Dy_es(k,l)  =  (pi*l/yl)*es(k,l)
         enddo
      enddo
    end function ec_Dy_es

    function es_Dy_ec(ec)   ! スペクトル COSY に作用する y 微分演算子
      !
      ! COS(Y)型入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 l をかけている. 
      !
      real(8), dimension(-km:km,lm)                 :: es_Dy_ec
      !(out) スペクトルデータの Y 微分, SIN(Y)型.

      real(8), dimension(-km:km,0:lm), intent(in)   :: ec
      !(in) COS(Y)型入力スペクトルデータ

      integer k,l

      do l=1,lm
         do k=-km,km
            es_Dy_ec(k,l)  =  -(pi*l/yl)*ec(k,l)
         enddo
      enddo
    end function es_Dy_ec

 !------------------- 非線形項計算 ----------------------
    function es_Jacobian_es_es(es_a,es_b) !スペクトル SINY に作用するヤコビアン
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
      real(8), dimension(-km:km,lm)                :: es_Jacobian_es_es
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-km:km,lm), intent(in)    :: es_A,es_B
      !(in) 2つの SIN(Y)型入力スペクトルデータ

      real(8)                                      :: ws((2*km+1)*(lm+1))
      real(8)                                      :: wgj((jm+1)*im*3)
      real(8)                                      :: es_work(-km:km,lm)
      ! 作業領域

      integer k,l

      call c2ajcb(lm,km,jm,im,es_A,es_B,es_work,ws,wgj,itj,tj,iti,ti)

      do l=1,lm
         do k=-km,km
            es_Jacobian_es_es(k,l) = (2*pi/xl)*(pi/yl)*es_work(k,l)
         enddo
      enddo
    end function es_Jacobian_es_es

    function ec_Jacobian_es_ec(es,ec)  ! スペクトル COS(Y) に作用するヤコビアン
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
      real(8), dimension(-km:km,0:lm)              :: ec_Jacobian_es_ec
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-km:km,lm), intent(in)    :: es
      !(in) 1つ目の SIN(Y)型入力スペクトルデータ

      real(8), dimension(-km:km,0:lm), intent(in)  :: ec
      !(in) 2つ目の COS(Y)型入力スペクトルデータ

      real(8)                                      :: ws((2*km+1)*(lm+1))
      real(8)                                      :: wgj((jm+1)*im*3)
      real(8)                                      :: ec_work(-km:km,0:lm)
      ! 作業領域

      integer k,l

      call c2ajcc(lm,km,jm,im,es,ec,ec_work,ws,wgj,itj,tj,iti,ti)

      do l=0,lm
         do k=-km,km
            ec_Jacobian_es_ec(k,l) = (2*pi/xl)*(pi/yl)*ec_work(k,l)
         enddo
      enddo
    end function ec_Jacobian_es_ec

  !--------------- 積分計算 -----------------
    function IntYX_yx(yx)
      !
      ! 2 次元格子点データの全領域積分および平均.
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

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
      ! 1 次元(X)格子点データの X 方向積分
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm)          :: y_IntX_yx
      !(out) X 方向に積分された 1 次元(Y)格子点データ

      integer :: i

      y_IntX_yx = 0.0d0
      do i=0,im-1
         y_IntX_yx(:) = y_IntX_yx(:) + yx(:,i) * x_X_Weight(i)
      enddo
    end function y_IntX_yx

    function x_IntY_yx(yx)
      !
      ! 2 次元格子点データの Y 方向積分
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)        :: x_IntY_yx
      !(out) Y 方向に積分された 1 次元(X)格子点データ

      integer :: j

      x_IntY_yx = 0.0d0
      do j=0,jm
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntX_x(x)      ! X 方向積分
      !
      ! 1 次元(X)格子点データの X 方向積分
      !
      real(8), dimension(0:im-1)   :: x
      !(in) 1 次元(X)格子点データ

      real(8)                      :: IntX_x
      !(out) 積分値

      IntX_x = sum(x*x_X_Weight)
    end function IntX_x

    function IntY_y(y)      ! Y 方向積分
      !
      ! 1 次元(Y)格子点データの Y 方向積分
      !
      real(8), dimension(0:jm)   :: y
      !(in) 1 次元(Y)格子点データ

      real(8)                    :: IntY_y
      !(out) 積分値

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

  !--------------- 平均計算 -----------------
    function AvrYX_yx(yx)    ! 全領域平均
      !
      ! 2 次元格子点データの全領域平均
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8)                           :: AvrYX_yx
      !(out) 平均値

      AvrYX_yx = IntYX_yx(yx)/(sum(x_X_weight)*sum(y_Y_weight))
    end function AvrYX_yx

    function y_AvrX_yx(yx)   ! X 方向平均
      !
      ! 1 次元(X)格子点データの X 方向平均
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm)          :: y_AvrX_yx
      !(out) X 方向に平均された 1 次元(Y)格子点データ

      y_AvrX_yx = y_IntX_yx(yx)/sum(x_X_weight)
    end function y_AvrX_yx

    function x_AvrY_yx(yx)   ! Y 方向平均
      !
      ! 2 次元格子点データの Y 方向平均
      !
      real(8), dimension(0:jm,0:im-1)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)        :: x_AvrY_yx
      !(out) Y 方向に平均された 1 次元(X)格子点データ

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

    function AvrX_x(x)       ! X 方向平均
      !
      ! 1 次元(X)格子点データの X 方向平均
      !
      real(8), dimension(0:im-1)   :: x
      !(in) 1 次元(X)格子点データ

      real(8)                      :: AvrX_x
      !(out) 平均値

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrY_y(y)       ! Y 方向平均
      !
      ! 1 次元(Y)格子点データの Y 方向平均
      !
      real(8), dimension(0:jm)   :: y
      !(in) 1 次元(Y)格子点データ

      real(8)                    :: AvrY_y
      ! 平均値

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

  end module esc_module
