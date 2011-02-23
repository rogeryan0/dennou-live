!--
!----------------------------------------------------------------------
! Copyright(c) 2009-2010 SPMDODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  tee_module
!
!    spml/tee_module モジュールは平行平板間での 3 次元流体運動を
!    スペクトル法によって数値計算するための Fortran90 関数を提供する
!    ものである. 
!
!    水平方向にフーリエ数変換および上下の境界壁を扱うための
!    チェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
!    関数を提供する. 
!
!    内部で ee_module, at_module を用いている. 最下部ではフーリエ
!    およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
!    サブルーチンを用いている.
!
!
!履歴  2009/12/19  竹広真一  新規作成
!      2010/03/10  佐々木洋平  threadprivate 削除(コンパイラ依存)
!
!凡例
!      データ種類と index
!        x : 水平(X)        y : 水平(Y)        z : 鉛直
!        e : フーリエ変換スペクトル
!        l : フーリエ関数スペクトル(X 方向波数)
!        m : フーリエ関数スペクトル(Y 方向波数)
!        t : チェビシェフ関数スペクトル
!        a : 任意の次元
!
!        zyx : 3 次元格子点データ
!        yx  : 水平 2 次元格子点データ
!        zy  : 鉛直 2 次元格子点データ
!        zx  : 鉛直 2 次元格子点データ
!
!        zee : 水平スペクトル鉛直格子点データ
!        tee : スペクトルデータ鉛直チェビシェフデータ
!
!++
module tee_module
  !
  != tee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: tee_module.f90,v 1.2 2010-03-02 10:09:06 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !    spml/tee_module モジュールは平行平板間での 3 次元流体運動を
  !    スペクトル法によって数値計算するための Fortran90 関数を提供する
  !    ものである. 
  !
  !    水平方向にフーリエ数変換および上下の境界壁を扱うための
  !    チェビシェフ変換を用いる場合のスペクトル計算のためのさまざまな
  !    関数を提供する. 
  !
  !    内部で ee_module, at_module を用いている. 最下部ではフーリエ
  !    およびチェビシェフ変換のエンジンとして ISPACK の Fortran77 
  !    サブルーチンを用いている.
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !
  ! * 関数名の先頭 (tee_, zyx_, zee_, ee_, yx_, x_, y_, z_, a_) は, 
  !   返す値の形を示している.
  !   tee_  :: スペクトルデータ(2 重フーリエ・チェビシェフ変換)
  !   zyx_ :: 3 次元格子点データ(水平 2 次元鉛直 1 次元・)
  !   zee_ :: 水平スペクトル, 鉛直格子点データ
  !   e2a_ :: 1 次元化した水平スペクトル, 任意座標データ
  !   aee_ :: 任意座標データ, 水平スペクトルデータ
  !
  ! * 関数名の間の文字列(Dx, Dy, Dz, Lapla,..)
  !   は, その関数の作用を表している.
  !
  ! * 関数名の最後 (tee_, zyx_, zee_, ee_, yx_, x_, y_, z_, a_) は, 入力変数の
  !   形がスペクトルデータおよび格子点データであることを示している.
  !   _tee :: スペクトルデータ(2 重フーリエ・チェビシェフ変換)
  !   _zyx :: 3 次元格子点データ(水平 2 次元鉛直 1 次元・)
  !   _zee :: 水平スペクトル, 鉛直格子点データ
  !   _e2a :: 1 次元化した水平スペクトル, 任意座標データ
  !   _aee :: 任意座標データ, 水平スペクトルデータ
  !
  !=== 各データの種類の説明
  !
  ! * zyx : 3 次元格子点データ(鉛直, 水平 2 次元)
  !   * 変数の種類と次元は real(8), dimension(0:km,0:jm-1,0:im-1). 
  !   * im, jm, km はそれぞれ水平 X, Y, 鉛直 Z 座標の格子点数であり, 
  !     サブルーチン tee_Initial にてあらかじめ設定しておく.
  !
  ! * tee : スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension(0:nm,-mm:mm,-lm:lm). 
  !   * lm, mm は X,Y 方向最大波数, nm はチェビシェフ多項式の最大次数
  !     であり, サブルーチン tee_Initial にてあらかじめ設定しておく. 
  !
  ! * zee : 水平スペクトル, 鉛直格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:km,-mm:mm,-lm:lm).
  !
  ! * e2a : 1 次元化した水平スペクトル, 任意座標データ
  !   * 変数の種類と次元は real(8), dimension((2*mm+1)*(2*lm+1),:)
  !
  ! * aee :   任意座標データ, 水平スペクトルデータ
  !   * 変数の種類と次元は real(8), dimension(:,-mm:mm,-lm:lm).
  !
  ! * tee_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * zyx_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * zee_ で始まる関数が返す値は水平スペクトル, 動径格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  ! 
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! tee_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  ! 
  !==== 座標変数
  !
  ! x_X, y_Y, z_Z                :: 格子点座標(水平 X,Y, 鉛直 Z 座標)を
  !                                 格納した1 次元配列
  ! x_X_Weight, y_X_Weight, z_Z_Weight :: 重み座標を格納した 1 次元配列
  ! zyx_X, zyx_Y, zyx_Z          :: 格子点データの水平鉛直座標(X,Y,Z)
  !                                 (格子点データ型 3 次元配列)
  ! yx_X, yx_Y                   :: 格子点データの水平座標(X,Y)
  ! zy_Z, zy_Y                   :: 格子点データの鉛直水平座標(Z,Y)
  ! zx_Z, zx_X                   :: 格子点データの鉛直水平座標(Z,X)
  !                                 (格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! zyx_tee, tee_zyx :: スペクトルデータと 3 次元格子データの間の変換
  !                     (2 重フーリエ, チェビシェフ変換)
  ! zyx_zee, zee_zyx :: 3 次元格子データと水平スペクトル・鉛直格子データとの間
  !                     の変換 (2 重フーリエ変換)
  ! zee_tee, tee_zee :: スペクトルデータと水平スペクトル・鉛直格子データとの間
  !                     の変換 (チェビシェフ変換)
  ! ee_yx, yx_ee     :: スペクトルデータと 2 次元水平格子データの間の変換
  !                     (2 重フーリエ変換) 
  ! az_at, at_az     :: 同時に複数個行う (チェビシェフ変換)格子データと
  !                     チェビシェフデータの間の変換を
  ! e2a_aee, aee_e2a :: 水平スペクトル軸を 1 次元化し転置, 転置 ２次元化する.
  !
  !==== 微分
  !
  ! tee_Dx_tee          :: スペクトルデータに動径微分∂/∂x を作用させる
  ! tee_Dy_tee          :: スペクトルデータに動径微分∂/∂y を作用させる
  ! tee_Dz_tee          :: スペクトルデータに動径微分∂/∂z を作用させる
  !
  ! tee_Lapla_tee       :: スペクトルデータにラプラシアンを作用させる
  ! tee_LaplaH_tee      :: スペクトルデータに水平ラプラシアンを作用させる
  ! tee_LaplaHInv_tee   :: スペクトルデータに逆水平ラプラシアンを作用させる
  !
  ! tee_Div_zyx_zyx_zyx :: ベクトル成分である 3 つの格子データに
  !                        発散を作用させる
  !
  !==== トロイダルポロイダル計算用微分
  !
  ! tee_ZRot_zyx_zyx         :: ベクトル v の渦度と動径ベクトル r の内積
  !                             z・(▽×v) を計算する
  ! tee_ZRotRot_zyx_zyx_zyx  :: ベクトルの v の z・(▽×▽×v) を計算する
  ! tee_Potential2Vector     :: トロイダルポロイダルポテンシャルから
  !                             ベクトル場を計算する
  ! tee_Potential2Rotation   :: トロイダルポロイダルポテンシャルで表される
  !                             非発散ベクトル場の回転の各成分を計算する
  !
  !==== ポロイダル/トロイダルモデル用スペクトル解析
  !
  ! zee_ToroidalEnergySpectrum_tee, zk_ToroidalEnergySpectrum_tee   ::
  !     トロイダルポテンシャルからエネルギーのフーリエ各成分を計算する
  ! zee_PoloidalEnergySpectrum_tee, zk_PoloidalEnergySpectrum_tee   :: 
  !     ポロイダルポテンシャルからエネルギーのフーリエ各成分を計算する
  !
  !==== 境界値問題
  !
  ! tee_BoundariesTau, tee_BoundariesGrid, tee_Boundaries                ::
  !     ディリクレ, ノイマン境界条件を適用する(タウ法, 選点法)
  !
  ! tee_TorBoundariesTau, tee_TorBoundariesGrid, tee_TorBoundaries       ::
  !     速度トロイダルポテンシャルの境界条件を適用する(タウ法,選点法) 
  !
  ! zee_LaplaPol2Pol_zee, tee_LaplaPol2PolGrid_tee                       ::
  !     速度ポロイダルポテンシャルΦを▽^2Φから求める
  !     (入出力がそれぞれチェビシェフ格子点,チェビシェフ係数)
  !
  ! tee_TorMagBoundariesTau, tee_TorMagBoundariesGrid, tee_TorMagBoundaries ::
  !     磁場トロイダルポテンシャルの境界条件を適用する(タウ法, 選点法)
  !
  ! tee_PolMagBoundariesTau, tee_PolMagBoundariesGrid, tee_PolMagBoundaries ::
  !     磁場トロイダルポテンシャル境界の境界条件を適用する(タウ法, 選点法)
  !
  !==== 積分・平均(3 次元データ)
  !
  ! IntZYX_zyx, AvrZYX_zyx     :: 3 次元格子点データの全領域積分および平均
  ! z_IntYX_zyx, z_AvrYX_zyx   :: 3 次元格子点データの水平積分および平均
  ! y_IntZX_zyx, y_AvrZX_zyx   :: 3 次元格子点データのZX積分および平均
  ! z_IntZY_zyx, z_AvrZY_zyx   :: 3 次元格子点データのZY積分および平均
  ! zy_IntX_zyx, zy_AvrX_zyx   :: 3 次元格子点データの水平X方向積分および平均
  ! zx_IntY_zyx, zx_AvrY_zyx   :: 3 次元格子点データの水平Y方向積分および平均
  ! zx_IntZ_zyx, zx_AvrZ_zyx   :: 3 次元格子点データの鉛直方向積分および平均
  !
  !==== 積分・平均(2 次元データ)
  !
  ! IntYX_yx, AvrYX_yx :: 2 次元格子点データの水平積分および平均
  ! IntZX_zx, AvrZX_zx :: 2 次元(ZX)格子点データのZX積分および平均
  ! IntZY_zy, AvrZY_zy :: 2 次元(ZY)格子点データのZY積分および平均 
  ! y_IntX_yx, y_AvrX_yx   :: 水平 2 次元格子点データのX方向積分および平均
  ! x_IntY_yx, x_AvrY_yx   :: 水平2 次元格子点データのY方向積分および平均
  ! z_IntX_zx, z_AvrX_zx   :: 2 次元(ZX)格子点データのX方向積分および平均
  ! x_IntZ_zx, x_AvrZ_zx   :: 2 次元(ZX)格子点データのZ方向積分および平均
  ! z_IntY_zy, z_AvrY_zy   :: 2 次元(ZY)格子点データのY方向積分および平均
  ! y_IntZ_zy, y_AvrZ_zy   :: 2 次元(ZY)格子点データのZ方向積分および平均
  !
  !==== 積分・平均(1 次元データ)
  !
  ! IntX_x, AvrX_x  :: 1 次元(X)格子点データのX方向積分および平均
  ! IntY_y, AvrY_y  :: 1 次元(Y)格子点データのY方向積分および平均
  ! IntZ_z, AvrZ_z  :: 1 次元(Z)格子点データのZ方向積分および平均
  !
  !==== 補間計算
  !
  ! Interpolate_tee :: スペクトルデータから任意の点の値を補間する. 
  ! 
  use dc_message
  use lumatrix
  use ee_module
  use at_module, z_Z => g_X, z_Z_WEIGHT => g_X_WEIGHT, &
                 at_az => at_ag, az_at => ag_at, &
                 t_z => t_g, z_t => g_t, &
                 t_Dz_t => t_Dx_t, at_Dz_at => at_Dx_at
  implicit none
  private

  public tee_Initial

  public x_X, x_X_Weight
  public y_Y, y_Y_Weight
  public z_Z, z_Z_Weight
  public yx_X, yx_Y, zy_Z, zy_Y, zx_X, zx_Z
  public zyx_X, zyx_Y, zyx_Z
  public zee_Z
  public tee_VMiss

  public ee_yx, yx_ee
  public at_Dz_at, t_Dz_t, az_at, at_az, z_t, t_z
  public zyx_tee, tee_zyx, zyx_zee, zee_zyx, zee_tee, tee_zee
  public aee_e2a, e2a_aee, ee_e2, e2_ee
  public tee_Dx_tee, tee_Dy_tee, tee_Dz_tee
  public tee_Lapla_tee, tee_LaplaH_tee, tee_LaplaHInv_tee

  public zy_IntX_zyx, zx_IntY_zyx, yx_IntZ_zyx
  public x_IntZY_zyx, y_IntZX_zyx, z_IntYX_zyx
  public IntZYX_zyx

  public x_IntY_yx, y_IntX_yx, IntYX_yx
  public z_IntY_zy, y_IntZ_zy, IntZY_zy
  public z_IntX_zx, x_IntZ_zx, IntZX_zx
  public IntX_x, IntY_y, IntZ_z

  public zy_AvrX_zyx, zx_AvrY_zyx, yx_AvrZ_zyx
  public x_AvrZY_zyx, y_AvrZX_zyx, z_AvrYX_zyx
  public AvrZYX_zyx

  public x_AvrY_yx, y_AvrX_yx, AvrYX_yx
  public z_AvrY_zy, y_AvrZ_zy, AvrZY_zy
  public z_AvrX_zx, x_AvrZ_zx, AvrZX_zx
  public AvrX_x, AvrY_y, AvrZ_z

  public tee_ZRot_zyx_zyx, tee_ZRotRot_zyx_zyx_zyx
  public tee_Potential2vector, tee_Potential2Rotation

  public Interpolate_tee

  public zee_ToroidalEnergySpectrum_tee ! nz_ToroidalEnergySpectrum_wt
  public zee_PoloidalEnergySpectrum_tee ! nz_PoloidalEnergySpectrum_wt

  public tee_Boundaries, zee_LaplaPol2Pol_zee, tee_TorBoundaries
  public tee_TormagBoundaries, tee_PolmagBoundaries

  public tee_BoundariesTau, tee_TorBoundariesTau
  public tee_TormagBoundariesTau, tee_PolmagBoundariesTau

  public tee_BoundariesGrid, tee_LaplaPol2PolGrid_tee, tee_TorBoundariesGrid
  public tee_TormagBoundariesGrid, tee_PolmagBoundariesGrid

  interface tee_Boundaries
     module procedure tee_BoundariesTau
  end interface

  interface tee_TorBoundaries
     module procedure tee_TorBoundariesTau
  end interface

  interface tee_TorMagBoundaries
     module procedure tee_TorMagBoundariesTau
  end interface

  interface tee_PolMagBoundaries
     module procedure tee_PolMagBoundariesTau
  end interface

  integer            :: im=32, jm=32, km=16  ! 格子点の設定(X,Y,Z)
  integer            :: lm=10, mm=10, nm=16  ! 切断波数の設定(水平X,Y, 鉛直Z)
  real(8)            :: xl, yl, zl           ! 領域の大きさ(水平X,Y, 鉛直Z)

  real(8), parameter :: pi=3.1415926535897932385D0

  real(8), dimension(:,:,:), allocatable :: zyx_X, zyx_Y, zyx_Z    ! 座標
  real(8), dimension(:,:),   allocatable :: zy_Z, zy_Y             ! 座標
  real(8), dimension(:,:),   allocatable :: zx_Z, zx_X             ! 座標
  real(8), dimension(:,:,:), allocatable :: zee_Z                  ! 座標

  real(8) :: tee_VMiss = -999.0        ! 欠損値

  save im, jm, km, lm, mm, nm, xl, yl, zl

  contains
  !--------------- 初期化 -----------------
   subroutine tee_Initial(i,j,k,l,m,n,xmin,xmax,ymin,ymax,zmin,zmax)
     !
     ! スペクトル変換の格子点数, 波数, 各座標の範囲を設定する.
     !
     ! 他の関数を呼ぶ前に, 最初にこのサブルーチンを呼んで初期設定を
     ! しなければならない. 
     !
     integer,intent(in) :: i              ! 格子点数(水平X)
     integer,intent(in) :: j              ! 格子点数(水平Y)
     integer,intent(in) :: k              ! 格子点数(鉛直Z)
     integer,intent(in) :: l              ! 切断波数(水平X波数)
     integer,intent(in) :: m              ! 切断波数(水平Y波数)
     integer,intent(in) :: n              ! 切断波数(鉛直Z波数)

     real(8),intent(in) :: xmin, xmax     ! X 方向領域
     real(8),intent(in) :: ymin, ymax     ! Y 方向領域
     real(8),intent(in) :: zmin, zmax     ! Z 方向領域

     integer            :: id

     im = i  ; jm = j ; km = k
     lm = l  ; mm = m ; nm = n 
     xl = xmax - xmin
     yl = ymax - ymin
     zl = zmax - zmin

     call ee_Initial(im,jm,lm,mm,xmin,xmax,ymin,ymax,id)

     call at_Initial(km,nm,zmin,zmax)

     allocate(zyx_X(0:km,0:jm-1,0:im-1))
     allocate(zyx_Y(0:km,0:jm-1,0:im-1))
     allocate(zyx_Z(0:km,0:jm-1,0:im-1))

     allocate(zy_Z(0:km,0:jm-1))
     allocate(zy_Y(0:km,0:jm-1))
     allocate(zx_Z(0:km,0:im-1))
     allocate(zx_X(0:km,0:im-1))

     allocate(zee_Z(0:nm,-mm:mm,-lm:lm))

     zyx_X = spread(yx_X,1,km+1)
     zyx_Y = spread(yx_Y,1,km+1)
     zyx_Z = spread(spread(z_Z,2,jm),3,im)

     zy_Z = spread(z_Z,2,jm)
     zy_Y = spread(y_Y,1,km+1)

     zx_Z = spread(z_Z,2,im)
     zx_X = spread(x_X,1,km+1)

     zee_Z = spread(spread(z_Z,2,2*mm+1),3,2*lm+1)

     call MessageNotify('M','tee_initial', &
          'tee_module (2009/12/19) is initialized')

   end subroutine tee_Initial

  !--------------- 基本変換 -----------------

    function zyx_tee(tee)
      !
      ! スペクトルデータから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 重フーリエチェビシェフスペクトルデータ
      real(8), dimension(0:km,0:jm-1,0:im-1)             :: zyx_tee
      !(out) 3 次元格子点データ

      zyx_tee = zyx_zee(zee_tee(tee))

    end function zyx_tee

    function tee_zyx(zyx)
      !
      ! 3 次元格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ
      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_zyx
      !(out) 2 重フーリエチェビシェフスペクトルデータ

      tee_zyx = tee_zee(zee_zyx(zyx))

    end function tee_zyx

    function zyx_zee(zee)
      !
      ! 水平スペクトル・鉛直格子点データから 3 次元格子点データへ(逆)変換する.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm), intent(in) :: zee
      !(in) 2 次元水平スペクトル・鉛直格子点データ
      real(8), dimension(0:km,0:jm-1,0:im-1)             :: zyx_zee
      !(out) 3 次元格子点データ

      integer :: k

      do k = 0, km
         zyx_zee(k,:,:) = yx_ee(zee(k,:,:))
      enddo

    end function zyx_zee

    function zee_zyx(zyx)
      !
      ! 3 次元格子データから水平スペクトル・鉛直格子点データへ(正)変換する.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ
      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_zyx
      !(out) 2 次元スペクトル・鉛直格子点データ

      integer :: k

      do k = 0, km
         zee_zyx(k,:,:) = ee_yx(zyx(k,:,:))
      enddo

    end function zee_zyx

    function zee_tee(tee)
      !
      ! スペクトルデータから水平スペクトル・鉛直格子点データへ(正)変換する.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 次元水平スペクトル鉛直チェビシェフスペクトルデータ
      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_tee
      !(out) 2 次元水平スペクトル・鉛直格子点データ

      zee_tee = aee_e2a(az_at(e2a_aee(tee)))

    end function zee_tee
    
    function tee_zee(zee)
      !
      ! 水平スペクトル・鉛直格子点データからスペクトルデータへ(正)変換する.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm), intent(in) :: zee
      !(in) 2 次元水平スペクトル・鉛直格子点データ
      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_zee
      !(out) 2 次元水平鉛直チェビシェフスペクトルデータ

      tee_zee = aee_e2a(at_az(e2a_aee(zee)))

    end function tee_zee

    function e2a_aee(aee)
      !
      ! 水平スペクトルを 1 次元化転置する. 
      !
      real(8), dimension(:,:,:), intent(in)              :: aee
      !(in) 任意座標・水平 2 次元スペクトルデータ dimension(:,-mm:mm,-lm:lm)
      real(8), dimension((2*lm+1)*(2*mm+1),size(aee,1))  :: e2a_aee
      !(out) 1 次元化された水平スペクトル・任意座標データ

      if ( size(aee,2) /= 2*mm+1 ) &
           call MessageNotify('E','e2a_aee',&
                              '2nd dimension of input data invalid')
      if ( size(aee,3) /= 2*lm+1 ) &
           call MessageNotify('E','e2a_aee',&
                              '3rd dimension of input data invalid')

      e2a_aee = transpose(reshape(aee,(/size(aee,1),(2*lm+1)*(2*mm+1)/)))

    end function e2a_aee

    function aee_e2a(e2a)
      !
      ! 水平スペクトルを転置展開する. 
      !
      real(8), dimension(:,:),intent(in)                 :: e2a
      !(in) 1 次元化された水平スペクトル・任意座標データ 
      !     dimmension((2*mm*1)*(2*lm*1),:)
      real(8), dimension(size(e2a,2),-mm:mm,-lm:lm)      :: aee_e2a
      !(out) 任意座標・水平 2 次元スペクトルデータ

      if ( size(e2a,1) /= (2*mm+1)*(2*lm+1) ) &
           call MessageNotify('E','aee_e2a',&
                              '1st dimension of input data invalid')

      aee_e2a = reshape(transpose(e2a),(/size(e2a,2),2*mm+1,2*lm+1/))

    end function aee_e2a

    function e2_ee(ee)
      !
      ! 水平スペクトルを 1 次元化転置する. 
      !
      real(8), dimension(:,:), intent(in)              :: ee
      !(in) 任意座標・水平 2 次元スペクトルデータ dimension(-mm:mm,-lm:lm)
      real(8), dimension((2*lm+1)*(2*mm+1))            :: e2_ee
      !(out) 1 次元化された水平スペクトル・任意座標データ

      e2_ee = reshape(e2a_aee(reshape(ee,(/1,2*mm+1,2*lm+1/))), &
                      (/(2*lm+1)*(2*mm+1)/))

    end function e2_ee

    function ee_e2(e2)
      !
      ! 水平スペクトルを転置展開する. 
      !
      real(8), dimension(:),intent(in)                   :: e2
      !(in) 1 次元化された水平スペクトル・任意座標データ 
      !     dimmension((2*mm*1)*(2*lm*1),:)
      real(8), dimension(-mm:mm,-lm:lm)                  :: ee_e2
      !(out) 任意座標・水平 2 次元スペクトルデータ

      ee_e2 = reshape(aee_e2a(reshape(e2,(/(2*mm+1)*(2*lm+1),1/))), &
                      (/2*mm+1,2*lm+1/))

    end function ee_e2
    
  !--------------- 微分計算 -----------------
    function tee_Dx_tee(tee)
      !
      ! 入力スペクトルデータに水平微分 ∂/∂x を作用する.
      !
      ! スペクトルデータのX微分とは, 対応する格子点データにX微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 入力スペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dx_tee
      !(in) X微分されたスペクトルデータ

      integer :: n

      do n=0,nm
         tee_Dx_tee(n,:,:) = ee_Dx_ee(tee(n,:,:))
      enddo

    end function tee_Dx_tee

    function tee_Dy_tee(tee)
      !
      ! 入力スペクトルデータに水平微分 ∂/∂y を作用する.
      !
      ! スペクトルデータのY微分とは, 対応する格子点データにY微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 入力スペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dy_tee
      !(in) X微分されたスペクトルデータ

      integer :: n

      do n=0,nm
         tee_Dy_tee(n,:,:) = ee_Dy_ee(tee(n,:,:))
      enddo

    end function tee_Dy_tee

    function tee_Dz_tee(tee)
      !
      ! 入力スペクトルデータに鉛直微分 ∂/∂z を作用する.
      !
      ! スペクトルデータの鉛直微分とは, 対応する格子点データに鉛直微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 入力スペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Dz_tee
      !(in) 鉛直微分されたスペクトルデータ

      tee_Dz_tee = aee_e2a(at_Dz_at(e2a_aee(tee)))

    end function tee_Dz_tee

    function tee_Lapla_tee(tee)
      !
      ! 入力スペクトルデータにラプラシアン
      !
      !     ▽^2 = ∂^2/∂X^2 + ∂^2/∂Y^2 + ∂^2/∂Z^2
      !
      ! を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_Lapla_tee
      !(out) ラプラシアンを作用された 2 次元スペクトルデータ

      tee_Lapla_tee = tee_LaplaH_tee(tee) + tee_Dz_tee(tee_Dz_tee(tee))

    end function tee_Lapla_tee

    function tee_LaplaH_tee(tee)
      !
      ! 入力スペクトルデータに水平ラプラシアン
      !
      !     ▽^2_H = ∂^2/∂X^2 + ∂^2/∂Y^2 
      !
      ! を作用する.
      !
      ! スペクトルデータの水平ラプラシアンとは, 対応する格子点データに
      ! 水平ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 入力スペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_LaplaH_tee
      !(out) 水平ラプラシアンを作用された 2 次元スペクトルデータ

      integer :: n
      
      do n=0,nm
         tee_LaplaH_tee(n,:,:) = ee_Lapla_ee(tee(n,:,:))
      enddo

    end function tee_LaplaH_tee

    function tee_LaplaHInv_tee(tee)
      !
      ! 入力スペクトルデータに逆水平ラプラシアン
      !
      !     ▽^-2_H = (∂^2/∂X^2 + ∂^2/∂Y^2)^-1
      !
      ! を作用する.
      !
      ! スペクトルデータの逆水平ラプラシアンとは, 対応する格子点データに
      ! 逆水平ラプラシアンを作用させたデータのスペクトル変換のことである. 
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee
      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ

      real(8), dimension(0:nm,-mm:mm,-lm:lm)             :: tee_LaplaHInv_tee
      !(out) 逆水平ラプラシアンを作用された 2 次元スペクトルデータ

      integer :: n
      
      do n=0,nm
         tee_LaplaHInv_tee(n,:,:) = ee_LaplaInv_ee(tee(n,:,:))
      enddo

    end function tee_LaplaHInv_tee

    function tee_Div_zyx_zyx_zyx(zyx_VX,zyx_VY,zyx_VZ)
      !
      ! べクトル成分である 3 つの格子データに発散を作用させた
      ! スペクトルデータを返す.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) ベクトル場のX成分
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) ベクトル場のY成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VZ
      !(in) ベクトル場のZ成分

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_Div_zyx_zyx_zyx
      !(out) ベクトル場の発散

      tee_Div_zyx_zyx_zyx =   tee_Dx_tee(tee_zyx(zyx_VX)) &
                            + tee_Dy_tee(tee_zyx(zyx_VY)) &
                            + tee_Dz_tee(tee_zyx(zyx_VZ))

    end function tee_Div_zyx_zyx_zyx

  !--------------- 積分計算 -----------------
    !----(入力データ zyx)---
    function zy_IntX_zyx(zyx)  ! X積分
      !
      ! 3 次元格子点データのX方向積分.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:km,0:jm-1)  :: zy_IntX_zyx
      !(out) X方向積分された 2 次元ZY格子点データ

      integer :: i

      zy_IntX_zyx = 0.0d0
      do i=0,im-1
         zy_IntX_zyx(:,:) = zy_IntX_zyx(:,:) &
                       + zyx(:,:,i) * x_X_Weight(i)
      enddo
    end function zy_IntX_zyx

    function zx_IntY_zyx(zyx)
      !
      ! 3 次元格子点データのY方向域積分.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:km,0:im-1)  :: zx_IntY_zyx        
      !(out) Y積分された 2 次元ZY格子点データ.

      integer :: j

      zx_IntY_zyx = 0.0d0
      do j=0,jm-1
         zx_IntY_zyx(:,:) = zx_IntY_zyx(:,:) &
                       + zyx(:,j,:) * y_Y_Weight(j)
      enddo
    end function zx_IntY_zyx

    function yx_IntZ_zyx(zyx)  ! Z積分
      !
      ! 3 次元格子点データのZ方向積分.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:jm-1,0:im-1)  :: yx_IntZ_zyx
      !(out) Z積分された 2 次元YX(水平)格子点データ

      integer :: k

      yx_IntZ_zyx = 0.0d0
      do k=0,km
         yx_IntZ_zyx(:,:) = yx_IntZ_zyx(:,:) &
                       + zyx(k,:,:) * z_Z_Weight(k) 
      enddo
    end function yx_IntZ_zyx

    function x_IntZY_zyx(zyx)
      !
      ! 3 次元格子点データのZY積分
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8), dimension(0:im-1)     :: x_IntZY_zyx
      !(out) ZY(子午面)積分された 1 次元X格子点データ

      integer :: j, k

      x_IntZY_zyx = 0.0D0
      do j=0,jm-1
         do k=0,km
            x_IntZY_zyx = x_IntZY_zyx &
                 + zyx(k,j,:) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function x_IntZY_zyx

    function y_IntZX_zyx(zyx)
      !
      ! 3 次元格子点データのZX積分.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8), dimension(0:jm-1)       :: y_IntZX_zyx
      !(out) ZX積分された 1 次元Y格子点データ

      integer :: i, k

      y_IntZX_zyx = 0
      do i=0,im-1
         do k=0,km
            y_IntZX_zyx = y_IntZX_zyx &
                 + zyx(k,:,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function y_IntZX_zyx

    function z_IntYX_zyx(zyx)  ! YX(水平)積分
      !
      ! 3 次元格子点データのYX(水平)積分
      ! 
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8), dimension(0:km)     :: z_IntYX_zyx
      !(out) YX(水平)積分された 1 次元Z格子点データ

      integer :: i, j

      z_IntYX_zyx = 0
      do j=0,jm-1
         do i=0,im-1
            z_IntYX_zyx = z_IntYX_zyx &
                 + zyx(:,j,i) * x_X_Weight(i) * y_Y_Weight(j)
         enddo
      enddo
    end function z_IntYX_zyx

    function IntZYX_zyx(zyx) ! ZYX(全領域)積分
      !
      ! 3 次元格子点データのZYX(全領域)積分
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx 
      !(in) 3 次元格子点データ

      real(8)                     :: IntZYX_zyx 
      !(out) 全領域積分値

      integer :: i, j, k

      IntZYX_zyx = 0
      do i=0,im-1
         do j=0,jm-1
            do k=0,km
               IntZYX_zyx = IntZYX_zyx &
                    + zyx(k,j,i) * x_X_Weight(i) &
                         * y_Y_Weight(j) * z_Z_Weight(k)
            enddo
         enddo
      enddo
    end function IntZYX_zyx

    !----(入力データ zy)---
    function z_IntY_zy(zy)  ! Y積分
      !
      ! 2 次元(ZY)格子点データのY方向積分.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY(子午面)格子点データ

      real(8), dimension(0:km)  :: z_IntY_zy
      !(out) Y積分された 1 次元Z格子点データ

      integer :: j

      z_IntY_zy = 0.0d0
      do j=0,jm-1
         z_IntY_zy(:) = z_IntY_zy(:) + zy(:,j) * y_Y_Weight(j)
      enddo
    end function z_IntY_zy

    function y_IntZ_zy(zy)  ! Z積分
      !
      ! 2 次元(ZY)格子点データのZ方向域積分.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:jm-1)  :: y_IntZ_zy
      !(out) Z積分された 1 次元Y格子点データ

      integer :: k

      y_IntZ_zy = 0.0d0
      do k=0,km
         y_IntZ_zy(:) = y_IntZ_zy(:) &
                       + zy(k,:) * z_Z_Weight(k) 
      enddo
    end function y_IntZ_zy

    function IntZY_zy(zy)
      !
      ! 2 次元(ZY)格子点データのZY積分
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY(子午面)格子点データ

      real(8)                   :: IntZY_zy
      !(out) 積分値
      integer :: j, k

      IntZY_zy = 0
      do j=0,jm-1
         do k=0,km
            IntZY_zy = IntZY_zy &
                 + zy(k,j) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZY_zy

    !----(入力データ zx)---
    function z_IntX_zx(zx)
      !
      ! 2 次元(ZX)格子点データのX方向積分.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:km)  :: z_IntX_zx
      !(out) X積分された 1 次元Z格子点データ

      integer :: i

      z_IntX_zx = 0.0d0
      do i=0,im-1
         z_IntX_zx(:) = z_IntX_zx(:) + zx(:,i) * x_X_Weight(i)
      enddo

    end function z_IntX_zx

    function x_IntZ_zx(zx)
      !
      ! 2 次元(ZX)格子点データのZ方向積分.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:im-1)  :: x_IntZ_zx
      !(out) Z積分された 1 次元X格子点データ

      integer :: k

      x_IntZ_zx = 0.0d0
      do k=0,km
         x_IntZ_zx(:) = x_IntZ_zx(:) &
                       + zx(k,:) * z_Z_Weight(k) 
      enddo

    end function x_IntZ_zx

    function IntZX_zx(zx)  ! ZX積分
      !
      ! 2 次元(ZX)格子点データのZX積分
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 次元ZY格子点データ

      real(8)                                 :: IntZX_zx
      !(out) 積分値

      integer :: i, k

      IntZX_zx = 0
      do i=0,im-1
         do k=0,km
            IntZX_zx = IntZX_zx &
                 + zx(k,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZX_zx

    !----(入力データ z)---
    function IntZ_z(z)  ! Z積分
      !
      ! 1 次元(Z)格子点データのZ方向積分.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 次元Z格子点データ

      real(8)                              :: IntZ_z
      !(out) 積分値

      integer :: k

      IntZ_z = 0.0d0
      do k=0,km
         IntZ_z = IntZ_z + z(k) * z_Z_Weight(k) 
      enddo
    end function IntZ_z

  !--------------- 平均計算 -----------------
    !----(入力データ zyx)---
    function zy_AvrX_zyx(zyx)  ! X積分
      !
      ! 3 次元格子点データのX方向平均.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8), dimension(0:km,0:jm-1)  :: zy_AvrX_zyx
      !(out) X方向平均された 2 次元子午面格子点データ

      zy_AvrX_zyx = zy_IntX_zyx(zyx)/sum(x_X_Weight)

    end function zy_AvrX_zyx

    function zx_AvrY_zyx(zyx)  ! Y平均
      !
      ! 3 次元格子点データのY方向域平均.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:km,0:im-1)  :: zx_AvrY_zyx
      !(out) Y平均された 2 次元ZX格子点データ

      zx_AvrY_zyx = zx_IntY_zyx(zyx)/sum(y_Y_Weight)

    end function zx_AvrY_zyx

    function yx_AvrZ_zyx(zyx)
      !
      ! 3 次元格子点データのZ方向平均.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:jm-1,0:im-1)  :: yx_AvrZ_zyx          
      !(out) Z平均された 2 次元YX(水平)格子点データ

      yx_AvrZ_zyx = yx_IntZ_zyx(zyx)/sum(z_Z_Weight)

    end function yx_AvrZ_zyx

    function x_AvrZY_zyx(zyx)  ! ZY積分
      !
      ! 3 次元格子点データのZY平均
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8), dimension(0:im-1)     :: x_AvrZY_zyx
      !(out) ZY平均された 1 次元X格子点データ

      x_AvrZY_zyx = x_IntZY_zyx(zyx) &
                   /( sum(y_Y_Weight)*sum(z_Z_Weight) )

    end function x_AvrZY_zyx

    function y_AvrZX_zyx(zyx)  ! ZX積分
      !
      ! 3 次元格子点データのZX平均.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:jm-1)       :: y_AvrZX_zyx
      !(out) ZX平均された 1 次元Y格子点データ

      y_AvrZX_zyx = y_IntZX_zyx(zyx) &
                 /(sum(x_X_Weight)*sum(z_Z_Weight))

    end function y_AvrZX_zyx

    function z_AvrYX_zyx(zyx)  ! YX(水平)積分
      !
      ! 3 次元格子点データのYX(水平)積分
      ! 
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元格子点データ

      real(8), dimension(0:km)     :: z_AvrYX_zyx
      !(out) YX(水平)平均された 1 次元Z格子点データ

      z_AvrYX_zyx = z_IntYX_zyx(zyx) &
                 /(sum(x_X_Weight)*sum(y_Y_Weight))

    end function z_AvrYX_zyx

    function AvrZYX_zyx(zyx) ! ZYX(全領域)積分
      !
      ! 3 次元格子点データのZYX(全領域)積分
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx
      !(in) 3 次元ZYX格子点データ

      real(8)                     :: AvrZYX_zyx
      !(out) 全領域平均値

      AvrZYX_zyx = IntZYX_zyx(zyx) &
            /(sum(x_X_Weight)*sum(y_Y_Weight) * sum(z_Z_Weight))

    end function AvrZYX_zyx

    !----(入力データ zy)---
    function z_AvrY_zy(zy)
      !
      ! 2 次元(ZY)格子点データのY方向平均.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:km)  :: z_AvrY_zy
      !(out) Y平均された 1 次元Z格子点データ

      z_AvrY_zy = z_IntY_zy(zy)/sum(y_Y_Weight)

    end function z_AvrY_zy

    function y_AvrZ_zy(zy)
      !
      ! 2 次元(ZY)格子点データのZ方向平均.
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:jm-1)  :: y_AvrZ_zy
      !(out) Z平均された 1 次元Y格子点データ

      y_AvrZ_zy = y_IntZ_zy(zy)/sum(z_Z_Weight)

    end function y_AvrZ_zy

    function AvrZY_zy(zy)  ! ZY平均
      !
      ! 2 次元(ZY)格子点データのZY平均
      !
      real(8), dimension(0:km,0:jm-1), intent(in) :: zy
      !(in) 2 次元ZY(子午面)格子点データ

      real(8)                   :: AvrZY_zy
      !(out) 平均値

      AvrZY_zy = IntZY_zy(zy)/(sum(y_Y_Weight)*sum(z_Z_Weight))

    end function AvrZY_zy

    !----(入力データ zx)---
    function z_AvrX_zx(zx)  ! X積分
      !
      ! 2 次元(ZX)格子点データのX方向平均.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:km)  :: z_AvrX_zx 
      !(out) X平均された 1 次元Z格子点データ

      z_AvrX_zx = z_IntX_zx(zx)/sum(x_X_Weight)

    end function z_AvrX_zx

    function x_AvrZ_zx(zx)  ! Z平均
      !
      ! 2 次元(ZX)格子点データのZ方向平均.
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx
      !(in) 2 次元ZY格子点データ

      real(8), dimension(0:im-1)  :: x_AvrZ_zx
      !(out) Z平均された 1 次元X格子点データ

      x_AvrZ_zx = x_IntZ_zx(zx)/sum(z_Z_Weight)

    end function x_AvrZ_zx

    function AvrZX_zx(zx)  ! ZX積分
      !
      ! 2 次元(ZX)格子点データのZX平均
      !
      real(8), dimension(0:km,0:im-1), intent(in) :: zx    
      ! (in) 2 次元格子点データ
      real(8)                                 :: AvrZX_zx      
      ! 平均値

      AvrZX_zx = IntZX_zx(zx)/(sum(x_X_Weight)*sum(z_Z_Weight))

    end function AvrZX_zx

    !----(入力データ z)---
    function AvrZ_z(z)
      !
      ! 1 次元(Z)格子点データのZ方向平均.
      !
      real(8), dimension(0:km), intent(in) :: z
      !(in) 1 次元Z格子点データ
      real(8)                              :: AvrZ_z
      !(out) 平均値

      AvrZ_z = IntZ_z(z)/sum(z_Z_Weight)

    end function AvrZ_z

!!$  !--------------- ポロイダル/トロイダルモデル用微分 -----------------
!!$
!!$    function wt_KxRGrad_wt(wt)
!!$      !
!!$      ! 入力スペクトルデータにX微分 k×r・▽ = ∂/∂λを作用する.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_KxRGrad_wt
!!$      !(out) X微分を作用された 2 次元スペクトルデータ
!!$
!!$      wt_KxRGrad_wt =  wa_Dlon_wa(wt)
!!$
!!$    end function wt_KxRGrad_wt
!!$
!!$    function zyx_KGrad_wt(wt)    ! k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r
!!$      !
!!$      ! 入力スペクトルデータに対応する格子データに軸方向微分 
!!$      !
!!$      !    k・▽ = cosφ/r ∂/∂φ + sinφ∂/∂r 
!!$      !
!!$      ! を作用させた格子データが返される. 
!!$      ! ここでベクトル k は球の中心から北極向きの単位ベクトルである.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
!!$
!!$      real(8), dimension(0:km,0:jm-1,0:im-1)                     :: yzx_KGrad_wt
!!$      !(out) 軸方向微分を作用された 2 次元スペクトルデータ
!!$
!!$      xzy_KGrad_wt =  cos(xyz_Y)*xyz_GradY_wt(wt) &
!!$                    + sin(xyz_Y)*xyz_wt(wt_Drad_wt(wt))
!!$
!!$    end function xyz_KGrad_wt
!!$
!!$    function wt_L2_wt(wt)
!!$      !
!!$      ! 入力スペクトルデータに L^2 演算子(=-水平ラプラシアン)を作用する.
!!$      !
!!$      ! L^2 演算子は単位球面上の水平ラプラシアンの逆符号にあたる. 
!!$      !  入力スペクトルデ ータに対応する格子点データに演算子 
!!$      !
!!$      !     L^2 = -1/cos^2φ・∂^2/∂λ^2 - 1/cosφ・∂/∂φ(cosφ∂/∂φ)
!!$      !
!!$      ! を作用させたデータのスペクトル変換が返される.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_L2_wt
!!$      !(out) L^2 演算子を作用された 2 次元スペクトルデータ
!!$
!!$      wt_L2_wt = -wa_Lapla_wa(wt)
!!$
!!$    end function wt_L2_wt
!!$
!!$    function wt_L2Inv_wt(wt)
!!$      !
!!$      ! 入力スペクトルデータに L^2 演算子の逆演算(-逆水平ラプラシアン)を
!!$      ! 作用する.
!!$      !
!!$      ! スペクトルデータに L^2 演算子を作用させる関数 wt_L2_wt の逆計算を
!!$      ! 行う関数である.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_L2Inv_wt
!!$      !(out) L^2 演算子の逆演算を作用された 2 次元スペクトルデータ
!!$
!!$      wt_L2Inv_wt = -wa_LaplaInv_wa(wt)
!!$
!!$    end function wt_L2Inv_wt
!!$
!!$    function wt_QOperator_wt(wt)
!!$      !
!!$      ! 入力スペクトルデータに対応する格子点データに演算子 
!!$      !
!!$      !    Q=(k・▽-1/2(L2 k・▽+ k・▽L2)) 
!!$      !
!!$      ! を作用させたデータのスペクトル変換が返される.
!!$      !
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm), intent(in) :: wt
!!$      !(in) 2 次元球面調和函数チェビシェフスペクトルデータ
!!$
!!$      real(8), dimension(-lm:lm,-mm,mm,0:nm)             :: wt_QOperator_wt
!!$      !(out) Q 演算子を作用された 2 次元スペクトルデータ
!!$
!!$      wt_QOperator_wt = &
!!$             wt_xyz(xyz_KGrad_wt(wt) - xyz_KGrad_wt(wt_L2_wt(wt))/2) &
!!$           - wt_L2_wt(wt_xyz(xyz_KGrad_wt(wt)))/2
!!$
!!$    end function wt_QOperator_wt

    function tee_ZRot_zyx_zyx(zyx_VX,zyx_VY)  ! z・(▽×v)
      !
      ! ベクトルの渦度とZベクトルの内積 z・(▽×v) を計算する.
      !
      ! 第 1, 2 引数(v[x], v[y])がそれぞれベクトルのX成分, Y成分を表す.
      !
      !    z・(▽×v) = ∂v[y]/∂x - ∂(v[x])/∂y
      !
      ! のスペクトル データが返される.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) ベクトルのX成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) ベクトルのY成分

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_ZRot_zyx_zyx
      !(out) ベクトルの渦度とZベクトルの内積

      tee_ZRot_zyx_zyx = tee_DX_tee(tee_zyx(zyx_VY)) &
                       - tee_DY_tee(tee_zyx(zyx_VX))
      
    end function tee_ZRot_zyx_zyx

    function tee_ZRotRot_zyx_zyx_zyx(zyx_VX,zyx_VY,zyx_VZ) 
      ! 
      ! ベクトル v に対して z・(▽×▽×v) を計算する.
      !
      ! 第 1, 2, 3 引数(v[x], v[y], v[z])がそれぞれベクトルのX成分, 
      ! Y成分, Z成分を表す. 
      !
      !    z・(▽×▽×v)  = ∂/∂z (∂v[x]/∂x + ∂(v[y])/∂y ) ) 
      !                    - ▽_H^2 v[z]
      !
      ! のスペクトルデータが返される.
      !
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VX
      !(in) ベクトルのX成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VY
      !(in) ベクトルのY成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(in) :: zyx_VZ
      !(in) ベクトルのZ成分

      real(8), dimension(0:nm,-mm:mm,-lm:lm)     :: tee_ZRotRot_zyx_zyx_zyx
      !(out) ベクトル v の z・(▽×▽×v) 

      tee_ZRotRot_zyx_zyx_zyx = &
               tee_DZ_tee(   tee_DX_tee(tee_zyx(zyx_VX))   &
                           + tee_DY_tee(tee_zyx(zyx_VY)) ) &
             - tee_LaplaH_tee(tee_zyx(zyx_VZ))

    end function tee_ZRotRot_zyx_zyx_zyx

    subroutine tee_Potential2Vector(&
         zyx_VX,zyx_VY,zyx_VZ,tee_Torvel,tee_Polvel)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψz) + ▽x▽x(Φz) 
      !
      ! の各成分を計算する
      !
      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VX
      !(out) ベクトル場のX成分

      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VY
      !(out) ベクトル場のY成分

      real(8), dimension(0:km,0:jm-1,0:im-1)     :: zyx_VZ
      !(out) ベクトル場のZ成分

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Torvel
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Polvel
      !(in) ポロイダルポテンシャル

      zyx_VX = zyx_tee(   tee_DY_tee(tee_Torvel) &
                        + tee_DX_tee(tee_DZ_tee(tee_Polvel))  )
      zyx_VY = zyx_tee( - tee_DX_tee(tee_Torvel) &
                        + tee_DY_tee(tee_DZ_tee(tee_Polvel))  )
      zyx_VZ = -zyx_tee(tee_LaplaH_tee(tee_Polvel))

    end subroutine tee_Potential2Vector

    subroutine tee_Potential2Rotation(&
       zyx_RotVX,zyx_RotVY,zyx_RotVZ,tee_Torvel,tee_Polvel)
      !
      ! トロイダルポロイダルポテンシャルΨ,Φで表される非発散ベクトル場
      !
      !     v = ▽x(Ψz) + ▽x▽x(Φz) 
      !
      ! に対して, その回転
      !
      !     ▽xv = ▽x▽x(Ψz) + ▽x▽x▽x(Φz) = ▽x▽x(Ψz) - ▽x((▽^2Φ)z)
      !
      ! を計算する. 
      
      ! ベクトル場の回転
      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVX
      !(out) 回転のX成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVY
      !(out) 回転のY成分

      real(8), dimension(0:km,0:jm-1,0:im-1), intent(OUT) :: zyx_RotVZ
      !(out) 回転のZ成分

      ! 入力ベクトル場を表すポテンシャル
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Torvel
      !(in) トロイダルポテンシャル

      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_Polvel
      !(in) ポロイダルポテンシャル

      call tee_Potential2Vector( &
           zyx_RotVX,zyx_RotVY,zyx_RotVZ, &
           -tee_Lapla_tee(tee_Polvel), tee_Torvel)

    end subroutine tee_Potential2Rotation

  !--------------- 補間計算 -----------------
    function Interpolate_tee(tee_data,x,y,z)
      !
      ! (x,y,z) における関数値を
      ! そのスペクトル係数 tee_data から補間計算する
      !
      real(8), intent(IN) :: tee_data(0:nm,-mm:mm,-lm:lm)  ! スペクトルデータ
      real(8), intent(IN) :: x                             ! 補間する位置(X)
      real(8), intent(IN) :: y                             ! 補間する位置(Y)
      real(8), intent(IN) :: z                             ! 補間する位置(Z)
      real(8) :: Interpolate_tee                           ! 補間した値
      
      Interpolate_tee = &
           Interpolate_ee(ee_e2(a_Interpolate_at(e2a_aee(tee_data),z)),x,y)

    end function Interpolate_tee

  !--------------- ポロイダル/トロイダルモデル用スペクトル解析 ----------------

    function zee_ToroidalEnergySpectrum_tee(tee_TORPOT)
      !
      ! トロイダルポテンシャルから, トロイダルエネルギーの
      ! 水平 X 波数 l, Y  波数 m の各成分を計算する
      !
      !  * X 波数 l, Y 波数 m のトロイダルポテンシャルのスペクトル成分
      !    ψ(l,m,z)から水平 X 波数 l, Y 波数 m 成分のトロイダルエネルギー
      !    スペクトルは  (1/2)(l**2+m**2)|ψ(l,m,z)|^2  と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和をZ積分したものが領域内での
      !    水平平均エネルギーに等しい.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_TORPOT
      !(in) トロイダルポテンシャル

      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_ToroidalEnergySpectrum_tee
      !(out) エネルギースペクトルトロイダル成分

      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_Work
      integer :: l, m

      zee_Work = zee_tee(tee_Torpot)
      do l=-lm,lm
         do m=-mm,mm
            zee_ToroidalEnergySpectrum_tee(:,m,l) &
              = 0.5 * ((2*PI*l/xl)**2+(2*PI*m/yl)**2) &
              * ( zee_Work(:,m,l)**2 + zee_Work(:,-m,-l)**2 )
         enddo
      enddo

    end function zee_ToroidalEnergySpectrum_tee

!!$    function nz_ToroidalEnergySpectrum_wt(wt_TORPOT)
!!$      !
!!$      ! トロイダルポテンシャルから, トロイダルエネルギーの
!!$      ! 球面調和函数全波数の各成分を計算する.
!!$      !
!!$      !  * 全波数 n, 帯状波数 m のトロイダルポテンシャルのスペクトル成分
!!$      !    ψ(n,m,r)から全波数 n 成分のトロイダルエネルギースペクトルは
!!$      !    Σ[m=-n]^n(1/2)n(n+1)4πr^2ψ(n,m,r)^2 と計算される.
!!$      !
!!$      ! * 全てのエネルギースペクトル成分の和をZ積分したもの(r^2の重み無し)
!!$      !    が球殻内での全エネルギーに等しい.
!!$      !
!!$      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: wt_TORPOT
!!$      !(in) トロイダルポテンシャル
!!$
!!$      real(8), dimension(0:nm,0:km) :: nz_ToroidalEnergySpectrum_wt 
!!$      !(out) エネルギースペクトルトロイダル成分
!!$
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA   ! 作業領域
!!$      integer :: n, m
!!$
!!$      wz_DATA = wz_wt(wt_TORPOT)
!!$      do n=0,nm
!!$         nz_ToroidalEnergySpectrum_wt(n,:) &
!!$              = 0.5 * n*(n+1)* (4*pi) * z_Z**2 &
!!$                * sum(wz_Data(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)
!!$      enddo
!!$
!!$    end function nz_ToroidalEnergySpectrum_wt

    function zee_PoloidalEnergySpectrum_tee(tee_POLPOT)
      !
      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
      ! 水平 X 波数 l, Y 波数 m の各成分を計算する.
      !
      !  * X 波数 l, Y 波数 m のポロイダルポテンシャルのスペクトル成分
      !    φ(l,m,z)から X 波数 l, Y 波数 m 成分のポロイダルエネルギー
      !    スペクトルは 
      !
      !      (1/2)(l**2+m**2){[dφ(n,m,z)/dz]^2 + (l**2+m**2)φ(n,m,z)^2} 
      !
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和をZ積分したもの
      !    が水平平均エネルギーに等しい.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: tee_POLPOT
      !(in) ポロイダルポテンシャル

      real(8), dimension(0:km,-nm:nm,-lm:lm) :: zee_PoloidalEnergySpectrum_tee
      !(out) エネルギースペクトルポロイダル成分


      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_Data   ! 作業領域
      real(8), dimension(0:km,-mm:mm,-lm:lm) :: zee_DData  ! 作業領域
      integer :: l, m

      zee_Data = zee_tee(tee_POLPOT)
      zee_DData = zee_tee(tee_DZ_tee(tee_POLPOT))

      do l=-lm,lm
         do m=-mm,mm
            zee_PoloidalEnergySpectrum_tee(:,m,l) =                   &
                 + 0.5* ((2*pi*l/xl)**2+(2*pi*m/yl)**2)               &
                 *(   zee_DData(:,m,l)**2 + zee_DData(:,-m,-l)**2     &
                    + ((2*pi*l/xl)**2+(2*pi*m/yl)**2)                 &
                         *( zee_Data(:,m,l)**2 + zee_Data(:,-m,-l)**2)) 
         enddo
      enddo

    end function zee_PoloidalEnergySpectrum_tee

!!$    function nz_PoloidalEnergySpectrum_wt(wt_POLPOT)
!!$      !
!!$      ! ポロイダルポテンシャルから, ポロイダルエネルギーの
!!$      ! 球面調和函数全波数の各成分を計算する
!!$      !
!!$      !  * 全波数 n, 帯状波数 m のポロイダルポテンシャルのスペクトル成分
!!$      !    φ(n,m,r)から全波数 n 成分のポロイダルエネルギースペクトルは
!!$      !
!!$      !      Σ[m=-n]^n ((1/2)n(n+1)4πr^2{[d(rφ(n,m,r))/dr]^2 
!!$      !                 + n(n+1)φ(n,m,r)^2} 
!!$      !
!!$      !    と計算される.
!!$      !
!!$      !  * 全ての全波数に対してのエネルギースペクトル成分の和をZ積分したもの
!!$      !    (r^2の重み無し)が球殻内での全エネルギーに等しい.
!!$      !
!!$      real(8), dimension(0:nm,-mm:mm,-lm:lm), intent(in) :: wt_POLPOT
!!$      !(in) ポロイダルポテンシャル
!!$
!!$      real(8), dimension(0:nm,0:km) :: nz_PoloidalEnergySpectrum_wt
!!$      !(out) エネルギースペクトルポロイダル成分
!!$
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA1   ! 作業領域
!!$      real(8), dimension((nm+1)*(nm+1),0:km) ::wz_DATA2   ! 作業領域
!!$      integer :: n, m
!!$
!!$      wz_Data1 = wz_wt(wt_POLPOT)
!!$      wz_Data2 = wz_Z*wz_wt(wt_DZ_wt(wt_POLPOT)) &    ! d(rφ)/dr
!!$               + wz_wt(wt_POLPOT)                         ! = rdφ/dr+φ
!!$
!!$      do n=0,nm
!!$         nz_PoloidalEnergySpectrum_wt(n,:) = &
!!$              + 0.5* n*(n+1)* (4*pi) &
!!$              *( sum(wz_Data2(l_nm(n,(/(m,m=-n,n)/)),:)**2,1)  &
!!$                + n*(n+1)*sum(wz_Data1(l_nm(n,(/(m,m=-n,n)/)),:)**2,1) )
!!$      enddo
!!$
!!$    end function nz_PoloidalEnergySpectrum_wt
!!$
!!$
  !--------------- 境界値問題 -----------------

    subroutine tee_BoundariesTau(tee,values,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! Chebyshev 空間での境界条件適用(タウ法)
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を
      ! 定める方法をとっている(タウ法).
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)      :: tee
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(2,-mm:mm,-lm:lm), intent(in), optional :: values
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !     省略時は値/勾配 0 となる. 

      character(len=2), intent(in), optional             :: cond
              !(in) 境界条件. 省略時は 'DD'
              !        DD : 両端ディリクレ条件
              !        DN : 上端ディリクレ, 下端ノイマン条件
              !        ND : 上端ノイマン, 下端ディリクレ条件
              !        NN : 両端ノイマン条件

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)         :: e2t

      real(8), dimension((2*lm+1)*(2*mm+1),2)            :: e22_values

      e2t = e2a_aee(tee)

      if (present(values)) then
         e22_values = e2a_aee(values)
      endif

      if (.not. present(cond)) then
         if (present(values)) then
            call at_BoundariesTau_DD(e2t,e22_values)
         else
            call at_BoundariesTau_DD(e2t)
         endif
         goto 99
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesTau_NN(e2t,e22_values)
         else
            call at_BoundariesTau_NN(e2t)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesTau_DN(e2t,e22_values)
         else
            call at_BoundariesTau_DN(e2t)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesTau_ND(e2t,e22_values)
         else
            call at_BoundariesTau_ND(e2t)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesTau_DD(e2t,e22_values)
         else
            call at_BoundariesTau_DD(e2t)
         endif
      case default
         call MessageNotify('E','tee_BoundariesTau','B.C. not supported')
      end select

99    tee = aee_e2a(e2t)

    end subroutine tee_BoundariesTau

    subroutine tee_BoundariesGrid(tee,values,cond)
      !
      ! スペクトルデータにディリクレ・ノイマン境界条件を適用する
      ! 実空間での境界条件適用
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! tee_Initial にて設定するチェビシェフ切断波数(nm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)      :: tee
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      real(8), dimension(2,-mm:mm,-lm:lm), intent(in), optional :: values
              !(in) 境界での 値/勾配 分布を水平スペクトル変換したものを与える. 
              !     省略時は値/勾配 0 となる. 

      character(len=2), intent(in), optional             :: cond
              !(in) 境界条件. 省略時は 'DD'
              !        DD : 両端ディリクレ条件
              !        DN : 上端ディリクレ, 下端ノイマン条件
              !        ND : 上端ノイマン, 下端ディリクレ条件
              !        NN : 両端ノイマン条件

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)         :: e2t

      real(8), dimension((2*lm+1)*(2*mm+1),2)            :: e22_values

      e2t = e2a_aee(tee)

      if (present(values)) then
         e22_values = e2a_aee(values)
      endif

      if (.not. present(cond)) then
         if (present(values)) then
            call at_boundariesGrid_DD(e2t,e22_values)
         else
            call at_boundariesGrid_DD(e2t)
         endif
         goto 199
      endif

      select case(cond)
      case ('NN')
         if (present(values)) then
            call at_BoundariesGrid_NN(e2t,e22_values)
         else
            call at_BoundariesGrid_NN(e2t)
         endif
      case ('DN')
         if (present(values)) then
            call at_BoundariesGrid_DN(e2t,e22_values)
         else
            call at_BoundariesGrid_DN(e2t)
         endif
      case ('ND')
         if (present(values)) then
            call at_BoundariesGrid_ND(e2t,e22_values)
         else
            call at_BoundariesGrid_ND(e2t)
         endif
      case ('DD')
         if (present(values)) then
            call at_BoundariesGrid_DD(e2t,e22_values)
         else
            call at_BoundariesGrid_DD(e2t)
         endif
      case default
         call MessageNotify('E','tee_BoundariesGrid','B.C. not supported')
      end select

199   tee = aee_e2a(e2t)

    end subroutine tee_BoundariesGrid

    subroutine tee_TorBoundariesTau(tee_TOR,cond,new)
      
      ! トロイダル速度ポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). トロイダルポテンシャルの境界条件は
      !
      !     ψ = 0 at boundaries           (粘着条件) 
      !     ∂ψ/∂z = 0 at boundaries    (応力なし条件)
      ! 
      ! であるから tee_Boundaries で対応可能だが, 将来のため別途作成しておく.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: rigid1, rigid2   ! 境界条件
      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n
      save    :: alu, kp, first

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
            call MessageNotify('E','tee_TorBoundariesTau','B.C. not supported')
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
         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work= 0.0D0 ; e2t_work(:,n)= 1.0D0
            alu(:,:,n) = e2t_work

            ! 粘着条件
            ! 力学的条件粘着条件 
            if ( rigid1 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work = az_at(at_Dz_at(e2t_work))
            endif
            alu(:,nm-1,n) = e2z_work(:,0)

            ! 力学的条件粘着条件 
            if ( rigid2 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work = az_at(at_Dz_at(e2t_work))
            endif
            alu(:,nm,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)
         call MessageNotify('M','tee_TorBoundariesTau',&
                            'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_Tor)

      e2t_work(:,nm-1) = 0.0D0
      e2t_work(:,nm)   = 0.0D0

      tee_Tor = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_TorBoundariesTau

    subroutine tee_TorBoundariesGrid(tee_TOR,cond,new)
      !
      ! トロイダル速度ポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! tee_Initial にて設定するチェビシェフ切断波数(nm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! トロイダル速度ポテンシャルの境界条件は
      !
      !     ψ = 0 at boundaries           (粘着条件) 
      !     ∂ψ/∂z = 0 at boundaries    (応力なし条件)
      ! 
      ! であるので tee_Boundaries で対応可能だが, 将来のため別途作成しておく
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: rigid1, rigid2   ! 境界条件
      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

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
            call MessageNotify('E','tee_TorBoundariesGrid','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_TorBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work          ! 内部領域は値そのまま.


            ! 粘着条件
            ! 力学的条件粘着条件 
            if ( rigid1 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work=az_at(at_Dz_at(e2t_work))
            endif
            alu(:,0,n) = e2z_work(:,0)

            ! 力学的条件粘着条件 
            if ( rigid2 ) then
               e2z_work = az_at(e2t_work)
            else
               e2z_work=az_at(at_Dz_at(e2t_work))
            endif
            alu(:,km,n)   = e2z_work(:,km)

         enddo
         call ludecomp(alu,kp)
         call MessageNotify('M','TorBoundariesGrid',&
                            'Matrix to apply  b.c. newly produced.')
      endif
      
      e2z_work = az_at(e2a_aee(tee_Tor))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_TOR = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_TorBoundariesGrid

    function zee_LaplaPol2Pol_zee(zee,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      !
      ! チェビシェフ格子点空間で境界条件を適用している. 
      ! この関数を用いるためには wt_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !   ▽^2Φ = f
      !     Φ = const. at boundaries.
      !     ∂Φ/∂z = 0 at boundaries           (粘着条件) 
      !     or ∂^2Φ/∂z^2 = 0 at boundaries    (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:km,-mm:mm,-lm:lm),intent(in)  :: zee
              !(in) 入力▽^2φ分布

      real(8), dimension(0:km,-mm:mm,-lm:lm)             :: zee_LaplaPol2Pol_zee
              !(out) 出力ポロイダルポテンシャル分布

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:km)  :: e2z_work
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: k
      save    :: alu, kp, first

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
            call MessageNotify('E','zee_laplapol2pol_zee','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','zee_LaplaPol2Pol_zee', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:km),kp((2*lm+1)*(2*mm+1),0:km))

         do k=0,km
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,k) &
                 = e2a_aee(zee_tee(tee_lapla_tee(tee_zee(aee_e2a(e2z_work)))))
         enddo

         do k=0,km
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0

            ! 運動学的条件. 流線は境界で一定
            alu(:,0,k)   = e2z_work(:,0)
            alu(:,km,k)  = e2z_work(:,km)

            ! 力学的条件粘着条件 
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0
            if ( rigid1 ) then
               e2z_work=az_at(at_Dz_at(at_az(e2z_work)))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(at_az(e2z_work))))
            endif
            alu(:,1,k) = e2z_work(:,0)

            ! 力学的条件粘着条件 
            e2z_work = 0.0D0 ; e2z_work(:,k) = 1.0D0
            if ( rigid2 ) then
               e2z_work=az_at(at_Dz_at(at_az(e2z_work)))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(at_az(e2z_work))))
            endif
            alu(:,km-1,k) = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','zee_LaplaPol2Pol_zee',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work        = e2a_aee(zee)
      e2z_work(:,1)    = 0.0D0               ! 力学的条件
      e2z_work(:,km-1) = 0.0D0               ! 力学的条件
      e2z_work(:,0)    = 0.0D0               ! 運動学的条件
      e2z_work(:,km)   = 0.0D0               ! 運動学的条件 

      e2z_work = lusolve(alu,kp,e2z_work)

      zee_laplapol2pol_zee = aee_e2a(e2z_work)

    end function zee_LaplaPol2Pol_zee

    function tee_LaplaPol2PolGrid_tee(tee,cond,new)
      !
      ! 速度ポロイダルポテンシャルΦを▽^2Φから計算する.
      ! チェビシェフ格子点空間で境界条件を適用している. 
      !
      ! この関数を用いるためには tee_Initial にて設定する
      ! チェビシェフ切断波数(lm)と鉛直格子点数(km)を等しく
      ! しておく必要がある. 
      !
      ! 速度ポロイダルポテンシャルΦを f = ▽^2Φから定める式は
      !
      !    ▽^2Φ = f
      !      Φ = const. at boundaries.
      !      ∂Φ/∂z = 0 at boundaries          (粘着条件) 
      !      or ∂^2Φ/∂z^2 = 0 at boundaries   (応力なし条件)
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      ! 最終的にチェビシェフ係数の解が欲しい場合には, tee_LaplaPol2Pol_tee に
      ! 比べてチェビシェフ -- 格子点変換が 1 回分少なくて済む.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(in) :: tee
              !(in) 入力▽^2φ分布

      real(8), dimension(0:nm,-mm:mm,-lm:lm)      :: tee_LaplaPol2PolGrid_tee
              !(out) 出力ポロイダルポテンシャル分布

      character(len=2), intent(in), optional  :: cond
              !(in) 境界条件スイッチ. 省略時は 'RR'
              !     RR    : 両端粘着条件
              !     RF    : 上端粘着, 下端応力なし条件
              !     FR    : 上端応力なし, 下端粘着条件
              !     FF    : 両端応力なし条件

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm)  :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km)  :: e2z_work
      logical                                 :: rigid1, rigid2   ! 境界条件

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: n
      save    :: alu, kp, first

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
            call MessageNotify('E','tee_LaplaPol2PolGrid_tee','B.C. not supported')
         end select
      endif

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_LaplaPol2PolGrid_tee', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0

            ! 各水平波数に関して独立の式
            alu(:,:,n) = e2a_aee(zee_tee(tee_lapla_tee(aee_e2a(e2t_work))))
         enddo

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            e2z_work = az_at(e2t_work)

            ! 運動学的条件. 流線は境界で一定
            alu(:,0,n)   = e2z_work(:,0)
            alu(:,km,n)  = e2z_work(:,km)

            ! 力学的条件粘着条件 
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            if ( rigid1 ) then
               e2z_work=az_at(at_Dz_at(e2t_work))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(e2t_work)))
            endif
            alu(:,1,n) = e2z_work(:,0)

            ! 力学的条件粘着条件 
            e2t_work = 0.0D0 ; e2t_work(:,n) = 1.0D0
            if ( rigid2 ) then
               e2z_work=az_at(at_Dz_at(e2t_work))
            else
               e2z_work=az_at(at_Dz_at(at_Dz_at(e2t_work)))
            endif
            alu(:,km-1,n) = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_LaplaPol2PolGrid_tee',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work         = az_at(e2a_aee(tee))
      e2z_work(:,1)    = 0.0D0               ! 力学的条件
      e2z_work(:,km-1) = 0.0D0               ! 力学的条件
      e2z_work(:,0)    = 0.0D0               ! 運動学的条件
      e2z_work(:,km)   = 0.0D0               ! 運動学的条件 

      tee_LaplaPol2PolGrid_tee = aee_e2a(lusolve(alu,kp,e2z_work))

    end function tee_LaplaPol2PolGrid_tee

    subroutine tee_TormagBoundariesTau(tee_TOR,new)
      
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 上側
      !    tee_psi = 0   at the outer boundary
      ! 下側
      !    tee_psi = 0   at the inner boundary
      ! 
      ! であるから tee_Boundaries で対応可能だが, 将来のため別途作成しておく.
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work= 0.0D0 ; e2t_work(:,n)= 1.0D0
            alu(:,:,n) = e2t_work

            ! 非電気伝導体
            e2z_work = az_at(e2t_work)
            alu(:,nm-1,n) = e2z_work(:,0)
            alu(:,nm,n)   = e2z_work(:,km)

         enddo

         call ludecomp(alu,kp)
         call MessageNotify('M','tee_TormagBoundariesTau',&
                            'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_Tor)

      e2t_work(:,nm-1) = 0.0D0
      e2t_work(:,nm)   = 0.0D0


      tee_Tor = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_TormagBoundariesTau

    subroutine tee_TormagBoundariesGrid(tee_TOR,new)
      !
      ! 磁場トロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用.
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! tee_Initial にて設定するチェビシェフ切断波数(nm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場トロイダルポテンシャルの境界条件は
      !
      ! 上側
      !    tee_psi = 0   at the outer boundary
      ! 下側
      !    tee_psi = 0   at the inner boundary
      ! 
      ! であるので tee_Boundaries で対応可能だが, 将来のため別途作成しておく
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_TOR
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: n
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_TorMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work          ! 内部領域は値そのまま.

            ! 非電気伝導体
            alu(:,0,n)  = e2z_work(:,0)
            alu(:,km,n) = e2z_work(:,km)

         enddo
         call ludecomp(alu,kp)
         call MessageNotify('M','TormagBoundariesGrid',&
                            'Matrix to apply  b.c. newly produced.')
      endif
      
      e2z_work = az_at(e2a_aee(tee_Tor))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_TOR = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_TormagBoundariesGrid

    subroutine tee_PolmagBoundariesTau(tee_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! Chebyshev 空間での境界条件適用
      !
      ! チェビシェフ空間において境界条件を満たすべく高次の係数を定める方法を
      ! とっている(タウ法). 現在のところ境界物質が非電気伝導体の場合のみ
      ! 対応している. その場合, 磁場ポロイダルポテンシャルの各水平スペクトル
      ! 成分 h にたいして境界条件が与えられ,
      !
      !  * 上側境界 : dh/dz + K h = 0
      !  * 下側境界 : dh/dz - K h = 0
      !
      ! である. ここで K=sqrt(l^2+m^2) は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_kh

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer :: l, m, n, e2index
      save    :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) ) deallocate(kp)

         allocate(alu((2*lm+1)*(2*mm+1),0:nm,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            alu(:,:,n) = e2t_work(:,:)    ! 内部領域は同じ
         enddo

         ! 非電気伝導体
         do m=-mm,mm
            do l=-lm,lm
               e2index = (2*mm+1)*(l+lm) + (m+mm+1)
               e2t_kh(e2index,:) = sqrt(dble(l**2+m**2))
            enddo
         enddo
         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            e2z_work = az_at(at_Dz_at(e2t_work) + e2t_kh * e2t_work)
            alu(:,lm-1,n) = e2z_work(:,0)

            e2z_work = az_at(at_Dz_at(e2t_work) - e2t_kh * e2t_work)
            alu(:,lm,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_PolmagBoundariesTau',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2t_work = e2a_aee(tee_POL)
      e2t_work(:,lm-1) = 0.0D0
      e2t_work(:,lm)   = 0.0D0
      tee_POL = aee_e2a(lusolve(alu,kp,e2t_work))

    end subroutine tee_PolmagBoundariesTau

    subroutine tee_PolmagBoundariesGrid(tee_POL,new)
      !
      ! 磁場ポロイダルポテンシャルに対して境界条件を適用する.
      ! 鉛直実空間での境界条件適用. 
      !
      ! 鉛直実格子点空間において内部領域の値と境界条件を満たすように
      ! 条件を課している(選点法). このルーチンを用いるためには 
      ! tee_Initial にて設定するチェビシェフ切断波数(nm)と鉛直格子点数(km)を
      ! 等しくしておく必要がある. 
      !
      ! 現在のところ境界物質が非電気伝導体の場合のみ対応している. 
      ! その場合, 磁場ポロイダルポテンシャルの各水平スペクトル成分 h に
      ! たいして境界条件が与えられ,
      !
      !  * 上側境界 : dh/dz + K h = 0
      !  * 下側境界 : dh/dz - K h = 0
      !
      ! である. ここで K=sqrt(l^2+m^2) は h の水平全波数である. 
      !
      ! 最初に呼ばれるときはオプショナル引数 new に関係なく行列が設定される.
      !
      real(8), dimension(0:nm,-mm:mm,-lm:lm),intent(inout)   :: tee_POL
              !(inout) 境界条件を適用するデータ. 修正された値を返す. 

      logical, intent(IN), optional :: new
              !(in) true だと境界条件計算用行列を強制的に新たに作る.
              !     default は false.

      real(8), dimension(:,:,:), allocatable  :: alu
      integer, dimension(:,:), allocatable    :: kp

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_work
      real(8), dimension((2*lm+1)*(2*mm+1),0:km) :: e2z_work

      real(8), dimension((2*lm+1)*(2*mm+1),0:nm) :: e2t_kh

      logical :: first = .true.
      logical :: new_matrix = .false.
      integer  :: l, m, n, e2index
      save     :: alu, kp, first

      if (.not. present(new)) then
         new_matrix=.false.
      else 
         new_matrix=new
      endif

      if ( first .OR. new_matrix ) then
         first = .false.

         if ( nm /= km ) then
            call MessageNotify('E','tee_PolMagBoundariesGrid', &
             'Chebyshev truncation and number of grid points should be same.')
         endif

         if ( allocated(alu) ) deallocate(alu)
         if ( allocated(kp) )  deallocate(kp)
         allocate(alu((2*lm+1)*(2*mm+1),0:km,0:nm),kp((2*lm+1)*(2*mm+1),0:nm))

         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0
            e2z_work = az_at(e2t_work)

            alu(:,:,n) = e2z_work    ! 内部領域は同じ
         enddo

         ! 非電気伝導体
         do m=-mm,mm
            do l=-lm,lm
               e2index = (2*mm+1)*(l+lm) + (m+mm+1)
               e2t_kh(e2index,:) = sqrt(dble(l**2+m**2))
            enddo
         enddo
         do n=0,nm
            e2t_work = 0.0D0 ; e2t_work(:,n)=1.0D0

            e2z_work = az_at(at_Dz_at(e2t_work) + e2t_kh * e2t_work)
            alu(:,0,n) = e2z_work(:,0)

            e2z_work = az_at(at_Dz_at(e2t_work) - e2t_kh * e2t_work)
            alu(:,km,n)   = e2z_work(:,km)
         enddo

         call ludecomp(alu,kp)

         call MessageNotify('M','tee_PolmagBoundariesGrid',&
                           'Matrix to apply  b.c. newly produced.')
      endif

      e2z_work = az_at(e2a_aee(tee_POL))
      e2z_work(:,0)  = 0.0D0
      e2z_work(:,km) = 0.0D0
      tee_POL = aee_e2a(lusolve(alu,kp,e2z_work))

    end subroutine tee_PolmagBoundariesGrid

end module tee_module
