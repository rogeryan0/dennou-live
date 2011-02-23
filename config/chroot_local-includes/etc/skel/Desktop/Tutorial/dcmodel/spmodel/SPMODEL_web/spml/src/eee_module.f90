!--
!----------------------------------------------------------------------
!     Copyright 2008-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_module
!
!      spml/eee_module モジュールは周期境界条件の下での 3 次元矩形領域の
!      流体運動をスペクトル法により数値計算するための Fortran90 関数を
!      提供する. 
!
!      内部で ISPACK/P3PACK の Fortran77 サブルーチンを呼んでいる. 
!      スペクトルデータおよび格子点データの格納方法については
!      ISPACK/P3PACK のマニュアルを参照されたい. 
!
!履歴  2008/05/03  竹広真一
!      2008/05/10  竹広真一 解像度領域複数対応
!      2008/05/11  竹広真一 xyz_zyx 追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に修正
!      2009/07/31  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!
!++
module eee_module
  !
  != eee_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eee_module.f90,v 1.5 2009-07-31 03:06:18 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/eee_module モジュールは周期境界条件の下での 3 次元矩形領域の
  ! 流体運動をスペクトル法により数値計算するための Fortran90 関数を
  ! 提供する. 
  !
  ! 内部で ISPACK/P3PACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法については
  ! ISPACK/P3PACK のマニュアルを参照されたい. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !  
  ! * 関数名の先頭 (eee_, zyx_, x_, y_) は, 返す値の形を示している.
  !   eee_ :: スペクトルデータ(第 1,2 3 次元がそれぞれ Z, Y,X 方向波数)
  !   eee2 :: 2 つのスペクトルデータの並んだもの
  !   zyx_ :; 3 次元格子点データ(第 1,2 3 次元がそれぞれ Z, Y,X 方向の格子点)
  !   yx_ :: XY 方向 2 次元格子点データ 
  !   zy_ :: YZ 方向 2 次元格子点データ
  !   zx_ :: XZ 方向 2 次元格子点データ
  !   x_ :: X 方向 1 次元格子点データ
  !   y_ :: Y 方向 1 次元格子点データ
  !   z_ :: Z 方向 1 次元格子点データ
  !  
  ! * 関数名の間の文字列(Dx, Dy, Dz, Lapla, LaplaInv)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_eee_eee,_eee,_zyx, _x, _y) は, 入力変数の形が
  !   スペクトルデータおよび格子点データであることを示している.
  !   _eee     :: スペクトルデータ
  !   _eee_eee :: 2 つのスペクトルデータ
  !   _eee2    :: 2 つのスペクトルデータの並んだもの
  !   _zyx     :: 3 次元格子点データ
  !   _x       :: X 方向 1 次元格子点データ
  !   _y       :: Y 方向 1 次元格子点データ.
  !  
  !=== 各データの種類の説明
  !  
  ! * zyx : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:km-1,0:jm-1,0:im-1). 
  !   * im, jm, km はそれぞれ X, Y Z 座標の格子点数であり, サブルーチン 
  !     eee_initial にてあらかじめ設定しておく.
  !   * 第 1 次元が Z 座標の格子点位置番号, 第 2 次元が Y 座標, 
  !     第 3 次元が X 座標の格子点位置番号であることに注意.
  !     (X, Y, Zの順ではない)
  !
  ! * eee : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-nm:nm,-mm:mm,-lm:lm). 
  !   * lm, mm, nm はそれぞれ X, Y, Z方向の最大波数であり, サブルーチン 
  !     eee_initial にてあらかじめ設定しておく.
  !     (X, Y, Z 方向波数の順ではない)ことに注意. 
  !   * スペクトルデータの格納のされ方については ISPACK/P3PACK の
  !     マニュアルを参照すること. 
  !
  ! * eee2 : 2 つのスペクトルデータのならび. 
  !   * 変数の種類と次元は real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2). 
  !
  ! * x, y, z : X, Y, Z 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ 
  !     real(8), dimension(0:im-1),
  !     real(8), dimension(0:jm-1), 
  !     および real(8), dimension(0:km-1).
  !
  ! * eee_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * zyx_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * x_, y_ z_ で始まる関数が返す値は 1 次元格子点データに同じ.
  !
  ! * スペクトルデータに対する微分等の作用とは, 対応する格子点データに
  !   微分などを作用させたデータをスペクトル変換したものことである.
  !
  !== 変数・手続き群の要約
  !
  !==== 初期化 
  !
  ! eee_Initial :: スペクトル変換の格子点数, 切断波数の設定
  ! eee_ChangeResolution :: 解像度設定の変更
  ! 
  !==== 座標変数
  !
  ! x_X, y_Y, z_Z    ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight,  z_Z_Weight ::  重み座標を格納した 1 次元配列
  ! zyx_X, zyx_Y, zyx_Z  :: 格子点データの XYZ 座標(X,Y,Z)
  !                         (格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! zyx_eee :: スペクトルデータから格子データへの変換
  ! eee_zyx :: 格子データからスペクトルデータへの変換
  ! xyz_zyx :: 格子点データの添え字順序変換
  !
  !==== 微分
  !
  ! eee_Lapla_eee       :: スペクトルデータにラプラシアンを作用させる
  ! eee_LaplaInv_eee    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! eee_Dx_eee          :: スペクトルデータに X 微分を作用させる
  ! eee_Dy_eee          :: スペクトルデータに Y 微分を作用させる
  ! eee_Dz_eee          :: スペクトルデータに Z 微分を作用させる
  !
  !==== 非線形計算
  !
  ! eee2_RotVelxVor_eee2   :: Euler 方程式の非線形項を計算する
  ! eee_VorFromZeta_eee2   :: 渦度 2 成分(ζ_1, ζ_2)ら渦度の 1 成分を計算する
  ! eee_VelFromZeta_eee2   :: 渦度 2 成分(ζ_1, ζ_2)ら速度の 1 成分を計算する
  ! eee2_ZetaFromVor_eee_eee_eee :: 渦度から渦度 2 成分(ζ_1, ζ_2)を計算する
  !
  !==== 積分・平均
  !
  ! IntZYX_zyx, AvrZYX_zyx   :: 3 次元格子点データの全領域積分および平均
  ! z_IntYX_zyx, z_AvrYX_zyx :: 3 次元格子点データの X,Y 方向積分および平均
  ! x_IntZY_zyx, x_AvrZY_zyx :: 3 次元格子点データの Y,Z 方向積分および平均
  ! y_IntZX_zyx, y_AvrZX_zyx :: 3 次元格子点データの Z,X 方向積分および平均
  ! zy_IntX_zyx, zy_AvrX_zyx :: 3 次元格子点データの X 方向積分および平均
  ! zx_IntY_zyx, zx_AvrY_zyx :: 3 次元格子点データの Y 方向積分および平均
  ! yx_IntZ_zyx, yx_AvrZ_zyx :: 3 次元格子点データの Z 方向積分および平均
  !
  ! IntYX_yx,  AvrYX_yx  :: 2 次元(XY)格子点データの X,Y 方向積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元(XY)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元(XY)格子点データの Y 方向積分および平均
  ! IntZX_zx, AvrZX_zx   :: 2 次元(ZX)格子点データの Z,X 方向積分および平均
  ! z_IntX_zx, z_AvrX_zx :: 2 次元(ZX)格子点データの X 方向積分および平均
  ! x_IntZ_zx, x_AvrZ_zx :: 2 次元(ZX)格子点データの Z 方向積分および平均
  ! IntZY_zy, AvrZY_zy   :: 2 次元(YZ)格子点データの Y,Z 方向積分および平均
  ! y_IntZ_zy, y_AvrZ_zy :: 2 次元(YZ)格子点データの Z 方向積分および平均
  ! z_IntY_zy, z_AvrY_zy :: 2 次元(YZ)格子点データの Y 方向積分および平均
  !
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  ! IntZ_z, AvrZ_z       :: 1 次元(Z)格子点データの Z 方向積分および平均
  !
  !==== スペクトル解析
  !
  ! EnergyHelicityFromZeta_eee2  :: 全エネルギーと全ヘリシティーを計算する. 
  ! ESpectralFromZeta            :: エネルギースペクトルを計算する. 
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public eee_Initial                                  ! 初期化ルーチン
  public eee_ChangeResolution                         ! 解像度設定の変更
  public zyx_eee, eee_zyx, xyz_zyx                    ! 基本変換
  public eee_Dx_eee, eee_Dy_eee, eee_Dz_eee           ! 微分
  public eee_Lapla_eee, eee_LaplaInv_eee              ! 微分
  public eee2_RotVelxVor_eee2                         ! 非線形計算
  public eee_VorFromZeta_eee2, eee_VelFromZeta_eee2   ! 渦度速度計算
  public eee2_ZetaFromVor_eee_eee_eee                 ! 渦度変換

  public IntZYX_zyx, AvrZYX_zyx                       ! 積分・平均
  public z_IntYX_zyx, z_AvrYX_zyx                     ! 積分・平均
  public x_IntZY_zyx, x_AvrZY_zyx                     ! 積分・平均
  public y_IntZX_zyx, y_AvrZX_zyx                     ! 積分・平均
  public zy_IntX_zyx, zy_AvrX_zyx                     ! 積分・平均
  public zx_IntY_zyx, zx_AvrY_zyx                     ! 積分・平均
  public yx_IntZ_zyx, yx_AvrZ_zyx                     ! 積分・平均

  public IntYX_yx,  AvrYX_yx                          ! 積分・平均
  public y_IntX_yx, y_AvrX_yx                         ! 積分・平均
  public x_IntY_yx, x_AvrY_yx                         ! 積分・平均
  public IntZX_zx, AvrZX_zx                           ! 積分・平均
  public z_IntX_zx, z_AvrX_zx                         ! 積分・平均
  public x_IntZ_zx, x_AvrZ_zx                         ! 積分・平均
  public IntZY_zy, AvrZY_zy                           ! 積分・平均
  public y_IntZ_zy, y_AvrZ_zy                         ! 積分・平均
  public z_IntY_zy, z_AvrY_zy                         ! 積分・平均

  public IntX_x, AvrX_x                               ! 積分・平均
  public IntY_y, AvrY_y                               ! 積分・平均
  public IntZ_z, AvrZ_z                               ! 積分・平均

  public EnergyHelicityFromZeta_eee2                  ! エネルギーヘリシティー
  public ESpectralFromZeta                            ! エネルギースペクトル

  public x_X, y_Y, z_Z                                ! 座標変数
  public x_X_Weight, y_Y_Weight, z_Z_Weight           ! 座標変数
  public zyx_X, zyx_Y, zyx_Z                          ! 座標変数

  integer   :: im=32, jm=32, km=32                    ! 格子点の設定(X,Y,Z)
  integer   :: lm=10, mm=10, nm=10                    ! 切断波数の設定(X,Y,Z)
  real(8)   :: xl=1.0, yl=1.0, zl=1.0                 ! 領域の大きさ


  integer, dimension(:),   pointer :: itk => null()
  real(8), dimension(:),   pointer :: tk => null()
  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()


  real(8), dimension(:),   pointer :: x_X => null()   ! 格子点座標(X)
  real(8), dimension(:),   pointer :: y_Y => null()   ! 格子点座標(Y)
  real(8), dimension(:),   pointer :: z_Z => null()   ! 格子点座標(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! 格子点重み(X)
                                         ! X 方向の格子点の間隔が格納してある.
  real(8), dimension(:),   pointer :: y_Y_Weight => null()
                                         ! 格子点重み(Y)
                                         ! Y 方向の格子点の間隔が格納してある.
  real(8), dimension(:),   pointer :: z_Z_Weight => null()
                                         ! 格子点重み(Y)
                                         ! Z 方向の格子点の間隔が格納してある.

  real(8), dimension(:,:,:), pointer :: zyx_X => null()
                          ! 格子点(X)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の X 座標を格納した格子データ
  real(8), dimension(:,:,:), pointer :: zyx_Y => null()
                          ! 格子点(Y)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の Y 座標を格納した格子データ
  real(8), dimension(:,:,:), pointer :: zyx_Z => null()
                          ! 格子点(Z)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の Z 座標を格納した格子データ


  integer, parameter :: nparams_max = 10  ! eee_Initial を呼べる最大回数
  type eee_param                          ! 解像度領域情報構造体
     integer   :: im, jm, km
     integer   :: lm, mm, nm
     integer, dimension(:),     pointer :: itk
     real(8), dimension(:),     pointer :: tk
     integer, dimension(:),     pointer :: itj
     real(8), dimension(:),     pointer :: tj
     integer, dimension(:),     pointer :: iti
     real(8), dimension(:),     pointer :: ti
     real(8), dimension(:),     pointer :: x_X
     real(8), dimension(:),     pointer :: y_Y
     real(8), dimension(:),     pointer :: z_Z
     real(8), dimension(:),     pointer :: x_X_Weight
     real(8), dimension(:),     pointer :: y_Y_Weight
     real(8), dimension(:),     pointer :: z_Z_Weight
     real(8), dimension(:,:,:), pointer :: zyx_X
     real(8), dimension(:,:,:), pointer :: zyx_Y 
     real(8), dimension(:,:,:), pointer :: zyx_Z
  end type eee_param
  type(eee_param) :: params(nparams_max)  ! 解像度領域情報
  integer :: nparams                      ! 解像度領域情報の個数

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, mm, nm, itk, tk, itj, tj, iti, ti
  save x_X, y_Y, z_Z, x_X_Weight, y_Y_Weight, z_Z_Weight, zyx_X, zyx_Y, zyx_Z
  save params, nparams

  contains
  !--------------- 初期化 -----------------
    subroutine eee_Initial(i,j,k,l,m,n,id)
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      ! 領域の範囲は [0,2pi]x[0,2pi]x[0,2pi] に決め打ち
      !
      ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      ! オプショナル引数 id を用いて異なる解像度, 領域を同時に
      ! 扱うことができる. eee_Initial を解像度, 領域ごとに呼んで
      ! 引数 id をキープし, eee_ChangeResolution で切替える. 
      !
      integer,intent(in) :: i           ! 格子点数(X)
      integer,intent(in) :: j           ! 格子点数(Y)
      integer,intent(in) :: k           ! 格子点数(Z)
      integer,intent(in) :: l           ! 切断波数(X)
      integer,intent(in) :: m           ! 切断波数(Y)
      integer,intent(in) :: n           ! 切断波数(X)

      integer, intent(out), optional :: id  ! 解像度領域情報番号

      character(len=3) cid
      integer :: ii, jj, kk

      im = i         ; jm = j         ; km = k
      lm = l         ; mm = m         ; nm = n

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','eee_initial',&
              'too many call of eee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%mm = mm
      params(nparams)%nm = nm

      allocate(params(nparams)%itk(5))
      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tk(km*2))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%y_Y(0:jm-1))
      allocate(params(nparams)%y_Y_Weight(0:jm-1))
      allocate(params(nparams)%z_Z(0:km-1))
      allocate(params(nparams)%z_Z_Weight(0:km-1))
      allocate(params(nparams)%zyx_X(0:km-1,0:jm-1,0:im-1))
      allocate(params(nparams)%zyx_Y(0:km-1,0:jm-1,0:im-1))
      allocate(params(nparams)%zyx_Z(0:km-1,0:jm-1,0:im-1))

      call eee_ChangeResolution(nparams)

      call p3init(km,jm,im,itk,tk,itj,tj,iti,ti)

      do ii=0,im-1
         x_X(ii) = 2*pi/im*ii
      enddo
      x_X_Weight = 2*pi/im

      do jj=0,jm-1
         y_Y(jj) = 2*pi/jm*jj
      enddo
      y_Y_Weight = 2*pi/jm

      do kk=0,km-1
         z_Z(kk) = 2*pi/km*kk
      enddo
      z_Z_Weight = 2*pi/km

      zyx_X = spread(spread(x_X,1,jm),1,km)
      zyx_Y = spread(spread(y_Y,2,im),1,km)
      zyx_Z = spread(spread(z_Z,2,jm),3,im)

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','eee_initial','eee_module (2009/07/31) is initialized')
      call MessageNotify('M','eee_initial',&
           'Resolution ID is '//trim(adjustl(cid)))
    end subroutine eee_initial

  !--------------- id 変更 -----------------
    subroutine eee_ChangeResolution(id)
      !
      ! 解像度設定の変更. eee_Initial で設定する際に
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
      mm = params(id)%mm
      nm = params(id)%nm
      itk => params(id)%itk
      tk => params(id)%tk
      itj => params(id)%itj
      tj => params(id)%tj
      iti => params(id)%iti
      ti => params(id)%ti
      x_X => params(id)%x_X
      y_Y => params(id)%y_Y
      z_Z => params(id)%z_Z
      x_X_Weight => params(id)%x_X_Weight
      y_Y_Weight => params(id)%y_Y_Weight
      z_Z_Weight => params(id)%z_Z_Weight
      zyx_X => params(id)%zyx_X
      zyx_Y => params(id)%zyx_Y
      zyx_Z => params(id)%zyx_Z

    end subroutine eee_ChangeResolution

  !--------------- 基本変換 -----------------
    function zyx_eee(eee)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1)             :: zyx_eee 
      !(out) 格子点データ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in) :: eee
      !(in)  スペクトルデータ

      real(8), dimension(km*jm*im)                         :: w
      ! 作業領域

      call p3s2ga(nm,mm,lm,km,jm,im,eee,zyx_eee,w,itk,tk,itj,tj,iti,ti)
    end function zyx_eee

    function eee_zyx(zyx)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_zyx
      !(out)  スペクトルデータ

      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(in)  :: zyx
      !(in) 格子点データ

      real(8), dimension(km*jm*im)                         :: w
      real(8), dimension(0:km-1,0:jm-1,0:im-1)             :: zyx_tmp
      ! 作業領域

      zyx_tmp = zyx
      call p3g2sa(nm,mm,lm,km,jm,im,zyx_tmp,eee_zyx,w,itk,tk,itj,tj,iti,ti)
    end function eee_zyx

    function xyz_zyx(zyx)
      !
      ! 格子データの添え字順序変更
      !
      real(8), dimension(0:im-1,0:jm-1,0:km-1)             :: xyz_zyx
                                                        !(out) 格子点データ

      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(in)  :: zyx
                                                      !(in) 格子点データ
      integer :: i,j,k

      do k=0,km-1
         do j=0,jm-1
            do i=0,im-1
               xyz_zyx(i,j,k) = zyx(k,j,i)
            enddo
         enddo
      enddo

    end function xyz_zyx
    
  !--------------- 微分計算 -----------------
    function eee_Lapla_eee(eee)
      !
      ! 入力スペクトルデータにラプラシアン(∂xx+∂yy+∂zz)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (l**2 + m**2 + n**2) をかける
      ! 計算を行っている. 
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Lapla_eee
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数

         do l=-lm,lm
            do m=-mm,mm
               do n=-nm,nm
                  eee_Lapla_eee(n,m,l) = -(l**2+m**2+n**2)*eee(n,m,l)
               enddo
            enddo
         enddo
    end function eee_Lapla_eee

    function eee_LaplaInv_eee(eee)
      !
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy+∂zz)**(-1)を作用する.
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (l**2 + m**2 + n**2) で割る
      ! 計算を行っている. l=m=n=0 成分には 0 を代入している. 
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)             :: eee_LaplaInv_eee
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in) :: eee
      !(in) スペクトルデータ

      integer l,m,n

      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               if ( l.ne.0 .or. m.ne.0 .or. n.ne.0 ) then
                  eee_LaplaInv_eee(n,m,l) = -eee(n,m,l)/(l**2+m**2+n**2)
               else
                  eee_LaplaInv_eee(n,m,l) = 0.0
               endif
            enddo
         enddo
      enddo
    end function eee_LaplaInv_eee

    function eee_Dx_eee(eee)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 l をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dx_eee
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dx_eee(n,m,l) = -l*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dx_eee

    function eee_Dy_eee(eee)
      !
      ! 入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの Y 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに Y 方向波数 m をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dy_eee
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dy_eee(n,m,l) = -m*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dy_eee

    function eee_Dz_eee(eee)
      !
      ! 入力スペクトルデータに Z 微分(∂z)を作用する.
      !
      ! スペクトルデータの Z 微分とは, 対応する格子点データに Z 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに Z 方向波数 n をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)              :: eee_Dz_eee
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数


      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               eee_Dz_eee(n,m,l) = -n*eee(-n,-m,-l)
            enddo
         enddo
      enddo
    end function eee_Dz_eee

  !--------------- 非線形項計算 -----------------

    function eee2_RotVelxVor_eee2(eee2)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから Euler 方程式の非線形項
      !
      !     ▽x(u x ω) 
      !
      !  の 2 成分を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)     :: eee2_RotVelxVor_eee2
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      real(8), dimension((2*lm+1)*(2*mm+1)*(2*nm+1))      :: ws
      real(8), dimension(km*jm*im*4)                      :: wg

      call p3elnl(nm,mm,lm,km,jm,im,eee2,eee2_RotVelxVor_eee2, &
                  ws,wg,itk,tk,itj,tj,iti,ti)

    end function eee2_RotVelxVor_eee2

    function eee_VorFromZeta_eee2(eee2,isw)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから渦度のスペクトルの 1 成分
      !  を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)     :: eee_VorFromZeta_eee2
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      integer, intent(IN) :: isw
      !(in) 出力する渦度の成分のインデックス(1,2,3)

      call p3geto(nm,mm,lm,eee2,eee_VorFromZeta_eee2,isw)

    end function eee_VorFromZeta_eee2

    function eee_VelFromZeta_eee2(eee2,isw)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから速度スペクトルの 1 成分
      !  を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm)     :: eee_VelFromZeta_eee2
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in)  :: eee2
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      integer, intent(IN) :: isw
      !(in) 出力する渦度の成分のインデックス(1,2,3)

      call p3getu(nm,mm,lm,eee2,eee_VelFromZeta_eee2,isw)

    end function eee_VelFromZeta_eee2

    function eee2_ZetaFromVor_eee_eee_eee(eee_1,eee_2,eee_3)
      !
      !  渦度のスペクトルから渦度 2 成分(ζ_1, ζ_2)を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)     :: eee2_ZetaFromVor_eee_eee_eee
      !(out) 渦度の 2 成分(ζ_1, ζ_2)

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm), intent(in)  :: eee_1, eee_2, eee_3
      !(in) 渦度スペクトルデータの各成分

      integer :: l,m,n

      do l=-lm,lm
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_2(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_3(n,m,l)
               elseif( m /= 0 ) then
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_3(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_1(n,m,l)
               else
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,1) = eee_1(n,m,l)
                  eee2_ZetaFromVor_eee_eee_eee(n,m,l,2) = eee_2(n,m,l)
               endif
            enddo
         enddo
      enddo

    end function eee2_ZetaFromVor_eee_eee_eee

  !--------------- 積分計算 -----------------
    function IntZYX_zyx(zyx)
      !
      ! 2 次元格子点データの全領域積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in)  3 次元格子点データ

      real(8)                                    :: IntZYX_zyx
      !(out) 積分値

      integer :: i, j, k
      ! 作業変数

      IntZYX_zyx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            do k=0,km-1
               IntZYX_zyx = IntZYX_zyx &
                    + zyx(k,j,i) * z_Z_Weight(k) * y_Y_Weight(j) * x_X_Weight(i)
            enddo
         enddo
      end do
    end function IntZYX_zyx

    function z_IntYX_zyx(zyx)
      !
      ! 3 次元格子点データの X,Y 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1)          :: z_IntYX_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i, j
      ! 作業変数

      z_IntYX_zyx = 0.0d0
      do i=0,im-1
         do j=0,jm-1
            z_IntYX_zyx(:) = &
                 z_IntYX_zyx(:) + zyx(:,j,i) * x_X_Weight(i)* y_Y_Weight(j)
         enddo
      enddo
    end function z_IntYX_zyx

    function y_IntZX_zyx(zyx)
      !
      ! 3 次元格子点データの X,Z 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1)          :: y_IntZX_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i, k
      ! 作業変数

      y_IntZX_zyx = 0.0d0
      do i=0,im-1
         do k=0,km-1
            y_IntZX_zyx(:) = &
                 y_IntZX_zyx(:) + zyx(k,:,i) * x_X_Weight(i)* z_Z_Weight(k)
         enddo
      enddo
    end function y_IntZX_zyx

    function x_IntZY_zyx(zyx)
      !
      ! 3 次元格子点データの Y,Z 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_IntZY_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: j, k
      ! 作業変数

      x_IntZY_zyx = 0.0d0
      do j=0,jm-1
         do k=0,km-1
            x_IntZY_zyx(:) = &
                 x_IntZY_zyx(:) + zyx(k,j,:) * y_Y_Weight(j)* z_Z_Weight(k)
         enddo
      enddo
    end function x_IntZY_zyx

    function zy_IntX_zyx(zyx)
      !
      ! 3 次元格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:jm-1)          :: zy_IntX_zyx
      !(out) 積分された 2 次元(ZY)格子点データ

      integer :: i
      ! 作業変数

      zy_IntX_zyx = 0.0d0
      do i=0,im-1
         zy_IntX_zyx(:,:) = zy_IntX_zyx(:,:) + zyx(:,:,i) * x_X_Weight(i)
      enddo

    end function zy_IntX_zyx

    function zx_IntY_zyx(zyx)
      !
      ! 3 次元格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntY_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: j
      ! 作業変数

      zx_IntY_zyx = 0.0d0
      do j=0,jm-1
         zx_IntY_zyx(:,:) = zx_IntY_zyx(:,:) + zyx(:,j,:) * y_Y_Weight(j)
      enddo
    end function zx_IntY_zyx

    function yx_IntZ_zyx(zyx)
      !
      ! 3 次元格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1,0:im-1)          :: yx_IntZ_zyx
      !(out) 積分された 1 次元(YX)格子点データ

      integer :: k 
      ! 作業変数

      yx_IntZ_zyx = 0.0d0
      do k=0,km-1
         yx_IntZ_zyx(:,:) = yx_IntZ_zyx(:,:) + zyx(k,:,:) * z_Z_Weight(k)
      enddo

    end function yx_IntZ_zyx

    function IntYX_yx(yx)
      !
      ! 2 次元(YX)格子点データの全領域積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx          
      !(in)  2 次元(YX)格子点データ

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
      ! 2 次元(YX)格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in) 2 次元(YX)格子点データ

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
      ! 2 次元(YX)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx      
      !(in)  2 次元(YX)格子点データ

      real(8), dimension(0:im-1)        :: x_IntY_yx 
      !(out) 積分された 1 次元(X)格子点データ

      integer :: j
      ! 作業変数

      x_IntY_yx = 0.0d0
      do j=0,jm-1
         x_IntY_yx(:) = x_IntY_yx(:) + yx(j,:) * y_Y_Weight(j)
      enddo
    end function x_IntY_yx

    function IntZY_zy(zy)
      !
      ! 2 次元(ZY)格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に z_Z_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 次元(ZY)格子点データ

      real(8)                             :: IntZY_zy
      !(out) 積分値

      integer :: j, k
      ! 作業変数

      IntZY_zy = 0.0d0
      do j=0,jm-1
         do k=0,km-1
            IntZY_zy = IntZY_zy + zy(k,j) * y_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZY_zy

    function y_IntZ_zy(zy)
      !
      ! 2 次元(ZY)格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 次元(ZY)格子点データ

      real(8), dimension(0:jm-1)          :: y_IntZ_zy
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: k
      ! 作業変数

      y_IntZ_zy = 0.0d0
      do k=0,km-1
         y_IntZ_zy(:) = y_IntZ_zy(:) + zy(k,:) * z_Z_Weight(k)
      enddo
    end function y_IntZ_zy

    function z_IntY_zy(zy)
      !
      ! 2 次元(ZY)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy          
      !(in)  2 次元格子点データ

      real(8), dimension(0:km-1)        :: z_IntY_zy 
      !(out) 積分された 1 次元(X)格子点データ

      integer :: j
      ! 作業変数

      z_IntY_zy = 0.0d0
      do j=0,jm-1
         z_IntY_zy(:) = z_IntY_zy(:) + zy(:,j) * y_Y_Weight(j)
      enddo
    end function z_IntY_zy

    function IntZX_zx(zx)
      !
      ! 2 次元(ZX)格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に z_Z_Weight, x_X_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 次元(ZX)格子点データ

      real(8)                             :: IntZX_zx
      !(out) 積分値

      integer :: i, k
      ! 作業変数

      IntZX_zx = 0.0d0
      do i=0,im-1
         do k=0,km-1
            IntZX_zx = IntZX_zx + zx(k,i) * x_X_Weight(i) * z_Z_Weight(k)
         enddo
      enddo
    end function IntZX_zx

    function x_IntZ_zx(zx)
      !
      ! 2 次元(ZX)格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 次元(ZX)格子点データ

      real(8), dimension(0:im-1)          :: x_IntZ_zx
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: k
      ! 作業変数

      x_IntZ_zx = 0.0d0
      do k=0,km-1
         x_IntZ_zx(:) = x_IntZ_zx(:) + zx(k,:) * z_Z_Weight(k)
      enddo
    end function x_IntZ_zx

    function z_IntX_zx(zx)
      !
      ! 2 次元(ZX)格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx          
      !(in)  2 次元格子点データ

      real(8), dimension(0:km-1)        :: z_IntX_zx 
      !(out) 積分された 1 次元(X)格子点データ

      integer :: i
      ! 作業変数

      z_IntX_zx = 0.0d0
      do i=0,im-1
         z_IntX_zx(:) = z_IntX_zx(:) + zx(:,i) * x_X_Weight(i)
      enddo
    end function z_IntX_zx

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

    function IntY_y(y)
      !
      ! 1 次元(Y)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:jm-1)   :: y          !(in)  1 次元格子点データ
      real(8)                      :: IntY_y     !(out) 積分値

      IntY_y = sum(y*y_Y_Weight)
    end function IntY_y

    function IntZ_z(z)
      !
      ! 1 次元(Z)格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1)   :: z         !(in)  1 次元格子点データ
      real(8)                      :: IntZ_z     !(out) 積分値

      IntZ_z = sum(z*z_Z_Weight)
    end function IntZ_z

  !--------------- 平均計算 -----------------
    function AvrZYX_zyx(zyx)
      !
      ! 2 次元格子点データの全領域平均.
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight, z_Z_Weight を
      ! かけた総和を計算し, x_X_Weight*y_Y_Weight*z_Z_Weight の総和で割る
      ! ことで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in)  3 次元格子点データ

      real(8)                                    :: AvrZYX_zyx
      !(out) 積分値

      AvrZYX_zyx = IntZYX_zyx(zyx)/(sum(x_X_weight)*sum(y_Y_weight)*sum(z_Z_weight))

    end function AvrZYX_zyx

    function z_AvrYX_zyx(zyx)
      !
      ! 3 次元格子点データの X,Y 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1)          :: z_AvrYX_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      z_AvrYX_zyx = z_IntYX_zyx(zyx)/(sum(x_X_weight)*sum(y_Y_weight))

    end function z_AvrYX_zyx

    function y_AvrZX_zyx(zyx)
      !
      ! 3 次元格子点データの X,Z 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, z_Z_Weight をかけた
      ! 総和を計算し, x_X_Weight*z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1)          :: y_AvrZX_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      y_AvrZX_zyx = y_IntZX_zyx(zyx)/(sum(x_X_weight)*sum(z_Z_weight))

    end function y_AvrZX_zyx

    function x_AvrZY_zyx(zyx)
      !
      ! 3 次元格子点データの Y,Z 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算し, y_Y_Weight*z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_AvrZY_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      x_AvrZY_zyx = x_IntZY_zyx(zyx)/(sum(y_Y_weight)*sum(z_Z_weight))

    end function x_AvrZY_zyx

    function zy_AvrX_zyx(zyx)
      !
      ! 3 次元格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた
      ! 総和を計算し, x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:jm-1)          :: zy_AvrX_zyx
      !(out) 積分された 2 次元(ZY)格子点データ

      zy_AvrX_zyx = zy_IntX_zyx(zyx)/sum(x_X_weight)

    end function zy_AvrX_zyx

    function zx_AvrY_zyx(zyx)
      !
      ! 3 次元格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた
      ! 総和を計算し, y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:im-1)          :: zx_AvrY_zyx
      !(out) 積分された 1 次元(Y)格子点データ

      zx_AvrY_zyx = zx_IntY_zyx(zyx)/sum(y_Y_weight)

    end function zx_AvrY_zyx

    function yx_AvrZ_zyx(zyx)
      !
      ! 3 次元格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた
      ! 総和を計算し, z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1,0:im-1), intent(IN)   :: zyx
      !(in) 2 次元格子点データ

      real(8), dimension(0:jm-1,0:im-1)          :: yx_AvrZ_zyx
      !(out) 積分された 1 次元(YX)格子点データ

      yx_AvrZ_zyx = yx_IntZ_zyx(zyx)/sum(z_Z_weight)

    end function yx_AvrZ_zyx

    function AvrYX_yx(yx)
      !
      ! 2 次元格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
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
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
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
      real(8), dimension(0:jm-1,0:im-1), intent(IN)   :: yx
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_AvrY_yx
      !(out) 平均された 1 次元(X)格子点データ

      x_AvrY_yx = x_IntY_yx(yx)/sum(y_Y_weight)
    end function x_AvrY_yx

    function AvrZX_zx(zx)
      !
      ! 2 次元(ZX)格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, z_Z_Weight をかけた
      ! 総和を計算し, x_X_Weight*z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in)  2 次元(ZX)格子点データ

      real(8)                             :: AvrZX_zx    
      !(out) 平均値

      AvrZX_zx = IntZX_zx(zx)/(sum(x_X_weight)*sum(z_Z_weight))
    end function AvrZX_zx

    function z_AvrX_zx(zx)
      !
      ! 2 次元(ZX)格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in) 2 次元(ZX)格子点データ

      real(8), dimension(0:km-1)          :: z_AvrX_zx
      !(out) 平均された 1 次元(Z)格子点データ

      z_AvrX_zx = z_IntX_zx(zx)/sum(x_X_weight)
    end function z_AvrX_zx

    function x_AvrZ_zx(zx)
      !
      ! 2 次元(ZX)格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算し, 
      ! z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1), intent(IN)   :: zx
      !(in) 2 次元(ZX)格子点データ

      real(8), dimension(0:im-1)          :: x_AvrZ_zx
      !(out) 平均された 1 次元(X)格子点データ

      x_AvrZ_zx = x_IntZ_zx(zx)/sum(z_Z_weight)
    end function x_AvrZ_zx

    function AvrZY_zy(zy)
      !
      ! 2 次元(ZY)格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight, y_Y_Weight をかけた
      ! 総和を計算し, z_Z_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in)  2 次元(ZY)格子点データ

      real(8)                             :: AvrZY_zy    
      !(out) 平均値

      AvrZY_zy = IntZY_zy(zy)/(sum(y_Y_weight)*sum(z_Z_weight))
    end function AvrZY_zy

    function z_AvrY_zy(zy)
      !
      ! 2 次元(ZY)格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in) 2 次元(ZY)格子点データ

      real(8), dimension(0:km-1)          :: z_AvrY_zy
      !(out) 平均された 1 次元(Z)格子点データ

      z_AvrY_zy = z_IntY_zy(zy)/sum(y_Y_weight)
    end function z_AvrY_zy

    function y_AvrZ_zy(zy)
      !
      ! 2 次元(ZY)格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算し, 
      ! z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:jm-1), intent(IN)   :: zy
      !(in) 2 次元(ZY)格子点データ

      real(8), dimension(0:jm-1)          :: y_AvrZ_zy
      !(out) 平均された 1 次元(X)格子点データ

      y_AvrZ_zy = y_IntZ_zy(zy)/sum(z_Z_weight)
    end function y_AvrZ_zy

    function AvrX_x(x)
      !
      ! 1 次元(X)格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1), intent(IN) :: x       !(in)  1 次元格子点データ
      real(8)                                :: AvrX_x  !(out) 平均値

      AvrX_x = IntX_x(x)/sum(x_X_weight)
    end function AvrX_x

    function AvrY_y(y)
      !
      ! 1 次元(Y)格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:jm-1), intent(IN) :: y      !(in)  1 次元格子点データ
      real(8)                                :: AvrY_y !(out) 平均値

      AvrY_y = IntY_y(y)/sum(y_Y_weight)
    end function AvrY_y

    function AvrZ_z(z)
      !
      ! 1 次元(Z)格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算し, 
      ! z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1), intent(IN) :: z      !(in)  1 次元格子点データ
      real(8)                                :: AvrZ_z !(out) 平均値

      AvrZ_z = IntZ_z(z)/sum(z_Z_weight)
    end function AvrZ_z

  !--------------- スペクトル計算 -----------------
    function EnergyHelicityFromZeta_eee2(eee2)
      !
      ! 渦度成分(ζ_1, ζ_2)から全領域平均エネルギーとヘリシティーを計算する. 
      !
      real(8), dimension(2)                         :: EnergyHelicityFromZeta_eee2
      ! エネルギーヘリシティー

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in) :: eee2
      ! 渦度成分(ζ_1, ζ_2)

      real(8) :: E, H           ! エネルギー, ヘリシティー
      
      call p3cnsv(nm,mm,lm,eee2,E,H)

      EnergyHelicityFromZeta_eee2(1) = E ; EnergyHelicityFromZeta_eee2(2) = H

    end function EnergyHelicityFromZeta_eee2

    subroutine ESpectralFromZeta(esp,eee2)
      !
      ! 渦度成分(ζ_1, ζ_2)からエネルギースペクトルを計算する. 
      !
      !   * esp のサイズで求められる波数範囲が定められる
      !   * esp の総和が EFFromZeta で求められるエネルギー
      !     (全領域平均値)に等しい
      !
      real(8), dimension(:), intent(OUT)  :: esp
      ! エネルギースペクトル

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(in) :: eee2
      ! 渦度成分(ζ_1, ζ_2)

      integer kmax
      ! 作業変数

      kmax=size(esp)
      call p3espt(nm,mm,lm,kmax,eee2,esp)

    end subroutine ESpectralFromZeta

end module eee_module
