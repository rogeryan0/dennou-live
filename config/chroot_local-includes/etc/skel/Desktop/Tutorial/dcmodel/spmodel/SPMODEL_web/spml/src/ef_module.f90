!--
!----------------------------------------------------------------------
!     Copyright 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  ef_module
!
!      spml/ef_module モジュールは x 方向に周期的, y 方向に無限領域の
!      下での 2 次元領域の流体運動をスペクトル法により数値計算するための
!       Fortran90 関数を提供する. 
!
!      内部で ISPACK/U2PACK の Fortran77 サブルーチンを呼んでいる. 
!      スペクトルデータおよび格子点データの格納方法については
!      ISPACK/U2PACK のマニュアルを参照されたい. 
!
!履歴  2009/12/13  竹広真一
!
!++
module ef_module
  !
  != ef_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: ef_module.f90,v 1.1 2009-12-26 02:34:32 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/ef_module モジュールは x 方向に周期的, y 方向に無限領域の
  ! 下での 2 次元領域の流体運動をスペクトル法により数値計算するための
  !  Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK/U2PACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法については
  ! ISPACK/U2PACK のマニュアルを参照されたい. 
  !
  !== 関数・変数の名前と型について
  !
  !=== 命名法
  !  
  ! * 関数名の先頭 (ef_, yx_, x_, y_) は, 返す値の形を示している.
  !   ef_ :: スペクトルデータ(第 1,2 次元がそれぞれ Y,X 方向波数)
  !   yx_ :: 2 次元格子点データ(第 1,2 次元がそれぞれ Y,X 方向の格子点)
  !   x_  :: X 方向 1 次元格子点データ, y_ : Y 方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dx, Dy, Lapla, LaplaInv, Jacobian)は, 
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_ef_ef, _ef, _yx, _x, _y) は, 入力変数の形が
  !   スペクトルデータおよび格子点データであることを示している.
  !   _ef    :: スペクトルデータ
  !   _ef_ef :: 2 つのスペクトルデータ
  !   _yx    :: 2 次元格子点データ
  !   _x     :: X 方向 1 次元格子点データ
  !   _y     :: Y 方向 1 次元格子点データ.
  !  
  !=== 各データの種類の説明
  !  
  ! * yx : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:jm-1,0:im-1). 
  !   * im, jm はそれぞれ X, Y 座標の格子点数であり, サブルーチン 
  !     ef_initial にてあらかじめ設定しておく.
  !   * 第 1 次元が Y 座標の格子点位置番号, 第 2 次元が X 座標の
  !     格子点位置番号である (X, Y の順ではない)ことに注意.
  !
  ! * ef : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-lm:lm,-km:km). 
  !   * km, lm はそれぞれ X, Y 方向の最大波数であり, サブルーチン 
  !     ef_initial にてあらかじめ設定しておく.
  !     (X, Y 方向波数の順ではない)ことに注意. 
  !   * スペクトルデータの格納のされ方については...
  !
  ! * x, y : X, Y 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ
  !     real(8), dimension(0:im-1) および real(8), dimension(0:jm-1).
  !
  ! * ef_ で始まる関数が返す値はスペクトルデータに同じ.
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
  ! ef_Initial :: スペクトル変換の格子点数, 波数, 領域の大きさの設定
  !
  !==== 座標変数
  !
  ! x_X, y_Y     ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, y_Y_Weight ::  重み座標を格納した 1 次元配列
  ! yx_X, yx_Y   :: 格子点データの XY 座標(X,Y)(格子点データ型 2 次元配列)
  !
  !==== 基本変換
  !
  ! yx_ef :: スペクトルデータから格子データへの変換
  ! ef_yx :: 格子データからスペクトルデータへの変換
  !
  !==== 微分
  !
  ! yx_Lapla_ef       :: スペクトルデータにラプラシアンを作用させる
  ! yx_Dx_ef          :: スペクトルデータに X 微分を作用させる
  ! yx_Dy_ef          :: スペクトルデータに Y 微分を作用させる
  ! ef_IntX_yx        :: 格子点データに X 積分を作用させる
  ! ef_IntY_yx        :: 格子点データに Y 積分を作用させる
  ! yx_Jacobian_ef_ef :: 2 つのスペクトルデータからヤコビアンを計算する
  !
  !==== 積分・平均
  !
  ! IntYX_yx, AvrYX_yx   :: 2 次元格子点データの全領域積分および平均
  ! y_IntX_yx, y_AvrX_yx :: 2 次元格子点データの X 方向積分および平均
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! x_IntY_yx, x_AvrY_yx :: 2 次元格子点データの Y 方向積分および平均
  ! IntY_y, AvrY_y       :: 1 次元(Y)格子点データの Y 方向積分および平均
  !
  !==== 補間計算
  !
  ! Interpolate_ef       :: 任意の点の値をスペクトルデータから計算する
  !
  use dc_message, only : MessageNotify
  implicit none

  private
  public ef_Initial                                       ! 初期化ルーチン
  public ef_ChangeResolutionDomain                        ! ドメイン変更
  public yx_ef, ef_yx                                     ! 基本変換
  public yx_Lapla_ef, yx_Dx_ef, yx_Dy_ef                  ! 微分
  public ef_IntX_yx, ef_IntY_yx                           ! 積分
  public yx_Jacobian_ef_ef                                ! 非線形計算
  public IntYX_yx, y_IntX_yx, x_IntY_yx, IntX_x, IntY_y   ! 積分
  public AvrYX_yx, y_AvrX_yx, x_AvrY_yx, AvrX_x, AvrY_y   ! 平均
  public Interpolate_ef                                   ! 補間
  public x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y     ! 座標変数

  integer   :: im=32, jm=32                      ! 格子点の設定(X,Y)
  integer   :: km=10, lm=10                      ! 切断波数の設定(X,Y)
  real(8)   :: xl=1.0                            ! X 領域の大きさ
  real(8)   :: yr=1.0                            ! Y 方向スケーリング

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
     real(8)   :: xl, yr
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

  save im, jm, km, lm, itj, tj, iti, ti, xl, yr
  save x_X, y_Y, x_X_Weight, y_Y_Weight, yx_X, yx_Y
  save params, nparams

  contains
  !--------------- 初期化 -----------------
    subroutine ef_Initial(i,j,k,l,xmin,xmax,yrad,id)
      !
      ! スペクトル変換の格子点数, 波数, 領域の大きさを設定する.
      !
      ! 他の関数や変数を呼ぶ前に, 最初にこのサブルーチンを呼んで
      ! 初期設定をしなければならない.
      !
      ! オプショナル引数 id を用いて異なる解像度, 領域を同時に
      ! 扱うことができる. ef_Initial を解像度, 領域ごとに呼んで
      ! 引数 id をキープし, ef_ChangeResolutionDomain で切替える. 
      !
      integer,intent(in) :: i           ! 格子点数(X)
      integer,intent(in) :: j           ! 格子点数(Y)
      integer,intent(in) :: K           ! 切断波数(X)
      integer,intent(in) :: l           ! 切断波数(Y)

      real(8),intent(in) :: xmin, xmax     ! X 座標範囲
      real(8),intent(in) :: yrad           ! Y スケーリング

      integer, intent(out), optional :: id  ! 解像度領域情報番号

      character(len=3) cid
      integer :: ii

      im = i         ; jm = j
      km = k         ; lm = l
      xl = xmax-xmin ; yr = yrad

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','ef_initial',&
              'too many call of ef_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%xl = xl
      params(nparams)%yr = yr

      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tj(jm*3))
      allocate(params(nparams)%ti(im*2))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%y_Y(0:jm-1))
      allocate(params(nparams)%y_Y_Weight(0:jm-1))
      allocate(params(nparams)%yx_X(0:jm-1,0:im-1))
      allocate(params(nparams)%yx_Y(0:jm-1,0:im-1))

      call ef_ChangeResolutionDomain(nparams)

      call u2init(jm,im,itj,tj,iti,ti,y_Y,yr)

      do ii=0,im-1
         x_X(ii) = xmin + xl/im*ii
      enddo
      x_X_Weight = xl/im

      y_Y_Weight = (2*PI/jm)*(yrad/cos(atan(y_Y/(2*yr)))**2)

      yx_X = spread(x_X,1,jm)
      yx_Y = spread(y_Y,2,im)

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','ef_initial','ee_module (2009/12/11) is initialized')
      call MessageNotify('M','ef_initial',&
           'ResolutionDomain ID is '//trim(adjustl(cid)))
    end subroutine ef_Initial

  !--------------- id 変更 -----------------
    subroutine ef_ChangeResolutionDomain(id)
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
      yr = params(id)%yr
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
      
    end subroutine ef_ChangeResolutionDomain

  !--------------- 基本変換 -----------------
    function yx_ef(ef)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:jm-1,0:im-1)             :: yx_ef
      !(out) 格子点データ

      real(8), dimension(-lm:lm,-km:km), intent(in) :: ef
      !(in)  スペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call u2s2ga(lm,km,jm,im,ef,yx_ef,w,itj,tj,iti,ti)
    end function yx_ef

    function ef_yx(yx)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-lm:lm,-km:km)              :: ef_yx
      !(out)  スペクトルデータ

      real(8), dimension(0:jm-1,0:im-1), intent(in)  :: yx
      !(in) 格子点データ

      real(8), dimension(jm*im)                     :: w
      real(8), dimension(0:jm-1,0:im-1)             :: yx_tmp
      ! 作業用配列

      yx_tmp = yx
      call u2g2sa(lm,km,jm,im,yx_tmp,ef_yx,w,itj,tj,iti,ti)

    end function ef_yx

  !--------------- 微分計算 -----------------
    function yx_Lapla_ef(ef)
      !
      ! 入力スペクトルデータにラプラシアン(∂xx+∂yy)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      real(8), dimension(0:jm-1,0:im-1)              :: yx_Lapla_ef
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ef
      !(in) 入力スペクトルデータ

      ! 作業変数

      yx_Lapla_ef = yx_Dx_ef(ef_yx(yx_Dx_ef(ef))) &
                  + yx_Dy_ef(ef_yx(yx_Dy_ef(ef)))
      
    end function yx_Lapla_ef

    function yx_Dx_ef(ef)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用させて格子点データに変換する.
      !
      real(8), dimension(0:jm-1,0:im-1)              :: yx_Dx_ef
      !(out) スペクトルデータの X 微分

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ef
      !(in) 入力スペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call u2s2gx(lm,km,jm,im,ef,yx_Dx_ef,w,itj,tj,iti,ti)

      yx_Dx_ef = yx_Dx_ef * (2*PI)/xl

    end function yx_Dx_ef

    function yx_Dy_ef(ef)
      !
      ! 入力スペクトルデータに Y 微分(∂y)を作用させて格子点データへ変換する.
      !
      real(8), dimension(0:jm-1,0:im-1)              :: yx_Dy_ef
      !(out) スペクトルデータの Y 微分

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ef
      !(in) 入力スペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call u2s2gy(lm,km,jm,im,ef,yx_Dy_ef,w,itj,tj,iti,ti,y_Y,yr)

    end function yx_Dy_ef

    function yx_Jacobian_ef_ef(ef_a,ef_b)
      !
      !  2 つのスペクトルデータからヤコビアン
      !
      !     J(A,B)=(∂xA)(∂yB)-(∂yA)(∂xB)
      !
      !  を計算する.
      !
      real(8), dimension(0:jm-1,0:im-1)              :: yx_Jacobian_ef_ef
      !(out) 2 つのスペクトルデータのヤコビアン

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ef_a
      !(in) 1つ目の入力スペクトルデータ

      real(8), dimension(-lm:lm,-km:km), intent(in)  :: ef_b
      !(in) 2つ目の入力スペクトルデータ

      yx_Jacobian_ef_ef  = yx_Dx_ef(ef_a)*yx_Dy_ef(ef_b) &
                         - yx_Dy_ef(ef_a)*yx_Dx_ef(ef_b)

    end function yx_Jacobian_ef_ef

    function ef_IntX_yx(yx)
      !
      ! 入力格子点データに X 積分を作用させてスペクトルデータへ変換する.
      !
      real(8), dimension(0:jm-1,0:im-1), intent(in)  :: yx
      !(in) 入力格子点データ

      real(8), dimension(-lm:lm,-km:km)              :: ef_IntX_yx
      !(in) X 積分されたスペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call u2g2sx(lm,km,jm,im,yx,ef_IntX_yx,w,itj,tj,iti,ti)

    end function ef_IntX_yx

    function ef_IntY_yx(yx)
      !
      ! 入力格子点データに Y 積分を作用させてスペクトルデータへ変換する.
      !
      real(8), dimension(0:jm-1,0:im-1), intent(in)  :: yx
      !(in) 入力格子点データ

      real(8), dimension(-lm:lm,-km:km)              :: ef_IntY_yx
      !(in) X 積分されたスペクトルデータ

      real(8), dimension(jm*im)                     :: w
      ! 作業用配列

      call u2g2sx(lm,km,jm,im,yx,ef_IntY_yx,w,itj,tj,iti,ti,y_Y,yr)

    end function ef_IntY_yx

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

      AvrYX_yx = AvrX_x(x_AvrY_yx(yx))
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

      real(8), dimension(0:jm-1,0:im-1)   :: yx_Data
      real(8), dimension(-lm:lm,-km:km)   :: ef_Data

      ef_Data = ef_yx(yx)
      ef_Data(1:lm,:) = 0.0D0
      ef_Data(-lm:-1,:) = 0.0D0

      yx_Data = yx_ef(ef_Data)

      x_AvrY_yx = yx_Data(1,:)

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

      real(8), dimension(0:jm-1,0:im-1)   :: yx_Data
      real(8), dimension(0:im-1)          :: x_Data

      yx_Data = spread(y,2,im)
      x_Data  = x_AvrY_yx(yx_Data)
      AvrY_y = x_Data(0)

    end function AvrY_y

  !--------------- 補間計算 -----------------
    function Interpolate_ef( ef_Data, x, y )
      real(8), intent(IN)  :: ef_data(-lm:lm,-km:km)  ! スペクトルデータ
      real(8), intent(IN)  :: x                       ! 補間する点の x 座標 
      real(8), intent(IN)  :: y                       ! 補間する点の y 座標 
      real(8)              :: Interpolate_ef          ! 補間した値

      integer :: k, l
      real(8) :: xx, theta

      xx =(2*PI/xl)*(x - x_X(0))
      theta = 2.0D0*atan(y/(2*yr))

      Interpolate_ef = ef_Data(0,0)

      ! l=0
      do k=1,km
         Interpolate_ef = Interpolate_ef &
              +   ( ef_Data(0,k)*cos(k*xx) - ef_Data(0,-k)*sin(k*xx) ) &
              +   ( ef_Data(0,k)*cos(-k*xx) + ef_Data(0,-k)*sin(-k*xx) )
      end do

      ! l /= 0
      do l=1,lm
         do k=1,km
            Interpolate_ef = Interpolate_ef &
              +    (   ef_Data(l,k)*cos(k*xx)*cos(l*theta)    &
                     - ef_Data(l,-k)*sin(k*xx)*cos(l*theta)   &
                     + ef_Data(-l,k)*cos(k*xx)*sin(l*theta)   &
                     + ef_Data(-l,-k)*sin(k*xx)*sin(l*theta) )&
              +    (   ef_Data(l,k)*cos(-k*xx)*cos(l*theta)    &
                     + ef_Data(l,-k)*sin(-k*xx)*cos(l*theta)   &
                     + ef_Data(-l,k)*cos(-k*xx)*sin(l*theta)   &
                     - ef_Data(-l,-k)*sin(-k*xx)*sin(l*theta) )
         end do
      end do

    end function Interpolate_ef

end module ef_module
