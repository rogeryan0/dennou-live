!--
!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  eee_mpi_module
!
!      spml/eee_module モジュールは周期境界条件の下での 3 次元矩形領域の
!      流体運動をスペクトル法により数値計算するための Fortran90 関数を
!      提供する. 
!
!      内部で ISPACK/P3PACK, P3PACK-MPI の Fortran77 サブルーチンを呼んでいる.
!      スペクトルデータおよび格子点データの格納方法については
!      ISPACK/P3PACK のマニュアルを参照されたい. 
!
!履歴  2008/05/21  竹広真一  eee_module より改造
!      2008/06/03  竹広真一  ee2f_ZetaFromVor_eef_eef_eef bug fix
!      2008/06/03  竹広真一  ESpectralFromZeta bug fix
!      2009/02/23  佐々木洋平  RDoc 用のドキュメントを修正
!      2010/01/07  佐々木洋平  include 'mpif.h' -> use mpi
!
!++
module eee_mpi_module
  !
  != eee_mpi_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: eee_mpi_module.f90,v 1.5 2010-02-18 15:28:23 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../../COPYRIGHT]
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
  ! * 関数名の先頭 (eef_, zxv_, x_, v_, z_) は, 返す値の形を示している.
  !   eef_ :: スペクトルデータ
  !           (第 1,2 3 次元がそれぞれ Z,Y,X 方向波数(X について分割配置))
  !   ee2f :: 2 つのスペクトルデータの並んだもの
  !   zxv_ :: 3 次元格子点データ
  !           (第 1,2 3 次元がそれぞれ Z,X,Y 方向の格子点(Y について分割配置)
  !   xv_  :: XY 方向 2 次元格子点データ, zv_ : YZ 方向 2 次元格子点データ
  !   zx_  :: XZ 方向 2 次元格子点データ
  !   x_   :: X 方向 1 次元格子点データ, v_ : Y 方向 1 次元格子点データ
  !   z_   :: Z 方向 1 次元格子点データ
  !
  ! * 関数名の間の文字列(Dx, Dy, Dz, Lapla, LaplaInv)は,
  !   その関数の作用を表している.
  !
  ! * 関数名の最後 (_eee_eee,_eee,_zyx, _x, _v) は, 入力変数の形が
  !   スペクトルデータおよび格子点データであることを示している.
  !    _eef     :: スペクトルデータ
  !    _eef_eef :: 2 つのスペクトルデータ
  !    _ee2f    :: 2 つのスペクトルデータの並んだもの
  !    _zxv     :: 3 次元格子点データ
  !    _x       :: X 方向 1 次元格子点データ
  !    _v       :: Y 方向 1 次元格子点データ.
  !
  !=== 各データの種類の説明
  !
  ! * zxv : 2 次元格子点データ.
  !   * 変数の種類と次元は real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)). 
  !   * im, km はそれぞれ X, Z 座標の格子点数であり, サブルーチン 
  !     eee_mpi_initial にてあらかじめ設定しておく.
  !   * eee_mpi_Initial によってプロセス毎に Y 方向の
  !     分割配置が設定され, その情報が public 変数 js,je,jc に設定される. 
  !   * js が分割配置の最小格子点, je が最大格子点, jc が格子点数である. 
  !   * 第 1 次元が Z 座標の格子点位置番号, 
  !     第 2 次元が X 座標の格子点位置番号, 
  !     第 3 次元が Y 座標であることに注意(X, Y, Zの順ではない).
  !
  ! * eee : スペクトルデータ.
  !   * 変数の種類と次元は real(8), dimension(-nm:nm,-mm:mm,2*lc). 
  !   * mm, nm はそれぞれ Y, Z方向の最大波数であり, サブルーチン 
  !     eee_initial にてあらかじめ設定しておく
  !     (X, Y, Z 方向波数の順ではない)ことに注意. 
  !   * eee_mpi_Initial によってプロセス毎に X 方向波数の
  !     分割配置が設定され, その情報が public 変数 ls,le,lc に設定される. 
  !   * ls が分割配置の最小波数, le が最大波数, jc が波数の数である. 
  !     実際の格納のされ方は ls, ls+1,...,le,-le,-le+1,...,-ls の順である.
  !     ISPACK/P3PACK-MPI マニュアル参照のこと.
  !
  ! * ee2f : 2 つのスペクトルデータのならび. 
  !   * 変数の種類と次元は real(8), dimension(-nm:nm,-mm:mm,2,2*lc). 
  !
  ! * x, v, z : X, Y, Z 方向 1 次元格子点データ.
  !   * 変数の種類と次元はそれぞれ real(8), dimension(0:im-1),
  !     real(8), dimension(js(ip):je(ip)) および real(8), dimension(0:km-1).
  !
  ! * eee_ で始まる関数が返す値はスペクトルデータに同じ.
  !
  ! * zxv_ で始まる関数が返す値は 3 次元格子点データに同じ.
  !
  ! * x_, v_ z_ で始まる関数が返す値は 1 次元格子点データに同じ.
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
  ! lf_l        :: X 方向波数の位置情報
  !
  !==== 座標変数
  !
  ! x_X, v_Y, z_Z    ::  格子点座標(X,Y座標)を格納した 1 次元配列
  ! x_X_Weight, v_Y_Weight,  z_Z_Weight ::  重み座標を格納した 1 次元配列
  ! zxv_X, zxv_Y, zxv_Z  :: 格子点データの XYZ 座標(X,Y,Z)
  !                         (格子点データ型 3 次元配列)
  !
  !==== 基本変換
  !
  ! zxv_eef   :: スペクトルデータから格子データへの変換
  ! eef_zxv   :: 格子データからスペクトルデータへの変換
  ! xyz_zxv   :: 分散格子点データの集積
  ! eee2_ee2f :: 分散スペクトルデータの集積
  ! ee2f_eee2 :: 集積スペクトルデータの分散
  !
  !==== 微分
  !
  ! eef_Lapla_eef       :: スペクトルデータにラプラシアンを作用させる
  ! eef_LaplaInv_eef    :: スペクトルデータにラプラシアンの逆変換を作用させる
  ! eef_Dx_eef          :: スペクトルデータに X 微分を作用させる
  ! eef_Dy_eef          :: スペクトルデータに Y 微分を作用させる
  ! eef_Dz_eef          :: スペクトルデータに Z 微分を作用させる
  !
  !==== 非線形計算
  !
  ! ee2f_RotVelxVor_ee2f   :: Euler 方程式の非線形項を計算する
  ! eef_VorFromZeta_ee2f   :: 渦度 2 成分(ζ_1, ζ_2)ら渦度の 1 成分を計算する
  ! eef_VelFromZeta_ee2f   :: 渦度 2 成分(ζ_1, ζ_2)ら速度の 1 成分を計算する
  ! ee2f_ZetaFromVor_eef_eef_eef :: 渦度から渦度 2 成分(ζ_1, ζ_2)を計算する
  !
  !==== 積分・平均
  !
  ! IntZXV_zxv, AvrZXV_zxv   :: 3 次元格子点データの全領域積分および平均
  ! z_IntXV_zxv, z_AvrXV_zxv :: 3 次元格子点データの X,Y 方向積分および平均
  ! x_IntZV_zxv, x_AvrZV_zxv :: 3 次元格子点データの Y,Z 方向積分および平均
  ! y_IntZX_zxv, y_AvrZX_zxv :: 3 次元格子点データの Z,X 方向積分および平均
  ! zv_IntX_zxv, zv_AvrX_zxv :: 3 次元格子点データの X 方向積分および平均
  ! zx_IntY_zxv, zx_AvrY_zxv :: 3 次元格子点データの Y 方向積分および平均
  ! vx_IntZ_zxv, vx_AvrZ_zxv :: 3 次元格子点データの Z 方向積分および平均
  !
  ! IntXV_xv,  AvrXV_xv  :: 2 次元(XY)格子点データの X,Y 方向積分および平均
  ! v_IntX_xv, v_AvrX_xv :: 2 次元(XY)格子点データの X 方向積分および平均
  ! x_IntV_xv, x_AvrV_xv :: 2 次元(XY)格子点データの Y 方向積分および平均
  ! IntZX_zx, AvrZX_zx   :: 2 次元(ZX)格子点データの Z,X 方向積分および平均
  ! z_IntX_zx, z_AvrX_zx :: 2 次元(ZX)格子点データの X 方向積分および平均
  ! x_IntZ_zx, x_AvrZ_zx :: 2 次元(ZX)格子点データの Z 方向積分および平均
  ! IntZV_zv, AvrZV_zv   :: 2 次元(YZ)格子点データの Y,Z 方向積分および平均
  ! v_IntZ_zv, v_AvrZ_zv :: 2 次元(YZ)格子点データの Z 方向積分および平均
  ! z_IntV_zv, z_AvrV_zv :: 2 次元(YZ)格子点データの Y 方向積分および平均
  !
  ! IntX_x, AvrX_x       :: 1 次元(X)格子点データの X 方向積分および平均
  ! IntV_v, AvrV_v       :: 1 次元(Y)格子点データの Y 方向積分および平均
  ! IntZ_z, AvrZ_z       :: 1 次元(Z)格子点データの Z 方向積分および平均
  !
  !==== スペクトル解析
  !
  ! EnergyHelicityFromZeta_ee2f  :: 全エネルギーと全ヘリシティーを計算する. 
  ! ESpectralFromZeta            :: エネルギースペクトルを計算する. 
  !
  use dc_message, only : MessageNotify
  use mpi
  implicit none

  private
  public ls, le, lc
  public js, je, jc
  public lf_l
  public eee_mpi_Initial                                  ! 初期化ルーチン
  public eee_ChangeResolution                             ! 解像度設定の変更
  public zxv_eef, eef_zxv                                 ! 基本変換
  public xyz_zxv                                          ! 基本変換
  public eee2_ee2f, ee2f_eee2                             ! 基本変換
  public eef_Dx_eef, eef_Dy_eef, eef_Dz_eef               ! 微分
  public eef_Lapla_eef, eef_LaplaInv_eef                  ! 微分
  public ee2f_RotVelxVor_ee2f                             ! 非線形計算
  public eef_VorFromZeta_ee2f, eef_VelFromZeta_ee2f       ! 渦度速度計算
  public ee2f_ZetaFromVor_eef_eef_eef                     ! 渦度変換

  public IntZXV_zxv, AvrZXV_zxv                           ! 積分・平均
  public z_IntXV_zxv, z_AvrXV_zxv                         ! 積分・平均
  public x_IntZV_zxv, x_AvrZV_zxv                         ! 積分・平均
  public v_IntZX_zxv, v_AvrZX_zxv                         ! 積分・平均
  public zv_IntX_zxv, zv_AvrX_zxv                         ! 積分・平均
  public zx_IntV_zxv, zx_AvrV_zxv                         ! 積分・平均
  public xv_IntZ_zxv, xv_AvrZ_zxv                         ! 積分・平均

  public IntXV_xv,  AvrXV_xv                              ! 積分・平均
  public v_IntX_xv, v_AvrX_xv                             ! 積分・平均
  public x_IntV_xv, x_AvrV_xv                             ! 積分・平均
  public IntZX_zx, AvrZX_zx                               ! 積分・平均
  public z_IntX_zx, z_AvrX_zx                             ! 積分・平均
  public x_IntZ_zx, x_AvrZ_zx                             ! 積分・平均
  public IntZV_zv, AvrZV_zv                               ! 積分・平均
  public v_IntZ_zv, v_AvrZ_zv                             ! 積分・平均
  public z_IntV_zv, z_AvrV_zv                             ! 積分・平均

  public IntX_x, AvrX_x                                   ! 積分・平均
  public IntV_v, AvrV_v                                   ! 積分・平均
  public IntZ_z, AvrZ_z                                   ! 積分・平均

  public EnergyHelicityFromZeta_ee2f                      ! エネルギーヘリシティー
  public ESpectralFromZeta                                ! エネルギースペクトル

  public x_X, v_Y, z_Z                                    ! 座標変数
  public x_X_Weight, v_Y_Weight, z_Z_Weight               ! 座標変数
  public zxv_X, zxv_Y, zxv_Z                              ! 座標変数

  integer   :: im=32, jm=32, km=32                      ! 格子点の設定(X,Y,Z)
  integer   :: lm=10, mm=10, nm=10                      ! 切断波数の設定(X,Y,Z)

  integer, dimension(:),   pointer :: le => null()      ! X 波数範囲, 数
  integer, dimension(:),   pointer :: ls => null()      ! X 波数範囲, 数
  integer, dimension(:),   pointer :: lc => null()      ! X 波数範囲, 数
  integer, dimension(:),   pointer :: je => null()      ! Y 座標格子点範囲, 数
  integer, dimension(:),   pointer :: js => null()      ! Y 座標格子点範囲, 数
  integer, dimension(:),   pointer :: jc => null()      ! Y 座標格子点範囲, 数
  integer   :: np, ip                             ! MPI プロセス数, プロセス ID

  integer, dimension(:),   pointer :: itk => null()
  real(8), dimension(:),   pointer :: tk => null()
  integer, dimension(:),   pointer :: itj => null()
  real(8), dimension(:),   pointer :: tj => null()
  integer, dimension(:),   pointer :: iti => null()
  real(8), dimension(:),   pointer :: ti => null()


  real(8), dimension(:),   pointer :: x_X => null()   ! 格子点座標(X)
  real(8), dimension(:),   pointer :: v_Y => null()   ! 格子点座標(Y)
  real(8), dimension(:),   pointer :: z_Z => null()   ! 格子点座標(Y)

  real(8), dimension(:),   pointer :: x_X_Weight => null()
                                         ! 格子点重み(X)
                                         ! X 方向の格子点の間隔が格納してある.
  real(8), dimension(:),   pointer :: v_Y_Weight => null()
                                         ! 格子点重み(Y)
                                         ! Y 方向の格子点の間隔が格納してある.
  real(8), dimension(:),   pointer :: z_Z_Weight => null()
                                         ! 格子点重み(Y)
                                         ! Z 方向の格子点の間隔が格納してある.

  real(8), dimension(:,:,:), pointer :: zxv_X => null()
                          ! 格子点(X)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の X 座標を格納した格子データ
  real(8), dimension(:,:,:), pointer :: zxv_Y => null()
                          ! 格子点(Y)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の Y 座標を格納した格子データ
  real(8), dimension(:,:,:), pointer :: zxv_Z => null()
                          ! 格子点(Z)座標(3 次元)
                          ! 各格子点(i,j,k)の位置の Z 座標を格納した格子データ

  real(8), dimension(:), pointer :: w => null(), ws => null(), wg => null()
  real(8), dimension(:), pointer :: sgwork => null()

  integer, parameter :: nparams_max = 10  ! eee_Initial を呼べる最大回数
  type eee_param                          ! 解像度領域情報構造体
     integer   :: im, jm, km
     integer   :: lm, mm, nm
     integer, dimension(:),     pointer :: ls, le, lc
     integer, dimension(:),     pointer :: js, je, jc
     integer, dimension(:),     pointer :: itk
     real(8), dimension(:),     pointer :: tk
     integer, dimension(:),     pointer :: itj
     real(8), dimension(:),     pointer :: tj
     integer, dimension(:),     pointer :: iti
     real(8), dimension(:),     pointer :: ti
     real(8), dimension(:),     pointer :: x_X
     real(8), dimension(:),     pointer :: v_Y
     real(8), dimension(:),     pointer :: z_Z
     real(8), dimension(:),     pointer :: x_X_Weight
     real(8), dimension(:),     pointer :: v_Y_Weight
     real(8), dimension(:),     pointer :: z_Z_Weight
     real(8), dimension(:,:,:), pointer :: zxv_X
     real(8), dimension(:,:,:), pointer :: zxv_Y 
     real(8), dimension(:,:,:), pointer :: zxv_Z
     real(8), dimension(:),     pointer :: w, ws, wg
     real(8), dimension(:),     pointer :: sgwork
  end type eee_param
  type(eee_param) :: params(nparams_max)  ! 解像度領域情報
  integer :: nparams                      ! 解像度領域情報の個数

  real(8), parameter  :: pi=3.1415926535897932385D0

  save im, jm, km, lm, mm, nm, itk, tk, itj, tj, iti, ti
  save ls, le, lc, js, je, jc, np, ip
  save x_X, v_Y, z_Z, x_X_Weight, v_Y_Weight, z_Z_Weight, zxv_X, zxv_Y, zxv_Z
  save params, nparams

  contains
  !--------------- 初期化 -----------------
    subroutine eee_mpi_Initial(i,j,k,l,m,n,id)
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
      integer :: lls, lle, llc, jjs, jje, jjc
      integer :: iip
      integer :: lp, jp
      integer :: ierr

      im = i         ; jm = j         ; km = k
      lm = l         ; mm = m         ; nm = n

      if ( nparams .ge. nparams_max ) then
         call MessageNotify('W','eee_initial',&
              'too many call of eee_Initial, nothing was done.')
         if ( present(id) ) id = -1
         return
      end if

      CALL MPI_COMM_RANK(MPI_COMM_WORLD,IP,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NP,IERR)

      lp=lm/np+1
      lls=lp*ip
      lle=min(lp*(ip+1)-1,lm)
      if(lle.ge.lls) then
         llc=lle-lls+1
      else
         llc=0 ;  lls=0  ; lle=0
      end if

      jp=(jm-1)/np+1
      jjs=jp*ip
      jje=min(jp*(ip+1)-1,jm-1)
      if(jje.ge.jjs) then
         jjc=jje-jjs+1
      else
         jjc=0 ; jjs=0 ; jje=0
      end if

      nparams = nparams + 1

      params(nparams)%im = im
      params(nparams)%jm = jm
      params(nparams)%km = km
      params(nparams)%lm = lm
      params(nparams)%mm = mm
      params(nparams)%nm = nm
      allocate(params(nparams)%ls(0:np-1))
      allocate(params(nparams)%le(0:np-1))
      allocate(params(nparams)%lc(0:np-1))
      allocate(params(nparams)%js(0:np-1))
      allocate(params(nparams)%je(0:np-1))
      allocate(params(nparams)%jc(0:np-1))

      allocate(params(nparams)%itk(5))
      allocate(params(nparams)%itj(5))
      allocate(params(nparams)%iti(5))
      allocate(params(nparams)%tk(km*2))
      allocate(params(nparams)%tj(jm*2))
      allocate(params(nparams)%ti(im*2))
      allocate(params(nparams)%w(km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))
      allocate(params(nparams)%ws((2*mm+1)*(2*nm+1)*2*2*(lm+1)))
      allocate(params(nparams)%wg(4*km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))
      allocate(params(nparams)%sgwork(km*max(im*((jm-1)/np+1),jm*2*(lm/np+1))))

      allocate(params(nparams)%x_X(0:im-1))
      allocate(params(nparams)%x_X_Weight(0:im-1))
      allocate(params(nparams)%v_Y(jjs:jje))
      allocate(params(nparams)%v_Y_Weight(jjs:jje))
      allocate(params(nparams)%z_Z(0:km-1))
      allocate(params(nparams)%z_Z_Weight(0:km-1))
      allocate(params(nparams)%zxv_X(0:km-1,0:im-1,jjs:jje))
      allocate(params(nparams)%zxv_Y(0:km-1,0:im-1,jjs:jje))
      allocate(params(nparams)%zxv_Z(0:km-1,0:im-1,jjs:jje))

      call eee_ChangeResolution(nparams)

      call p3init(km,jm,im,itk,tk,itj,tj,iti,ti)

      do iip=0,np-1
         lp=lm/np+1
         ls(iip)=lp*iip
         le(iip)=min(lp*(iip+1)-1,lm)
         if(le(iip).ge.ls(iip)) then
            lc(iip)=le(iip)-ls(iip)+1
         else
            lc(iip)=0 ;  ls(iip)=0  ; le(iip)=0
         end if

         jp=(jm-1)/np+1
         js(iip)=jp*iip
         je(iip)=min(jp*(iip+1)-1,jm-1)
         if(je(iip).ge.js(iip)) then
            jc(iip)=je(iip)-js(iip)+1
         else
            jc(iip)=0 ; js(iip)=0 ; je(iip)=0
         end if
      enddo

      do ii=0,im-1
         x_X(ii) = 2*pi/im*ii
      enddo
      x_X_Weight = 2*pi/im

      do jj=js(ip),je(ip)
         v_Y(jj) = 2*pi/jm*jj
      enddo
      v_Y_Weight = 2*pi/jm

      do kk=0,km-1
         z_Z(kk) = 2*pi/km*kk
      enddo
      z_Z_Weight = 2*pi/km

      zxv_X = spread(spread(x_X,1,km),3,jc(ip))
      zxv_Y = spread(spread(v_Y,1,im),1,km)
      zxv_Z = spread(spread(z_Z,2,im),3,jc(ip))

      if ( present(id) ) id = nparams

      write(cid,'(I3)') nparams
      call MessageNotify('M','eee_mpi_initial','eee_mpi_module is initialized')
      call MessageNotify('M','eee_mpi_initial',&
           'Resolution ID is '//trim(adjustl(cid)))
    end subroutine eee_mpi_Initial

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
      ls => params(id)%ls
      le => params(id)%le
      lc => params(id)%lc
      js => params(id)%js
      je => params(id)%je
      jc => params(id)%jc
      itk => params(id)%itk
      tk  => params(id)%tk
      itj => params(id)%itj
      tj  => params(id)%tj
      iti => params(id)%iti
      ti  => params(id)%ti
      x_X => params(id)%x_X
      v_Y => params(id)%v_Y
      z_Z => params(id)%z_Z
      x_X_Weight => params(id)%x_X_Weight
      v_Y_Weight => params(id)%v_Y_Weight
      z_Z_Weight => params(id)%z_Z_Weight
      zxv_X => params(id)%zxv_X
      zxv_Y => params(id)%zxv_Y
      zxv_Z => params(id)%zxv_Z
      w => params(id)%w
      ws => params(id)%ws
      wg => params(id)%wg
      sgwork => params(id)%sgwork

    end subroutine eee_ChangeResolution

  !--------------- 波数位置問い合わせ -----------------
    function lf_l(l)
      !
      ! スペクトルデータ eef(-nm:nm,-mm,mm,2*lc) での
      ! X 方向波数 L の位置情報(第 3 添え字)を調べる.
      !
      ! X 方向波数については ls,ls+1,... le, -le,-le+1,...,-ls
      ! の順に格納されている(ISPACK/P3PACK マニュアル参照のこと)
      !
      integer, intent(IN)  ::  l        ! X 方向波数
      integer              ::  lf_l     ! スペクトルデータ第 3 添え字

      if ( abs(l) < ls(ip) .OR. le(ip) < abs(l) ) then
         call MessageNotify('E','l_l','Input wavenumber out of range')
      endif
      
      if ( l >= 0 ) then
         lf_l = l-ls(ip)+1
      else
         lf_l = ls(ip)+2*lc(ip)-abs(l)
      endif

    end function lf_l

  !--------------- 基本変換 -----------------
    function zxv_eef(eef)
      !
      ! スペクトルデータから格子データへ変換する.
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip))        :: zxv_eef
                                                        !(out) 格子点データ
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in) :: eef
                                                        !(in)  スペクトルデータ
      sgwork(1:(2*nm+1)*(2*mm+1)*2*lc(ip)) &
           = reshape(eef,(/(2*nm+1)*(2*mm+1)*2*lc(ip)/))

      call p3smgb(nm,mm,lm,km,jm,im,sgwork,w,itk,tk,itj,tj,iti,ti)

      zxv_eef = reshape(sgwork(1:im*km*jc(ip)),(/km,im,jc(ip)/))
    end function zxv_eef

    function eef_zxv(zxv)
      !
      ! 格子データからスペクトルデータへ変換する.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))                  :: eef_zxv
                                                      !(out)  スペクトルデータ
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(in) :: zxv
                                                      !(in) 格子点データ

      sgwork(1:im*jc(ip)*km) = reshape(zxv,(/im*jc(ip)*km/))

      call p3gmsb(nm,mm,lm,km,jm,im,sgwork,w,itk,tk,itj,tj,iti,ti)

      eef_zxv = reshape(sgwork(1:(2*nm+1)*(2*mm+1)*2*lc(ip)),&
                               (/2*nm+1,2*mm+1,2*lc(ip)/))

    end function eef_zxv

    function eee2_ee2f(ee2f)
      !
      ! 分散スペクトルデータを集積する
      !
      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2)             :: eee2_ee2f
      !(out)  スペクトルデータ

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)),intent(IN) :: ee2f
      !(in) 分散スペクトルデータ
      
      real(8), dimension(-nm:nm,-mm:mm,2,0:2*sum(lc)-1)       :: ee2e
      ! 作業データ
      integer, dimension(0:np-1) :: nm2ls
      integer :: ierr, iip
      integer :: l

      nm2ls(0)   = 0
      do iip=1,np-1
         nm2ls(iip) = nm2ls(iip-1) + (2*nm+1)*(2*mm+1)*2*2*lc(iip-1)
      enddo

      call MPI_ALLGATHERV(ee2f,(2*nm+1)*(2*mm+1)*2*2*lc(ip),MPI_REAL8,&
                          ee2e,(2*nm+1)*(2*mm+1)*2*2*lc,nm2ls,&
                          MPI_REAL8,MPI_COMM_WORLD,IERR)

      do l=ls(0),le(0)
         eee2_ee2f(:,:,l,:) = ee2e(:,:,:,l-ls(0))
         if ( l /= 0 ) then
            eee2_ee2f(:,:,-l,:) = ee2e(:,:,:,2*lc(0)-(l-ls(0)+1))
         endif
      enddo

      do iip=1,np-1
         do l=ls(iip),le(iip)
            eee2_ee2f(:,:,l,:) = ee2e(:,:,:,2*sum(lc(0:iip-1))+(l-ls(iip)))
            eee2_ee2f(:,:,-l,:) = ee2e(:,:,:,2*sum(lc(0:iip))-(l-ls(iip)+1))
         enddo
      enddo

    end function eee2_ee2f

    function ee2f_eee2(eee2)
      !
      ! スペクトルデータを分散する
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))       :: ee2f_eee2
      !(out)  スペクトルデータ

      real(8), dimension(-nm:nm,-mm:mm,-lm:lm,2), intent(IN) :: eee2
      !(in) 分散スペクトルデータ
      
      ! 作業データ
      integer :: ierr, iip
      integer :: l 

      do l=ls(ip),le(ip)
         ee2f_eee2(:,:,:,l-ls(ip)+1) = eee2(:,:,l,:)
         ee2f_eee2(:,:,:,2*lc(ip)-(l-ls(ip))) = eee2(:,:,-l,:)
      enddo
      
    end function ee2f_eee2

    function xyz_zxv(zxv)
      !
      ! 分散格子データの集積
      !
      real(8), dimension(0:im-1,0:jm-1,0:km-1)             :: xyz_zxv
                                                        !(out) 格子点データ

      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(in)  :: zxv
                                                        !(in) 格子点データ
      real(8), dimension(0:km-1,0:im-1,0:jm-1)  :: zxy
      ! 作業変数

      integer :: i,j,k
      integer :: ierr

      call MPI_ALLGATHERV(zxv,km*im*jc(ip),MPI_REAL8,&
                          zxy,km*im*jc,km*im*js,MPI_REAL8,&
                          MPI_COMM_WORLD,IERR)

      do k=0,km-1
         do j=0,jm-1
            do i=0,im-1
               xyz_zxv(i,j,k) = zxy(k,i,j)
            enddo
         enddo
      enddo

    end function xyz_zxv
    
  !--------------- 微分計算 -----------------
    function eef_Lapla_eef(eef)
      !
      ! 入力スペクトルデータにラプラシアン(∂xx+∂yy+∂zz)を作用する.
      !
      ! スペクトルデータのラプラシアンとは, 対応する格子点データに
      ! ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (l**2 + m**2 + n**2) をかける
      ! 計算を行っている. 
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Lapla_eef
      !(out) スペクトルデータのラプラシアン

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数

      eef_Lapla_eef = 0.0

      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Lapla_eef(n,m,lf_l(l)) = -(l**2+m**2+n**2)*eef(n,m,lf_l(l))
               eef_Lapla_eef(n,m,lf_l(-l)) = -(l**2+m**2+n**2)*eef(n,m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Lapla_eef

    function eef_LaplaInv_eef(eef)
      !
      ! 入力スペクトルデータに逆ラプラシアン(∂xx+∂yy+∂zz)**(-1)を作用する.
      !
      ! スペクトルデータの逆ラプラシアンとは, 対応する格子点データに
      ! 逆ラプラシアンを作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに全波数 (l**2 + m**2 + n**2) で割る
      ! 計算を行っている. l=m=n=0 成分には 0 を代入している. 
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))             :: eef_LaplaInv_eef
      !(out) スペクトルデータの逆ラプラシアン

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in) :: eef
      !(in) スペクトルデータ

      integer l,m,n

      eef_LaplaInv_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l.ne.0 .or. m.ne.0 .or. n.ne.0 ) then
                  eef_LaplaInv_eef(n,m,lf_l(l)) = -eef(n,m,lf_l(l))/(l**2+m**2+n**2)
                  eef_LaplaInv_eef(n,m,lf_l(-l)) = -eef(n,m,lf_l(-l))/(l**2+m**2+n**2)
               endif
            enddo
         enddo
      enddo
    end function eef_LaplaInv_eef

    function eef_Dx_eef(eef)
      !
      ! 入力スペクトルデータに X 微分(∂x)を作用する.
      !
      ! スペクトルデータの X 微分とは, 対応する格子点データに X 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに X 方向波数 l をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dx_eef
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数

      eef_Dx_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dx_eef(n,m,lf_l(l)) = -l*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dx_eef(n,m,lf_l(l)) = -l*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo
    end function eef_Dx_eef

    function eef_Dy_eef(eef)
      !
      ! 入力スペクトルデータに Y 微分(∂y)を作用する.
      !
      ! スペクトルデータの Y 微分とは, 対応する格子点データに Y 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに Y 方向波数 m をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dy_eef
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数

      eef_Dy_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dy_eef(n,m,lf_l(l)) = -m*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dy_eef(n,m,lf_l(l)) = -m*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Dy_eef

    function eef_Dz_eef(eef)
      !
      ! 入力スペクトルデータに Z 微分(∂z)を作用する.
      !
      ! スペクトルデータの Z 微分とは, 対応する格子点データに Z 微分を
      ! 作用させたデータのスペクトル変換のことである.
      !
      ! 実際にはスペクトルデータに Z 方向波数 n をかけて
      ! 実部 <-> 虚部成分に入れ換える計算を行っている.
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))              :: eef_Dz_eef
      !(out) スペクトルデータの X 微分

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef
      !(in) 入力スペクトルデータ

      integer l,m,n
      ! 作業変数

      eef_Dz_eef = 0.0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dz_eef(n,m,lf_l(l)) = -n*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               eef_Dz_eef(n,m,lf_l(l)) = -n*eef(-n,-m,lf_l(-l))
            enddo
         enddo
      enddo

    end function eef_Dz_eef

  !--------------- 非線形項計算 -----------------

    function ee2f_RotVelxVor_ee2f(ee2f)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから Euler 方程式の非線形項
      !
      !     ▽x(u x ω) 
      !
      !  の 2 成分を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))     :: ee2f_RotVelxVor_ee2f
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      call p3emnl(nm,mm,lm,km,jm,im,ee2f,ee2f_RotVelxVor_ee2f, &
                  wg,itk,tk,itj,tj,iti,ti)

    end function ee2f_RotVelxVor_ee2f

    function eef_VorFromZeta_ee2f(ee2f,isw)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから渦度のスペクトルの 1 成分
      !  を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))     :: eef_VorFromZeta_ee2f
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      integer, intent(IN) :: isw
      !(in) 出力する渦度の成分のインデックス(1,2,3)

      call p3gmto(nm,mm,lm,ee2f,eef_VorFromZeta_ee2f,isw)

    end function eef_VorFromZeta_ee2f

    function eef_VelFromZeta_ee2f(ee2f,isw)
      !
      !  渦度 2 成分(ζ_1, ζ_2)のスペクトルデータから速度スペクトルの 1 成分
      !  を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip))     :: eef_VelFromZeta_ee2f
      !(out) 非線形項のスペクトルデータの 2 つの成分

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in)  :: ee2f
      !(in) 入力スペクトルデータ. 渦度の 2 成分(ζ_1, ζ_2)

      integer, intent(IN) :: isw
      !(in) 出力する渦度の成分のインデックス(1,2,3)

      call p3gmtu(nm,mm,lm,ee2f,eef_VelFromZeta_ee2f,isw)

    end function eef_VelFromZeta_ee2f

    function ee2f_ZetaFromVor_eef_eef_eef(eef_1,eef_2,eef_3)
      !
      !  渦度のスペクトルから渦度 2 成分(ζ_1, ζ_2)を計算する.
      !
      !  (ζ_1, ζ_2) と渦度 ω との関係は ISPACK/P3PACK のマニュアルを参照
      !
      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip))     :: ee2f_ZetaFromVor_eef_eef_eef
      !(out) 渦度の 2 成分(ζ_1, ζ_2)

      real(8), dimension(-nm:nm,-mm:mm,2*lc(ip)), intent(in)  :: eef_1, eef_2, eef_3
      !(in) 渦度スペクトルデータの各成分

      integer :: l,m,n

      ee2f_ZetaFromVor_eef_eef_eef = 0.0D0
      do l=ls(ip),le(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_2(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_3(n,m,lf_l(l))
               elseif( m /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_3(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_1(n,m,lf_l(l))
               else
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_1(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_2(n,m,lf_l(l))
               endif
            enddo
         enddo
      enddo

      do l=-le(ip),-ls(ip)
         do m=-mm,mm
            do n=-nm,nm
               if ( l /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_2(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_3(n,m,lf_l(l))
               elseif( m /= 0 ) then
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_3(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_1(n,m,lf_l(l))
               else
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,1,lf_l(l)) = eef_1(n,m,lf_l(l))
                  ee2f_ZetaFromVor_eef_eef_eef(n,m,2,lf_l(l)) = eef_2(n,m,lf_l(l))
               endif
            enddo
         enddo
      enddo

    end function ee2f_ZetaFromVor_eef_eef_eef

  !--------------- 積分計算 -----------------
    function IntZXV_zxv(zxv)
      !
      ! 3 次元格子点データの全領域積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in)  3 次元格子点データ

      real(8)                                    :: IntZXV_zxv
      !(out) 積分値

      real(8) :: IntZXVTMP
      integer :: i, j, k
      integer :: ierr
      ! 作業変数

      IntZXV_zxv = 0.0d0
      do j=js(ip),je(ip)
         do i=0,im-1
            do k=0,km-1
               IntZXV_zxv = IntZXV_zxv &
                    + zxv(k,i,j) * z_Z_Weight(k) * v_Y_Weight(j) * x_X_Weight(i)
            enddo
         enddo
      end do

      IntZXVTMP=IntZXV_zxv
      CALL MPI_ALLREDUCE(IntZXVTMP,IntZXV_zxv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntZXV_zxv

    function z_IntXV_zxv(zxv)
      !
      ! 3 次元格子点データの X,Y 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1)          :: z_IntXV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8), dimension(0:km-1)          :: z_IntXVTmp
      integer :: i, j
      integer :: ierr
      ! 作業変数

      z_IntXV_zxv = 0.0d0
      do i=0,im-1
         do j=js(ip),je(ip)
            z_IntXV_zxv(:) = &
                 z_IntXV_zxv(:) + zxv(:,i,j) * x_X_Weight(i)* v_Y_Weight(j)
         enddo
      enddo

      z_IntXVTMP=z_IntXV_zxv
      CALL MPI_ALLREDUCE(z_IntXVTMP,z_IntXV_zxv,km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntXV_zxv

    function v_IntZX_zxv(zxv)
      !
      ! 3 次元格子点データの X,Z 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_IntZX_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i, k
      ! 作業変数

      v_IntZX_zxv = 0.0d0
      do i=0,im-1
         do k=0,km-1
            v_IntZX_zxv(:) = &
                 v_IntZX_zxv(:) + zxv(k,i,:) * x_X_Weight(i)* z_Z_Weight(k)
         enddo
      enddo
    end function v_IntZX_zxv

    function x_IntZV_zxv(zxv)
      !
      ! 3 次元格子点データの Y,Z 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_IntZV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8), dimension(0:im-1)          :: x_IntZVTMP
      integer :: j, k
      integer :: ierr
      ! 作業変数

      x_IntZV_zxv = 0.0d0
      do j=js(ip),je(ip)
         do k=0,km-1
            x_IntZV_zxv(:) = &
                 x_IntZV_zxv(:) + zxv(k,:,j) * v_Y_Weight(j)* z_Z_Weight(k)
         enddo
      enddo

      x_IntZVTMP=x_IntZV_zxv
      CALL MPI_ALLREDUCE(x_IntZVTMP,x_IntZV_zxv,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function x_IntZV_zxv

    function zv_IntX_zxv(zxv)
      !
      ! 3 次元格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,js(ip):je(ip))          :: zv_IntX_zxv
      !(out) 積分された 2 次元(ZY)格子点データ

      integer :: i
      ! 作業変数

      zv_IntX_zxv = 0.0d0
      do i=0,im-1
         zv_IntX_zxv(:,:) = zv_IntX_zxv(:,:) + zxv(:,i,:) * x_X_Weight(i)
      enddo

    end function zv_IntX_zxv

    function zx_IntV_zxv(zxv)
      !
      ! 3 次元格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8), dimension(0:km-1,0:im-1)          :: zx_IntVTMP
      integer :: j
      integer :: ierr
      ! 作業変数

      zx_IntV_zxv = 0.0d0
      do j=js(ip),je(ip)
         zx_IntV_zxv(:,:) = zx_IntV_zxv(:,:) + zxv(:,:,j) * v_Y_Weight(j)
      enddo

      zx_IntVTMP=zx_IntV_zxv
      CALL MPI_ALLREDUCE(zx_IntVTMP,zx_IntV_zxv,im*km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function zx_IntV_zxv

    function xv_IntZ_zxv(zxv)
      !
      ! 3 次元格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1,js(ip):je(ip))          :: xv_IntZ_zxv
      !(out) 積分された 1 次元(YX)格子点データ

      integer :: k 
      ! 作業変数

      xv_IntZ_zxv = 0.0d0
      do k=0,km-1
         xv_IntZ_zxv(:,:) = xv_IntZ_zxv(:,:) + zxv(k,:,:) * z_Z_Weight(k)
      enddo

    end function xv_IntZ_zxv

    function IntXV_xv(xv)
      !
      ! 2 次元(XY)格子点データの全領域積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv          
      !(in)  2 次元(YX)格子点データ

      real(8)                             :: IntXV_xv
      !(out) 積分値

      real(8) :: IntXVTMP
      integer :: i, j
      integer :: ierr
      ! 作業変数

      IntXV_xv = 0.0d0
      do j=js(ip),je(ip)
         do i=0,im-1
            IntXV_xv = IntXV_xv + xv(i,j) * v_Y_Weight(j) * x_X_Weight(i)
         enddo
      enddo

      IntXVTMP=IntXV_xv
      CALL MPI_ALLREDUCE(IntXVTMP,IntXV_xv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntXV_xv

    function v_IntX_xv(xv)
      !
      ! 2 次元(XV)格子点データの X 方向積分
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 次元(YX)格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_IntX_xv
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: i
      ! 作業変数

      v_IntX_xv = 0.0d0
      do i=0,im-1
         v_IntX_xv(:) = v_IntX_xv(:) + xv(i,:) * x_X_Weight(i)
      enddo
    end function v_IntX_xv

    function x_IntV_xv(xv)
      !
      ! 2 次元(YX)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv      
      !(in)  2 次元(XV)格子点データ

      real(8), dimension(0:im-1)        :: x_IntV_xv 
      !(out) 積分された 1 次元(X)格子点データ

      real(8), dimension(0:im-1)        :: x_IntVTMP
      integer :: j
      integer :: ierr
      ! 作業変数

      x_IntV_xv = 0.0d0
      do j=js(ip),je(ip)
         x_IntV_xv(:) = x_IntV_xv(:) + xv(:,j) * v_Y_Weight(j)
      enddo

      x_IntVTMP=x_IntV_xv
      CALL MPI_ALLREDUCE(x_IntVTMP,x_IntV_xv,im,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function x_IntV_xv

    function IntZV_zv(zv)
      !
      ! 2 次元(ZY)格子点データの全領域積分および平均.
      !
      ! 実際には格子点データ各点毎に z_Z_Weight, y_Y_Weight をかけた
      ! 総和を計算している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 次元(ZY)格子点データ

      real(8)                             :: IntZV_zv
      !(out) 積分値

      real(8) :: IntZVTMP
      integer :: j, k
      integer :: ierr
      ! 作業変数

      IntZV_zv = 0.0d0
      do j=js(ip),je(ip)
         do k=0,km-1
            IntZV_zv = IntZV_zv + zv(k,j) * v_Y_Weight(j) * z_Z_Weight(k)
         enddo
      enddo

      IntZVTMP=IntZV_zv
      CALL MPI_ALLREDUCE(IntZVTMP,IntZV_zv,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function IntZV_zv

    function v_IntZ_zv(zv)
      !
      ! 2 次元(ZY)格子点データの Z 方向積分
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 次元(ZY)格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_IntZ_zv
      !(out) 積分された 1 次元(Y)格子点データ

      integer :: k
      ! 作業変数

      v_IntZ_zv = 0.0d0
      do k=0,km-1
         v_IntZ_zv(:) = v_IntZ_zv(:) + zv(k,:) * z_Z_Weight(k)
      enddo
    end function v_IntZ_zv

    function z_IntV_zv(zv)
      !
      ! 2 次元(ZY)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv          
      !(in)  2 次元格子点データ

      real(8), dimension(0:km-1)        :: z_IntV_zv 
      !(out) 積分された 1 次元(X)格子点データ

      real(8), dimension(0:km-1)        :: z_IntVTMP
      integer :: j
      integer :: ierr
      ! 作業変数

      z_IntV_zv = 0.0d0
      do j=js(ip),je(ip)
         z_IntV_zv(:) = z_IntV_zv(:) + zv(:,j) * v_Y_Weight(j)
      enddo

      z_IntVTMP=z_IntV_zv
      CALL MPI_ALLREDUCE(z_IntVTMP,z_IntV_zv,km,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)

    end function z_IntV_zv

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

    function IntV_v(v)
      !
      ! 1 次元(Y)格子点データの Y 方向積分
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算している. 
      !
      real(8), dimension(js(ip):je(ip))    :: v          !(in)  1 次元格子点データ
      real(8)                      :: IntV_v     !(out) 積分値
      
      real(8) :: IntVTMP
      integer :: ierr

      IntV_v = sum(v*v_Y_Weight)

      IntVTMP=IntV_v
      CALL MPI_ALLREDUCE(IntVTMP,IntV_v,1,MPI_REAL8, &
                         MPI_SUM,MPI_COMM_WORLD,IERR)
    end function IntV_v

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
    function AvrZXV_zxv(zxv)
      !
      ! 2 次元格子点データの全領域平均.
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight, z_Z_Weight を
      ! かけた総和を計算し, x_X_Weight*y_Y_Weight*z_Z_Weight の総和で割る
      ! ことで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in)  3 次元格子点データ

      real(8)                                    :: AvrZXV_zxv
      !(out) 積分値

      real(8) :: Vol, VolTMP
      integer :: ierr
      ! 作業変数

      VolTMP=sum(x_X_weight)*sum(v_Y_weight)*sum(z_Z_weight)
      
      CALL MPI_ALLREDUCE(VolTMP,Vol,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrZXV_zxv = IntZXV_zxv(zxv)/Vol

    end function AvrZXV_zxv

    function z_AvrXV_zxv(zxv)
      !
      ! 3 次元格子点データの X,Y 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1)          :: z_AvrXV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! 作業変数

      AreaTMP = sum(x_X_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      z_AvrXV_zxv = z_IntXV_zxv(zxv)/Area

    end function z_AvrXV_zxv

    function v_AvrZX_zxv(zxv)
      !
      ! 3 次元格子点データの X,Z 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, z_Z_Weight をかけた
      ! 総和を計算し, x_X_Weight*z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_AvrZX_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      v_AvrZX_zxv = v_IntZX_zxv(zxv)/(sum(x_X_weight)*sum(z_Z_weight))

    end function v_AvrZX_zxv

    function x_AvrZV_zxv(zxv)
      !
      ! 3 次元格子点データの Y,Z 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight, z_Z_Weight をかけた
      ! 総和を計算し, y_Y_Weight*z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_AvrZV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! 作業変数

      AreaTMP = sum(z_Z_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      x_AvrZV_zxv = x_IntZV_zxv(zxv)/Area

    end function x_AvrZV_zxv

    function zv_AvrX_zxv(zxv)
      !
      ! 3 次元格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた
      ! 総和を計算し, x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,js(ip):je(ip))          :: zv_AvrX_zxv
      !(out) 積分された 2 次元(ZY)格子点データ

      zv_AvrX_zxv = zv_IntX_zxv(zxv)/sum(x_X_weight)

    end function zv_AvrX_zxv

    function zx_AvrV_zxv(zxv)
      !
      ! 3 次元格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた
      ! 総和を計算し, v_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:km-1,0:im-1)          :: zx_AvrV_zxv
      !(out) 積分された 1 次元(Y)格子点データ

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! 作業変数

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      zx_AvrV_zxv = zx_IntV_zxv(zxv)/Length

    end function zx_AvrV_zxv

    function xv_AvrZ_zxv(zxv)
      !
      ! 3 次元格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた
      ! 総和を計算し, z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,0:im-1,js(ip):je(ip)), intent(IN)   :: zxv
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1,js(ip):je(ip))          :: xv_AvrZ_zxv
      !(out) 積分された 1 次元(YX)格子点データ

      xv_AvrZ_zxv = xv_IntZ_zxv(zxv)/sum(z_Z_weight)

    end function xv_AvrZ_zxv

    function AvrXV_xv(xv)
      !
      ! 2 次元格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight, y_Y_Weight をかけた
      ! 総和を計算し, x_X_Weight*y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in)  2 次元格子点データ

      real(8)                             :: AvrXV_xv    
      !(out) 平均値

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! 作業変数

      AreaTMP = sum(x_X_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrXV_xv = IntXV_xv(xv)/Area

    end function AvrXV_xv

    function v_AvrX_xv(xv)
      !
      ! 2 次元格子点データの X 方向平均
      !
      ! 実際には格子点データ各点毎に x_X_Weight をかけた総和を計算し, 
      ! x_X_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 次元格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_AvrX_xv
      !(out) 平均された 1 次元(Y)格子点データ

      v_AvrX_xv = v_IntX_xv(xv)/sum(x_X_weight)
    end function v_AvrX_xv

    function x_AvrV_xv(xv)
      !
      ! 2 次元格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:im-1,js(ip):je(ip)), intent(IN)   :: xv
      !(in) 2 次元格子点データ

      real(8), dimension(0:im-1)          :: x_AvrV_xv
      !(out) 平均された 1 次元(X)格子点データ

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! 作業変数

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      x_AvrV_xv = x_IntV_xv(xv)/Length
    end function x_AvrV_xv

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

    function AvrZV_zv(zv)
      !
      ! 2 次元(ZV)格子点データの全領域平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight, y_Y_Weight をかけた
      ! 総和を計算し, z_Z_Weight*v_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in)  2 次元(ZY)格子点データ

      real(8)                             :: AvrZV_zv    
      !(out) 平均値

      real(8) :: Area, AreaTMP
      integer :: ierr
      ! 作業変数

      AreaTMP = sum(z_Z_weight)*sum(v_Y_weight)
      CALL MPI_ALLREDUCE(AreaTMP,Area,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrZV_zv = IntZV_zv(zv)/Area
    end function AvrZV_zv

    function z_AvrV_zv(zv)
      !
      ! 2 次元(ZY)格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に v_Y_Weight をかけた総和を計算し, 
      ! v_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in) 2 次元(ZV)格子点データ

      real(8), dimension(0:km-1)          :: z_AvrV_zv
      !(out) 平均された 1 次元(Z)格子点データ
      real(8) :: Length, LengthTMP
      integer :: ierr
      ! 作業変数

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      z_AvrV_zv = z_IntV_zv(zv)/Length
    end function z_AvrV_zv

    function v_AvrZ_zv(zv)
      !
      ! 2 次元(ZY)格子点データの Z 方向平均
      !
      ! 実際には格子点データ各点毎に z_Z_Weight をかけた総和を計算し, 
      ! z_Z_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(0:km-1,js(ip):je(ip)), intent(IN)   :: zv
      !(in) 2 次元(ZY)格子点データ

      real(8), dimension(js(ip):je(ip))          :: v_AvrZ_zv
      !(out) 平均された 1 次元(X)格子点データ

      v_AvrZ_zv = v_IntZ_zv(zv)/sum(z_Z_weight)
    end function v_AvrZ_zv

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

    function AvrV_v(v)
      !
      ! 1 次元(Y)格子点データの Y 方向平均
      !
      ! 実際には格子点データ各点毎に y_Y_Weight をかけた総和を計算し, 
      ! y_Y_Weight の総和で割ることで平均している. 
      !
      real(8), dimension(js(ip):je(ip)), intent(IN) :: v
      !(in)  1 次元格子点データ

      real(8)                               :: AvrV_v !(out) 平均値

      real(8) :: Length, LengthTMP
      integer :: ierr
      ! 作業変数

      LengthTMP = sum(v_Y_weight)
      CALL MPI_ALLREDUCE(LengthTMP,Length,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,IERR)

      AvrV_v = IntV_v(v)/Length
    end function AvrV_v

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
    function EnergyHelicityFromZeta_ee2f(ee2f)
      !
      ! 渦度成分(ζ_1, ζ_2)から全領域平均エネルギーとヘリシティーを計算する.
      !
      real(8), dimension(2)                 :: EnergyHelicityFromZeta_ee2f
      ! エネルギーヘリシティー

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in) :: ee2f
      ! 渦度成分(ζ_1, ζ_2)

      real(8) :: E, H           ! エネルギー, ヘリシティー
      
      call p3cmsv(nm,mm,lm,ee2f,E,H)

      EnergyHelicityFromZeta_ee2f(1) = E ; EnergyHelicityFromZeta_ee2f(2) = H

    end function EnergyHelicityFromZeta_ee2f

    subroutine ESpectralFromZeta(esp,ee2f)
      !
      ! 渦度成分(ζ_1, ζ_2)からエネルギースペクトルを計算する. 
      !
      !   * esp のサイズで求められる波数範囲が定められる
      !   * esp の総和が EFFromZeta で求められるエネルギー
      !     (全領域平均値)に等しい
      !
      real(8), dimension(:), intent(OUT)  :: esp
      ! エネルギースペクトル

      real(8), dimension(-nm:nm,-mm:mm,2,2*lc(ip)), intent(in) :: ee2f
      ! 渦度成分(ζ_1, ζ_2)

      integer kmax
      ! 作業変数
      real(8), allocatable :: wesp(:)

      kmax=size(esp)
      allocate(wesp(kmax))
      call p3empt(nm,mm,lm,kmax,ee2f,esp,wesp)
      deallocate(wesp)

    end subroutine ESpectralFromZeta

  end module eee_mpi_module
