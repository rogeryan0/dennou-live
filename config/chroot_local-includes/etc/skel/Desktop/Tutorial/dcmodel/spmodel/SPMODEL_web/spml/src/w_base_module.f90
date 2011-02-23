!--
!----------------------------------------------------------------------
!     Copyright (c) 2002--2006 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_base_module
!
!  spml/w_base_module モジュールは球面上での 2 次元流体運動を球面調和函
!  数を用いたスペクトル法によって数値計算するためのモジュール w_module
!  の下部モジュールであり, スペクトル計算の基本的な Fortran90 関数を提
!  供する.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んで
!  いる. スペクトルデータおよび格子点データの格納方法や変換の詳しい計算
!  法については ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!== 履歴
!
!      2001/12/08  竹広真一
!      2001/12/26  竹広真一  関数,変数名前変更
!      2002/02/07  竹広真一  関数,変数名前再変更
!      2002/03/30  竹広真一  関数,変数名前再再変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2005/03/13  竹広真一  l_nm, nm_l を配列で引数を渡せるように拡張
!      2005/07/04  竹広真一  OpenMP 版変換ルーチンに対応
!                            バンク競合を避けるための作業配列追加
!      2005/07/10  竹広真一  OpenMP セットアップのメッセージ出力
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2008/02/23  佐々木洋平 格子点データの配列を(im,jm) から (0:im-1, 0:jm-1)
!                             に変更.
!      2008/06/25  佐々木洋平 格子点データの配列を(0:im-1,1:jm) に変更
!      2008/07/04  佐々木洋平 コメントを RDoc 用に微修正
!      2008/12/28  竹広真一   xy_w, w_xy のコメントを追加
!      2009/01/09  竹広真一   w_base_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/30  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module w_base_module
  !
  != w_base_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_base_module.f90,v 1.16 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要.
  !
  ! spml/w_base_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための
  ! モジュール w_module の下部モジュールであり, スペクトル法の
  ! 基本的な Fortran90 関数を提供する.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチン
  ! を呼んでいる. スペクトルデータおよび格子点データの格納方法
  ! や変換の詳しい計算法については ISPACK/SNPACK,SPPACK のマニ
  ! ュアルを参照されたい.
  !
  use dc_message
  implicit none

  integer               :: im=64            ! 格子点の設定(東西)
  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: nm=21            ! 切断波数の設定
  integer               :: np=1             ! OPENMP 最大スレッド数

  logical               :: openmp=.false.   ! OPENMP スイッチ

  integer               :: it(6)            ! 変換用配列
  real(8), allocatable  :: t(:)             ! 変換用配列
  integer, allocatable  :: ip(:)            ! 変換用配列
  real(8), allocatable  :: p(:), r(:)       ! 変換用配列
  integer, allocatable  :: ia(:)            ! 変換用配列
  real(8), allocatable  :: a(:)             ! 変換用配列
  real(8), allocatable  :: y(:,:)           ! 変換用配列

  real(8), allocatable  :: q(:)             ! 作業配列
  real(8), allocatable  :: ww(:), ws(:)     ! 作業配列
  real(8), allocatable  :: wv(:)            ! 作業配列(OPENMP用)

  real(8), allocatable  :: x_Lon(:), y_Lat(:)                ! 緯度経度
  real(8), allocatable  :: x_Lon_Weight(:), y_Lat_Weight(:)  ! 座標重み
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  integer               :: id=65, jd=33     ! xy_work の大きさ
  integer               :: iw               ! ww,ws の大きさ

  real(8), parameter    :: pi=3.1415926535897932385D0

  private

  public im, jm, nm                           ! 格子点数, 切断波数, 半径
  public it, t, y, ip, p, r, ia, a            ! 変換用作業配列
  public openmp, np                           ! OPENMP 用変数

  public w_base_Initial                       ! 初期化サブルーチン
  public x_Lon, y_Lat                         ! 格子座標
  public x_Lon_Weight, y_Lat_Weight           ! 格子座標重み
  public xy_Lon, xy_Lat                       ! 格子座標(im,jm)
  public l_nm, nm_l                           ! 波数格納位置
  public xy_w, w_xy                           ! 変換関数

  interface l_nm
     module procedure l_nm_array00
     module procedure l_nm_array01
     module procedure l_nm_array10
     module procedure l_nm_array11
  end interface

  interface nm_l
     module procedure nm_l_int
     module procedure nm_l_array
  end interface

  save im, jm, nm                             ! 格子点数, 切断波数, 半径を記憶
  save it, t, y, ip, p, r, ia, a              ! 変換用配列を記憶
  save id, jd, iw                             ! 変換用配列の大きさ

  contains
  !--------------- 初期化 -----------------
    subroutine w_base_Initial(n_in,i_in,j_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数および OPENMP 使用時の
      ! 最大スレッド数を設定する.
      !
      ! 実際の使用には上位サブルーチン w_Initial を用いること.
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西)
      integer,intent(in) :: j_in              !(in) 格子点数(南北)
      integer,intent(in) :: n_in              !(in) 切断全波数
      integer,intent(in), optional :: np_in   !(in) OPENMP での最大スレッド数

      integer :: i, j

      im = i_in  ; jm = j_in  ; nm = n_in

      if ( present(np_in) )then
         np = np_in

         if ( np .gt. 1 ) then
            openmp = .true. 
            allocate(wv((nm+4)*(nm+3)*np))
            call MessageNotify('M','w_base_Initial', &
                 'OpenMP computation was set up.')
         else
            openmp = .false. 
         endif

      else
         openmp = .false. 
      endif

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( openmp ) then
         jd = jm
      else if ( jm/2*2 .eq. jm ) then
         jd = jm+1
      else
         jd = jm
      endif
      allocate(t(im*2))                       ! 変換用配列
      allocate(ip(((nm+1)/2+nm+1)*2))         ! 変換用配列
      allocate(p(((nm+1)/2+nm+1)*jm))         ! 変換用配列
      allocate(r(((nm+1)/2*2+3)*(nm/2+1)))    ! 変換用配列
      allocate(ia((nm+1)*(nm+1)*4))           ! 変換用配列
      allocate(a((nm+1)*(nm+1)*6))            ! 変換用配列
      allocate(y(jm/2,4))                     ! 変換用配列

      if ( openmp ) then
         iw=(im+nm+1)*3*jm/2
      else
         iw=max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      endif

      allocate(x_Lon(0:im-1))                ! 格子点座標格納配列(経度)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(y_Lat(1:jm))
      allocate(y_Lat_Weight(1:jm))             ! 格子点座標格納配列
      allocate(xy_Lat(0:im-1,1:jm))            ! 格子点座標格納配列

      call sninit(nm,im,jm,it,t,y,ip,p,r,ia,a)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! 経度座標
         x_Lon_Weight(i) = 2*pi/im           ! 経度座標重み
      enddo


      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(y(j,1))        ! 緯度座標
         y_Lat(jm/2-j+1) = -asin(y(j,1))        ! 緯度座標
         y_Lat_Weight(jm/2+j)   = 2*y(j,2)      ! 緯度重み(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*y(j,2)      ! 緯度重み(Gauss grid)
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      call MessageNotify('M','w_base_initial',&
           'w_base_module (2009/07/30) is initialized')

    end subroutine w_base_Initial

  !--------------- 基本変換 -----------------

    function l_nm_array00(n,m)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 引数 n,m がともに整数値の場合, 整数値を返す. 
      !
      integer               :: l_nm_array00   
      !(out) スペクトルデータの格納位置 

      integer, intent(in)   :: n     !(in) 全波数
      integer, intent(in)   :: m     !(in) 帯状波数           

      call snnm2l(n,m,l_nm_array00)
    end function l_nm_array00

    function l_nm_array01(n,marray)           ! スペクトルデータの格納位置 
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 n が整数, 第 2 引数 marray が整数 1 次元配列の場合, 
      ! marray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: n               !(in) 全波数
      integer, intent(in)  :: marray(:)       !(in) 帯状波数
      integer              :: l_nm_array01(size(marray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(marray)
         l_nm_array01(i) = l_nm_array00(n,marray(i))
      enddo
    end function l_nm_array01

    function l_nm_array10(narray,m)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1 引数 narray が整数 1 次元配列, 第 2 引数  m が整数の場合, 
      ! narray と同じ大きさの 1 次元整数配列を返す. 
      !
      integer, intent(in)  :: narray(:)           !(in) 全波数  
      integer, intent(in)  :: m                   !(in) 帯状波数
      integer              :: l_nm_array10(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      do i=1, size(narray)
         l_nm_array10(i) = l_nm_array00(narray(i),m)
      enddo
    end function l_nm_array10

    function l_nm_array11(narray,marray)
      !
      ! 全波数(n)と東西波数(m)からそのスペクトルデータの格納位置を返す.
      ! 
      ! 第 1,2 引数 narray, marray がともに整数 1 次元配列の場合, 
      ! narray, marray と同じ大きさの 1 次元整数配列を返す. 
      ! narray, marray は同じ大きさでなければならない. 
      !
      integer, intent(in)  :: narray(:)          !(in) 全波数  
      integer, intent(in)  :: marray(:)          !(in) 帯状波数
      integer              :: l_nm_array11(size(narray))
      !(out) スペクトルデータ位置

      integer              :: i 

      if ( size(narray) .ne. size(marray) ) then
         call MessageNotify('E','l_nm_array11',&
              'dimensions of input arrays  n and m are different.')
      endif

      do i=1, size(narray)
         l_nm_array11(i) = l_nm_array00(narray(i),marray(i))
      enddo
    end function l_nm_array11

    function nm_l_int(l)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 l が整数値の場合, 対応する全波数と帯状波数を
      ! 長さ 2 の 1 次元整数値を返す. 
      ! nm_l(1) が全波数, nm_l(2) が帯状波数である. 
      !
      integer               :: nm_l_int(2)  !(out) 全波数, 帯状波数
      integer, intent(in)   :: l            !(in) スペクトルデータの格納位置
      
      call snl2nm(l,nm_l_int(1),nm_l_int(2))
    end function nm_l_int

    function nm_l_array(larray)
      ! 
      ! スペクトルデータの格納位置(l)から全波数(n)と東西波数(m)を返す.
      !
      ! 引数 larray が整数 1 次元配列の場合, 
      ! larray に対応する n, m を格納した 2 次元整数配列を返す. 
      ! nm_l_array(:,1) が全波数, nm_l_array(:,2) が帯状波数である. 
      !
      integer, intent(in)  :: larray(:)
      !(out) 全波数, 帯状波数

      integer              :: nm_l_array(size(larray),2)
      !(in) スペクトルデータの格納位置

      integer              :: i

      do i=1, size(larray)
         nm_l_array(i,:) = nm_l_int(larray(i))
      enddo
    end function nm_l_array

    function xy_w(w_data,ipow,iflag)
      !
      ! スペクトルデータから格子データへ変換する(1 層用).
      !
      real(8)               :: xy_w(0:im-1,1:jm)
      !(out) 格子点データ

      real(8), intent(in)   :: w_data((nm+1)*(nm+1))
      !(in) スペクトルデータ

      integer, intent(in), optional  :: ipow      
      !(in) 作用させる 1/cosφ の次数. 省略時は 0. 

      integer, intent(in), optional  :: iflag
      !(in) 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた逆変換
      !    1 : 緯度微分 cosφ・∂/∂φ を作用させた逆変換
      !    2 : sinφを作用させた逆変換
      !    省略時は 0.
      !
      real(8) :: xy_work(id,jd)                   ! w_xy,xy_w 変換用配列
      real(8) :: q(((nm+1)/2+nm+1)*jm)            ! 作業配列
      real(8) :: ws(iw),ww(iw)                    ! 作業用配列

      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval, i, j

      logical :: first=.true.                    ! 初回判定スイッチ
      save first

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','xy_w', &
                 'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntsog(nm,im,id,jm,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call snts2g(nm,im,id,jm,jd,1,w_data,xy_work,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      do i=0,im-1
        do j=1,jm
          xy_w(i,j) = xy_work(i+1,j)
        enddo
      enddo
      first = .false.

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! 格子データからスペクトルデータへ(正)変換する(1 層用).
      !
      real(8)               :: w_xy((nm+1)*(nm+1))
      !(out) スペクトルデータ

      real(8), intent(in)   :: xy_data(0:im-1,1:jm)
      !(in) 格子点データ

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた正変換 
      !    1 : 緯度微分 1/cosφ・∂(f cos^2φ)/∂φ を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.

      real(8) :: xy_work(id,jd)                   ! w_xy,xy_w 変換用配列
      real(8) :: q(((nm+1)/2+nm+1)*jm)            ! 作業配列
      real(8) :: ws(iw),ww(iw)                    ! 作業用配列

      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval, i, j

      logical :: first=.true.                     ! 初回判定スイッチ
      save first

      if (present(ipow)) then
         ipval = ipow
      else
         ipval = ipow_default
      endif

      if (present(iflag)) then
         ifval = iflag
      else
         ifval = iflag_default
      endif
      
      do i=0,im-1
        do j=1,jm
          xy_work(i+1,j)=xy_data(i,j)
        enddo
      enddo

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','w_xy', &
                 'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntgos(nm,im,id,jm,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call sntg2s(nm,im,id,jm,jd,1,xy_work,w_xy,&
              it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)
      endif
      first = .false.

    end function w_xy

  end module w_base_module
