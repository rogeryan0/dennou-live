!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  w_base_module_sjpack
!
!  spml/w_base_module_sjpack モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するためのモジュール 
!  w_module_sjpack の下部モジュールであり, スペクトル計算の基本的な 
!  Fortran90 関数を提供する.
!
!  内部で ISPACK の LJPACK(SJPACK) の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算
!  法については ISPACK/SJPACK のマニュアルを参照されたい.
!
!== 履歴
!
!      2009/09/03  竹広真一  w_base_module より改造
!      2009/09/20  竹広真一  w_base_initialize 変数導入
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!         ・波数切断の仕方は三角波数切断に決めうち. 
!
!++
module w_base_module_sjpack
  !
  != w_base_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_base_module_sjpack.f90,v 1.3 2009-09-23 06:35:59 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要.
  !
  ! spml/w_base_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための
  ! モジュール w_module_sjpack の下部モジュールであり, スペクトル法の
  ! 基本的な Fortran90 関数を提供する.
  !
  ! 内部で ISPACK の SJPACK Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の
  ! 詳しい計算法については ISPACK/SJPACK のマニュアルを
  ! 参照されたい.
  !
  use dc_message
  implicit none

  integer               :: im=64            ! 格子点の設定(東西)
  integer               :: jm=32            ! 格子点の設定(南北)
  integer               :: nm=21            ! 計算する最大の全波数の設定
  integer               :: nn=22            ! 切断波数(全波数)の設定
  integer               :: mm=21            ! 切断波数(東西波数)の設定
  integer               :: np=1             ! OPENMP 最大スレッド数

  logical               :: openmp=.false.   ! OPENMP スイッチ

  real(8), allocatable  :: p(:,:), r(:)     ! 変換用配列
  integer               :: it(4)            ! 変換用配列
  real(8), allocatable  :: t(:)             ! 変換用配列

  real(8), allocatable  :: c(:)             ! 作業配列

  real(8), allocatable  :: x_Lon(:), y_Lat(:)                ! 緯度経度
  real(8), allocatable  :: x_Lon_Weight(:), y_Lat_Weight(:)  ! 座標重み
  real(8), allocatable  :: xy_Lon(:,:), xy_Lat(:,:)

  logical               :: w_base_initialize=.false.   ! 初期化フラッグ

  real(8), parameter    :: pi=3.1415926535897932385D0

  private

  public im, jm, nn, mm, nm                   ! 格子点数, 切断波数
  public it, t, p, r                          ! 変換用作業配列
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

  save im, jm, nm, mm, nn                     ! 格子点数, 切断波数を記憶
  save it, t, p, r                            ! 変換用配列を記憶
  save c                                      ! 変換用配列の大きさ
  save openmp, np                             ! 変換用配列の大きさ
  save w_base_initialize                      ! 初期化フラグ

  contains
  !--------------- 初期化 -----------------
    subroutine w_base_Initial(n_in,i_in,j_in,np_in)
      !
      ! スペクトル変換の格子点数, 波数および OPENMP 使用時の
      ! 最大スレッド数を設定する.
      !
      ! 実際の使用には上位サブルーチン w_Initial を用いること.
      !
      integer,intent(in) :: i_in              !(in) 格子点数(東西), 2の巾乗(<=2048)
      integer,intent(in) :: j_in              !(in) 格子点数(南北), 4 の倍数
      integer,intent(in) :: n_in              !(in) 切断全波数
      integer,intent(in), optional :: np_in   !(in) OPENMP での最大スレッド数

      integer :: iw, i, j

      w_base_initialize = .true.

      im = i_in   ; jm = j_in
      nn = n_in   ; nm = n_in+1 ;  mm = n_in      ! default は三角波数切断

      if ( present(np_in) )then
         np = np_in

         if ( np .gt. 1 ) then
            openmp = .true. 
            call MessageNotify('M','w_base_Initial', &
                 'OpenMP computation was set up.')
         else
            openmp = .false. 
         endif

      else
         openmp = .false. 
         np = 1
      endif

      allocate(p(jm/2,mm+4))                  ! 変換用配列
      allocate(r((mm+1)*(2*nm-mm-1)+1))       ! 変換用配列
      allocate(t(im*6))                       ! 変換用配列

      allocate(c((mm+1)*(mm+1)))              ! 変換用作業配列

      allocate(x_Lon(0:im-1))                 ! 格子点座標格納配列(経度)
      allocate(x_Lon_Weight(0:im-1))
      allocate(xy_Lon(0:im-1,1:jm))
      allocate(y_Lat(1:jm))
      allocate(y_Lat_Weight(1:jm))             ! 格子点座標格納配列
      allocate(xy_Lat(0:im-1,1:jm))        ! 格子点座標格納配列

      call sjinit(mm,nm,jm,im,p,r,it,t)

      call sjinic(mm,c)

      do i=0,im-1
         x_Lon(i)  = 2*pi/im*i               ! 経度座標
         x_Lon_Weight(i) = 2*pi/im           ! 経度座標重み
      enddo


      do j=1,jm/2
         y_Lat(jm/2+j)   =  asin(p(j,1))        ! 緯度座標
         y_Lat(jm/2-j+1) = -asin(p(j,1))        ! 緯度座標
         y_Lat_Weight(jm/2+j)   = 2*p(j,2)      ! 緯度重み(Gauss grid)
         y_Lat_Weight(jm/2-j+1) = 2*p(j,2)      ! 緯度重み(Gauss grid)
      enddo

      do j=1,jm
         xy_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xy_Lat(i,:) = y_Lat
      enddo

      call MessageNotify('M','w_base_initial',&
           'w_base_module_sjpack (2009/09/04) is initialized')

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

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','l_nm_array00',&
              'w_base_module not initialize yet. Use sjnm2l routine in ISPACK directly.')
      else
         call sjnm2l(nn,n,m,l_nm_array00)
      endif

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
      
      if ( .not. w_base_initialize ) then
         call MessageNotify('E','nm_l_int',&
              'w_base_module not initialize yet. Use sjl2nm routine in ISPACK directly.')
      else
         call sjl2nm(nn,l,nm_l_int(1),nm_l_int(2))
      endif

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

      real(8), intent(in)   :: w_data((mm+1)*(mm+1))
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
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval

      real(8)             :: w_Rdata((2*nn+1-mm)*mm+nn+1)
      ! 作業用スペクトルデータ(SJTS2G 出力用)
      real(8)             :: w_Xdata((mm+1)*(mm+1))
      ! 作業用スペクトルデータ(SJCS2X 出力用)
      real(8)             :: w_Ydata((mm+4)*mm+2)
      ! 作業用スペクトルデータ(SJCS2Y 出力用)

      real(8)  :: q(jm/2*7*np)               ! 変換用作業配列
      real(8)  :: ws(2*(nn+1)*np)            ! 変換用作業配列
      real(8)  :: wg((im+2)*jm)              ! 変換用作業配列
      real(8)  :: w((jm+1)*im)               ! 変換用作業配列

      logical :: first=.true.                    ! 初回判定スイッチ
      save first

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','xy_w',&
              'w_base_module not initialize yet.')
      endif

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

      if ( openmp .and. first ) then
         call MessageNotify('M','xy_w', &
              'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
      endif

      if ( ifval==0 ) then
         call sjcrup(mm,nn,w_data,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==-1 ) then
         call sjcs2x(mm,w_data,w_Xdata)
         call sjcrup(mm,nn,w_Xdata,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==1 ) then
         call sjcs2y(mm,w_data,w_Ydata,c)
         if ( openmp ) then
            call sjtsog(mm,nm,nm,im,jm,w_Ydata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nm,im,jm,w_Ydata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
      else if( ifval==2 ) then
         call sjcrup(mm,nn,w_data,w_Rdata)
         if ( openmp ) then
            call sjtsog(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjts2g(mm,nm,nn,im,jm,w_Rdata,xy_w,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         xy_w = xy_w * sin(xy_Lat)
      else
         call MessageNotify('E','xy_w','invalid value of iflag')
      endif

      first = .false.

    end function xy_w

    function w_xy(xy_data,ipow,iflag)
      !
      ! 格子データからスペクトルデータへ(正)変換する(1 層用).
      !
      real(8)               :: w_xy((mm+1)*(mm+1))
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


      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval, i, j

      real(8)             :: w_Rdata((2*nn+1-mm)*mm+nn+1)
      ! 作業用スペクトルデータ(SJTS2G 出力用)
      real(8)             :: w_Xdata((mm+1)*(mm+1))
      ! 作業用スペクトルデータ(SJCS2X 出力用)
      real(8)             :: w_Ydata((mm+4)*nm+2)
      ! 作業用スペクトルデータ(SJCY2S 出力用)

      real(8)  :: q(jm/2*7*np)               ! 変換用作業配列
      real(8)  :: ws(2*(nn+1)*np)            ! 変換用作業配列
      real(8)  :: wg((im+2)*jm)              ! 変換用作業配列
      real(8)  :: w((jm+1)*im)               ! 変換用作業配列

      logical :: first=.true.                     ! 初回判定スイッチ
      save first

      if ( .not. w_base_initialize ) then
         call MessageNotify('E','xy_w',&
              'w_base_module not initialize yet.')
      endif

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
      
      if ( openmp .and. first ) then
         call MessageNotify('M','w_xy', &
              'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
      endif

      if ( ifval == 0 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_xy)
      else if ( ifval == -1 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_Xdata)
         call sjcs2x(mm,w_Xdata,w_xy)
      else if ( ifval == 1 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nm,im,jm,w_Ydata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nm,im,jm,w_Ydata,xy_data,&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcy2s(mm,w_Ydata,w_xy,c)
      else if ( ifval == 2 ) then
         if ( openmp ) then
            call sjtgos(mm,nm,nn,im,jm,w_Rdata,xy_data*sin(xy_Lat),&
                        it,t,p,q,r,ws,wg,w,ipval)
         else
            call sjtg2s(mm,nm,nn,im,jm,w_Rdata,xy_data*sin(xy_Lat),&
                        it,t,p,q,r,ws,wg,w,ipval)
         endif
         call sjcrdn(mm,nn,w_Rdata,w_xy)
      end if

      first = .false.

    end function w_xy

  end module w_base_module_sjpack
