!----------------------------------------------------------------------
! Copyright (c) 2008-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_base_module
!
!  spml/w_base_module モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module の下部モジュールであり, スペクトル計算の
!  基本的な Fortran90 関数を提供する. 
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2008/05/26  竹広真一  w_base_module を MPI 化
!      2010/01/07  佐々木洋平  RDoc 用のドキュメント修正, 
!
module w_base_mpi_module
  !
  ! w_base_mpi_module
  !
  !  spml/w_base_module モジュールは球面上での 2 次元流体運動を
  !  球面調和函数を用いたスペクトル法と MPI によって数値計算するための 
  !  モジュール w_mpi_module の下部モジュールであり, スペクトル法の
  !  基本的なな Fortran90 関数を提供する. 
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use dc_message
  use w_base_module, only : im, jm, nm, x_Lon

  implicit none

  integer               :: it(6)            ! 変換用配列(分散格子点用)
  real(8), allocatable  :: t(:)             ! 変換用配列(分散格子点用)
  integer, allocatable  :: ip(:)            ! 変換用配列(分散格子点用)
  real(8), allocatable  :: p(:), r(:)       ! 変換用配列(分散格子点用)
  integer, allocatable  :: ia(:)            ! 変換用配列(分散格子点用)
  real(8), allocatable  :: a(:)             ! 変換用配列(分散格子点用)
  real(8), allocatable  :: y(:)             ! 変換用配列(分散格子点用)

  integer               :: jc               ! 分散配置用変数
  real(8), allocatable  :: yy(:,:)          ! 変換用配列
  
  real(8), allocatable  :: q(:)             ! 作業配列
  real(8), allocatable  :: ww(:), ws(:)     ! 作業配列
  real(8), allocatable  :: w(:)             ! 作業配列

  real(8), allocatable  :: v_Lat(:),v_Lat_Weight(:)      ! 緯度経度

  real(8), allocatable  :: xv_Lon(:,:), xv_Lat(:,:)

  real(8), allocatable  :: xv_work(:,:)     ! w_xv,xv_w 変換用配列

  integer               :: id=65, jd=33     ! xv_work の大きさ

  real(8), parameter    :: pi=3.1415926535897932385D0

  private
  private im, jm, nm                          ! Intel Fortran 対策

  public it, t, y, ip, p, r, ia, a            ! 変換用作業配列
  public id, jd                               ! 作業用配列の大きさ
  public jc                                   ! 分散配置情報

  public w_base_mpi_Initial                   ! 初期化サブルーチン
  public v_Lat, v_Lat_Weight                  ! 経度分散格子座標・重み
  public xv_Lon, xv_Lat                       ! 分散格子座標(im,jc)
  public xv_w, w_xv                           ! 変換関数

  save it, t, y, ip, p, r, ia, a              ! 変換用配列を記憶
  save jc                                     ! 分散格子点配列の大きさ
  save id, jd                                 ! 変換用配列の大きさ

  contains
  !--------------- 初期化 -----------------
    subroutine w_base_mpi_Initial
      !
      ! スペクトル変換の格子点数, 波数を設定する.
      !
      ! 実際の使用には上位サブルーチン w_mpi_Initial を用いること.
      !
      integer :: iw, i, j

      allocate(t(im*2))                       ! 変換用配列(分散配置)
      allocate(ip(((nm+1)/2+nm+1)*2))         ! 変換用配列(分散配置)
      allocate(p(((nm+1)/2+nm+1)*jm))         ! 変換用配列(分散配置)
      allocate(r(((nm+1)/2*2+3)*(nm/2+1)))    ! 変換用配列(分散配置)
      allocate(ia((nm+1)*(nm+1)*4))           ! 変換用配列(分散配置)
      allocate(a((nm+1)*(nm+1)*6))            ! 変換用配列(分散配置)
      allocate(y(jm*2))                       ! 変換用配列(分散配置)

      ! 注意 : 別ルーチンによって w_base_Initial が呼んであることを仮定
      call snmini(nm,im,jm,jc,it,t,y,ip,p,r,ia,a)

      if ( im/2*2 .eq. im ) then
         id = im+1 
      else
         id = im
      endif
      if ( jc/2*2 .eq. jc ) then
         jd = jc+1
      else
         jd = jc
      endif
      allocate(xv_work(id,jd))                ! 変換用配列

      allocate(q(((nm+1)/2+nm+1)*jm))         ! 作業配列
 
      iw=max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      allocate(ws(iw),ww(iw), w((nm+1)*(nm+1)))    ! 作業用配列
      allocate(yy(jc/2,4))                         ! 変換用配列

      allocate(v_Lat(jc),v_Lat_Weight(jc))             ! 格子点座標格納配列

      allocate(xv_Lon(0:im-1,jc),xv_Lat(0:im-1,jc))   ! 格子点座標格納配列

      yy = reshape(y(1:2*jc),(/jc/2,4/))

      do j=1,jc/2
         v_Lat(jc/2+j)   =  asin(yy(j,1))        ! 緯度座標
         v_Lat(jc/2-j+1) = -asin(yy(j,1))        ! 緯度座標
         v_Lat_Weight(jc/2+j)   = 2*yy(j,2)      ! 緯度重み(Gauss grid)
         v_Lat_Weight(jc/2-j+1) = 2*yy(j,2)      ! 緯度重み(Gauss grid)
      enddo
  
      do j=1,jc
         xv_Lon(:,j) = x_Lon
      enddo

      do i=0,im-1
         xv_Lat(i,:) = v_Lat
      enddo

      call MessageNotify('M','w_base_mpi_initial',&
                         'w_base_mpi_module is initialized')
    end subroutine w_base_mpi_Initial

  !--------------- 基本変換(格子点分散配置) -----------------

    function xv_w(w_data,ipow,iflag)
      !
      ! スペクトルデータから分散格子データへ変換する(1 層用).
      !
      real(8)               :: xv_w(0:im-1,jc)
      !(out) 格子点データ

      real(8), intent(in)   :: w_data((nm+1)*(nm+1))
      !(in) スペクトルデータ

      integer, intent(in), optional  :: ipow      
      !(in) 作用させる 1/cosφ の次数. 省略時は 0. 

      integer, intent(in), optional  :: iflag
      !(in) 変換の種類
      !     0 : 通常の正変換
      !     1 : 経度微分を作用させた正変換
      !    -1 : 緯度微分を作用させた正変換
      !     2 : sinφを作用させた正変換
      !     省略時は 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0

      integer ipval, ifval

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

      call snts2g(nm,im,id,jc,jd,1,w_data,xv_work,&
                  it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval)

      xv_w=xv_work(1:im,1:jc)

    end function xv_w

    function w_xv(xv_data,ipow,iflag)
      !
      ! 格子データからスペクトルデータへ(正)変換する(1 層用).
      !
      real(8)               :: w_xv((nm+1)*(nm+1))
      !(out) スペクトルデータ

      real(8), intent(in)   :: xv_data(0:im-1,jc)
      !(in) 格子点データ

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !     0 : 通常の正変換
      !     1 : 経度微分を作用させた正変換
      !    -1 : 緯度微分を作用させた正変換
      !     2 : sinφを作用させた正変換
      !   省略時は 0.


      integer, parameter  :: ipow_default  = 0    ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0    ! スイッチデフォルト値

      integer ipval, ifval

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

      xv_work(1:im,1:jc)=xv_data

      call sntgms(nm,im,id,jc,jd,1,xv_work,w_xv,&
                 it,t,y,ip,p,r,ia,a,q,ws,ww,ipval,ifval,w)

    end function w_xv

end module w_base_mpi_module
