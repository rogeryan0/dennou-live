!----------------------------------------------------------------------
!     Copyright (c) 2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_base_mpi_module
!
!  spml/wa_base_mpi_module モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法と MPI 並列化によって
!  数値計算するための モジュール wa_mpi_module の下部モジュールであり, 
!  スペクトル計算の基本的な Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_base_mpi_module モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2008/05/26  竹広真一  wa_base_module.f90 を mpi 化
!
module wa_base_mpi_module
  !
  ! wa_base_mpi_module
  !
  !  spml/wa_base_mpi_module モジュールは球面上での流体運動を
  !  球面調和函数を用いたスペクトル法と MPI 並列化によって
  !  数値計算するための モジュール wa_mpi_module の下部モジュールであり, 
  !  スペクトル計算の基本的な Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_base_mpi_module モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  !
  use dc_message
  use w_base_module, only : im, jm, nm
  use w_base_mpi_module, only : id, jd, jc, &
                                it, t, y, ip, p, r, ia, a
  use wa_base_module, only : km
  implicit none

  integer, allocatable  :: ipk(:,:)            ! 変換用配列(多層用)
  real(8), allocatable  :: pk(:,:), rk(:,:)    ! 変換用配列(多層用)

  real(8), allocatable  :: q(:)                ! 作業配列
  real(8), allocatable  :: ww(:), ws(:)        ! 作業配列
  real(8), allocatable  :: w(:)                ! 作業配列

  real(8), allocatable  :: xva_work(:,:,:)     ! wa_xva,xva_wa 変換用配列

  real(8), parameter    :: pi=3.14159265358979

  private
  private im, jm, nm                           ! Intel Fortran 対策

  public wa_base_mpi_Initial                   ! 初期化サブルーチン
  public xva_wa, wa_xva                        ! 変換関数

  save ipk, pk, rk                             ! 変換用配列を記憶

  contains
  !--------------- 初期化 -----------------
    subroutine wa_base_mpi_initial
      ! 
      ! スペクトル変換の最大データ数(層数)を設定する.
      !
      ! このサブルーチンを単独で用いるのでなく, 
      ! 上位サブルーチン wa_Initial を使用すること.
      !
      integer :: iw

      allocate(ipk(km,((nm+1)/2+nm+1)*2))      ! 変換用配列(多層用)
      allocate(pk(km,((nm+1)/2+nm+1)*jm))      ! 変換用配列(多層用)
      allocate(rk(km,((nm+1)/2*2+3)*(nm/2+1))) ! 変換用配列(多層用)

      allocate(q(km*((nm+1)/2+nm+1)*jm))       ! 作業配列(多層用)

      allocate(xva_work(id,jd,km))                ! 変換用配列

      iw=km * max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)

      allocate(ws(iw),ww(iw),w((nm+1)*(nm+1)*km))    ! 作業用配列(多層用)

      call snkini(nm,jm,km,ip,p,r,ipk,pk,rk)

      call MessageNotify('M','wa_base_mpi_initial',&
                         'wa_base_mpi_module is initialized')

    end subroutine wa_base_mpi_initial

  !--------------- 基本変換 -----------------

    function xva_wa(wa_data,ipow,iflag)    ! 球面調和関数スペクトル -> 格子点
      !
      ! スペクトルデータから格子データへ変換する(多層用).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) スペクトルデータ

      real(8)               :: xva_wa(im,jc,size(wa_data,2))
      !(out) 格子点データ

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
      integer k

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

      k= size(wa_data,2)
      if  ( k > km ) then
         call MessageNotify('E','xva_wa','Size of 3rd dimension invalid.')
      else
         call snts2g(nm,im,id,jc,jd,k,wa_data, xva_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif
      xva_wa=xva_work(1:im,1:jc,1:k)

    end function xva_wa

    function wa_xva(xva_data,ipow,iflag) ! 格子点 -> 球面調和関数スペクトル
      !
      ! 格子データからスペクトルデータへ(正)変換する(多層用).
      !
      real(8), intent(in)   :: xva_data(:,:,:)
      !(in) 格子点データ(im,jm,*)

      real(8)               :: wa_xva((nm+1)*(nm+1),size(xva_data,3))
      !(out) スペクトルデータ

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !     0 : 通常の正変換
      !     1 : 経度微分を作用させた正変換
      !    -1 : 緯度微分を作用させた正変換
      !     2 : sinφを作用させた正変換
      !   省略時は 0.

      integer, parameter  :: ipow_default  = 0      ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0      ! スイッチデフォルト値

      integer ipval, ifval
      integer k

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

      k = size(xva_data,3)
      if ( k > km ) then
         call MessageNotify('E','wa_xva','Size of 3rd dimension invalid.')
      endif

      xva_work = 0.0
      xva_work(1:im,1:jc,1:k) = xva_data

      call sntgms(nm,im,id,jc,jd,k,xva_work,wa_xva,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval,w)

    end function wa_xva

end module wa_base_mpi_module
