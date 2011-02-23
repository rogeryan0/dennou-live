!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 SPMDOEL Development Group
!----------------------------------------------------------------------
!表題  wa_base_module
!
!  spml/wa_base_module モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module の下部モジュールであり, スペクトル計算の
!  基本的な Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_base_module モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!  このモジュールを使うためには前もって w_base_initial を呼んで
!  切断波数, 格子点数の設定をしておく必要がある. 
!
!
!履歴  2002/02/02  竹広真一  多層用に改造
!      2002/03/30  竹広真一  モジュール名変更
!      2002/05/25  竹広真一  格子点座標を区別すべく命名法変更
!      2005/01/09  竹広真一  msgdmp -> MessageNotify に変更
!      2005/07/09  竹広真一  OPENMP 版変換ルーチンに対応
!                            バンク競合を避けるための作業配列追加
!      2005/07/10  竹広真一  OpenMP セットアップのメッセージ出力
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2007/11/21  竹広真一  初期化サブルーチンメッセージ出力
!      2008/05/31  竹広真一  初期化サブルーチン分離
!      2008/06/22  佐々木洋平 水平方向の格子点データ配列の始点を
!                              1 から 0 に変更
!      2008/06/28  佐々木洋平 コメントを RDoc 用に微修正
!      2008/07/07  竹広真一  配列最終次元の不定性を許容する変更
!      2008/12/29  竹広真一  xya_wa, wa_xya コメント修正
!      2009/01/09  竹広真一  wa_base_Initial メッセージに日付を追加
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/30  竹広真一   作業領域をローカル変数に変更(for OpenMP)
!
!      * 変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module wa_base_module
  !
  != wa_base_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_base_module.f90,v 1.16 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wa_base_module モジュールは球面上での流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール wa_module の下部モジュールであり, スペクトル計算の
  ! 基本的な Fortran90 関数を提供する. 
  !
  ! 球面上の 1 層モデル用 w_base_module モジュールを多層モデル用に
  ! 拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  ! 対する変換が行える.
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  ! このモジュールを使うためには前もって w_base_initial を呼んで
  ! 切断波数, 格子点数の設定をしておく必要がある. 
  !
  use dc_message
  use w_base_module, only : im, jm, nm, it, t, y, ip, p, r, ia, a, openmp, np
  implicit none

  integer               :: km=16         ! 同時に処理する最大データ数(層の数)

  integer, allocatable  :: ipk(:,:)            ! 変換用配列(多層用)
  real(8), allocatable  :: pk(:,:), rk(:,:)    ! 変換用配列(多層用)

  integer               :: id=65, jd=33        ! xya_work の大きさ
  integer               :: iw                  ! ww, ws の大きさ

  real(8), parameter    :: pi=3.14159265358979

  private

  public km                                    ! 層数
  public wa_base_Initial                       ! 初期化サブルーチン
  public xya_wa, wa_xya                        ! 変換関数

  save km                                      ! 最大データ数(層数)を記憶
  save ipk, pk, rk                             ! 変換用配列を記憶
  save id, jd, iw                              ! 変換用配列の大きさ

  contains
  !--------------- 初期化 -----------------
    subroutine wa_base_initial(k_in)
      ! 
      ! スペクトル変換の最大データ数(層数)を設定する.
      !
      ! このサブルーチンを単独で用いるのでなく, 
      ! 上位サブルーチン wa_Initial を使用すること.
      !
      integer,intent(in) :: k_in               !(in) 最大データ数(層数)

      km = k_in

      allocate(ipk(km,((nm+1)/2+nm+1)*2))      ! 変換用配列(多層用)
      allocate(pk(km,((nm+1)/2+nm+1)*jm))      ! 変換用配列(多層用)
      allocate(rk(km,((nm+1)/2*2+3)*(nm/2+1))) ! 変換用配列(多層用)

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

      if ( openmp ) then
         iw=km*(im+nm+1)*3*jm/2
         call MessageNotify('M','wa_base_Initial', &
              'OpenMP computation was set up.')
      else
         iw=km * max((nm+4)*(nm+3),jd*3*(nm+1),jd*im)
      endif

      call snkini(nm,jm,km,ip,p,r,ipk,pk,rk)

      call MessageNotify('M','wa_base_initial',&
           'wa_base_module (2009/07/30) is initialized')

    end subroutine wa_base_Initial

  !--------------- 基本変換 -----------------

    function xya_wa(wa_data,ipow,iflag)    ! 球面調和関数スペクトル -> 格子点
      !
      ! スペクトルデータから格子データへ変換する(多層用).
      !
      real(8), intent(in)   :: wa_data(:,:)
      !(in) スペクトルデータ((nm+1)*(nm+1),:)
      !
      real(8)               :: xya_wa(0:im-1,1:jm,size(wa_data,2))
      !(out) 格子点データ(0:im-1,1:jm,:)
      !
      integer, intent(in), optional  :: ipow
      !(in) 作用させる 1/cosφ の次数. 省略時は 0. 
      integer, intent(in), optional  :: iflag
      !(in) 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた正変換
      !    1 : 緯度微分を作用させた正変換
      !    2 : sinφを作用させた正変換
      !    省略時は 0.
      !
      integer, parameter  :: ipow_default  = 0
      integer, parameter  :: iflag_default = 0
      integer ipval, ifval
      integer k, i

      real(8) :: xya_work(id,jd,km)              ! 変換用配列
      real(8) :: q(km*((nm+1)/2+nm+1)*jm)        ! 作業配列(多層用)
      real(8) :: ws(iw),ww(iw)                   ! 作業用配列(多層用)
      real(8) :: wv(km*(nm+4)*(nm+3)*np)         ! 作業用配列(OPENMP用)

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

      if  ( size(wa_data,1) /= (nm+1)**2 ) then
         call MessageNotify('E','xya_wa','Size of 1st dimension invalid.')
      end if

      k=size(wa_data,2)
      if  ( k > km ) then
         call MessageNotify('E','xya_wa','Size of 2nd dimension invalid.')
      else  if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','xya_wa', &
                 'OpenMP routine SNTSOG/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntsog(nm,im,id,jm,k,wa_data,xya_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),&
              ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call snts2g(nm,im,id,jm,jd,k,wa_data, xya_work,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif

      do i=0,im-1
        xya_wa(i,1:jm,1:k) = xya_work(i+1,1:jm,1:k)
      enddo
      first = .false.

    end function xya_wa

    function wa_xya(xya_data,ipow,iflag) ! 格子点 -> 球面調和関数スペクトル
      !
      ! 格子データからスペクトルデータへ(正)変換する(多層用).
      !
      real(8), intent(in)   :: xya_data(0:,:,:)
      !(in) 格子点データ(0:im-1,1:jm,:)

      real(8)               :: wa_xya((nm+1)*(nm+1),size(xya_data,3))
      !(out) スペクトルデータ((nm+1)*(nm+1),:)

      integer, intent(in), optional  :: ipow
      !(in) 変換時に同時に作用させる 1/cosφ の次数. 省略時は 0.

      integer, intent(in), optional  :: iflag
      ! 変換の種類
      !    0 : 通常の正変換
      !   -1 : 経度微分を作用させた正変換
      !    1 : 緯度微分を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.

      integer, parameter  :: ipow_default  = 0      ! スイッチデフォルト値
      integer, parameter  :: iflag_default = 0      ! スイッチデフォルト値

      integer ipval, ifval
      integer i,k

      real(8) :: xya_work(id,jd,km)               ! 変換用配列
      real(8) :: q(km*((nm+1)/2+nm+1)*jm)         ! 作業配列(多層用)
      real(8) :: ws(iw),ww(iw)                    ! 作業用配列(多層用)
      real(8) :: wv(km*(nm+4)*(nm+3)*np)          ! 作業用配列(OPENMP用)

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

      if ( size(xya_data,1) /= im ) then
         call MessageNotify('E','wa_xya','Size of 1st dimension invalid.')
      endif

      if ( size(xya_data,2) /= jm ) then
         call MessageNotify('E','wa_xya','Size of 2nd dimension invalid.')
      endif

      k = size(xya_data,3)
      if ( k > km ) then
         call MessageNotify('E','wa_xya','Size of 3rd dimension invalid.')
      endif

      do i=0,im-1
        xya_work(i+1,1:jm,1:k) = xya_data(i,1:jm,1:k)
      enddo

      if ( openmp ) then
         if ( first ) then
            call MessageNotify('M','wa_xya', &
                 'OpenMP routine SNTGOS/SNPACK is used for spherical harmonic transformation.')
         endif
         call sntgos(nm,im,id,jm,k,xya_work,wa_xya,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),&
              ia,a,q,ws,ww,wv,ipval,ifval)
      else
         call sntg2s(nm,im,id,jm,jd,k,xya_work,wa_xya,&
              it,t,y,ipk(1:k,:),pk(1:k,:),rk(1:k,:),ia,a,q,ws,ww,ipval,ifval)
      endif
      first = .false.

    end function wa_xya

  end module wa_base_module
