!--
!----------------------------------------------------------------------
!     Copyright (c) 2002-2009 SPMDOEL Development Group
!----------------------------------------------------------------------
!表題  wa_base_module
!
!  spml/wa_base_module_sjpack モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module_sjpack の下部モジュールであり, スペクトル計算の
!  基本的な Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_base_module_sjpack モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!  このモジュールを使うためには前もって w_base_initial を呼んで
!  切断波数, 格子点数の設定をしておく必要がある. 
!
!
!履歴  2009/09/05  竹広真一  wa_base_module より SJPACK 対応改造
!
!      * 変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module wa_base_module_sjpack
  !
  != wa_base_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_base_module_sjpack.f90,v 1.3 2009-09-17 16:05:24 uwabami Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wa_base_module_sjpack モジュールは球面上での流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール wa_module_sjpack の下部モジュールであり, スペクトル計算の
  ! 基本的な Fortran90 関数を提供する. 
  !
  ! 球面上の 1 層モデル用 w_base_module_sjpack モジュールを多層モデル用に
  ! 拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  ! 対する変換が行える.
  !
  ! 内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  ! このモジュールを使うためには前もって w_base_initial を呼んで
  ! 切断波数, 格子点数の設定をしておく必要がある. 
  !
  use dc_message
  use w_base_module_sjpack, only : im, jm, nm=>nn, xy_w, w_xy
  implicit none

  integer               :: km=16         ! 同時に処理する最大データ数(層の数).
                                         ! SNPACK 用ルーチンとの互換性のため.
                                         ! wa_base_module_sjpack では
                                         ! このパラメタによる制限がない.

  private

  public km                                    ! 層数
  public wa_base_Initial                       ! 初期化サブルーチン
  public xya_wa, wa_xya                        ! 変換関数

  save km                                      ! 最大データ数(層数)を記憶

  contains
  !--------------- 初期化 -----------------
    subroutine wa_base_initial(k_in)
      ! 
      ! SNPACK 用 wa_base_initial の互換のためのダミーサブルーチン
      !
      integer,intent(in) :: k_in               !(in) 最大データ数(層数)

      km = k_in

      call MessageNotify('M','wa_base_initial',&
        'No need to set maximum level number and in wa_base_module_sjpack (2009/09/05) ')
            
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
      !   -1 : 経度微分を作用させた逆変換
      !    1 : 緯度微分 cosφ・∂/∂φ を作用させた逆変換
      !    2 : sinφを作用させた逆変換
      !    省略時は 0.
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

      do k=1,size(wa_data,2)
        xya_wa(:,:,k) = xy_w(wa_data(:,k),iflag=ifval,ipow=ipval)
      enddo

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
      !    1 : 緯度微分 1/cosφ・∂(f cos^2φ)/∂φ を作用させた正変換
      !    2 : sinφを作用させた正変換
      !  省略時は 0.
      !
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

      do k=1,size(xya_data,3)
         wa_xya(:,k) = w_xy(xya_data(:,:,k),iflag=ifval,ipow=ipval)
      enddo

    end function wa_xya

end module wa_base_module_sjpack
