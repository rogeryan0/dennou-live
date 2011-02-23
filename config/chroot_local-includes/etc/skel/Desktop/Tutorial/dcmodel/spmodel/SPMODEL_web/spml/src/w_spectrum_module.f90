!--
!----------------------------------------------------------------------
! Copyright(C) 2005-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_spectrum_module
!
!  spml/w_spectrum_module モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module の下部モジュールであり, 
!  スペクトル解析計算のための Fortran90 関数を提供する. 
!
!  内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
!
!
!履歴  2005/04/23  竹広真一 
!      2005/05/16  竹広真一 関数名を短縮
!      2006/03/08  竹広真一  コメントを RDoc 用に修正
!      2008/06/25  佐々木洋平 コメントを RDoc 用に修正
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/30  竹広真一   DO 変数をローカルに変更(for OpenMP)
!
!++
module w_spectrum_module
  !
  !=  w_spectrum_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_spectrum_module.f90,v 1.8 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_spectrum_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module の下部モジュールであり, 
  ! スペクトル解析計算のための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use w_base_module, only : nm, l_nm

  implicit none

  private
 
  public nm_EnergyFromStreamfunc_w          ! エネルギースペクトル           
                                            ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnergyFromStreamfunc_w           ! エネルギースペクトル
                                            ! (水平全波数 n 空間) 
  public nm_EnstrophyFromStreamfunc_w       ! エンストロフィースペクトル     
                                            ! (水平全波数 n, 帯状波数 m 空間)
  public n_EnstrophyFromStreamfunc_w        ! エンストロフィースペクトル  
                                            !  (水平全波数 n 空間)
  public w_spectrum_VMiss                   ! 欠損値

  real(8) :: w_spectrum_VMiss = -999.000    ! 欠損値初期値

  contains

  !--------------- エネルギースペクトル計算 -----------------
    function nm_EnergyFromStreamfunc_w(w_Strfunc)
      ! 
      ! 流線関数のスペクトルデータからエネルギーの球面調和函数成分
      ! (スペクトル)を計算する(1 層用).
      !
      !  * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルは (1/2)n(n+1)ψ(n,m)^2 と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に4πをかけたものが球面上での
      !    全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 w_spectrum_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnergyFromStreamfunc_w
      !(out) エネルギースペクトル(水平全波数 n, 帯状波数 m 空間)

      integer n,m                               ! DO 変数

      nm_EnergyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=-n,n
            nm_EnergyFromStreamfunc_w(n,m) &
                 = 0.5 * n*(n+1) * w_Strfunc(l_nm(n,m))**2
         enddo
      enddo
    end function nm_EnergyFromStreamfunc_w

    function n_EnergyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用).
      !
      !  * 全波数 n の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルはΣ[m=-nm]^nm(1/2)n(n+1)ψ(n,m)^2 
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に 4πをかけたものが
      !    球面上での全エネルギーに等しい.
      !

      real(8), intent(in)      :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm) :: n_EnergyFromStreamfunc_w
      !(out) エネルギースペクトル (水平全波数 n 空間) 

      integer n,m                                 ! DO 変数

      do n=0,nm
         n_EnergyFromStreamfunc_w(n)  &
              = 0.5 * n*(n+1) &
                * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2,1)
      enddo

    end function n_EnergyFromStreamfunc_w

  !--------------- エンストロフィースペクトル計算 -----------------
    function nm_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータからエンストロフィーの球面調和函数成分
      ! (スペクトル)を計算する(1 層用). 
      !
      ! * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エンストロフィースペクトルは (1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !
      ! * 全てのエンストロフィースペクトル成分の和に4π/R^2をかけたものが
      !   球面上での全エンストロフィーに等しい. ここで R は球面の半径である.
      !
      ! * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !   欠損値の値はモジュール変数 w_spectrum_VMiss によって設定できる
      !   (初期値は -999.0)
      !
      real(8), intent(in)   :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm) :: nm_EnstrophyFromStreamfunc_w
      ! エンストロフィースペクトル (水平全波数 n, 帯状波数 m 空間)

      integer n,m                               ! DO 変数

      nm_EnstrophyFromStreamfunc_w = w_spectrum_VMiss

      do n=0,nm
         do m=-n,n
            nm_EnstrophyFromStreamfunc_w(n,m) &
                 = 0.5 * n**2 * (n+1)**2 &
                    * w_Strfunc(l_nm(n,m))**2
         enddo
      enddo
    end function nm_EnstrophyFromStreamfunc_w

    function n_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用)
      !
      ! * 全波数 n の流線関数のスペクトル成分ψ(n,m) からエンストロフィー
      !   スペクトルはΣ[m=-nm]^nm(1/2)n^2(n+1)^2ψ(n,m)^2 と計算される.
      !    
      ! * 全てのエネルギースペクトル成分の和に 4π/R^2 をかけたものが
      !   球面上での全エンストフィーに等しい.
      !
      real(8), intent(in)      :: w_Strfunc(:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm) :: n_EnstrophyFromStreamfunc_w  
      !(out) エンストロフィースペクトル(水平全波数 n 空間)

      integer n,m                                ! DO 変数

      do n=0,nm
         n_EnstrophyFromStreamfunc_w(n)  &
              = 0.5 * n**2 * (n+1)**2 &
                * sum(w_StrFunc(l_nm(n,(/(m,m=-n,n)/)))**2,1)
      enddo
    end function n_EnstrophyFromStreamfunc_w

end module w_spectrum_module
