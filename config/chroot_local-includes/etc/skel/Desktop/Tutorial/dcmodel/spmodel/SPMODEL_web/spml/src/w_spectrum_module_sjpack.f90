!--
!----------------------------------------------------------------------
! Copyright(C) 2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!表題  w_spectrum_module_sjpack
!
!  spml/w_spectrum_module_sjpack モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module_sjpack の下部モジュールであり, 
!  スペクトル解析計算のための Fortran90 関数を提供する. 
!
!  スペクトル計算の方法については doc/w_module_sjpack.tex を参照のこと. 
!  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!
!履歴  2009/09/04  竹広真一 w_spectrum_module を改造, SJPACK 対応
!      2009/09/23  竹広真一 nm_* の計算修正
!
!++
module w_spectrum_module_sjpack
  !
  !=  w_spectrum_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_spectrum_module_sjpack.f90,v 1.2 2009-09-23 06:35:59 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_spectrum_module_sjpack モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module_sjpack の下部モジュールであり, 
  ! スペクトル解析計算のための Fortran90 関数を提供する. 
  !
  ! スペクトル計算の方法については doc/w_module.tex を参照のこと. 
  ! 内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  use w_base_module_sjpack, only : nm=>nn, l_nm

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
      !    エネルギースペクトルは (1/2)n(n+1)|ψ(n,m)|^2 と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に4πをかけたものが球面上での
      !    全エネルギーに等しい.
      !
      !  * スペクトルの (n,m) 成分は全波数 n, 東西波数 m の実部成分, 
      !    (n,-m) 成分は全波数 n, 東西波数 m の虚部成分が格納されている.
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
         nm_EnergyFromStreamfunc_w(n,0) &
              = 0.5 * n*(n+1) * w_Strfunc(l_nm(n,0))**2
         do m=1,n
            nm_EnergyFromStreamfunc_w(n,m) &
                 = 0.5 * n*(n+1) &
                 * (w_Strfunc(l_nm(n,m))**2+w_Strfunc(l_nm(n,-m))**2)
            nm_EnergyFromStreamfunc_w(n,-m) &
                 = nm_EnergyFromStreamfunc_w(n,m)
         enddo
      enddo
    end function nm_EnergyFromStreamfunc_w

    function n_EnergyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用).
      !
      !  * 全波数 n の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルはΣ[m=-nm]^nm(1/2)n(n+1)|ψ(n,m)|^2 
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

      n_EnergyFromStreamfunc_w = 0.0D0
      do n=0,nm
         n_EnergyFromStreamfunc_w(n) = &
                0.5 * n*(n+1) * w_StrFunc(l_nm(n,0))**2
         do m=1,n
            n_EnergyFromStreamfunc_w(n) = n_EnergyFromStreamfunc_w(n)+ &
                2 * 0.5 * n*(n+1) &
                * (w_StrFunc(l_nm(n,m))**2+w_StrFunc(l_nm(n,-m))**2)
         enddo
      enddo

    end function n_EnergyFromStreamfunc_w

  !--------------- エンストロフィースペクトル計算 -----------------
    function nm_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータからエンストロフィーの球面調和函数成分
      ! (スペクトル)を計算する(1 層用). 
      !
      ! * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エンストロフィースペクトルは (1/2)n^2(n+1)^2|ψ(n,m)|^2 と計算される.
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
         nm_EnstrophyFromStreamfunc_w(n,0) &
                 = 0.5 * n**2 * (n+1)**2 * w_Strfunc(l_nm(n,0))**2 
         do m=1,n
            nm_EnstrophyFromStreamfunc_w(n,m) &
                 = 0.5 * n**2 * (n+1)**2 * &
                 (w_Strfunc(l_nm(n,m))**2 +w_Strfunc(l_nm(n,-m))**2 )
            nm_EnstrophyFromStreamfunc_w(n,-m) &
                 = nm_EnstrophyFromStreamfunc_w(n,m)
         enddo
      enddo
    end function nm_EnstrophyFromStreamfunc_w

    function n_EnstrophyFromStreamfunc_w(w_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(1 層用)
      !
      ! * 全波数 n の流線関数のスペクトル成分ψ(n,m) からエンストロフィー
      !   スペクトルはΣ[m=-nm]^nm(1/2)n^2(n+1)^2|ψ(n,m)|^2 と計算される.
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
              = 0.5 * n**2 * (n+1)**2 * w_StrFunc(l_nm(n,0))**2
         do m=1,n
            n_EnstrophyFromStreamfunc_w(n) &
               =  n_EnstrophyFromStreamfunc_w(n) &
               + 2* 0.5 * n**2 * (n+1)**2 &
                * (w_StrFunc(l_nm(n,m))**2+w_StrFunc(l_nm(n,-m))**2)
         enddo
      enddo
    end function n_EnstrophyFromStreamfunc_w

end module w_spectrum_module_sjpack
