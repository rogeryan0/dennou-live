!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_spectrum_module
!
!  spml/wa_spectrum_module_sjpack モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module の下部モジュールであり, 
!  スペクトル解析計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_spectrum_module_sjpack モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
!  対する変換が行える.
!
!  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
!  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
!  ついては ISPACK/SJPACK のマニュアルを参照されたい.
!
!
!履歴  2009/09/06  竹広真一 wa_spectrum_module より SJPACK 用に改造
!      2009/09/23  竹広真一 nma_* の計算修正
!
!++
module wa_spectrum_module_sjpack
  !
  != wa_spectrum_module_sjpack
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_spectrum_module_sjpack.f90,v 1.2 2009-09-23 06:35:59 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !  spml/wa_spectrum_module_sjpack モジュールは球面上での流体運動を
  !  球面調和函数を用いたスペクトル法によって数値計算するための 
  !  モジュール wa_module の下部モジュールであり, 
  !  スペクトル解析計算のための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_spectrum_module_sjpack モジュールを多層モデル用に
  !  拡張したものであり, 同時に複数個のスペクトルデータ, 格子点データに
  !  対する変換が行える.
  !
  !  内部で ISPACK の SJPACK の Fortran77 サブルーチンを呼んでいる. 
  !  スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  !  ついては ISPACK/SJPACK のマニュアルを参照されたい.
  !
  use w_base_module_sjpack, only : nm=>nn, l_nm

  implicit none

  private
 
  public nma_EnergyFromStreamfunc_wa      ! エネルギースペクトル
                                          ! (水平全波数 n, 帯状波数 m 空間)
  public na_EnergyFromStreamfunc_wa       ! エネルギースペクトル
                                          ! (水平全波数 n 空間)
  public nma_EnstrophyFromStreamfunc_wa   ! エンストロフィースペクトル
                                          ! (水平全波数 n, 帯状波数 m 空間)
  public na_EnstrophyFromStreamfunc_wa    ! エンストロフィースペクトル
                                          !  (水平全波数 n 空間)
  public wa_spectrum_VMiss                ! 欠損値

  real(8) :: wa_spectrum_VMiss = -999.000 ! 欠損値初期値

  contains

  !--------------- エネルギースペクトル計算 -----------------
    function nma_EnergyFromStreamfunc_wa(wa_Strfunc)
      ! 
      ! 流線関数のスペクトルデータからエネルギーの球面調和函数成分
      ! (スペクトル)を計算する(多層用).
      !
      !  * 全波数 n, 帯状波数 m の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルは (1/2)n(n+1)|ψ(n,m)|^2 と計算される.
      !
      !  * スペクトルの (n,m) 成分は全波数 n, 東西波数 m の実部成分, 
      !    (n,-m) 成分は全波数 n, 東西波数 m の虚部成分が格納されている.
      !
      !  * 全てのエネルギースペクトル成分の和に4πをかけたものが球面上での
      !    全エネルギーに等しい.
      !
      !  * データの存在しない全波数 n, 帯状波数 m の配列には欠損値が格納される.
      !    欠損値の値はモジュール変数 w_spectrum_VMiss によって設定できる
      !    (初期値は -999.0)
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnergyFromStreamfunc_wa
      !(out) エネルギースペクトル(水平全波数 n, 帯状波数 m 空間)

      integer n,m                             ! DO 変数

      nma_EnergyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         nma_EnergyFromStreamfunc_wa(n,0,:) &
                 = 0.5 * n*(n+1) * wa_Strfunc(l_nm(n,0),:)**2
         do m=1,n
             nma_EnergyFromStreamfunc_wa(n,m,:) &
                 = 0.5 * n*(n+1) * (  wa_Strfunc(l_nm(n,m),:)**2 &
                                    + wa_Strfunc(l_nm(n,-m),:)**2 )
             nma_EnergyFromStreamfunc_wa(n,-m,:) &
                  = nma_EnergyFromStreamfunc_wa(n,m,:)
         enddo
      enddo
    end function nma_EnergyFromStreamfunc_wa

    function na_EnergyFromStreamfunc_wa(wa_Strfunc)
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(多層用).
      !
      !  * 全波数 n の流線関数のスペクトル成分ψ(n,m) から
      !    エネルギースペクトルはΣ[m=-nm]^nm(1/2)n(n+1)|ψ(n,m)|^2 
      !    と計算される.
      !
      !  * 全てのエネルギースペクトル成分の和に 4πをかけたものが
      !    球面上での全エネルギーに等しい.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnergyFromStreamfunc_wa
      !(out) エネルギースペクトル (水平全波数 n 空間) 

      integer n,m                            ! DO 変数
  
      do n=0,nm
         na_EnergyFromStreamfunc_wa(n,:) = &
                0.5 * n*(n+1) * wa_StrFunc(l_nm(n,0),:)**2
         do m=1,n
            na_EnergyFromStreamfunc_wa(n,:) = na_EnergyFromStreamfunc_wa(n,:)+ &
                2 * 0.5 * n*(n+1) &
                * (wa_StrFunc(l_nm(n,m),:)**2+wa_StrFunc(l_nm(n,-m),:)**2)
         enddo
      enddo

    end function na_EnergyFromStreamfunc_wa

  !--------------- エンストロフィースペクトル計算 -----------------

    function nma_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! 流線関数のスペクトルデータからエンストロフィーの球面調和函数成分
      ! (スペクトル)を計算する(多層用). 
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
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,-nm:nm,size(wa_Strfunc,2)) &
           :: nma_EnstrophyFromStreamfunc_wa
      ! エンストロフィースペクトル (水平全波数 n, 帯状波数 m 空間)

      integer n,m                             ! DO 変数

      nma_EnstrophyFromStreamfunc_wa = wa_spectrum_VMiss

      do n=0,nm
         nma_EnstrophyFromStreamfunc_wa(n,0,:) &
                 = 0.5 * n**2 * (n+1)**2 &
                    * wa_Strfunc(l_nm(n,0),:)**2
         do m=1,n
            nma_EnstrophyFromStreamfunc_wa(n,m,:) &
                 = 0.5 * n**2 * (n+1)**2 &
                    * (wa_Strfunc(l_nm(n,m),:)**2+wa_Strfunc(l_nm(n,-m),:)**2)
            nma_EnstrophyFromStreamfunc_wa(n,-m,:) &
                 = nma_EnstrophyFromStreamfunc_wa(n,m,:)
         enddo
      enddo
    end function nma_EnstrophyFromStreamfunc_wa

    function na_EnstrophyFromStreamfunc_wa(wa_Strfunc)  ! 
      !
      ! 流線関数のスペクトルデータから各全波数のエネルギー成分(スペクトル)を
      ! 計算する(多層用)
      !
      ! * 全波数 n の流線関数のスペクトル成分ψ(n,m) からエンストロフィー
      !   スペクトルはΣ[m=-nm]^nm(1/2)n^2(n+1)^2|ψ(n,m)|^2 と計算される.
      !    
      ! * 全てのエネルギースペクトル成分の和に 4π/R^2 をかけたものが
      !   球面上での全エンストフィーに等しい.
      !
      real(8), intent(in)   :: wa_Strfunc(:,:)
      !(in) 流線関数(スペクトルデータ)

      real(8), dimension(0:nm,size(wa_Strfunc,2)) &
           :: na_EnstrophyFromStreamfunc_wa      ! エンストロフィースペクトル
      !(out) エンストロフィースペクトル(水平全波数 n 空間)

      integer n,m                                ! DO 変数

      do n=0,nm
         na_EnstrophyFromStreamfunc_wa(n,:)  &
              = 0.5 * n**2 * (n+1)**2 * wa_StrFunc(l_nm(n,0),:)**2
         do m=1,n
            na_EnstrophyFromStreamfunc_wa(n,:) &
               =  na_EnstrophyFromStreamfunc_wa(n,:) &
               + 2* 0.5 * n**2 * (n+1)**2 &
                * (wa_StrFunc(l_nm(n,m),:)**2+wa_StrFunc(l_nm(n,-m),:)**2)
         enddo
      enddo

    end function na_EnstrophyFromStreamfunc_wa

end module wa_spectrum_module_sjpack
