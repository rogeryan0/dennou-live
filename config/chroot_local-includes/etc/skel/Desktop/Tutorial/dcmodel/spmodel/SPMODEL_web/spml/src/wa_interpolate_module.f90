!--
!----------------------------------------------------------------------
!     Copyright (c) 2007-2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_interpolate_module
!
!  spml/wa_interpolate_module モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module の下部モジュールであり, スペクトル法による
!  補間計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_interpolate_module モジュールを多層モデル用に
!  拡張したものであり, 同時に複数個のスペクトルデータを用いた
!  補間計算が行える.
!
!  補間計算の方法については doc/w_module.tex を参照のこと. 
!  このサブルーチンは内部で ISPACK の SPPACK と SNPACK のサブルーチンを
!  呼んでいない. したがって同時にあつかえるスペクトルデータの数は
!  wa_Initial で設定した km より大きくても良い. 
!
!履歴  2007/10/31  竹広真一 新規作成
!      2008/06/28  佐々木洋平 コメントを RDoc 用に微修正
!      2009/01/04  竹広真一 spml 書法に反するため複数補間点対応版削除
!      2009/01/29  佐々木洋平 コメントを RDoc 用に微修正
!      2009/07/30  竹広真一   DO 変数をローカルに変更(for OpenMP)
!
!      制限
!      * 変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module wa_interpolate_module
  !
  != wa_interpolate_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_interpolate_module.f90,v 1.8 2009-07-30 12:50:23 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/wa_interpolate_module モジュールは球面上での流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール wa_module の下部モジュールであり, スペクトル法による
  ! 補間計算のための Fortran90 関数を提供する. 
  !
  ! 球面上の 1 層モデル用 w_interpolate_module モジュールを多層モデル用に
  ! 拡張したものであり, 同時に複数個のスペクトルデータを用いた
  ! 補間計算が行える.
  !
  ! 補間計算の方法については doc/w_module.tex を参照のこと. 
  ! このサブルーチンは内部で ISPACK の SPPACK と SNPACK のサブルーチンを
  ! 呼んでいない. したがって同時にあつかえるスペクトルデータの数は
  ! wa_Initial で設定した km より大きくても良い. 
  !
  use dc_message, only : MessageNotify
  use w_base_module, only : nm, l_nm
  implicit none
  private

  public a_Interpolate_wa                        ! 補間関数

  interface a_Interpolate_wa
     !
     ! 緯度 alon, 経度 alat における関数値を
     ! 球面調和変換係数 wa_data から補間計算する
     !
     ! 入力する緯度経度座標は１点の 1 種類
     !
     module procedure a_Interpolate_array00_wa
  end interface

  interface alpha
     module procedure alpha_array0
  end interface

  interface Pmm
     module procedure Pmm_array0
  end interface

  contains

  !--------------- 補間計算 -----------------
    function a_Interpolate_array00_wa(wa_data,alon,alat)
      !
      ! 緯度 alon, 経度 alat における関数値を
      ! その球面調和変換係数 wa_data から補間計算する
      !
      real(8), intent(IN) :: wa_data(:,:)             ! スペクトルデータ
      real(8), intent(IN) :: alon                     ! 補間する位置(経度)
      real(8), intent(IN) :: alat                     ! 補間する位置(緯度)
      real(8) :: a_Interpolate_array00_wa(size(wa_data,2))   ! 補間した値
      
      real(8) :: mu
      real(8), dimension(size(wa_data,2)) :: y0, y1, y2, AnmPnm
      integer :: k,m

      mu = sin(alat)
      a_Interpolate_array00_wa = 0.0D0

      !---- Σa_n^0 P_n^0 の計算
      y2 = 0.0D0 ; y1 = 0.0D0
      do k=nm,1,-1
         y0 = alpha(k,0,mu) * y1 + beta(k+1,0)*y2 + wa_data(l_nm(k,0),:)
         y2 = y1 ; y1 = y0
      enddo
      a_Interpolate_array00_wa = (  beta(1,0) * y2 + mu*sqrt(3.0D0) * y1 &
                       + wa_data(l_nm(0,0),:)  ) * Pmm(0,mu)

      !---- cos mλ Σa_n^m P_n^m の計算
      do m=1,nm
         y2 = 0.0D0 ; y1 = 0.0D0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m) * y2 + wa_data(l_nm(k,m),:)
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(wa_data(l_nm(m,m),:) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)

         a_Interpolate_array00_wa = a_Interpolate_array00_wa &
                                 + AnmPnm*sqrt(2.0D0)*cos(m*alon)
      end do

      !---- sin λ Σa_n^m P_n^m の計算
      do m=1,nm
         y2 = 0.0D0 ; y1 = 0.0D0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m)*y2 + wa_data(l_nm(k,-m),:)
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(wa_data(l_nm(m,-m),:) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)


         a_Interpolate_array00_wa = a_Interpolate_array00_wa &
                                 - AnmPnm*sqrt(2.0D0)*sin(m*alon)
      end do
      
    end function a_Interpolate_array00_wa

  !--------------- 下部ルーチン -----------------
    function alpha_array0(n,m,x)
      !
      !  漸化式の P_n^m の係数
      !
      integer, intent(IN) :: n,m 
      real(8), intent(IN) :: x
      real(8)             :: alpha_array0

      alpha_array0 = sqrt( (2.0D0*n+3)*(2.0D0*n+1)/((n-m+1)*(n+m+1)) ) * x
    end function alpha_array0

    function beta(n,m)
      !
      !  漸化式の P_{n-1}^m の係数
      !
      integer, intent(IN) :: n,m 
      real(8)             :: beta

      beta = - sqrt( (2.0D0*n+3)*(n+m)*(n-m)/((2*n-1)*(n+m+1)*(n-m+1)) )
    end function beta

    function Pmm_array0(m,x)
      !
      ! ルジャンドル陪函数 P_m^m(x) の計算
      !
      integer, intent(IN) :: m           ! ルジャンドル陪函数の次数
      real(8), intent(IN) :: x           ! 引き数
      real(8)             :: Pmm_array0  ! ルジャンドル陪函数の値
      real(8)             :: factrl      ! 階乗(外部関数)


      if ( m < 0 ) call MessageNotify('E','Pmm in wa_Intepolate_module',&
                                'order m should be larger equal to zero')

      Pmm_array0 = 1.0D0
      if ( m > 0 )then
            Pmm_array0 = &
                 sqrt( factrl(2*m+1) )/(2.0D0**m * factrl(m)) &
                 * (1-x**2)**(m/2.0D0)
      endif
    end function Pmm_array0

end module wa_interpolate_module
