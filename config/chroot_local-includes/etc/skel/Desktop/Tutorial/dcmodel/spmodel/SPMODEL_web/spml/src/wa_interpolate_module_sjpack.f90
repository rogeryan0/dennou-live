!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!表題  wa_interpolate_module_sjpack
!
!  spml/wa_interpolate_module_sjpack モジュールは球面上での流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール wa_module_sjpack の下部モジュールであり, スペクトル法による
!  補間計算のための Fortran90 関数を提供する. 
!
!  球面上の 1 層モデル用 w_interpolate_module_sjpack モジュールを
!  多層モデル用に拡張したものであり, 同時に複数個のスペクトルデータを
!  用いた補間計算が行える.
!
!  補間計算の方法については doc/w_module_sjpack.tex を参照のこと. 
!  このサブルーチンは内部で ISPACK の SJPACK のサブルーチンを
!  呼んでいない. 
!
!
!履歴  2009/09/06  竹広真一 wa_interpolate_module より SJPACK 用に改造
!
!      制限
!      * 変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module wa_interpolate_module_sjpack
  !
  != wa_interpolate_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: wa_interpolate_module_sjpack.f90,v 1.1 2009-09-07 07:26:49 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  !  spml/wa_interpolate_module_sjpack モジュールは球面上での流体運動を
  !  球面調和函数を用いたスペクトル法によって数値計算するための 
  !  モジュール wa_module_sjpack の下部モジュールであり, スペクトル法による
  !  補間計算のための Fortran90 関数を提供する. 
  !
  !  球面上の 1 層モデル用 w_interpolate_module_sjpack モジュールを
  !  多層モデル用に拡張したものであり, 同時に複数個のスペクトルデータを
  !  用いた補間計算が行える.
  !
  !  補間計算の方法については doc/w_module_sjpack.tex を参照のこと. 
  !  このサブルーチンは内部で ISPACK の SJPACK のサブルーチンを
  !  呼んでいない. 
  !
  use dc_message, only : MessageNotify
  use w_base_module_sjpack, only : nm=>nn, l_nm
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
                                 + AnmPnm * 2.0D0 * cos(m*alon)
      end do

      !---- sin λ Σa_n^m P_n^m の計算n
      do m=1,nm
         y2 = 0.0D0 ; y1 = 0.0D0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m)*y2 + wa_data(l_nm(k,-m),:)
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(wa_data(l_nm(m,-m),:) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)


         a_Interpolate_array00_wa = a_Interpolate_array00_wa &
                                 - AnmPnm * 2.0D0 * sin(m*alon)
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

end module wa_interpolate_module_sjpack
