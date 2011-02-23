!--
!----------------------------------------------------------------------
!     Copyright (c) 2009 SPMODEL Development Group.
!----------------------------------------------------------------------
!表題  w_interpolate_module
!
!  spml/w_interpolate_module_sjpack モジュールは球面上での 2 次元流体運動を
!  球面調和函数を用いたスペクトル法によって数値計算するための 
!  モジュール w_module の下部モジュールであり, スペクトル法による
!  補間計算のための Fortran90 関数を提供する. 
!
!  補間計算の方法については doc/w_module.tex を参照のこと. 
!  このサブルーチンは内部で ISPACK の SJPACK のサブルーチンを
!  呼んでいない. 
!
!履歴  2009/09/03  竹広真一 w_interpolate_module より改造, SJPACK 対応.
!
!      制限
!         ・変換する格子点データ, スペクトルデータの配列の大きさは決めうち
!
!++
module w_interpolate_module_sjpack
  !
  !=  w_interpolate_module
  !
  ! Authors:: Shin-ichi Takehiro, Youhei SASAKI
  ! Version:: $Id: w_interpolate_module_sjpack.f90,v 1.1 2009-09-07 07:26:48 takepiro Exp $
  ! Copyright&License:: See COPYRIGHT[link:../COPYRIGHT]
  !
  !== 概要
  !
  ! spml/w_deriv_module モジュールは球面上での 2 次元流体運動を
  ! 球面調和函数を用いたスペクトル法によって数値計算するための 
  ! モジュール w_module の下部モジュールであり, スペクトル法による
  ! 補間計算のための Fortran90 関数を提供する. 
  !
  ! 内部で ISPACK の SPPACK と SNPACK の Fortran77 サブルーチンを呼んでいる. 
  ! スペクトルデータおよび格子点データの格納方法や変換の詳しい計算法に
  ! ついては ISPACK/SNPACK,SPPACK のマニュアルを参照されたい.
  !
  use dc_message, only : MessageNotify
  use w_base_module_sjpack, only : nm=>nn, l_nm
  implicit none
  private

  public Interpolate_w                        ! 補間関数

  interface Interpolate_w
     !
     ! 緯度 alon, 経度 alat における関数値を
     ! その球面調和変換係数 w_data から補間計算する
     !
     ! 入力する緯度経度座標は, 
     !       １点, 経度1点緯度複数点, 経度複数点緯度1点, 経度緯度複数点
     ! の 4 種類
     !
     module procedure Interpolate_array00_w
  end interface

  interface alpha
     module procedure alpha_array0
  end interface

  interface Pmm
     module procedure Pmm_array0
  end interface

  contains

  !--------------- 補間計算 -----------------
    function Interpolate_array00_w(w_data,alon,alat)
      !
      ! 緯度 alat, 経度 alon における関数値を
      ! その球面調和変換係数 w_data から補間計算する
      !
      real(8), intent(IN) :: w_data((nm+1)*(nm+1))  ! スペクトルデータ
      real(8), intent(IN) :: alon                   ! 補間する位置(経度)
      real(8), intent(IN) :: alat                   ! 補間する位置(緯度)
      real(8)             :: Interpolate_array00_w  ! 補間した値
      
      real(8) :: mu
      real(8) :: y0, y1, y2, AnmPnm
      integer :: k,m

      mu = sin(alat)
      Interpolate_array00_w = 0.0D0

      !---- Σa_n^0 P_n^0 の計算
      y2 = 0 ; y1 = 0
      do k=nm,1,-1
         y0 = alpha(k,0,mu) * y1 + beta(k+1,0)*y2 + w_data(l_nm(k,0))
         y2 = y1 ; y1 = y0
      enddo
      Interpolate_array00_w = (  beta(1,0) * y2 + mu*sqrt(3.0D0) * y1 &
                       + w_data(l_nm(0,0))  ) * Pmm(0,mu)

      !----  Σ Re[s_n^m] P_n^m exp(imλ)の計算
      do m=1,nm
         y2 = 0 ; y1 = 0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m) * y2 + w_data(l_nm(k,m))
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(w_data(l_nm(m,m)) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)

         Interpolate_array00_w = Interpolate_array00_w &
                                 + AnmPnm * 2 * cos(m*alon)
      end do

      !----  Σ Im[s_n^m] P_n^m exp(imλ)の計算
      do m=1,nm
         y2 = 0 ; y1 = 0
         do k=nm,m+1,-1
            y0 = alpha(k,m,mu) * y1 + beta(k+1,m)*y2 + w_data(l_nm(k,-m))
            y2 = y1 ; y1 = y0
         enddo

         AnmPnm =(w_data(l_nm(m,-m)) + beta(m+1,m)*y2 &
                   + mu*sqrt(2.0D0*m+3)*y1 ) * Pmm(m,mu)


         Interpolate_array00_w = Interpolate_array00_w &
                                 - AnmPnm * 2 * sin(m*alon)
      end do
      
    end function Interpolate_array00_w

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
      !         sqrt( factrl(2*m+1) )/(2.0D0**m * factrl(m)) &
      !          * (1-x**2)**(m/2.0D0)
      !
      ! 係数が発散しないように対数で計算する
      !
      integer, intent(IN) :: m           ! ルジャンドル陪函数の次数
      real(8), intent(IN) :: x           ! 引き数
      real(8)             :: Pmm_array0  ! ルジャンドル陪函数の値
      real(8)             :: gammaln     ! Γ関数の対数

      if ( m < 0 ) call MessageNotify('E','Pmm in w_Intepolate_module',&
                                'order m should be larger equal to zero')

      Pmm_array0 = 1.0
      if ( m > 0 )then
            Pmm_array0 = &
                 exp(gammaln(2.0D0*m+2.0)/2.0D0 - m*log(2.0D0) - gammaln(m+1.0D0)) &
                 * (1-x**2)**(m/2.0D0)
      endif
    end function Pmm_array0

end module w_interpolate_module_sjpack
