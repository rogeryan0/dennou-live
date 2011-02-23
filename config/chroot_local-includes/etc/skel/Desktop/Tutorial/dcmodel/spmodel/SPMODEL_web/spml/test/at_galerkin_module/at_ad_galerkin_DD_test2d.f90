!----------------------------------------------------------------------
!     Copyright (c) 2010 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  at_ad_galerkin_DD_test2d
!
!      チェビシェフ−ガラーキン法
!      両端ディリクレ境界条件用モジュールテストプログラム(2次元)
!          f(:,x=xmin)=f(:,x=xmax)=0
!
!      例1 : f(x) = sin(pi*x),    0<x<1
!      例2 : f(x) = sin(2*pi*x),  0<x<1
!
!履歴  2005/12/30  竹広真一  新規作成
!      2006/01/23  竹広真一  モジュール名変更に伴う改訂
!      2010/01/01  佐々木洋平 dc_test を使うように修正
!
program at_ad_galerkin_DD_test2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use at_ad_galerkin_DD
  use at_module

  implicit none

  integer, parameter :: jm=2                   ! 1 次元目格子点数
  integer, parameter :: im=32                  ! 格子点数              
  integer, parameter :: km=32                  ! チェビシェフ切断波数  
  integer, parameter :: ks=2                   ! ガラーキン基底最低次数
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0 ! 計算領域

  real(8) :: ag_data(jm,0:im)
  real(8) :: ag_data_orig(jm,0:im)
  real(8) :: ad_data(jm,ks:km)
  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11
  real(8) :: pi
  pi = atan(1.0D0)*4.0D0

  call MessageNotify('M','at_ad_galerkin_DD_test2d', &
       & '2D both Dirichlet B.C.')

  call at_Initial(im,km,xmin,xmax)
  call at_ad_galerkin_DD_Initial(im,km)           !--- 両端ディリクレ ---

  ag_data(1,:) = sin(pi*g_X)
  ag_data(2,:) = sin(2*pi*g_X)
  ag_data_orig = ag_data

  ad_data = ad_ag(ag_data)
  ag_data = ag_ad(ad_data)

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> Grid ')

!  だめな例
!  ag_data = ag_ad(ad_Dx_ad(ad_Dx_ad(ad_data)))

!  よい例 : 微分値はチェビシェフ係数で保持するべし
  ad_data = ad_at(at_Dx_at(at_Dx_at(at_ad(ad_data))))
  ag_data = ag_ad(ad_data)
  
  ag_data_orig(1,:) = ag_data_orig(1,:) * (-pi**2)
  ag_data_orig(2,:) = ag_data_orig(2,:) * (-4*pi**2)

  call check2d(ag_data, ag_data_orig, &
    & 'Grid -> Galerkin -> (Dx)^2 -> Grid ')

  call MessageNotify('M','at_ad_galerkin_DD_test2d', &
       & '2D both Dirichlet B.C. succeeded!')

contains
  subroutine check2d(var, true, funcname)
    real(8) :: var(:,:)
    real(8) :: true(:,:)
    character(len=*), optional :: funcname

    call AssertEqual(&
      & message = funcname,   &
      & answer  = true,       &
      & check   = var,        &
      significant_digits = check_digits, ignore_digits = ignore   &
      )
  end subroutine check2d
  

end program at_ad_galerkin_DD_test2d
