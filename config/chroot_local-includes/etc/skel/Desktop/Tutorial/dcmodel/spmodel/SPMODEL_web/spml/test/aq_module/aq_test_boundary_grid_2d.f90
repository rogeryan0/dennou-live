!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module テストプログラム
!      1 次元境界値問題
!
!履歴  2008/04/02  竹広真一
!      2009/07/22  佐々木洋平 dc_test を使用するように修正
!
program aq_test_boundary_grid_2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=4, km=7         ! 格子点数, 切断波数
  real(8), parameter :: ra=2               ! 領域の大きさ

  integer, parameter :: lm=2               ! 切断波数の設定

 !---- 座標変数など ----
  real(8), parameter :: alpha=0.5D0        ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 1.0D0        ! 展開多項式パラメター  0 < β
!  real(8), parameter :: gamma=2*alpha+beta ! 展開多項式パラメター 

 !---- 変数 ----
  real(8), dimension(lm,im) :: ag_a
  real(8), dimension(lm,im) :: ag_dadx
!  real(8), dimension(lm,0:km) :: aq_a
  real(8), dimension(lm)      :: value

  ! 判定誤差設定
  integer, parameter :: check_digits = 14
  integer, parameter :: ignore = -15

  value(:) = (/2.0D0,1.0D0/)

  call aq_Initial(im,km,ra,alpha,beta,(/1,2/))

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
!!$  ag_a(1,:) = g_R
!!$  ag_a(2,:) = g_R**2
!  aq_a = aq_ag(ag_a)

!----- ag_BoundaryGrid_D ------
  call MessageNotify('M', 'aq_test_boundary_grid_2d', &
       'aq_module boundary test(2d)')

  call ag_BoundaryGrid_D(ag_a,value)
  call AssertEqual(&
    message='ag_BoundaryGrid_D',                                &
    answer = value,                                             &
    check  = ag_a(:,im),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

!----- ag_BoundaryGrid_N ------

  ag_a(1,:) = sin(g_R)
  ag_a(2,:) = cos(g_R)
!  aq_a = aq_ag(ag_a)

  call ag_BoundaryGrid_N(ag_a,value)
  ag_dadx = ag_aq(aq_rDr_aq(aq_ag(ag_a)))/spread(g_R,1,lm)

  call AssertEqual(&
    message='ag_BoundaryGrid_N',                                &
    answer = value,                                             &
    check  = ag_dadx(:,im),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_boundary_grid_1d', &
       'aq_module boundary test(2d) succeded.')

end program aq_test_boundary_grid_2d
