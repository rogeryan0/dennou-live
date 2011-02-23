!----------------------------------------------------------------------
!     Copyright (c) 2002 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!表題  aq_module テストプログラム
!      1 次元境界値問題
!
!履歴  2008/04/02  竹広真一
!      2009/07/24  佐々木洋平 dc_test を使用するように修正
!
program aq_test_boundary_tau_1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

 !---- 空間解像度設定 ----
  integer, parameter :: im=16, km=31       ! 格子点数, 切断波数
  real(8), parameter :: ra=2               ! 領域の大きさ

 !---- 座標変数など ----
  real(8), parameter :: alpha=0.5D0        ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 1.0D0        ! 展開多項式パラメター  0 < β
!  real(8), parameter :: gamma=2*alpha+beta ! 展開多項式パラメター 

  real(8), parameter :: amin=2.0D0

  real(8), dimension(im)   :: g_a
  real(8), dimension(im)   :: g_dadx
  real(8), dimension(0:km) :: q_a
  real(8)                  :: value=amin

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call aq_Initial(im,km,ra,alpha,beta,(/0/))

  g_a = (g_R-ra)**2.0D0 + 1.0D0
  q_a = q_g(g_a)

!----- aq_BoundaryTau_D ------
  call MessageNotify('M', 'aq_test_boundary_tau_1d', &
       'aq_module boundary test(1d)')
  call aq_BoundaryTau_D(q_a,value)
  g_a = g_q(q_a)
  call AssertEqual(&
    message='ag_BoundaryTau_D',                                 &
    answer = amin,                                              &
    check  = g_a(im),                                           &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

!----- aq_BoundaryTau_N ------
  call aq_BoundaryTau_N(q_a,value)
  g_dadx = g_q(q_rDr_q(q_a))/g_R
  call AssertEqual(&
    message='ag_BoundaryTau_D',                                 &
    answer = amin,                                              &
    check  = g_dadx(im),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M', 'aq_test_boundary_tau_1d', &
       'aq_module boundary test(1d) succeeded')

end program aq_test_boundary_tau_1d
