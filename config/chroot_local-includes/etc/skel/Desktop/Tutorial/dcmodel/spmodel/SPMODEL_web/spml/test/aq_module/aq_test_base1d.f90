!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!表題  aq_module テストプログラム
!
!履歴  2008/04/01  竹広真一
!      2008/07/07  佐々木洋平 誤差範囲を修正
!      2008/08/13  竹広真一 q_r2_q テスト追加
!      2009/07/22  佐々木洋平 dc_test を使用するように修正
!
program aq_test_base1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

  integer, parameter :: im=16, km=31         ! 格子点数, 切断波数
  real(8), parameter :: ra=2                 ! 領域の大きさ

  ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: alpha=1.0D0        
  ! 展開多項式パラメター  0 < β
  real(8), parameter :: beta= 1.0D0        

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8), dimension(im)   :: g_z
  real(8), dimension(0:km) :: q_z

  call MessageNotify('M','aq_test_base1d',&
    'aq_module basic function test')

  call aq_Initial(im,km,ra,alpha,beta,(/1/))

  g_z = g_R**5.0D0
  q_z = q_g(g_z)

  call AssertEqual(&
    message='Transform',                                        &
    answer = g_z,                                               &
    check  = g_q(q_g(g_z)),                                     &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='differentation',                                   &
    answer = 5.0D0*g_R**5.0D0,                                  &
    check  = g_q(q_rDr_q(q_g((g_z)))),                          &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call AssertEqual(&
    message='r^2 multipilication',                              &
    answer = g_R**7.0D0,                                        &
    check  = g_q(q_r2_q(q_g(g_z))),                             &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call AssertEqual(&
    message='r^-2 multipilication',                             &
    answer = g_R**3.0d0,                                        &
    check  = g_q(q_r2Inv_q(q_z)),                               &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call aq_Initial(im,km,ra,alpha,beta,(/2/))

  g_z = g_R**6.0D0
  q_z = q_g(g_z)

  call AssertEqual(&
    message='differentation',                                   &
    answer = 6.0D0*g_R**6.0D0,                                  &
    check  = g_q(q_rDr_q(q_g((g_z)))),                          &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call AssertEqual(&
    message='r^2 multipilication',                              &
    answer = g_R**8.0D0,                                        &
    check  = g_q(q_r2_q(q_g(g_z))),                             &
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  call AssertEqual(&
    message='r^-2 multipilication',                             &
    answer = g_R**4.0d0,                                        &
    check  = g_q(q_r2Inv_q(q_z)),                               &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','aq_test_base1d', &
       'Tests of basic functions g_q, q_g, q_rDr_q suceeded!')

end program aq_test_base1d
