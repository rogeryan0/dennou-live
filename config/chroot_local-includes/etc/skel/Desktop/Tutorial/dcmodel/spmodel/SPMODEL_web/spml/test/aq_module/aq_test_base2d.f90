!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!表題  au_module テストプログラム
!
!履歴  2008/04/01  竹広真一
!      2008/07/07  佐々木洋平 誤差範囲を修正
!      2008/08/13  竹広真一 aq_r2_aq テスト追加
!      2009/07/22  佐々木洋平 dc_test を使用するように修正
!      
!
program aq_test_base2d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use aq_module
  implicit none

  integer, parameter :: im=16, km=31         ! 格子点数, 切断波数
  real(8), parameter :: ra=2                 ! 領域の大きさ

  real(8), parameter :: alpha=1.0D0        ! 展開多項式パラメター  0 < α <= 1
  real(8), parameter :: beta= 1.0D0        ! 展開多項式パラメター  0 < β
  integer :: md(2)=(/2,3/)   

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  real(8), dimension(2,im) :: ag_data
  real(8), dimension(2,im) :: ag_data_deriv
  real(8), dimension(2,im) :: ag_data_r2
  real(8), dimension(2,im) :: ag_data_r2Inv


  call MessageNotify('M','aq_test_base2d',&
    'aq_module basic function test')

  call aq_Initial(im,km,ra,alpha,beta,md)

  ag_data(1,:) = g_R**6.0D0
  ag_data(2,:) = g_R**5.0D0

  ag_data_deriv(1,:) = 6.0D0*g_R**6.0D0
  ag_data_deriv(2,:) = 5.0D0*g_R**5.0D0

  ag_data_r2(1,:) = g_R**8.0D0
  ag_data_r2(2,:) = g_R**7.0D0

  ag_data_r2Inv(1,:) = g_R**4.0D0
  ag_data_r2Inv(2,:) = g_R**3.0D0


  call AssertEqual(&
    message='Transform',                                        &
    answer = ag_data,                                           &
    check  = ag_aq(aq_ag(ag_data)),                             &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='differentiation',                                  &
    answer = ag_data_deriv,                                     &
    check  = ag_aq(aq_rDr_aq(aq_ag(ag_data))),                  &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='r^2  multipilication',                             &
    answer = ag_data_r2,                                        &
    check  = ag_aq(aq_r2_aq(aq_ag(ag_data))),                   & 
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='r^2  multipilication',                             &
    answer = ag_data_r2Inv,                                     &
    check  = ag_aq(aq_r2Inv_aq(aq_ag(ag_data))),                &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','aq_test_base2d', &
       'Tests of basic functions ag_aq, aq_ag, aq_rDr_aq suceeded!')

end program aq_test_base2d

