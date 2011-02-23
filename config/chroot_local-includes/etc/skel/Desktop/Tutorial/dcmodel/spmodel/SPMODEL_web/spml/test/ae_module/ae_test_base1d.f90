!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!表題  ae_module テストプログラム
!
!履歴  2002/01/25  竹広真一
!      2002/04/10  竹広真一
!      2007/11/07  竹広真一  エラーメッセージ追加
!      2009/07/09  佐々木洋平 幾つかの数値代入を倍精度に修正
!      2009/07/22  佐々木洋平 dc_test を使用するように修正

program ae_test_base1d

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ae_module
  implicit none
  integer, parameter :: im=32, km=14
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), dimension(0:im-1) :: g_f, g_ans
  real(8), dimension(-km:km) :: e_f, e_f_sol
  ! 判定誤差設定
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12
  

  call MessageNotify('M','ae_test_base1d','ae_module 1-dim function tests')

  call ae_initial(im,km,xmin,xmax)

  !----------- test 1 ---------------

  call MessageNotify('M','ae_test_base1d', &
       'Test 1: g_f = 1.0D0')

  g_f = 1.0D0;  g_ans=0.0D0
  e_f_sol = 0.0D0 ; e_f_sol(0)=1.0D0
  e_f = e_g(g_f)

  call AssertEqual(&
    message='Test of e_g(g_f)',                                   &
    answer = e_f_sol,                                             &
    check = e_f,                                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='Test of g_e(e_g(g_f)',                               &
    answer = g_f,                                                 &
    check  = g_e(e_f),                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='Test of g_e(e_Dx_e(e_g(g_f)))',                      &
    answer = g_ans,                                               &
    check  = g_e(e_Dx_e(e_f)),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  
  !----------- test 2 ---------------


  call MessageNotify('M','ae_test_base1d', &
       'Test 1: g_f = sin(2*\pi*x)')

  g_f = sin(2.0D0*pi*g_x);  g_ans = 2.0D0*pi*cos(2.0D0*pi*g_x)
  e_f_sol = 0.0D0 ; e_f_sol(-1)=-0.5D0
  e_f = e_g(g_f)

  call AssertEqual(&
    message='Test of e_g(g_f)',                                 &
    answer = e_f_sol,                                           &
    check = e_f,                                                &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='Test of g_e(e_g(g_f))',                            &
    answer = g_f,                                               &
    check = g_e(e_f),                                           &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='Test of g_e(e_dx_e(e_g(g_f)))',                    &
    answer = g_ans,                                             &
    check = g_e(e_Dx_e(e_g(g_f))),                              &
    significant_digits = check_digits, ignore_digits = ignore   &
    )


  !----------- test 3 ---------------

  call MessageNotify('M','ae_test_base1d', &
    'Test 1: g_f = cos(4*\pi*x)')

  g_f = cos(4.0D0*pi*g_x); g_ans = - (4.0D0*pi)**2.0D0*g_f
  e_f_sol = 0.0D0 ; e_f_sol(2)=0.5D0
  e_f = e_g(g_f)

  call AssertEqual(&
    message='Test of e_g(g_f)',                                 &
    answer = e_f_sol,                                           &
    check = e_f,                                                &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='Test of g_e(e_g(g_f))',                            &
    answer = g_f,                                               &
    check = g_e(e_f),                                           &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message='Test of g_e(e_dx_e(e_dx_e(e_f))) ',                &
    answer = g_ans,                                             &
    check = g_e(e_Dx_e(e_Dx_e(e_g(g_f)))),                      &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','ae_test_base1d', &
       'ae_module 1-dim function tests succeeded!')

end program ae_test_base1d

