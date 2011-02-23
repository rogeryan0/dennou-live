!----------------------------------------------------------------------
! Copyright (c) 2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!表題  ae_module_fftj テストプログラム
!
!      基本変換のテスト(1 次元)
!
!履歴  2009/09/08  竹広真一  ae_test_base1d.f90 を ae_module_fftj 用に改造
!

program ae_module_fftj_base1d_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ae_module_fftj
  implicit none
  integer, parameter :: im=32, km=14
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), dimension(0:im-1) :: g_f, g_Dx_ans
  real(8), dimension(-km:km) :: e_f, e_f_sol

  integer :: k

  ! 判定誤差設定
  integer, parameter :: check_digits = 10
  integer, parameter :: ignore = -11

  call MessageNotify('M','ae_module_fftj_base1d_test',&
                     'ae_module_fftj 1-dim function tests')

  call ae_initial(im,km,xmin,xmax)

  !----------- test 1 ---------------

  call MessageNotify('M','ae_test_base1d', &
       'Test 1: g_f = 1.0D0')

  g_f = 1.0D0;  g_Dx_ans=0.0D0
  e_f_sol = 0.0D0 ; e_f_sol(0)=1.D0
  e_f = e_g(g_f)

  call AssertEqual(&
    message='Test of e_g(g_f)',                                   &
    answer = e_f_sol,                                             &
    check = e_f,                                                  &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='Test of g_e(e_g(g_f))',                              &
    answer = g_f,                                                 &
    check  = g_e(e_f),                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call AssertEqual(&
    message='Test of g_e(e_Dx_e(e_g(g_f)))',                      &
    answer = g_Dx_ans,                                            &
    check  = g_e(e_Dx_e(e_f)),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  
  !----------- test 2 ---------------


  call MessageNotify('M','ae_test_base1d', &
       'Test 1: g_f = sin(2*\pi*x)')

  g_f = sin(2.0D0*pi*g_x);  g_Dx_ans = 2.0D0*pi*cos(2.0D0*pi*g_x)
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
    answer = g_Dx_ans,                                          &
    check = g_e(e_Dx_e(e_g(g_f))),                              &
    significant_digits = check_digits, ignore_digits = ignore   &
    )


  !----------- test 3 ---------------

  call MessageNotify('M','ae_test_base1d', &
    'Test 1: g_f = cos(4*\pi*x)')

  g_f = cos(4.0D0*pi*g_x); g_Dx_ans = - (4.0D0*pi)**2.0D0*g_f
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
    answer = g_Dx_ans,                                          &
    check = g_e(e_Dx_e(e_Dx_e(e_g(g_f)))),                      &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M','ae_module_fftj_base1d_test', &
       'ae_module_fftj 1-dim function tests succeeded!')

end program ae_module_fftj_base1d_test

