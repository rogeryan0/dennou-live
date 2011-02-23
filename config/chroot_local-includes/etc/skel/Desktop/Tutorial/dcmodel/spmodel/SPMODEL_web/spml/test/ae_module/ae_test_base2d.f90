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
!
program ae_test_base2d

  use dc_message, only : MessageNotify
  use dc_test,    only : AssertEqual
  use ae_module
  implicit none
  integer, parameter :: im=8, km=3, nm=3
  real(8), parameter :: xmin=0.0D0, xmax=1.0D0
  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), dimension(nm,0:im-1) :: ag_f, ag_dfdx_sol
  real(8), dimension(nm,-km:km) :: ae_f_sol
  ! 判定誤差設定
  integer, parameter :: check_digits = 16
  integer, parameter :: ignore = -14


  call MessageNotify('M','ae_test_base2d','ae_module 2-dim function tests')

  call ae_initial(im,km,xmin,xmax)

  ag_f(1,:) = 1.0D0
  ag_f(2,:) = sin(2.0D0*pi*g_x)
  ag_f(3,:) = cos(4.0D0*pi*g_x)

  ae_f_sol = 0.0D0
  ae_f_sol(1,0)=1.0D0
  ae_f_sol(2,-1)=-0.5D0
  ae_f_sol(3,2)=0.5D0

  ag_dfdx_sol(1,:) =  0.0D0
  ag_dfdx_sol(2,:) =  2.0D0*pi*cos(2.0D0*pi*g_x)
  ag_dfdx_sol(3,:) = -4.0D0*pi*sin(4.0D0*pi*g_x)

  call MessageNotify('M','test function ',&
    'f(1,:) = 1')
  call MessageNotify('M','test function ',&
    'f(2,:) = sin(2*pi*g_x)')
  call MessageNotify('M','test function ',&
    'f(3,:) = cos(4*pi*g_x)')

  call AssertEqual(&
    message = 'Forward Transform ',                             &
    answer = ae_f_sol,                                          &
    check = ae_ag(ag_f),                                        &
    significant_digits = check_digits, ignore_digits = ignore   &
  )
  call AssertEqual(&
    message = 'Inverse Transform ',                             &
    answer = ag_f,                                              &
    check = ag_ae(ae_ag(ag_f)),                                 &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message = 'Differentiation',                                &
    answer = ag_dfdx_sol,                                       &
    check = ag_ae(ae_Dx_ae(ae_ag(ag_f))),                       &
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call MessageNotify('M',&
    'ae_test_base2d','ae_module 2-dim function tests succeeded!')

end program ae_test_base2d

