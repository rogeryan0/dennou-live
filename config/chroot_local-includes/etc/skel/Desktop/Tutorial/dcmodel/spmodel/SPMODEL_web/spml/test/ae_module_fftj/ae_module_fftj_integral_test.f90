!----------------------------------------------------------------------
! Copyright (c) 2002-2009 SPMODEL Development Group. All rights reserved.!
!----------------------------------------------------------------------
!
!ɽ��  ae_module �ƥ��ȥץ����
!
!      ��ʬ��ʿ�ѤΥƥ���
!
!����  2009/09/08  �ݹ�����  ae_test_intavr.f90 �� ae_module_fftj �Ѥ˲�¤
!
program ae_module_fftj_integral_test

  use dc_message, only : MessageNotify
  use dc_test,    only : AssertEqual
  use ae_module_fftj
  implicit none
  
  !---- ���ֲ��������� ----
  integer, parameter :: im=16            ! �ʻ���������
  integer, parameter :: km=5,lm=2        ! �����ȿ�������
  
  !---- �ѿ� ----
  real(8)            :: g_Data(0:im-1)    ! �ʻҥǡ���
  real(8)            :: ag_Data(lm,0:im-1)    ! �ʻҥǡ���

  !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: pi=3.1415926535897932385D0
  ! Ƚ�������
  integer, parameter :: check_digits = 15
  integer, parameter :: ignore = -16

  call MessageNotify('M','ae_module_fftj_integral_test',& 
    'ae_module_fftj integrate/averagin function tests')

  !---------------- ��ɸ�ͤ����� ---------------------
  ! ���ڥ��ȥ�����
  call ae_initial(im,km,xmin,xmax)               

  !------------------- ��������� ----------------------
  g_Data = 1 - sin(2*pi*g_X)
  
  call AssertEqual(&
    message= '1-dim integrate function test',                   &
    answer = 2.0D0,                                             &
    check  = Int_g(g_Data),                                     & 
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message= '1-dim averaging function test',                   &
    answer = 1.0D0,                                             &
    check  = Avr_g(g_Data),                                     & 
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  ag_Data(1,:) = cos(pi*g_X)**2
  ag_Data(2,:) = sin(pi*g_X)**2
  call AssertEqual(&
    message= '1-dim integrate function test',                   &
    answer = (/1.0D0, 1.0D0/),                                  &
    check  = a_Int_ag(ag_Data),                                 & 
    significant_digits = check_digits, ignore_digits = ignore   &
    )

  call AssertEqual(&
    message= '1-dim averaging function test',                   &
    answer = (/0.5D0, 0.5D0/),                                  &
    check  = a_Avr_ag(ag_Data),                                 & 
    significant_digits = check_digits, ignore_digits = ignore   &
    )
  
  call MessageNotify('M','ae_module_fftj_integral_test',& 
    'ae_module_fftj integrate/averagin function tests')

end program ae_module_fftj_integral_test

