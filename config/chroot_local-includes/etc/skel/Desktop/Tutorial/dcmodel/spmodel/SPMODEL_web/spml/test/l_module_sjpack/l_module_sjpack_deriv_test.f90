!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  l_module_sjpack �ƥ��ȥץ���� :: ��ʬ�ؿ��Υƥ���
!
!����  2009/09/22  �ݹ�����
!
program l_module_sjpack_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use l_module_sjpack
  implicit none

  integer, parameter :: jm=16, nm=10

  real(8), dimension(1:jm)  ::  y_data1              ! ���δؿ�
  real(8), dimension(1:jm)  ::  y_ddata              ! ��ʬ������
  real(8), dimension(1:jm)  ::  mu                   ! ��=sin��

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','l_module_sjpack_deriv_test', &
                         'l_module_sjpack tests of derivative functions') 

  call l_Initial( nm, jm )

  !============== ��ʬ�׻� (�պ�ɸ����) �Υƥ��� ==============

  !---- P_1 �Υƥ��� ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)         ! P_1

  y_ddata = -2*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  call AssertEqual(&
    message='P_1 Test of l_Lapla_l',                              &
    answer = y_ddata,                                             &
    check = y_l(l_Lapla_l(l_y(y_data1))),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_ddata = -0.5*sqrt(3.0D0)*sin(y_Lat)      ! l_Lapla_l
  call AssertEqual(&
    message='P_1 Test of l_LaplaInv_l',                           &
    answer = y_ddata,                                             &
    check = y_l(l_LaplaInv_l(l_y(y_data1))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_ddata = sqrt(3.0D0)*cos(y_Lat)         ! y_GradLat_l
  call AssertEqual(&
    message='P_1 Test of y_GradLat_l',                            &
    answer = y_ddata,                                             &
    check = y_GradLat_l(l_y(y_data1)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_1 cos�� �Υƥ��� ----
  y_data1 = sqrt(3.0D0)*sin(y_Lat)*cos(y_Lat)               ! P_1
  y_ddata = sqrt(3.0D0)*(cos(y_Lat)**2-2*sin(y_Lat)**2)     ! p_DivLat_y
  call AssertEqual(&
    message='P_1 cos�� Test of l_DivLat_y',                             &
    answer = y_ddata,                                             &
    check = y_l(l_DivLat_y(y_data1)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_2 �Υƥ��� ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)       ! P_2

  y_ddata = -6*y_data1                                    ! l_Lapla_l
  call AssertEqual(&
    message='P_2 Test of l_Lapla_l',                              &
    answer = y_ddata,                                             &
    check = y_l(l_Lapla_l(l_y(y_data1))),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_ddata = -y_data1/6.0D0                                ! l_Lapla_l
  call AssertEqual(&
    message='P_2 Test of l_LaplaInv_l',                           &
    answer = y_ddata,                                             &
    check = y_l(l_LaplaInv_l(l_y(y_data1))),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  y_ddata = sqrt(5.0D0)*3.0*sin(y_Lat)*cos(y_Lat)         ! y_GradLat_l
  call AssertEqual(&
    message='P_2 Test of y_GradLat_l',                            &
    answer = y_ddata,                                             &
    check = y_GradLat_l(l_y(y_data1)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !---- P_2 cos�� �Υƥ��� ----
  y_data1 = sqrt(5.0D0)*(3.0/2*sin(y_Lat)**2-1/2.0)*cos(y_Lat)    ! P_2 cos��
  y_ddata = sqrt(5.0D0)*(3.0*(sin(y_Lat)*cos(y_Lat)**2-sin(y_Lat)**3) &
                         +sin(y_Lat))                             ! l_DivLat_y
  call AssertEqual(&
    message='P_2 cos�� Test of l_DivLat_y',                             &
    answer = y_ddata,                                             &
    check = y_l(l_DivLat_y(y_data1)),                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !============== ��ʬ�׻� (�̺�ɸ����) �Υƥ��� ==============
  mu = sin(y_Lat)

  !----- P_2 �Υƥ��� -----
  y_data1 = 3*mu**2-1                              ! P_2
  y_ddata = 6*mu*(1-mu**2)                         ! (1-��^2)��/�ߦ� 

  call AssertEqual(&
    message='P_2 Test of y_GradMu_l',                             &
    answer = y_ddata,                                             &
    check = y_GradMu_l(l_y(y_data1)),                             &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  !----- P_2 (1-��^2) �Υƥ��� -----
  y_data1 = (3*mu**2-1)*(1-mu**2)                ! P_2 (1-��^2)
  y_ddata = (2-3*mu**2)*4*mu                     ! ��/�ߦ�

  call AssertEqual(&
    message='P_2 (1-��^2) Test of y_DivMu_l',                     &
    answer = y_ddata,                                             &
    check = y_l(l_DivMu_y(y_data1)),                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','l_module_sjpack_deriv_test', &
       'l_module_sjpack tests of derivative functions succeeded!') 

end program l_module_sjpack_deriv_test

