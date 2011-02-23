!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ef_module �ƥ��ȥץ���� (��ʬ�Ѵ�)
!
!����  2009/12/11  �ݹ�����
!
program ef_module_deriv_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ef_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32              ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=21              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: ef_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  real(8)            :: ef_A(-lm:lm,-km:km)       ! ���ڥ��ȥ�ǡ���
  real(8)            :: ef_B(-lm:lm,-km:km)       ! ���ڥ��ȥ�ǡ���

  integer            :: k=2,l=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = 0.0d0, xmax=2.0d0
  real(8), parameter :: yrad = 1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 2
  integer, parameter :: ignore = -3

  call MessageNotify('M','ef_module_deriv_test', &
       'ef_module derivative function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ef_initial(im,jm,km,lm,xmin,xmax,yrad)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  yx_Data = sin(k*pi*yx_X) * exp(-yx_Y**2)
  ef_Data = ef_yx(yx_Data)

  yx_Data = k*pi*cos(k*pi*yx_X) * exp(-yx_Y**2)
  call AssertEqual(&
    message='yx_Dx_ef with sin(k*pi*X)*exp(-Y**2)',               &
    answer = yx_Data,                                             &
    check = yx_Dx_ef(ef_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = -sin(k*pi*yx_X) * 2*yx_Y *exp(-yx_Y**2)
  call AssertEqual(&
    message='yx_Dy_ef with sin(k*pi*X)*exp(-Y**2)',               &
    answer = yx_Data,                                             &
    check = yx_Dy_ef(ef_Data),                                    &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = -(k*pi)**2*sin(k*pi*yx_X) * exp(-yx_Y**2) &
            + sin(k*pi*yx_X)*(4*yx_Y**2-2)*exp(-yx_Y**2)
  call AssertEqual(&
    message='yx_Lapla_ef with sin(k*pi*X)*exp(-Y**2)',            &
    answer = yx_Data,                                             &
    check = yx_Lapla_ef(ef_Data),                                 &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  ef_A = ef_yx(yx_Data)
  ef_B = ef_yx(yx_Data)
  yx_Data = 0.0D0
  call AssertEqual(&
    message='yx_Jacobian_ef_ef with sin(k*pi*X)*sin(l*pi*Y)',     &
    answer = yx_Data,                                             &
    check = yx_Jacobian_ef_ef(ef_A,ef_B),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) * exp(-yx_Y**2) 
  ef_A = ef_yx(yx_Data)
  yx_Data = cos(l*pi*yx_X) * exp(-yx_Y**2)
  ef_B = ef_yx(yx_Data)
  yx_Data = (k*pi)*cos(k*pi*yx_X)*cos(l*pi*yx_X)*(-2*yx_Y)*exp(-2*yx_Y**2) &
           +(l*pi)*sin(k*pi*yx_X)*sin(l*pi*yx_X)*(-2*yx_Y)*exp(-2*yx_Y**2)

  call AssertEqual(&
    message='yx_Jacobian_ef_ef with sin,cos,exp(-Y**2)',          &
    answer = yx_Data,                                             &
    check = yx_Jacobian_ef_ef(ef_A,ef_B),                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )


  call MessageNotify('M','ef_module_deriv_test', &
       'ef_module derivative function tests succeeded!')

end program ef_module_deriv_test

