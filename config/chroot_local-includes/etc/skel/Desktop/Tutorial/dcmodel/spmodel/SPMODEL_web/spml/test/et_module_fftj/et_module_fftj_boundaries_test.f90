!----------------------------------------------------------------------
!   Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  et_module_fftj �ƥ��ȥץ���� (����������)
!
!����  2009/09/11  �ݹ�����  et_test_boundaries.f90 �� fftj �Ѥ˲�¤
!
program et_module_fftj_boundaries_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use et_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=21              ! �����ȿ�������(X,Y)

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = 0.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  real(8)            :: yx_A(0:jm,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_dAdy(0:jm,0:im-1)  ! �ʻҥǡ���
  real(8)            :: et_A(-km:km,0:lm)     ! ���ڥ��ȥ�ǡ���
  real(8)            :: x_Ans(0:im-1)         ! �ʻҥǡ���

  real(8), dimension(-km:km,2)  :: values

  integer            :: k=2

  ! Ƚ�������
  integer, parameter :: check_digits = 8
  integer, parameter :: ignore = -9

  call MessageNotify('M','et_module_fftj_boundaries_test', &
       'et_module_fftj subroutines for boundary vaule problems tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call et_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

!----- et_Boundaries (DD) ------
  yx_A = sin(k*pi*yx_X)*sin(pi*yx_Y)
  et_A = et_yx(yx_a)
  values = 0.0

  call et_Boundaries(et_A,values=values,cond='DD')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  x_Ans = values(0,1)
  call AssertEqual(&
    message='et_Boundaries (DD, top)',                            &
    answer = x_Ans,                                               &
    check = yx_A(0,:),                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = values(0,2)
  call AssertEqual(&
    message='et_Boundaries (DD, bottom)',                         &
    answer = x_Ans,                                               &
    check = yx_A(jm,:),                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Boundaries (DN) ------
  yx_A = sin(k*pi*yx_X)*sin(pi/2*yx_Y)
  et_A = et_yx(yx_a)

  call et_Boundaries(et_A,values,cond='DN')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  x_Ans = values(0,1)
  call AssertEqual(&
    message='et_Boundaries (DN, top)',                            &
    answer = x_Ans,                                               &
    check = yx_A(0,:),                                            &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = values(0,2)
  call AssertEqual(&
    message='et_Boundaries (DN, bottom)',                         &
    answer = x_Ans,                                               &
    check = yx_dAdy(jm,:),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Boundaries (ND) ------
  yx_A = sin(k*pi*yx_X)*cos(pi/2*yx_Y)
  et_A = et_yx(yx_a)
  call et_Boundaries(et_A,values,cond='ND')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  x_Ans = values(0,1)
  call AssertEqual(&
    message='et_Boundaries (ND, top)',                            &
    answer = x_Ans,                                               &
    check = yx_dAdy(0,:),                                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = values(0,2)
  call AssertEqual(&
    message='et_Boundaries (ND, bottom)',                         &
    answer = x_Ans,                                               &
    check = yx_A(jm,:),                                           &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

!----- et_Boundaries (NN) ------
  yx_A = sin(k*pi*yx_X)*cos(pi*yx_Y)
  et_A = et_yx(yx_a)
  call et_Boundaries(et_A,values,cond='NN')
  yx_A = yx_et(et_A)
  yx_dAdy = yx_et(et_Dy_et(et_A))

  x_Ans = values(0,1)
  call AssertEqual(&
    message='et_Boundaries (ND, top)',                            &
    answer = x_Ans,                                               &
    check = yx_dAdy(0,:),                                         &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  x_Ans = values(0,2)
  call AssertEqual(&
    message='et_Boundaries (ND, bottom)',                         &
    answer = x_Ans,                                               &
    check = yx_dAdy(jm,:),                                        &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','et_module_fftj_boundaries_test', &
       'et_module_fftj subroutines for boundary vaule problems tests succeeded!')

end program et_module_fftj_boundaries_test
