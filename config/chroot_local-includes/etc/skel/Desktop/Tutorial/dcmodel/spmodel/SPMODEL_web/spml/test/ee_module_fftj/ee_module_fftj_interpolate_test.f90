!----------------------------------------------------------------------
!     Copyright (c) 2009 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module_fftj �ƥ��ȥץ���� (��ַ׻�)
!
!����  2009/09/11  �ݹ�����  ee_test_interpolate.f90 ��� ee_module_fftj �Ѥ˲�¤
!
program ee_module_fftj_interpolate_test

  use dc_message, only : MessageNotify
  use dc_test, only : AssertEqual
  use ee_module_fftj
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_Data1(0:jm-1,0:im-1)   ! �ʻҥǡ���
  real(8)            :: ee_Data(-lm:lm,-km:km)    ! ���ڥ��ȥ�ǡ���

  integer            :: k=2,l=5
  integer            :: i, j

 !---- ��ɸ�ѿ��ʤ� ----
!!$  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: xmin = 0.0d0, xmax=4.0d0
!!$  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0
  real(8), parameter :: ymin = 2.0d0, ymax=4.0d0

  real(8), parameter :: x0 = 2.0/3.0, x1=3.0/5.0d0
  real(8), parameter :: y0 = 1.0/7.0, y1=5.0/9.0d0

  real(8), parameter :: pi=3.1415926535897932385D0

  ! Ƚ�������
  integer, parameter :: check_digits = 11
  integer, parameter :: ignore = -12

  call MessageNotify('M','ee_module_fftj_interpolate_test', &
       'ee_module_fftj interpolate function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- �ؿ������å� ----------------------
  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X)*sin(l*pi*Y)',        &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X0)*sin(l*pi*Y0)',      &
    answer = sin(k*pi*x0)*sin(l*pi*y0),                           &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X1)*sin(l*pi*Y1)',      &
    answer = sin(k*pi*x1)*sin(l*pi*y1),                           &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X)*cos(l*pi*Y)',        &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X0)*cos(l*pi*Y0)',      &
    answer = cos(k*pi*x0)*cos(l*pi*y0),                           &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X1)*cos(l*pi*Y1)',      &
    answer = cos(k*pi*x1)*cos(l*pi*y1),                           &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X)*cos(l*pi*Y)',        &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X0)*cos(l*pi*Y0)',      &
    answer = sin(k*pi*x0)*cos(l*pi*y0),                           &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X1)*cos(l*pi*Y1)',      &
    answer = sin(k*pi*x1)*cos(l*pi*y1),                           &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X)*sin(l*pi*Y)',        &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X0)*sin(l*pi*Y0)',      &
    answer = cos(k*pi*x0)*sin(l*pi*y0),                           &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X1)*sin(l*pi*Y1)',      &
    answer = cos(k*pi*x1)*sin(l*pi*y1),                           &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(k*pi*yx_X)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X)',                    &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X0)',                   &
    answer = sin(k*pi*x0),                                        &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(k*pi*X1)',                   &
    answer = sin(k*pi*x1),                                        &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(k*pi*yx_X) 
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X)',                    &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X0)',                   &
    answer = cos(k*pi*x0),                                        &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(k*pi*X1)',                   &
    answer = cos(k*pi*x1),                                        &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = cos(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with cos(l*pi*Y)',                    &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(l*pi*Y0)',                   &
    answer = cos(l*pi*y0),                                        &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with cos(l*pi*Y1)',                   &
    answer = cos(l*pi*y1),                                        &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = sin(l*pi*yx_Y)
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo
  call AssertEqual(&
    message='Interpolate_ee with sin(l*pi*Y)',                    &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(l*pi*Y0)',                   &
    answer = sin(l*pi*y0),                                        &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with sin(l*pi*Y1)',                   &
    answer = sin(l*pi*y1),                                        &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  yx_Data = 1.0D0
  ee_Data = ee_yx(yx_Data)
  do i=0,im-1
     do j=0,jm-1
        yx_Data1(j,i) = Interpolate_ee( ee_Data, x_X(i), y_Y(j) )
     enddo
  enddo

  call AssertEqual(&
    message='Interpolate_ee with 1',                              &
    answer = yx_Data1,                                            &
    check = yx_Data,                                              &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with 1',                              &
    answer = 1.0D0,                                               &
    check = Interpolate_ee(ee_Data, x0, y0),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )
  call AssertEqual(&
    message='Interpolate_ee with 1',                              &
    answer = 1.0D0,                                               &
    check = Interpolate_ee(ee_Data, x1, y1),                      &
    significant_digits = check_digits, ignore_digits = ignore     &
    )

  call MessageNotify('M','ee_module_fftj_interpolate_test', &
       'ee_module_fftj interpolate function tests succeeded!')

end program ee_module_fftj_interpolate_test

