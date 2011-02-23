!----------------------------------------------------------------------
!     Copyright (c) 2002-2007 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  esc_module �ƥ��ȥץ����
!
!����  2002/08/20  �ݹ�����
!      2007/11/12  �ݹ�����  ���顼��å������ɲ�
!
program esc_test_intavr

  use dc_message, only : MessageNotify
  use esc_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm,0:im-1)    ! �ʻҥǡ���
  real(8)            :: x_Data(0:im-1)          ! �ʻҥǡ���
  real(8)            :: y_Data(0:jm)            ! �ʻҥǡ���

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  call MessageNotify('M','esc_test_intvar', &
                     'esc_module integrate/averaging function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call esc_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- �ƥ��� ----------------------
 ! y �����δؿ�������ؿ��Ǥʤ�������٤��Ǥʤ�. 
 !
  yx_Data = sin(pi*yx_X) * cos(2*pi*yx_Y)
  write(6,*) 'f = sin(pi*X)*Y**2'

    call check0d(IntYX_yx(yx_Data)-0.0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data)-0.0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 0.0, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - 0.0, eps, 'y_AvrX_yx') 

    call check1d(x_IntY_yx(yx_Data) - 0.0d0 * sin(pi*x_X), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.0d0 * sin(pi*x_X), eps, 'x_AvrY_yx')

  yx_Data = (1 - sin(pi*yx_X))  * cos(pi*yx_Y)**2
  write(6,*)
  write(6,*) 'f = (1-sin(pi*X)) * cos(pi*y_Y)**2'

    call check0d(IntYX_yx(yx_Data) - 2.0d0, eps, 'IntYX_yx') 
    call check0d(AvrYX_yx(yx_Data) - 0.5d0, eps, 'AvrYX_yx')

    call check1d(y_IntX_yx(yx_Data) - 2.0*cos(pi*y_Y)**2, eps, 'y_IntX_yx') 
    call check1d(y_AvrX_yx(yx_Data) - 1.0*cos(pi*y_Y)**2, eps, 'y_AvrX_yx')

    call check1d(x_IntY_yx(yx_Data) - 1.0*(1 - sin(pi*x_X)), eps, 'x_IntY_yx')
    call check1d(x_AvrY_yx(yx_Data) - 0.5*(1 - sin(pi*x_X)), eps, 'x_AvrY_yx')

  x_Data = 1 - sin(2*pi*x_X)
  write(6,*)
  write(6,*) 'f = 1-sin(2*pi*X)'

    call check0d(IntX_x(x_Data) - 2.0d0, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 1.0d0, eps, 'AvrX_x')

  x_Data = cos(pi*x_X)**2
  write(6,*)
  write(6,*) 'f = cos(pi*X)**2'

    call check0d(IntX_x(x_Data) - 1.0, eps, 'IntX_x')
    call check0d(AvrX_x(x_Data) - 0.5, eps, 'AvrX_x')

  y_Data = cos(pi*y_Y)**4
  write(6,*)
  write(6,*) 'f = cos(pi*Y)**4'

    call check0d(IntY_y(y_Data) - 3.0D0/4.0D0, eps, 'IntY_y')
    call check0d(AvrY_y(y_Data) - 3.0D0/8.0D0, eps, 'AvrY_y')

 call MessageNotify('M','esc_test_intvar', &
                  'esc_module integrate/averaging function tests succeeded!')
 stop

contains

  subroutine check1d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:)                  ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
    character(len=*), optional :: funcname
    integer i

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var)
       if (abs(var(i)) .gt. eps ) then
          write(6,*) '    Value larger than EPS : i= ', i, var(i)
          call MessageNotify('E','esc_test_intavr', &
               'Integrate/Avrage error too large.')
       endif
    enddo
  end subroutine check1d

  subroutine check0d(var,eps,funcname)   ! var �������ͤ� eps �ʾ���Ƚ���
    real(8) :: var                       ! Ƚ�ꤹ������
    real(8) :: eps                       ! ��
    real(8) :: vartmp(1)                 ! 
    character(len=*), optional :: funcname

    vartmp(1) = var
    if ( present(funcname) ) then
       call check1d(vartmp,eps,funcname)
    else
       call check1d(vartmp,eps)
    endif
  end subroutine check0d

end program esc_test_intavr
