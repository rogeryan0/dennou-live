!----------------------------------------------------------------------
!     Copyright (c) 2005 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ������ (��ʬ�׻�)
!
!����  2005/07/19  �ݹ�����
!      2007/11/09  �ݹ�����  ���顼��å������ɲ�
!
program ee_test_derivative

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=32, jm=32            ! �ʻ���������(X,Y)
  integer, parameter :: km=10, lm=10              ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_Data(0:jm-1,0:im-1)    ! �ʻҥǡ���
  real(8)            :: yx_Deriv(0:jm-1,0:im-1)   ! �ʻҥǡ���

  integer            :: k=2, l=3

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ�����

  call MessageNotify('M','ee_test_derivative', &
       'ee_module derivative function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of ee_module : derivative function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  yx_Data = sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  yx_Deriv = k*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dx_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = l*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dy_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Lapla_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(sin(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(sin(k*pi*X)*sin(l*pi*Y))')

  yx_Data = cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  yx_Deriv = -k*pi*sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dx_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -l*pi*cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dy_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Lapla_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(cos(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(cos(k*pi*X)*cos(l*pi*Y))')

  yx_Data = sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  yx_Deriv = k*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dx_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -l*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dy_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Lapla_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(sin(k*pi*X)*cos(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * sin(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(sin(k*pi*X)*cos(l*pi*Y))')

  yx_Data = cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  yx_Deriv = -k*pi*sin(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dx_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dx(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = l*pi*cos(k*pi*yx_X) * cos(l*pi*yx_Y)
  call check2d(yx_ee(ee_Dy_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Dy(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_Lapla_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'Lapla(cos(k*pi*X)*sin(l*pi*Y))')
  yx_Deriv = -1.0/((k*pi)**2 + (l*pi)**2) * cos(k*pi*yx_X) * sin(l*pi*yx_Y)
  call check2d(yx_ee(ee_LaplaInv_ee(ee_yx(yx_Data)))-yx_Deriv, &
       eps, 'LaplaInv(cos(k*pi*X)*sin(l*pi*Y))')

  call MessageNotify('M','ee_test_derivative', &
       'ee_module derivative function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:)                ! Ƚ�ꤹ������
    real(8) :: eps                     ! ����
    character(len=*), optional :: funcname
    integer i, j

    if ( present(funcname) )then
       write(6,*) '  Checking ', funcname, '...'
    endif

    do i=1,size(var,1)
       do j=1,size(var,2)
          if (abs(var(i,j)) .gt. eps ) then
             write(6,*) &
                  '    Value larger than EPS : i= ', i, '  j= ', j, var(i,j)
             call MessageNotify('E','ee_test_derivative', &
                  'derivative error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program ee_test_derivative