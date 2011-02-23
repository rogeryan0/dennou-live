!----------------------------------------------------------------------
!     Copyright (C) 2005-2008 Shin-ichi Takehiro. All rights reserved.
!----------------------------------------------------------------------
!
!ɽ��  ee_module �ƥ��ȥץ���� (�䥳�ӥ���׻�)
!
!����  2005/07/19  �ݹ�����
!      2007/11/09  �ݹ�����  ���顼��å������ɲ�
!      2008/05/10  �ݹ�����  ������ʣ����������å�
!
program ee_test_jacobian

  use dc_message, only : MessageNotify
  use ee_module
  implicit none

 !---- ���ֲ��������� ----
  integer, parameter :: im=16, jm=16            ! �ʻ���������(X,Y)
  integer, parameter :: km=5, lm=5              ! �����ȿ�������(X,Y)

  integer, parameter :: im2=64, jm2=64            ! �ʻ���������(X,Y)
  integer, parameter :: km2=21, lm2=21            ! �����ȿ�������(X,Y)

 !---- �ѿ� ----
  real(8)            :: yx_A(0:jm-1,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_B(0:jm-1,0:im-1)     ! �ʻҥǡ���
  real(8)            :: yx_Jacob(0:jm-1,0:im-1)  ! �ʻҥǡ���

  real(8)            :: yx_A2(0:jm2-1,0:im2-1)     ! �ʻҥǡ���
  real(8)            :: yx_B2(0:jm2-1,0:im2-1)     ! �ʻҥǡ���
  real(8)            :: yx_Jacob2(0:jm2-1,0:im2-1)  ! �ʻҥǡ���

  integer            :: k=4,l=2

 !---- ��ɸ�ѿ��ʤ� ----
  real(8), parameter :: xmin = -1.0d0, xmax=1.0d0
  real(8), parameter :: ymin = -1.0d0, ymax=1.0d0

  real(8), parameter :: pi=3.1415926535897932385D0
  real(8), parameter :: eps = 1.0d-10            ! Ƚ���

  integer            :: id1, id2

  call MessageNotify('M','ee_test_jacobian', &
       'ee_module jacobian function tests')

 !---------------- ��ɸ�ͤ����� ---------------------
  call ee_initial(im,jm,km,lm,xmin,xmax,ymin,ymax,id1)        ! ���ڥ��ȥ�����
  call ee_initial(im2,jm2,km2,lm2,xmin,xmax,ymin,ymax,id2)    ! ���ڥ��ȥ�����

 !------------------- ��������� ----------------------
  write(6,*) '*** Test of ee_module : Jacobian function check.'
  write(6,*) '  The result will be printed '
  write(6,*) '  only when the error is larger than ', eps
  write(6,*)

!!$  write(6,*) '  Input wavenumbers of the grid data, k and l :'
!!$  read(5,*) k,l
  write(6,*) '  k,l = ', k,l

  ! id1
  write(*,*) 'for id1'
  call ee_ChangeResolutionDomain(id1)

  yx_A = sin(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X),sin(l*pi*Y))')

  yx_A = cos(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(cos(k*pi*X),cos(l*pi*Y))')

  yx_A = sin(k*pi*yx_X)
  yx_B = cos(l*pi*yx_Y)
  yx_Jacob = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(sin(k*pi*X),cos(l*pi*Y))')

  yx_A = cos(k*pi*yx_X)
  yx_B = sin(l*pi*yx_Y)
  yx_Jacob = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A),ee_yx(yx_B)))-yx_Jacob, &
       eps, 'Jacob(cos(k*pi*X),sin(l*pi*Y))')


  ! id2
  write(*,*) 'for id2'
  call ee_ChangeResolutionDomain(id2)

  yx_A2 = sin(k*pi*yx_X)
  yx_B2 = sin(l*pi*yx_Y)
  yx_Jacob2 = k*pi*cos(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2)))-yx_Jacob2, &
       eps, 'Jacob(sin(k*pi*X),sin(l*pi*Y))')

  yx_A2 = cos(k*pi*yx_X)
  yx_B2 = cos(l*pi*yx_Y)
  yx_Jacob2 = k*pi*sin(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2)))-yx_Jacob2, &
       eps, 'Jacob(cos(k*pi*X),cos(l*pi*Y))')

  yx_A2 = sin(k*pi*yx_X)
  yx_B2 = cos(l*pi*yx_Y)
  yx_Jacob2 = - k*pi*cos(k*pi*yx_X) * l*pi*sin(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2)))-yx_Jacob2, &
       eps, 'Jacob(sin(k*pi*X),cos(l*pi*Y))')

  yx_A2 = cos(k*pi*yx_X)
  yx_B2 = sin(l*pi*yx_Y)
  yx_Jacob2 = - k*pi*sin(k*pi*yx_X) * l*pi*cos(l*pi*yx_Y)

  call check2d(yx_ee(ee_Jacobian_ee_ee(ee_yx(yx_A2),ee_yx(yx_B2)))-yx_Jacob2, &
       eps, 'Jacob(cos(k*pi*X),sin(l*pi*Y))')

  call MessageNotify('M','ee_test_derivative', &
       'ee_module jacobian function tests succeeded!')

 stop
contains

  subroutine check2d(var,eps,funcname) ! �����ͤ� eps �ʾ�� var �����Ǥ����
    real(8) :: var(:,:)                ! Ƚ�ꤹ������
    real(8) :: eps                     ! ��
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
             call MessageNotify('E','ee_test_jacobian', &
                  'jacobian error too large')
          endif
       enddo
    enddo
  end subroutine check2d

end program ee_test_jacobian
